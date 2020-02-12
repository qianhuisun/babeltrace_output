#ifndef BABELTRACE_CTFSER_INTERNAL_H
#define BABELTRACE_CTFSER_INTERNAL_H

/*
 * Copyright 2010-2011 EfficiOS Inc. and Linux Foundation
 * Copyright 2013, 2014 Jérémie Galarneau <jeremie.galarneau@efficios.com>
 * Copyright 2017-2019 Philippe Proulx <pproulx@efficios.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * The Common Trace Format (CTF) Specification is available at
 * http://www.efficios.com/ctf
 */

#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include "compat/mman.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "common/align.h"
#include "compat/endian.h"
#include "common/common.h"
#include "common/mmap-align.h"
#include <babeltrace2/types.h>
#include "common/assert.h"
#include "common/macros.h"
#include "compat/bitfield.h"
#include <glib.h>

struct bt_ctfser {
	/* Stream file's descriptor */
	int fd;

	/* Offset (bytes) of memory map (current packet) in the stream file */
	off_t mmap_offset;

	/* Offset (bytes) of packet's first byte in the memory map */
	off_t mmap_base_offset;

	/* Current offset (bits) within current packet */
	uint64_t offset_in_cur_packet_bits;

	/* Current packet size (bytes) */
	uint64_t cur_packet_size_bytes;

	/* Previous packet size (bytes) */
	uint64_t prev_packet_size_bytes;

	/* Current stream size (bytes) */
	uint64_t stream_size_bytes;

	/* Memory map base address */
	struct mmap_align *base_mma;

	/* Stream file's path (for debugging) */
	GString *path;

	/* Serializer's log level */
	int log_level;
};

/*
 * Initializes a CTF serializer.
 *
 * This function opens the file `path` for writing.
 */
BT_HIDDEN
int bt_ctfser_init(struct bt_ctfser *ctfser, const char *path,
		int log_level);

/*
 * Finalizes a CTF serializer.
 *
 * This function truncates the stream file so that there's no extra
 * padding after the last packet, and then closes the file.
 */
BT_HIDDEN
int bt_ctfser_fini(struct bt_ctfser *ctfser);

/*
 * Opens a new packet.
 *
 * All the next writing functions are performed within this new packet.
 */
BT_HIDDEN
int bt_ctfser_open_packet(struct bt_ctfser *ctfser);

/*
 * Closes the current packet, making its size `packet_size_bytes`.
 */
BT_HIDDEN
void bt_ctfser_close_current_packet(struct bt_ctfser *ctfser,
		uint64_t packet_size_bytes);

BT_HIDDEN
int _bt_ctfser_increase_cur_packet_size(struct bt_ctfser *ctfser);

static inline
uint64_t _bt_ctfser_cur_packet_size_bits(struct bt_ctfser *ctfser)
{
	return ctfser->cur_packet_size_bytes * 8;
}

static inline
uint64_t _bt_ctfser_prev_packet_size_bits(struct bt_ctfser *ctfser)
{
	return ctfser->prev_packet_size_bytes * 8;
}

static inline
uint64_t _bt_ctfser_offset_bytes(struct bt_ctfser *ctfser)
{
	return ctfser->offset_in_cur_packet_bits / 8;
}

static inline
uint8_t *_bt_ctfser_get_addr(struct bt_ctfser *ctfser)
{
	/* Only makes sense to get the address after aligning on byte */
	BT_ASSERT_DBG(ctfser->offset_in_cur_packet_bits % 8 == 0);
	return ((uint8_t *) mmap_align_addr(ctfser->base_mma)) +
		ctfser->mmap_base_offset + _bt_ctfser_offset_bytes(ctfser);
}

static inline
bool _bt_ctfser_has_space_left(struct bt_ctfser *ctfser, uint64_t size_bits)
{
	bool has_space_left = true;

	if (G_UNLIKELY((ctfser->offset_in_cur_packet_bits + size_bits >
			_bt_ctfser_cur_packet_size_bits(ctfser)))) {
		has_space_left = false;
		goto end;
	}

	if (G_UNLIKELY(size_bits > UINT64_MAX - ctfser->offset_in_cur_packet_bits)) {
		has_space_left = false;
		goto end;
	}

end:
	return has_space_left;
}

static inline
void _bt_ctfser_incr_offset(struct bt_ctfser *ctfser, uint64_t size_bits)
{
	BT_ASSERT_DBG(_bt_ctfser_has_space_left(ctfser, size_bits));
	ctfser->offset_in_cur_packet_bits += size_bits;
}

/*
 * Aligns the current offset within the current packet to
 * `alignment_bits` bits (power of two, > 0).
 */
static inline
int bt_ctfser_align_offset_in_current_packet(struct bt_ctfser *ctfser,
		uint64_t alignment_bits)
{
	int ret = 0;
	uint64_t align_size_bits;

	BT_ASSERT_DBG(alignment_bits > 0);
	align_size_bits = ALIGN(ctfser->offset_in_cur_packet_bits,
			alignment_bits) - ctfser->offset_in_cur_packet_bits;

	if (G_UNLIKELY(!_bt_ctfser_has_space_left(ctfser, align_size_bits))) {
		ret = _bt_ctfser_increase_cur_packet_size(ctfser);
		if (G_UNLIKELY(ret)) {
			goto end;
		}
	}

	_bt_ctfser_incr_offset(ctfser, align_size_bits);

end:
	return ret;
}

static inline
int _bt_ctfser_write_byte_aligned_unsigned_int_no_align(
		struct bt_ctfser *ctfser, uint64_t value,
		unsigned int size_bits, int byte_order)
{
	int ret = 0;

	/* Reverse byte order? */
	bool rbo = byte_order != BYTE_ORDER;

	BT_ASSERT_DBG(size_bits % 8 == 0);
	BT_ASSERT_DBG(_bt_ctfser_has_space_left(ctfser, size_bits));

	switch (size_bits) {
	case 8:
	{
		uint8_t v = (uint8_t) value;

		memcpy(_bt_ctfser_get_addr(ctfser), &v, sizeof(v));
		break;
	}
	case 16:
	{
		uint16_t v = (uint16_t) value;

		if (rbo) {
			v = GUINT16_SWAP_LE_BE(v);
		}

		memcpy(_bt_ctfser_get_addr(ctfser), &v, sizeof(v));
		break;
	}
	case 32:
	{
		uint32_t v = (uint32_t) value;

		if (rbo) {
			v = GUINT32_SWAP_LE_BE(v);
		}

		memcpy(_bt_ctfser_get_addr(ctfser), &v, sizeof(v));
		break;
	}
	case 64:
	{
		uint64_t v = (uint64_t) value;

		if (rbo) {
			v = GUINT64_SWAP_LE_BE(v);
		}

		memcpy(_bt_ctfser_get_addr(ctfser), &v, sizeof(v));
		break;
	}
	default:
		bt_common_abort();
	}

	_bt_ctfser_incr_offset(ctfser, size_bits);
	return ret;
}

static inline
int _bt_ctfser_write_byte_aligned_signed_int_no_align(
		struct bt_ctfser *ctfser, int64_t value,
		unsigned int size_bits, int byte_order)
{
	int ret = 0;

	/* Reverse byte order? */
	bool rbo = byte_order != BYTE_ORDER;

	BT_ASSERT_DBG(size_bits % 8 == 0);
	BT_ASSERT_DBG(_bt_ctfser_has_space_left(ctfser, size_bits));

	switch (size_bits) {
	case 8:
	{
		int8_t v = (int8_t) value;

		memcpy(_bt_ctfser_get_addr(ctfser), &v, sizeof(v));
		break;
	}
	case 16:
	{
		int16_t v = (int16_t) value;

		if (rbo) {
			v = GUINT16_SWAP_LE_BE(v);
		}

		memcpy(_bt_ctfser_get_addr(ctfser), &v, sizeof(v));
		break;
	}
	case 32:
	{
		int32_t v = (int32_t) value;

		if (rbo) {
			v = GUINT32_SWAP_LE_BE(v);
		}

		memcpy(_bt_ctfser_get_addr(ctfser), &v, sizeof(v));
		break;
	}
	case 64:
	{
		int64_t v = (int64_t) value;

		if (rbo) {
			v = GUINT64_SWAP_LE_BE(v);
		}

		memcpy(_bt_ctfser_get_addr(ctfser), &v, sizeof(v));
		break;
	}
	default:
		bt_common_abort();
	}

	_bt_ctfser_incr_offset(ctfser, size_bits);
	return ret;
}

/*
 * Writes an unsigned integer known to have a size that is a multiple of
 * 8 and an alignment that is >= 8 at the current offset within the
 * current packet.
 */
static inline
int bt_ctfser_write_byte_aligned_unsigned_int(struct bt_ctfser *ctfser,
	uint64_t value, unsigned int alignment_bits,
	unsigned int size_bits, int byte_order)
{
	int ret;

	BT_ASSERT_DBG(alignment_bits % 8 == 0);
	ret = bt_ctfser_align_offset_in_current_packet(ctfser, alignment_bits);
	if (G_UNLIKELY(ret)) {
		goto end;
	}

	if (G_UNLIKELY(!_bt_ctfser_has_space_left(ctfser, size_bits))) {
		ret = _bt_ctfser_increase_cur_packet_size(ctfser);
		if (G_UNLIKELY(ret)) {
			goto end;
		}
	}

	ret = _bt_ctfser_write_byte_aligned_unsigned_int_no_align(ctfser, value,
		size_bits, byte_order);
	if (G_UNLIKELY(ret)) {
		goto end;
	}

end:
	return ret;
}

/*
 * Writes a signed integer known to have a size that is a multiple of 8
 * and an alignment that is >= 8 at the current offset within the
 * current packet.
 */
static inline
int bt_ctfser_write_byte_aligned_signed_int(struct bt_ctfser *ctfser,
	int64_t value, unsigned int alignment_bits,
	unsigned int size_bits, int byte_order)
{
	int ret;

	BT_ASSERT_DBG(alignment_bits % 8 == 0);
	ret = bt_ctfser_align_offset_in_current_packet(ctfser, alignment_bits);
	if (G_UNLIKELY(ret)) {
		goto end;
	}

	if (G_UNLIKELY(!_bt_ctfser_has_space_left(ctfser, size_bits))) {
		ret = _bt_ctfser_increase_cur_packet_size(ctfser);
		if (G_UNLIKELY(ret)) {
			goto end;
		}
	}

	ret = _bt_ctfser_write_byte_aligned_signed_int_no_align(ctfser, value,
		size_bits, byte_order);
	if (G_UNLIKELY(ret)) {
		goto end;
	}

end:
	return ret;
}

/*
 * Writes an unsigned integer at the current offset within the current
 * packet.
 */
static inline
int bt_ctfser_write_unsigned_int(struct bt_ctfser *ctfser, uint64_t value,
	unsigned int alignment_bits, unsigned int size_bits,
	int byte_order)
{
	int ret = 0;

	ret = bt_ctfser_align_offset_in_current_packet(ctfser, alignment_bits);
	if (G_UNLIKELY(ret)) {
		goto end;
	}

	if (G_UNLIKELY(!_bt_ctfser_has_space_left(ctfser, size_bits))) {
		ret = _bt_ctfser_increase_cur_packet_size(ctfser);
		if (G_UNLIKELY(ret)) {
			goto end;
		}
	}

	if (alignment_bits % 8 == 0 && size_bits % 8 == 0) {
		ret = _bt_ctfser_write_byte_aligned_unsigned_int_no_align(
			ctfser, value, size_bits, byte_order);
		goto end;
	}

	if (byte_order == LITTLE_ENDIAN) {
		bt_bitfield_write_le(mmap_align_addr(ctfser->base_mma) +
			ctfser->mmap_base_offset, uint8_t,
			ctfser->offset_in_cur_packet_bits, size_bits, value);
	} else {
		bt_bitfield_write_be(mmap_align_addr(ctfser->base_mma) +
			ctfser->mmap_base_offset, uint8_t,
			ctfser->offset_in_cur_packet_bits, size_bits, value);
	}

	_bt_ctfser_incr_offset(ctfser, size_bits);

end:
	return ret;
}

/*
 * Writes a signed integer at the current offset within the current
 * packet.
 */
static inline
int bt_ctfser_write_signed_int(struct bt_ctfser *ctfser, int64_t value,
	unsigned int alignment_bits, unsigned int size_bits,
	int byte_order)
{
	int ret = 0;

	ret = bt_ctfser_align_offset_in_current_packet(ctfser, alignment_bits);
	if (G_UNLIKELY(ret)) {
		goto end;
	}

	if (G_UNLIKELY(!_bt_ctfser_has_space_left(ctfser, size_bits))) {
		ret = _bt_ctfser_increase_cur_packet_size(ctfser);
		if (G_UNLIKELY(ret)) {
			goto end;
		}
	}

	if (alignment_bits % 8 == 0 && size_bits % 8 == 0) {
		ret = _bt_ctfser_write_byte_aligned_signed_int_no_align(
			ctfser, value, size_bits, byte_order);
		goto end;
	}

	if (byte_order == LITTLE_ENDIAN) {
		bt_bitfield_write_le(mmap_align_addr(ctfser->base_mma) +
			ctfser->mmap_base_offset, uint8_t,
			ctfser->offset_in_cur_packet_bits, size_bits, value);
	} else {
		bt_bitfield_write_be(mmap_align_addr(ctfser->base_mma) +
			ctfser->mmap_base_offset, uint8_t,
			ctfser->offset_in_cur_packet_bits, size_bits, value);
	}

	_bt_ctfser_incr_offset(ctfser, size_bits);

end:
	return ret;
}

/*
 * Writes a 32-bit floating point number at the current offset within
 * the current packet.
 */
static inline
int bt_ctfser_write_float32(struct bt_ctfser *ctfser, double value,
	unsigned int alignment_bits, int byte_order)
{
	union u32f {
		uint32_t u;
		float f;
	} u32f;

	u32f.f = (float) value;
	return bt_ctfser_write_unsigned_int(ctfser, (uint64_t) u32f.u,
		alignment_bits, 32, byte_order);
}

/*
 * Writes a 64-bit floating point number at the current offset within
 * the current packet.
 */
static inline
int bt_ctfser_write_float64(struct bt_ctfser *ctfser, double value,
	unsigned int alignment_bits, int byte_order)
{
	union u64f {
		uint64_t u;
		double d;
	} u64f;

	u64f.d = value;
	return bt_ctfser_write_unsigned_int(ctfser, u64f.u, alignment_bits,
		64, byte_order);
}

/*
 * Writes a C string, including the terminating null character, at the
 * current offset within the current packet.
 */
static inline
int bt_ctfser_write_string(struct bt_ctfser *ctfser, const char *value)
{
	int ret = 0;
	const char *at = value;

	ret = bt_ctfser_align_offset_in_current_packet(ctfser, 8);
	if (G_UNLIKELY(ret)) {
		goto end;
	}

	while (true) {
		if (G_UNLIKELY(!_bt_ctfser_has_space_left(ctfser, 8))) {
			ret = _bt_ctfser_increase_cur_packet_size(ctfser);
			if (G_UNLIKELY(ret)) {
				goto end;
			}
		}

		memcpy(_bt_ctfser_get_addr(ctfser), at, sizeof(*at));
		_bt_ctfser_incr_offset(ctfser, 8);

		if (G_UNLIKELY(*at == '\0')) {
			break;
		}

		at++;
	}

end:
	return ret;
}

/*
 * Returns the current offset within the current packet (bits).
 */
static inline
uint64_t bt_ctfser_get_offset_in_current_packet_bits(struct bt_ctfser *ctfser)
{
	return ctfser->offset_in_cur_packet_bits;
}

/*
 * Sets the current offset within the current packet (bits).
 */
static inline
void bt_ctfser_set_offset_in_current_packet_bits(struct bt_ctfser *ctfser,
		uint64_t offset_bits)
{
	BT_ASSERT_DBG(offset_bits <= _bt_ctfser_cur_packet_size_bits(ctfser));
	ctfser->offset_in_cur_packet_bits = offset_bits;
}

static inline
const char *bt_ctfser_get_file_path(struct bt_ctfser *ctfser)
{
	return ctfser->path->str;
}

#endif /* BABELTRACE_CTFSER_INTERNAL_H */
