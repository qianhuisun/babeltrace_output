#ifndef BABELTRACE2_CTF_WRITER_TRACE_H
#define BABELTRACE2_CTF_WRITER_TRACE_H

/*
 * Copyright (c) 2010-2019 EfficiOS Inc. and Linux Foundation
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
 */

#ifdef __cplusplus
extern "C" {
#endif

struct bt_ctf_trace;
struct bt_ctf_stream_class;
struct bt_ctf_stream;

extern enum bt_ctf_byte_order bt_ctf_trace_get_native_byte_order(
		struct bt_ctf_trace *trace);

extern int bt_ctf_trace_set_native_byte_order(struct bt_ctf_trace *trace,
		enum bt_ctf_byte_order native_byte_order);

extern const uint8_t *bt_ctf_trace_get_uuid(
		struct bt_ctf_trace *trace);

extern int bt_ctf_trace_set_uuid(struct bt_ctf_trace *trace,
		const uint8_t *uuid);

extern int bt_ctf_trace_set_environment_field_integer(
		struct bt_ctf_trace *trace, const char *name,
		int64_t value);

extern int bt_ctf_trace_set_environment_field_string(
		struct bt_ctf_trace *trace, const char *name,
		const char *value);

extern struct bt_ctf_field_type *bt_ctf_trace_get_packet_header_field_type(
		struct bt_ctf_trace *trace);

extern int bt_ctf_trace_set_packet_header_field_type(struct bt_ctf_trace *trace,
		struct bt_ctf_field_type *packet_header_type);

extern int64_t bt_ctf_trace_get_stream_class_count(
		struct bt_ctf_trace *trace);

extern struct bt_ctf_stream_class *bt_ctf_trace_get_stream_class_by_index(
		struct bt_ctf_trace *trace, uint64_t index);

extern struct bt_ctf_stream_class *bt_ctf_trace_get_stream_class_by_id(
		struct bt_ctf_trace *trace, uint64_t id);

extern int bt_ctf_trace_add_stream_class(struct bt_ctf_trace *trace,
		struct bt_ctf_stream_class *stream_class);

extern int64_t bt_ctf_trace_get_stream_count(struct bt_ctf_trace *trace);

extern struct bt_ctf_stream *bt_ctf_trace_get_stream_by_index(
		struct bt_ctf_trace *trace, uint64_t index);

extern const char *bt_ctf_trace_get_name(struct bt_ctf_trace *trace);

#ifdef __cplusplus
}
#endif

#endif /* BABELTRACE2_CTF_WRITER_TRACE_H */
