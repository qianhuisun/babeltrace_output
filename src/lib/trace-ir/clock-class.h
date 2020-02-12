#ifndef BABELTRACE_TRACE_IR_CLOCK_CLASS_INTERNAL_H
#define BABELTRACE_TRACE_IR_CLOCK_CLASS_INTERNAL_H

/*
 * Copyright 2017-2018 Philippe Proulx <pproulx@efficios.com>
 * Copyright 2013, 2014 Jérémie Galarneau <jeremie.galarneau@efficios.com>
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

#include <babeltrace2/trace-ir/clock-class.h>
#include "lib/object.h"
#include "common/macros.h"
#include "common/common.h"
#include "lib/object-pool.h"
#include "common/uuid.h"
#include <babeltrace2/types.h>
#include "lib/property.h"
#include "common/assert.h"
#include <stdbool.h>
#include <stdint.h>
#include <glib.h>

#include "lib/func-status.h"

struct bt_clock_class {
	struct bt_object base;

	/* Owned by this */
	struct bt_value *user_attributes;

	struct {
		GString *str;

		/* NULL or `str->str` above */
		const char *value;
	} name;

	struct {
		GString *str;

		/* NULL or `str->str` above */
		const char *value;
	} description;

	uint64_t frequency;
	uint64_t precision;
	int64_t offset_seconds;
	uint64_t offset_cycles;

	struct {
		bt_uuid_t uuid;

		/* NULL or `uuid` above */
		bt_uuid value;
	} uuid;

	bool origin_is_unix_epoch;

	/*
	 * This is computed every time you call
	 * bt_clock_class_set_frequency() or
	 * bt_clock_class_set_offset(), as well as initially. It is the
	 * base offset in nanoseconds including both `offset_seconds`
	 * and `offset_cycles` above in the result. It is used to
	 * accelerate future calls to
	 * bt_clock_snapshot_get_ns_from_origin() and
	 * bt_clock_class_cycles_to_ns_from_origin().
	 *
	 * `overflows` is true if the base offset cannot be computed
	 * because of an overflow.
	 */
	struct {
		int64_t value_ns;
		bool overflows;
	} base_offset;

	/* Pool of `struct bt_clock_snapshot *` */
	struct bt_object_pool cs_pool;

	bool frozen;
};

BT_HIDDEN
void _bt_clock_class_freeze(const struct bt_clock_class *clock_class);

#ifdef BT_DEV_MODE
# define bt_clock_class_freeze		_bt_clock_class_freeze
#else
# define bt_clock_class_freeze(_cc)
#endif

BT_HIDDEN
bt_bool bt_clock_class_is_valid(struct bt_clock_class *clock_class);

static inline
int bt_clock_class_clock_value_from_ns_from_origin(
		struct bt_clock_class *cc, int64_t ns_from_origin,
		uint64_t *raw_value)
{
	BT_ASSERT_DBG(cc);
	return bt_common_clock_value_from_ns_from_origin(cc->offset_seconds,
		cc->offset_cycles, cc->frequency, ns_from_origin,
		raw_value) ? BT_FUNC_STATUS_OVERFLOW_ERROR : BT_FUNC_STATUS_OK;
}

#endif /* BABELTRACE_TRACE_IR_CLOCK_CLASS_INTERNAL_H */
