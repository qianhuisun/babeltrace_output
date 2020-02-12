#ifndef BABELTRACE_TRACE_IR_TRACE_INTERNAL_H
#define BABELTRACE_TRACE_IR_TRACE_INTERNAL_H

/*
 * Copyright 2017-2018 Philippe Proulx <pproulx@efficios.com>
 * Copyright 2014 Jérémie Galarneau <jeremie.galarneau@efficios.com>
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

#include "lib/assert-pre.h"
#include <babeltrace2/trace-ir/trace.h>
#include <babeltrace2/trace-ir/field-class.h>
#include <babeltrace2/trace-ir/field.h>
#include "lib/object.h"
#include "lib/object-pool.h"
#include "common/macros.h"
#include <babeltrace2/value.h>
#include <babeltrace2/types.h>
#include <glib.h>
#include <stdbool.h>
#include <sys/types.h>
#include "common/uuid.h"

#include "attributes.h"
#include "clock-class.h"
#include "stream-class.h"
#include "trace-class.h"

struct bt_trace {
	struct bt_object base;

	/* Owned by this */
	struct bt_value *user_attributes;

	/* Owned by this */
	struct bt_trace_class *class;

	struct {
		GString *str;

		/* NULL or `str->str` above */
		const char *value;
	} name;

	struct {
		bt_uuid_t uuid;

		/* NULL or `uuid` above */
		bt_uuid value;
	} uuid;

	struct bt_value *environment;

	/* Array of `struct bt_stream *` */
	GPtrArray *streams;

	/*
	 * Stream class (weak, owned by owned trace class) to number of
	 * instantiated streams, used to automatically assign stream IDs
	 * per stream class within this trace.
	 */
	GHashTable *stream_classes_stream_count;

	GArray *destruction_listeners;
	bool frozen;
};

BT_HIDDEN
void _bt_trace_freeze(const struct bt_trace *trace);

#ifdef BT_DEV_MODE
# define bt_trace_freeze		_bt_trace_freeze
#else
# define bt_trace_freeze(_trace)
#endif

BT_HIDDEN
void bt_trace_add_stream(struct bt_trace *trace, struct bt_stream *stream);

BT_HIDDEN
uint64_t bt_trace_get_automatic_stream_id(const struct bt_trace *trace,
		const struct bt_stream_class *stream_class);

#endif /* BABELTRACE_TRACE_IR_TRACE_INTERNAL_H */
