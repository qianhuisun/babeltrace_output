#ifndef BABELTRACE_CTF_WRITER_CLOCK_INTERNAL_H
#define BABELTRACE_CTF_WRITER_CLOCK_INTERNAL_H

/*
 * Copyright 2017 Philippe Proulx <pproulx@efficios.com>
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

#include <babeltrace2-ctf-writer/clock.h>
#include "common/macros.h"
#include <glib.h>

#include "clock-class.h"
#include "object.h"
#include "trace.h"

struct bt_ctf_clock {
	struct bt_ctf_object base;
	struct bt_ctf_clock_class *clock_class;
	uint64_t value;		/* Current clock value */
};

struct metadata_context;

BT_HIDDEN
int bt_ctf_clock_get_value(struct bt_ctf_clock *clock, uint64_t *value);

BT_HIDDEN
void bt_ctf_clock_class_serialize(struct bt_ctf_clock_class *clock_class,
		struct metadata_context *context);

#endif /* BABELTRACE_CTF_WRITER_CLOCK_INTERNAL_H */
