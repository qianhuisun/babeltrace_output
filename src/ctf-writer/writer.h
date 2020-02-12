#ifndef BABELTRACE_CTF_WRITER_WRITER_INTERNAL_H
#define BABELTRACE_CTF_WRITER_WRITER_INTERNAL_H

/*
 * Copyright 2013, 2014 Jérémie Galarneau <jeremie.galarneau@efficios.com>
 *
 * Author: Jérémie Galarneau <jeremie.galarneau@efficios.com>
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

#include <dirent.h>
#include <glib.h>
#include <sys/types.h>

#include <babeltrace2-ctf-writer/trace.h>
#include <babeltrace2-ctf-writer/writer.h>

#include "common/macros.h"

#include "object.h"

struct metadata_context {
	GString *string;
	GString *field_name;
	unsigned int current_indentation_level;
};

struct bt_ctf_writer {
	struct bt_ctf_object base;
	int frozen; /* Protects attributes that can't be changed mid-trace */
	struct bt_ctf_trace *trace;
	GString *path;
	int metadata_fd;
};

enum field_type_alias {
	FIELD_TYPE_ALIAS_UINT5_T = 0,
	FIELD_TYPE_ALIAS_UINT8_T,
	FIELD_TYPE_ALIAS_UINT16_T,
	FIELD_TYPE_ALIAS_UINT27_T,
	FIELD_TYPE_ALIAS_UINT32_T,
	FIELD_TYPE_ALIAS_UINT64_T,
	NR_FIELD_TYPE_ALIAS,
};

BT_HIDDEN
struct bt_ctf_field_type *get_field_type(enum field_type_alias alias);

BT_HIDDEN
const char *bt_ctf_get_byte_order_string(enum bt_ctf_byte_order byte_order);

BT_HIDDEN
void bt_ctf_writer_freeze(struct bt_ctf_writer *writer);

#endif /* BABELTRACE_CTF_WRITER_WRITER_INTERNAL_H */
