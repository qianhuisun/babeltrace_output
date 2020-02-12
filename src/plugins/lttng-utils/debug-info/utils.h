#ifndef BABELTRACE_PLUGIN_DEBUG_INFO_UTILS_H
#define BABELTRACE_PLUGIN_DEBUG_INFO_UTILS_H
/*
 * Babeltrace - Debug Info Utilities
 *
 * Copyright 2016 Jérémie Galarneau <jeremie.galarneau@efficios.com>
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

#include "common/macros.h"
#include "trace-ir-mapping.h"

/*
 * Return the location of a path's file (the last element of the path).
 * Returns the original path on error.
 */
BT_HIDDEN
const char *get_filename_from_path(const char *path);

BT_HIDDEN
bt_bool is_event_common_ctx_dbg_info_compatible(
		const bt_field_class *in_field_class,
		const char *debug_info_field_class_name);

#endif	/* BABELTRACE_PLUGIN_DEBUG_INFO_UTILS_H */
