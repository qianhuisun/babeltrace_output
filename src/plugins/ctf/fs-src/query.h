#ifndef BABELTRACE_PLUGIN_CTF_FS_QUERY_H
#define BABELTRACE_PLUGIN_CTF_FS_QUERY_H

/*
 * BabelTrace - CTF on File System Component
 *
 * Copyright 2017 Jérémie Galarneau <jeremie.galarneau@efficios.com>
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
#include <babeltrace2/babeltrace.h>

BT_HIDDEN
bt_component_class_query_method_status metadata_info_query(
		bt_self_component_class_source *comp_class,
		const bt_value *params,
		bt_logging_level log_level, const bt_value **result);

BT_HIDDEN
bt_component_class_query_method_status trace_infos_query(
		bt_self_component_class_source *comp_class,
		const bt_value *params, bt_logging_level log_level,
		const bt_value **result);

BT_HIDDEN
bt_component_class_query_method_status support_info_query(
		bt_self_component_class_source *comp_class,
		const bt_value *params, bt_logging_level log_level,
		const bt_value **result);

#endif /* BABELTRACE_PLUGIN_CTF_FS_QUERY_H */
