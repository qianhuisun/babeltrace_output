#ifndef BABELTRACE_PLUGINS_UTILS_TRIMMER_H
#define BABELTRACE_PLUGINS_UTILS_TRIMMER_H

/*
 * BabelTrace - Trace Trimmer Plug-in
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
#include <babeltrace2/babeltrace.h>

BT_HIDDEN
void trimmer_finalize(bt_self_component_filter *self_comp);

BT_HIDDEN
bt_component_class_initialize_method_status trimmer_init(
		bt_self_component_filter *self_comp,
		bt_self_component_filter_configuration *config,
		const bt_value *params, void *init_data);

BT_HIDDEN
bt_message_iterator_class_initialize_method_status trimmer_msg_iter_init(
		bt_self_message_iterator *self_msg_iter,
		bt_self_message_iterator_configuration *config,
		bt_self_component_port_output *port);

BT_HIDDEN
bt_message_iterator_class_next_method_status trimmer_msg_iter_next(
		bt_self_message_iterator *self_msg_iter,
		bt_message_array_const msgs, uint64_t capacity,
		uint64_t *count);

BT_HIDDEN
void trimmer_msg_iter_finalize(bt_self_message_iterator *self_msg_iter);

#endif /* BABELTRACE_PLUGINS_UTILS_TRIMMER_H */
