#ifndef BABELTRACE_PLUGINS_UTILS_MUXER_H
#define BABELTRACE_PLUGINS_UTILS_MUXER_H

/*
 * Copyright 2016 Jérémie Galarneau <jeremie.galarneau@efficios.com>
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

#include <stdint.h>
#include <babeltrace2/babeltrace.h>
#include "common/macros.h"

BT_HIDDEN
bt_component_class_initialize_method_status muxer_init(
		bt_self_component_filter *self_comp,
		bt_self_component_filter_configuration *config,
		const bt_value *params, void *init_data);

BT_HIDDEN
void muxer_finalize(bt_self_component_filter *self_comp);

BT_HIDDEN
bt_message_iterator_class_initialize_method_status muxer_msg_iter_init(
		bt_self_message_iterator *self_msg_iter,
		bt_self_message_iterator_configuration *config,
		bt_self_component_port_output *self_port);

BT_HIDDEN
void muxer_msg_iter_finalize(
		bt_self_message_iterator *self_msg_iter);

BT_HIDDEN
bt_message_iterator_class_next_method_status muxer_msg_iter_next(
		bt_self_message_iterator *self_msg_iter,
		bt_message_array_const msgs, uint64_t capacity,
		uint64_t *count);

BT_HIDDEN
bt_component_class_port_connected_method_status muxer_input_port_connected(
		bt_self_component_filter *comp,
		bt_self_component_port_input *self_port,
		const bt_port_output *other_port);

BT_HIDDEN
bt_message_iterator_class_can_seek_beginning_method_status
muxer_msg_iter_can_seek_beginning(
		bt_self_message_iterator *message_iterator, bt_bool *can_seek);

BT_HIDDEN
bt_message_iterator_class_seek_beginning_method_status muxer_msg_iter_seek_beginning(
		bt_self_message_iterator *message_iterator);

#endif /* BABELTRACE_PLUGINS_UTILS_MUXER_H */
