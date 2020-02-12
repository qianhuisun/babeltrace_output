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

#define BT_LOG_TAG "LIB/EVENT"
#include "lib/logging.h"

#include "lib/assert-pre.h"
#include <babeltrace2/trace-ir/event.h>
#include <babeltrace2/trace-ir/event-class.h>
#include <babeltrace2/trace-ir/stream-class.h>
#include <babeltrace2/trace-ir/packet.h>
#include <babeltrace2/trace-ir/trace.h>
#include "common/assert.h"
#include "compat/compiler.h"
#include <inttypes.h>
#include <stdbool.h>

#include "field.h"
#include "field-class.h"
#include "event.h"
#include "stream-class.h"
#include "stream.h"
#include "packet.h"
#include "trace.h"
#include "packet.h"
#include "attributes.h"
#include "event-class.h"

BT_HIDDEN
void _bt_event_set_is_frozen(const struct bt_event *event, bool is_frozen)
{
	BT_ASSERT_DBG(event);
	BT_LIB_LOGD("Setting event's frozen state: %!+e, is-frozen=%d",
		event, is_frozen);

	if (event->common_context_field) {
		BT_LOGD_STR("Setting event's common context field's frozen state.");
		bt_field_set_is_frozen(
			event->common_context_field, is_frozen);
	}

	if (event->specific_context_field) {
		BT_LOGD_STR("Setting event's specific context field's frozen state.");
		bt_field_set_is_frozen(event->specific_context_field,
			is_frozen);
	}

	if (event->payload_field) {
		BT_LOGD_STR("Setting event's payload field's frozen state.");
		bt_field_set_is_frozen(event->payload_field,
			is_frozen);
	}

	((struct bt_event *) event)->frozen = is_frozen;

	if (event->packet) {
		BT_LOGD_STR("Setting event's packet's frozen state.");
		bt_packet_set_is_frozen(event->packet, is_frozen);
	}
}

BT_HIDDEN
struct bt_event *bt_event_new(struct bt_event_class *event_class)
{
	struct bt_event *event = NULL;
	struct bt_stream_class *stream_class;
	struct bt_field_class *fc;

	BT_ASSERT(event_class);
	event = g_new0(struct bt_event, 1);
	if (!event) {
		BT_LIB_LOGE_APPEND_CAUSE("Failed to allocate one event.");
		goto error;
	}

	bt_object_init_unique(&event->base);
	stream_class = bt_event_class_borrow_stream_class(event_class);
	BT_ASSERT(stream_class);
	fc = stream_class->event_common_context_fc;
	if (fc) {
		event->common_context_field = bt_field_create(fc);
		if (!event->common_context_field) {
			/* bt_field_create() logs errors */
			goto error;
		}
	}

	fc = event_class->specific_context_fc;
	if (fc) {
		event->specific_context_field = bt_field_create(fc);
		if (!event->specific_context_field) {
			/* bt_field_create() logs errors */
			goto error;
		}
	}

	fc = event_class->payload_fc;
	if (fc) {
		event->payload_field = bt_field_create(fc);
		if (!event->payload_field) {
			/* bt_field_create() logs errors */
			goto error;
		}
	}

	goto end;

error:
	if (event) {
		bt_event_destroy(event);
		event = NULL;
	}

end:
	return event;
}

struct bt_event_class *bt_event_borrow_class(struct bt_event *event)
{
	BT_ASSERT_PRE_DEV_NON_NULL(event, "Event");
	return event->class;
}

const struct bt_event_class *bt_event_borrow_class_const(
		const struct bt_event *event)
{
	return bt_event_borrow_class((void *) event);
}

struct bt_stream *bt_event_borrow_stream(struct bt_event *event)
{
	BT_ASSERT_PRE_DEV_NON_NULL(event, "Event");
	return event->stream;
}

const struct bt_stream *bt_event_borrow_stream_const(
		const struct bt_event *event)
{
	return bt_event_borrow_stream((void *) event);
}

struct bt_field *bt_event_borrow_common_context_field(struct bt_event *event)
{
	BT_ASSERT_PRE_DEV_NON_NULL(event, "Event");
	return event->common_context_field;
}

const struct bt_field *bt_event_borrow_common_context_field_const(
		const struct bt_event *event)
{
	BT_ASSERT_PRE_DEV_NON_NULL(event, "Event");
	return event->common_context_field;
}

struct bt_field *bt_event_borrow_specific_context_field(struct bt_event *event)
{
	BT_ASSERT_PRE_DEV_NON_NULL(event, "Event");
	return event->specific_context_field;
}

const struct bt_field *bt_event_borrow_specific_context_field_const(
		const struct bt_event *event)
{
	BT_ASSERT_PRE_DEV_NON_NULL(event, "Event");
	return event->specific_context_field;
}

struct bt_field *bt_event_borrow_payload_field(struct bt_event *event)
{
	BT_ASSERT_PRE_DEV_NON_NULL(event, "Event");
	return event->payload_field;
}

const struct bt_field *bt_event_borrow_payload_field_const(
		const struct bt_event *event)
{
	BT_ASSERT_PRE_DEV_NON_NULL(event, "Event");
	return event->payload_field;
}

BT_HIDDEN
void bt_event_destroy(struct bt_event *event)
{
	BT_ASSERT(event);
	BT_LIB_LOGD("Destroying event: %!+e", event);

	if (event->common_context_field) {
		BT_LOGD_STR("Destroying event's stream event context field.");
		bt_field_destroy(event->common_context_field);
		event->common_context_field = NULL;
	}

	if (event->specific_context_field) {
		BT_LOGD_STR("Destroying event's context field.");
		bt_field_destroy(event->specific_context_field);
		event->specific_context_field = NULL;
	}

	if (event->payload_field) {
		BT_LOGD_STR("Destroying event's payload field.");
		bt_field_destroy(event->payload_field);
		event->payload_field = NULL;
	}

	BT_LOGD_STR("Putting event's class.");
	bt_object_put_ref(event->class);
	BT_LOGD_STR("Putting event's packet.");
	BT_OBJECT_PUT_REF_AND_RESET(event->packet);
	BT_LOGD_STR("Putting event's stream.");
	BT_OBJECT_PUT_REF_AND_RESET(event->stream);
	g_free(event);
}

struct bt_packet *bt_event_borrow_packet(struct bt_event *event)
{
	BT_ASSERT_PRE_DEV_NON_NULL(event, "Event");
	return event->packet;
}

const struct bt_packet *bt_event_borrow_packet_const(
		const struct bt_event *event)
{
	return bt_event_borrow_packet((void *) event);
}
