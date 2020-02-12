/*
 * Copyright 2017-2018 Philippe Proulx <pproulx@efficios.com>
 * Copyright 2016 Jérémie Galarneau <jeremie.galarneau@efficios.com>
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

#define BT_LOG_TAG "LIB/MSG-EVENT"
#include "lib/logging.h"

#include "common/assert.h"
#include "lib/assert-pre.h"
#include "lib/assert-post.h"
#include "compat/compiler.h"
#include "lib/object.h"
#include <babeltrace2/trace-ir/event.h>
#include "lib/trace-ir/event.h"
#include "lib/trace-ir/event-class.h"
#include "lib/trace-ir/stream-class.h"
#include <babeltrace2/trace-ir/trace.h>
#include "lib/trace-ir/clock-snapshot.h"
#include "lib/graph/graph.h"
#include <babeltrace2/graph/message.h>
#include <babeltrace2/types.h>
#include <stdbool.h>
#include <inttypes.h>

#include "event.h"

static inline bool event_class_has_trace(struct bt_event_class *event_class)
{
	struct bt_stream_class *stream_class;

	stream_class = bt_event_class_borrow_stream_class_inline(event_class);
	BT_ASSERT_DBG(stream_class);
	return bt_stream_class_borrow_trace_class(stream_class);
}

BT_HIDDEN
struct bt_message *bt_message_event_new(
		struct bt_graph *graph)
{
	struct bt_message_event *message = NULL;

	message = g_new0(struct bt_message_event, 1);
	if (!message) {
		BT_LIB_LOGE_APPEND_CAUSE(
			"Failed to allocate one event message.");
		goto error;
	}

	bt_message_init(&message->parent, BT_MESSAGE_TYPE_EVENT,
		(bt_object_release_func) bt_message_event_recycle, graph);
	goto end;

error:
	BT_OBJECT_PUT_REF_AND_RESET(message);

end:
	return (void *) message;
}

static inline
struct bt_message *create_event_message(
		struct bt_self_message_iterator *self_msg_iter,
		const struct bt_event_class *c_event_class,
		const struct bt_packet *c_packet,
		const struct bt_stream *c_stream, bool with_cs,
		uint64_t raw_value)
{
	struct bt_message_iterator *msg_iter =
		(void *) self_msg_iter;
	struct bt_message_event *message = NULL;
	struct bt_event_class *event_class = (void *) c_event_class;
	struct bt_stream_class *stream_class;
	struct bt_packet *packet = (void *) c_packet;
	struct bt_stream *stream = (void *) c_stream;
	struct bt_event *event;

	BT_ASSERT_DBG(stream);
	BT_ASSERT_PRE_NON_NULL(msg_iter, "Message iterator");
	BT_ASSERT_PRE_NON_NULL(event_class, "Event class");
	BT_ASSERT_PRE(event_class_has_trace(event_class),
		"Event class is not part of a trace: %!+E", event_class);
	stream_class = bt_event_class_borrow_stream_class_inline(event_class);
	BT_ASSERT_DBG(stream_class);
	BT_ASSERT_PRE((with_cs && stream_class->default_clock_class) ||
		(!with_cs && !stream_class->default_clock_class),
		"Creating an event message with a default clock snapshot, but without "
		"a default clock class, or without a default clock snapshot, "
		"but with a default clock class: ",
		"%![ec-]+E, %![sc-]+S, with-cs=%d, "
		"cs-val=%" PRIu64,
		event_class, stream_class, with_cs, raw_value);
	BT_LIB_LOGD("Creating event message object: %![ec-]+E", event_class);
	event = bt_event_create(event_class, packet, stream);
	if (G_UNLIKELY(!event)) {
		BT_LIB_LOGE_APPEND_CAUSE(
			"Cannot create event from event class: "
			"%![ec-]+E", event_class);
		goto error;
	}

	/*
	 * Create message from pool _after_ we have everything
	 * (in this case, a valid event object) so that we never have an
	 * error condition with a non-NULL message object.
	 * Otherwise:
	 *
	 * * We cannot recycle the message on error because
	 *   bt_message_event_recycle() expects a complete
	 *   message (and the event or clock class priority map
	 *   object could be unset).
	 *
	 * * We cannot destroy the message because we would need
	 *   to notify the graph (pool owner) so that it removes the
	 *   message from its message array.
	 */
	message = (void *) bt_message_create_from_pool(
		&msg_iter->graph->event_msg_pool, msg_iter->graph);
	if (G_UNLIKELY(!message)) {
		/* bt_message_create_from_pool() logs errors */
		goto error;
	}

	if (with_cs) {
		BT_ASSERT_DBG(stream_class->default_clock_class);
		message->default_cs = bt_clock_snapshot_create(
			stream_class->default_clock_class);
		if (!message->default_cs) {
			goto error;
		}

		bt_clock_snapshot_set_raw_value(message->default_cs, raw_value);
	}

	BT_ASSERT_DBG(!message->event);
	message->event = event;

	if (packet) {
		bt_packet_set_is_frozen(packet, true);
	}

	bt_stream_freeze(stream);
	bt_event_class_freeze(event_class);
	BT_LIB_LOGD("Created event message object: "
		"%![msg-]+n, %![event-]+e", message, event);
	goto end;

error:
	BT_ASSERT(!message);
	bt_event_destroy(event);

end:
	return (void *) message;
}

struct bt_message *bt_message_event_create(
		struct bt_self_message_iterator *msg_iter,
		const struct bt_event_class *event_class,
		const struct bt_stream *stream)
{
	BT_ASSERT_PRE_DEV_NO_ERROR();
	BT_ASSERT_PRE_NON_NULL(stream, "Stream");
	return create_event_message(msg_iter, event_class, NULL, stream, false, 0);
}

struct bt_message *bt_message_event_create_with_packet(
		struct bt_self_message_iterator *msg_iter,
		const struct bt_event_class *event_class,
		const struct bt_packet *packet)
{
	BT_ASSERT_PRE_DEV_NO_ERROR();
	BT_ASSERT_PRE_NON_NULL(packet, "Packet");
	return create_event_message(msg_iter, event_class, packet,
		packet->stream, false, 0);
}

struct bt_message *bt_message_event_create_with_default_clock_snapshot(
		struct bt_self_message_iterator *msg_iter,
		const struct bt_event_class *event_class,
		const struct bt_stream *stream,
		uint64_t raw_value)
{
	BT_ASSERT_PRE_DEV_NO_ERROR();
	BT_ASSERT_PRE_NON_NULL(stream, "Stream");
	return create_event_message(msg_iter, event_class, NULL, stream,
		true, raw_value);
}

struct bt_message *
bt_message_event_create_with_packet_and_default_clock_snapshot(
		struct bt_self_message_iterator *msg_iter,
		const struct bt_event_class *event_class,
		const struct bt_packet *packet,
		uint64_t raw_value)
{
	BT_ASSERT_PRE_DEV_NO_ERROR();
	BT_ASSERT_PRE_NON_NULL(packet, "Packet");
	return create_event_message(msg_iter, event_class, packet,
		packet->stream, true, raw_value);
}

BT_HIDDEN
void bt_message_event_destroy(struct bt_message *msg)
{
	struct bt_message_event *event_msg = (void *) msg;

	BT_LIB_LOGD("Destroying event message: %!+n", msg);

	if (event_msg->event) {
		BT_LIB_LOGD("Recycling event: %!+e", event_msg->event);
		bt_event_recycle(event_msg->event);
		event_msg->event = NULL;
	}

	if (event_msg->default_cs) {
		bt_clock_snapshot_recycle(event_msg->default_cs);
		event_msg->default_cs = NULL;
	}

	g_free(msg);
}

BT_HIDDEN
void bt_message_event_recycle(struct bt_message *msg)
{
	struct bt_message_event *event_msg = (void *) msg;
	struct bt_graph *graph;

	BT_ASSERT_DBG(event_msg);

	if (G_UNLIKELY(!msg->graph)) {
		bt_message_event_destroy(msg);
		return;
	}

	BT_LIB_LOGD("Recycling event message: %![msg-]+n, %![event-]+e",
		msg, event_msg->event);
	bt_message_reset(msg);
	BT_ASSERT_DBG(event_msg->event);
	bt_event_recycle(event_msg->event);
	event_msg->event = NULL;

	if (event_msg->default_cs) {
		bt_clock_snapshot_recycle(event_msg->default_cs);
		event_msg->default_cs = NULL;
	}

	graph = msg->graph;
	msg->graph = NULL;
	bt_object_pool_recycle_object(&graph->event_msg_pool, msg);
}

static inline
struct bt_event *borrow_event(struct bt_message *message)
{
	struct bt_message_event *event_message;

	BT_ASSERT_PRE_DEV_NON_NULL(message, "Message");
	BT_ASSERT_PRE_DEV_MSG_IS_TYPE(message, BT_MESSAGE_TYPE_EVENT);
	event_message = container_of(message,
			struct bt_message_event, parent);
	return event_message->event;
}

struct bt_event *bt_message_event_borrow_event(
		struct bt_message *message)
{
	return borrow_event(message);
}

const struct bt_event *bt_message_event_borrow_event_const(
		const struct bt_message *message)
{
	return borrow_event((void *) message);
}

const struct bt_clock_snapshot *
bt_message_event_borrow_default_clock_snapshot_const(
		const struct bt_message *msg)
{
	struct bt_message_event *event_msg = (void *) msg;
	struct bt_stream_class *stream_class;

	BT_ASSERT_PRE_DEV_NON_NULL(msg, "Message");
	BT_ASSERT_PRE_DEV_MSG_IS_TYPE(msg, BT_MESSAGE_TYPE_EVENT);
	stream_class = bt_event_class_borrow_stream_class_inline(
		event_msg->event->class);
	BT_ASSERT_DBG(stream_class);
	BT_ASSERT_PRE_DEV(stream_class->default_clock_class,
		"Message's stream's class has no default clock class: "
		"%![msg-]+n, %![sc-]+S", msg, stream_class);
	return event_msg->default_cs;
}

const bt_clock_class *
bt_message_event_borrow_stream_class_default_clock_class_const(
		const bt_message *msg)
{
	struct bt_message_event *event_msg = (void *) msg;
	struct bt_stream_class *stream_class;

	BT_ASSERT_PRE_DEV_NON_NULL(msg, "Message");
	BT_ASSERT_PRE_DEV_MSG_IS_TYPE(msg, BT_MESSAGE_TYPE_EVENT);
	stream_class = bt_event_class_borrow_stream_class_inline(
		event_msg->event->class);
	BT_ASSERT_DBG(stream_class);
	return stream_class->default_clock_class;
}
