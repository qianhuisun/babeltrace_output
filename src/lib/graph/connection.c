/*
 * Copyright 2017-2018 Philippe Proulx <pproulx@efficios.com>
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

#define BT_LOG_TAG "LIB/CONNECTION"
#include "lib/logging.h"

#include "common/assert.h"
#include "lib/assert-pre.h"
#include "lib/assert-post.h"
#include <babeltrace2/graph/connection.h>
#include "lib/object.h"
#include "compat/compiler.h"
#include <stdbool.h>
#include <stdlib.h>
#include <glib.h>

#include "component.h"
#include "connection.h"
#include "graph.h"
#include "message/iterator.h"
#include "port.h"

static
void destroy_connection(struct bt_object *obj)
{
	struct bt_connection *connection = container_of(obj,
			struct bt_connection, base);

	BT_LIB_LOGI("Destroying connection: %!+x", connection);

	/*
	 * Make sure that each message iterator which was created for
	 * this connection is finalized before we destroy it. Once a
	 * message iterator is finalized, you cannot use it.
	 *
	 * Because connections are destroyed before components within a
	 * graph, this ensures that message iterators are always
	 * finalized before their upstream component.
	 *
	 * Ending the connection does exactly this. We pass `false` to
	 * bt_connection_end() here to avoid removing this connection
	 * from the graph: if we're here, we're already in the graph's
	 * destructor.
	 */
	bt_connection_end(connection, false);
	g_ptr_array_free(connection->iterators, TRUE);
	connection->iterators = NULL;

	/*
	 * No bt_object_put_ref on ports as a connection only holds _weak_
	 * references to them.
	 */
	g_free(connection);
}

static
void try_remove_connection_from_graph(struct bt_connection *connection)
{
	void *graph = (void *) bt_object_borrow_parent(&connection->base);

	if (connection->base.ref_count > 0 ||
			connection->downstream_port ||
			connection->upstream_port ||
			connection->iterators->len > 0) {
		return;
	}

	/*
	 * At this point we know that:
	 *
	 * 1. The connection is ended (ports were disconnected).
	 * 2. All the message iterators that this connection
	 *    created, if any, are finalized.
	 * 3. The connection's reference count is 0, so only the
	 *    parent (graph) owns this connection after this call.
	 *
	 * In other words, no other object than the graph knows this
	 * connection.
	 *
	 * It is safe to remove the connection from the graph, therefore
	 * destroying it.
	 */
	BT_LIB_LOGD("Removing self from graph's connections: "
		"%![graph-]+g, %![conn-]+x", graph, connection);
	bt_graph_remove_connection(graph, connection);
}

static
void parent_is_owner(struct bt_object *obj)
{
	struct bt_connection *connection = container_of(obj,
			struct bt_connection, base);

	try_remove_connection_from_graph(connection);
}

BT_HIDDEN
struct bt_connection *bt_connection_create(struct bt_graph *graph,
		struct bt_port *upstream_port,
		struct bt_port *downstream_port)
{
	struct bt_connection *connection = NULL;

	BT_LIB_LOGI("Creating connection: "
		"%![graph-]+g, %![up-port-]+p, %![down-port-]+p",
		graph, upstream_port, downstream_port);
	connection = g_new0(struct bt_connection, 1);
	if (!connection) {
		BT_LIB_LOGE_APPEND_CAUSE("Failed to allocate one connection.");
		goto end;
	}

	bt_object_init_shared_with_parent(&connection->base,
		destroy_connection);
	bt_object_set_parent_is_owner_listener_func(&connection->base,
		parent_is_owner);
	connection->iterators = g_ptr_array_new();
	if (!connection->iterators) {
		BT_LIB_LOGE_APPEND_CAUSE("Failed to allocate a GPtrArray.");
		BT_OBJECT_PUT_REF_AND_RESET(connection);
		goto end;
	}

	/* Weak references are taken, see comment in header. */
	connection->upstream_port = upstream_port;
	connection->downstream_port = downstream_port;
	BT_LIB_LOGD("Setting upstream port's connection: %!+p", upstream_port);
	bt_port_set_connection(upstream_port, connection);
	BT_LIB_LOGD("Setting downstream port's connection: %!+p",
		downstream_port);
	bt_port_set_connection(downstream_port, connection);
	bt_object_set_parent(&connection->base, &graph->base);
	BT_LIB_LOGI("Created connection: %!+x", connection);

end:
	return connection;
}

BT_HIDDEN
void bt_connection_end(struct bt_connection *conn, bool try_remove_from_graph)
{
	struct bt_port *downstream_port = conn->downstream_port;
	struct bt_port *upstream_port = conn->upstream_port;
	size_t i;

	BT_LIB_LOGI("Ending connection: %!+x, try-remove-from-graph=%d",
		conn, try_remove_from_graph);

	/*
	 * Any of the following message callback functions could
	 * remove one of the connection's ports from its component. To
	 * make sure that at least logging in called functions works
	 * with existing objects, get a local reference on both ports.
	 */
	bt_object_get_ref(downstream_port);
	bt_object_get_ref(upstream_port);

	if (downstream_port) {
		BT_LIB_LOGD("Disconnecting connection's downstream port: %!+p",
			downstream_port);
		bt_port_set_connection(downstream_port, NULL);
		conn->downstream_port = NULL;
	}

	if (upstream_port) {
		BT_LIB_LOGD("Disconnecting connection's upstream port: %!+p",
			upstream_port);
		bt_port_set_connection(upstream_port, NULL);
		conn->upstream_port = NULL;
	}

	/*
	 * It is safe to put the local port references now that we don't
	 * need them anymore. This could indeed destroy them.
	 */
	bt_object_put_ref(downstream_port);
	bt_object_put_ref(upstream_port);

	/*
	 * Because this connection is ended, finalize each message
	 * iterator created from it.
	 *
	 * In practice, this only happens when the connection is
	 * destroyed and not all its message iterators were finalized,
	 * which is on graph destruction.
	 */
	for (i = 0; i < conn->iterators->len; i++) {
		struct bt_message_iterator *iterator =
			g_ptr_array_index(conn->iterators, i);

		BT_LIB_LOGD("Finalizing message iterator created by "
			"this ended connection: %![iter-]+i", iterator);
		bt_message_iterator_try_finalize(
			iterator);

		/*
		 * Make sure this iterator does not try to remove itself
		 * from this connection's iterators on destruction
		 * because this connection won't exist anymore.
		 */
		bt_message_iterator_set_connection(
			iterator, NULL);
	}

	g_ptr_array_set_size(conn->iterators, 0);

	if (try_remove_from_graph) {
		try_remove_connection_from_graph(conn);
	}
}

const struct bt_port_output *bt_connection_borrow_upstream_port_const(
		const struct bt_connection *connection)
{
	BT_ASSERT_PRE_DEV_NON_NULL(connection, "Connection");
	return (void *) connection->upstream_port;
}

const struct bt_port_input *bt_connection_borrow_downstream_port_const(
		const struct bt_connection *connection)
{
	BT_ASSERT_PRE_DEV_NON_NULL(connection, "Connection");
	return (void *) connection->downstream_port;
}

BT_HIDDEN
void bt_connection_remove_iterator(struct bt_connection *conn,
		struct bt_message_iterator *iterator)
{
	g_ptr_array_remove(conn->iterators, iterator);
	BT_LIB_LOGD("Removed message iterator from connection: "
		"%![conn-]+x, %![iter-]+i", conn, iterator);
	try_remove_connection_from_graph(conn);
}

void bt_connection_get_ref(const struct bt_connection *connection)
{
	bt_object_get_ref(connection);
}

void bt_connection_put_ref(const struct bt_connection *connection)
{
	bt_object_put_ref(connection);
}
