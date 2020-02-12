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

#define BT_LOG_TAG "LIB/PORT"
#include "lib/logging.h"

#include "common/assert.h"
#include "lib/assert-pre.h"
#include <babeltrace2/graph/port.h>
#include <babeltrace2/graph/self-component-port.h>
#include "lib/object.h"
#include "compat/compiler.h"

#include "component.h"
#include "connection.h"
#include "port.h"

static
void destroy_port(struct bt_object *obj)
{
	struct bt_port *port = (void *) obj;

	BT_LIB_LOGI("Destroying port: %!+p", port);

	if (port->name) {
		g_string_free(port->name, TRUE);
		port->name = NULL;
	}

	g_free(port);
}

BT_HIDDEN
struct bt_port *bt_port_create(struct bt_component *parent_component,
		enum bt_port_type type, const char *name, void *user_data)
{
	struct bt_port *port = NULL;

	BT_ASSERT(name);
	BT_ASSERT(parent_component);
	BT_ASSERT(type == BT_PORT_TYPE_INPUT || type == BT_PORT_TYPE_OUTPUT);
	BT_ASSERT(strlen(name) > 0);
	port = g_new0(struct bt_port, 1);
	if (!port) {
		BT_LIB_LOGE_APPEND_CAUSE("Failed to allocate one port.");
		goto end;
	}

	BT_LIB_LOGI("Creating port for component: %![comp-]+c, port-type=%s, "
		"port-name=\"%s\"", parent_component, bt_port_type_string(type),
		name);
	bt_object_init_shared_with_parent(&port->base, destroy_port);
	port->name = g_string_new(name);
	if (!port->name) {
		BT_LIB_LOGE_APPEND_CAUSE("Failed to allocate one GString.");
		BT_OBJECT_PUT_REF_AND_RESET(port);
		goto end;
	}

	port->type = type;
	port->user_data = user_data;
	bt_object_set_parent(&port->base, &parent_component->base);
	BT_LIB_LOGI("Created port for component: "
		"%![comp-]+c, %![port-]+p", parent_component, port);

end:
	return port;
}

const char *bt_port_get_name(const struct bt_port *port)
{
	BT_ASSERT_PRE_DEV_NON_NULL(port, "Port");
	return port->name->str;
}

enum bt_port_type bt_port_get_type(const struct bt_port *port)
{
	BT_ASSERT_PRE_DEV_NON_NULL(port, "Port");
	return port->type;
}

const struct bt_connection *bt_port_borrow_connection_const(
		const struct bt_port *port)
{
	BT_ASSERT_PRE_DEV_NON_NULL(port, "Port");
	return port->connection;
}

const struct bt_component *bt_port_borrow_component_const(
		const struct bt_port *port)
{
	BT_ASSERT_PRE_DEV_NON_NULL(port, "Port");
	return bt_port_borrow_component_inline(port);
}

struct bt_self_component *bt_self_component_port_borrow_component(
		struct bt_self_component_port *port)
{
	BT_ASSERT_PRE_DEV_NON_NULL(port, "Port");
	return (void *) bt_object_borrow_parent((void *) port);
}

BT_HIDDEN
void bt_port_set_connection(struct bt_port *port,
		struct bt_connection *connection)
{
	/*
	 * Don't take a reference on connection as its existence is
	 * guaranteed by the existence of the graph in which the
	 * connection exists.
	 */
	port->connection = connection;
	BT_LIB_LOGI("Set port's connection: %![port-]+p, %![conn-]+x", port,
		connection);
}

bt_bool bt_port_is_connected(const struct bt_port *port)
{
	BT_ASSERT_PRE_DEV_NON_NULL(port, "Port");
	return port->connection ? BT_TRUE : BT_FALSE;
}

void *bt_self_component_port_get_data(const struct bt_self_component_port *port)
{
	BT_ASSERT_PRE_DEV_NON_NULL(port, "Port");
	return ((struct bt_port *) port)->user_data;
}

void bt_port_get_ref(const struct bt_port *port)
{
	bt_object_get_ref(port);
}

void bt_port_put_ref(const struct bt_port *port)
{
	bt_object_put_ref(port);
}

void bt_port_input_get_ref(const struct bt_port_input *port_input)
{
	bt_object_get_ref(port_input);
}

void bt_port_input_put_ref(const struct bt_port_input *port_input)
{
	bt_object_put_ref(port_input);
}

void bt_port_output_get_ref(const struct bt_port_output *port_output)
{
	bt_object_get_ref(port_output);
}

void bt_port_output_put_ref(const struct bt_port_output *port_output)
{
	bt_object_put_ref(port_output);
}
