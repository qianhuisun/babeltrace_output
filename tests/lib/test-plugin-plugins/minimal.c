/*
 * Copyright (c) 2017 Philippe Proulx <pproulx@efficios.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; under version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <babeltrace2/babeltrace.h>
#include <stdlib.h>
#include <glib.h>

static bt_plugin_initialize_func_status plugin_init(bt_self_plugin *plugin)
{
	g_setenv("BT_TEST_PLUGIN_INITIALIZE_CALLED", "1", 1);
	return BT_PLUGIN_INITIALIZE_FUNC_STATUS_OK;
}

static void plugin_finalize(void)
{
	g_setenv("BT_TEST_PLUGIN_FINALIZE_CALLED", "1", 1);
}

BT_PLUGIN_MODULE();
BT_PLUGIN(test_minimal);
BT_PLUGIN_DESCRIPTION("Minimal Babeltrace plugin with no component classes");
BT_PLUGIN_AUTHOR("Janine Sutto");
BT_PLUGIN_LICENSE("Beerware");
BT_PLUGIN_INITIALIZE_FUNC(plugin_init);
BT_PLUGIN_FINALIZE_FUNC(plugin_finalize);
