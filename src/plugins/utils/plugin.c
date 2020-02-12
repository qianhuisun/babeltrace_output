/*
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

#include <babeltrace2/babeltrace.h>
#include "dummy/dummy.h"
#include "counter/counter.h"
#include "muxer/muxer.h"
#include "trimmer/trimmer.h"

#ifndef BT_BUILT_IN_PLUGINS
BT_PLUGIN_MODULE();
#endif

BT_PLUGIN(utils);
BT_PLUGIN_DESCRIPTION("Common graph utilities");
BT_PLUGIN_AUTHOR("EfficiOS <https://www.efficios.com/>");
BT_PLUGIN_LICENSE("MIT");

/* sink.utils.dummy */
BT_PLUGIN_SINK_COMPONENT_CLASS(dummy, dummy_consume);
BT_PLUGIN_SINK_COMPONENT_CLASS_INITIALIZE_METHOD(dummy, dummy_init);
BT_PLUGIN_SINK_COMPONENT_CLASS_FINALIZE_METHOD(dummy, dummy_finalize);
BT_PLUGIN_SINK_COMPONENT_CLASS_GRAPH_IS_CONFIGURED_METHOD(dummy,
	dummy_graph_is_configured);
BT_PLUGIN_SINK_COMPONENT_CLASS_DESCRIPTION(dummy,
	"Consume messages and discard them.");
BT_PLUGIN_SINK_COMPONENT_CLASS_HELP(dummy,
	"See the babeltrace2-sink.utils.dummy(7) manual page.");

/* sink.utils.counter */
BT_PLUGIN_SINK_COMPONENT_CLASS(counter, counter_consume);
BT_PLUGIN_SINK_COMPONENT_CLASS_INITIALIZE_METHOD(counter, counter_init);
BT_PLUGIN_SINK_COMPONENT_CLASS_FINALIZE_METHOD(counter, counter_finalize);
BT_PLUGIN_SINK_COMPONENT_CLASS_GRAPH_IS_CONFIGURED_METHOD(counter,
	counter_graph_is_configured);
BT_PLUGIN_SINK_COMPONENT_CLASS_DESCRIPTION(counter,
	"Count messages and print the statistics.");
BT_PLUGIN_SINK_COMPONENT_CLASS_HELP(counter,
	"See the babeltrace2-sink.utils.counter(7) manual page.");

/* flt.utils.trimmer */
BT_PLUGIN_FILTER_COMPONENT_CLASS(trimmer, trimmer_msg_iter_next);
BT_PLUGIN_FILTER_COMPONENT_CLASS_DESCRIPTION(trimmer,
	"Discard messages that occur outside a specific time range.");
BT_PLUGIN_FILTER_COMPONENT_CLASS_HELP(trimmer,
	"See the babeltrace2-filter.utils.trimmer(7) manual page.");
BT_PLUGIN_FILTER_COMPONENT_CLASS_INITIALIZE_METHOD(trimmer, trimmer_init);
BT_PLUGIN_FILTER_COMPONENT_CLASS_FINALIZE_METHOD(trimmer, trimmer_finalize);
BT_PLUGIN_FILTER_COMPONENT_CLASS_MESSAGE_ITERATOR_CLASS_INITIALIZE_METHOD(trimmer,
	trimmer_msg_iter_init);
BT_PLUGIN_FILTER_COMPONENT_CLASS_MESSAGE_ITERATOR_CLASS_FINALIZE_METHOD(trimmer,
	trimmer_msg_iter_finalize);

/* flt.utils.muxer */
BT_PLUGIN_FILTER_COMPONENT_CLASS(muxer, muxer_msg_iter_next);
BT_PLUGIN_FILTER_COMPONENT_CLASS_DESCRIPTION(muxer,
	"Sort messages from multiple input ports to a single output port by time.");
BT_PLUGIN_FILTER_COMPONENT_CLASS_HELP(muxer,
	"See the babeltrace2-filter.utils.muxer(7) manual page.");
BT_PLUGIN_FILTER_COMPONENT_CLASS_INITIALIZE_METHOD(muxer, muxer_init);
BT_PLUGIN_FILTER_COMPONENT_CLASS_FINALIZE_METHOD(muxer, muxer_finalize);
BT_PLUGIN_FILTER_COMPONENT_CLASS_INPUT_PORT_CONNECTED_METHOD(muxer,
	muxer_input_port_connected);
BT_PLUGIN_FILTER_COMPONENT_CLASS_MESSAGE_ITERATOR_CLASS_INITIALIZE_METHOD(muxer,
	muxer_msg_iter_init);
BT_PLUGIN_FILTER_COMPONENT_CLASS_MESSAGE_ITERATOR_CLASS_FINALIZE_METHOD(muxer,
	muxer_msg_iter_finalize);
BT_PLUGIN_FILTER_COMPONENT_CLASS_MESSAGE_ITERATOR_CLASS_SEEK_BEGINNING_METHODS(muxer,
	muxer_msg_iter_seek_beginning, muxer_msg_iter_can_seek_beginning);
