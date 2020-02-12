/*
 * Copyright 2018 Philippe Proulx <pproulx@efficios.com>
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

#define BT_LOG_TAG "CTF-WRITER/FIELD-WRAPPER"
#include "logging.h"

#include <glib.h>

#include "fields.h"
#include "field-wrapper.h"
#include "object.h"

BT_HIDDEN
struct bt_ctf_field_wrapper *bt_ctf_field_wrapper_new(void *data)
{
	struct bt_ctf_field_wrapper *field_wrapper =
		g_new0(struct bt_ctf_field_wrapper, 1);

	BT_LOGD_STR("Creating empty field wrapper object.");

	if (!field_wrapper) {
		BT_LOGE("Failed to allocate one field wrapper.");
		goto end;
	}

	bt_ctf_object_init_unique(&field_wrapper->base);
	BT_LOGD("Created empty field wrapper object: addr=%p",
		field_wrapper);

end:
	return field_wrapper;
}

BT_HIDDEN
void bt_ctf_field_wrapper_destroy(struct bt_ctf_field_wrapper *field_wrapper)
{
	BT_LOGD("Destroying field wrapper: addr=%p", field_wrapper);
	BT_ASSERT_DBG(!field_wrapper->field);
	BT_LOGD_STR("Putting stream class.");
	g_free(field_wrapper);
}

BT_HIDDEN
struct bt_ctf_field_wrapper *bt_ctf_field_wrapper_create(
		struct bt_ctf_object_pool *pool, struct bt_ctf_field_type *ft)
{
	struct bt_ctf_field_wrapper *field_wrapper = NULL;

	BT_ASSERT_DBG(pool);
	BT_ASSERT_DBG(ft);
	field_wrapper = bt_ctf_object_pool_create_object(pool);
	if (!field_wrapper) {
		BT_LOGE("Cannot allocate one field wrapper");
		goto end;
	}

	BT_ASSERT_DBG(field_wrapper->field);

end:
	return field_wrapper;
}
