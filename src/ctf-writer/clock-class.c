/*
 * clock-class.c
 *
 * Babeltrace CTF writer - Clock class
 *
 * Copyright 2013, 2014 Jérémie Galarneau <jeremie.galarneau@efficios.com>
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

#define BT_LOG_TAG "CTF-WRITER/CLOCK-CLASS"
#include "logging.h"

#include "common/uuid.h"
#include <babeltrace2-ctf-writer/utils.h>
#include <babeltrace2-ctf-writer/object.h>
#include "compat/compiler.h"
#include <babeltrace2/types.h>
#include "compat/string.h"
#include <stdbool.h>
#include <inttypes.h>
#include "common/assert.h"

#include "assert-pre.h"
#include "clock-class.h"
#include "object.h"

static
void bt_ctf_clock_class_destroy(struct bt_ctf_object *obj);

BT_HIDDEN
bt_ctf_bool bt_ctf_clock_class_is_valid(struct bt_ctf_clock_class *clock_class)
{
	return clock_class && clock_class->name;
}

BT_HIDDEN
int bt_ctf_clock_class_set_name(struct bt_ctf_clock_class *clock_class,
		const char *name)
{
	int ret = 0;

	if (!clock_class) {
		BT_LOGW_STR("Invalid parameter: clock class is NULL.");
		ret = -1;
		goto end;
	}

	if (clock_class->frozen) {
		BT_LOGW("Invalid parameter: clock class is frozen: addr=%p, name=\"%s\"",
			clock_class, bt_ctf_clock_class_get_name(clock_class));
		ret = -1;
		goto end;
	}

	if (!bt_ctf_identifier_is_valid(name)) {
		BT_LOGW("Clock class's name is not a valid CTF identifier: "
			"addr=%p, name=\"%s\"",
			clock_class, name);
		ret = -1;
		goto end;
	}

	if (clock_class->name) {
		g_string_assign(clock_class->name, name);
	} else {
		clock_class->name = g_string_new(name);
		if (!clock_class->name) {
			BT_LOGE_STR("Failed to allocate a GString.");
			ret = -1;
			goto end;
		}
	}

	BT_LOGT("Set clock class's name: addr=%p, name=\"%s\"",
		clock_class, name);

end:
	return ret;
}

static
bool validate_freq(struct bt_ctf_clock_class *clock_class,
		const char *name, uint64_t freq)
{
	bool is_valid = true;

	if (freq == -1ULL || freq == 0) {
		BT_LOGW("Invalid parameter: frequency is invalid: "
			"addr=%p, name=\"%s\", freq=%" PRIu64,
			clock_class, name, freq);
		is_valid = false;
		goto end;
	}

end:
	return is_valid;
}

BT_HIDDEN
struct bt_ctf_clock_class *bt_ctf_clock_class_create(const char *name,
		uint64_t freq)
{
	int ret;
	struct bt_ctf_clock_class *clock_class = NULL;

	BT_LOGD("Creating default clock class object: name=\"%s\"",
		name);

	if (!validate_freq(NULL, name, freq)) {
		/* validate_freq() logs errors */
		goto error;
	}

	clock_class = g_new0(struct bt_ctf_clock_class, 1);
	if (!clock_class) {
		BT_LOGE_STR("Failed to allocate one clock class.");
		goto error;
	}

	clock_class->precision = 1;
	clock_class->frequency = freq;
	bt_ctf_object_init_shared(&clock_class->base, bt_ctf_clock_class_destroy);

	if (name) {
		ret = bt_ctf_clock_class_set_name(clock_class, name);
		if (ret) {
			/* bt_ctf_clock_class_set_name() logs errors */
			goto error;
		}
	}

	BT_LOGD("Created clock class object: addr=%p, name=\"%s\"",
		clock_class, name);
	return clock_class;
error:
	BT_CTF_OBJECT_PUT_REF_AND_RESET(clock_class);
	return clock_class;
}

BT_HIDDEN
const char *bt_ctf_clock_class_get_name(struct bt_ctf_clock_class *clock_class)
{
	const char *ret = NULL;

	if (!clock_class) {
		BT_LOGW_STR("Invalid parameter: clock class is NULL.");
		goto end;
	}

	if (clock_class->name) {
		ret = clock_class->name->str;
	}

end:
	return ret;
}

BT_HIDDEN
const char *bt_ctf_clock_class_get_description(
		struct bt_ctf_clock_class *clock_class)
{
	const char *ret = NULL;

	if (!clock_class) {
		BT_LOGW_STR("Invalid parameter: clock class is NULL.");
		goto end;
	}

	if (clock_class->description) {
		ret = clock_class->description->str;
	}
end:
	return ret;
}

BT_HIDDEN
int bt_ctf_clock_class_set_description(struct bt_ctf_clock_class *clock_class,
		const char *desc)
{
	int ret = 0;

	if (!clock_class || !desc) {
		BT_LOGW("Invalid parameter: clock class or description is NULL: "
			"clock-class-addr=%p, name=\"%s\", desc-addr=%p",
			clock_class, bt_ctf_clock_class_get_name(clock_class),
			desc);
		ret = -1;
		goto end;
	}

	if (clock_class->frozen) {
		BT_LOGW("Invalid parameter: clock class is frozen: addr=%p, name=\"%s\"",
			clock_class, bt_ctf_clock_class_get_name(clock_class));
		ret = -1;
		goto end;
	}

	clock_class->description = g_string_new(desc);
	ret = clock_class->description ? 0 : -1;
	BT_LOGT("Set clock class's description: addr=%p, "
		"name=\"%s\", desc=\"%s\"",
		clock_class, bt_ctf_clock_class_get_name(clock_class), desc);
end:
	return ret;
}

BT_HIDDEN
uint64_t bt_ctf_clock_class_get_frequency(
		struct bt_ctf_clock_class *clock_class)
{
	uint64_t ret = -1ULL;

	if (!clock_class) {
		BT_LOGW_STR("Invalid parameter: clock class is NULL.");
		goto end;
	}

	ret = clock_class->frequency;
end:
	return ret;
}

BT_HIDDEN
int bt_ctf_clock_class_set_frequency(struct bt_ctf_clock_class *clock_class,
		uint64_t freq)
{
	int ret = 0;

	if (!clock_class) {
		BT_LOGW("Invalid parameter: clock class is NULL or frequency is invalid: "
			"addr=%p, name=\"%s\"",
			clock_class, bt_ctf_clock_class_get_name(clock_class));
		ret = -1;
		goto end;
	}

	if (!validate_freq(clock_class, bt_ctf_clock_class_get_name(clock_class),
			freq)) {
		/* validate_freq() logs errors */
		goto end;
	}

	if (clock_class->frozen) {
		BT_LOGW("Invalid parameter: clock class is frozen: addr=%p, name=\"%s\"",
			clock_class, bt_ctf_clock_class_get_name(clock_class));
		ret = -1;
		goto end;
	}

	clock_class->frequency = freq;
	BT_LOGT("Set clock class's frequency: addr=%p, name=\"%s\", freq=%" PRIu64,
		clock_class, bt_ctf_clock_class_get_name(clock_class), freq);
end:
	return ret;
}

BT_HIDDEN
uint64_t bt_ctf_clock_class_get_precision(struct bt_ctf_clock_class *clock_class)
{
	uint64_t ret = -1ULL;

	if (!clock_class) {
		BT_LOGW_STR("Invalid parameter: clock class is NULL.");
		goto end;
	}

	ret = clock_class->precision;
end:
	return ret;
}

BT_HIDDEN
int bt_ctf_clock_class_set_precision(struct bt_ctf_clock_class *clock_class,
		uint64_t precision)
{
	int ret = 0;

	if (!clock_class || precision == -1ULL) {
		BT_LOGW("Invalid parameter: clock class is NULL or precision is invalid: "
			"addr=%p, name=\"%s\", precision=%" PRIu64,
			clock_class, bt_ctf_clock_class_get_name(clock_class),
			precision);
		ret = -1;
		goto end;
	}

	if (clock_class->frozen) {
		BT_LOGW("Invalid parameter: clock class is frozen: addr=%p, name=\"%s\"",
			clock_class, bt_ctf_clock_class_get_name(clock_class));
		ret = -1;
		goto end;
	}

	clock_class->precision = precision;
	BT_LOGT("Set clock class's precision: addr=%p, name=\"%s\", precision=%" PRIu64,
		clock_class, bt_ctf_clock_class_get_name(clock_class),
		precision);
end:
	return ret;
}

BT_HIDDEN
int bt_ctf_clock_class_get_offset_s(struct bt_ctf_clock_class *clock_class,
		int64_t *offset_s)
{
	int ret = 0;

	if (!clock_class || !offset_s) {
		BT_LOGW("Invalid parameter: clock class or offset pointer is NULL: "
			"clock-class-addr=%p, name=\"%s\", offset-addr=%p",
			clock_class, bt_ctf_clock_class_get_name(clock_class),
			offset_s);
		ret = -1;
		goto end;
	}

	*offset_s = clock_class->offset_s;
end:
	return ret;
}

BT_HIDDEN
int bt_ctf_clock_class_set_offset_s(struct bt_ctf_clock_class *clock_class,
		int64_t offset_s)
{
	int ret = 0;

	if (!clock_class) {
		BT_LOGW_STR("Invalid parameter: clock class is NULL.");
		ret = -1;
		goto end;
	}

	if (clock_class->frozen) {
		BT_LOGW("Invalid parameter: clock class is frozen: addr=%p, name=\"%s\"",
			clock_class, bt_ctf_clock_class_get_name(clock_class));
		ret = -1;
		goto end;
	}

	clock_class->offset_s = offset_s;
	BT_LOGT("Set clock class's offset (seconds): "
		"addr=%p, name=\"%s\", offset-s=%" PRId64,
		clock_class, bt_ctf_clock_class_get_name(clock_class),
		offset_s);
end:
	return ret;
}

BT_HIDDEN
int bt_ctf_clock_class_get_offset_cycles(struct bt_ctf_clock_class *clock_class,
		int64_t *offset)
{
	int ret = 0;

	if (!clock_class || !offset) {
		BT_LOGW("Invalid parameter: clock class or offset pointer is NULL: "
			"clock-class-addr=%p, name=\"%s\", offset-addr=%p",
			clock_class, bt_ctf_clock_class_get_name(clock_class),
			offset);
		ret = -1;
		goto end;
	}

	*offset = clock_class->offset;
end:
	return ret;
}

BT_HIDDEN
int bt_ctf_clock_class_set_offset_cycles(struct bt_ctf_clock_class *clock_class,
		int64_t offset)
{
	int ret = 0;

	if (!clock_class) {
		BT_LOGW_STR("Invalid parameter: clock class is NULL.");
		ret = -1;
		goto end;
	}

	if (clock_class->frozen) {
		BT_LOGW("Invalid parameter: clock class is frozen: addr=%p, name=\"%s\"",
			clock_class, bt_ctf_clock_class_get_name(clock_class));
		ret = -1;
		goto end;
	}

	clock_class->offset = offset;
	BT_LOGT("Set clock class's offset (cycles): addr=%p, name=\"%s\", offset-cycles=%" PRId64,
		clock_class, bt_ctf_clock_class_get_name(clock_class), offset);
end:
	return ret;
}

BT_HIDDEN
bt_ctf_bool bt_ctf_clock_class_is_absolute(struct bt_ctf_clock_class *clock_class)
{
	int ret = -1;

	if (!clock_class) {
		BT_LOGW_STR("Invalid parameter: clock class is NULL.");
		goto end;
	}

	ret = clock_class->absolute;
end:
	return ret;
}

BT_HIDDEN
int bt_ctf_clock_class_set_is_absolute(struct bt_ctf_clock_class *clock_class,
		bt_ctf_bool is_absolute)
{
	int ret = 0;

	if (!clock_class) {
		BT_LOGW_STR("Invalid parameter: clock class is NULL.");
		ret = -1;
		goto end;
	}

	if (clock_class->frozen) {
		BT_LOGW("Invalid parameter: clock class is frozen: addr=%p, name=\"%s\"",
			clock_class, bt_ctf_clock_class_get_name(clock_class));
		ret = -1;
		goto end;
	}

	clock_class->absolute = !!is_absolute;
	BT_LOGT("Set clock class's absolute flag: addr=%p, name=\"%s\", is-absolute=%d",
		clock_class, bt_ctf_clock_class_get_name(clock_class),
		is_absolute);
end:
	return ret;
}

BT_HIDDEN
const uint8_t *bt_ctf_clock_class_get_uuid(
		struct bt_ctf_clock_class *clock_class)
{
	const uint8_t *ret;

	if (!clock_class) {
		BT_LOGW_STR("Invalid parameter: clock class is NULL.");
		ret = NULL;
		goto end;
	}

	if (!clock_class->uuid_set) {
		BT_LOGT("Clock class's UUID is not set: addr=%p, name=\"%s\"",
			clock_class, bt_ctf_clock_class_get_name(clock_class));
		ret = NULL;
		goto end;
	}

	ret = clock_class->uuid;
end:
	return ret;
}

BT_HIDDEN
int bt_ctf_clock_class_set_uuid(struct bt_ctf_clock_class *clock_class,
		const uint8_t *uuid)
{
	int ret = 0;

	if (!clock_class || !uuid) {
		BT_LOGW("Invalid parameter: clock class or UUID is NULL: "
			"clock-class-addr=%p, name=\"%s\", uuid-addr=%p",
			clock_class, bt_ctf_clock_class_get_name(clock_class),
			uuid);
		ret = -1;
		goto end;
	}

	if (clock_class->frozen) {
		BT_LOGW("Invalid parameter: clock class is frozen: addr=%p, name=\"%s\"",
			clock_class, bt_ctf_clock_class_get_name(clock_class));
		ret = -1;
		goto end;
	}

	bt_uuid_copy(clock_class->uuid, uuid);
	clock_class->uuid_set = 1;
	BT_LOGT("Set clock class's UUID: addr=%p, name=\"%s\", uuid=\"" BT_UUID_FMT "\"",
		clock_class, bt_ctf_clock_class_get_name(clock_class),
		BT_UUID_FMT_VALUES(uuid));
end:
	return ret;
}

BT_HIDDEN
void bt_ctf_clock_class_freeze(struct bt_ctf_clock_class *clock_class)
{
	if (!clock_class || clock_class->frozen) {
		return;
	}

	BT_LOGD("Freezing clock class: addr=%p, name=\"%s\"",
		clock_class, bt_ctf_clock_class_get_name(clock_class));
	clock_class->frozen = 1;
}

static
void bt_ctf_clock_class_destroy(struct bt_ctf_object *obj)
{
	struct bt_ctf_clock_class *clock_class;

	clock_class = container_of(obj, struct bt_ctf_clock_class, base);
	BT_LOGD("Destroying clock class: addr=%p, name=\"%s\"",
		obj, bt_ctf_clock_class_get_name(clock_class));

	if (clock_class->name) {
		g_string_free(clock_class->name, TRUE);
	}

	if (clock_class->description) {
		g_string_free(clock_class->description, TRUE);
	}

	g_free(clock_class);
}

BT_HIDDEN
int bt_ctf_clock_class_compare(struct bt_ctf_clock_class *clock_class_a,
		struct bt_ctf_clock_class *clock_class_b)
{
	int ret = 1;
	BT_ASSERT_DBG(clock_class_a);
	BT_ASSERT_DBG(clock_class_b);

	/* Name */
	if (strcmp(clock_class_a->name->str, clock_class_b->name->str) != 0) {
		BT_LOGT("Clock classes differ: different names: "
			"cc-a-name=\"%s\", cc-b-name=\"%s\"",
			clock_class_a->name->str,
			clock_class_b->name->str);
		goto end;
	}

	/* Description */
	if (clock_class_a->description) {
		if (!clock_class_b->description) {
			BT_LOGT_STR("Clock classes differ: clock class A has a "
				"description, but clock class B does not.");
			goto end;
		}

		if (strcmp(clock_class_a->name->str, clock_class_b->name->str)
				!= 0) {
			BT_LOGT("Clock classes differ: different descriptions: "
				"cc-a-descr=\"%s\", cc-b-descr=\"%s\"",
				clock_class_a->description->str,
				clock_class_b->description->str);
			goto end;
		}
	} else {
		if (clock_class_b->description) {
			BT_LOGT_STR("Clock classes differ: clock class A has "
				"no description, but clock class B has one.");
			goto end;
		}
	}

	/* Frequency */
	if (clock_class_a->frequency != clock_class_b->frequency) {
		BT_LOGT("Clock classes differ: different frequencies: "
			"cc-a-freq=%" PRIu64 ", cc-b-freq=%" PRIu64,
			clock_class_a->frequency,
			clock_class_b->frequency);
		goto end;
	}

	/* Precision */
	if (clock_class_a->precision != clock_class_b->precision) {
		BT_LOGT("Clock classes differ: different precisions: "
			"cc-a-freq=%" PRIu64 ", cc-b-freq=%" PRIu64,
			clock_class_a->precision,
			clock_class_b->precision);
		goto end;
	}

	/* Offset (seconds) */
	if (clock_class_a->offset_s != clock_class_b->offset_s) {
		BT_LOGT("Clock classes differ: different offsets (seconds): "
			"cc-a-offset-s=%" PRId64 ", cc-b-offset-s=%" PRId64,
			clock_class_a->offset_s,
			clock_class_b->offset_s);
		goto end;
	}

	/* Offset (cycles) */
	if (clock_class_a->offset != clock_class_b->offset) {
		BT_LOGT("Clock classes differ: different offsets (cycles): "
			"cc-a-offset-s=%" PRId64 ", cc-b-offset-s=%" PRId64,
			clock_class_a->offset,
			clock_class_b->offset);
		goto end;
	}

	/* UUIDs */
	if (clock_class_a->uuid_set) {
		if (!clock_class_b->uuid_set) {
			BT_LOGT_STR("Clock classes differ: clock class A has a "
				"UUID, but clock class B does not.");
			goto end;
		}

		if (bt_uuid_compare(clock_class_a->uuid, clock_class_b->uuid) != 0) {
			BT_LOGT("Clock classes differ: different UUIDs: "
				"cc-a-uuid=\"" BT_UUID_FMT "\", "
				"cc-b-uuid=\"" BT_UUID_FMT "\"",
				BT_UUID_FMT_VALUES(clock_class_a->uuid),
				BT_UUID_FMT_VALUES(clock_class_b->uuid));
			goto end;
		}
	} else {
		if (clock_class_b->uuid_set) {
			BT_LOGT_STR("Clock classes differ: clock class A has "
				"no UUID, but clock class B has one.");
			goto end;
		}
	}

	/* Absolute */
	if (!!clock_class_a->absolute != !!clock_class_b->absolute) {
		BT_LOGT("Clock classes differ: one is absolute, the other "
			"is not: cc-a-is-absolute=%d, cc-b-is-absolute=%d",
			!!clock_class_a->absolute,
			!!clock_class_b->absolute);
		goto end;
	}

	/* Equal */
	ret = 0;

end:
	return ret;
}
