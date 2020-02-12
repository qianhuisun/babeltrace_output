/*
 * validation.c
 *
 * Babeltrace - CTF writer: Validation of trace, stream class, and event class
 *
 * Copyright 2016-2018 Philippe Proulx <pproulx@efficios.com>
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

#define BT_LOG_TAG "CTF-WRITER/VALIDATION"
#include "logging.h"

#include <babeltrace2-ctf-writer/object.h>

#include "common/macros.h"

#include "assert-pre.h"
#include "event-class.h"
#include "field-types.h"
#include "field-types.h"
#include "resolve.h"
#include "stream-class.h"
#include "trace.h"
#include "validation.h"
#include "values.h"

/*
 * This function resolves and validates the field types of an event
 * class. Only `event_context_type` and `event_payload_type` are
 * resolved and validated; the other field types are used as eventual
 * resolving targets.
 *
 * All parameters are owned by the caller.
 */
static
int validate_event_class_types(struct bt_ctf_private_value *environment,
		struct bt_ctf_field_type_common *packet_header_type,
		struct bt_ctf_field_type_common *packet_context_type,
		struct bt_ctf_field_type_common *event_header_type,
		struct bt_ctf_field_type_common *stream_event_ctx_type,
		struct bt_ctf_field_type_common *event_context_type,
		struct bt_ctf_field_type_common *event_payload_type)
{
	int ret = 0;

	BT_LOGT("Validating event class field types: "
		"packet-header-ft-addr=%p, "
		"packet-context-ft-addr=%p, "
		"event-header-ft-addr=%p, "
		"stream-event-context-ft-addr=%p, "
		"event-context-ft-addr=%p, "
		"event-payload-ft-addr=%p",
		packet_header_type, packet_context_type, event_header_type,
		stream_event_ctx_type, event_context_type, event_payload_type);

	/* Resolve sequence type lengths and variant type tags first */
	ret = bt_ctf_resolve_types(environment, packet_header_type,
		packet_context_type, event_header_type, stream_event_ctx_type,
		event_context_type, event_payload_type,
		BT_CTF_RESOLVE_FLAG_EVENT_CONTEXT |
		BT_CTF_RESOLVE_FLAG_EVENT_PAYLOAD);
	if (ret) {
		BT_LOGW("Cannot resolve event class field types: ret=%d",
			ret);
		goto end;
	}

	/* Validate field types individually */
	if (event_context_type) {
		ret = bt_ctf_field_type_common_validate(event_context_type);
		if (ret) {
			BT_LOGW("Invalid event class's context field type: "
				"ret=%d", ret);
			goto end;
		}
	}

	if (event_payload_type) {
		ret = bt_ctf_field_type_common_validate(event_payload_type);
		if (ret) {
			BT_LOGW("Invalid event class's payload field type: "
				"ret=%d", ret);
			goto end;
		}
	}

end:
	return ret;
}

/*
 * This function resolves and validates the field types of a stream
 * class. Only `packet_context_type`, `event_header_type`, and
 * `stream_event_ctx_type` are resolved and validated; the other field
 * type is used as an eventual resolving target.
 *
 * All parameters are owned by the caller.
 */
static
int validate_stream_class_types(struct bt_ctf_private_value *environment,
		struct bt_ctf_field_type_common *packet_header_type,
		struct bt_ctf_field_type_common *packet_context_type,
		struct bt_ctf_field_type_common *event_header_type,
		struct bt_ctf_field_type_common *stream_event_ctx_type)
{
	int ret = 0;

	BT_LOGT("Validating stream class field types: "
		"packet-header-ft-addr=%p, "
		"packet-context-ft-addr=%p, "
		"event-header-ft-addr=%p, "
		"stream-event-context-ft-addr=%p",
		packet_header_type, packet_context_type, event_header_type,
		stream_event_ctx_type);

	/* Resolve sequence type lengths and variant type tags first */
	ret = bt_ctf_resolve_types(environment, packet_header_type,
		packet_context_type, event_header_type, stream_event_ctx_type,
		NULL, NULL,
		BT_CTF_RESOLVE_FLAG_PACKET_CONTEXT |
		BT_CTF_RESOLVE_FLAG_EVENT_HEADER |
		BT_CTF_RESOLVE_FLAG_STREAM_EVENT_CTX);
	if (ret) {
		BT_LOGW("Cannot resolve stream class field types: ret=%d",
			ret);
		goto end;
	}

	/* Validate field types individually */
	if (packet_context_type) {
		ret = bt_ctf_field_type_common_validate(packet_context_type);
		if (ret) {
			BT_LOGW("Invalid stream class's packet context field type: "
				"ret=%d", ret);
			goto end;
		}
	}

	if (event_header_type) {
		ret = bt_ctf_field_type_common_validate(event_header_type);
		if (ret) {
			BT_LOGW("Invalid stream class's event header field type: "
				"ret=%d", ret);
			goto end;
		}
	}

	if (stream_event_ctx_type) {
		ret = bt_ctf_field_type_common_validate(
			stream_event_ctx_type);
		if (ret) {
			BT_LOGW("Invalid stream class's event context field type: "
				"ret=%d", ret);
			goto end;
		}
	}

end:
	return ret;
}

/*
 * This function resolves and validates the field types of a trace.
 *
 * All parameters are owned by the caller.
 */
static
int validate_trace_types(struct bt_ctf_private_value *environment,
		struct bt_ctf_field_type_common *packet_header_type)
{
	int ret = 0;

	BT_LOGT("Validating event class field types: "
		"packet-header-ft-addr=%p", packet_header_type);

	/* Resolve sequence type lengths and variant type tags first */
	ret = bt_ctf_resolve_types(environment, packet_header_type,
		NULL, NULL, NULL, NULL, NULL,
		BT_CTF_RESOLVE_FLAG_PACKET_HEADER);
	if (ret) {
		BT_LOGW("Cannot resolve trace field types: ret=%d",
			ret);
		goto end;
	}

	/* Validate field types individually */
	if (packet_header_type) {
		ret = bt_ctf_field_type_common_validate(packet_header_type);
		if (ret) {
			BT_LOGW("Invalid trace's packet header field type: "
				"ret=%d", ret);
			goto end;
		}
	}

end:
	return ret;
}

/*
 * Checks whether or not `field_type` contains a variant or a sequence
 * field type, recursively. Returns 1 if it's the case.
 *
 * `field_type` is owned by the caller.
 */
static
int field_type_contains_sequence_or_variant_ft(struct bt_ctf_field_type_common *type)
{
	int ret = 0;
	enum bt_ctf_field_type_id type_id = bt_ctf_field_type_common_get_type_id(type);

	switch (type_id) {
	case BT_CTF_FIELD_TYPE_ID_SEQUENCE:
	case BT_CTF_FIELD_TYPE_ID_VARIANT:
		ret = 1;
		goto end;
	case BT_CTF_FIELD_TYPE_ID_ARRAY:
	case BT_CTF_FIELD_TYPE_ID_STRUCT:
	{
		int i;
		int field_count = bt_ctf_field_type_common_get_field_count(type);

		if (field_count < 0) {
			ret = -1;
			goto end;
		}

		for (i = 0; i < field_count; ++i) {
			struct bt_ctf_field_type_common *child_type =
				bt_ctf_field_type_common_borrow_field_at_index(
					type, i);

			ret = field_type_contains_sequence_or_variant_ft(
				child_type);
			if (ret != 0) {
				goto end;
			}
		}
		break;
	}
	default:
		break;
	}

end:
	return ret;
}

BT_HIDDEN
int bt_ctf_validate_class_types(struct bt_ctf_private_value *environment,
		struct bt_ctf_field_type_common *packet_header_type,
		struct bt_ctf_field_type_common *packet_context_type,
		struct bt_ctf_field_type_common *event_header_type,
		struct bt_ctf_field_type_common *stream_event_ctx_type,
		struct bt_ctf_field_type_common *event_context_type,
		struct bt_ctf_field_type_common *event_payload_type,
		int trace_valid, int stream_class_valid, int event_class_valid,
		struct bt_ctf_validation_output *output,
		enum bt_ctf_validation_flag validate_flags,
		bt_ctf_validation_flag_copy_field_type_func copy_field_type_func)
{
	int ret = 0;
	int contains_seq_var;
	int valid_ret;

	BT_LOGT("Validating field types: "
		"packet-header-ft-addr=%p, "
		"packet-context-ft-addr=%p, "
		"event-header-ft-addr=%p, "
		"stream-event-context-ft-addr=%p, "
		"event-context-ft-addr=%p, "
		"event-payload-ft-addr=%p, "
		"trace-is-valid=%d, stream-class-is-valid=%d, "
		"event-class-is-valid=%d, validation-flags=%x",
		packet_header_type, packet_context_type, event_header_type,
		stream_event_ctx_type, event_context_type, event_payload_type,
		trace_valid, stream_class_valid, event_class_valid,
		(unsigned int) validate_flags);

	/* Clean output values */
	memset(output, 0, sizeof(*output));

	/* Set initial valid flags according to valid parameters */
	if (trace_valid) {
		output->valid_flags |= BT_CTF_VALIDATION_FLAG_TRACE;
	}

	if (stream_class_valid) {
		output->valid_flags |= BT_CTF_VALIDATION_FLAG_STREAM;
	}

	if (event_class_valid) {
		output->valid_flags |= BT_CTF_VALIDATION_FLAG_EVENT;
	}

	/* Own the type parameters */
	bt_ctf_object_get_ref(packet_header_type);
	bt_ctf_object_get_ref(packet_context_type);
	bt_ctf_object_get_ref(event_header_type);
	bt_ctf_object_get_ref(stream_event_ctx_type);
	bt_ctf_object_get_ref(event_context_type);
	bt_ctf_object_get_ref(event_payload_type);

	/* Validate trace */
	if ((validate_flags & BT_CTF_VALIDATION_FLAG_TRACE) && !trace_valid) {
		struct bt_ctf_field_type_common *packet_header_type_copy = NULL;

		/* Create field type copies */
		if (packet_header_type) {
			contains_seq_var =
				field_type_contains_sequence_or_variant_ft(
					packet_header_type);
			if (contains_seq_var < 0) {
				ret = contains_seq_var;
				goto error;
			} else if (!contains_seq_var) {
				/* No copy is needed */
				packet_header_type_copy = packet_header_type;
				bt_ctf_object_get_ref(packet_header_type_copy);
				goto skip_packet_header_type_copy;
			}

			BT_LOGT_STR("Copying packet header field type because it contains at least one sequence or variant field type.");
			packet_header_type_copy =
				copy_field_type_func(packet_header_type);
			if (!packet_header_type_copy) {
				ret = -1;
				BT_LOGE_STR("Cannot copy packet header field type.");
				goto error;
			}

			/*
			 * Freeze this copy: if it's returned to the
			 * caller, it cannot be modified any way since
			 * it will be resolved.
			 */
			bt_ctf_field_type_common_freeze(packet_header_type_copy);
		}

skip_packet_header_type_copy:
		/* Put original reference and move copy */
		BT_CTF_OBJECT_MOVE_REF(packet_header_type, packet_header_type_copy);

		/* Validate trace field types */
		valid_ret = validate_trace_types(environment,
			packet_header_type);
		if (valid_ret == 0) {
			/* Trace is valid */
			output->valid_flags |= BT_CTF_VALIDATION_FLAG_TRACE;
		}
	}

	/* Validate stream class */
	if ((validate_flags & BT_CTF_VALIDATION_FLAG_STREAM) &&
			!stream_class_valid) {
		struct bt_ctf_field_type_common *packet_context_type_copy = NULL;
		struct bt_ctf_field_type_common *event_header_type_copy = NULL;
		struct bt_ctf_field_type_common *stream_event_ctx_type_copy = NULL;

		if (packet_context_type) {
			contains_seq_var =
				field_type_contains_sequence_or_variant_ft(
					packet_context_type);
			if (contains_seq_var < 0) {
				ret = contains_seq_var;
				goto error;
			} else if (!contains_seq_var) {
				/* No copy is needed */
				packet_context_type_copy = packet_context_type;
				bt_ctf_object_get_ref(packet_context_type_copy);
				goto skip_packet_context_type_copy;
			}

			BT_LOGT_STR("Copying packet context field type because it contains at least one sequence or variant field type.");
			packet_context_type_copy =
				copy_field_type_func(packet_context_type);
			if (!packet_context_type_copy) {
				BT_LOGE_STR("Cannot copy packet context field type.");
				goto sc_validation_error;
			}

			/*
			 * Freeze this copy: if it's returned to the
			 * caller, it cannot be modified any way since
			 * it will be resolved.
			 */
			bt_ctf_field_type_common_freeze(packet_context_type_copy);
		}

skip_packet_context_type_copy:
		if (event_header_type) {
			contains_seq_var =
				field_type_contains_sequence_or_variant_ft(
					event_header_type);
			if (contains_seq_var < 0) {
				ret = contains_seq_var;
				goto error;
			} else if (!contains_seq_var) {
				/* No copy is needed */
				event_header_type_copy = event_header_type;
				bt_ctf_object_get_ref(event_header_type_copy);
				goto skip_event_header_type_copy;
			}

			BT_LOGT_STR("Copying event header field type because it contains at least one sequence or variant field type.");
			event_header_type_copy =
				copy_field_type_func(event_header_type);
			if (!event_header_type_copy) {
				BT_LOGE_STR("Cannot copy event header field type.");
				goto sc_validation_error;
			}

			/*
			 * Freeze this copy: if it's returned to the
			 * caller, it cannot be modified any way since
			 * it will be resolved.
			 */
			bt_ctf_field_type_common_freeze(event_header_type_copy);
		}

skip_event_header_type_copy:
		if (stream_event_ctx_type) {
			contains_seq_var =
				field_type_contains_sequence_or_variant_ft(
					stream_event_ctx_type);
			if (contains_seq_var < 0) {
				ret = contains_seq_var;
				goto error;
			} else if (!contains_seq_var) {
				/* No copy is needed */
				stream_event_ctx_type_copy =
					stream_event_ctx_type;
				bt_ctf_object_get_ref(stream_event_ctx_type_copy);
				goto skip_stream_event_ctx_type_copy;
			}

			BT_LOGT_STR("Copying stream event context field type because it contains at least one sequence or variant field type.");
			stream_event_ctx_type_copy =
				copy_field_type_func(stream_event_ctx_type);
			if (!stream_event_ctx_type_copy) {
				BT_LOGE_STR("Cannot copy stream event context field type.");
				goto sc_validation_error;
			}

			/*
			 * Freeze this copy: if it's returned to the
			 * caller, it cannot be modified any way since
			 * it will be resolved.
			 */
			bt_ctf_field_type_common_freeze(stream_event_ctx_type_copy);
		}

skip_stream_event_ctx_type_copy:
		/* Put original references and move copies */
		BT_CTF_OBJECT_MOVE_REF(packet_context_type, packet_context_type_copy);
		BT_CTF_OBJECT_MOVE_REF(event_header_type, event_header_type_copy);
		BT_CTF_OBJECT_MOVE_REF(stream_event_ctx_type, stream_event_ctx_type_copy);

		/* Validate stream class field types */
		valid_ret = validate_stream_class_types(environment,
			packet_header_type, packet_context_type,
			event_header_type, stream_event_ctx_type);
		if (valid_ret == 0) {
			/* Stream class is valid */
			output->valid_flags |= BT_CTF_VALIDATION_FLAG_STREAM;
		}

		goto sc_validation_done;

sc_validation_error:
		BT_CTF_OBJECT_PUT_REF_AND_RESET(packet_context_type_copy);
		BT_CTF_OBJECT_PUT_REF_AND_RESET(event_header_type_copy);
		BT_CTF_OBJECT_PUT_REF_AND_RESET(stream_event_ctx_type_copy);
		ret = -1;
		goto error;
	}

sc_validation_done:
	/* Validate event class */
	if ((validate_flags & BT_CTF_VALIDATION_FLAG_EVENT) &&
			!event_class_valid) {
		struct bt_ctf_field_type_common *event_context_type_copy = NULL;
		struct bt_ctf_field_type_common *event_payload_type_copy = NULL;

		if (event_context_type) {
			contains_seq_var =
				field_type_contains_sequence_or_variant_ft(
					event_context_type);
			if (contains_seq_var < 0) {
				ret = contains_seq_var;
				goto error;
			} else if (!contains_seq_var) {
				/* No copy is needed */
				event_context_type_copy = event_context_type;
				bt_ctf_object_get_ref(event_context_type_copy);
				goto skip_event_context_type_copy;
			}

			BT_LOGT_STR("Copying event context field type because it contains at least one sequence or variant field type.");
			event_context_type_copy =
				copy_field_type_func(event_context_type);
			if (!event_context_type_copy) {
				BT_LOGE_STR("Cannot copy event context field type.");
				goto ec_validation_error;
			}

			/*
			 * Freeze this copy: if it's returned to the
			 * caller, it cannot be modified any way since
			 * it will be resolved.
			 */
			bt_ctf_field_type_common_freeze(event_context_type_copy);
		}

skip_event_context_type_copy:
		if (event_payload_type) {
			contains_seq_var =
				field_type_contains_sequence_or_variant_ft(
					event_payload_type);
			if (contains_seq_var < 0) {
				ret = contains_seq_var;
				goto error;
			} else if (!contains_seq_var) {
				/* No copy is needed */
				event_payload_type_copy = event_payload_type;
				bt_ctf_object_get_ref(event_payload_type_copy);
				goto skip_event_payload_type_copy;
			}

			BT_LOGT_STR("Copying event payload field type because it contains at least one sequence or variant field type.");
			event_payload_type_copy =
				copy_field_type_func(event_payload_type);
			if (!event_payload_type_copy) {
				BT_LOGE_STR("Cannot copy event payload field type.");
				goto ec_validation_error;
			}

			/*
			 * Freeze this copy: if it's returned to the
			 * caller, it cannot be modified any way since
			 * it will be resolved.
			 */
			bt_ctf_field_type_common_freeze(event_payload_type_copy);
		}

skip_event_payload_type_copy:
		/* Put original references and move copies */
		BT_CTF_OBJECT_MOVE_REF(event_context_type, event_context_type_copy);
		BT_CTF_OBJECT_MOVE_REF(event_payload_type, event_payload_type_copy);

		/* Validate event class field types */
		valid_ret = validate_event_class_types(environment,
			packet_header_type, packet_context_type,
			event_header_type, stream_event_ctx_type,
			event_context_type, event_payload_type);
		if (valid_ret == 0) {
			/* Event class is valid */
			output->valid_flags |= BT_CTF_VALIDATION_FLAG_EVENT;
		}

		goto ec_validation_done;

ec_validation_error:
		BT_CTF_OBJECT_PUT_REF_AND_RESET(event_context_type_copy);
		BT_CTF_OBJECT_PUT_REF_AND_RESET(event_payload_type_copy);
		ret = -1;
		goto error;
	}

ec_validation_done:
	/*
	 * Validation is complete. Move the field types that were used
	 * to validate (and that were possibly altered by the validation
	 * process) to the output values.
	 */
	BT_CTF_OBJECT_MOVE_REF(output->packet_header_type, packet_header_type);
	BT_CTF_OBJECT_MOVE_REF(output->packet_context_type, packet_context_type);
	BT_CTF_OBJECT_MOVE_REF(output->event_header_type, event_header_type);
	BT_CTF_OBJECT_MOVE_REF(output->stream_event_ctx_type, stream_event_ctx_type);
	BT_CTF_OBJECT_MOVE_REF(output->event_context_type, event_context_type);
	BT_CTF_OBJECT_MOVE_REF(output->event_payload_type, event_payload_type);
	return ret;

error:
	BT_CTF_OBJECT_PUT_REF_AND_RESET(packet_header_type);
	BT_CTF_OBJECT_PUT_REF_AND_RESET(packet_context_type);
	BT_CTF_OBJECT_PUT_REF_AND_RESET(event_header_type);
	BT_CTF_OBJECT_PUT_REF_AND_RESET(stream_event_ctx_type);
	BT_CTF_OBJECT_PUT_REF_AND_RESET(event_context_type);
	BT_CTF_OBJECT_PUT_REF_AND_RESET(event_payload_type);
	return ret;
}

BT_HIDDEN
void bt_ctf_validation_replace_types(struct bt_ctf_trace_common *trace,
		struct bt_ctf_stream_class_common *stream_class,
		struct bt_ctf_event_class_common *event_class,
		struct bt_ctf_validation_output *output,
		enum bt_ctf_validation_flag replace_flags)
{
	if ((replace_flags & BT_CTF_VALIDATION_FLAG_TRACE) && trace) {
		bt_ctf_field_type_common_freeze(trace->packet_header_field_type);
		BT_CTF_OBJECT_MOVE_REF(trace->packet_header_field_type,
			output->packet_header_type);
	}

	if ((replace_flags & BT_CTF_VALIDATION_FLAG_STREAM) && stream_class) {
		bt_ctf_field_type_common_freeze(stream_class->packet_context_field_type);
		bt_ctf_field_type_common_freeze(stream_class->event_header_field_type);
		bt_ctf_field_type_common_freeze(stream_class->event_context_field_type);
		BT_CTF_OBJECT_MOVE_REF(stream_class->packet_context_field_type,
			output->packet_context_type);
		BT_CTF_OBJECT_MOVE_REF(stream_class->event_header_field_type,
			output->event_header_type);
		BT_CTF_OBJECT_MOVE_REF(stream_class->event_context_field_type,
			output->stream_event_ctx_type);
	}

	if ((replace_flags & BT_CTF_VALIDATION_FLAG_EVENT) && event_class) {
		bt_ctf_field_type_common_freeze(event_class->context_field_type);
		bt_ctf_field_type_common_freeze(event_class->payload_field_type);
		BT_CTF_OBJECT_MOVE_REF(event_class->context_field_type, output->event_context_type);
		BT_CTF_OBJECT_MOVE_REF(event_class->payload_field_type, output->event_payload_type);
	}
}

BT_HIDDEN
void bt_ctf_validation_output_put_types(
		struct bt_ctf_validation_output *output)
{
	BT_CTF_OBJECT_PUT_REF_AND_RESET(output->packet_header_type);
	BT_CTF_OBJECT_PUT_REF_AND_RESET(output->packet_context_type);
	BT_CTF_OBJECT_PUT_REF_AND_RESET(output->event_header_type);
	BT_CTF_OBJECT_PUT_REF_AND_RESET(output->stream_event_ctx_type);
	BT_CTF_OBJECT_PUT_REF_AND_RESET(output->event_context_type);
	BT_CTF_OBJECT_PUT_REF_AND_RESET(output->event_payload_type);
}
