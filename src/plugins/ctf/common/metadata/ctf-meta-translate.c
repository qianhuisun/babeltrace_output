/*
 * Copyright 2018 - Philippe Proulx <pproulx@efficios.com>
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
 */

#include <babeltrace2/babeltrace.h>
#include "common/macros.h"
#include "common/assert.h"
#include <glib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <inttypes.h>

#include "ctf-meta-visitors.h"

struct ctx {
	bt_self_component *self_comp;
	bt_trace_class *ir_tc;
	bt_stream_class *ir_sc;
	struct ctf_trace_class *tc;
	struct ctf_stream_class *sc;
	struct ctf_event_class *ec;
	enum ctf_scope scope;
};

static inline
bt_field_class *ctf_field_class_to_ir(struct ctx *ctx,
		struct ctf_field_class *fc);

static inline
void ctf_field_class_int_set_props(struct ctf_field_class_int *fc,
		bt_field_class *ir_fc)
{
	bt_field_class_integer_set_field_value_range(ir_fc,
		fc->base.size);
	bt_field_class_integer_set_preferred_display_base(ir_fc,
		fc->disp_base);
}

static inline
bt_field_class *ctf_field_class_int_to_ir(struct ctx *ctx,
		struct ctf_field_class_int *fc)
{
	bt_field_class *ir_fc;

	if (fc->is_signed) {
		ir_fc = bt_field_class_integer_signed_create(ctx->ir_tc);
	} else {
		ir_fc = bt_field_class_integer_unsigned_create(ctx->ir_tc);
	}

	BT_ASSERT(ir_fc);
	ctf_field_class_int_set_props(fc, ir_fc);
	return ir_fc;
}

static inline
bt_field_class *ctf_field_class_enum_to_ir(struct ctx *ctx,
		struct ctf_field_class_enum *fc)
{
	int ret;
	bt_field_class *ir_fc;
	uint64_t i;

	if (fc->base.is_signed) {
		ir_fc = bt_field_class_enumeration_signed_create(ctx->ir_tc);
	} else {
		ir_fc = bt_field_class_enumeration_unsigned_create(ctx->ir_tc);
	}

	BT_ASSERT(ir_fc);
	ctf_field_class_int_set_props((void *) fc, ir_fc);

	for (i = 0; i < fc->mappings->len; i++) {
		struct ctf_field_class_enum_mapping *mapping =
			ctf_field_class_enum_borrow_mapping_by_index(fc, i);
		void *range_set;
		uint64_t range_i;

		if (fc->base.is_signed) {
			range_set = bt_integer_range_set_signed_create();
		} else {
			range_set = bt_integer_range_set_unsigned_create();
		}

		BT_ASSERT(range_set);

		for (range_i = 0; range_i < mapping->ranges->len; range_i++) {
			struct ctf_range *range =
				ctf_field_class_enum_mapping_borrow_range_by_index(
					mapping, range_i);

			if (fc->base.is_signed) {
				ret = bt_integer_range_set_signed_add_range(
					range_set, range->lower.i,
					range->upper.i);
			} else {
				ret = bt_integer_range_set_unsigned_add_range(
					range_set, range->lower.u,
					range->upper.u);
			}

			BT_ASSERT(ret == 0);
		}

		if (fc->base.is_signed) {
			ret = bt_field_class_enumeration_signed_add_mapping(
				ir_fc, mapping->label->str, range_set);
			BT_INTEGER_RANGE_SET_SIGNED_PUT_REF_AND_RESET(range_set);
		} else {
			ret = bt_field_class_enumeration_unsigned_add_mapping(
				ir_fc, mapping->label->str, range_set);
			BT_INTEGER_RANGE_SET_UNSIGNED_PUT_REF_AND_RESET(range_set);
		}

		BT_ASSERT(ret == 0);
	}

	return ir_fc;
}

static inline
bt_field_class *ctf_field_class_float_to_ir(struct ctx *ctx,
		struct ctf_field_class_float *fc)
{
	bt_field_class *ir_fc;

	if (fc->base.size == 32) {
		ir_fc = bt_field_class_real_single_precision_create(ctx->ir_tc);
	} else {
		ir_fc = bt_field_class_real_double_precision_create(ctx->ir_tc);
	}
	BT_ASSERT(ir_fc);

	return ir_fc;
}

static inline
bt_field_class *ctf_field_class_string_to_ir(struct ctx *ctx,
		struct ctf_field_class_string *fc)
{
	bt_field_class *ir_fc = bt_field_class_string_create(ctx->ir_tc);

	BT_ASSERT(ir_fc);
	return ir_fc;
}

static inline
void translate_struct_field_class_members(struct ctx *ctx,
		struct ctf_field_class_struct *fc, bt_field_class *ir_fc,
		bool with_header_prefix,
		struct ctf_field_class_struct *context_fc)
{
	uint64_t i;
	int ret;

	for (i = 0; i < fc->members->len; i++) {
		struct ctf_named_field_class *named_fc =
			ctf_field_class_struct_borrow_member_by_index(fc, i);
		bt_field_class *member_ir_fc;
		const char *name = named_fc->name->str;

		if (!named_fc->fc->in_ir) {
			continue;
		}

		member_ir_fc = ctf_field_class_to_ir(ctx, named_fc->fc);
		BT_ASSERT(member_ir_fc);
		ret = bt_field_class_structure_append_member(ir_fc, name,
			member_ir_fc);
		BT_ASSERT(ret == 0);
		bt_field_class_put_ref(member_ir_fc);
	}
}

static inline
bt_field_class *ctf_field_class_struct_to_ir(struct ctx *ctx,
		struct ctf_field_class_struct *fc)
{
	bt_field_class *ir_fc = bt_field_class_structure_create(ctx->ir_tc);

	BT_ASSERT(ir_fc);
	translate_struct_field_class_members(ctx, fc, ir_fc, false, NULL);
	return ir_fc;
}

static inline
bt_field_class *borrow_ir_fc_from_field_path(struct ctx *ctx,
		struct ctf_field_path *field_path)
{
	bt_field_class *ir_fc = NULL;
	struct ctf_field_class *fc = ctf_field_path_borrow_field_class(
		field_path, ctx->tc, ctx->sc, ctx->ec);

	BT_ASSERT(fc);

	if (fc->in_ir) {
		ir_fc = fc->ir_fc;
	}

	return ir_fc;
}

static inline
const void *find_ir_enum_field_class_mapping_by_label(const bt_field_class *fc,
		const char *label, bool is_signed)
{
	const void *mapping = NULL;
	uint64_t i;

	for (i = 0; i < bt_field_class_enumeration_get_mapping_count(fc); i++) {
		const bt_field_class_enumeration_mapping *this_mapping;
		const void *spec_this_mapping;

		if (is_signed) {
			spec_this_mapping =
				bt_field_class_enumeration_signed_borrow_mapping_by_index_const(
					fc, i);
			this_mapping =
				bt_field_class_enumeration_signed_mapping_as_mapping_const(
					spec_this_mapping);
		} else {
			spec_this_mapping =
				bt_field_class_enumeration_unsigned_borrow_mapping_by_index_const(
					fc, i);
			this_mapping =
				bt_field_class_enumeration_unsigned_mapping_as_mapping_const(
					spec_this_mapping);
		}

		BT_ASSERT(this_mapping);
		BT_ASSERT(spec_this_mapping);

		if (strcmp(bt_field_class_enumeration_mapping_get_label(
				this_mapping), label) == 0) {
			mapping = spec_this_mapping;
			goto end;
		}
	}

end:
	return mapping;
}

static inline
bt_field_class *ctf_field_class_variant_to_ir(struct ctx *ctx,
		struct ctf_field_class_variant *fc)
{
	int ret;
	bt_field_class *ir_fc;
	uint64_t i;
	bt_field_class *ir_tag_fc = NULL;

	if (fc->tag_path.root != CTF_SCOPE_PACKET_HEADER &&
			fc->tag_path.root != CTF_SCOPE_EVENT_HEADER) {
		ir_tag_fc = borrow_ir_fc_from_field_path(ctx, &fc->tag_path);
		BT_ASSERT(ir_tag_fc);
	}

	ir_fc = bt_field_class_variant_create(ctx->ir_tc, ir_tag_fc);
	BT_ASSERT(ir_fc);

	for (i = 0; i < fc->options->len; i++) {
		struct ctf_named_field_class *named_fc =
			ctf_field_class_variant_borrow_option_by_index(fc, i);
		bt_field_class *option_ir_fc;

		BT_ASSERT(named_fc->fc->in_ir);
		option_ir_fc = ctf_field_class_to_ir(ctx, named_fc->fc);
		BT_ASSERT(option_ir_fc);

		if (ir_tag_fc) {
			/*
			 * At this point the trace IR selector
			 * (enumeration) field class already exists if
			 * the variant is tagged (`ir_tag_fc`). This one
			 * already contains range sets for its mappings,
			 * so we just reuse the same, finding them by
			 * matching a variant field class's option's
			 * _original_ name (with a leading underscore,
			 * possibly) with a selector field class's
			 * mapping name.
			 */
			if (fc->tag_fc->base.is_signed) {
				const bt_field_class_enumeration_signed_mapping *mapping =
					find_ir_enum_field_class_mapping_by_label(
						ir_tag_fc,
						named_fc->orig_name->str, true);
				const bt_integer_range_set_signed *range_set;

				BT_ASSERT(mapping);
				range_set =
					bt_field_class_enumeration_signed_mapping_borrow_ranges_const(
						mapping);
				BT_ASSERT(range_set);
				ret = bt_field_class_variant_with_selector_field_integer_signed_append_option(
					ir_fc, named_fc->name->str,
					option_ir_fc, range_set);
			} else {
				const bt_field_class_enumeration_unsigned_mapping *mapping =
					find_ir_enum_field_class_mapping_by_label(
						ir_tag_fc,
						named_fc->orig_name->str,
						false);
				const bt_integer_range_set_unsigned *range_set;

				BT_ASSERT(mapping);
				range_set =
					bt_field_class_enumeration_unsigned_mapping_borrow_ranges_const(
						mapping);
				BT_ASSERT(range_set);
				ret = bt_field_class_variant_with_selector_field_integer_unsigned_append_option(
					ir_fc, named_fc->name->str,
					option_ir_fc, range_set);
			}
		} else {
			ret = bt_field_class_variant_without_selector_append_option(
				ir_fc, named_fc->name->str, option_ir_fc);
		}

		BT_ASSERT(ret == 0);
		bt_field_class_put_ref(option_ir_fc);
	}

	return ir_fc;
}

static inline
bt_field_class *ctf_field_class_array_to_ir(struct ctx *ctx,
		struct ctf_field_class_array *fc)
{
	bt_field_class *ir_fc;
	bt_field_class *elem_ir_fc;

	if (fc->base.is_text) {
		ir_fc = bt_field_class_string_create(ctx->ir_tc);
		BT_ASSERT(ir_fc);
		goto end;
	}

	elem_ir_fc = ctf_field_class_to_ir(ctx, fc->base.elem_fc);
	BT_ASSERT(elem_ir_fc);
	ir_fc = bt_field_class_array_static_create(ctx->ir_tc, elem_ir_fc,
		fc->length);
	BT_ASSERT(ir_fc);
	bt_field_class_put_ref(elem_ir_fc);

end:
	return ir_fc;
}

static inline
bt_field_class *ctf_field_class_sequence_to_ir(struct ctx *ctx,
		struct ctf_field_class_sequence *fc)
{
	bt_field_class *ir_fc;
	bt_field_class *elem_ir_fc;
	bt_field_class *length_fc = NULL;

	if (fc->base.is_text) {
		ir_fc = bt_field_class_string_create(ctx->ir_tc);
		BT_ASSERT(ir_fc);
		goto end;
	}

	elem_ir_fc = ctf_field_class_to_ir(ctx, fc->base.elem_fc);
	BT_ASSERT(elem_ir_fc);

	if (fc->length_path.root != CTF_SCOPE_PACKET_HEADER &&
			fc->length_path.root != CTF_SCOPE_EVENT_HEADER) {
		length_fc = borrow_ir_fc_from_field_path(ctx, &fc->length_path);
		BT_ASSERT(length_fc);
	}

	ir_fc = bt_field_class_array_dynamic_create(ctx->ir_tc, elem_ir_fc,
		length_fc);
	BT_ASSERT(ir_fc);
	bt_field_class_put_ref(elem_ir_fc);
	BT_ASSERT(ir_fc);

end:
	return ir_fc;
}

static inline
bt_field_class *ctf_field_class_to_ir(struct ctx *ctx,
		struct ctf_field_class *fc)
{
	bt_field_class *ir_fc = NULL;

	BT_ASSERT(fc);
	BT_ASSERT(fc->in_ir);

	switch (fc->type) {
	case CTF_FIELD_CLASS_TYPE_INT:
		ir_fc = ctf_field_class_int_to_ir(ctx, (void *) fc);
		break;
	case CTF_FIELD_CLASS_TYPE_ENUM:
		ir_fc = ctf_field_class_enum_to_ir(ctx, (void *) fc);
		break;
	case CTF_FIELD_CLASS_TYPE_FLOAT:
		ir_fc = ctf_field_class_float_to_ir(ctx, (void *) fc);
		break;
	case CTF_FIELD_CLASS_TYPE_STRING:
		ir_fc = ctf_field_class_string_to_ir(ctx, (void *) fc);
		break;
	case CTF_FIELD_CLASS_TYPE_STRUCT:
		ir_fc = ctf_field_class_struct_to_ir(ctx, (void *) fc);
		break;
	case CTF_FIELD_CLASS_TYPE_ARRAY:
		ir_fc = ctf_field_class_array_to_ir(ctx, (void *) fc);
		break;
	case CTF_FIELD_CLASS_TYPE_SEQUENCE:
		ir_fc = ctf_field_class_sequence_to_ir(ctx, (void *) fc);
		break;
	case CTF_FIELD_CLASS_TYPE_VARIANT:
		ir_fc = ctf_field_class_variant_to_ir(ctx, (void *) fc);
		break;
	default:
		bt_common_abort();
	}

	fc->ir_fc = ir_fc;
	return ir_fc;
}

static inline
bool ctf_field_class_struct_has_immediate_member_in_ir(
		struct ctf_field_class_struct *fc)
{
	uint64_t i;
	bool has_immediate_member_in_ir = false;

	/*
	 * If the structure field class has no members at all, then it
	 * was an empty structure in the beginning, so leave it existing
	 * and empty.
	 */
	if (fc->members->len == 0) {
		has_immediate_member_in_ir = true;
		goto end;
	}

	for (i = 0; i < fc->members->len; i++) {
		struct ctf_named_field_class *named_fc =
			ctf_field_class_struct_borrow_member_by_index(fc, i);

		if (named_fc->fc->in_ir) {
			has_immediate_member_in_ir = true;
			goto end;
		}
	}

end:
	return has_immediate_member_in_ir;
}

static inline
bt_field_class *scope_ctf_field_class_to_ir(struct ctx *ctx)
{
	bt_field_class *ir_fc = NULL;
	struct ctf_field_class *fc = NULL;

	switch (ctx->scope) {
	case CTF_SCOPE_PACKET_CONTEXT:
		fc = ctx->sc->packet_context_fc;
		break;
	case CTF_SCOPE_EVENT_COMMON_CONTEXT:
		fc = ctx->sc->event_common_context_fc;
		break;
	case CTF_SCOPE_EVENT_SPECIFIC_CONTEXT:
		fc = ctx->ec->spec_context_fc;
		break;
	case CTF_SCOPE_EVENT_PAYLOAD:
		fc = ctx->ec->payload_fc;
		break;
	default:
		bt_common_abort();
	}

	if (fc && ctf_field_class_struct_has_immediate_member_in_ir(
			(void *) fc)) {
		ir_fc = ctf_field_class_to_ir(ctx, fc);
	}

	return ir_fc;
}

static inline
void ctf_event_class_to_ir(struct ctx *ctx)
{
	int ret;
	bt_event_class *ir_ec = NULL;
	bt_field_class *ir_fc;

	BT_ASSERT(ctx->ec);

	if (ctx->ec->is_translated) {
		ir_ec = bt_stream_class_borrow_event_class_by_id(
			ctx->ir_sc, ctx->ec->id);
		BT_ASSERT(ir_ec);
		goto end;
	}

	ir_ec = bt_event_class_create_with_id(ctx->ir_sc, ctx->ec->id);
	BT_ASSERT(ir_ec);
	bt_event_class_put_ref(ir_ec);
	ctx->scope = CTF_SCOPE_EVENT_SPECIFIC_CONTEXT;
	ir_fc = scope_ctf_field_class_to_ir(ctx);
	if (ir_fc) {
		ret = bt_event_class_set_specific_context_field_class(
			ir_ec, ir_fc);
		BT_ASSERT(ret == 0);
		bt_field_class_put_ref(ir_fc);
	}

	ctx->scope = CTF_SCOPE_EVENT_PAYLOAD;
	ir_fc = scope_ctf_field_class_to_ir(ctx);
	if (ir_fc) {
		ret = bt_event_class_set_payload_field_class(ir_ec,
			ir_fc);
		BT_ASSERT(ret == 0);
		bt_field_class_put_ref(ir_fc);
	}

	if (ctx->ec->name->len > 0) {
		ret = bt_event_class_set_name(ir_ec, ctx->ec->name->str);
		BT_ASSERT(ret == 0);
	}

	if (ctx->ec->emf_uri->len > 0) {
		ret = bt_event_class_set_emf_uri(ir_ec, ctx->ec->emf_uri->str);
		BT_ASSERT(ret == 0);
	}

	if (ctx->ec->is_log_level_set) {
		bt_event_class_set_log_level(ir_ec, ctx->ec->log_level);
	}

	ctx->ec->is_translated = true;
	ctx->ec->ir_ec = ir_ec;

end:
	return;
}


static inline
void ctf_stream_class_to_ir(struct ctx *ctx)
{
	int ret;
	bt_field_class *ir_fc;

	BT_ASSERT(ctx->sc);

	if (ctx->sc->is_translated) {
		ctx->ir_sc = bt_trace_class_borrow_stream_class_by_id(
			ctx->ir_tc, ctx->sc->id);
		BT_ASSERT(ctx->ir_sc);
		goto end;
	}

	ctx->ir_sc = bt_stream_class_create_with_id(ctx->ir_tc, ctx->sc->id);
	BT_ASSERT(ctx->ir_sc);
	bt_stream_class_put_ref(ctx->ir_sc);

	if (ctx->sc->default_clock_class) {
		BT_ASSERT(ctx->sc->default_clock_class->ir_cc);
		ret = bt_stream_class_set_default_clock_class(ctx->ir_sc,
			ctx->sc->default_clock_class->ir_cc);
		BT_ASSERT(ret == 0);
	}

	bt_stream_class_set_supports_packets(ctx->ir_sc, BT_TRUE,
		ctx->sc->packets_have_ts_begin, ctx->sc->packets_have_ts_end);
	bt_stream_class_set_supports_discarded_events(ctx->ir_sc,
		ctx->sc->has_discarded_events,
		ctx->sc->discarded_events_have_default_cs);
	bt_stream_class_set_supports_discarded_packets(ctx->ir_sc,
		ctx->sc->has_discarded_packets,
		ctx->sc->discarded_packets_have_default_cs);
	ctx->scope = CTF_SCOPE_PACKET_CONTEXT;
	ir_fc = scope_ctf_field_class_to_ir(ctx);
	if (ir_fc) {
		ret = bt_stream_class_set_packet_context_field_class(
			ctx->ir_sc, ir_fc);
		BT_ASSERT(ret == 0);
		bt_field_class_put_ref(ir_fc);
	}

	ctx->scope = CTF_SCOPE_EVENT_COMMON_CONTEXT;
	ir_fc = scope_ctf_field_class_to_ir(ctx);
	if (ir_fc) {
		ret = bt_stream_class_set_event_common_context_field_class(
			ctx->ir_sc, ir_fc);
		BT_ASSERT(ret == 0);
		bt_field_class_put_ref(ir_fc);
	}

	bt_stream_class_set_assigns_automatic_event_class_id(ctx->ir_sc,
		BT_FALSE);
	bt_stream_class_set_assigns_automatic_stream_id(ctx->ir_sc, BT_FALSE);

	ctx->sc->is_translated = true;
	ctx->sc->ir_sc = ctx->ir_sc;

end:
	return;
}

static inline
void ctf_clock_class_to_ir(bt_clock_class *ir_cc, struct ctf_clock_class *cc)
{
	int ret;

	if (strlen(cc->name->str) > 0) {
		ret = bt_clock_class_set_name(ir_cc, cc->name->str);
		BT_ASSERT(ret == 0);
	}

	if (strlen(cc->description->str) > 0) {
		ret = bt_clock_class_set_description(ir_cc, cc->description->str);
		BT_ASSERT(ret == 0);
	}

	bt_clock_class_set_frequency(ir_cc, cc->frequency);
	bt_clock_class_set_precision(ir_cc, cc->precision);
	bt_clock_class_set_offset(ir_cc, cc->offset_seconds, cc->offset_cycles);

	if (cc->has_uuid) {
		bt_clock_class_set_uuid(ir_cc, cc->uuid);
	}

	bt_clock_class_set_origin_is_unix_epoch(ir_cc, cc->is_absolute);
}

static inline
int ctf_trace_class_to_ir(struct ctx *ctx)
{
	int ret = 0;
	uint64_t i;

	BT_ASSERT(ctx->tc);
	BT_ASSERT(ctx->ir_tc);

	if (ctx->tc->is_translated) {
		goto end;
	}

	for (i = 0; i < ctx->tc->clock_classes->len; i++) {
		struct ctf_clock_class *cc = ctx->tc->clock_classes->pdata[i];

		cc->ir_cc = bt_clock_class_create(ctx->self_comp);
		ctf_clock_class_to_ir(cc->ir_cc, cc);
	}

	bt_trace_class_set_assigns_automatic_stream_class_id(ctx->ir_tc,
		BT_FALSE);
	ctx->tc->is_translated = true;
	ctx->tc->ir_tc = ctx->ir_tc;

end:
	return ret;
}

BT_HIDDEN
int ctf_trace_class_translate(bt_self_component *self_comp,
		bt_trace_class *ir_tc, struct ctf_trace_class *tc)
{
	int ret = 0;
	uint64_t i;
	struct ctx ctx = { 0 };

	ctx.self_comp = self_comp;
	ctx.tc = tc;
	ctx.ir_tc = ir_tc;
	ret = ctf_trace_class_to_ir(&ctx);
	if (ret) {
		goto end;
	}

	for (i = 0; i < tc->stream_classes->len; i++) {
		uint64_t j;
		ctx.sc = tc->stream_classes->pdata[i];

		ctf_stream_class_to_ir(&ctx);

		for (j = 0; j < ctx.sc->event_classes->len; j++) {
			ctx.ec = ctx.sc->event_classes->pdata[j];

			ctf_event_class_to_ir(&ctx);
			ctx.ec = NULL;
		}

		ctx.sc = NULL;
	}

end:
	return ret;
}
