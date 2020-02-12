#ifndef BABELTRACE_CTF_WRITER_FIELD_TYPES_INTERNAL_H
#define BABELTRACE_CTF_WRITER_FIELD_TYPES_INTERNAL_H

/*
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
 *
 * The Common Trace Format (CTF) Specification is available at
 * http://www.efficios.com/ctf
 */

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

#include <babeltrace2-ctf-writer/field-types.h>
#include <babeltrace2-ctf-writer/types.h>

#include "common/macros.h"

#include "assert-pre.h"
#include "clock-class.h"
#include "object.h"
#include "writer.h"

#define BT_CTF_ASSERT_PRE_CTF_FT_COMMON_HAS_ID(_ft, _type_id, _name)	\
	BT_CTF_ASSERT_PRE(((struct bt_ctf_field_type_common *) (_ft))->id == (_type_id), \
		_name " has the wrong type ID: expected-type-id=%s, "	\
		"ft-addr=%p", bt_ctf_field_type_id_string(_type_id), (_ft))

#define BT_CTF_ASSERT_PRE_CTF_FT_HOT(_ft, _name)				\
	BT_CTF_ASSERT_PRE_HOT((_ft), (_name), ": ft-addr=%p", (_ft))

#define BT_CTF_FIELD_TYPE_COMMON_STRUCTURE_FIELD_AT_INDEX(_ft, _index)	\
	(&g_array_index(((struct bt_ctf_field_type_common_structure *) (_ft))->fields, \
		struct bt_ctf_field_type_common_structure_field, (_index)))

#define BT_CTF_FIELD_TYPE_COMMON_VARIANT_CHOICE_AT_INDEX(_ft, _index)	\
	(&g_array_index(((struct bt_ctf_field_type_common_variant *) (_ft))->choices, \
		struct bt_ctf_field_type_common_variant_choice, (_index)))

struct bt_ctf_field_common;
struct bt_ctf_field_type_common;

typedef void (*bt_ctf_field_type_common_method_freeze)(
		struct bt_ctf_field_type_common *);
typedef int (*bt_ctf_field_type_common_method_validate)(
		struct bt_ctf_field_type_common *);
typedef void (*bt_ctf_field_type_common_method_set_byte_order)(
		struct bt_ctf_field_type_common *, enum bt_ctf_byte_order);
typedef struct bt_ctf_field_type_common *(*bt_ctf_field_type_common_method_copy)(
		struct bt_ctf_field_type_common *);
typedef int (*bt_ctf_field_type_common_method_compare)(
		struct bt_ctf_field_type_common *,
		struct bt_ctf_field_type_common *);

struct bt_ctf_field_type_common_methods {
	bt_ctf_field_type_common_method_freeze freeze;
	bt_ctf_field_type_common_method_validate validate;
	bt_ctf_field_type_common_method_set_byte_order set_byte_order;
	bt_ctf_field_type_common_method_copy copy;
	bt_ctf_field_type_common_method_compare compare;
};

struct bt_ctf_field_type_common {
	struct bt_ctf_object base;
	enum bt_ctf_field_type_id id;
	unsigned int alignment;

	/* Virtual table */
	struct bt_ctf_field_type_common_methods *methods;

	/*
	 * A type can't be modified once it is added to an event or after a
	 * a field has been instanciated from it.
	 */
	int frozen;

	/*
	 * This flag indicates if the field type is valid. A valid
	 * field type is _always_ frozen. All the nested field types of
	 * a valid field type are also valid (and thus frozen).
	 */
	int valid;

	/*
	 * Specialized data for either CTF IR or CTF writer APIs.
	 * Having this here ensures that:
	 *
	 * * The type-specific common data is always found at the same
	 *   offset when the common API has a `struct
	 *   bt_ctf_field_type_common *` so that you can cast it to `struct
	 *   bt_ctf_field_type_common_integer *` for example and access the
	 *   common integer field type fields.
	 *
	 * * The specific CTF IR and CTF writer APIs can access their
	 *   specific field type fields in this union at an offset known
	 *   at build time. This avoids a pointer to specific data so
	 *   that all the fields, common or specific, of a CTF IR
	 *   integer field type or of a CTF writer integer field type,
	 *   for example, are contained within the same contiguous block
	 *   of memory.
	 */
	union {
		struct {
		} ir;
		struct {
			void *serialize_func;
		} writer;
	} spec;
};

struct bt_ctf_field_type_common_integer {
	struct bt_ctf_field_type_common common;

	/* Owned by this */
	struct bt_ctf_clock_class *mapped_clock_class;

	enum bt_ctf_byte_order user_byte_order;
	bt_ctf_bool is_signed;
	unsigned int size;
	enum bt_ctf_integer_base base;
	enum bt_ctf_string_encoding encoding;
};

struct bt_ctf_enumeration_mapping {
	union {
		uint64_t _unsigned;
		int64_t _signed;
	} range_start;
	union {
		uint64_t _unsigned;
		int64_t _signed;
	} range_end;
	GQuark string;
};

struct bt_ctf_field_type_common_enumeration {
	struct bt_ctf_field_type_common common;

	/* Owned by this */
	struct bt_ctf_field_type_common_integer *container_ft;

	/* Array of `struct bt_ctf_enumeration_mapping *`, owned by this */
	GPtrArray *entries;

	/* Only set during validation */
	bt_ctf_bool has_overlapping_ranges;
};

enum bt_ctf_field_type_enumeration_mapping_iterator_type {
	CTF_ITERATOR_BY_NAME,
	CTF_ITERATOR_BY_SIGNED_VALUE,
	CTF_ITERATOR_BY_UNSIGNED_VALUE,
};

struct bt_ctf_field_type_enumeration_mapping_iterator {
	struct bt_ctf_object base;

	/* Owned by this */
	struct bt_ctf_field_type_common_enumeration *enumeration_ft;

	enum bt_ctf_field_type_enumeration_mapping_iterator_type type;
	int index;
	union {
		GQuark name_quark;
		int64_t signed_value;
		uint64_t unsigned_value;
	} u;
};

struct bt_ctf_field_type_common_floating_point {
	struct bt_ctf_field_type_common common;
	enum bt_ctf_byte_order user_byte_order;
	unsigned int exp_dig;
	unsigned int mant_dig;
};

struct bt_ctf_field_type_common_structure_field {
	GQuark name;

	/* Owned by this */
	struct bt_ctf_field_type_common *type;
};

struct bt_ctf_field_type_common_structure {
	struct bt_ctf_field_type_common common;
	GHashTable *field_name_to_index;

	/*
	 * Array of `struct bt_ctf_field_type_common_structure_field`,
	 * owned by this
	 */
	GArray *fields;
};

struct bt_ctf_field_type_common_variant_choice_range {
	union {
		int64_t i;
		uint64_t u;
	} lower;
	union {
		int64_t i;
		uint64_t u;
	} upper;
};

struct bt_ctf_field_type_common_variant_choice {
	GQuark name;

	/* Owned by this */
	struct bt_ctf_field_type_common *type;

	/* Array of `struct bt_ctf_field_type_common_variant_choice_range` */
	GArray *ranges;
};

struct bt_ctf_field_type_common_variant {
	struct bt_ctf_field_type_common common;
	GString *tag_name;
	bool choices_up_to_date;

	/* Owned by this */
	struct bt_ctf_field_type_common_enumeration *tag_ft;

	/* Owned by this */
	struct bt_ctf_field_path *tag_field_path;

	GHashTable *choice_name_to_index;

	/*
	 * Array of `struct bt_ctf_field_type_common_variant_choice`,
	 * owned by this */
	GArray *choices;
};

struct bt_ctf_field_type_common_array {
	struct bt_ctf_field_type_common common;

	/* Owned by this */
	struct bt_ctf_field_type_common *element_ft;

	unsigned int length;
};

struct bt_ctf_field_type_common_sequence {
	struct bt_ctf_field_type_common common;

	/* Owned by this */
	struct bt_ctf_field_type_common *element_ft;

	GString *length_field_name;

	/* Owned by this */
	struct bt_ctf_field_path *length_field_path;
};

struct bt_ctf_field_type_common_string {
	struct bt_ctf_field_type_common common;
	enum bt_ctf_string_encoding encoding;
};

typedef struct bt_ctf_field_common *(* bt_ctf_field_common_create_func)(
		struct bt_ctf_field_type_common *);

BT_HIDDEN
void bt_ctf_field_type_common_initialize(struct bt_ctf_field_type_common *ft,
		bool init_bo, bt_ctf_object_release_func release_func,
		struct bt_ctf_field_type_common_methods *methods);

BT_HIDDEN
void bt_ctf_field_type_common_integer_initialize(
		struct bt_ctf_field_type_common *ft,
		unsigned int size, bt_ctf_object_release_func release_func,
		struct bt_ctf_field_type_common_methods *methods);

BT_HIDDEN
void bt_ctf_field_type_common_floating_point_initialize(
		struct bt_ctf_field_type_common *ft,
		bt_ctf_object_release_func release_func,
		struct bt_ctf_field_type_common_methods *methods);

BT_HIDDEN
void bt_ctf_field_type_common_enumeration_initialize(
		struct bt_ctf_field_type_common *ft,
		struct bt_ctf_field_type_common *container_ft,
		bt_ctf_object_release_func release_func,
		struct bt_ctf_field_type_common_methods *methods);

BT_HIDDEN
void bt_ctf_field_type_common_string_initialize(
		struct bt_ctf_field_type_common *ft,
		bt_ctf_object_release_func release_func,
		struct bt_ctf_field_type_common_methods *methods);

BT_HIDDEN
void bt_ctf_field_type_common_structure_initialize(
		struct bt_ctf_field_type_common *ft,
		bt_ctf_object_release_func release_func,
		struct bt_ctf_field_type_common_methods *methods);

BT_HIDDEN
void bt_ctf_field_type_common_array_initialize(
		struct bt_ctf_field_type_common *ft,
		struct bt_ctf_field_type_common *element_ft,
		unsigned int length, bt_ctf_object_release_func release_func,
		struct bt_ctf_field_type_common_methods *methods);

BT_HIDDEN
void bt_ctf_field_type_common_sequence_initialize(
		struct bt_ctf_field_type_common *ft,
		struct bt_ctf_field_type_common *element_ft,
		const char *length_field_name,
		bt_ctf_object_release_func release_func,
		struct bt_ctf_field_type_common_methods *methods);

BT_HIDDEN
void bt_ctf_field_type_common_variant_initialize(
		struct bt_ctf_field_type_common *ft,
		struct bt_ctf_field_type_common *tag_ft,
		const char *tag_name,
		bt_ctf_object_release_func release_func,
		struct bt_ctf_field_type_common_methods *methods);

BT_HIDDEN
void bt_ctf_field_type_common_integer_destroy(struct bt_ctf_object *obj);

BT_HIDDEN
void bt_ctf_field_type_common_floating_point_destroy(struct bt_ctf_object *obj);

BT_HIDDEN
void bt_ctf_field_type_common_enumeration_destroy_recursive(struct bt_ctf_object *obj);

BT_HIDDEN
void bt_ctf_field_type_common_string_destroy(struct bt_ctf_object *obj);

BT_HIDDEN
void bt_ctf_field_type_common_structure_destroy_recursive(struct bt_ctf_object *obj);

BT_HIDDEN
void bt_ctf_field_type_common_array_destroy_recursive(struct bt_ctf_object *obj);

BT_HIDDEN
void bt_ctf_field_type_common_sequence_destroy_recursive(struct bt_ctf_object *obj);

BT_HIDDEN
void bt_ctf_field_type_common_variant_destroy_recursive(struct bt_ctf_object *obj);

BT_HIDDEN
int bt_ctf_field_type_common_integer_validate(struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_enumeration_validate_recursive(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_sequence_validate_recursive(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_array_validate_recursive(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_structure_validate_recursive(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_variant_validate_recursive(
		struct bt_ctf_field_type_common *type);

BT_HIDDEN
int bt_ctf_field_type_common_validate(struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_integer_get_size(struct bt_ctf_field_type_common *ft);

BT_HIDDEN
bt_ctf_bool bt_ctf_field_type_common_integer_is_signed(struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_integer_set_is_signed(struct bt_ctf_field_type_common *ft,
		bt_ctf_bool is_signed);

BT_HIDDEN
int bt_ctf_field_type_common_integer_set_size(struct bt_ctf_field_type_common *ft,
		unsigned int size);

BT_HIDDEN
enum bt_ctf_integer_base bt_ctf_field_type_common_integer_get_base(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_integer_set_base(struct bt_ctf_field_type_common *ft,
		enum bt_ctf_integer_base base);

BT_HIDDEN
enum bt_ctf_string_encoding bt_ctf_field_type_common_integer_get_encoding(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_integer_set_encoding(struct bt_ctf_field_type_common *ft,
		enum bt_ctf_string_encoding encoding);

BT_HIDDEN
struct bt_ctf_clock_class *bt_ctf_field_type_common_integer_borrow_mapped_clock_class(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_integer_set_mapped_clock_class_no_check_frozen(
		struct bt_ctf_field_type_common *ft,
		struct bt_ctf_clock_class *clock_class);

BT_HIDDEN
int bt_ctf_field_type_common_integer_set_mapped_clock_class(
		struct bt_ctf_field_type_common *ft,
		struct bt_ctf_clock_class *clock_class);

BT_HIDDEN
struct bt_ctf_field_type_enumeration_mapping_iterator *
bt_ctf_field_type_common_enumeration_find_mappings_by_name(
		struct bt_ctf_field_type_common *ft, const char *name);

BT_HIDDEN
struct bt_ctf_field_type_enumeration_mapping_iterator *
bt_ctf_field_type_common_enumeration_signed_find_mappings_by_value(
		struct bt_ctf_field_type_common *ft, int64_t value);

BT_HIDDEN
struct bt_ctf_field_type_enumeration_mapping_iterator *
bt_ctf_field_type_common_enumeration_unsigned_find_mappings_by_value(
		struct bt_ctf_field_type_common *ft, uint64_t value);

BT_HIDDEN
int bt_ctf_field_type_enumeration_mapping_iterator_next(
		struct bt_ctf_field_type_enumeration_mapping_iterator *iter);

BT_HIDDEN
int bt_ctf_field_type_enumeration_mapping_iterator_signed_get(
		struct bt_ctf_field_type_enumeration_mapping_iterator *iter,
		const char **mapping_name, int64_t *range_begin,
		int64_t *range_end);

BT_HIDDEN
int bt_ctf_field_type_enumeration_mapping_iterator_unsigned_get(
		struct bt_ctf_field_type_enumeration_mapping_iterator *iter,
		const char **mapping_name, uint64_t *range_begin,
		uint64_t *range_end);

BT_HIDDEN
int bt_ctf_field_type_common_enumeration_signed_get_mapping_by_index(
		struct bt_ctf_field_type_common *ft, uint64_t index,
		const char **mapping_name, int64_t *range_begin,
		int64_t *range_end);

BT_HIDDEN
int bt_ctf_field_type_common_enumeration_unsigned_get_mapping_by_index(
		struct bt_ctf_field_type_common *ft, uint64_t index,
		const char **mapping_name, uint64_t *range_begin,
		uint64_t *range_end);

BT_HIDDEN
struct bt_ctf_field_type_common *
bt_ctf_field_type_common_enumeration_borrow_container_field_type(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_enumeration_signed_add_mapping(
		struct bt_ctf_field_type_common *ft, const char *string,
		int64_t range_start, int64_t range_end);

BT_HIDDEN
int bt_ctf_field_type_common_enumeration_unsigned_add_mapping(
		struct bt_ctf_field_type_common *ft, const char *string,
		uint64_t range_start, uint64_t range_end);

BT_HIDDEN
int64_t bt_ctf_field_type_common_enumeration_get_mapping_count(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_floating_point_get_exponent_digits(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_floating_point_set_exponent_digits(
		struct bt_ctf_field_type_common *ft,
		unsigned int exponent_digits);

BT_HIDDEN
int bt_ctf_field_type_common_floating_point_get_mantissa_digits(
		struct bt_ctf_field_type_common *type);

BT_HIDDEN
int bt_ctf_field_type_common_floating_point_set_mantissa_digits(
		struct bt_ctf_field_type_common *ft, unsigned int mantissa_digits);

BT_HIDDEN
int bt_ctf_field_type_common_structure_replace_field(
		struct bt_ctf_field_type_common *ft,
		const char *field_name,
		struct bt_ctf_field_type_common *field_type);

BT_HIDDEN
int bt_ctf_field_type_common_structure_add_field(struct bt_ctf_field_type_common *ft,
		struct bt_ctf_field_type_common *field_type,
		const char *field_name);

BT_HIDDEN
int64_t bt_ctf_field_type_common_structure_get_field_count(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_structure_borrow_field_by_index(
		struct bt_ctf_field_type_common *ft,
		const char **field_name,
		struct bt_ctf_field_type_common **field_type, uint64_t index);

BT_HIDDEN
struct bt_ctf_field_type_common *
bt_ctf_field_type_common_structure_borrow_field_type_by_name(
		struct bt_ctf_field_type_common *ft, const char *name);

BT_HIDDEN
struct bt_ctf_field_type_common *
bt_ctf_field_type_common_variant_borrow_tag_field_type(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
const char *bt_ctf_field_type_common_variant_get_tag_name(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_variant_set_tag_name(
		struct bt_ctf_field_type_common *ft, const char *name);

BT_HIDDEN
int bt_ctf_field_type_common_variant_add_field(struct bt_ctf_field_type_common *ft,
		struct bt_ctf_field_type_common *field_type,
		const char *field_name);

BT_HIDDEN
int bt_ctf_field_type_common_variant_update_choices(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
struct bt_ctf_field_type_common *
bt_ctf_field_type_common_variant_borrow_field_type_by_name(
		struct bt_ctf_field_type_common *ft,
		const char *field_name);

BT_HIDDEN
int64_t bt_ctf_field_type_common_variant_get_field_count(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_variant_borrow_field_by_index(
		struct bt_ctf_field_type_common *ft,
		const char **field_name,
		struct bt_ctf_field_type_common **field_type, uint64_t index);

BT_HIDDEN
struct bt_ctf_field_type_common *
bt_ctf_field_type_common_array_borrow_element_field_type(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_array_set_element_field_type(
		struct bt_ctf_field_type_common *ft,
		struct bt_ctf_field_type_common *element_ft);

BT_HIDDEN
int64_t bt_ctf_field_type_common_array_get_length(struct bt_ctf_field_type_common *ft);

BT_HIDDEN
struct bt_ctf_field_type_common *
bt_ctf_field_type_common_sequence_borrow_element_field_type(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_sequence_set_element_field_type(
		struct bt_ctf_field_type_common *ft,
		struct bt_ctf_field_type_common *element_ft);

BT_HIDDEN
const char *bt_ctf_field_type_common_sequence_get_length_field_name(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
enum bt_ctf_string_encoding bt_ctf_field_type_common_string_get_encoding(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_string_set_encoding(struct bt_ctf_field_type_common *ft,
		enum bt_ctf_string_encoding encoding);

BT_HIDDEN
int bt_ctf_field_type_common_get_alignment(struct bt_ctf_field_type_common *type);

BT_HIDDEN
int bt_ctf_field_type_common_set_alignment(struct bt_ctf_field_type_common *ft,
		unsigned int alignment);

BT_HIDDEN
enum bt_ctf_byte_order bt_ctf_field_type_common_get_byte_order(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_set_byte_order(struct bt_ctf_field_type_common *ft,
		enum bt_ctf_byte_order byte_order);

BT_HIDDEN
enum bt_ctf_field_type_id bt_ctf_field_type_common_get_type_id(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
void bt_ctf_field_type_common_freeze(struct bt_ctf_field_type_common *ft);

BT_HIDDEN
struct bt_ctf_field_type_common *
bt_ctf_field_type_common_variant_borrow_field_type_signed(
		struct bt_ctf_field_type_common_variant *var_ft,
		int64_t tag_value);

BT_HIDDEN
struct bt_ctf_field_type_common *
bt_ctf_field_type_common_variant_borrow_field_type_unsigned(
		struct bt_ctf_field_type_common_variant *var_ft,
		uint64_t tag_value);

BT_HIDDEN
struct bt_ctf_field_type_common *bt_ctf_field_type_common_copy(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_structure_get_field_name_index(
		struct bt_ctf_field_type_common *ft, const char *name);

BT_HIDDEN
int bt_ctf_field_type_common_variant_get_field_name_index(
		struct bt_ctf_field_type_common *ft, const char *name);

BT_HIDDEN
int bt_ctf_field_type_common_sequence_set_length_field_path(
		struct bt_ctf_field_type_common *ft, struct bt_ctf_field_path *path);

BT_HIDDEN
int bt_ctf_field_type_common_variant_set_tag_field_path(
		struct bt_ctf_field_type_common *ft,
		struct bt_ctf_field_path *path);

BT_HIDDEN
int bt_ctf_field_type_common_variant_set_tag_field_type(
		struct bt_ctf_field_type_common *ft,
		struct bt_ctf_field_type_common *tag_ft);

BT_HIDDEN
void bt_ctf_field_type_common_generic_freeze(struct bt_ctf_field_type_common *ft);

BT_HIDDEN
void bt_ctf_field_type_common_enumeration_freeze_recursive(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
void bt_ctf_field_type_common_structure_freeze_recursive(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
void bt_ctf_field_type_common_variant_freeze_recursive(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
void bt_ctf_field_type_common_array_freeze_recursive(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
void bt_ctf_field_type_common_sequence_freeze_recursive(
		struct bt_ctf_field_type_common *type);

BT_HIDDEN
void bt_ctf_field_type_common_integer_set_byte_order(
		struct bt_ctf_field_type_common *ft, enum bt_ctf_byte_order byte_order);

BT_HIDDEN
void bt_ctf_field_type_common_enumeration_set_byte_order_recursive(
		struct bt_ctf_field_type_common *ft, enum bt_ctf_byte_order byte_order);

BT_HIDDEN
void bt_ctf_field_type_common_floating_point_set_byte_order(
		struct bt_ctf_field_type_common *ft, enum bt_ctf_byte_order byte_order);

BT_HIDDEN
void bt_ctf_field_type_common_structure_set_byte_order_recursive(
		struct bt_ctf_field_type_common *ft,
		enum bt_ctf_byte_order byte_order);

BT_HIDDEN
void bt_ctf_field_type_common_variant_set_byte_order_recursive(
		struct bt_ctf_field_type_common *ft,
		enum bt_ctf_byte_order byte_order);

BT_HIDDEN
void bt_ctf_field_type_common_array_set_byte_order_recursive(
		struct bt_ctf_field_type_common *ft,
		enum bt_ctf_byte_order byte_order);

BT_HIDDEN
void bt_ctf_field_type_common_sequence_set_byte_order_recursive(
		struct bt_ctf_field_type_common *ft,
		enum bt_ctf_byte_order byte_order);

BT_HIDDEN
int bt_ctf_field_type_common_integer_compare(struct bt_ctf_field_type_common *ft_a,
		struct bt_ctf_field_type_common *ft_b);

BT_HIDDEN
int bt_ctf_field_type_common_floating_point_compare(
		struct bt_ctf_field_type_common *ft_a,
		struct bt_ctf_field_type_common *ft_b);

BT_HIDDEN
int bt_ctf_field_type_common_enumeration_compare_recursive(
		struct bt_ctf_field_type_common *ft_a,
		struct bt_ctf_field_type_common *ft_b);

BT_HIDDEN
int bt_ctf_field_type_common_string_compare(struct bt_ctf_field_type_common *ft_a,
		struct bt_ctf_field_type_common *ft_b);

BT_HIDDEN
int bt_ctf_field_type_common_structure_compare_recursive(
		struct bt_ctf_field_type_common *ft_a,
		struct bt_ctf_field_type_common *ft_b);

BT_HIDDEN
int bt_ctf_field_type_common_variant_compare_recursive(
		struct bt_ctf_field_type_common *ft_a,
		struct bt_ctf_field_type_common *ft_b);

BT_HIDDEN
int bt_ctf_field_type_common_array_compare_recursive(
		struct bt_ctf_field_type_common *ft_a,
		struct bt_ctf_field_type_common *ft_b);

BT_HIDDEN
int bt_ctf_field_type_common_sequence_compare_recursive(
		struct bt_ctf_field_type_common *ft_a,
		struct bt_ctf_field_type_common *ft_b);

BT_HIDDEN
int bt_ctf_field_type_common_compare(struct bt_ctf_field_type_common *ft_a,
		struct bt_ctf_field_type_common *ft_b);

BT_HIDDEN
int64_t bt_ctf_field_type_common_get_field_count(struct bt_ctf_field_type_common *ft);

BT_HIDDEN
struct bt_ctf_field_type_common *bt_ctf_field_type_common_borrow_field_at_index(
		struct bt_ctf_field_type_common *ft, int index);

BT_HIDDEN
int bt_ctf_field_type_common_get_field_index(struct bt_ctf_field_type_common *ft,
		const char *name);

BT_HIDDEN
struct bt_ctf_field_path *bt_ctf_field_type_common_variant_borrow_tag_field_path(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
struct bt_ctf_field_path *bt_ctf_field_type_common_sequence_borrow_length_field_path(
		struct bt_ctf_field_type_common *ft);

BT_HIDDEN
int bt_ctf_field_type_common_validate_single_clock_class(
		struct bt_ctf_field_type_common *ft,
		struct bt_ctf_clock_class **expected_clock_class);

BT_HIDDEN
int64_t bt_ctf_field_type_common_variant_find_choice_index(
		struct bt_ctf_field_type_common *ft, uint64_t uval,
		bool is_signed);

BT_HIDDEN
int bt_ctf_field_type_serialize_recursive(struct bt_ctf_field_type *type,
		struct metadata_context *context);

BT_HIDDEN
struct bt_ctf_field_type *bt_ctf_field_type_copy(struct bt_ctf_field_type *ft);

#endif /* BABELTRACE_CTF_WRITER_FIELD_TYPES_INTERNAL_H */
