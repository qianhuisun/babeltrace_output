/*
 * test_bt_uuid.c
 *
 * Copyright 2019 Michael Jeanson <mjeanson@efficios.com>
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

#include <stdio.h>
#include <string.h>

#include <tap/tap.h>

#include "common/uuid.h"

#define NR_TESTS 23

static const char valid_str_1[] = "3d260c88-75ea-47b8-a7e2-d6077c0378d9";
static const char valid_str_2[] = "611cf3a6-a68b-4515-834f-208bc2762592";
static const char valid_str_3[] = "1b4855cc-96de-4ae8-abe3-86449c2a43c4";
static const char valid_str_4[] = "8ADED5B9-ACD2-439F-A60C-897403AA2AB4";
static const char valid_str_5[] = "f109e0a2-C619-4d18-b760-20EA20E0F69A";

static bt_uuid_t valid_uuid_1 = {
	0x3d, 0x26, 0x0c, 0x88, 0x75, 0xea, 0x47, 0xb8,
	0xa7, 0xe2, 0xd6, 0x07, 0x7c, 0x03, 0x78, 0xd9
};
static bt_uuid_t valid_uuid_2 = {
	0x61, 0x1c, 0xf3, 0xa6, 0xa6, 0x8b, 0x45, 0x15,
	0x83, 0x4f, 0x20, 0x8b, 0xc2, 0x76, 0x25, 0x92
};
static bt_uuid_t valid_uuid_3 = {
	0x1b, 0x48, 0x55, 0xcc, 0x96, 0xde, 0x4a, 0xe8,
	0xab, 0xe3, 0x86, 0x44, 0x9c, 0x2a, 0x43, 0xc4
};

static const char invalid_str_1[] = "1b485!cc-96de-4XX8-abe3-86449c2a43?4";
static const char invalid_str_2[] = "c2e6eddb&3955&4006&be3a&70bb63bd5f25";
static const char invalid_str_3[] = "81b1cb88-ff42-45b9-ba4d-964088ee45";
static const char invalid_str_4[] = "2d-6c6d756574-470e-9142-a4e6ad03f143";
static const char invalid_str_5[] = "4542ad19-9e4f-4931-8261-2101c3e089ae7";
static const char invalid_str_6[] = "XX0123";

static
void run_test_bt_uuid_from_str(void)
{
	int ret;
	bt_uuid_t uuid1;

	/*
	 * Parse valid UUID strings, expect success.
	 */
	ret = bt_uuid_from_str(valid_str_1, uuid1);
	ok(ret == 0, "bt_uuid_from_str - Parse valid string '%s', expect success", valid_str_1);

	ret = bt_uuid_from_str(valid_str_2, uuid1);
	ok(ret == 0, "bt_uuid_from_str - Parse valid string '%s', expect success", valid_str_2);

	ret = bt_uuid_from_str(valid_str_3, uuid1);
	ok(ret == 0, "bt_uuid_from_str - Parse valid string '%s', expect success", valid_str_3);

	ret = bt_uuid_from_str(valid_str_4, uuid1);
	ok(ret == 0, "bt_uuid_from_str - Parse valid string '%s', expect success", valid_str_4);

	ret = bt_uuid_from_str(valid_str_5, uuid1);
	ok(ret == 0, "bt_uuid_from_str - Parse valid string '%s', expect success", valid_str_5);

	/*
	 * Parse invalid UUID strings, expect failure.
	 */
	ret = bt_uuid_from_str(invalid_str_1, uuid1);
	ok(ret != 0, "bt_uuid_from_str - Parse invalid string '%s', expect failure", invalid_str_1);

	ret = bt_uuid_from_str(invalid_str_2, uuid1);
	ok(ret != 0, "bt_uuid_from_str - Parse invalid string '%s', expect failure", invalid_str_2);

	ret = bt_uuid_from_str(invalid_str_3, uuid1);
	ok(ret != 0, "bt_uuid_from_str - Parse invalid string '%s', expect failure", invalid_str_3);

	ret = bt_uuid_from_str(invalid_str_4, uuid1);
	ok(ret != 0, "bt_uuid_from_str - Parse invalid string '%s', expect failure", invalid_str_4);

	ret = bt_uuid_from_str(invalid_str_5, uuid1);
	ok(ret != 0, "bt_uuid_from_str - Parse invalid string '%s', expect failure", invalid_str_5);

	ret = bt_uuid_from_str(invalid_str_6, uuid1);
	ok(ret != 0, "bt_uuid_from_str - Parse invalid string '%s', expect failure", invalid_str_6);
}

static
void run_test_bt_uuid_to_str(void)
{
	char uuid_str[BT_UUID_STR_LEN + 1];

	bt_uuid_to_str(valid_uuid_1, uuid_str);
	ok(strcmp(uuid_str, valid_str_1) == 0, "bt_uuid_to_str - Convert UUID '%s' to string, expect success", valid_str_1);

	bt_uuid_to_str(valid_uuid_2, uuid_str);
	ok(strcmp(uuid_str, valid_str_2) == 0, "bt_uuid_to_str - Convert UUID '%s' to string, expect success", valid_str_2);

	bt_uuid_to_str(valid_uuid_3, uuid_str);
	ok(strcmp(uuid_str, valid_str_3) == 0, "bt_uuid_to_str - Convert UUID '%s' to string, expect success", valid_str_3);
}

static
void run_test_bt_uuid_compare(void)
{
	int ret;
	bt_uuid_t uuid1, uuid2;

	bt_uuid_from_str(valid_str_1, uuid1);
	bt_uuid_from_str(valid_str_1, uuid2);
	ret = bt_uuid_compare(uuid1, uuid2);
	ok(ret == 0, "bt_uuid_compare - Compare same UUID, expect success");

	bt_uuid_from_str(valid_str_2, uuid2);
	ret = bt_uuid_compare(uuid1, uuid2);
	ok(ret != 0, "bt_uuid_compare - Compare different UUID, expect failure");
	ok(ret < 0, "bt_uuid_compare - Compare different UUID, expect uuid1 smaller");
	ret = bt_uuid_compare(uuid2, uuid1);
	ok(ret > 0, "bt_uuid_compare - Compare different UUID, expect uuid2 bigger");
}

static
void run_test_bt_uuid_copy(void)
{
	int ret;
	bt_uuid_t uuid1;

	bt_uuid_copy(uuid1, valid_uuid_1);
	ret = bt_uuid_compare(uuid1, valid_uuid_1);

	ok(ret == 0, "bt_uuid_copy - Compare copied UUID with source, expect success");
}

static
void run_test_bt_uuid_generate(void)
{
	int ret;
	bt_uuid_t uuid1, uuid2;

	bt_uuid_generate(uuid1);
	bt_uuid_generate(uuid2);

	ok(bt_uuid_compare(uuid1, uuid2) != 0, "bt_uuid_generate - Generated UUIDs are different");

	/*
	 * Set the two most significant bits (bits 6 and 7) of the
	 * clock_seq_hi_and_reserved to zero and one, respectively.
	 */
	ret = uuid1[8] & (1 << 6);
	ok(ret == 0, "bt_uuid_generate - bit 6 of clock_seq_hi_and_reserved is set to zero");

	ret = uuid1[8] & (1 << 7);
	ok(ret != 0, "bt_uuid_generate - bit 7 of clock_seq_hi_and_reserved is set to one");

	/*
	 * Set the four most significant bits (bits 12 through 15) of the
	 * time_hi_and_version field to the 4-bit version number from
	 * Section 4.1.3.
	 */
	ret = uuid1[6] >> 4;
	ok(ret == BT_UUID_VER, "bt_uuid_generate - Generated UUID version check");
}

static
void run_test(void)
{
	plan_tests(NR_TESTS);

	run_test_bt_uuid_from_str();
	run_test_bt_uuid_to_str();
	run_test_bt_uuid_compare();
	run_test_bt_uuid_copy();
	run_test_bt_uuid_generate();
}

int main(int argc, char **argv)
{
	/* Run tap-formated tests */
	run_test();

	return exit_status();
}
