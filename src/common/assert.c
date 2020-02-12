/*
 * Copyright 2019 EfficiOS Inc.
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

#include <stdio.h>

#include "common/assert.h"
#include "common/common.h"

void bt_common_assert_failed(const char *file, int line, const char *func,
		const char *assertion)
{
	fprintf(stderr,
		"%s\n%s%s%s (╯°□°)╯︵ ┻━┻ %s %s%s%s%s:%s%d%s: %s%s()%s: "
		"%sAssertion %s%s`%s`%s%s failed.%s\n",
		bt_common_color_reset(),
		bt_common_color_bold(),
		bt_common_color_bg_yellow(),
		bt_common_color_fg_bright_red(),
		bt_common_color_reset(),
		bt_common_color_bold(),
		bt_common_color_fg_bright_magenta(),
		file,
		bt_common_color_reset(),
		bt_common_color_fg_green(),
		line,
		bt_common_color_reset(),
		bt_common_color_fg_cyan(),
		func,
		bt_common_color_reset(),
		bt_common_color_fg_red(),
		bt_common_color_bold(),
		bt_common_color_fg_bright_red(),
		assertion,
		bt_common_color_reset(),
		bt_common_color_fg_red(),
		bt_common_color_reset());
	bt_common_abort();
}
