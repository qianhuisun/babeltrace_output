/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 1048 "parser.y" /* yacc.c:1909  */

#ifndef ALLOW_INCLUDE_PARSER_H
# error "Don't include parser.h directly, include parser-wrap.h instead."
#endif

#line 50 "parser.h" /* yacc.c:1909  */

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    CTF_INTEGER_LITERAL = 258,
    CTF_STRING_LITERAL = 259,
    CTF_CHARACTER_LITERAL = 260,
    CTF_LSBRAC = 261,
    CTF_RSBRAC = 262,
    CTF_LPAREN = 263,
    CTF_RPAREN = 264,
    CTF_LBRAC = 265,
    CTF_RBRAC = 266,
    CTF_RARROW = 267,
    CTF_STAR = 268,
    CTF_PLUS = 269,
    CTF_MINUS = 270,
    CTF_LT = 271,
    CTF_GT = 272,
    CTF_TYPEASSIGN = 273,
    CTF_COLON = 274,
    CTF_SEMICOLON = 275,
    CTF_DOTDOTDOT = 276,
    CTF_DOT = 277,
    CTF_EQUAL = 278,
    CTF_COMMA = 279,
    CTF_CONST = 280,
    CTF_CHAR = 281,
    CTF_DOUBLE = 282,
    CTF_ENUM = 283,
    CTF_ENV = 284,
    CTF_EVENT = 285,
    CTF_FLOATING_POINT = 286,
    CTF_FLOAT = 287,
    CTF_INTEGER = 288,
    CTF_INT = 289,
    CTF_LONG = 290,
    CTF_SHORT = 291,
    CTF_SIGNED = 292,
    CTF_STREAM = 293,
    CTF_STRING = 294,
    CTF_STRUCT = 295,
    CTF_TRACE = 296,
    CTF_CALLSITE = 297,
    CTF_CLOCK = 298,
    CTF_TYPEALIAS = 299,
    CTF_TYPEDEF = 300,
    CTF_UNSIGNED = 301,
    CTF_VARIANT = 302,
    CTF_VOID = 303,
    CTF_BOOL = 304,
    CTF_COMPLEX = 305,
    CTF_IMAGINARY = 306,
    CTF_TOK_ALIGN = 307,
    IDENTIFIER = 308,
    ID_TYPE = 309,
    CTF_ERROR = 310
  };
#endif
/* Tokens.  */
#define CTF_INTEGER_LITERAL 258
#define CTF_STRING_LITERAL 259
#define CTF_CHARACTER_LITERAL 260
#define CTF_LSBRAC 261
#define CTF_RSBRAC 262
#define CTF_LPAREN 263
#define CTF_RPAREN 264
#define CTF_LBRAC 265
#define CTF_RBRAC 266
#define CTF_RARROW 267
#define CTF_STAR 268
#define CTF_PLUS 269
#define CTF_MINUS 270
#define CTF_LT 271
#define CTF_GT 272
#define CTF_TYPEASSIGN 273
#define CTF_COLON 274
#define CTF_SEMICOLON 275
#define CTF_DOTDOTDOT 276
#define CTF_DOT 277
#define CTF_EQUAL 278
#define CTF_COMMA 279
#define CTF_CONST 280
#define CTF_CHAR 281
#define CTF_DOUBLE 282
#define CTF_ENUM 283
#define CTF_ENV 284
#define CTF_EVENT 285
#define CTF_FLOATING_POINT 286
#define CTF_FLOAT 287
#define CTF_INTEGER 288
#define CTF_INT 289
#define CTF_LONG 290
#define CTF_SHORT 291
#define CTF_SIGNED 292
#define CTF_STREAM 293
#define CTF_STRING 294
#define CTF_STRUCT 295
#define CTF_TRACE 296
#define CTF_CALLSITE 297
#define CTF_CLOCK 298
#define CTF_TYPEALIAS 299
#define CTF_TYPEDEF 300
#define CTF_UNSIGNED 301
#define CTF_VARIANT 302
#define CTF_VOID 303
#define CTF_BOOL 304
#define CTF_COMPLEX 305
#define CTF_IMAGINARY 306
#define CTF_TOK_ALIGN 307
#define IDENTIFIER 308
#define ID_TYPE 309
#define CTF_ERROR 310

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 1082 "parser.y" /* yacc.c:1909  */

	long long ll;
	unsigned long long ull;
	char c;
	char *s;
	struct ctf_node *n;

#line 180 "parser.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif



int yyparse (struct ctf_scanner *scanner, yyscan_t yyscanner);
/* "%code provides" blocks.  */
#line 1054 "parser.y" /* yacc.c:1909  */

	BT_HIDDEN
	void setstring(struct ctf_scanner *scanner, YYSTYPE *lvalp, const char *src);
	
	BT_HIDDEN
	int import_string(struct ctf_scanner *scanner, YYSTYPE *lvalp, const char *src, char delim);

#line 200 "parser.h" /* yacc.c:1909  */

#endif /* !YY_YY_PARSER_H_INCLUDED  */
