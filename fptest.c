/*-
 * Copyright (c) 2020 Michael Roe
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <getopt.h>
#include <arpa/inet.h>

#define OP_ADD 1
#define OP_SUB 2
#define OP_MUL 3
#define OP_DIV 4
#define OP_FMA 5
#define OP_SQRT 6
#define OP_REM 7
#define OP_RFI 8
#define OP_CFF 9
#define OP_CFI 10
#define OP_CIF 11
#define OP_CFD 12
#define OP_CDF 13
#define OP_QCMP 14
#define OP_SCMP 15

#define OP_ISFINITE 16
#define OP_ISINFINITE 17

#define OP_ISNAN 18
#define OP_ISNORMAL 19
#define OP_ISSUBNORMAL 20
#define OP_ISSIGNALLING 21
#define OP_ISSIGNED 22
#define OP_ISZERO 23

#define OP_ABS 24
#define OP_COPY 25
#define OP_NEG 26

#define OP_MIN 27
#define OP_MAX 28
#define OP_MINMAG 29
#define OP_MAXMAG 30

#define ROUNDING_RNE 0
#define ROUNDING_UP 1
#define ROUNDING_DOWN 2

#define EXCEPT_INEXACT   1
#define EXCEPT_UNDERFLOW 2
#define EXCEPT_OVERFLOW  4
#define EXCEPT_DIVZERO   8
#define EXCEPT_INVALID  16

#define TYPE_FLOAT 1
#define TYPE_DOUBLE 2
#define TYPE_QUAD 3
#define TYPE_INT 4

static int test_fma = 0;
static int test_fma_only = 0;
static int test_quad = 0;
static int test_fcsr = 0;
static int test_maxmag = 0;
static int test_signalling = 0;
static int test_signalling_max = 0;
static int test_issigned_nan = 0; /* isSigned is non-arithmetic in IEEE 745:2008 */

static unsigned int signalling_nan = 0x7fa00000;

static unsigned long long int signalling_nan_d = 0x7fa00000;

int is_signalling(float f)
{
  unsigned int u;

  if (!isnan(f))
    return 0;
  u = *((unsigned int *) &f);
  return ((u & 0x400000) == 0);
}

float strict_convert(double d)
{
  if (isnan(d))
  {
    return copysign((float) NAN, d);
  }
  else
  {
    return (float) d;
  }
}

float parse_float(char *cp, int *except)
{
char *cp2;
float f;
static char buff2[80];
char *pindex;
int exponent;
unsigned int mantissa;
int i;
unsigned int frep;

      *except = 0;
      cp2 = cp;
      while (*cp2 && *cp2 != ' ')
      {
	*cp = tolower(*cp);
	cp2++;
      }
      f = 0.0;
      if (strncmp(cp, "-Inf", 4) == 0)
      {
	f = -INFINITY;
      }
      else if (strncmp(cp, "+Inf", 4) == 0)
      {
	f = INFINITY;
      }
      else if (strncmp(cp, "+Zero", 5) == 0)
      {
        f = 0.0;
      }
      else if (strncmp(cp, "-Zero", 5) == 0)
      {
	f = -0.0;
      }
      else if (strncmp(cp, "q", 1) == 0)
      {
	f = NAN;
      }
      else if (strncmp(cp, "s", 1) == 0)
      {
        f = *((float *) &signalling_nan);
	if (!isnan(f))
	  printf("SNAN is not a NAN!\n");
	if (!is_signalling(f))
          printf("SNAN is not signalling!\n");
      }
      else if ((*cp == '-') || (*cp == '+'))
      {
        strcpy(buff2, cp + 3);
	cp2 = buff2;
	pindex = index(buff2, 'P');
	if (pindex)
        {
          *pindex = '\0';
	  exponent = strtol(pindex + 1, NULL, 10);
	}
	mantissa = strtol(buff2, NULL, 16);
	if (cp[1] == '1')
        {
	  frep = ((exponent + 127) & 0xff) << 23 | mantissa;
	}
	else
	{
	  frep = mantissa;
	}
	f = *((float *) &frep);
	if (*cp == '-')
	  f = -f;
      }
      else if (*cp == '#')
      {
        *except = 1;
      }
      else
      {
        fprintf(stdout /* FIXME */, "Unexpected value: %s\n", cp);
        return -1;
      }
      return f;
}

double parse_double(char *cp, int *except)
{
char *cp2;
double d;
static char buff2[80];
char *pindex;
int exponent;
unsigned long long mantissa;
int i;
unsigned long long frep;

      *except = 0;
      cp2 = cp;
      while (*cp2 && *cp2 != ' ')
      {
	*cp = tolower(*cp);
	cp2++;
      }
      d = 0.0;
      if (strncmp(cp, "-Inf", 4) == 0)
      {
	d = -INFINITY;
      }
      else if (strncmp(cp, "+Inf", 4) == 0)
      {
	d = INFINITY;
      }
      else if (strncmp(cp, "+Zero", 5) == 0)
      {
        d = 0.0;
      }
      else if (strncmp(cp, "-Zero", 5) == 0)
      {
	d = -0.0;
      }
      else if (strncmp(cp, "q", 1) == 0)
      {
	d = NAN;
      }
      else if (strncmp(cp, "s", 1) == 0)
      {
        d = *((float *) &signalling_nan_d);
	if (!isnan(d))
	  printf("SNAN is not a NAN!\n");
	if (!is_signalling(d))
          printf("SNAN is not signalling!\n");
      }
      else if ((*cp == '-') || (*cp == '+'))
      {
        strcpy(buff2, cp + 3);
	cp2 = buff2;
	pindex = index(buff2, 'P');
	if (pindex)
        {
          *pindex = '\0';
	  exponent = strtol(pindex + 1, NULL, 10);
	}
	mantissa = strtoll(buff2, NULL, 16);
	if (cp[1] == '1')
        {
	  frep = ((long long)((exponent + 1023) & 0x7ff)) << 52 | mantissa;
	}
	else
	{
	  frep = mantissa;
	}
	d = *((double *) &frep);
	if (*cp == '-')
	  d = -d;
      }
      else if (*cp == '#')
      {
        *except = 1;
      }
      else
      {
        fprintf(stdout /* FIXME */, "Unexpected value: %s\n", cp);
        return -1;
      }
      return d;
}

static int label_number = 0;
static int test_number = 0;

void riscv_test(FILE *gen_file, int result_type, int op, float f1, float f2, float f3, double expected_double, float expected,
  int expected_int, int exceptions_raised , char *description)
{
unsigned int v;
unsigned long long u;
int run_test;

  run_test = 1;

#if 0
  if ((/* (op == OP_ADD) || (op == OP_SUB) || (op == OP_MUL) || (op == OP_DIV) || (op == OP_SQRT) || */ /* (op == OP_ABS) || (op == OP_NEG) || (op == OP_MAX) || (op == OP_ISFINITE) || (op == OP_ISNORMAL) || (op == OP_ISSIGNALLING) || (op == OP_FMA) )*/ ((result_type != TYPE_FLOAT) || (!isnan(expected))) && (op != OP_MAXMAG) && (op != OP_ISZERO) && (op != OP_ISSIGNED) && (op != OP_FMA) ) /* && (!is_signalling(f1)) && (!is_signalling(f2)) */ )
#endif

#if 0
  if (result_type != TYPE_QUAD)
  {
    run_test = 0;
  }
#endif

  if ((result_type == TYPE_QUAD) && (test_quad == 0))
  {
    run_test = 0;
  }

  if (((is_signalling(f1)) || (is_signalling(f2))) && (test_signalling == 0))
  {
    run_test = 0;
  }

  if (((is_signalling(f1)) || (is_signalling(f2))) && (op == OP_MAX) && (test_signalling_max == 0))
  {
    run_test = 0;
  }

  if ((op == OP_FMA) && (test_fma == 0))
  {
    run_test = 0;
  }

  if ((op != OP_FMA) && test_fma_only)
  {
    run_test = 0;
  }

  if (isnan(f1) && (op == OP_NEG))
  {
    /* In IEEE 754:2008, Neg is non-arithmetic, so signalling nans
     * won't raise an exception.
     */
    run_test = 0;
  }

  if (isnan(f1) && (op == OP_ISSIGNED) && (test_issigned_nan == 0))
  {
    run_test = 0;
  }

  if ((op == OP_MAXMAG) && (test_maxmag == 0))
  {
    run_test = 0;
  }

  if (run_test)
  { 
    test_number++;
    fprintf(gen_file, "\n");
    fprintf(gen_file, "\t# Test %d\n", test_number);
    fprintf(gen_file, "\t# %s\n", description);
    fprintf(gen_file, "\n");
    fprintf(gen_file, "test%d:\n", test_number);
    v = *((unsigned int *) &f1);
    fprintf(gen_file, "\taddi a1, a1, 1\n");
    fprintf(gen_file, "\tli t0, 0x%x\t #%.8a\n", v, f1);
    fprintf(gen_file, "\tfmv.s.x f1, t0\n");
    v = *((unsigned int *) &f2);
    fprintf(gen_file, "\tli t0, 0x%x\t #%.8a\n", v, f2);
    fprintf(gen_file, "\tfmv.s.x f2, t0\n");
    if (op == OP_FMA)
    {
      v = *((unsigned int *) &f3);
      fprintf(gen_file, "\tli t0, 0x%x\t #%.8a\n", v, f3);
      fprintf(gen_file, "\tfmv.s.x f3, t0\n");
    }

    fprintf(gen_file, "\tcsrrci zero, fflags, 0x1f\n");
    switch (op)
    {
      case OP_ADD:
        fprintf(gen_file, "\tfadd.s f4, f1, f2\n");
	break;

      case OP_SUB:
	fprintf(gen_file, "\tfsub.s f4, f1, f2\n");
	break;

      case OP_MUL:
	fprintf(gen_file, "\tfmul.s f4, f1, f2\n");
	break;

      case OP_DIV:
	fprintf(gen_file, "\tfdiv.s f4, f1, f2\n");
	break;

      case OP_SQRT:
	fprintf(gen_file, "\tfsqrt.s f4, f1\n");
	break;

      case OP_MAX:
	 fprintf(gen_file, "\tfmax.s f4, f1, f2\n");
	 break;

      case OP_ABS:
	fprintf(gen_file, "\tfsgnjx.s f4, f1, f1\n");
	break;

      case OP_NEG:
	fprintf(gen_file, "\tfsgnjn.s f4, f1, f1\n");
	break;

      case OP_COPY:
	fprintf(gen_file, "\tfmv.s f4, f1\n");
	break;

      case OP_FMA:
	fprintf(gen_file, "\tfmadd.s f4, f1, f2, f3\n");
	break;

      case OP_CFF:
#if 0
	printf("CFF: result_type = %d\n", result_type);
	printf("CFF: description = %s\n", description);
	printf("expected_double = %.12a\n", expected_double);
#endif
	switch (result_type)
	{
	  case TYPE_DOUBLE:
	    fprintf(gen_file, "\tfcvt.d.s f4, f1\n");
	    break;
	  case TYPE_QUAD:
	    printf("generated Q test\n");
	    printf("test_quad = %d\n", test_quad);
	    fprintf(gen_file, "\t.word 0x46008253\t# fcvt.q.s d4, f1\n");
	    break;
	} 
	break;

      case OP_ISFINITE:
	fprintf(gen_file, "\tfclass.s a0, f1\n");
	fprintf(gen_file, "\tandi a0, a0, 0x%x\n", (1 << 1) | (1 << 2) | (1 << 3) | (1 << 4) | (1 << 5) | (1 << 6));
	fprintf(gen_file, "\tsltiu a0, a0, 1\n");
	fprintf(gen_file, "\txori a0, a0, 1\n");
	break;

      case OP_ISNORMAL:
	fprintf(gen_file, "\tfclass.s a0, f1\n");
        fprintf(gen_file, "\tandi a0, a0, 0x%x\n", (1 << 1) | (1 << 6));
	fprintf(gen_file, "\tsltiu a0, a0, 1\n");
	fprintf(gen_file, "\txori a0, a0, 1\n");
        break;

      case OP_ISSIGNALLING:
        fprintf(gen_file, "\tfclass.s a0, f1\n");
        fprintf(gen_file, "\tandi a0, a0, 0x%x\n", 1 << 8);
        fprintf(gen_file, "\tsltiu a0, a0, 1\n");
	fprintf(gen_file, "\txori a0, a0, 1\n");
        break;

      case OP_ISINFINITE:
	fprintf(gen_file, "\tfclass.s a0, f1\n");
        fprintf(gen_file, "\tandi a0, a0, 0x%x\n", (1 << 7) | 1);
        fprintf(gen_file, "\tsltiu a0, a0, 1\n");
        fprintf(gen_file, "\txori a0, a0, 1\n");
	break;

      case OP_ISNAN:
	/* An alternative way to check for NAN is not equal to self */
	fprintf(gen_file, "\tfclass.s a0, f1\n");
        fprintf(gen_file, "\tandi a0, a0, 0x%x\n", (1 << 8) | (1 << 9));
        fprintf(gen_file, "\tsltiu a0, a0, 1\n");
        fprintf(gen_file, "\txori a0, a0, 1\n");
        break;

      case OP_ISSUBNORMAL:
	fprintf(gen_file, "\tfclass.s a0, f1\n");
        fprintf(gen_file, "\tandi a0, a0, 0x%x\n", (1 << 5) | (1 << 2));
        fprintf(gen_file, "\tsltiu a0, a0, 1\n");
        fprintf(gen_file, "\txori a0, a0, 1\n");
        break;

      case OP_ISZERO:
	fprintf(gen_file, "\tfclass.s a0, f1\n");
        fprintf(gen_file, "\tandi a0, a0, 0x%x\n", (1 << 4) | (1 << 3));
	fprintf(gen_file, "\tsltiu a0, a0, 1\n");
        fprintf(gen_file, "\txori a0, a0, 1\n");
        break;

      default:
	printf("Unsupported operation: %d\n", op);
	exit(-1);
	break;
    }

    if (test_fcsr)
    {
      fprintf(gen_file, "\tcsrrw a2, fflags, zero\n");
      fprintf(gen_file, "\tli t0, 0x%x\n", exceptions_raised);
      fprintf(gen_file, "\tbeq t0, a2, test%da\n", test_number);
      fprintf(gen_file, "\tret\n");
      fprintf(gen_file, "test%da:\n", test_number);
    }

    switch (result_type)
    {
      case TYPE_FLOAT:
        fprintf(gen_file, "\tfmv.x.s a0, f4\n");
        v = *((unsigned int *) &expected);
        fprintf(gen_file, "\tli t0, 0x%lx\t #%.8a\n", v & 0x80000000 ? v + 0xffffffff00000000 : v, expected);
	break;

      case TYPE_DOUBLE:
        fprintf(gen_file, "\tfmv.x.d a0, f4\n");
        u = *((unsigned long long *) &expected_double);
        fprintf(gen_file, "\tli t0, 0x%llx\t #%.12la\n", u, expected_double);
	break;

      case TYPE_QUAD:
	fprintf(gen_file, "\t.word 0x00414027\t# fsq f4, 0(sp)\n");
        fprintf(gen_file, "\tld a0, 8(sp)\n"); 
	break;

      case TYPE_INT:
        fprintf(gen_file, "\tli t0, 0x%x\n", expected_int);
	break;
    }

    fprintf(gen_file, "\tbeq t0, a0, test%d\n", test_number + 1);
    fprintf(gen_file, "\tret\n");
    fprintf(gen_file, "\n");
  }
}


void write_end_of_tests(FILE *f)
{
  test_number++;
  fprintf(f, "test%d:\n", test_number);
}

void hol_float(FILE *gen_file, float f)
{
unsigned long l;

  l = *((unsigned long*) &f);

  fprintf(gen_file, "<| ");
  fprintf(gen_file, "Sign := %dw; ", (l & 0x80000000) ? 1 : 0);
  fprintf(gen_file, "Exponent := %ldw; ", (l >> 23) & 0xff);
  fprintf(gen_file, "Significand := 0x%lxw ", (l & 0x7fffff));
  fprintf(gen_file, "|> : (23,8)float");
}

void hol_preamble(FILE *gen_file)
{
  fprintf(gen_file, "load \"testutils\";\n");
  fprintf(gen_file, "load \"bossLib\";\n");
  fprintf(gen_file, "load \"binary_ieeeLib\";\n");
  fprintf(gen_file, "load \"fp64Syntax\";\n");
  fprintf(gen_file, "open HolKernel;\n");
  fprintf(gen_file, "open boolLib;\n");
  fprintf(gen_file, "open testutils;\n");
  fprintf(gen_file, "open bossLib;\n");
  fprintf(gen_file, "open binary_ieeeLib;\n");
  fprintf(gen_file, "\n");
}

void hol_test(FILE *gen_file, int result_type, int op, float f1, float f2, float f3, double expected_double, float expected,
  int expected_int, int exceptions_raised , char *description)
{
int run_test = 1;

  if ((op == OP_FMA) && (test_fma == 0))
  {
    run_test = 0;
  }

  if ((op != OP_FMA) && test_fma_only)
  {
    run_test = 0;
  }

  if (run_test)
  {
    fprintf(gen_file, "(* %s *)\n", description);
    fprintf(gen_file, "val f1 = ``");
    hol_float(gen_file, f1);
    fprintf(gen_file, "``;\n");
    fprintf(gen_file, "val f2 = ``");
    hol_float(gen_file, f2);
    fprintf(gen_file, "``;\n");
    fprintf(gen_file, "val f3 = ``");
    hol_float(gen_file, f3);
    fprintf(gen_file, "``;\n");
    fprintf(gen_file, "val expected = ``");
    hol_float(gen_file, expected);
    fprintf(gen_file, "``;\n");
    fprintf(gen_file, "EVAL ``float_equal ^expected (SND(float_mul_add roundTiesToEven ^f1 ^f2 ^f3))``;\n");
  }
}

int main(int argc, char **argv)
{
static char buff[80];
int precision;
int op;
int rounding;
int exceptions_enabled;
int exceptions_raised;
int expected_int;
int result_type;
int result_int;
float result_float;
double result_double;
float expected_float;
double expected_double;
int result_is_int;
int i;
char *cp;
char *cp2;
float f;
float f2;
float f3;
double d;
double d2;
double d3;
int raised;
FILE *gen_file;
FILE *hol_file;
int opt;

  while ((opt = getopt(argc, argv, "Fcmqs")) != -1)
  {
    switch (opt)
    {
      case 'F':
        test_fma = 1;
	test_fma_only = 1;
	break;
      case 'M':
	test_maxmag = 1;
	break;
      case 'c':
	test_fcsr = 1;
	break;
      case 'm':
	test_issigned_nan = 1;
	break;
      case 'q':
	test_quad = 1;
	break;
      case 's':
	test_signalling = 1;
	break;
    }
  }

  /* Skip everything up to the first blank line, ie. the copyright header */
  buff[0] = '\0';
  while (!feof(stdin) && (buff[0] != '\n'))
  {
    fgets(buff, sizeof(buff), stdin);
    /* printf("%s", buff); */
  }

  gen_file = fopen("generated.s", "w");
  if (gen_file == NULL)
  {
    fprintf(stderr, "Couldn't write to generated.s\n");
    return -1;
  }
  fprintf(gen_file, "\tla sp, _stack\n");
  fprintf(gen_file, "\tli a1, 1\n");
  fprintf(gen_file, "\tla t0, _tests\n");
  fprintf(gen_file, "\tjalr 0(t0)\n");
  fprintf(gen_file, "\tj _fail\n");
  fprintf(gen_file, "_tests:\n");

  hol_file = fopen("generated.sml", "w");
  if (hol_file == NULL)
  {
    fprintf(stderr, "Couldn't write to generated.sml\n");
    return -1;
  }
  hol_preamble(hol_file);

  while (!feof(stdin))
  {
    buff[1] = 42;
    fgets(buff, sizeof(buff), stdin);
    /* printf("*** %s", buff);  */

    if (buff[0] != '\n')
    {
      cp = buff;
      if (strncmp(cp, "b32", 3) == 0)
      {
        precision = 32;
	cp += 3;
	result_type = TYPE_FLOAT; /* result type defaults to the same as argument type */
      }
      else
      {
        fprintf(stderr, "Unsupported precision: %s\n", buff);
        return -1;
      }
      if (strncmp(cp, "b64", 3) == 0)
      {
        /* printf("destination precision = 64\n"); */
	result_type = TYPE_DOUBLE;
	cp += 3;
      }
      else if (strncmp(cp, "b128", 4) == 0)
      {
        /* printf("destination precision = 128\n"); */
	result_type = TYPE_QUAD;
        cp += 4;
      }

      if (strncmp(cp, "?f", 2) == 0)
      {
        op = OP_ISFINITE;
	cp += 2;
      }
      else if (strncmp(cp, "?i", 2) == 0)
      {
        op = OP_ISINFINITE;
	cp += 2;
      }
      else if (strncmp(cp, "?N", 2) == 0)
      {
        op = OP_ISNAN;
	cp += 2;
      }
      else if (strncmp(cp, "?n", 2) == 0)
      {
        op = OP_ISNORMAL;
	cp += 2;
      }
      else if (strncmp(cp, "?sN", 3) == 0)
      {
        op = OP_ISSIGNALLING;
        cp += 3;
      }
      else if (strncmp(cp, "?s", 2) == 0)
      {
        op = OP_ISSUBNORMAL;
        cp += 2;
      }
      else if (strncmp(cp, "?-", 2) == 0)
      {
        op = OP_ISSIGNED;
        cp += 2;
      }
      else if (strncmp(cp, "?0", 2) == 0)
      {
        op = OP_ISZERO;
        cp += 2;
      }
      else if (strncmp(cp, "A", 1) == 0)
      {
        op = OP_ABS;
        cp += 1;
      }
      else if (strncmp(cp, "cp", 2) == 0)
      {
        op = OP_COPY;
        cp += 2;
      }
      else if (strncmp(cp, "~", 1) == 0)
      {
        op = OP_NEG;
        cp += 1;
      }
      else if (strncmp(cp, "+", 1) == 0)
      {
        op = OP_ADD;
        cp += 1;
      }
      else if (strncmp(cp, "-", 1) == 0)
      {
        op = OP_SUB;
        cp += 1;
      }
      else if (strncmp(cp, "*+", 2) == 0)
      {
        op = OP_FMA;
        cp += 2;
      }
      else if (strncmp(cp, "*", 1) == 0)
      {
        op = OP_MUL;
        cp += 1;
      }
      else if (strncmp(cp, "/", 1) == 0)
      {
        op = OP_DIV;
        cp += 1;
      }
      else if (strncmp(cp, "V", 1) == 0)
      {
        op = OP_SQRT;
        cp += 1;
      }
      else if (strncmp(cp, ">A", 2) == 0)
      {
        op = OP_MAXMAG;
        cp += 2;
      }
      else if (strncmp(cp, ">C", 2) == 0)
      {
        op = OP_MAX;
        cp += 2;
      }
      else if (strncmp(cp, "cff", 3) == 0)
      {
        op = OP_CFF;
	cp += 3;
      }
      else if (*cp == ' ')
      {
      }
      else
      {
	fclose(gen_file);
        fprintf(stdout /* FIXME */, "Unsupported operation: %s\n", buff);
	return -1;
      }
      while (*cp == ' ')
        cp++;
      if (strncmp(cp, "=0", 2) == 0)
      {
        rounding = ROUNDING_RNE;
	cp += 2;
      }
      else if (strncmp(cp, "-Inf", 4) == 0)
      {
        rounding = ROUNDING_DOWN;
	cp += 4;
      }
      else
      {
        fprintf(stderr, "Unsupported rounding mode: %s\n", buff);
        return -1;
      }
      while (*cp == ' ')
        cp++;

      exceptions_enabled = 0;
      while (*cp && (*cp != ' '))
      {
        if (*cp == 'i')
	{
          exceptions_enabled |= EXCEPT_INVALID;
	  cp++;
	}
	else 
	{
          break; /* anything else is the start of the next field */
	}
      }
      while (*cp && (*cp == ' '))
        cp++;

      f = parse_float(cp, &raised);

      while (*cp && (*cp != ' '))
        cp++;

      while (*cp && (*cp == ' '))
        cp++;

      /* If the next token isn't ->, there is a second argument */
      if (strncmp(cp, "->", 2) != 0)
      {
	f2 = parse_float(cp, &raised);
	while (*cp && (*cp != ' '))
          cp++;
	while (*cp && (*cp == ' '))
          cp++;
      }
      else
      {
        f2 = 0.0;
      }

      /* If the next token isn't ->, there is a third argument */
      if (strncmp(cp, "->", 2) != 0)
      {
        f3 = parse_float(cp, &raised);
        while (*cp && (*cp != ' '))
          cp++;
        while (*cp && (*cp == ' '))
          cp++;
      }
      else
      {
        f3 = 0.0;
      }

      if (*cp == 0)
      {
        /* When we reach the table of constants that doesn't have a ->,
	 * we've reached the end.
	 */
        write_end_of_tests(gen_file);
	return 0;
      }

      if (strncmp(cp, "->", 2) != 0)
      {
        fprintf(stderr, "Expecting ->: %s (%s)\n", buff, cp);
	return -1;
      }
      else
      {
        cp += 2;
      }

      while (*cp && (*cp == ' '))
        cp++;

      if (strncmp(cp, "0x", 2) == 0)
      {
        sscanf(cp + 2, "%x", &expected_int);
	result_is_int = -1;
	result_type = TYPE_INT;
      }
      else
      {
	if (result_type == TYPE_FLOAT)
	{
          expected_float = parse_float(cp, &raised);
	}
	else if (result_type == TYPE_DOUBLE)
	{
	  expected_double = parse_double(cp, &raised);
        }
	result_is_int = 0;
      }

      while (*cp && (*cp != ' '))
        cp++;
      while (*cp == ' ')
        cp++;

      exceptions_raised = 0;
      while (*cp)
      {
        switch (*cp)
        {
	  case 'i':
	    exceptions_raised |= EXCEPT_INVALID;
	    break;
	  case 'o':
	    exceptions_raised |= EXCEPT_OVERFLOW;
	    break;
	  case 'u':
	    exceptions_raised |= EXCEPT_UNDERFLOW;
	    break;
	  case 'x':
	    exceptions_raised |= EXCEPT_INEXACT;
	    break;
	  case 'z':
	    exceptions_raised |= EXCEPT_DIVZERO;
	    break;
	  case '\n': /* ignore the newline at the end of the line */
	    break;
	  default:
	    printf("unrecognized exception, rest of line = %s\n", cp);
	    break;
        }
	cp++;
      }

 #if 0
      printf("%s", buff);
      printf("input = %.7a, expecting 0x%x\n", d, expected_int);
      if ((d == 0) && (copysign(1.0, d) < 0.0))
        printf("negative zero\n");
#endif

      /* Do the test on the local host */
      d = (double) f;
      d2 = (double) f2;
      d3 = (double) f3;

      switch (op)
      {
        case OP_ISFINITE:
          result_int = isfinite(d);
	  break;
	case OP_ISINFINITE:
	  result_int = (isinf(d) != 0);
          break;
	case OP_ISNAN:
	  result_int = isnan(d);
	  break;
	case OP_ISNORMAL:
	  result_int = isnormal(f);
	  break;
	case OP_ISSUBNORMAL:
	  result_int = (fpclassify(f) == FP_SUBNORMAL);
          break;
	case OP_ISSIGNALLING:
	  result_int = isnan(f);
	  if (result_int && !is_signalling(f))
          {
            result_int = 0;
	  }
	  break;
	case OP_ISSIGNED:
	  result_int = copysign(1.0, d) < 0.0;
	  /* printf("copysign = %lf\n", copysign(1.0, d)); */
	  break;
	case OP_ISZERO:
	  result_int = /* iszero(f); */ (fpclassify(d) == FP_ZERO);
	  break;
	case OP_ABS:
	  result_float = fabs(f);
	  break;
	case OP_COPY:
	  result_float = f;
	  break;
	case OP_NEG:
	  result_float = -f;
	  break;
	case OP_ADD:
	  result_float = f + f2;
	  break;
	case OP_SUB:
	  result_float = f - f2;
          break;
	case OP_MUL:
	  result_float = f*f2;
	  break;
        case OP_DIV:
	  result_float = f/f2;
	  break;
	case OP_SQRT:
	  result_float = sqrtf(f);
	  break;
	case OP_FMA:
	  result_float = fmaf(f, f2, f3); /* expected_float; */ /* FIXME */
	  break;
	case OP_MAX:
	  if (isnan(f2))
          {
            if (is_signalling(f2))
	      result_float = NAN;
	    else
              result_float = f;
	  }
	  else if (isnan(f))
	  {
	    if (is_signalling(f))
	      result_float = NAN;
	    else
	      result_float = f2;
	  }
	  else if (f < f2)
	    result_float = f2;
	  else if (f > f2)
	    result_float = f;
	  else if ((f == 0) && (copysign(1.0, f) < 0))
	    result_float = f2;
	  else
          {
            result_float = f;
	  }
	  break;
	case OP_MAXMAG:
	  result_float = fmaxf(f, f2);
          break;
	case OP_CFF:
	  result_double = (double) f; /* FIXME */
	  break;
	default:
	  printf("Unsupported operation\n");
	  return -1;
	  break;
      }
      if (raised == 0)
      {
        if (result_is_int)
	{
          if ((op == OP_ISSIGNED) && isnan(f) && (test_issigned_nan == 0))
	  {
            /* skip issigned(nan) */
	  }
	  else if (result_int != expected_int)
          {
	    printf("FAIL %s", buff);
            printf("*** unexpected result %d != %d\n", expected_int, result_int);
	    printf("f = %.8a, d = %.8la\n", f, d);
	    printf("raised = %d\n", raised);
	    printf("\n");
          }
	  else
	  {
	    riscv_test(gen_file, result_type, op, f, f2, f3, 0.0, expected_float, expected_int, exceptions_raised, buff);
	  }
	}
	else if (result_type == TYPE_FLOAT)
        {
	  if ((op == OP_MAXMAG) && (test_maxmag == 0))
	  {
            /* ignore OP_MAXMAG */
          }
	  else if ((memcmp((const void *) &result_float, (const void *) &expected_float, sizeof(float)) != 0) && (!isnan(result_float) || !isnan(expected_float)))
          {
            printf("FAIL %s", buff);
            printf("*** unexpected result %.8a != %.8a\n", expected_float, result_float);
            printf("f1 = %.8a (%.8e), d1 = %.8la\n", f, f, d);
	    printf("f2 = %.8a (%.8e), d1 = %.8la\n", f2, f2, d2);
	    printf("fr = %.8a (%.8e), dr = %.8la\n", expected_float, expected_float, expected_double);
	    for (i=3; i>=0; i--)
              printf("%02x", ((unsigned char *) &expected_float)[i]);
	    printf(" (expected)\n");
	    for (i=3; i>=0; i--)
              printf("%02x", ((unsigned char *) &result_float)[i]);
            printf(" (actual)\n");
	    printf("result = %.8a\n", result_float);
	    printf("\n");
	  }
	  else
	  {
	    riscv_test(gen_file, result_type, op, f, f2, f3, 0.0, expected_float, expected_int, exceptions_raised, buff);
	    hol_test(hol_file, result_type, op, f, f2, f3, 0.0, expected_float, expected_int, exceptions_raised, buff);
	  }
	}
        else if (result_type == TYPE_DOUBLE)
	{
	  if ((memcmp((const void *) &result_double, (const void *) &expected_double, sizeof(double)) != 0) && (!isnan(result_double) || !isnan(expected_double)))

	  {
	    printf("FAIL %s", buff);
	    printf("*** unexpected result %.13la != %.13la\n", expected_double, result_double);
	    printf("*** %08llx %08llx (expected, actual)\n", *((unsigned long long *) &expected_double),
	      *((unsigned long long *) &result_double));
	  }
	  else
	  {
		  riscv_test(gen_file, result_type, op, f, f2, f3, expected_double, 0.0, expected_int, exceptions_raised, buff);
          }
	}
	else if (result_type == TYPE_QUAD)
	{
#if 0
          printf("128 bit result in main()\n");
#endif
	  riscv_test(gen_file, result_type, op, f, f2, f3, expected_double, 0.0, expected_int, exceptions_raised, buff);
	}
      }

    }
  }
  return 0;
}
