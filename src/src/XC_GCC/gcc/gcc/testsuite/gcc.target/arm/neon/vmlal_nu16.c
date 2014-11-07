/* Test the `vmlal_nu16' ARM Neon intrinsic.  */
/* This file was autogenerated by neon-testgen.  */

/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-save-temps -O0" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"

void test_vmlal_nu16 (void)
{
  uint32x4_t out_uint32x4_t;
  uint32x4_t arg0_uint32x4_t;
  uint16x4_t arg1_uint16x4_t;
  uint16_t arg2_uint16_t;

  out_uint32x4_t = vmlal_n_u16 (arg0_uint32x4_t, arg1_uint16x4_t, arg2_uint16_t);
}

/* { dg-final { scan-assembler "vmlal\.u16\[ 	\]+\[qQ\]\[0-9\]+, \[dD\]\[0-9\]+, \[dD\]\[0-9\]+\\\[\[0-9\]+\\\]!?\(\[ 	\]+@.*\)?\n" } } */
/* { dg-final { cleanup-saved-temps } } */
