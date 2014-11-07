/* Test the `vsetQ_lanes16' ARM Neon intrinsic.  */
/* This file was autogenerated by neon-testgen.  */

/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-save-temps -O0" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"

void test_vsetQ_lanes16 (void)
{
  int16x8_t out_int16x8_t;
  int16_t arg0_int16_t;
  int16x8_t arg1_int16x8_t;

  out_int16x8_t = vsetq_lane_s16 (arg0_int16_t, arg1_int16x8_t, 1);
}

/* { dg-final { scan-assembler "vmov\.16\[ 	\]+\[dD\]\[0-9\]+\\\[\[0-9\]+\\\], \[rR\]\[0-9\]+!?\(\[ 	\]+@.*\)?\n" } } */
/* { dg-final { cleanup-saved-temps } } */
