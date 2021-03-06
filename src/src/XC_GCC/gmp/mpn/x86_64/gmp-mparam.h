/* AMD K8 gmp-mparam.h -- Compiler/machine parameter header file.

Copyright 1991, 1993, 1994, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
2008, 2009 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.  */

#define BITS_PER_MP_LIMB 64
#define BYTES_PER_MP_LIMB 8

/* 2200 MHz Opteron / rev A / 1024 Kibyte cache / socket 940 */

/* Generated by tuneup.c, 2009-01-14, gcc 3.4 */

#define MUL_KARATSUBA_THRESHOLD          28
#define MUL_TOOM3_THRESHOLD              97
#define MUL_TOOM44_THRESHOLD            406

#define SQR_BASECASE_THRESHOLD            0  /* always (native) */
#define SQR_KARATSUBA_THRESHOLD          38
#define SQR_TOOM3_THRESHOLD             133
#define SQR_TOOM4_THRESHOLD             547

#define MULLOW_BASECASE_THRESHOLD        27
#define MULLOW_DC_THRESHOLD              28
#define MULLOW_MUL_N_THRESHOLD          199

#define DIV_SB_PREINV_THRESHOLD           0  /* always */
#define DIV_DC_THRESHOLD                 74
#define POWM_THRESHOLD                  146

#define MATRIX22_STRASSEN_THRESHOLD      24
#define HGCD_THRESHOLD                  143
#define GCD_DC_THRESHOLD                529
#define GCDEXT_DC_THRESHOLD             639
#define JACOBI_BASE_METHOD                1

#define MOD_1_NORM_THRESHOLD              0  /* always */
#define MOD_1_UNNORM_THRESHOLD            0  /* always */
#define MOD_1_1_THRESHOLD                 4
#define MOD_1_2_THRESHOLD                 7
#define MOD_1_4_THRESHOLD                64
#define USE_PREINV_DIVREM_1               1  /* native */
#define USE_PREINV_MOD_1                  1
#define DIVEXACT_1_THRESHOLD              0  /* always (native) */
#define MODEXACT_1_ODD_THRESHOLD          0  /* always (native) */

#define GET_STR_DC_THRESHOLD             18
#define GET_STR_PRECOMPUTE_THRESHOLD     32
#define SET_STR_DC_THRESHOLD            248
#define SET_STR_PRECOMPUTE_THRESHOLD   2124

#define MUL_FFT_TABLE  { 432, 928, 2624, 3840, 11264, 36864, 147456, 327680, 0 }
#define MUL_FFT_MODF_THRESHOLD          656
#define MUL_FFT_THRESHOLD              7936

#define SQR_FFT_TABLE  { 432, 928, 2368, 4352, 11264, 28672, 114688, 327680, 0 }
#define SQR_FFT_MODF_THRESHOLD          560
#define SQR_FFT_THRESHOLD              7936

#define MUL_FFT_TABLE2 {{1,4}, {337,5}, {673,6}, {1729,7}, {1793,6}, {2017,7}, {5633,8}, {11009,9}, {11777,8}, {14593,9}, {15873,8}, {16897,9}, {22017,10}, {23553,9}, {29697,10}, {31745,9}, {36353,10}, {39937,9}, {44545,10}, {48129,9}, {50689,10}, {56833,11}, {63489,10}, {78337,11}, {79873,10}, {86017,11}, {88065,10}, {92161,11}, {96257,10}, {106497,11}, {129025,10}, {141313,11}, {145409,10}, {146433,11}, {161793,10}, {167937,11}, {227329,12}, {258049,11}, {326657,12}, {389121,11}, {424961,13}, {516097,12}, {520193,11}, {528385,10}, {538625,11}, {547841,10}, {552961,11}, {587777,12}, {651265,11}, {718849,10}, {719873,12}, {782337,11}, {787457,10}, {791553,11}, {796673,10}, {802817,11}, {849921,10}, {850945,12}, {913409,11}, {915457,13}, {1040385,12}, {1044481,11}, {1112065,12}, {1175553,11}, {1243137,12}, {1306625,11}, {1374209,12}, {1437697,13}, {1564673,12}, {1568769,11}, {1581057,12}, {1585153,11}, {1595393,12}, {1597441,11}, {1630209,12}, {1699841,11}, {1761281,12}, {1830913,14}, {2080769,13}, {2088961,12}, {2486273,13}, {2613249,12}, {3010561,13}, {3137537,12}, {3534849,13}, {3661825,12}, {3928065,13}, {3964929,14}, {4014081,13}, {4046849,14}, {4136961,13}, {4186113,12}, {4452353,13}, {4710401,12}, {4976641,13}, {5234689,12}, {5238785,13}, {5349377,12}, {5353473,13}, {5758977,12}, {5763073,14}, {6275073,13}, {7856129,14}, {8372225,13}, {9953281,14}, {10469377,13}, {12050433,14}, {12566529,13}, {13623297,14}, {14663681,13}, {15196161,15}, {16744449,14}, {16760833,13}, {17293313,14}, {18857985,13}, {19394561,14}, {MP_SIZE_T_MAX,0}}

#define SQR_FFT_TABLE2 {{1,4}, {305,5}, {609,6}, {1601,7}, {4737,8}, {4865,7}, {5121,8}, {11009,9}, {11777,8}, {13057,9}, {13825,10}, {15361,9}, {15873,8}, {16129,9}, {22017,10}, {23553,9}, {28161,10}, {31745,9}, {36353,10}, {39937,9}, {42497,10}, {56321,11}, {63489,10}, {89601,11}, {96257,10}, {107521,12}, {126977,11}, {129025,10}, {135169,11}, {137217,10}, {139265,11}, {163841,10}, {173057,11}, {195073,9}, {196097,11}, {196609,10}, {201729,11}, {212993,12}, {217089,11}, {221185,12}, {258049,11}, {260609,10}, {261121,9}, {261633,11}, {292865,10}, {296961,11}, {299009,10}, {302081,11}, {325633,12}, {389121,11}, {392193,9}, {392705,11}, {393217,13}, {401409,11}, {404481,13}, {421889,11}, {424961,13}, {516097,12}, {520193,11}, {526337,10}, {532481,11}, {542721,10}, {543745,11}, {593921,12}, {598017,11}, {608257,12}, {610305,11}, {616449,12}, {651265,11}, {653313,10}, {687617,11}, {718849,10}, {749569,12}, {782337,11}, {784385,10}, {788481,11}, {793601,10}, {800769,11}, {802817,10}, {813057,11}, {850945,12}, {913409,11}, {917505,13}, {1040385,12}, {1044481,11}, {1113089,12}, {1175553,11}, {1243137,12}, {1309697,11}, {1347585,12}, {1351681,11}, {1368065,12}, {1437697,11}, {1503233,13}, {1564673,12}, {1568769,11}, {1628161,12}, {1839105,14}, {1851393,12}, {1884161,14}, {2080769,13}, {2088961,12}, {2488321,13}, {2613249,12}, {3010561,13}, {3137537,12}, {3403777,13}, {3661825,12}, {3928065,14}, {4177921,13}, {4186113,12}, {4452353,13}, {4710401,12}, {4976641,13}, {5234689,12}, {5500929,13}, {5758977,12}, {5763073,14}, {6275073,13}, {6283265,12}, {6549505,13}, {7856129,15}, {8011777,14}, {8060929,15}, {8355841,14}, {8372225,13}, {9953281,14}, {10469377,13}, {12050433,14}, {12566529,13}, {13623297,14}, {14663681,13}, {15196161,15}, {16744449,14}, {16760833,13}, {17293313,14}, {23052289,15}, {25133057,14}, {29343745,16}, {MP_SIZE_T_MAX,0}}

#define INV_NEWTON_THRESHOLD             47
#define BINV_NEWTON_THRESHOLD            18
