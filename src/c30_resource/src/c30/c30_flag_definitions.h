#ifndef _C30_FLAG_DEFS_H
#define _C30_FLAG_DEFS_H

/*
 *  Copyright 2006 Microchip Technology Inc
 */

/*
 *  This file is shared with the compiler sources and resource input file 
 *  Comments and pre-processor statements only!
 */

#define P30F     1
#define P30FSMPS 2
#define P33F     4
#define P24F     8
#define P24H     16

#define FAMILY_MASK (0xFF)

#define HAS_DSP    256
#define HAS_EEDATA 512
#define HAS_DMA    1024

#define IS_VECTOR_ID 0x20000000
#define IS_DEVICE_ID 0x40000000
#endif
