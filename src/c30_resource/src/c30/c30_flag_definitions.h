#ifndef _C30_FLAG_DEFS_H
#define _C30_FLAG_DEFS_H

/*
 *  Copyright 2006 Microchip Technology Inc
 */

/*
 *  This file is shared with the compiler sources and resource input file 
 *  Comments and pre-processor statements only!
 */

/*
** Family-specific flags
*/
#define P30F     1<<0
#define P30FSMPS 1<<1
#define P33F     1<<2
#define P24F     1<<3
#define P24H     1<<4

#define FAMILY_MASK (0xFF)
#define CODEGUARD_MASK (0x00FFFF00)

/*
** Flags that identify record types
*/
#define IS_CODEGUARD_ID 0x10000000
#define IS_VECTOR_ID    0x20000000
#define IS_DEVICE_ID    0x40000000

/*
** Flags that are specific to record types
*/

/* IS_DEVICE_ID flags */
#define HAS_DSP    1<<8
#define HAS_EEDATA 1<<9
#define HAS_DMA    1<<10
#define HAS_CODEGUARD 1<<11

/* IS_CODEGUARD_ID flags */
#define FLASH         1<<8
#define RAM           1<<9
#define EEPROM        1<<10
#define NONE          1<<11
#define SMALL         1<<12
#define MEDIUM        1<<13
#define LARGE         1<<14
#define BOOT          1<<15
#define SECURE        1<<16
#define GENERAL       1<<17
#define CODE_PROTECT  1<<18
#define WRITE_PROTECT 1<<19
#define STANDARD      1<<20
#define HIGH          1<<21
#define ON            1<<22
#define OFF           1<<23

#endif
