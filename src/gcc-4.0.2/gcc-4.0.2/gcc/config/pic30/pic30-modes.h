/*
 *  A new mode for C30
 */
#if 0
DEF_MACHMODE(P24Pmode, "P24P", MODE_INT, BITS_PER_UNIT*4, 4, 4, SImode, VOIDmode)
DEF_MACHMODE(P24Tmode, "P24T", MODE_INT, BITS_PER_UNIT*4, 4, 4, SImode, VOIDmode)
#endif

/* 
    P24PSV represents a small (__psv__) pointer
    P24PROG represetns a large (__prog__) pointer
*/

TARGET_POINTER_MODE(P24PROG, 24, 4);  // __prog__ pointer
TARGET_POINTER_MODE(P24PSV,  24, 4);  // __psv__ pointer
TARGET_POINTER_MODE(P16PMP, 16, 2);   // __pmp__ pointer
TARGET_POINTER_MODE(P32EXT, 32, 4);   // __external__ pointer

