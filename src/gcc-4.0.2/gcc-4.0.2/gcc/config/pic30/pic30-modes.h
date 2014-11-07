/*
 *  A new mode for C30
 */
#if 0
DEF_MACHMODE(P24Pmode, "P24P", MODE_INT, BITS_PER_UNIT*4, 4, 4, SImode, VOIDmode)
DEF_MACHMODE(P24Tmode, "P24T", MODE_INT, BITS_PER_UNIT*4, 4, 4, SImode, VOIDmode)
#endif

/*  new gcc won't let us use a new mode that is the same as another mode, like
    above ... it will choose this mode for other things which is crap because
    there is no distinct pointer type and no way to refer to the type of
    the variable (and hence its attributes) after its an RTX

    P24p represents a small (__psv__) pointer
    P24P represetns a large (__prog__) pointer
    We have to cheat and use an extra bit for one.
*/

#if 0
#define TARGET_POINTER_MODE_FITS 64
#define TARGET_POINTER_MODE_BYTES 8
#endif

#if 1
TARGET_POINTER_MODE(P24PROG, 24, 4);
TARGET_POINTER_MODE(P24PSV,  24, 4);
#endif

#if 0
FRACTIONAL_INT_MODE(P24PROG, 24, 4);
FRACTIONAL_INT_MODE(P24PSV, 25, 4);
#endif

#if 0
PARTIAL_INT_MODE2(SI, P24PROG);
PARTIAL_INT_MODE2(SI, P24PSV);
#endif

