/* Wind River GNU/Linux Configuration.
   Copyright (C) 2006, 2007
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Add -te500v1 and -te500v2 options for convenience in generating
   multilibs.  */
#undef CC1_EXTRA_SPEC
#define CC1_EXTRA_SPEC "%{te500v1: -mcpu=8540 -mfloat-gprs=single -mspe=yes -mabi=spe} %{te500v2: -mcpu=8548 -mfloat-gprs=double -mspe=yes -mabi=spe}"

#undef ASM_DEFAULT_SPEC
#define ASM_DEFAULT_SPEC			\
  "%{te500v1:-mppc -mspe -me500 ;		\
     te500v2:-mppc -mspe -me500 ;		\
     :-mppc%{m64:64}}"

/* The GLIBC headers are in /usr/include, relative to the sysroot; the
   uClibc headers are in /uclibc/usr/include.  */
#undef SYSROOT_HEADERS_SUFFIX_SPEC
#define SYSROOT_HEADERS_SUFFIX_SPEC		\
  "%{muclibc:/uclibc}" 

/* The various C libraries each have their own subdirectory.  */
#undef SYSROOT_SUFFIX_SPEC
#define SYSROOT_SUFFIX_SPEC			\
  "%{muclibc:/uclibc ;				\
     msoft-float:/soft-float ;			\
     te500v1:/te500v1 ;				\
     te500v2:/te500v2}"
