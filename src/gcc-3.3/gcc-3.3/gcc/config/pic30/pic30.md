;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;- Machine description for GNU compiler
;;- Microchip dsPIC30 version.
;; Copyright (C) 1994, 1995, 1996, 1997 Free Software Foundation, Inc.

;; Contributed by John Elliott (john.elliott@microchip.com)

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	This is the machine description for the Microchip dsPIC.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;- instruction definitions
;;
;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.
;;
;; dsPIC30 specific remarks:
;;
;; 1) BITS_PER_UNIT = 8
;;
;; 2) GCC to dsPIC30 data type mappings:
;;    QImode => char (8 bits or 1 reg).
;;    HImode => short/int (16 bits or 1 reg).
;;    SImode => long (32 bits or 2 regs).
;;    DImode => long long (64 bits or 4 regs).
;;    SFmode => single precision float (32 bits or 2 regs).
;;    DFmode => double precision float (64 bits or 4 regs).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Condition code settings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clobber - value of cc is unknown
;; unchanged - insn does not affect cc
;; set - set to agree with the results of the operation
;; change0 - insn does not affect cc but it does modify operand 0
;;	cc only changed if the item previously set into the condition code
;;	has been modified.
;; math - value of cc has incorrect C and OV flags
;; move - value of cc has incorrect C and OV flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define_attr "cc" "clobber,unchanged,set,change0,math,move"
  (const_string "clobber"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uses of UNSPEC in this file:

(define_constants
 [(UNSPEC_POPHI		1) ; pop HI
  (UNSPEC_PUSHSHADOW	2) ; push.s -- push shadowed registers
  (UNSPEC_POPSHADOW	3) ; pop.s -- pop shadowed registers
 ]
)

;; UNSPEC_VOLATILE:

(define_constants
 [
  (UNSPECV_BLOCKAGE	0) ; block insn movement
  (UNSPECV_PA		1) ; enable/disable PA
  (UNSPECV_IV		2) ; interrupt vector
  (UNSPECV_PP		3) ; pre-prologue
  (UNSPECV_NOP		4) ; '__builtin_nop' instruction
  (UNSPECV_TBLPAGE	5) ; '__builtin_tblpage' directive
  (UNSPECV_TBLOFFSET	6) ; '__builtin_tbloffset' directive
  (UNSPECV_PSVPAGE	7) ; '__builtin_psvpage' directive
  (UNSPECV_PSVOFFSET	8) ; '__builtin_psvoffset' directive
  (UNSPECV_DIVSD	9) ; '__builtin_divsd' instruction
  (UNSPECV_DIVUD       10) ; '__builtin_divud' instruction
  (UNSPECV_MULSS       11) ; '__builtin_mulss' instruction
  (UNSPECV_MULSU       12) ; '__builtin_mulsu' instruction
  (UNSPECV_MULUS       13) ; '__builtin_mulus' instruction
  (UNSPECV_MULUU       14) ; '__builtin_muluu' instruction
  (UNSPECV_READSFR     15) ; '__builtin_readsfr' instruction
  (UNSPECV_DISI        16) ; disi instruction
  (UNSPECV_WRITESFR    17) ; '__builtin_writesfr' instruction
  (UNSPECV_DMAOFFSET   18) ; dmaoffset
  (UNSPECV_SAC         19) ; __builtin_sac
  (UNSPECV_SACR        20) ; __builtin_sacr
  (UNSPECV_FBCL        21) ; __builtin_fbcl
  (UNSPECV_LAC         22) ; __builtin_lac
  (UNSPECV_SFTAC       23) ; __builtin_sftac
  (UNSPECV_DSPMULT     24) ; __builtin_mpy, etc
  (UNSEPCV_GENLABEL    25) ; __builtin_unique_id
  (UNSPECV_WRITEOSCCONL 26) ; __builtin_write_OSCCONL
  (UNSPECV_WRITEOSCCONH 27) ; __builtin_write_OSCCONH
  (UNSPECV_WRITEONVM   28) ; __builtin_write_NVM
  (UNSPECV_MODSD       29) ; __builtin_modsd
  (UNSPECV_MODUD       30) ; __builtin_modud
  (UNSPECV_DIVMODSD    31) ; __builtin_divmodsd
  (UNSPECV_DIVMODUD    32) ; __builtin_divmodud
 ]
)

;; Hard registers (SFRs)

(define_constants
 [
  (FPREG	14) ; Frame-pointer hard register number
  (SPREG	15) ; Stack-pointer hard register number
  (RCOUNT	16) ; Repeat-count hard register number
 ]
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction scheduling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Memory access attribute.
;; We only care about loads and stores.
;; use -- indirect memory load
;; def -- store to register
;; Everything else is 'etc'.
;;
;; For dealing with RAW stalls, we use the following truth table,
;; where D denotes a store to a wreg (definition), and U denotes an
;; indirect load of a source operand (use).
;;
;;           type   description
;; +---+---+
;; | D | U | 
;; +---+---+
;; | F | F | etc    Insn does not load indirect, does not store to wreg
;; | F | T | use    Insn loads indirect
;; | T | F | def    Insn stores to wreg
;; | T | T | defuse Insn loads indirect, and stores to wreg
;; +---+---+
;;
;; RAW stalls occur when D is followed by U.

(define_automaton "dsPIC30F")

(define_cpu_unit "sched_raw" "dsPIC30F")

(define_insn_reservation "insn_def" 0
  (eq_attr "type" "def")
  "sched_raw")

(define_insn_reservation "insn_use" 1
  (eq_attr "type" "use")
  "sched_raw")

(define_insn_reservation "insn_defuse" 1
  (eq_attr "type" "defuse")
  "sched_raw")

(define_insn_reservation "insn_etc" 0
  (eq_attr "type" "etc")
  "nothing")

;; Insn type.

(define_attr "type"
  "def,use,defuse,etc"
  (const_string "etc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;; dsp instructions
; DSP instructions
;

;; ********* to support automagic generation
;
;(define_insn "movhi_accumulator"
;  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
;        (match_operand: HI 1 "pic30_mode3_operand" "RS<>r"))]
;  ""
;  "*
;   {
;     /* lac %1, %0 */
;     error(\"Automatic generation of DSP instructions not yet supported; \"
;           \"use __builtin_lac() instead\");
;     return \"; bad op\";
;   }
;  "
;  [(set_attr "cc" "unchanged")]
;)  
;
;(define_insn "movhi_accumulator2"
;  [(set (match_operand:HI 0 "pic30_mode3_operand" "=RS<>r")
;        (match_operand:HI 1 "pic30_accumulator_operand" "w"))]
;  ""
;  "*
;   {
;     /* sac %1, %0 */
;     error(\"Automatic generation of DSP instructions not yet supported; \"
;           \"use __builtin_sac() instead\");
;     return \"; bad op\";
;   }
;  "
;  [(set_attr "cc" "unchanged")]
;)
;
;(define_insn "movhi_accumulator3"
;  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
;        (match_operand:HI 1 "immediate_operand" "i"))]
;  ""
;  "*
;   {
;     if (INTVAL(operands[1]) == 0) {
;       /* clr %0 */
;       error(\"Automatic generation of DSP instructions not yet supported; \"
;             \"use __builtin_clr() instead\");
;     } else {
;       /* lac ... */
;       error(\"Automatic generation of DSP instructions not yet supported; \"
;             \"use __builtin_lac() instead\");
;     }
;     return \"; bad op\";
;   }
;  "
;  [(set_attr "cc" "unchanged")]
;)
;
;(define_insn "auto_mac"
;  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
;        (plus:HI
;          (match_operand:HI 1 "pic30_accumulator_operand" "0")
;          (subreg:HI 
;            (mult:SI
;              (sign_extend:SI 
;                (match_operand: HI 2 "pic30_mac_input_operand" "z"))
;              (sign_extend:SI
;                (match_operand: HI 3 "pic30_mac_input_operand" "z"))) 0)))]
;  ""
;  "*
;   {
;     error(\"Automatic generation of DSP instructions not yet supported; \"
;           \"use __builtin_mac() instead\");
;     return \"; bad op\";
;     if (REGNO(operands[2]) < REGNO(operands[3])) {
;       return \"mac %2*%3, %0\";
;     } else {
;       return \"mac %3*%2, %0\";
;     }
;   }
;  "
;  [(set_attr "cc" "unchanged")]
;)
;
;(define_insn "auto_mac1"
;  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
;        (plus:HI
;          (subreg:HI
;            (mult:SI
;              (sign_extend:SI
;                (match_operand: HI 1 "pic30_mac_input_operand" "z"))
;              (sign_extend:SI
;                (match_operand: HI 2 "pic30_mac_input_operand" "z"))) 0)
;          (match_operand:HI 3 "pic30_accumulator_operand" "0")))]
;  ""
;  "*
;   {
;     error(\"Automatic generation of DSP instructions not yet supported; \"
;           \"use __builtin_mac() instead\");
;     return \"; bad op\";
;     if (REGNO(operands[1]) < REGNO(operands[2])) {
;       return \"mac %1*%2, %0\";
;     } else {
;       return \"mac %2*%1, %0\";
;     }
;   }
;  "
;  [(set_attr "cc" "unchanged")]
;)
;
;(define_insn "auto_mpy"
;  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
;        (neg: HI
;          (subreg:HI
;            (mult:SI
;              (sign_extend:SI
;                (match_operand: HI 1 "pic30_mac_input_operand" "z"))
;              (sign_extend:SI
;                (match_operand: HI 2 "pic30_mac_input_operand" "z"))) 0)))]
;  ""
;  "*
;   {
;     error(\"Automatic generation of DSP instructions not yet supported; \"
;           \"use __builtin_mpy() instead\");
;     return \"; bad op\";
;     if (REGNO(operands[1]) < REGNO(operands[2])) {
;       return \"mpy.n %1*%2, %0\";
;     } else {
;       return \"mpy.n %2*%1, %0\";
;     }
;   }
;  "
;  [(set_attr "cc" "unchanged")]
;)
;
;(define_insn "auto_msc"
;  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
;        (minus:HI
;          (match_operand:HI 1 "pic30_accumulator_operand" "0")
;          (subreg:HI
;            (mult:SI
;              (sign_extend:SI
;                (match_operand: HI 2 "pic30_mac_input_operand" "z"))
;              (sign_extend:SI
;                (match_operand: HI 3 "pic30_mac_input_operand" "z"))) 0)))]
;  ""
;  "*
;   {
;     error(\"Automatic generation of DSP instructions not yet supported; \"
;           \"use __builtin_msc() instead\");
;     return \"; bad op\";
;     if (REGNO(operands[2]) < REGNO(operands[3])) {
;       return \"msc %2*%3, %0\";
;     } else {
;       return \"msc %3*%2, %0\";
;     }
;   }
;  "
;  [(set_attr "cc" "unchanged")]
;)
;
;(define_insn "auto_msc1"
;  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
;        (minus:HI
;          (subreg:HI
;            (mult:SI
;              (sign_extend:SI
;                (match_operand: HI 1 "pic30_mac_input_operand" "z"))
;              (sign_extend:SI
;                (match_operand: HI 2 "pic30_mac_input_operand" "z"))) 0)
;          (match_operand:HI 3 "pic30_accumulator_operand" "0")))]
;  ""
;  "*
;   {
;     error(\"Automatic generation of DSP instructions not yet supported; \"
;           \"use __builtin_msc() instead\");
;     return \"; bad op\";
;     if (REGNO(operands[1]) < REGNO(operands[2])) {
;       return \"msc %1*%2, %0\";
;     } else {
;       return \"msc %2*%1, %0\";
;     }
;   }
;  "
;  [(set_attr "cc" "unchanged")]
;)
;
;(define_insn "auto_sftacr"
;  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
;        (ashiftrt:HI
;          (match_dup 0)
;          (match_operand:HI 1 "immediate_operand"       "W")))]
;  "(INTVAL(operands[1]) > 0)"
;  "*
;   {
;     /* sftac %0, #%1 */
;     error(\"Automatic generation of DSP instructions not yet supported; \"
;           \"use __builtin_sftac() instead\");
;     return \"; bad op\";
;   }
;  "
;  [
;    (set_attr "cc" "unchanged")
;  ]
;)
;
;(define_insn "auto_sftacl"
;  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
;        (ashift:HI 
;          (match_dup 0)
;          (match_operand:HI 1 "immediate_operand"       "W")))]
;  "(INTVAL(operands[1]) > 0)"
;  "*
;   { static char buffer[20];
;
;     error(\"Automatic generation of DSP instructions not yet supported; \"
;           \"use __builtin_sftac() instead\");
;     return \"; bad op\";
;     sprintf(buffer,\"sftac %%0, #%d\", -INTVAL(operands[1]));
;     return buffer;
;   }
;  "
;  [(set_attr "cc" "unchanged")]
;)
;
;(define_insn "addacr_hi"
;  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w")
;        (plus:HI
;           (match_operand: HI 1 "pic30_mode3_operand" "RS<>r")
;           (match_operand: HI 2 "pic30_accumulator_operand" "0")))]
;  ""
;  "*
;   {
;     /* add %1, %0 */
;     error(\"Automatic generation of DSP instructions not yet supported; \"
;           \"use __builtin_add() instead\");
;     return \"; bad op\";
;   }
;  "
;  [
;    (set_attr "cc" "unchanged")
;  ]
;)
;
;(define_insn "addacr1_hi"
;  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w")
;        (plus:HI
;           (match_operand: HI 1 "pic30_accumulator_operand" "0")
;           (match_operand: HI 2 "pic30_mode3_operand" "RS<>r")))]
;  ""
;  "*
;   {
;     /* add %2, %0 */
;     error(\"Automatic generation of DSP instructions not yet supported; \"
;           \"use __builtin_add() instead\");
;     return \"; bad op\";
;   }
;  "
;  [
;    (set_attr "cc" "unchanged")
;  ]
;)
;

; ********* to support builtins

(define_insn "addac_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand"  "=w")
        (plus:HI (match_dup 0)
                 (match_operand: HI 1 "pic30_accumulator_operand" "w")))]
  "(REGNO(operands[1]) != REGNO(operands[0]))"
  "add %0"
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "addacr_shiftrt_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w")
        (plus:HI 
           (ashiftrt:HI (match_operand: HI 1 "pic30_mode3_operand" "RS<>r")
                        (match_operand: HI 2 "immediate_operand" "Z"))
           (match_dup 0)))]
  "(INTVAL(operands[2]) > 0)"
  "add %1, #%2, %0"
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "addacr_shiftlt_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w")
        (plus:HI
           (ashift:HI (match_operand: HI 1 "pic30_mode3_operand" "RS<>r")
                      (match_operand: HI 2 "immediate_operand" "Z"))
           (match_dup 0)))]
  "(INTVAL(operands[2]) > 0)"
  "*
   { static char buffer[20];
  
     sprintf(buffer, 80, \"add %%1, #%d, %%0\", -INTVAL(operands[2]));
     return buffer;
   }
  "
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "addacr_noshift_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w")
        (plus:HI
           (match_operand: HI 1 "pic30_mode3_operand" "RS<>r")
           (match_dup 0)))]
  ""
  "add %1, #0, %0"
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "addacr1_noshift_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w")
        (plus:HI
           (match_dup 0)
           (match_operand: HI 1 "pic30_mode3_operand" "RS<>r")))]
  ""      
  "add %1, #0, %0"
  [     
    (set_attr "cc" "unchanged")
  ]
)  

(define_insn "addacr_shiftrt1_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w")
        (plus:HI 
           (match_dup 0)
           (ashiftrt:HI (match_operand: HI 1 "pic30_mode3_operand" "RS<>r")
                      (match_operand: HI 2 "immediate_operand" "Z"))))]
  "(INTVAL(operands[2]) > 0)"
  "add %1, #%2, %0"
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "addacr_shiftlt1_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w")
        (plus:HI
           (ashift:HI (match_operand: HI 1 "pic30_mode3_operand" "RS<>r")
                      (match_operand: HI 2 "immediate_operand" "Z"))
           (match_dup 0)))]
  "(INTVAL(operands[2]) > 0)"
  "*
   { static char buffer[20];
  
     sprintf(buffer, 80, \"add %%1, #%d, %%0\", -INTVAL(operands[2]));
     return buffer;
   }
  "
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "clrac_gen_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w,w,w,w,w,w,w,w,w")
        (const_int 0))
   (set (match_operand: HI 1 "pic30_mac_input_operand"   "=z,i,i,z,z,z,z,z,i")
        (mem:HI
          (match_operand: HI 2 "pic30_xprefetch_operand" " x,i,i,x,x,x,x,x,i")))
   (set (match_operand: HI 3 "pic30_xprefetch_operand"   "=2,i,i,2,i,i,2,i,i")
        (plus:HI
          (match_dup 3)
          (match_operand: HI 4 "immediate_operand"       " Y,i,i,Y,i,i,Y,i,i")))
   (set (match_operand: HI 5 "pic30_mac_input_operand"   "=z,z,z,i,i,z,z,z,i")
        (mem:HI
          (match_operand: HI 6 "pic30_yprefetch_operand" " y,y,y,i,i,y,y,y,i")))
   (set (match_operand: HI 7 "pic30_yprefetch_operand"   "=6,6,i,i,i,i,i,6,i")
        (plus:HI
          (match_dup 7)
          (match_operand: HI 8 "immediate_operand"       " Y,Y,i,i,i,i,i,Y,i")))
  ]
  ""
  "@
   clr %0, [%2]+=%4, %1, [%6]+=%8, %5
   clr %0, [%6]+=%8, %5
   clr %0, [%6], %5
   clr %0, [%2]+=%4, %1
   clr %0, [%2], %1
   clr %0, [%2], %1, [%6], %5
   clr %0, [%2]+=%4, %1, [%6], %5
   clr %0, [%2], %1, [%6]+=%8, %5
   clr %0"
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "clracawb_gen_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w,w,w,w,w,w,w,w,w")
        (const_int 0))
   (set (match_operand: HI 1 "pic30_mac_input_operand"   "=z,i,i,z,z,z,z,z,i")
        (mem:HI
          (match_operand: HI 2 "pic30_xprefetch_operand" " x,i,i,x,x,x,x,x,i")))
   (set (match_operand: HI 3 "pic30_xprefetch_operand"   "=2,i,i,2,i,i,2,i,i")
        (plus:HI
          (match_dup 3)
          (match_operand: HI 4 "immediate_operand"       " Y,i,i,Y,i,i,Y,i,i")))
   (set (match_operand: HI 5 "pic30_mac_input_operand"   "=z,z,z,i,i,z,z,z,i")
        (mem:HI
          (match_operand: HI 6 "pic30_yprefetch_operand" " y,y,y,i,i,y,y,y,i")))
   (set (match_operand: HI 7 "pic30_yprefetch_operand"   "=6,6,i,i,i,i,i,6,i")
        (plus:HI
          (match_dup 7)
          (match_operand: HI 8 "immediate_operand"       " Y,Y,i,i,i,i,i,Y,i")))
   (set (match_operand: HI 9 "pic30_awb_operand"         "=v,v,v,v,v,v,v,v,v")
        (match_operand:HI 10 "pic30_accumulator_operand" " w,w,w,w,w,w,w,w,w"))
  ]
  ""
  "@
   clr %0, [%2]+=%4, %1, [%6]+=%8, %5, %9
   clr %0, [%6]+=%8, %5, %9
   clr %0, [%6], %5, %9
   clr %0, [%2]+=%4, %1, %9
   clr %0, [%2], %1, %9
   clr %0, [%2], %1, [%6], %5, %9
   clr %0, [%2]+=%4, %1, [%6], %5, %9
   clr %0, [%2], %1, [%6]+=%8, %5, %9
   clr %0, %9"
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "ed_hi"
 [
  (set (match_operand:HI 0 "pic30_accumulator_operand" "=w,w,w,w")
       (unspec:HI [
           (match_operand:HI 1 "pic30_mac_input_operand" "z,z,z,z")
           (match_dup 1)
         ] UNSPECV_DSPMULT))
  (set (match_operand:HI 2 "pic30_mac_input_operand" "=z,z,z,z")
       (minus: HI
         (mem: HI (match_operand:HI 3 "pic30_xprefetch_operand" "x,x,x,x"))
         (mem: HI (match_operand:HI 4 "pic30_yprefetch_operand" "y,y,y,y"))))
  (set (match_operand:HI 5 "pic30_xprefetch_operand" "=3,3,i,i")
       (plus: HI
         (match_dup 5)
         (match_operand: HI 6 "immediate_operand" "Y,Y,i,i")))
  (set (match_operand:HI 7 "pic30_yprefetch_operand" "=4,i,i,4")
       (plus: HI
         (match_dup 7)
         (match_operand: HI 8 "immediate_operand" "Y,i,i,Y")))
 ]
 ""
 "@
  ed %1*%1, %0, [%3]+=%6, [%4]+=%8, %2
  ed %1*%1, %0, [%3]+=%6, [%4], %2
  ed %1*%1, %0, [%3], [%4], %2
  ed %1*%1, %0, [%3], [%4]+=%8, %2"
 [
   (set_attr "cc" "unchanged")
 ]
)

(define_insn "edac_hi"
 [
  (set (match_operand:HI 0 "pic30_accumulator_operand" "=w,w,w,w")
       (plus: HI
         (match_dup 0)
         (unspec:HI [
             (match_operand:HI 1 "pic30_mac_input_operand" "z,z,z,z")
             (match_dup 1)
           ] UNSPECV_DSPMULT)))
  (set (match_operand:HI 2 "pic30_mac_input_operand" "=z,z,z,z")
       (minus: HI
         (mem: HI (match_operand:HI 3 "pic30_xprefetch_operand" "x,x,x,x"))
         (mem: HI (match_operand:HI 4 "pic30_yprefetch_operand" "y,y,y,y"))))
  (set (match_operand:HI 5 "pic30_xprefetch_operand" "=3,3,i,i")
       (plus: HI
         (match_dup 5)
         (match_operand: HI 6 "immediate_operand" "Y,Y,i,i")))
  (set (match_operand:HI 7 "pic30_yprefetch_operand" "=4,i,i,4")
       (plus: HI
         (match_dup 7)
         (match_operand: HI 8 "immediate_operand" "Y,i,i,Y")))
 ]
 ""
 "@
  edac %1*%1, %0, [%3]+=%6, [%4]+=%8, %2
  edac %1*%1, %0, [%3]+=%6, [%4], %2
  edac %1*%1, %0, [%3], [%4], %2
  edac %1*%1, %0, [%3], [%4]+=%8, %2"
 [
   (set_attr "cc" "unchanged")
 ]
)

(define_insn "fbcl_hi"
 [ 
  (set (match_operand:HI 0 "pic30_register_operand" "=r")
       (unspec: HI [
                     (match_operand: HI 1 "pic30_mode2_operand" "rR<>")
                   ] UNSPECV_FBCL))
 ]
 ""
 "fbcl %1, %0"
 [
  (set_attr "cc" "clobber")
 ]
)

(define_insn "lac_hi"
 [
  (set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
        (unspec:HI [
                     (match_operand: HI 1 "pic30_mode3_operand" "rRS<>")
                     (match_operand: HI 2 "immediate_operand" "Z")
                   ] UNSPECV_SAC))
  ]
  ""
  "lac %1, #%2, %0"
  [
    (set_attr "cc" "unchanged")
  ]
)


(define_insn "sac_gen_hi"
  [
   (set (match_operand: HI 0 "pic30_mode3_operand" "=RS<>r")
        (unspec:HI [
                     (match_operand: HI 1 "pic30_accumulator_operand" "w")
                     (match_operand: HI 2 "immediate_operand" "Z")
                   ] UNSPECV_SAC))
  ]
  ""
  "sac %1, #%2, %0"
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "sacr_gen_hi"
  [
   (set (match_operand: HI 0 "pic30_mode3_operand" "=RS<>r")
        (unspec:HI [
                     (match_operand: HI 1 "pic30_accumulator_operand" "w")
                     (match_operand: HI 2 "immediate_operand" "Z")
                   ] UNSPECV_SACR))
  ]
  ""
  "sac.r %1, #%2, %0"
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "mac_gen_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w,w,w,w,w,w,w,w,w")
        (plus:HI 
          (match_dup 0)
          (unspec:HI [
            (match_operand:HI 1 "pic30_mac_input_operand"  " z,z,z,z,z,z,z,z,z")
            (match_operand:HI 2 "pic30_mac_input_operand"  " z,z,z,z,z,z,z,z,z")
            ] UNSPECV_DSPMULT)))
   (set (match_operand: HI 3 "pic30_mac_input_operand"   "=z,i,i,z,z,z,z,z,i")
        (mem:HI
          (match_operand: HI 4 "pic30_xprefetch_operand" " x,i,i,x,x,x,x,x,i")))
   (set (match_operand: HI 5 "pic30_xprefetch_operand"   "=4,i,i,4,i,i,4,i,i")
        (plus:HI
          (match_dup 5)
          (match_operand: HI 6 "immediate_operand"       " Y,i,i,Y,i,i,Y,i,i")))
   (set (match_operand: HI 7 "pic30_mac_input_operand"   "=z,z,z,i,i,z,z,z,i")
        (mem:HI
          (match_operand: HI 8 "pic30_yprefetch_operand" " y,y,y,i,i,y,y,y,i")))
   (set (match_operand: HI 9 "pic30_yprefetch_operand"   "=8,8,i,i,i,i,i,8,i")
        (plus:HI
          (match_dup 9)
          (match_operand: HI 10 "immediate_operand"      " Y,Y,i,i,i,i,i,Y,i")))
  ]
  "" 
  "* 
   {
     const char *mac_options[] = {
       \"mac %1*%2, %0, [%4]+=%6, %3, [%8]+=%10, %7\",  /* 0 */
       \"mac %1*%2, %0, [%8]+=%10, %7\",                /* 1 */
       \"mac %1*%2, %0, [%8], %7\",                     /* 2 */
       \"mac %1*%2, %0, [%4]+=%6, %3\",                 /* 3 */
       \"mac %1*%2, %0, [%4], %3\",                     /* 4 */
       \"mac %1*%2, %0, [%4], %3, [%8], %7\",           /* 5 */
       \"mac %1*%2, %0, [%4]+=%6, %3, [%8], %7\",       /* 6 */
       \"mac %1*%2, %0, [%4], %3, [%8]+=%10, %7\",      /* 7 */
       \"mac %1*%2, %0\",                               /* 8 */
       \"mac %2*%1, %0, [%4]+=%6, %3, [%8]+=%10, %7\",
       \"mac %2*%1, %0, [%8]+=%10, %7\",
       \"mac %2*%1, %0, [%8], %7\",
       \"mac %2*%1, %0, [%4]+=%6, %3\",
       \"mac %2*%1, %0, [%4], %3\",
       \"mac %2*%1, %0, [%4], %3, [%8], %7\",
       \"mac %2*%1, %0, [%4]+=%6, %3, [%8], %7\",
       \"mac %2*%1, %0, [%4], %3, [%8]+=%10, %7\",
       \"mac %2*%1, %0\" };

     if (REGNO(operands[1]) < REGNO(operands[2])) {
       return mac_options[which_alternative];
     } else {
       return mac_options[which_alternative+9];
     }
   }
  "
  [
    (set_attr "cc" "unchanged")
  ]
)

;
; GCC had trouble respecting the pre-condition; for now restrict the
;   register choices so that u and t don't overlap... this can be improved.
;   also for msc_gen_hi and msc_awbgen_hi
;
(define_insn  "macawb_gen_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand"   "=w,w,w,w,w,w,w,w,w")
        (plus:HI
          (match_dup 0)
          (unspec:HI [
            (match_operand:HI 1 "pic30_mac_input_operand"  " u,u,u,u,u,u,u,u,u")
            (match_operand:HI 2 "pic30_mac_input_operand"  " t,t,t,t,t,t,t,t,t")
            ] UNSPECV_DSPMULT)))
   (set (match_operand: HI 3 "pic30_mac_input_operand"     "=z,i,i,z,z,z,z,z,i")
        (mem:HI
          (match_operand: HI 4 "pic30_xprefetch_operand"   " x,i,i,x,x,x,x,x,i")
   ))
   (set (match_operand: HI 5 "pic30_xprefetch_operand"     "=4,i,i,4,i,i,4,i,i")
        (plus:HI
          (match_dup 5)
          (match_operand: HI 6 "immediate_operand"         " Y,i,i,Y,i,i,Y,i,i")
   ))
   (set (match_operand: HI 7 "pic30_mac_input_operand"     "=z,z,z,i,i,z,z,z,i")
        (mem:HI
          (match_operand: HI 8 "pic30_yprefetch_operand"   " y,y,y,i,i,y,y,y,i")
   ))
   (set (match_operand: HI 9 "pic30_yprefetch_operand"     "=8,8,i,i,i,i,i,8,i")
        (plus:HI
          (match_dup 9)
          (match_operand: HI 10 "immediate_operand"        " Y,Y,i,i,i,i,i,Y,i")
   ))
   (set (match_operand: HI 11 "pic30_awb_operand"          "=v,v,v,v,v,v,v,v,v")
        (match_operand:HI 12 "pic30_accumulator_operand"   " w,w,w,w,w,w,w,w,w")
   )
  ]
  "(REGNO(operands[1]) != REGNO(operands[2]))"
  "*
   {
     const char *mac_options[] = { 
       \"mac %1*%2, %0, [%4]+=%6, %3, [%8]+=%10, %7, %11\",              /* 0 */
       \"mac %1*%2, %0, [%8]+=%10, %7, %11\",                            /* 1 */
       \"mac %1*%2, %0, [%8], %7, %11\",                                 /* 2 */
       \"mac %1*%2, %0, [%4]+=%6, %3, %11\",                             /* 3 */
       \"mac %1*%2, %0, [%4], %3, %11\",                                 /* 4 */
       \"mac %1*%2, %0, [%4], %3, [%8], %7, %11\",                       /* 5 */
       \"mac %1*%2, %0, [%4]+=%6, %3, [%8], %7, %11\",                   /* 6 */
       \"mac %1*%2, %0, [%4], %3, [%8]+=%10, %7, %11\",                  /* 7 */
       \"mac %1*%2, %0, %11\",                                           /* 8 */
       \"mac %2*%1, %0, [%4]+=%6, %3, [%8]+=%10, %7, %11\",
       \"mac %2*%1, %0, [%8]+=%10, %7, %11\",
       \"mac %2*%1, %0, [%8], %7, %11\",
       \"mac %2*%1, %0, [%4]+=%6, %3, %11\",
       \"mac %2*%1, %0, [%4], %3, %11\",
       \"mac %2*%1, %0, [%4], %3, [%8], %7, %11\",
       \"mac %2*%1, %0, [%4]+=%6, %3, [%8], %7, %11\",
       \"mac %2*%1, %0, [%4], %3, [%8]+=%10, %7, %11\",
       \"mac %2*%1, %0, %11\" };

     if (REGNO(operands[1]) < REGNO(operands[2])) {
       return mac_options[which_alternative];
     } else {
       return mac_options[which_alternative+9];
     }
   }
  "
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "movsac_gen_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w,w,w,w,w,w,w,w,w")
        (match_dup 0))
   (set (match_operand: HI 1 "pic30_mac_input_operand"   "=z,i,i,z,z,z,z,z,i")
        (mem:HI
          (match_operand: HI 2 "pic30_xprefetch_operand" " x,i,i,x,x,x,x,x,i")))
   (set (match_operand: HI 3 "pic30_xprefetch_operand"   "=2,i,i,2,i,i,2,i,i")
        (plus:HI
          (match_dup 3)
          (match_operand: HI 4 "immediate_operand"       " Y,i,i,Y,i,i,Y,i,i")))
   (set (match_operand: HI 5 "pic30_mac_input_operand"   "=z,z,z,i,i,z,z,z,i")
        (mem:HI
          (match_operand: HI 6 "pic30_yprefetch_operand" " y,y,y,i,i,y,y,y,i")))
   (set (match_operand: HI 7 "pic30_yprefetch_operand"   "=6,6,i,i,i,i,i,6,i")
        (plus:HI
          (match_dup 7)
          (match_operand: HI 8 "immediate_operand"       " Y,Y,i,i,i,i,i,Y,i")))
  ]
  ""
  "@
   movsac %0, [%2]+=%4, %1, [%6]+=%8, %5
   movsac %0, [%6]+=%8, %5
   movsac %0, [%6], %5
   movsac %0, [%2]+=%4, %1
   movsac %0, [%2], %1
   movsac %0, [%2], %1, [%6], %5
   movsac %0, [%2]+=%4, %1, [%6], %5
   movsac %0, [%2], %1, [%6]+=%8, %5
   movsac %0" 
  [
    (set_attr "cc" "unchanged")
  ] 
) 

(define_insn "movsacawb_gen_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w,w,w,w,w,w,w,w,w")
        (match_dup 0))
   (set (match_operand: HI 1 "pic30_mac_input_operand"   "=z,i,i,z,z,z,z,z,i")
        (mem:HI
          (match_operand: HI 2 "pic30_xprefetch_operand" " x,i,i,x,x,x,x,x,i")))
   (set (match_operand: HI 3 "pic30_xprefetch_operand"   "=2,i,i,2,i,i,2,i,i")
        (plus:HI
          (match_dup 3)
          (match_operand: HI 4 "immediate_operand"       " Y,i,i,Y,i,i,Y,i,i")))
   (set (match_operand: HI 5 "pic30_mac_input_operand"   "=z,z,z,i,i,z,z,z,i")
        (mem:HI
          (match_operand: HI 6 "pic30_yprefetch_operand" " y,y,y,i,i,y,y,y,i")))
   (set (match_operand: HI 7 "pic30_yprefetch_operand"   "=6,6,i,i,i,i,i,6,i")
        (plus:HI
          (match_dup 7)
          (match_operand: HI 8 "immediate_operand"       " Y,Y,i,i,i,i,i,Y,i")))
   (set (match_operand: HI 9 "pic30_awb_operand"         "=v,v,v,v,v,v,v,v,v")
        (match_operand:HI 10 "pic30_accumulator_operand" " w,w,w,w,w,w,w,w,w"))
  ]
  ""
  "@
   movsac %0, [%2]+=%4, %1, [%6]+=%8, %5, %9
   movsac %0, [%6]+=%8, %5, %9
   movsac %0, [%6], %5, %9
   movsac %0, [%2]+=%4, %1, %9
   movsac %0, [%2], %1, %9
   movsac %0, [%2], %1, [%6], %5, %9
   movsac %0, [%2]+=%4, %1, [%6], %5, %9
   movsac %0, [%2], %1, [%6]+=%8, %5, %9
   movsac %0, %9"
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "mpy_gen_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w,w,w,w,w,w,w,w,w")
        (unspec:HI [
            (match_operand:HI 1 "pic30_mac_input_operand"  " z,z,z,z,z,z,z,z,z")
            (match_operand:HI 2 "pic30_mac_input_operand"  " z,z,z,z,z,z,z,z,z")
          ] UNSPECV_DSPMULT))
   (set (match_operand: HI 3 "pic30_mac_input_operand"   "=z,i,i,z,z,z,z,z,i")
        (mem:HI
          (match_operand: HI 4 "pic30_xprefetch_operand" " x,i,i,x,x,x,x,x,i")))
   (set (match_operand: HI 5 "pic30_xprefetch_operand"   "=4,i,i,4,i,i,4,i,i")
        (plus:HI
          (match_dup 5)
          (match_operand: HI 6 "immediate_operand"       " Y,i,i,Y,i,i,Y,i,i")))
   (set (match_operand: HI 7 "pic30_mac_input_operand"   "=z,z,z,i,i,z,z,z,i")
        (mem:HI
          (match_operand: HI 8 "pic30_yprefetch_operand" " y,y,y,i,i,y,y,y,i")))
   (set (match_operand: HI 9 "pic30_yprefetch_operand"   "=8,8,i,i,i,i,i,8,i")
        (plus:HI
          (match_dup 9)
          (match_operand: HI 10 "immediate_operand"      " Y,Y,i,i,i,i,i,Y,i")))
  ]
  "" 
  "* 
   {
     const char *mpy_options[] = {
       \"mpy %1*%2, %0, [%4]+=%6, %3, [%8]+=%10, %7\", /* 0 */
       \"mpy %1*%2, %0, [%8]+=%10, %7\",               /* 1 */
       \"mpy %1*%2, %0, [%8], %7\",                    /* 2 */
       \"mpy %1*%2, %0, [%4]+=%6, %3\",                /* 3 */
       \"mpy %1*%2, %0, [%4], %3\",                    /* 4 */
       \"mpy %1*%2, %0, [%4], %3, [%8], %7\",          /* 5 */
       \"mpy %1*%2, %0, [%4]+=%6, %3, [%8], %7\",      /* 6 */
       \"mpy %1*%2, %0, [%4], %3, [%8]+=%10, %7\",     /* 7 */
       \"mpy %1*%2, %0\",                              /* 8 */
       \"mpy %2*%1, %0, [%4]+=%6, %3, [%8]+=%10, %7\",
       \"mpy %2*%1, %0, [%8]+=%10, %7\",
       \"mpy %2*%1, %0, [%8], %7\",
       \"mpy %2*%1, %0, [%4]+=%6, %3\",
       \"mpy %2*%1, %0, [%4], %3\",
       \"mpy %2*%1, %0, [%4], %3, [%8], %7\",
       \"mpy %2*%1, %0, [%4]+=%6, %3, [%8], %7\",
       \"mpy %2*%1, %0, [%4], %3, [%8]+=%10, %7\",
       \"mpy %2*%1, %0\"};

     if (REGNO(operands[1]) < REGNO(operands[2])) {
       return mpy_options[which_alternative];
     } else { 
       return mpy_options[which_alternative+9];
     }
   }
  "
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "mpyn_gen_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w,w,w,w,w,w,w,w,w")
        (unspec:HI [
           (neg:HI 
             (match_operand:HI 1 "pic30_mac_input_operand"" z,z,z,z,z,z,z,z,z"))
           (match_operand:HI 2 "pic30_mac_input_operand"  " z,z,z,z,z,z,z,z,z")
          ] UNSPECV_DSPMULT))
   (set (match_operand: HI 3 "pic30_mac_input_operand"   "=z,i,i,z,z,z,z,z,i")
        (mem:HI
          (match_operand: HI 4 "pic30_xprefetch_operand" " x,i,i,x,x,x,x,x,i")))
   (set (match_operand: HI 5 "pic30_xprefetch_operand"   "=4,i,i,4,i,i,4,i,i")
        (plus:HI
          (match_dup 5)
          (match_operand: HI 6 "immediate_operand"       " Y,i,i,Y,i,i,Y,i,i")))
   (set (match_operand: HI 7 "pic30_mac_input_operand"   "=z,z,z,i,i,z,z,z,i")
        (mem:HI
          (match_operand: HI 8 "pic30_yprefetch_operand" " y,y,y,i,i,y,y,y,i")))
   (set (match_operand: HI 9 "pic30_yprefetch_operand"   "=8,8,i,i,i,i,i,8,i")
        (plus:HI
          (match_dup 9)
          (match_operand: HI 10 "immediate_operand"      " Y,Y,i,i,i,i,i,Y,i")))
  ]
  "(REGNO(operands[1]) < REGNO(operands[2]))"
  "*
   {
     const char *mpy_options[] = {
       \"mpy.n %1*%2, %0, [%4]+=%6, %3, [%8]+=%10, %7\", /* 0 */
       \"mpy.n %1*%2, %0, [%8]+=%10, %7\",               /* 1 */
       \"mpy.n %1*%2, %0, [%8], %7\",                    /* 2 */
       \"mpy.n %1*%2, %0, [%4]+=%6, %3\",                /* 3 */
       \"mpy.n %1*%2, %0, [%4], %3\",                    /* 4 */
       \"mpy.n %1*%2, %0, [%4], %3, [%8], %7\",          /* 5 */
       \"mpy.n %1*%2, %0, [%4]+=%6, %3, [%8], %7\",      /* 6 */
       \"mpy.n %1*%2, %0, [%4], %3, [%8]+=%10, %7\",     /* 7 */
       \"mpy.n %1*%2, %0\",                              /* 8 */
       \"mpy.n %2*%1, %0, [%4]+=%6, %3, [%8]+=%10, %7\",
       \"mpy.n %2*%1, %0, [%8]+=%10, %7\",
       \"mpy.n %2*%1, %0, [%8], %7\",
       \"mpy.n %2*%1, %0, [%4]+=%6, %3\",
       \"mpy.n %2*%1, %0, [%4], %3\",
       \"mpy.n %2*%1, %0, [%4], %3, [%8], %7\",
       \"mpy.n %2*%1, %0, [%4]+=%6, %3, [%8], %7\",
       \"mpy.n %2*%1, %0, [%4], %3, [%8]+=%10, %7\",
       \"mpy.n %2*%1, %0\"};

     if (REGNO(operands[1]) < REGNO(operands[2])) {
       return mpy_options[which_alternative];
     } else {
       return mpy_options[which_alternative+9];
     }
   }
  "
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "msc_gen_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand"   "=w,w,w,w,w,w,w,w,w")
        (minus:HI 
          (match_dup 0)
          (unspec:HI [
            (match_operand:HI 1 "pic30_mac_input_operand"  " u,u,u,u,u,u,u,u,u")
            (match_operand:HI 2 "pic30_mac_input_operand"  " t,t,t,t,t,t,t,t,t")
            ] UNSPECV_DSPMULT)))
   (set (match_operand: HI 3 "pic30_mac_input_operand"     "=z,i,i,z,z,z,z,z,i")
        (mem:HI
          (match_operand: HI 4 "pic30_xprefetch_operand"   " x,i,i,x,x,x,x,x,i")
   ))
   (set (match_operand: HI 5 "pic30_xprefetch_operand"     "=4,i,i,4,i,i,4,i,i")
        (plus:HI
          (match_dup 5)
          (match_operand: HI 6 "immediate_operand"         " Y,i,i,Y,i,i,Y,i,i")
   ))
   (set (match_operand: HI 7 "pic30_mac_input_operand"     "=z,z,z,i,i,z,z,z,i")
        (mem:HI
          (match_operand: HI 8 "pic30_yprefetch_operand"   " y,y,y,i,i,y,y,y,i")
   ))
   (set (match_operand: HI 9 "pic30_yprefetch_operand"     "=8,8,i,i,i,i,i,8,i")
        (plus:HI
          (match_dup 9)
          (match_operand: HI 10 "immediate_operand"        " Y,Y,i,i,i,i,i,Y,i")
   ))
  ]
  "" 
  "* 
   {
     const char *msc_options[] = {
       \"msc %1*%2, %0, [%4]+=%6, %3, [%8]+=%10, %7\",
       \"msc %1*%2, %0, [%8]+=%10, %7\",
       \"msc %1*%2, %0, [%8], %7\",
       \"msc %1*%2, %0, [%4]+=%6, %3\",
       \"msc %1*%2, %0, [%4], %3\",
       \"msc %1*%2, %0, [%4], %3, [%8], %7\",
       \"msc %1*%2, %0, [%4]+=%6, %3, [%8], %7\",
       \"msc %1*%2, %0, [%4], %3, [%8]+=%10, %7\",
       \"msc %1*%2, %0\",
       \"msc %2*%1, %0, [%4]+=%6, %3, [%8]+=%10, %7\",
       \"msc %2*%1, %0, [%8]+=%10, %7\",
       \"msc %2*%1, %0, [%8], %7\",
       \"msc %2*%1, %0, [%4]+=%6, %3\",
       \"msc %2*%1, %0, [%4], %3\",
       \"msc %2*%1, %0, [%4], %3, [%8], %7\",
       \"msc %2*%1, %0, [%4]+=%6, %3, [%8], %7\",
       \"msc %2*%1, %0, [%4], %3, [%8]+=%10, %7\",
       \"msc %2*%1, %0\"};

     if (REGNO(operands[1]) < REGNO(operands[2])) {
       return msc_options[which_alternative];
     } else { 
       return msc_options[which_alternative+9];
     }
   }
  "
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "mscawb_gen_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand"   "=w,w,w,w,w,w,w,w,w")
        (minus:HI
          (match_dup 0)
          (unspec:HI [
            (match_operand:HI 1 "pic30_mac_input_operand"  " u,u,u,u,u,u,u,u,u")
            (match_operand:HI 2 "pic30_mac_input_operand"  " t,t,t,t,t,t,t,t,t")
            ] UNSPECV_DSPMULT)))
   (set (match_operand: HI 3 "pic30_mac_input_operand"     "=z,i,i,z,z,z,z,z,i")
        (mem:HI
          (match_operand: HI 4 "pic30_xprefetch_operand"   " x,i,i,x,x,x,x,x,i")
   ))
   (set (match_operand: HI 5 "pic30_xprefetch_operand"     "=4,i,i,4,i,i,4,i,i")
        (plus:HI
          (match_dup 5)
          (match_operand: HI 6 "immediate_operand"         " Y,i,i,Y,i,i,Y,i,i")
   ))
   (set (match_operand: HI 7 "pic30_mac_input_operand"     "=z,z,z,i,i,z,z,z,i")
        (mem:HI
          (match_operand: HI 8 "pic30_yprefetch_operand"   " y,y,y,i,i,y,y,y,i")
   ))
   (set (match_operand: HI 9 "pic30_yprefetch_operand"     "=8,8,i,i,i,i,i,8,i")
        (plus:HI
          (match_dup 9)
          (match_operand: HI 10 "immediate_operand"        " Y,Y,i,i,i,i,i,Y,i")
   ))
   (set (match_operand: HI 11 "pic30_awb_operand"          "=v,v,v,v,v,v,v,v,v")
        (match_operand:HI 12 "pic30_accumulator_operand"   " w,w,w,w,w,w,w,w,w")
   )
  ]
  ""
  "*
   {
     const char *msc_options[] = {
       \"msc %1*%2, %0, [%4]+=%6, %3, [%8]+=%10, %7, %11\",
       \"msc %1*%2, %0, [%8]+=%10, %7, %11\",
       \"msc %1*%2, %0, [%8], %7, %11\",
       \"msc %1*%2, %0, [%4]+=%6, %3, %11\",
       \"msc %1*%2, %0, [%4], %3, %11\",
       \"msc %1*%2, %0, [%4], %3, [%8], %7, %11\",
       \"msc %1*%2, %0, [%4]+=%6, %3, [%8], %7, %11\",
       \"msc %1*%2, %0, [%4], %3, [%8]+=%10, %7, %11\",
       \"msc %1*%2, %0, %11\",
       \"msc %2*%1, %0, [%4]+=%6, %3, [%8]+=%10, %7, %11\",
       \"msc %2*%1, %0, [%8]+=%10, %7, %11\",
       \"msc %2*%1, %0, [%8], %7, %11\",
       \"msc %2*%1, %0, [%4]+=%6, %3, %11\",
       \"msc %2*%1, %0, [%4], %3, %11\",
       \"msc %2*%1, %0, [%4], %3, [%8], %7, %11\",
       \"msc %2*%1, %0, [%4]+=%6, %3, [%8], %7, %11\",
       \"msc %2*%1, %0, [%4], %3, [%8]+=%10, %7, %11\",
       \"msc %2*%1, %0, %11\"};

     if (REGNO(operands[1]) < REGNO(operands[2])) {
       return msc_options[which_alternative];
     } else {
       return msc_options[which_alternative+9];
     }
   }
  " 
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "sftac_gen_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w,w")
        (unspec:HI [
                     (match_dup 0)
                     (match_operand: HI 1 "pic30_reg_or_imm_operand" "r,W")
                   ] UNSPECV_SFTAC)
   )]
  ""
  "@
   sftac %0, %1
   sftac %0, #%1"
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "subac_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand"  "=w")
        (minus:HI (match_dup 0)
                  (match_operand: HI 1 "pic30_accumulator_operand" "w")))]
  "(REGNO(operands[1]) != REGNO(operands[0]))"
  "sub %0"
  [
    (set_attr "cc" "unchanged")
  ]
)

; ********* to support automagic generation

(define_insn "movhi_accumulator"
  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w,w")
        (match_operand: HI 1 "pic30_move_operand" "RS<>r,QU"))]
  ""
  "*
   {
     /* lac %1, %0 */
     error(\"Automatic generation of DSP instructions not yet supported; \"
           \"use __builtin_lac() instead\");
     return \"; bad op\";
   }
  "
  [(set_attr "cc" "unchanged")]
)  

(define_insn "movhi_accumulator2"
  [(set (match_operand:HI 0 "pic30_move_operand" "=RS<>r,QU")
        (match_operand:HI 1 "pic30_accumulator_operand" "w,w"))]
  ""
  "*
   {
     /* sac %1, %0 */
     error(\"Automatic generation of DSP instructions not yet supported; \"
           \"use __builtin_sac() instead\");
     return \"; bad op\";
   }
  "
  [(set_attr "cc" "unchanged")]
)

(define_insn "movhi_accumulator3"
  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
        (match_operand:HI 1 "immediate_operand" "i"))]
  ""
  "*
   {
     if (INTVAL(operands[1]) == 0) {
       /* clr %0 */
       error(\"Automatic generation of DSP instructions not yet supported; \"
             \"use __builtin_clr() instead\");
     } else {
       /* lac ... */
       error(\"Automatic generation of DSP instructions not yet supported; \"
             \"use __builtin_lac() instead\");
     }
     return \"; bad op\";
   }
  "
  [(set_attr "cc" "unchanged")]
)

(define_insn "auto_mac"
  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
        (plus:HI
          (match_operand:HI 1 "pic30_accumulator_operand" "0")
          (subreg:HI 
            (mult:SI
              (sign_extend:SI 
                (match_operand: HI 2 "pic30_mac_input_operand" "z"))
              (sign_extend:SI
                (match_operand: HI 3 "pic30_mac_input_operand" "z"))) 0)))]
  ""
  "*
   {
     error(\"Automatic generation of DSP instructions not yet supported; \"
           \"use __builtin_mac() instead\");
     return \"; bad op\";
     if (REGNO(operands[2]) < REGNO(operands[3])) {
       return \"mac %2*%3, %0\";
     } else {
       return \"mac %3*%2, %0\";
     }
   }
  "
  [(set_attr "cc" "unchanged")]
)

(define_insn "auto_mac1"
  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
        (plus:HI
          (subreg:HI
            (mult:SI
              (sign_extend:SI
                (match_operand: HI 1 "pic30_mac_input_operand" "z"))
              (sign_extend:SI
                (match_operand: HI 2 "pic30_mac_input_operand" "z"))) 0)
          (match_operand:HI 3 "pic30_accumulator_operand" "0")))]
  ""
  "*
   {
     error(\"Automatic generation of DSP instructions not yet supported; \"
           \"use __builtin_mac() instead\");
     return \"; bad op\";
     if (REGNO(operands[1]) < REGNO(operands[2])) {
       return \"mac %1*%2, %0\";
     } else {
       return \"mac %2*%1, %0\";
     }
   }
  "
  [(set_attr "cc" "unchanged")]
)

(define_insn "auto_mpy"
  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
        (neg: HI
          (subreg:HI
            (mult:SI
              (sign_extend:SI
                (match_operand: HI 1 "pic30_mac_input_operand" "z"))
              (sign_extend:SI
                (match_operand: HI 2 "pic30_mac_input_operand" "z"))) 0)))]
  ""
  "*
   {
     error(\"Automatic generation of DSP instructions not yet supported; \"
           \"use __builtin_mpy() instead\");
     return \"; bad op\";
     if (REGNO(operands[1]) < REGNO(operands[2])) {
       return \"mpy.n %1*%2, %0\";
     } else {
       return \"mpy.n %2*%1, %0\";
     }
   }
  "
  [(set_attr "cc" "unchanged")]
)

(define_insn "auto_msc"
  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
        (minus:HI
          (match_operand:HI 1 "pic30_accumulator_operand" "0")
          (subreg:HI
            (mult:SI
              (sign_extend:SI
                (match_operand: HI 2 "pic30_mac_input_operand" "z"))
              (sign_extend:SI
                (match_operand: HI 3 "pic30_mac_input_operand" "z"))) 0)))]
  ""
  "*
   {
     error(\"Automatic generation of DSP instructions not yet supported; \"
           \"use __builtin_msc() instead\");
     return \"; bad op\";
     if (REGNO(operands[2]) < REGNO(operands[3])) {
       return \"msc %2*%3, %0\";
     } else {
       return \"msc %3*%2, %0\";
     }
   }
  "
  [(set_attr "cc" "unchanged")]
)

(define_insn "auto_msc1"
  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
        (minus:HI
          (subreg:HI
            (mult:SI
              (sign_extend:SI
                (match_operand: HI 1 "pic30_mac_input_operand" "z"))
              (sign_extend:SI
                (match_operand: HI 2 "pic30_mac_input_operand" "z"))) 0)
          (match_operand:HI 3 "pic30_accumulator_operand" "0")))]
  ""
  "*
   {
     error(\"Automatic generation of DSP instructions not yet supported; \"
           \"use __builtin_msc() instead\");
     return \"; bad op\";
     if (REGNO(operands[1]) < REGNO(operands[2])) {
       return \"msc %1*%2, %0\";
     } else {
       return \"msc %2*%1, %0\";
     }
   }
  "
  [(set_attr "cc" "unchanged")]
)

(define_insn "auto_sftacr"
  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
        (ashiftrt:HI
          (match_dup 0)
          (match_operand:HI 1 "immediate_operand"       "W")))]
  "(INTVAL(operands[1]) > 0)"
  "*
   {
     /* sftac %0, #%1 */
     error(\"Automatic generation of DSP instructions not yet supported; \"
           \"use __builtin_sftac() instead\");
     return \"; bad op\";
   }
  "
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "auto_sftacl"
  [(set (match_operand:HI 0 "pic30_accumulator_operand" "=w")
        (ashift:HI 
          (match_dup 0)
          (match_operand:HI 1 "immediate_operand"       "W")))]
  "(INTVAL(operands[1]) > 0)"
  "*
   { static char buffer[20];

     error(\"Automatic generation of DSP instructions not yet supported; \"
           \"use __builtin_sftac() instead\");
     return \"; bad op\";
     sprintf(buffer,\"sftac %%0, #%d\", -INTVAL(operands[1]));
     return buffer;
   }
  "
  [(set_attr "cc" "unchanged")]
)

(define_insn "addacr_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w")
        (plus:HI
           (match_operand: HI 1 "pic30_mode3_operand" "RS<>r")
           (match_operand: HI 2 "pic30_accumulator_operand" "0")))]
  ""
  "*
   {
     /* add %1, %0 */
     error(\"Automatic generation of DSP instructions not yet supported; \"
           \"use __builtin_add() instead\");
     return \"; bad op\";
   }
  "
  [
    (set_attr "cc" "unchanged")
  ]
)

(define_insn "addacr1_hi"
  [(set (match_operand: HI 0 "pic30_accumulator_operand" "=w")
        (plus:HI
           (match_operand: HI 1 "pic30_accumulator_operand" "0")
           (match_operand: HI 2 "pic30_mode3_operand" "RS<>r")))]
  ""
  "*
   {
     /* add %2, %0 */
     error(\"Automatic generation of DSP instructions not yet supported; \"
           \"use __builtin_add() instead\");
     return \"; bad op\";
   }
  "
  [
    (set_attr "cc" "unchanged")
  ]
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; __builtin_unique_id

(define_insn "unique_id"
  [(set (match_operand:HI 0 "pic30_register_operand" "=r")
        (unspec_volatile:HI [ (match_operand:SI 1 "immediate_operand" "i")
                              (match_operand:HI 2 "immediate_operand" "i") 
                            ] UNSEPCV_GENLABEL))]
  ""
  "*
   {  static char buffer[80];
      char *label;

      label = (char *)(INTVAL(operands[1]));
      sprintf(buffer,\".global %s\n%s:\n\tmov #%d,%%0\",
              label,label,INTVAL(operands[2]));
      return buffer;
   }
  "
)
                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stackpush
;; These patterns are used for passing arguments on the stack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define_insn "*pushqi"
  [(set (match_operand:QI 0 "push_operand"   "=>")
        (match_operand:QI 1 "general_operand" "r"))]
  ""
  "mov %1,[w15++]"
  [
   (set_attr "cc" "unchanged")
   (set_attr "type" "def")
  ]
)

(define_insn "*pushhi1"
  [(set (match_operand:HI 0 "push_operand"   "=>")
	(const_int -1))]
  ""
  "setm %0"
  [
   (set_attr "cc" "unchanged")
   (set_attr "type" "def")
  ]
)

(define_insn "pushhi"
  [(set (match_operand:HI 0 "push_operand"   "=>,>,>,>")
        (match_operand:HI 1 "general_operand" "r,>,O,T"))]
  ""
  "@
   mov %1,%0
   mov %1,%0
   clr %0
   push %1"
  [
   (set_attr "cc" "unchanged")
   (set_attr "type" "def,defuse,def,def")
  ]
)

(define_insn "pushsi"
  [(set (match_operand:SI 0 "push_operand"   "=>")
        (match_operand:SI 1 "general_operand" "r"))]
  ""
  "mov.d %1,[w15++]"
  [
   (set_attr "cc" "unchanged")
   (set_attr "type" "def")
  ]
)

(define_insn "*pushdi"
  [(set (match_operand:DI 0 "push_operand"   "=>")
        (match_operand:DI 1 "general_operand" "r"))]
  ""
  "mov.d %1,[w15++]\;mov.d %t1,[w15++]"
  [
   (set_attr "cc" "unchanged")
   (set_attr "type" "def")
  ]
)

(define_insn "*pushsf"
  [(set (match_operand:SF 0 "push_operand"   "=>")
        (match_operand:SF 1 "general_operand" "r"))]
  ""
  "mov.d %1,[w15++]"
  [
   (set_attr "cc" "unchanged")
   (set_attr "type" "def")
  ]
)

(define_insn "*pushdf"
  [(set (match_operand:DF 0 "push_operand"   "=>")
        (match_operand:DF 1 "general_operand" "r"))]
  ""
  "mov.d %1,[w15++]\;mov.d %t1,[w15++]"
  [
   (set_attr "cc" "unchanged")
   (set_attr "type" "def")
  ]
)

(define_insn "pushshadow"
  [(unspec [(const_int 0)] UNSPEC_PUSHSHADOW)
   (use (reg:HI 0))
   (use (reg:HI 1))
   (use (reg:HI 2))
   (use (reg:HI 3))
  ]
  ""
  "push.s")

(define_insn "popshadow"
  [(unspec [(const_int 0)] UNSPEC_POPSHADOW)
   (clobber (reg:HI 0))
   (clobber (reg:HI 1))
   (clobber (reg:HI 2))
   (clobber (reg:HI 3))
  ]
  ""
  "pop.s"
  [(set_attr "cc" "clobber")]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stackpop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "popqi"
  [(set (match_operand:QI 0 "pic30_register_operand" "=r")
        (match_operand:QI 1 "pop_operand"       "<"))]
  ""
  "mov %1,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "use")
  ]
)

(define_insn "pophi"
  [(set (match_operand:HI 0 "general_operand" "=r,<,O,T")
        (match_operand:HI 1 "pop_operand"    " <,<,<,<"))]
  ""
  "@
   mov %1,%0
   mov %1,%0
   clr %0
   pop %0"
  [
   (set_attr "cc" "unchanged")
   (set_attr "type" "def,defuse,def,def")
  ]
)

(define_insn "pophi_unspec"
  [(set (unspec:HI [(match_operand:HI 0 "general_operand" "=r<,T")]
                   UNSPEC_POPHI)
        (mem:HI (pre_dec:HI (reg:HI SPREG))))
   (clobber (match_dup 0))
  ]
  ""
  "@
   mov [--w15],%0
   pop %0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "defuse,use")
  ]
)

(define_insn "popsi"
  [(set (match_operand:SI 0 "pic30_register_operand" "=r")
        (match_operand:SI 1 "pop_operand"       "<"))
  ]
  ""
  "mov.d %1,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "use")
  ]
)

(define_insn "popdi"
  [(set (match_operand:DI 0 "pic30_register_operand" "=r")
        (match_operand:DI 1 "pop_operand"       "<"))]
  ""
  "mov.d %1,%t0\;mov.d %1,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "use")
  ]
)

(define_insn "popsf"
  [(set (match_operand:SF 0 "pic30_register_operand" "=r")
        (match_operand:SF 1 "pop_operand"       "<"))]
  ""
  "mov.d %1,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "use")
  ]
)
   
(define_insn "popdf"
  [(set (match_operand:DF 0 "pic30_register_operand" "=r")
        (match_operand:DF 1 "pop_operand"       "<"))]
  ""
  "mov.d %1,%t0\;mov.d %1,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "use")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stackcheck
;; If STACK_CHECK_BUILTIN is zero, and you define a named pattern called
;; check_stack in your 'md' file, GCC will call that pattern with one
;; argument which is the address to compare the stack value against.
;; You must arrange for this pattern to report an error if the stack
;; pointer is out of range.
;; (define_insn "check_stack"
;;   [(match_operand 0 "pic30_register_operand" "r")
;;    (clobber (match_scratch:HI 1 "=r"))]
;;   ""
;;   "mov .BOS,%1\;cpsge %0,%1\;reset")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test operations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "tstqi"
  [(set (cc0)
        (match_operand:QI 0 "pic30_near_mode2_operand" "U,r,R<>"))]
  ""
  "cp0.b %0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "etc,etc,use")
  ]
)

(define_insn "*tstqi_sfr"
  [(set (cc0)
        (match_operand:QI 0 "pic30_near_operand" "U"))]
  ""
  "cp0.b %0"
  [(set_attr "cc" "set")])

(define_insn "tsthi"
  [(set (cc0)
        (match_operand:HI 0 "pic30_near_mode2_operand" "U,r,R<>"))]
  ""
  "cp0 %0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "etc,etc,use")
  ]
)

(define_insn "*tsthi_sfr"
  [(set (cc0)
        (match_operand:HI 0 "pic30_near_operand" "U"))]
  ""
  "cp0 %0"
  [(set_attr "cc" "set")])

(define_insn "tstsi"
  [(set (cc0)
        (match_operand:SI 0 "pic30_register_operand" "r"))]
  ""
  "sub %0,#0,[w15]\;subb %d0,#0,[w15]"
  [(set_attr "cc" "set")])

(define_insn "tstdi"
  [(set (cc0)
        (match_operand:DI 0 "pic30_register_operand" "r"))]
  ""
  "sub %0,#0,[w15]\;subb %d0,#0,[w15]\;subb %t0,#0,[w15]\;subb %q0,#0,[w15]"
  [(set_attr "cc" "set")])

; With dsPIC30 floats, testing the most significant word does not suffice,
; since -0.0 == 0.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block moves.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "movstrhi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
		   (match_operand:BLK 1 "memory_operand" ""))
	      (use (match_operand:HI 2 "const_int_operand" ""))
	      (use (match_operand:HI 3 "const_int_operand" ""))
	      (clobber (match_dup 4))
	      (clobber (match_dup 5))
   	      (clobber (reg:HI RCOUNT))
  ])]
  ""
  "
{
	rtx addr0;
	rtx addr1;
	int n;

	if (GET_CODE (operands[2]) != CONST_INT)
		FAIL;
	n = INTVAL(operands[2]);
	if (n < 1)
		FAIL;

	switch (INTVAL(operands[3]))
	{
	case 1:
		if (n > 16383)
			FAIL;
		break;
	default:
		if ((n > (16383*2)) || (n & 1))
			FAIL;
	}

	addr0 = copy_to_mode_reg(Pmode, XEXP (operands[0], 0));
	addr1 = copy_to_mode_reg(Pmode, XEXP (operands[1], 0));

	operands[4] = addr0;
	operands[5] = addr1;
	operands[0] = change_address(operands[0], VOIDmode, addr0);
	operands[1] = change_address(operands[1], VOIDmode, addr1);
}")

(define_insn "*bcopy"
  [(set (mem:BLK (match_operand:HI 0 "pic30_register_operand" "r"))
        (mem:BLK (match_operand:HI 1 "pic30_register_operand" "r")))
   (use (match_operand:HI 2 "immediate_operand" ""))
   (use (match_operand:HI 3 "const_int_operand" ""))
   (clobber (match_dup 0))
   (clobber (match_dup 1))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  "*
   {
     switch (INTVAL(operands[3]))
     {
     case 1:
	/* 
	** Byte operation
	*/
	return \"repeat #%2-1\;mov.b [%1++],[%0++]\";
     default:
	/* 
	** Word operation
	*/
	return \"repeat #%2/2-1\;mov [%1++],[%0++]\";
     }
   } ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block clear.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument 0 is the destination
;; Argument 1 is the length
;; Argument 2 is the alignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "clrstrhi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
		   (const_int 0))
	      (use (match_operand:HI 1 "const_int_operand" ""))
	      (use (match_operand:HI 2 "const_int_operand" ""))
	      (clobber (match_dup 3))
	      (clobber (reg:HI RCOUNT))
  ])]
  ""
  "
{
	rtx addr0;
	int n;

	if (GET_CODE (operands[1]) != CONST_INT)
		FAIL;
	n = INTVAL(operands[1]);
	if (n < 1)
		FAIL;

	switch (INTVAL(operands[2]))
	{
	case 1:
		if (n > 16383)
			FAIL;
		break;
	default:
		if ((n > (16383/2)) || (n & 1))
			FAIL;
	}
	addr0 = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));

	operands[3] = addr0;

	operands[0] = gen_rtx_MEM (BLKmode, addr0);
}")

;; Block clear.
;; Argument 0 is the destination
;; Argument 1 is the length
;; Argument 2 is the alignment

(define_insn "*bzero"
  [(set (mem:BLK (match_operand:HI 0 "pic30_register_operand" "r"))
	(const_int 0))
   (use (match_operand:HI 1 "const_int_operand" ""))
   (use (match_operand:HI 2 "immediate_operand" ""))
   (clobber (match_dup 0))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  "*
{
	switch (INTVAL(operands[2]))
	{
	case 1:
		return \"repeat #%1-1\;clr.b [%0++]\";
	default:
		return \"repeat #%1/2-1\;clr [%0++]\";
	}
}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compare instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; quarter integer
;;;;;;;;;;;;;;;;;;

(define_insn "*cmpqi_imm"
  [(set (cc0)
        (compare (match_operand:QI 0 "pic30_register_operand" "r")
                 (match_operand:QI 1 "pic30_M_operand"  "M")))
   		 (clobber (match_dup 0))]
  ""
  "add.b #%J1,%0"
  [(set_attr "cc" "set")])

(define_insn "cmpqi3_sfr0"
  [(set (cc0)
        (compare:QI (match_operand:QI 0 "pic30_near_operand" "U,U")
                    (match_operand:QI 1 "pic30_wreg_operand" "a,?r")))
   (clobber (match_scratch:HI 2 "=X,r"))]
  ""
  "@
   cp.b %0
   mov #%0,%2\;sub.b %0,%2,[w15]"
  [(set_attr "cc" "set")])

(define_insn "cmpqi3_2sfr"
  [(set (cc0)
        (compare:QI 
           (match_operand:QI 0 "pic30_wreg_or_near_operand" "?r,U, U,  r")
           (match_operand:QI 1 "pic30_wreg_or_near_operand" "U, a, ?r, r")))
   (clobber (match_scratch:HI 2 "=r,X,r,X"))]
  ""
  "@
   mov #%1,%2\;sub.b %0,[%2],[w15]
   cp.b %0
   mov #%0,%2\;sub.b %0,[%2],[w15]
   cp.b %0,%1"
  [(set_attr "cc" "set")])

(define_expand "cmpqi"
 [(set (cc0)
       (compare (match_operand:QI 0 "pic30_near_mode1PN_operand" "")
                (match_operand:QI 1 "pic30_near_mode1PN_operand" "")))
  (match_dup 2)]
  ""
  "{
      if (pic30_near_operand(operands[0],QImode) &&
          pic30_near_operand(operands[1],QImode)) {
  
        rtx pop; 
  
        operands[2] = gen_reg_rtx(QImode);
        
        emit_insn(gen_movqi_gen_b(operands[2],operands[1]));
        emit_insn(gen_cmpqi3_sfr0(operands[0],operands[2]));
      } else if (pic30_near_operand(operands[1],QImode)) {
        if (pic30_wreg_operand(operands[0],QImode))
          emit_insn(gen_cmpqi3_2sfr(operands[0],operands[1]));
        else { 
          rtx pop; 

          operands[2] = gen_reg_rtx(HImode);
          pop = gen_rtx_MEM(QImode, operands[2]);
          emit_insn(gen_movhi_address(operands[2],  XEXP(operands[1],0)));
          emit_insn(gen_cmpqi_normal(operands[0],pop));
        }
      }
      else if (pic30_near_operand(operands[0],QImode)) {
        if (pic30_wreg_operand(operands[1],QImode))
          emit_insn(gen_cmpqi3_sfr0(operands[0],operands[1]));
        else {
          rtx pop;

          operands[2] = gen_reg_rtx(HImode);
          pop = gen_rtx_MEM(QImode, operands[2]);
          emit_insn(gen_movhi_address(operands[2], XEXP(operands[0],0)));
          emit_insn(gen_cmpqi_normal(pop,operands[1]));
        }
      } else {
        emit_insn(gen_cmpqi_normal(operands[0],operands[1]));
      }
      DONE;
   }
  "
)


(define_insn "cmpqi_normal"
  [(set (cc0)
        (compare (match_operand:QI 0 "pic30_mode1PN_operand"
				"r,r,  R<>,r,P,r")
                 (match_operand:QI 1 "pic30_mode1PN_operand"
				"r,R<>,r,  P,r,N")))]
  ""
  "@
   sub.b %0,%1,[w15]
   sub.b %0,%1,[w15]
   subr.b %1,%0,[w15]
   sub.b %0,#%1,[w15]
   subr.b %1,#%0,[w15]
   add.b %0,#%J1,[w15]"
  [
   (set_attr "cc" "set")
   (set_attr "type" "etc,use,use,etc,etc,etc")
  ]
)

;;;;;;;;;;;;;;;;;;
;; half integer
;;;;;;;;;;;;;;;;;;

(define_insn "*cmphi_imm"
  [(set (cc0)
        (compare (match_operand:HI 0 "pic30_register_operand" "r")
                 (match_operand:HI 1 "pic30_M_operand"  "M")))
   		 (clobber (match_dup 0))]
  ""
  "add #%J1,%0"
  [(set_attr "cc" "set")])

(define_insn "*cmphi_sfr0"
  [(set (cc0)
        (compare:HI (match_operand:HI 0 "pic30_near_operand" "U")
                    (match_operand:HI 1 "pic30_wreg_operand" "a")))]
  ""
  "cp %0"
  [(set_attr "cc" "set")])

(define_insn "cmphi"
  [(set (cc0)
        (compare (match_operand:HI 0 "pic30_mode1PN_operand"
				"r,r,  R<>,r,P,r,N")
                 (match_operand:HI 1 "pic30_mode1PN_operand"
				"r,R<>,r,  P,r,N,r")))]
  ""
  "@
   sub %0,%1,[w15]
   sub %0,%1,[w15]
   subr %1,%0,[w15]
   sub %0,#%1,[w15]
   subr %1,#%0,[w15]
   add %0,#%J1,[w15]
   add %1,#%J0,[w15]"
  [
   (set_attr "cc" "set")
   (set_attr "type" "etc,use,use,etc,etc,etc,etc")
  ]
)

;;;;;;;;;;;;;;;;;;
;; single integer
;;;;;;;;;;;;;;;;;;

(define_insn "cmpsi"
  [(set (cc0)
        (compare (match_operand:SI 0 "pic30_mode2mres_operand"
               "r,r,R,r,>")
                 (match_operand:SI 1 "pic30_mode2mres_operand"
               "r,R,r,>,r")))]
  ""
  "*
{
  static char *patterns[] = {
     \"sub %0,%1,[w15]\;subb %d0,%d1,[w15]\",
     \"sub %0,%I1,[w15]\;subb %d0,%D1,[w15]\",
     \"subr %1,%I0,[w15]\;subbr %d1,%D0,[w15]\",
     \"sub %0,%1,[w15]\;subb %d0,%1,[w15]\",
     \"subr %0,%1,[w15]\;subbr %d0,%1,[w15]\",
     \"sub %0,%1,[w15]\;subb %d0,%d1,[w15]\",
     \"sub %0,%I1,[w15]\;mov %D1,[w15++]\;subb %d0,[--w15],[w15]\",
     \"subr %1,%I0,[w15]\;mov %D0,[w15++]\;subbr %d1,[--w15],[w15]\",
     \"sub %0,%1,[w15]\;mov %1,[w15++]\;subb %d0,[--w15],[w15]\",
     \"subr %0,%1,[w15]\;mov %1,[w15++]\;subbr %d0,[--w15],[w15]\",
  0 };

  if (pic30_errata_mask & psv_errata) {
    which_alternative += 5;
  }
  return patterns[which_alternative];
}"
  [(set_attr "cc" "set")])

(define_insn "*cmpsi_zero"
  [(set (cc0)
        (compare (match_operand:SI 0 "pic30_register_operand" "r")
                 (match_operand:SI 1 "pic30_O_operand"  "O")))]
  ""
  "* return(pic30_compare(operands));"
  [(set_attr "cc" "clobber")])

(define_insn "*cmpsi_immNP"
  [(set (cc0)
        (compare (match_operand:SI 0 "pic30_register_operand"  "r,r,r")
                 (match_operand:SI 1 "immediate_operand" "P,N,i")))]
  "(((-31 <= INTVAL(operands[1])) && (INTVAL(operands[1]) <= 31)) ||
    ((0xFFE1 <= INTVAL(operands[1])) && (INTVAL(operands[1]) <= 0xFFFF))) &&
   (INTVAL(operands[1]) != 0)"
  "@
   sub %0,#%1,[w15]\;subb %d0,#0,[w15]
   add %0,#%J1,[w15]\;addc %d0,#0,[w15]
   add %0,#%j1,[w15]\;subb %d0,#0,[w15]"
  [(set_attr "cc" "set")])

(define_insn "*cmpsi_imm"
  [(set (cc0)
        (compare (match_operand:SI 0 "pic30_register_operand"  "r")
                 (match_operand:SI 1 "immediate_operand" "i")))
	         (clobber (match_scratch:HI 2           "=&r"))]
  "((1<INTVAL(operands[1])) && (INTVAL(operands[1])<65536))"
  "mov #%1,%2\;sub %0,%2,[w15]\;subb %d0,#0,[w15]"
  [(set_attr "cc" "clobber")])

(define_insn "*cmpsihi3"
  [(set (cc0)
        (compare (match_operand:SI 0 "pic30_register_operand" "r")
                 (zero_extend:SI (match_operand:HI 1 "pic30_register_operand" "r"))))]
  ""
  "sub %0,%1,[w15]\;subb %d0,#0,[w15]"
  [(set_attr "cc" "math")])

;;;;;;;;;;;;;;;;;;
;; double integer
;;;;;;;;;;;;;;;;;;

(define_insn "cmpdi"
  [(set (cc0)
        (compare (match_operand:DI 0 "pic30_register_operand"          "r,r")
                 (match_operand:DI 1 "pic30_reg_or_zero_operand" "r,O")))]
  ""
  "@
   cp %0,%1\;cpb %d0,%d1\;cpb %t0,%t1\;cpb %q0,%q1
   sub %0,#0,[w15]\;subb %d0,#0,[w15]\;subb %t0,#0,[w15]\;subb %q0,#0,[w15]"
  [(set_attr "cc" "clobber")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; truncation instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zero extension instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "pic30_register_operand"                   "=r,r")
        (zero_extend:HI (match_operand:QI 1 "pic30_mode2_operand" "r,R<>")) )]
  ""
  "ze %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse")
  ]
)

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "pic30_register_operand"                "=r")
        (zero_extend:SI (match_operand:QI 1 "pic30_register_operand" "r")) )]
  ""
  "*
{
	return(	\"ze %1,%0\;\"
		\"mov #0,%d0\");
}"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "zero_extendqidi2"
  [(set (match_operand:DI 0 "pic30_register_operand"                "=r")
        (zero_extend:DI (match_operand:QI 1 "pic30_register_operand" "r")) )]
  ""
  "*
{
	return(	\"ze %1,%0\;\"
		\"mov #0,%d0\;\"
		\"mov #0,%t0\;\"
		\"mov #0,%q0\");
}"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "pic30_register_operand"                "=r")
        (zero_extend:SI (match_operand:HI 1 "pic30_register_operand" "r")) )]
  ""
  "*
{
	int idDst;

	if (REGNO(operands[0]) == REGNO(operands[1]))
	{
		return(	\"mov #0,%d0\");
	}
	else
	{
		idDst =	REGNO(operands[0]);
		if (idDst & 1)
			return(	\"mov %1,%0\;\"
				\"mov #0,%d0\");
		else
			return(	\"mul.uu %1,#1,%0\");
	}
}"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def")
  ]
)

(define_insn "zero_extendhidi2"
  [(set (match_operand:DI 0 "pic30_register_operand"                "=r")
        (zero_extend:DI (match_operand:HI 1 "pic30_register_operand" "r")) )]
  ""
  "*
{
	if (REGNO(operands[0]) == REGNO(operands[1]))
	{
		return(	\"mov #0,%d0\;\"
			\"mov #0,%t0\;\"
			\"mov #0,%q0\");
	}
	else
	{
		return(	\"mov %1,%0\;\"
			\"mov #0,%d0\;\"
			\"mov #0,%t0\;\"
			\"mov #0,%q0\");
	}
}"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sign extension instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "pic30_register_operand"                   "=r,r")
        (sign_extend:HI (match_operand:QI 1 "pic30_mode2_operand" "r,R<>"))
   )
  ]
  ""
  "se %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse")
  ]
)

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "pic30_register_operand"                "=r")
        (sign_extend:SI (match_operand:QI 1 "pic30_register_operand" "r")) )]
  ""
  "*
{
	return(	\"se %1,%0\;\"
		\"asr %0,#15,%d0\");
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "extendqidi2"
  [(set (match_operand:DI 0 "pic30_register_operand"                "=r")
        (sign_extend:DI (match_operand:QI 1 "pic30_register_operand" "r")) )]
  ""
  "*
{
	return(	\"se %1,%0\;\"
		\"asr %0,#15,%d0\;\"
		\"mov %d0,%t0\;\"
		\"mov %t0,%q0\");
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "pic30_register_operand"                "=r")
        (sign_extend:SI (match_operand:HI 1 "pic30_register_operand" "r")) )]
  ""
  "*
{
	int idSrc, idDst;

	idDst =	REGNO(operands[0]);
	idSrc =	REGNO(operands[1]);
	if (idDst == idSrc)
	{
		return(	\"asr %0,#15,%d0\");
	}
	else
	{
		if (idDst & 1)
			return(	\"mov %1,%0\;\"
				\"asr %0,#15,%d0\");
		else
			return(	\"mul.su %1,#1,%0\");
	}
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "extendhidi2"
  [(set (match_operand:DI 0 "pic30_register_operand"                "=r,r")
        (sign_extend:DI (match_operand:HI 1 "pic30_register_operand" "0,r")) )]
  ""
  "*
{
	switch (which_alternative)
	{
	case 0:
		return(	\"asr %0,#15,%d0\;\"
			\"mov %d0,%t0\;\"
			\"mov %t0,%q0\");
	default:
		return(	\"mov %1,%0\;\"
			\"asr %0,#15,%d0\;\"
			\"mov %d0,%t0\;\"
			\"mov %t0,%q0\");
	}
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Invalid move

(define_insn "*movqi_invalid_1"
  [(set (match_operand:QI 0 "pic30_register_operand"  "=r")
        (match_operand:QI 1 "pic30_code_operand" "g"))]
  ""
  "*
{
	error(\"invalid address space for operand\");
	return(\"nop\");
}

")

(define_insn "*movqi_invalid_2"
  [(set (match_operand:QI 0 "pic30_code_operand" "=g")
        (match_operand:QI 1 "pic30_register_operand"    "r"))]
  ""
  "*
{
	error(\"invalid address space for operand\");
	return(\"nop\");
}
")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8-bit moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "*movqi_const0"
  [(set (match_operand:QI 0 "pic30_mode2_operand" "=r,R,<>")
	(const_int 0))]
  ""
  "@
   clr.b %0
   clr.b %0
   clr.b %0"
  [
   (set_attr "cc" "change0,change0,change0")
   (set_attr "type" "def,use,defuse")
  ]
)

(define_insn "*movqi_const1"
  [(set (match_operand:QI 0 "pic30_mode2_operand" "=r,R,<>")
	(const_int -1))]
  ""
  "@
   setm.b %0
   setm.b %0
   setm.b %0"
  [
   (set_attr "cc" "change0,change0,change0")
   (set_attr "type" "def,use,defuse")
  ]
)

(define_insn "*movqi_const0sfr"
  [(set (match_operand:QI 0 "pic30_near_operand" "=U")
	(const_int 0))]
  ""
  "clr.b %0"
  [(set_attr "cc" "unchanged")])

(define_insn "*movqi_const1sfr"
  [(set (match_operand:QI 0 "pic30_near_operand" "=U")
	(const_int -1))]
  ""
  "setm.b %0"
  [(set_attr "cc" "unchanged")])

(define_insn "movqi_rimm"
  [(set (match_operand:QI 0 "pic30_register_operand" "=r")
        (match_operand:QI 1 "immediate_operand" "i"))]
  ""
  "mov.b #%1,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def")
  ]
)

; general case
;;(define_insn "movqi_gen"
;;  [(set (match_operand:QI 0 "pic30_move_operand"
;;		"=r<>,R,r<>, R,   r<>,RS,r<>,RS, Q,r,a,U,!?d,!??????U")
;;        (match_operand:QI 1 "pic30_move_operand"
;;		 "r,  r,<>RS,<>RS,r,  r, R<>,R<>,r,Q,U,a,  U,!??????d"))
;;  ]
;;  ""
;;  "*
;;{  rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
;;   switch (which_alternative) {
;;     case 0:  return \"mov.b %1,%0\";
;;     case 1:  return \"mov.b %1,%0\";
;;     case 2:  return \"mov.b %1,%0\";
;;     case 3:  return \"mov.b %1,%0\";
;;     case 4:  return \"mov.b %1,%0\";
;;     case 5:  return \"mov.b %1,%0\";
;;     case 6:  return \"mov.b %1,%0\";
;;     case 7:  return \"mov.b %1,%0\";
;;     case 8:  return \"mov.b %1,%0\";
;;     case 9:  return \"mov.b %1,%0\";
;;     case 10: return \"mov.b %1,WREG\";
;;     case 11: return \"mov.b WREG,%0\";
;;     case 12: return \"mov #%1,%0\;mov.b [%0],%0\";
;;     case 13: if (pic30_dead_or_set_p(insn, w0))
;;                return \"mov %1,w0\;mov.b WREG,%0\";
;;              else
;;                return \"exch w0,%1\;mov.b WREG,%0\;exch w0,%1\";
;;  }
;;}
;;"
;;  [(set_attr "cc"
;;	"change0,change0,change0,change0,change0,change0,change0,change0,change0,change0,move,unchanged,change0,unchanged")
;;   (set_attr "type"
;;	"def,etc,defuse,use,def,etc,defuse,use,etc,defuse,def,etc,def,etc")
;;  ]
;;)
(define_insn "movqi_gen_a"
  [(set (match_operand:QI 0 "pic30_move_operand"
		"=r<>,R,r<>, R,   r<>,RS,r<>,RS, Q,r,U,U")
        (match_operand:QI 1 "pic30_move2_operand"
		 "r,  r,<>RS,<>RS,r,  r, R<>,R<>,r,Q,a,r"))
  ]
  ""
  "*
{  rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
   switch (which_alternative) {
     case 0:  return \"mov.b %1,%0\";
     case 1:  return \"mov.b %1,%0\";
     case 2:  return \"mov.b %1,%0\";
     case 3:  return \"mov.b %1,%0\";
     case 4:  return \"mov.b %1,%0\";
     case 5:  return \"mov.b %1,%0\";
     case 6:  return \"mov.b %1,%0\";
     case 7:  return \"mov.b %1,%0\";
     case 8:  return \"mov.b %1,%0\";
     case 9:  return \"mov.b %1,%0\";
     case 10: return \"mov.b WREG,%0\";
     case 11: if (REGNO(operands[1]) == WR0_REGNO)
                return \"mov.b WREG,%0\";
              else  if (pic30_dead_or_set_p(insn, w0))
                return \"mov %1,w0\;mov.b WREG,%0\";
              else if (pic30_errata_mask & exch_errata)
                return \"push w0\;mov %1,w0\;mov.b WREG,%0\;pop w0\";
              else
                return \"exch w0,%1\;mov.b WREG,%0\;exch w0,%1\";
  }
}
"
  [(set_attr "cc"
	"change0,change0,change0,change0,change0,change0,change0,change0,change0,change0,unchanged,unchanged")
   (set_attr "type"
	"def,etc,defuse,use,def,etc,defuse,use,etc,defuse,etc,etc")
  ]
)
(define_insn "movqi_gen_b"
  [(set (match_operand:QI 0 "pic30_move2_operand"
		"=r<>,R,r<>, R,   r<>,RS,r<>,RS, Q,r,a,r")
        (match_operand:QI 1 "pic30_move_operand"
		 "r,  r,<>RS,<>RS,r,  r, R<>,R<>,r,Q,U,  U"))
  ]
  ""
  "*
{  rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
   switch (which_alternative) {
     case 0:  return \"mov.b %1,%0\";
     case 1:  return \"mov.b %1,%0\";
     case 2:  return \"mov.b %1,%0\";
     case 3:  return \"mov.b %1,%0\";
     case 4:  return \"mov.b %1,%0\";
     case 5:  return \"mov.b %1,%0\";
     case 6:  return \"mov.b %1,%0\";
     case 7:  return \"mov.b %1,%0\";
     case 8:  return \"mov.b %1,%0\";
     case 9:  return \"mov.b %1,%0\";
     case 10: return \"mov.b %1,WREG\";
     case 11: if (REGNO(operands[0]) == WR0_REGNO) 
                return \"mov.b %1,WREG\";
              else return \"mov #%1,%0\;mov.b [%0],%0\";
  }
}
"
  [(set_attr "cc"
	"change0,change0,change0,change0,change0,change0,change0,change0,change0,change0,move,change0")
   (set_attr "type"
	"def,etc,defuse,use,def,etc,defuse,use,etc,defuse,def,def")
  ]
)

; rare sfr->sfr case
(define_insn "movqi_sfr_a"
   [(set (match_operand:QI 0 "pic30_near_operand" "=U,U,   U, U,   U,U")
         (match_operand:QI 1 "pic30_move_operand"  "U,RS,<>RS,RS<>,Q,r"))]
   ""
   "*
{
    rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
    int save=1;

    if (pic30_dead_or_set_p(insn, w0)) save=0;
    switch (which_alternative) {
      case 0:
      case 1:
      case 2:
      case 3:
      case 4:
        if (save) {
          return \"push w0\;mov.b %1,WREG\;mov.b WREG,%0\;pop w0\";
        } else return \"mov.b %1,WREG\;mov.b WREG,%0\";
        break;
      case 5:
        if (save) {
          if (pic30_errata_mask & exch_errata) 
            return \"push w0\;mov %1,w0\;mov.b WREG,%0\;pop w0\";
          else 
            return \"exch %1,w0\;mov.b WREG,%0\;exch %1,w0\";
        } else return \"mov %1,w0\;mov.b WREG,%0\";
        break;
   }
}
   "
    
   [(set_attr "cc" "change0,change0, change0,change0,change0,change0")
    (set_attr "type" "etc,etc,etc,etc,etc,etc")]
)

(define_insn "movqi_sfr_b"
   [(set (match_operand:QI 0 "pic30_move_operand" "=RS,<>RS,RS<>,Q,r")
         (match_operand:QI 1 "pic30_near_operand" " U,    U, U,  U,U"))]
   ""
   "*
{
    rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
    int save=1;
    
    if (pic30_dead_or_set_p(insn, w0)) save = 0;
    switch (which_alternative) {
      case 0:
      case 1:
      case 2:
        if (save) {
          return \"push w0\;mov.b %1,WREG\;mov.b WREG,%0\;pop w0\";
        } else return \"mov.b %1,WREG\;mov.b WREG,%0\";
        break;
      case 3:
        if (save) {
          if (pic30_errata_mask & exch_errata)
            return \"push w0\;mov.b %1,WREG\;mov.b WREG,%0\;pop w0\";
          else
            return \"exch %1,w0\;mov.b WREG,%0\;exch %1,w0\";
        } else return \"mov %1,w0\;mov.b WREG,%0\";
        break;
    }
}
   "
   [(set_attr "cc" "change0,change0,change0,change0,change0")
    (set_attr "type" "etc,etc,etc,etc,etc")]
)


;; If one of the operands is immediate and the other is not a register,
;; then we should emit two insns, using a scratch register.  This will produce
;; better code in loops if the source operand is invariant, since
;; the source reload can be optimised out.  During reload we cannot
;; use change_address or force_reg which will allocate new pseudo regs.

;; Unlike most other insns, the move insns can`t be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
        (match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
	pic30_emit_move_sequence(operands, QImode);
}")

;; (define_expand "reload_inqi"
;;   [(set (match_operand:QI 0      "pic30_register_operand" "=d")
;; 	(match_operand:QI 1     "pic30_near_operand" "U"))
;;    (clobber (match_operand:QI 2 "pic30_register_operand" "=&a"))]
;;   ""
;;   "
;; {
;;   emit_move_insn (operands[2], operands[1]);
;;   emit_move_insn (operands[0], operands[2]);
;;   DONE;
;; }")

;; (define_expand "reload_outqi"
;;   [(set (match_operand:QI 0    "pic30_near_operand" "=U")
;; 	(match_operand:QI 1       "pic30_register_operand" "d"))
;;    (clobber (match_operand:QI 2 "pic30_register_operand" "=&a"))]
;;   ""
;;   "
;; {
;;   emit_move_insn (operands[2], operands[1]);
;;   emit_move_insn (operands[0], operands[2]);
;;   DONE;
;; }")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 16-bit moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Invalid move

(define_insn "*movhi_invalid_1"
  [(set (match_operand:HI 0 "pic30_register_operand" "=r")
        (match_operand:HI 1 "pic30_code_operand"  "g"))]
  ""
  "*
{
        error(\"invalid address space for operand\");
        return(\"nop\");
}

")

(define_insn "*movhi_invalid_2"
  [(set (match_operand:HI 0 "pic30_code_operand" "=g")
        (match_operand:HI 1 "pic30_register_operand"  "r"))]
  ""
  "*
{
        error(\"invalid address space for operand\");
        return(\"nop\");
}
")

(define_insn "*movhi_invalid_3"
  [(set (match_operand:HI 0 "pic30_move_operand" "")
        (mem: HI (match_operand:HI 1 "pic30_invalid_address_operand"  "")))]
  ""
  "*
{
        error(\"invalid address space for operand\");
        return(\"nop\");
}
")

;;;;;;;;;;;;;;;;;
;; immediate zero
;;;;;;;;;;;;;;;;;

(define_insn "*movhi_const0sfr"
  [(set (match_operand:HI 0 "pic30_near_operand" "=U")
	(const_int 0))]
  ""
  "clr %0"
  [(set_attr "cc" "unchanged")])

(define_insn "*movhi_const1sfr"
  [(set (match_operand:HI 0 "pic30_near_operand" "=U")
	(const_int -1))]
  ""
  "setm %0"
  [(set_attr "cc" "unchanged")])

(define_insn "*movhi_const0"
  [(set (match_operand:HI 0 "pic30_mode2_operand" "=r,R,<>")
	(const_int 0))]
  ""
  "@
   clr %0
   clr %0
   clr %0"
  [
   (set_attr "cc" "change0,change0,change0")
   (set_attr "type" "def,use,defuse")
  ]
)

(define_insn "*movhi_const1"
  [(set (match_operand:HI 0 "pic30_mode2_operand" "=r,R,<>")
	(const_int -1))]
  ""
  "@
   setm %0 
   setm %0 
   setm %0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def,use,defuse")
  ]
)

;;;;;;;;;;;;;;;;;;;;
;; general immediate
;;;;;;;;;;;;;;;;;;;;

(define_insn "movhi_address"
  [(set (match_operand:HI 0 "pic30_register_operand"              "=r")
        (match_operand:HI 1 "pic30_symbolic_address_operand" "g"))]
  ""
  "*
{
	if (pic30_program_space_operand_p(operands[1]))
	{
		return(\"mov #handle(%1),%0\");
	}
	else
	{
		return(\"mov #%1,%0\");
	}
}"
 [
  (set_attr "cc" "change0")
  (set_attr "type" "def")
 ]
)

(define_insn "movhi_imm"
  [(set (match_operand:HI 0 "pic30_register_operand" "=r")
        (match_operand:HI 1 "immediate_operand" "i"))]
  ""
  "mov #%1,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builtin move directives
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "readsfr"
  [(set (match_operand:HI 0 "pic30_register_operand"           "=r")
        (unspec_volatile:HI [(match_operand:HI 1 "pic30_register_operand" "r")]
		    UNSPECV_READSFR))]
  ""
  "push SR\;bset SR,#5\;bset SR,#6\;bset SR,#7\;mov [%1],[w15]\;mov [%1],%0\;pop SR"
  [(set_attr "cc" "clobber")])

(define_insn "writesfr"
  [(unspec_volatile:HI [
     (match_operand:HI 0 "pic30_register_operand" "r")
     (match_operand:HI 1 "pic30_register_operand" "r")
  ] UNSPECV_WRITESFR)]
  ""
  "push SR\;bset SR,#5\;bset SR,#6\;bset SR,#7\;mov %0,%0\;mov %0,%0\;mov %1,[%0]\;pop SR"
  [(set_attr "cc" "clobber")])

(define_insn "tblpage"
  [(set (match_operand:HI 0 "pic30_register_operand"              "=r")
        (unspec:HI [(match_operand:HI 1 "pic30_symbolic_address_operand" "g")]
		    UNSPECV_TBLPAGE))]
  ""
  "mov #tblpage(%1),%0"
  [(set_attr "cc" "change0")])

(define_insn "tbloffset"
  [(set (match_operand:HI 0 "pic30_register_operand"              "=r")
        (unspec:HI [(match_operand:HI 1 "pic30_symbolic_address_operand" "g")]
		    UNSPECV_TBLOFFSET))]
  ""
  "mov #tbloffset(%1),%0"
  [(set_attr "cc" "change0")])

(define_insn "psvpage"
  [(set (match_operand:HI 0 "pic30_register_operand"              "=r")
        (unspec:HI [(match_operand:HI 1 "pic30_symbolic_address_operand" "g")]
		    UNSPECV_PSVPAGE))]
  ""
  "mov #psvpage(%1),%0"
  [(set_attr "cc" "change0")])

(define_insn "psvoffset"
  [(set (match_operand:HI 0 "pic30_register_operand"              "=r")
        (unspec:HI [(match_operand:HI 1 "pic30_symbolic_address_operand" "g")]
		    UNSPECV_PSVOFFSET))]
  ""
  "mov #psvoffset(%1),%0"
  [(set_attr "cc" "change0")])

(define_insn "dmaoffset"
  [(set (match_operand:HI 0 "pic30_register_operand"              "=r")
        (unspec:HI [(match_operand:HI 1 "pic30_symbolic_address_operand" "g")]
                    UNSPECV_DMAOFFSET))]
  ""
  "mov #dmaoffset(%1),%0"
  [(set_attr "cc" "change0")])


;;;;;;;;;;;;;;;
;; general case
;;;;;;;;;;;;;;;
(define_insn "movhi_gen"
  [(set (match_operand:HI 0
  	"pic30_move_operand" "=r<>, R,   r<>,R,S,S,  Q,r,r,T,a")
        (match_operand:HI 1
	"pic30_move_operand"  "RS<>,RS<>,r,  r,r,<>R,r,Q,T,r,U"))]
  ""
  "@
   mov %1,%0
   mov %1,%0
   mov %1,%0
   mov %1,%0
   mov %1,%0
   mov %1,%0
   mov %1,%0
   mov %1,%0
   mov %1,%0
   mov %1,%0
   mov %1,WREG"
  [(set_attr "cc"
  "change0,change0,change0,change0,change0,change0,change0,change0,change0,change0,move")
   (set_attr "type"
   "defuse,use,def,etc,etc,etc,use,defuse,def,etc,def")
  ])


;; If one of the operands is immediate and the other is not a register,
;; then we should emit two insns, using a scratch register.  This will produce
;; better code in loops if the source operand is invariant, since
;; the source reload can be optimised out.  During reload we cannot
;; use change_address or force_reg which will allocate new pseudo regs.

;; Unlike most other insns, the move insns can`t be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
        (match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
	pic30_emit_move_sequence(operands, HImode);
}")

;;
;; Reload can generate a partial load of a larger item
;;   Recognize the instruction. - Bug in nullstone cse -O[23]
;;

(define_insn "movhi_reload_lo"
  [(set (match_operand:HI 0 "pic30_move_operand"   "=r,R,r,R,a,T,r")
        (subreg:HI 
           (match_operand:SI 1 "pic30_move_operand" "r,r,R,R,U,r,T") 0))]
  ""
  "@
   mov %1,%0
   mov %1,%0
   mov %1,%0
   mov %1,%0
   mov %1,WREG
   mov %1,%0
   mov %1,%0"
  [
   (set_attr "cc" "change0,change0,change0,change0,move,change0,change0")
   (set_attr "type" "def,etc,defuse,use,def,etc,def")
  ]
)

(define_insn "movhi_reload_hi"
  [(set (match_operand:HI 0 "pic30_move_operand"   "=a,r")
        (subreg:HI 
           (match_operand:SI 1 "pic30_move_operand" "U,T") 2))]
  ""
  "@
   mov %1+2,WREG
   mov %1+2,%0"
  [
   (set_attr "cc" "move,change0")
   (set_attr "type" "def,def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 32-bit integer moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invalid move

(define_insn "*movsi_invalid_1"
  [(set (match_operand:SI 0 "pic30_register_operand"  "=r")
        (match_operand:SI 1 "pic30_code_operand" "g"))]
  ""
  "*
{
        error(\"invalid address space for operand\");
        return(\"nop\");
}

")
(define_insn "*movsi_invalid_2"
  [(set (match_operand:SI 0 "pic30_code_operand" "=g")
        (match_operand:SI 1 "pic30_register_operand"    "r"))]
  ""
  "*
{
        error(\"invalid address space for operand\");
        return(\"nop\");
}
")

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
        (match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
	pic30_emit_move_sequence(operands, SImode);
}")

(define_insn "*movsi_const0"
  [(set (match_operand:SI 0 "pic30_mode2res_operand" "=r,R,<,>")
	(const_int 0))]
  ""
  "@
   mul.uu %0,#0,%0
   clr %0\;mov %I0,%D0
   clr %0\;clr %0
   clr %0\;clr %0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def,use,defuse,defuse")
  ]
)

(define_insn "*movsi_const1"
  [(set (match_operand:SI 0 "pic30_mode2res_operand" "=r,R,<,>")
	(const_int -1))]
  ""
  "@
   setm %0\;setm %d0
   setm %I0\;setm %D0
   setm %0\;setm %0
   setm %0\;setm %0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def,use,defuse,defuse")
  ]
)

(define_insn "*movsi_const0sfr"
  [(set (match_operand:SI 0 "pic30_near_operand" "=U")
	(const_int 0))]
  ""
  "clr %0\;clr %0+2"
  [(set_attr "cc" "unchanged")])

(define_insn "*movsi_const1sfr"
  [(set (match_operand:SI 0 "pic30_near_operand" "=U")
	(const_int -1))]
  ""
  "setm %0\;setm %0+2"
  [(set_attr "cc" "unchanged")])

(define_insn "movsi_address"
  [(set (match_operand:SI 0 "pic30_register_operand"              "=r")
        (match_operand:SI 1 "pic30_symbolic_address_operand" "g"))]
  ""
  "mov #%z1,%0\;mov #%y1,%d0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "movsi_rimm"
  [(set (match_operand:SI 0 "pic30_register_operand" "=r,r")
        (match_operand:SI 1 "immediate_operand" "O,i"))]
  ""
  "@
   mul.uu %0,#0,%0
   mov #%z1,%0\;mov #%y1,%d0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "movsi_gen"
  [(set (match_operand:SI 0 "pic30_move_operand" "=r,r,r,r,R,>,>,Q,r,<,r,T")
        (match_operand:SI 1 "pic30_move_operand"  "r,R,>,Q,r,r,>,r,<,r,T,r"))]
  ""
  "*
{
        int idDst, idSrc, pre;

	switch (which_alternative)
	{
	case 0: /* r = r */
		return \"mov.d %1,%0\";
	case 1: /* r = R */
		return \"mov.d %1,%0\";
	case 2: /* r = > */
		if ((pre = pic30_pp_modify_valid(operands[1])) == 0) 
			return \"mov.d %1,%0\";
 		else if (pre == -1) /* pre increment */
			return \"add %r1,#4,%r1\;mov.d %s1,%0\";
		else if (pre == 1)  /* post increment */
			return \"mov.d %s1,%0\;add %r1,#4,%r1\";
	case 3: /* r = Q */
		idDst = REGNO(operands[0]);
		idSrc = REGNO(XEXP(XEXP(operands[1],0),0));
		if (idDst == idSrc)
		{
			return \"mov %Q1,%d0\;mov %1,%0\";
		} else {
			return \"mov %1,%0\;mov %Q1,%d0\";
		}
	case 4: /* R = r */
		return \"mov.d %1,%0\";
	case 5: /* > = r */
		return \"mov.d %1,%0\";
	case 6: /* > = > */
		return \"mov %1,%0\;mov %1,%0\";
	case 7: /* Q = r */
		return \"mov %1,%0\;mov %d1,%Q0\";
	case 8: /* r = < */
		if ((pre = pic30_pp_modify_valid(operands[1])) == 0) 
			return \"mov.d %1,%0\";
 		else if (pre == -1) /* pre decrement */
			return \"sub %r1,#4,%r1\;mov.d %s1,%0\";
		else if (pre == 1)  /* post decrement */
			return \"mov.d %s1,%0\;sub %r1,#4,%r1\";
	case 9: /* < = r */
		return \"mov.d %1,%0\";
	case 10: /* r = T */
		return \"mov %1,%0\;mov %Q1,%d0\";
	case 11: /* T = r */
		return \"mov %1,%0\;mov %d1,%Q0\";
	default:
		return \";\";
	}
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type"
             "def,defuse,defuse,def,etc,def,defuse,etc,defuse,def,def,etc")
  ]
)

(define_insn "*movsi_constnsfr"
  [(set (match_operand:SI 0 "pic30_near_operand" "=U")
        (match_operand:SI 1 "immediate_operand"   "i"))
	(clobber (match_scratch:HI 2             "=r"))]
  ""
  "*
{
	int imm = INTVAL(operands[1]);
	unsigned short msw, lsw;
	msw = (imm >> 16) & 0xffff;
	lsw = (imm) & 0xffff;
	if (msw == 0)
	{
		if (lsw == 0xffff)
		{
			return(\"setm %0\;clr %0+2\");
		}
		else
		{
			return(\"mov #%1,%2\;mov %2,%0\;clr %0+2\");
		}
	}
	else if (lsw == 0)
	{
		if (msw == 0xffff)
		{
			return(\"setm %0+2\;clr %0\");
		}
		else
		{
			return(\"mov #%y1,%2\;mov %2,%0+2\;clr %0\");
		}
	}
	else if (lsw == msw)
	{
		return(\"mov #%z1,%2\;mov %2,%0\;mov %2,%0+2\");
	}
	else
	{
		return(\"mov #%z1,%2\;mov %2,%0\;mov #%y1,%2\;mov %2,%0+2\");
	}

}"
  [(set_attr "cc" "clobber")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 64-bit integer moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Invalid move

(define_insn "*movdi_invalid_1"
  [(set (match_operand:DI 0 "pic30_register_operand"  "=r")
        (match_operand:DI 1 "pic30_code_operand" "g"))]
  ""
  "*
{
        error(\"invalid address space for operand\");
        return(\"nop\");
}

")
(define_insn "*movdi_invalid_2"
  [(set (match_operand:DI 0 "pic30_code_operand" "=g")
        (match_operand:DI 1 "pic30_register_operand"    "r"))]
  ""
  "*
{
        error(\"invalid address space for operand\");
        return(\"nop\");
}
")


(define_insn "*movdi_rimm"
  [(set (match_operand:DI 0 "pic30_register_operand" "=r,r")
        (match_operand:DI 1 "immediate_operand" "O,i"))]
  ""
  "*
{
	REAL_VALUE_TYPE r;
	long l[4] = { 0 };

	switch (which_alternative)
	{
	case 0:
          return(\"mul.uu %0,#0,%0\;\"
                 \"mul.uu %t0,#0,%t0\");
	default:
          if (GET_CODE(operands[1]) == CONST_DOUBLE)
          {
            REAL_VALUE_FROM_CONST_DOUBLE(r, operands[1]);
            switch (GET_MODE(operands[1]))
            {
              case VOIDmode:
                /*
		** Integer
		*/
                l[0] = CONST_DOUBLE_LOW(operands[1]);
                l[1] = CONST_DOUBLE_HIGH(operands[1]);
              	break;
              default:
                REAL_VALUE_TO_TARGET_LONG_DOUBLE(r, l);
                break;
            }
          }
          else
          {
            l[0] = INTVAL(operands[1]);
            l[1] = l[0] < 0 ? -1 : 0;
          }
          if (l[0] == 0)
          {
            return(\"mul.uu %0,#0,%0\;\"
                   \"mov #%x1,%t0\;\"
                   \"mov #%w1,%q0\");
          }
          else if (l[1] == 0)
          {
            return(\"mov #%z1,%0\;\"
                   \"mov #%y1,%d0\;\"
                   \"mul.uu %t0,#0,%t0\");
          }
          else if (l[0] == l[1])
          {
            return(\"mov #%z1,%0\;\"
                   \"mov #%y1,%d0\;\"
                   \"mov.d %0,%t0\");
          }
          return(\"mov #%z1,%0\;\"
                 \"mov #%y1,%d0\;\"
                 \"mov #%x1,%t0\;\"
                 \"mov #%w1,%q0\");
        }
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "*movdi_gen"
  [(set (match_operand:DI 0 "pic30_move_operand"
					"=r,r,r,r,R,>,>,Q,r,<,T,r")
        (match_operand:DI 1 "pic30_move_operand" 
					 "r,R,>,Q,r,r,>,r,<,r,r,T"))]
  ""
  "*
{
	int idSrc, idDst;
	char temp[48];
	char save[48];
	static char szInsn[48];

	szInsn[0] = 0;
	temp[0] = 0;
	save[0] = 0;

	switch (which_alternative)
	{
	case 0: /* r = r */
		idDst =	REGNO(operands[0]);
		idSrc =	REGNO(operands[1]);
		if (idDst <= idSrc)
		{
			return \"mov.d %1,%0\;mov.d %t1,%t0\";
		}
		else
		{
			return \"mov.d %t1,%t0\;mov.d %1,%0\";
		}
	case 1: /* r = R */
		idDst =	REGNO(operands[0]);
		idSrc =	REGNO(XEXP(operands[1],0));
		if (pic30_pp_modify_valid(0) == 0) {
			if ((idDst > idSrc) || ((idDst+4) <= idSrc))
			{
				/*
				** source & dest don't overlap
				*/
				return	\"mov.d %I1,%0\;\" \"mov.d %D1,%t0\";
		   }
			if ((idDst+2) > idSrc)
			{
				/*
				** [wn] -> wn+2:wn+3:wn:wn+1
				*/
				return \"mov.d %P1,%t0\;mov.d %p1,%0\";
			}
			else
			{
				/*
				** [wn] -> wn-2:wn-1:wn:wn+1
				*/
				return \"mov.d %I1,%0\;mov.d %1,%t0\";
			}
		} else { 
			if ((idDst > idSrc) || ((idDst + 3) <= idSrc)) {
				/*  don't significantly overlap */ 
				return \"mov.d %1,%0\;mov [%r1+4],%t0\;mov [%r1+6],%q0\";
			}
			/* idDst <= idSrc < idDst+3 */
			switch (idDst + 4 - idSrc) {
				case 4:  /* idSrc == idDst+0 */
				case 3:  /* idSrc == idDst+1 */
					return \"mov [%r1+4],%t0\;mov [%r1+6],%q0\;mov.d %1,%0\";
				case 2:  /* idSrc == idDst+2 */
					return \"mov.d %1,%0\;mov [%r1+6],%q0\;mov [%r1+4],%t0\";
				default: abort();
			}
		}
	case 2: /* r = > */
		if (pic30_pp_modify_valid(0) == 0)
			return \"mov.d %1,%0\;mov.d %1,%t0\";
		else return \"mov %1,%0\;mov %1,%d0\;mov %1,%t0\;mov %1,%q0\";
	case 3: /* r = Q */
		idDst =	REGNO(operands[0]);
		idSrc =	REGNO(XEXP(XEXP(operands[1],0),0));
		strcpy(temp, \"mov %1,%0\;\");
		if (idDst != idSrc)
			strcat(szInsn, temp);
		else
			strcat(save, temp);
		idDst++;
		strcpy(temp, \"mov %Q1,%d0\;\");
		if (idDst != idSrc)
			strcat(szInsn, temp);
		else
			strcat(save, temp);
		idDst++;
		strcpy(temp, \"mov %R1,%t0\;\");
		if (idDst != idSrc)
			strcat(szInsn, temp);
		else
			strcat(save, temp);
		idDst++;
		strcpy(temp, \"mov %S1,%q0\;\");
		if (idDst != idSrc)
			strcat(szInsn, temp);
		else
			strcat(save, temp);
		idDst++;
		strcat(szInsn, save);
		return(szInsn);
	case 4: /* R = r */
		return	\"mov.d %1,%I0\;\" \"mov.d %t1,%D0\";
	case 5: /* > = r */
		return \"mov.d %1,%0\;mov.d %t1,%0\";
	case 6: /* > = > */
		return \"mov %1,%0\;mov %1,%0\;mov %1,%0\;mov %1,%0\";
	case 7: /* Q = r */
		return \"mov %1,%0\;mov %d1,%Q0\;mov %t1,%R0\;mov %q1,%S0\";
	case 8: /* r = < */
		if (pic30_pp_modify_valid(0) == 0)
			return \"mov.d %1,%t0\;mov.d %1,%0\";
		else return \"mov %1,%q0\;mov %1,%t0\;mov %1,%d0\;mov %1,%0\";
	case 9: /* < = r */
		return \"mov.d %t1,%0\;mov.d %1,%0\";
	case 10: /* T = r */
		return	\"mov %1,%0\;\"
			\"mov %d1,%Q0\;\"
			\"mov %t1,%R0\;\"
			\"mov %q1,%S0\";
	case 11: /* r = T */
		return	\"mov %1,%0\;\"
			\"mov %Q1,%d0\;\"
			\"mov %R1,%t0\;\"
			\"mov %S1,%q0\";
	default:
		return \";\";
	}
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type"
             "def,defuse,defuse,defuse,etc,def,defuse,etc,defuse,def,etc,def")
  ]
)

(define_expand "movdi"
  [(set (match_operand:DI 0 "general_operand" "")
        (match_operand:DI 1 "general_operand" ""))]
  ""
  "
{
	pic30_emit_move_sequence(operands, DImode);
}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Single Float (32 bit) moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Invalid move

(define_insn "*movsf_invalid_1"
  [(set (match_operand:SF 0 "pic30_register_operand"  "=r")
        (match_operand:SF 1 "pic30_code_operand" "g"))]
  ""
  "*
{
        error(\"invalid address space for operand\");
        return(\"nop\");
}

")
(define_insn "*movsf_invalid_2"
  [(set (match_operand:SF 0 "pic30_code_operand" "=g")
        (match_operand:SF 1 "pic30_register_operand"    "r"))]
  ""
  "*
{
        error(\"invalid address space for operand\");
        return(\"nop\");
}
")


(define_insn "*movsf_rimm"
  [(set (match_operand:SF 0 "pic30_register_operand" "=r,r")
        (match_operand:SF 1 "immediate_operand" "G,i"))]
  ""
  "*
{
	switch (which_alternative)
	{
	case 0:
		return(\"mul.uu %0,#0,%0\;\");
	default:
		return(\"mov #%x1,%0\;mov #%w1,%d0\");
	}
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "movsf_general"
  [(set (match_operand:SF 0 "pic30_move_operand"
					"=r,r,r,r,R,R,>,>,Q,r,<,r,T")
        (match_operand:SF 1 "pic30_move_operand" 
					 "r,R,>,Q,r,R,r,>,r,<,r,T,r"))]
  ""
  "*
{
        int idDst, idSrc, pre;

	switch (which_alternative)
	{
	case 0: /* r = r */
		return \"mov.d %1,%0\";
	case 1: /* r = R */
		return \"mov.d %1,%0\";
	case 2: /* r = > */
		if ((pre = pic30_pp_modify_valid(operands[1])) == 0)
			return \"mov.d %1,%0\";
		else if (pre == -1) /* pre increment */
			return \"add %r1,#4,%r1\;mov.d %s1,%0\";
		else if (pre == 1)  /* post increment */
			return \"mov.d %s1,%0\;add %r1,#4,%r1\";
	case 3: /* r = Q */
                idDst = REGNO(operands[0]);
                idSrc = REGNO(XEXP(XEXP(operands[1],0),0));
                if (idDst == idSrc)
                {
                        return \"mov %Q1,%d0\;mov %1,%0\";
                }
                else
                {
                        return \"mov %1,%0\;mov %Q1,%d0\";
                }
	case 4: /* R = r */
		return \"mov.d %1,%0\";
	case 5: /* R = R */
		return \"mov %I1,%I0\;mov %D1,%D0\";
	case 6: /* > = r */
		return \"mov.d %1,%0\";
	case 7: /* > = > */
		return \"mov %1,%0\;mov %1,%0\";
	case 8: /* Q = r */
		return \"mov %1,%0\;mov %d1,%Q0\";
	case 9: /* r = < */
		if ((pre = pic30_pp_modify_valid(operands[1])) == 0)
			return \"mov.d %1,%0\";
		else if (pre == -1) /* pre decrement */
			return \"sub %r1,#4,%r1\;mov.d %s1,%0\";
		else if (pre == 1)  /* post decrement */
			return \"mov.d %s1,%0\;sub %r1,#4,%r1\";
	case 10: /* < = r */
		return \"mov.d %1,%0\";
	case 11: /* r = T */
		return \"mov %1,%0\;mov %Q1,%d0\";
	case 12: /* T = r */
		return \"mov %1,%0\;mov %d1,%Q0\";
	default:
		return \";\";
	}
}"
 [
  (set_attr "cc" "clobber")
  (set_attr "type" "def,defuse,defuse,defuse,etc,use,def,defuse,etc,defuse,def,def,etc")
 ]
)

(define_insn "*movsf_constnsfr"
  [(set (match_operand:SF 0 "pic30_near_operand" "=U,U")
        (match_operand:SF 1 "immediate_operand"   "G,i"))
	(clobber (match_scratch:HI 2             "=X,r"))]
  ""
  "*
{
	REAL_VALUE_TYPE r;
	long l = 0;

	switch (which_alternative)
	{
	case 0:
  		return(\"clr %0\;clr %0+2\");
	default:
		REAL_VALUE_FROM_CONST_DOUBLE(r, operands[1]);
		REAL_VALUE_TO_TARGET_SINGLE(r, l);
		if (l & 0xFFFF == 0)
		{
			return(	\"clr %0\;\"
				\"mov #%w1,%2\;\"
				\"mov %2,%0+2\");
		}
		else
		{
			return(	\"mov #%x1,%2\;\"
				\"mov %2,%0\;\"
				\"mov #%w1,%2\;\"
				\"mov %2,%0+2\");
		}
	}

}"
  [(set_attr "cc" "clobber")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
        (match_operand:SF 1 "general_operand" ""))]
  ""
  "
{
	pic30_emit_move_sequence(operands, SFmode);
}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Double float (64 bit) moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Invalid move

(define_insn "*movdf_invalid_1"
  [(set (match_operand:DF 0 "pic30_register_operand"  "=r")
        (match_operand:DF 1 "pic30_code_operand" "g"))]
  ""
  "*
{
        error(\"invalid address space for operand\");
        return(\"nop\");
}

")
(define_insn "*movdf_invalid_2"
  [(set (match_operand:DF 0 "pic30_code_operand" "=g")
        (match_operand:DF 1 "pic30_register_operand"    "r"))]
  ""
  "*
{
        error(\"invalid address space for operand\");
        return(\"nop\");
}
")


(define_insn "*movdf_rimm"
  [(set (match_operand:DF 0 "pic30_register_operand" "=r,r")
        (match_operand:DF 1 "immediate_operand" "G,i"))]
  ""
  "*
{
	REAL_VALUE_TYPE r;
	long l[4] = { 0 };

	switch (which_alternative)
	{
	case 0:
		return(	\"mul.uu %0,#0,%0\;\"
			\"mul.uu %t0,#0,%t0\");
	default:
		REAL_VALUE_FROM_CONST_DOUBLE(r, operands[1]);
		REAL_VALUE_TO_TARGET_DOUBLE(r, l);
		if (l[0] == 0)
		{
			return( \"mul.uu %0,#0,%0\;\"
				\"mov #%x1,%t0\;\"
				\"mov #%w1,%q0\");
		}
		else
		{
			return( \"mov #%z1,%0\;\"
				\"mov #%y1,%d0\;\"
				\"mov #%x1,%t0\;\"
				\"mov #%w1,%q0\");
		}
	}
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn_and_split "*movdf_rr"
  [(set (match_operand:DF 0 "pic30_register_operand" "=r")
        (match_operand:DF 1 "pic30_register_operand" " r"))]
  ""
  "mov.d %1,%0\;mov.d %t1,%t0"
  "reload_completed"
  [
   (set (subreg:SI (match_dup 0) 0) (subreg:SI (match_dup 1) 0))
   (set (subreg:SI (match_dup 0) 4) (subreg:SI (match_dup 1) 4))
  ]
  ""
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def")
  ]
)

(define_insn "movdf_gen"
  [(set (match_operand:DF 0 "pic30_move_operand"
					"=r,r,r,r,r,R,>,>,Q,<,r,T")
        (match_operand:DF 1 "pic30_move_operand" 
					 "r,R,<,>,Q,r,r,>,r,r,T,r"))]
  ""
  {
	int idSrc, idDst;
	char temp[48];
	char save[48];
static	char insn[48];

	insn[0] = 0;
	temp[0] = 0;
	save[0] = 0;
	switch (which_alternative)
	{
	case 0: /* r = r */
		idDst =	REGNO(operands[0]);
		idSrc =	REGNO(operands[1]);
		if (idDst <= idSrc)
		{
			return("mov.d %1,%0\;mov.d %t1,%t0");
		}
		else
		{
			return("mov.d %t1,%t0\;mov.d %1,%0");
		}
	case 1: /* r = R */
		idDst =	REGNO(operands[0]);
		idSrc =	REGNO(XEXP(operands[1],0));
		if (pic30_pp_modify_valid(0) == 0)
		{
			if ((idDst > idSrc) || ((idDst+4) <= idSrc))
			{
				/*
				** source & dest don't overlap
				*/
				return("mov.d %I1,%0\;mov.d %D1,%t0");
			}
			if ((idDst+2) > idSrc)
			{
				/*
				** [wn] -> wn+2:wn+3:wn:wn+1
				*/
				return("mov.d %P1,%t0\;mov.d %p1,%0");
			}
			else
			{
				/*
				** [wn] -> wn-2:wn-1:wn:wn+1
				*/
				return("mov.d %I1,%0\;mov.d %1,%t0");
			}
		}
		else
		{
			if ((idDst > idSrc) || ((idDst + 3) <= idSrc))
			{
				/*
				** don't signifcantly overlap
				*/
				return("mov.d %1,%0\;"
				       "mov [%r1+4],%t0\;"
				       "mov [%r1+6],%q0");
			}
			/* idDst <= idSrc < idDst+3 */
			switch (idDst + 4 - idSrc)
			{
			case 4:  /* idSrc == idDst+0 */
			case 3:  /* idSrc == idDst+1 */
				return("mov [%r1+4],%t0\;"
				       "mov [%r1+6],%q0\;"
				       "mov.d %1,%0");
			case 2:  /* idSrc == idDst+2 */
				return("mov.d %1,%0\;"
				       "mov [%r1+6],%q0\;"
				       "mov [%r1+4],%t0");
			default:
				abort();
			}
		}
	case 2: /* r = < */
		idDst =	REGNO(operands[0]);
		if (pic30_pp_modify_valid(0) == 0)                
		{
			return("mov.d %1,%t0\;mov.d %1,%0");
		}
      		else
		{
			return("mov %1,%q0\;"
			       "mov %1,%t0\;"
			       "mov %1,%d0\;"
			       "mov %1,%0");
		}
	case 3: /* r = > */
		idDst =	REGNO(operands[0]);
		if (pic30_pp_modify_valid(0) == 0)
		{
			return("mov.d %1,%0\;mov.d %1,%t0");
		}
		else
		{
			return("mov %1,%0\;"
			       "mov %1,%d0\;"
			       "mov %1,%t0\;"
			       "mov %1,%q0");
		}
	case 4: /* r = Q */
		idDst =	REGNO(operands[0]);
		idSrc =	REGNO(XEXP(XEXP(operands[1],0),0));
 		strcpy(temp, "mov %1,%0\;");
 		if (idDst != idSrc)
 			strcat(insn, temp);
 		else
 			strcat(save, temp);
 		idDst++;
 		strcpy(temp, "mov %Q1,%d0\;");
 		if (idDst != idSrc)
 			strcat(insn, temp);
 		else
 			strcat(save, temp);
 		idDst++;
 		strcpy(temp, "mov %R1,%t0\;");
 		if (idDst != idSrc)
 			strcat(insn, temp);
 		else
 			strcat(save, temp);
 		idDst++;
 		strcpy(temp, "mov %S1,%q0\;");
 		if (idDst != idSrc)
 			strcat(insn, temp);
 		else
 			strcat(save, temp);
 		idDst++;
 		strcat(insn, save);
		return(insn);
	case 5: /* R = r */
		idSrc =	REGNO(operands[1]);
		return("mov.d %1,%I0\;mov.d %t1,%D0");
	case 6: /* > = r */
		idSrc =	REGNO(operands[1]);
		return("mov.d %1,%0\;mov.d %t1,%0");
	case 7: /* > = > */
		return("mov %1,%0\;mov %1,%0\;mov %1,%0\;mov %1,%0");
	case 8: /* Q = r */
		return("mov %1,%0\;mov %d1,%Q0\;mov %t1,%R0\;mov %q1,%S0");
	case 9: /* < = r */
		idSrc =	REGNO(operands[1]);
		return("mov.d %t1,%0\;mov.d %1,%0");
	case 10: /* r = T */
		return("mov #%1,%0\;mov.d [++%0],%t0\;mov.d [--%0],%0");
	case 11: /* T = r */
		return("mov %1,%0\;mov %d1,%0+2\;mov %t1,%0+4\;mov %q1,%0+6");
	default: /* ? = ? */
		abort();
	}
	return("?mov? %1,%0");
  }
 [(set_attr "cc" "clobber")
  (set_attr "type"
            "def,defuse,defuse,defuse,defuse,etc,def,defuse,etc,def,def,etc")
 ]
)


(define_expand "movdf"
  [(set (match_operand:DI 0 "general_operand" "")
        (match_operand:DI 1 "general_operand" ""))]
  ""
  "
{
	pic30_emit_move_sequence(operands, DFmode);
}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add instructions 
;;
;; During the instruction canonicalization phase,
;; (minus x (const_int n)) is converted to (plus x (const_int -n)).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; quarter integer
;;;;;;;;;;;;;;;;;;

(define_insn "*addqi3_incdec"
  [(set (match_operand:QI 0 "pic30_mode2_operand"
  			"=r<>,R,r<>,R")
        (plus:QI (match_operand:QI 1 "pic30_math_operand"
			 "r,  r,R<>,R<>")
                 (match_operand:QI 2 "immediate_operand"
			 "i,  i,i,  i")))]
 "(-2<=INTVAL(operands[2]))&&(INTVAL(operands[2])!=0)&&(INTVAL(operands[2])<=2)"
  "*
{
	switch (INTVAL(operands[2]))
	{
	case -2:
		return(\"dec2.b %1,%0\");
	case -1:
		return(\"dec.b %1,%0\");
	case 1:
		return(\"inc.b %1,%0\");
	case 2:
		return(\"inc2.b %1,%0\");
	default:
		abort();
		return(\"nop\");
	}
}"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def,etc,defuse,use")
  ]
)

(define_insn "*addqi3_incdecsfr"
  [(set (match_operand:QI 0 "pic30_wreg_or_near_operand"  "=U,a")
        (plus:QI (match_operand:QI 1 "pic30_near_operand" "%0,U")
                 (match_operand:QI 2 "immediate_operand"   "i,i")))]
 "(-2<=INTVAL(operands[2]))&&(INTVAL(operands[2])!=0)&&(INTVAL(operands[2])<=2)"
  "*
{
	switch (INTVAL(operands[2]))
	{
	case -2:
		switch (which_alternative)
		{
		case 0:
			return(\"dec2.b %0\");
		case 1:
			return(\"dec2.b %1,WREG\");
		}
	case -1:
		switch (which_alternative)
		{
		case 0:
			return(\"dec.b %0\");
		case 1:
			return(\"dec.b %1,WREG\");
		}
	case 1:
		switch (which_alternative)
		{
		case 0:
			return(\"inc.b %0\");
		case 1:
			return(\"inc.b %1,WREG\");
		}
	case 2:
		switch (which_alternative)
		{
		case 0:
			return(\"inc2.b %0\");
		case 1:
			return(\"inc2.b %1,WREG\");
		}
	default:
		abort();
	}
}"
  [
   (set_attr "cc" "math")
   (set_attr "type" "etc,def")
  ]
)

(define_insn "addqi3"
  [(set (match_operand:QI 0 "pic30_mode2_operand"
	"=r<>,r<>,R,R,  r<>,R,r<>,R,r<>,R,  r<>,R,r<>,R,r,r")
        (plus:QI (match_operand:QI 1 "pic30_math_operand"
	"%r,  r,  r,r,  r,  r,r,  r,R<>,R<>,N,  N,P,  P,0,J")
                 (match_operand:QI 2 "pic30_mode1JN_operand"
	 "r,  R<>,r,R<>,N,  N,P,  P,r,  r,  r,  r,r,  r,J,0")))]
  ""
  "@
   add.b %1,%2,%0
   add.b %1,%2,%0
   add.b %1,%2,%0
   add.b %1,%2,%0
   sub.b %1,#%J2,%0
   sub.b %1,#%J2,%0
   add.b %1,#%2,%0
   add.b %1,#%2,%0
   add.b %2,%1,%0
   add.b %2,%1,%0
   sub.b %2,#%J1,%0
   sub.b %2,#%J1,%0
   add.b %2,#%1,%0
   add.b %2,#%1,%0
   add.b #%2,%0
   add.b #%1,%0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def,defuse,etc,use,def,etc,def,etc,defuse,use,def,etc,def,etc,def,def")
  ]
)

(define_insn "*addqi3_sfr0"
  [(set (match_operand:QI 0 "pic30_near_operand"         "=U")
        (plus:QI (match_dup 0)
                 (match_operand:QI 1 "pic30_wreg_operand" "a")))]
  ""
  "add.b %0"
  [(set_attr "cc" "math")])

(define_insn "*addqi3_sfr1"
  [(set (match_operand:QI 0 "pic30_near_operand"         "=U")
        (plus:QI (match_operand:QI 1 "pic30_wreg_operand" "a")
                 (match_dup 0)))]
  ""
  "add.b %0"
  [(set_attr "cc" "math")])

(define_insn_and_split "*addqi3_sfr2"
  [(set (match_operand:QI 0 "pic30_register_operand"            "=a ,a, d")
        (plus:QI (match_operand:QI 1 "pic30_near_operand" "%U ,U, U")
                 (match_operand:QI 2 "pic30_register_operand"   " a ,d, d")))
   (clobber (match_scratch:HI 3                           "=X ,X,&r"))]
  ""
  "@
   add.b %1,WREG
   mov.b %2,w0\;add.b %1,WREG
   mov #%1,%3\;add.b %2,[%3],%0"
  "reload_completed"
  [
   (const_int 0)
  ]
{
  if (!pic30_wreg_operand(operands[0], QImode) &&
      !pic30_wreg_operand(operands[2], QImode))
  {
  	rtx pop = gen_rtx_MEM(QImode, operands[3]);
	emit_insn(gen_movhi_address(operands[3], XEXP(operands[1],0)));
	emit_insn(gen_addqi3(operands[0], operands[2], pop));
  	DONE;
  }
  else
  {
  	FAIL;
  }
}
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;
;; half integer
;;;;;;;;;;;;;;;

(define_insn "*addhi3_incdec"
  [(set (match_operand:HI 0 "pic30_mode2_operand"
  		"=r<>,r<>,R,R")
        (plus:HI (match_operand:HI 1 "pic30_math_operand"
		 "R<>,r,  r,R<>")
                 (match_operand:HI 2 "pic30_inc_imm_operand"
		 "L,  L,  L,L")))]
 ""
  "*
{
	switch (INTVAL(operands[2]))
	{
	case -2:
		return(\"dec2 %1,%0\");
	case -1:
		return(\"dec %1,%0\");
	case 1:
		return(\"inc %1,%0\");
	case 2:
		return(\"inc2 %1,%0\");
	default:
		abort();
	}
}"
 [
  (set_attr "cc" "math")
  (set_attr "type" "defuse,def,etc,use")
 ]
)

(define_insn "addhi3"
  [(set (match_operand:HI 0 "pic30_mode2_operand"
		"=r<>,r<>,R,R,  r<>,R,r<>,R,R,r<>,R,  r<>,r<>,R,r<>,R,r,r,!?r")
        (plus:HI (match_operand:HI 1 "pic30_math_operand"
		"%r,  r,  r,r,  r,  r,r,  r,r,r,  R<>,R<>,N,  N,P,  P,0,J,  J")
                 (match_operand:HI 2 "pic30_math_operand"
		 "r,  R<>,r,R<>,N,  N,P,  P,r,r,  r,  r,  r,  r,r,  r,J,0,  r"))
  )]
  ""
  "@
   add %1,%2,%0
   add %1,%2,%0
   add %1,%2,%0
   add %1,%2,%0
   sub %1,#%J2,%0
   sub %1,#%J2,%0
   add %1,#%2,%0
   add %1,#%2,%0
   add %2,%1,%0
   add %2,%1,%0
   add %2,%1,%0
   add %2,%1,%0
   sub %2,#%J1,%0
   sub %2,#%J1,%0
   add %2,#%1,%0
   add %2,#%1,%0
   add #%2,%0
   add #%1,%0
   mov #%1,%0\;add %0,%2,%0"
  [(set_attr "cc" "math")
   (set_attr "type" "def,defuse,etc,use,def,etc,def,etc,etc,def,use,defuse,def,etc,def,etc,def,def,def")
  ])

(define_insn "*addhi3_sfr0"
  [(set (match_operand:HI 0 "pic30_near_operand"         "=U")
        (plus:HI (match_dup 0)
                 (match_operand:HI 1 "pic30_wreg_operand" "a")))]
  ""
  "add %0"
  [(set_attr "cc" "math")])

(define_insn "*addhi3_sfr1"
  [(set (match_operand:HI 0 "pic30_near_operand"         "=U")
        (plus:HI (match_operand:HI 1 "pic30_wreg_operand" "a")
                 (match_dup 0)))]
  ""
  "add %0"
  [(set_attr "cc" "math")])

(define_insn_and_split "*addhi3_sfr2"
  [(set (match_operand:HI 0 "pic30_register_operand"            "=a ,a, d")
        (plus:HI (match_operand:HI 1 "pic30_near_operand" "%U ,U, U")
                 (match_operand:HI 2 "pic30_register_operand"   " a ,d, d")))
   (clobber (match_scratch:HI 3                           "=X ,X,&r"))]
  ""
  "@
   add %1,WREG
   mov %2,w0\;add %1,WREG
   mov #%1,%3\;add %2,[%3],%0"
  "reload_completed"
  [
   (set (match_dup 3) (match_dup 1))
   (set (match_dup 0) (plus:HI (match_dup 2) (match_dup 3)))
  ]
{
  if (!pic30_wreg_operand(operands[0], HImode) &&
      !pic30_wreg_operand(operands[2], HImode))
  {
  }
  else
  {
  	FAIL;
  }
}
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "*addhi3_incdecsfr"
  [(set (match_operand:HI 0 "pic30_wreg_or_near_operand"  "=U,a,d")
        (plus:HI (match_operand:HI 1 "pic30_near_operand" "%0,U,U")
                 (match_operand:HI 2 "immediate_operand"  " i,i,i")))]
 "(-2<=INTVAL(operands[2]))&&(INTVAL(operands[2])!=0)&&(INTVAL(operands[2])<=2)"
  "*
{
	switch (INTVAL(operands[2]))
	{
	case -2:
		switch (which_alternative)
		{
		case 0:
			return(\"dec2 %0\");
		case 1:
			return(\"dec2 %1,WREG\");
		case 2:
			return(\"mov %1,%0\;dec2 %0,%0\");
		default:
			abort();
		}
	case -1:
		switch (which_alternative)
		{
		case 0:
			return(\"dec %0\");
		case 1:
			return(\"dec %1,WREG\");
		case 2:
			return(\"mov %1,%0\;dec %0,%0\");
		default:
			abort();
		}
	case 1:
		switch (which_alternative)
		{
		case 0:
			return(\"inc %0\");
		case 1:
			return(\"inc %1,WREG\");
		case 2:
			return(\"mov %1,%0\;inc %0,%0\");
		default:
			abort();
		}
	case 2:
		switch (which_alternative)
		{
		case 0:
			return(\"inc2 %0\");
		case 1:
			return(\"inc2 %1,WREG\");
		case 2:
			return(\"mov %1,%0\;inc2 %0,%0\");
		default:
			abort();
		}
	default:
		abort();
	}
}"
  [
   (set_attr "cc" "math")
   (set_attr "type" "etc,def,def")
  ]
)

;;;;;;;;;;;;;;;;;
;; single integer
;;;;;;;;;;;;;;;;;

(define_insn "*addsihi3"
  [(set (match_operand:SI 0 "pic30_register_operand"         "=r")
        (plus:SI (match_operand:SI 1 "pic30_register_operand" "r")
                 (zero_extend:SI (match_operand:HI 2 "pic30_register_operand" "r"))))]
  ""
  "add %1,%2,%0\;addc %d1,#0,%d0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "*addhisi3"
  [(set (match_operand:SI 0 "pic30_register_operand"                         "=r")
        (plus:SI (zero_extend:SI (match_operand:HI 1 "pic30_register_operand" "r"))
                                 (match_operand:SI 2 "pic30_register_operand" "r")))]
  ""
  "add %2,%1,%0\;addc %d2,#0,%d0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "*addsi3_imm"
  [(set (match_operand:SI 0 "pic30_register_operand"          "=r,r,r,r")
        (plus:SI (match_operand:SI 1 "pic30_register_operand" "%r,r,0,0")
                 (match_operand:SI 2 "pic30_JM_operand"  "P,N,J,M")))]
  ""
  "@
   add %1,#%2,%0\;addc %d1,#0,%d0
   sub %1,#%J2,%0\;subb %d1,#0,%d0
   add #%2,%0\;addc #0,%d0
   sub %0,#%J2\;subb %d0,#0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "*addsi3_immmsw"
  [(set (match_operand:SI 0 "pic30_register_operand"          "=r,r")
        (plus:SI (match_operand:SI 1 "pic30_register_operand" "%0,r")
                 (match_operand:SI 2 "immediate_operand" "i,i")))]
  "((INTVAL(operands[2]) & 0x0000FFFF) == 0) && 
   (-31 < (INTVAL(operands[2]) >> 16)) && ((INTVAL(operands[2]) >> 16) < 31)"
  "*
{
	int i = INTVAL(operands[2]) >> 16;
static	char szInsn[48];
	switch (which_alternative)
	{
	case 0:
		if (i < 0)
   			sprintf(szInsn, \"sub %%d1,#%d,%%d0\", -i);
		else
			sprintf(szInsn, \"add %%d1,#%d,%%d0\", i);
		break;
	case 1:
		if (i < 0)
   			sprintf(szInsn,
				\"sub %%1,#0,%%0\;subb %%d1,#%d,%%d0\",-i);
		else
   			sprintf(szInsn,
				\"add %%1,#0,%%0\;addc %%d1,#%d,%%d0\", i);
		break;
	}
	return(szInsn);
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "addsi3"
  [(set (match_operand:SI 0 "pic30_rR_or_near_operand"
               "=r,r,r,&r,r,r,r,R,R,R")
        (plus:SI (match_operand:SI 1 "pic30_rR_or_near_operand"
               "%r,0,r, R,r,r,0,r,r,r")
                 (match_operand:SI 2 "pic30_rR_or_JN_operand"
                "r,r,0, r,P,N,J,r,0,R")))]
  ""
  "*
{
   static char *patterns[] = {
      \"add %2,%1,%0\;addc %d2,%d1,%d0\",
      \"add %2,%1,%0\;addc %d2,%d1,%d0\",
      \"add %2,%1,%0\;addc %d2,%d1,%d0\",
      \"add %2,%I1,%0\;addc %d2,%D1,%d0\",
      \"add %1,#%2,%0\;addc %d1,#0,%d0\",
      \"sub %1,#%J2,%0\;subb %d1,#0,%d0\",
      \"add #%2,%0\;addc #%y2,%d0\",
      \"add %1,%2,%I0\;addc %d1,%d2,%D0\",
      \"add %1,%2,%0\;addc %d1,%P2,%D0\",
      \"add %1,%I2,%I0\;addc %d1,%D2,%D0\",
      0};

  if (pic30_errata_mask & psv_errata) {
    switch (which_alternative) {
      case 3: return \"add %2,%I1,%0\;mov %D1,[w15++]\;addc %d2,[--w15],%d0\";
              break;
      case 9: return \"add %1,%I2,%I0\;mov %D2,[w15++]\;addc %d1,[--w15],%D0\";
              break;
    }
  }
  return patterns[which_alternative];
}"
  [(set_attr
   "cc"
   "math,math,math,math,math,math,math,math,math,math"
  )])

(define_insn "*addsi3x"
  [(set (match_operand:SI 0 "pic30_mode2mres_operand"
               "=r,&r,&r,R,R,R,>,>,>")
        (plus:SI (match_operand:SI 1 "pic30_register_operand"
               "%r, r, r,r,r,r,r,r,r")
                 (match_operand:SI 2 "pic30_mode2mres_operand"
                "r, R, >,r,R,>,r,R,>")))]
  ""
  "*
{
   static char *patterns[] = {
      \"add %1,%2,%0\;addc %d1,%d2,%d0\",
      \"add %1,%I2,%0\;addc %d1,%D2,%d0\",
      \"add %1,%2,%0\;addc %d1,%d2,%d0\",
      \"add %1,%2,%I0\;addc %d1,%d2,%D0\",
      \"add %1,%I2,%I0\;addc %d1,%D2,%D0\",
      \"add %1,%2,%I0\;addc %d1,%d2,%D0\",
      \"add %1,%2,%0\;addc %d1,%d2,%d0\",
      \"add %1,%I2,%0\;addc %d1,%D2,%d0\",
      \"add %1,%2,%0\;addc %d1,%d2,%d0\",
      \"add %1,%2,%0\;addc %d1,%d2,%d0\",
      \"add %1,%I2,%0\;mov %D2,[w15++]\;addc %d1,[--w15],%d0\",
      \"add %1,%2,%0\;mov %d2,[w15++]\;addc %d1,[--w15],,%d0\",
      \"add %1,%2,%I0\;addc %d1,%d2,%D0\",
      \"add %1,%I2,%I0\;mov %D2,[--w15]\;addc %d1,[--w15],%D0\",
      \"add %1,%2,%I0\;mov %d2,[--w15]\;addc %d1,[--w15],%D0\",
      \"add %1,%2,%0\;addc %d1,%d2,%d0\",
      \"add %1,%I2,%0\;mov %D2,[w15++]\;addc %d1,[--w15],%d0\",
      \"add %1,%2,%0\;mov %d2,[w15++]\;addc %d1,[--w15],%d0\",
      0};

  if (pic30_errata_mask & psv_errata) which_alternative += 9;
  return patterns[which_alternative];
}"
  [(set_attr "cc" "math")])

;;;;;;;;;;;;;;;;;
;; double integer
;;;;;;;;;;;;;;;;;

(define_insn "*adddi3_imm"
  [(set (match_operand:DI 0 "pic30_register_operand"          "=r,r,r,r")
        (plus:DI (match_operand:DI 1 "pic30_register_operand" "%r,r,0,0")
                 (match_operand:DI 2 "pic30_JM_operand"  "P,N,J,M")))]
  ""
  "@
   add %1,#%2,%0\;addc %d1,#0,%d0\;addc %t1,#0,%t0\;addc %q1,#0,%q0
   sub %1,#%J2,%0\;subb %d1,#0,%d0\;subb %t1,#0,%t0\;subb %q1,#0,%q0
   add #%2,%0\;addc #%y2,%d0\;addc #%x2,%t0\;addc #%w2,%q0
   sub %0,#%J2\;subb %d0,#0\;subb %t0,#0\;subb %q0,#0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "adddi3"
  [(set (match_operand:DI 0 "pic30_register_operand"          "=r")
        (plus:DI (match_operand:DI 1 "pic30_register_operand" "%r")
                 (match_operand:DI 2 "pic30_register_operand"  "r")))]
  ""
  "add %2,%1,%0\;addc %d2,%d1,%d0\;addc %t2,%t1,%t0\;addc %q2,%q1,%q0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

;;;;;;;;
;; float
;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subtract instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; quarter integer
;;;;;;;;;;;;;;;;;;

(define_insn "*subqi3_imm"
  [(set (match_operand:QI 0 "pic30_register_operand"          "=r,r")
        (minus:QI (match_operand:QI 1 "pic30_register_operand" "0,0")
                  (match_operand:QI 2 "pic30_JM_operand" "J,M")))]
  ""
  "@
   sub.b %0,#%2
   add.b #%J2,%0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "subqi3"
  [(set (match_operand:QI 0 "pic30_mode2_operand"
  	"=r<>,r<>,R,R,  r<>,R,r<>,R")
        (minus:QI (match_operand:QI 1 "pic30_register_operand"
	 "r,  r,  r,r,  r,  r,r,  r")
                  (match_operand:QI 2 "pic30_mode1PN_operand"
	 "r,  R<>,r,R<>,N,  N,P,  P")))]
  ""
  "@
   sub.b %1,%2,%0
   sub.b %1,%2,%0
   sub.b %1,%2,%0
   sub.b %1,%2,%0
   add.b %1,#%J2,%0
   add.b %1,#%J2,%0
   sub.b %1,#%2,%0
   sub.b %1,#%2,%0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def,defuse,etc,use,def,etc,def,etc")
  ]
)

(define_insn "*subqi3_sfr0"
  [(set (match_operand:QI 0 "pic30_near_operand"          "=U")
        (minus:QI (match_dup 0)
                  (match_operand:QI 1 "pic30_wreg_operand" "a")))]
  ""
  "sub.b %0"
  [(set_attr "cc" "math")])

(define_insn "*subqi3_sfr1"
  [(set (match_operand:QI 0 "pic30_register_operand"             "=a,a,d")
        (minus:QI (match_operand:QI 1 "pic30_near_operand" " U,U,U")
                  (match_operand:QI 2 "pic30_register_operand"   " a,d,d")))
   (clobber (match_scratch:QI 3                            "=X,X,&r"))]
  ""
  "@
   sub.b %1,WREG
   mov.b %2,w0\;sub.b %1,WREG
   mov #%1,%3\;subr.b %2,[%3],%0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;
;; half integer
;;;;;;;;;;;;;;;

(define_insn "*subhi3_imm"
  [(set (match_operand:HI 0 "pic30_register_operand"          "=r,r")
        (minus:HI (match_dup 0)
                  (match_operand:HI 1 "pic30_JM_operand" "J,M")))]
  ""
  "@
   sub %0,#%1
   add #%J1,%0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "subhi3"
  [(set (match_operand:HI 0 "pic30_mode2_operand"
		"=r<>,R,r<>,R,  r<>,R,r<>,R,  r<>,R,r<>,R,r<>,R")
        (minus:HI (match_operand:HI 1 "pic30_mode1P_operand"
		 "r,  r,r,  r,  r,  r,R<>,R<>,r,  r,r,  r,P,  P")
                  (match_operand:HI 2 "pic30_mode1PN_operand"
		 "r,  r,R<>,R<>,r,  r,r,  r,  N,  N,P,  P,r,  r")))]
  ""
  "@
   sub %1,%2,%0
   sub %1,%2,%0
   sub %1,%2,%0
   sub %1,%2,%0
   subr %2,%1,%0
   subr %2,%1,%0
   subr %2,%1,%0
   subr %2,%1,%0
   add %1,#%J2,%0
   add %1,#%J2,%0
   sub %1,#%2,%0
   sub %1,#%2,%0
   subr %2,#%1,%0
   subr %2,#%1,%0"
  [(set_attr "cc" "math")
   (set_attr "type"
  	 "def,etc,defuse,use,def,etc,defuse,use,def,etc,def,etc,def,etc")
  ])

(define_insn "subhi3_sfr0"
  [(set (match_operand:HI 0 "pic30_near_operand"          "=U")
        (minus:HI (match_dup 0)
                  (match_operand:HI 1 "pic30_wreg_operand" "a")))]
  ""
  "sub %0"
  [(set_attr "cc" "math")])

(define_insn_and_split "*subhi3_sfr1"
  [(set (match_operand:HI 0 "pic30_register_operand"             "=a ,a, d")
        (minus:HI (match_operand:HI 1 "pic30_near_operand" " U ,U, U")
                  (match_operand:HI 2 "pic30_register_operand"   " a ,d, d")))
   (clobber (match_scratch:HI 3                            "=X ,X,&r"))]
  ""
  "@
   sub %1,WREG
   mov %2,w0\;sub %1,WREG
   mov #%1,%3\;subr %2,[%3],%0"
  "reload_completed"
  [
   (set (match_dup 3) (match_dup 1))
   (set (match_dup 0) (minus:HI (match_dup 3) (match_dup 2)))
  ]
{
  if (!pic30_wreg_operand(operands[0], HImode) &&
      !pic30_wreg_operand(operands[2], HImode))
  {
  }
  else
  {
  	FAIL;
  }
}
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;
;; single integer
;;;;;;;;;;;;;;;;;

(define_insn "*subsihi3"
  [(set (match_operand:SI 0 "pic30_register_operand"          "=r")
        (minus:SI (match_operand:SI 1 "pic30_register_operand" "r")
                  (zero_extend:SI (match_operand:HI 2 "pic30_register_operand" "r"))))
  ]
  ""
  "sub %1,%2,%0\;subb %d1,#0,%d0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "*subsi3_imm0"
  [(set (match_operand:SI 0 "pic30_register_operand"          "=r,r")
        (minus:SI (match_operand:SI 1 "pic30_register_operand" "r,r")
                  (match_operand:SI 2 "pic30_PN_operand" "N,P")))]
  ""
  "@
   add %1,#%J2,%0\;addc %d1,#0,%d0
   sub %1,#%2,%0\;subb %d1,#0,%d0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "*subsi3_imm1"
  [(set (match_operand:SI 0 "pic30_register_operand"          "=r")
        (minus:SI (match_operand:SI 1 "pic30_P_operand"  "P")
                  (match_operand:SI 2 "pic30_register_operand" "r")))]
  ""
  "subr %2,#%1,%0\;subbr %d2,#0,%d0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "*subsi3_imm2"
  [(set (match_operand:SI 0 "pic30_register_operand"          "=r,r")
        (minus:SI (match_operand:SI 1 "pic30_register_operand" "0,0")
                  (match_operand:SI 2 "pic30_JM_operand" "J,M")))]
  ""
  "@
   sub %0,#%2\;subb %d0,#0
   add #%J2,%0\;addc #0,%d0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "subsi3"
  [(set (match_operand:SI 0 "pic30_reg_or_R_operand"
               "=r,r,r,&r,R,R,R,&r,R,R")
        (minus:SI (match_operand:SI 1 "pic30_reg_or_R_operand"
                "r,0,r, r,r,r,r, R,0,R")
                  (match_operand:SI 2 "pic30_reg_or_R_operand"
                "r,r,0, R,r,0,R, r,r,r")))]
  ""
  "*
{
   static char *patterns[] = {
      \"sub %1,%2,%0\;subb %d1,%d2,%d0\",
      \"sub %1,%2,%0\;subb %d1,%d2,%d0\",
      \"sub %1,%2,%0\;subb %d1,%d2,%d0\",
      \"sub %1,%I2,%0\;subb %d1,%D2,%d0\",
      \"sub %1,%2,%I0\;subb %d1,%d2,%D0\",
      \"sub %1,%2,%I0\;subb %d1,%2,%D0\",
      \"sub %1,%I2,%I0\;subb %d1,%D2,%D0\",
      \"subr %2,%I1,%0\;subbr %d2,%D1,%d0\",
      \"subr %2,%1,%I0\;subbr %d2,%1,%D0\",
      \"subr %2,%I1,%I0\;subbr %d2,%D1,%D0\",
      0};

  if (pic30_errata_mask & psv_errata) switch (which_alternative) {
    case 3: return \"sub %1,%I2,%0\;mov %D2,[w15++]\;subb %d1,[--w15],%d0\";
            break;
    case 6: return \"sub %1,%I2,%I0\;mov %D2,[w15++]\;subb %d1,[--w15],%D0\";
            break;
    case 7: return \"subr %2,%I1,%0\;mov %D1,[w15++]\;subbr %d2,[--w15],%d0\";
            break;
    case 9: return \"subr %2,%I1,%I0\;mov %D1,[w15++]\;subbr %d2,[--w15],%D0\";
            break;
  }
  return patterns[which_alternative];
}"
  [(set_attr "cc" "math")])

;;;;;;;;;;;;;;;;;
;; double integer
;;;;;;;;;;;;;;;;;

(define_insn "*subdi3_imm"
  [(set (match_operand:DI 0 "pic30_register_operand"          "=r,r")
        (minus:DI (match_operand:DI 1 "pic30_register_operand" "0,0")
                  (match_operand:DI 2 "pic30_JM_operand" "J,M")))]
  ""
  "@
   sub %0,#%2\;subb %d0,#0\;subb %t0,#0\;subb %q0,#0
   add #%J2,%0\;addc #0,%d0\;addc #0,%t0\;addc #0,%q0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "subdi3"
  [(set (match_operand:DI 0 "pic30_register_operand"          "=r")
        (minus:DI (match_operand:DI 1 "pic30_register_operand" "r")
                  (match_operand:DI 2 "pic30_register_operand" "r")))]
  ""
  "sub %1,%2,%0\;subb %d1,%d2,%d0\;subb %t1,%t2,%t0\;subb %q1,%q2,%q0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiply instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 16-bit product

;; 8 x 8 => 16 (unsigned)

(define_expand "umulqihi3"
  [(set (match_operand:HI 0 "pic30_register_operand"
						"")
    (mult:HI (zero_extend:HI (match_operand:QI 1 "pic30_register_operand"
						""))
             (zero_extend:HI (match_operand:QI 2 "pic30_reg_imm_or_near_operand"
						 ""))))]
;; NULLSTONE CSE fails when enabled
  "0"
  "
{
	if (GET_CODE(operands[2]) == CONST_INT)
	{
		int pow2 = INTVAL(operands[2]);
		if (pic30_one_bit_set_p(pow2))
		{
			emit_insn(gen_umulqihi3pow2(operands[0],
						operands[1], operands[2]));
		}
		else
		{
			emit_insn(gen_umulqihi3imm(operands[0],
						operands[1], operands[2]));
		}
	}
	else
	{
		emit_insn(gen_umulqihi3gen(operands[0],
						operands[1], operands[2]));
	}
	DONE;
}")

(define_insn "umulqihi3gen"
  [(set (match_operand:HI 0 "pic30_creg_operand"
						"=c,c")
    (mult:HI (zero_extend:HI (match_operand:QI 1 "pic30_wreg_operand"
						"%a,a"))
             (zero_extend:HI (match_operand:QI 2 "pic30_reg_or_near_operand"
						 "r,U"))))]
  ""
  "@
   mul.b %m2
   mul.b %2"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def")
  ]
)

(define_insn "umulqihi3imm"
  [(set (match_operand:HI 0 "pic30_creg_operand"
						"=c")
    (mult:HI (zero_extend:HI (match_operand:QI 1 "pic30_wreg_operand"
						"%a"))
             (match_operand:HI 2 "immediate_operand" "i")))]
  ""
  "mov.b #%2,%0\;mul.b %m0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def")
  ]
)

(define_insn "umulqihi3pow2"
  [(set (match_operand:HI 0 "pic30_register_operand"                      "=r")
    (mult:HI (zero_extend:HI (match_operand:QI 1 "pic30_register_operand" "%r"))
             (match_operand:HI 2 "immediate_operand"                 "i")))]
  "pic30_one_bit_set_p(INTVAL(operands[2]))"
  "ze %1,%0\;sl %0,#%b2,%0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;; 16 x 16 => 16

;; this expansion is helpful in constant propagation
;; (but in general, it leads to code expansion, since
;; it causes all integer multiplication to be inline)

(define_expand "mulhi3"
  [(set (match_operand:HI 0 "pic30_wreg_operand" "")
        (mult:HI (match_operand:HI 1 "pic30_near_operand" "")
                 (match_operand:HI 2 "immediate_operand" "")))]
  ""
  "
{
	if (INTVAL(operands[2]) == 2)
	{
			emit_insn(gen_mulhi3imm(operands[0],
						operands[1],
						operands[2]));
	}
	else
	{
		FAIL;
	}
}")

(define_insn "mulhi3imm"
  [(set (match_operand:HI 0 "pic30_mode2_operand"
  	"=a,r<>,r<>,R,R")
        (mult: HI (match_operand:HI 1 "pic30_near_mode2_operand"
	 "U,r,  R<>,r,R<>")
                  (match_operand:HI 2 "immediate_operand"
	 "i,i,  i,  i,i")))]
  "(INTVAL(operands[2]) == 2)"
  "@
     sl %1,WREG
     sl %1,%0
     sl %1,%0
     sl %1,%0
     sl %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,def,defuse,etc,use")
  ]
)

; 32-bit product
; /* *_extend of an immediate_operand is illegal, apparantly 
;    so need two separate sequences */

(define_insn "*umulhisi3imm"
  [(set (match_operand:SI 0 "pic30_register_operand"                      "=r")
    (mult:SI (zero_extend:SI (match_operand:HI 1 "pic30_register_operand" "%r"))
             (match_operand:SI 2 "pic30_P_operand"                   "P")))]
  ""
  "mul.uu %1,#%2,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def")
  ]
)

(define_insn "umulhisi3"
  [(set (match_operand:SI 0 "pic30_register_operand"
							"=r,r")
    (mult:SI (zero_extend:SI (match_operand:HI 1 "pic30_register_operand"
							"%r,r"))
             (zero_extend:SI (match_operand:HI 2 "pic30_mode2_operand"
							 "r,R<>"))))]
  ""
  "mul.uu  %1,%2,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def,defuse")
  ]
)

(define_insn "*umulhisi3sfr"
  [(set (match_operand:SI 0 "pic30_creg_operand"
						"=C,C")
    (mult:SI (zero_extend:SI (match_operand:HI 1 "pic30_wreg_operand"
						"%a,a"))
             (zero_extend:SI (match_operand:HI 2 "pic30_reg_or_near_operand"
						 "r,U"))))]
  ""
  "@
   mul.w %m2
   mul.w %2"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def")
  ]
)

;; 16 x 16 => 32 (signed)

;; /* *_extend of an immediate_operand is illegal, apparantly -
;;   so need separate patterns */

(define_insn "mulhisi3_imm"
  [(set (match_operand:SI 0 "pic30_register_operand"                          "=r")
        (mult:SI (sign_extend:SI (match_operand:HI 1 "pic30_register_operand" "%r"))
                 (match_operand:SI 2 "pic30_P_operand"                  "P")))]
  ""
  "mul.su %1,#%2,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def")
  ]
)

(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "pic30_register_operand"                          "=r,r")
        (mult:SI (sign_extend:SI (match_operand:HI 1 "pic30_register_operand" "%r,r"))
                 (sign_extend:SI (match_operand:HI 2 "pic30_mode2_operand"
                                                                 "r,R<>"))))]
  ""
  "mul.ss %1,%2,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def,defuse")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builtin 16x16->32 instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "mulsu"
  [(set (match_operand:SI 0 "pic30_register_operand"                    "=r,r,  r")
        (unspec:SI [
	            (match_operand:HI 1 "pic30_register_operand"         "r,r,  r")
	            (match_operand:HI 2 "pic30_mode2_or_P_operand" "r,R<>,P")
	           ]
		    UNSPECV_MULSU))
  ]
  ""
  "@
   mul.su %1,%2,%0
   mul.su %1,%2,%0
   mul.su %1,#%2,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def,defuse,def")
  ]
)

(define_insn "muluu"
  [(set (match_operand:SI 0 "pic30_register_operand"                    "=r,r,  r")
        (unspec:SI [
	            (match_operand:HI 1 "pic30_register_operand"         "r,r,  r")
	            (match_operand:HI 2 "pic30_mode2_or_P_operand" "r,R<>,P")
	           ]
		    UNSPECV_MULUU))
  ]
  ""
  "@
   mul.uu %1,%2,%0
   mul.uu %1,%2,%0
   mul.uu %1,#%2,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def,defuse,def")
  ]
)

(define_insn "mulus"
  [(set (match_operand:SI 0 "pic30_register_operand"               "=r,r")
        (unspec:SI [
	            (match_operand:HI 1 "pic30_register_operand"    "r,r")
	            (match_operand:HI 2 "pic30_mode2_operand" "r,R<>")
	           ]
		    UNSPECV_MULUS))
  ]
  ""
  "mul.us %1,%2,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def,defuse")
  ]
)

(define_insn "mulss"
  [(set (match_operand:SI 0 "pic30_register_operand"               "=r,r")
        (unspec:SI [
	            (match_operand:HI 1 "pic30_register_operand"    "r,r")
	            (match_operand:HI 2 "pic30_mode2_operand" "r,R<>")
	           ]
		    UNSPECV_MULSS))
  ]
  ""
  "mul.ss %1,%2,%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def,defuse")
  ]
)

;; 16 x 32 => 32 (unsigned)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 32 x 32 => 32 (signed / unsigned) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "mulsi3"
  [(set (match_operand:SI 0 "pic30_register_operand" "")
        (mult:SI (match_operand:SI 1 "pic30_register_operand" "")
                 (match_operand:SI 2 "pic30_reg_or_imm_operand" "")))]
  "0"
  "
{
	if (GET_CODE(operands[2]) == CONST_INT)
	{
		int lit5 = INTVAL(operands[2]);
		if (CONST_OK_FOR_LETTER_P(lit5, 'P'))
		{
			emit_insn(gen_mulsi3imm(operands[0],
						operands[1],
						operands[2]));
		}
		else
		{
			FAIL;
		}
	}
	else
	{
		FAIL;
	}
}")

(define_insn "mulsi3imm"
  [(set (match_operand:SI 0 "pic30_register_operand"      "=r")
    (mult:SI (match_operand:SI 1 "pic30_register_operand" "%r")
             (match_operand 2 "immediate_operand"    "i")))
	     (clobber (match_scratch:SI 3           "=&r"))]
  "0"
  "*
{
	return	(
		\"mul.su %d1,#%2,%3\;\"
		\"mul.uu %1,#%2,%0\;\"
		\"add %3,%d0,%d0\"
		);
}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; divide instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builtin 32/16 instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "divsd"
  [(set (match_operand:HI 0 "pic30_register_operand"              "=a,?b,??e")
        (unspec_volatile:HI [
	            (match_operand:SI 1 "pic30_register_operand"   "r, r,  r")
	            (match_operand:HI 2 "pic30_ereg_operand" "e, e,  e")
	           ]
		    UNSPECV_DIVSD))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  {
     rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
     rtx w1 = gen_rtx_REG(HImode, WR1_REGNO);
     switch (which_alternative) {
       case 0:  /*
		** wm/wn -> w0
		*/
                if (pic30_dead_or_set_p(insn, w1)) {
                  return("repeat #18-1\;div.sd %1,%2");
                } else {
                    return("mov w1,[w15++]\;repeat #18-1\;"
                           "div.sd %1,%2\;mov [--w15],w1");
		}
       case 1:  /*
                ** wm/wn -> w1
                */
                if (pic30_dead_or_set_p(insn, w0)) {
                  return("repeat #18-1\;div.sd %1,%2\;mov w0,%0");
                } else {
                  return("mov w0,[w15++]\;repeat #18-1\;div.sd %1,%2\;"
                	 "mov w0,%0\;mov [--w15],w0");
                }
       default: /*
                ** wm/wn -> we
                */
                if (pic30_dead_or_set_p(insn, w0) &&
                    pic30_dead_or_set_p(insn, w1)) {
                  return("repeat #18-1\;div.sd %1,%2\;mov w0,%0");
                } else {
                  return("mov.d w0,[w15++]\;repeat #18-1\;"
                         "div.sd %1,%2\;mov w0,%0\;mov.d [--w15],w0");
                }
     }
  }
  [
    (set_attr "cc" "math")
    (set_attr "type" "def")
  ]
)

(define_insn "modsd"
  [(set (match_operand:HI 0 "pic30_register_operand"              "=b,?a,??e")
        (unspec_volatile:HI [
                    (match_operand:SI 1 "pic30_register_operand"   "r, r,  r")
                    (match_operand:HI 2 "pic30_ereg_operand" "e, e,  e")
                   ]
                    UNSPECV_MODSD))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  {
     rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
     rtx w1 = gen_rtx_REG(HImode, WR1_REGNO);
     switch (which_alternative) {
       case 1:  /*
                ** wm/wn -> w0
                */
                if (pic30_dead_or_set_p(insn, w1)) {
                  return("repeat #18-1\;div.sd %1,%2\;mov w1,%0");
                } else {
                    return("mov w1,[w15++]\;repeat #18-1\;"
                           "div.sd %1,%2\;mov w1,%0\;mov [--w15],w1");
                }
       case 0:  /*
                ** wm/wn -> w1
                */
                if (pic30_dead_or_set_p(insn, w0)) {
                  return("repeat #18-1\;div.sd %1,%2\;mov w1,%0");
                } else {
                  return("mov w0,[w15++]\;repeat #18-1\;div.sd %1,%2\;"
                         "mov [--w15],w0");
                }
       default: /*
                ** wm/wn -> we
                */
                if (pic30_dead_or_set_p(insn, w0) &&
                    pic30_dead_or_set_p(insn, w1)) {
                  return("repeat #18-1\;div.sd %1,%2\;mov w1,%0");
                } else {
                  return("mov.d w0,[w15++]\;repeat #18-1\;"
                         "div.sd %1,%2\;mov w1,%0\;mov.d [--w15],w0");
                }
     }
  }
  [
    (set_attr "cc" "math")
    (set_attr "type" "def")
  ]
)

(define_insn "divud"
  [(set (match_operand:HI 0 "pic30_register_operand"              "=a,?b,??e")
        (unspec_volatile:HI [
	            (match_operand:SI 1 "pic30_register_operand"   "r, r,  r")
	            (match_operand:HI 2 "pic30_ereg_operand" "e, e,  e")
	           ]
		    UNSPECV_DIVUD))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  {
     rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
     rtx w1 = gen_rtx_REG(HImode, WR1_REGNO);
     switch (which_alternative) {
       case 0:  /*
		** wm/wn -> w0
		*/
                if (pic30_dead_or_set_p(insn, w1)) {
                  return("repeat #18-1\;div.ud %1,%2");
                } else {
                    return("mov w1,[w15++]\;repeat #18-1\;"
                           "div.ud %1,%2\;mov [--w15],w1");
		}
       case 1:  /*
                ** wm/wn -> w1
                */
                if (pic30_dead_or_set_p(insn, w0)) {
                  return("repeat #18-1\;div.ud %1,%2\;mov w0,%0");
                } else {
                  return("mov w0,[w15++]\;repeat #18-1\;div.ud %1,%2\;"
                	 "mov w0,%0\;mov [--w15],w0");
                }
       default: /*
                ** wm/wn -> we
                */
                if (pic30_dead_or_set_p(insn, w0) &&
                    pic30_dead_or_set_p(insn, w1)) {
                  return("repeat #18-1\;div.ud %1,%2\;mov w0,%0");
                } else {
                  return("mov.d w0,[w15++]\;repeat #18-1\;"
                         "div.ud %1,%2\;mov w0,%0\;mov.d [--w15],w0");
                }
     }
  }
  [
    (set_attr "cc" "math")
    (set_attr "type" "def")
  ]
)

(define_insn "modud"
  [(set (match_operand:HI 0 "pic30_register_operand"              "=b,?a,??e")
        (unspec_volatile:HI [
                    (match_operand:SI 1 "pic30_register_operand"   "r, r,  r")
                    (match_operand:HI 2 "pic30_ereg_operand" "e, e,  e")
                   ]
                    UNSPECV_MODUD))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  {
     rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
     rtx w1 = gen_rtx_REG(HImode, WR1_REGNO);
     switch (which_alternative) {
       case 1:  /*
                ** wm/wn -> w0
                */
                if (pic30_dead_or_set_p(insn, w1)) {
                  return("repeat #18-1\;div.ud %1,%2\;mov w1,%0");
                } else {
                    return("mov w1,[w15++]\;repeat #18-1\;"
                           "div.ud %1,%2\;mov w1,%0\;mov [--w15],w1");
                }
       case 0:  /*
                ** wm/wn -> w1
                */
                if (pic30_dead_or_set_p(insn, w0)) {
                  return("repeat #18-1\;div.ud %1,%2\;mov w1,%0");
                } else {
                  return("mov w0,[w15++]\;repeat #18-1\;div.ud %1,%2\;"
                         "mov [--w15],w0");
                }
       default: /*
                ** wm/wn -> we
                */
                if (pic30_dead_or_set_p(insn, w0) &&
                    pic30_dead_or_set_p(insn, w1)) {
                  return("repeat #18-1\;div.ud %1,%2\;mov w1,%0");
                } else {
                  return("mov.d w0,[w15++]\;repeat #18-1\;"
                         "div.ud %1,%2\;mov w1,%0\;mov.d [--w15],w0");
                }
     }
  }
  [
    (set_attr "cc" "math")
    (set_attr "type" "def")
  ]
)

(define_insn "divmodsd"
  [(set (match_operand:HI 0 "pic30_register_operand"              "=a,?b,??e")
        (unspec_volatile:HI [
                    (match_operand:SI 1 "pic30_register_operand"   "r, r,  r")
                    (match_operand:HI 2 "pic30_ereg_operand" "e, e,  e")
                    (match_operand:HI 3 "pic30_register_operand" "e,e,e")
                   ]
                    UNSPECV_DIVMODSD))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  {
     rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
     rtx w1 = gen_rtx_REG(HImode, WR1_REGNO);
     switch (which_alternative) { 
       case 0:  /*
                ** wm/wn -> w0
                */ 
                if (pic30_dead_or_set_p(insn, w1)) {
                  return("repeat #18-1\;div.sd %1,%2\;mov w1,[%3]");
                } else {
                  return("mov w1,[w15++]\;repeat #18-1\;"
                         "div.sd %1,%2\;mov w1,[%3]\;mov [--w15],w1");
                }
       case 1:  /*
                ** wm/wn -> w1
                */ 
                if (pic30_dead_or_set_p(insn, w0)) {
                  return("repeat #18-1\;div.sd %1,%2\;mov w0,%0\;mov w1,[%3]");
                } else {
                  return("mov w0,[w15++]\;repeat #18-1\;div.sd %1,%2\;"
                         "mov w1,[%3]\;mov w0,%0\;mov [--w15],w0");
                }
       default: /*
                ** wm/wn -> we
                */ 
                if (pic30_dead_or_set_p(insn, w0) &&
                    pic30_dead_or_set_p(insn, w1)) {
                  return("repeat #18-1\;div.sd %1,%2\;mov w0,%0\;mov w1,[%3]");
                } else {
                  return("mov.d w0,[w15++]\;repeat #18-1\;"
                         "div.sd %1,%2\;mov w0,%0\;mov w1,[%3]\;"
                         "mov.d [--w15],w0");
                }
     }
  }
  [
    (set_attr "cc" "math")
    (set_attr "type" "def")
  ]
)

(define_insn "divmodud"
  [(set (match_operand:HI 0 "pic30_register_operand"              "=a,?b,??e")
        (unspec_volatile:HI [
                    (match_operand:SI 1 "pic30_register_operand"   "r, r,  r")
                    (match_operand:HI 2 "pic30_ereg_operand" "e, e,  e")
                    (match_operand:HI 3 "pic30_register_operand" "e,e,e")
                   ]
                    UNSPECV_DIVMODUD))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  {
     rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
     rtx w1 = gen_rtx_REG(HImode, WR1_REGNO);
     switch (which_alternative) { 
       case 0:  /*
                ** wm/wn -> w0
                */ 
                if (pic30_dead_or_set_p(insn, w1)) {
                  return("repeat #18-1\;div.ud %1,%2\;mov w1,[%3]");
                } else {
                    return("mov w1,[w15++]\;repeat #18-1\;"
                           "div.ud %1,%2\;mov w1,[%3]\;mov [--w15],w1");
                }
       case 1:  /*
                ** wm/wn -> w1
                */ 
                if (pic30_dead_or_set_p(insn, w0)) {
                  return("repeat #18-1\;div.ud %1,%2\;mov w1,[%3]\;mov w0,%0");
                } else {
                  return("mov w0,[w15++]\;repeat #18-1\;div.ud %1,%2\;"
                         "mov w1,[%3]\;mov w0,%0\;mov [--w15],w0");
                }
       default: /*
                ** wm/wn -> we
                */ 
                if (pic30_dead_or_set_p(insn, w0) &&
                    pic30_dead_or_set_p(insn, w1)) {
                  return("repeat #18-1\;div.ud %1,%2\;mov w0,%0\;mov w1,[%3]");
                } else {
                  return("mov.d w0,[w15++]\;repeat #18-1\;"
                         "div.ud %1,%2\;mov w0,%0\;mov w1,[%3]\;"
                         "mov.d [--w15],w0");
                }
     }
  }
  [
    (set_attr "cc" "math")
    (set_attr "type" "def")
  ]
)


;; Divide/Remainder instructions.
(define_insn "divmodhi4"
  [(set (match_operand:HI 0 "pic30_register_operand"             "=a,b,r")
      (div:HI (match_operand:HI 1 "pic30_register_operand"       "r,r,r")
              (match_operand:HI 2 "pic30_ereg_operand"     "e,e,e")))
   (set (match_operand:HI 3 "pic30_register_operand"             "=b,a,r")
      (mod:HI (match_dup 1) (match_dup 2)))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  "*
{
   int rquo = REGNO(operands[0]);
   int rnum = REGNO(operands[1]);
   int rden = REGNO(operands[2]);
   int rrem = REGNO(operands[3]);
   static char instr_sequence[120];
   char *f = instr_sequence;
   rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
   rtx w1 = gen_rtx_REG(HImode, WR1_REGNO);
   struct fred {
     unsigned int q:1;
     unsigned int r:1;
     unsigned int p_w0:1;
     unsigned int p_w1:1;
     int e_w0;
     int e_w1;
   } status = { 0 };

   switch (which_alternative) {
   case 2:
      /*
      ** wm/wn -> wq, wr
      */ 
      status.e_w0 = -1;
      status.e_w1 = -1;
      status.p_w0 = 1;
      status.p_w1 = 1;
      if (dead_or_set_p(insn,w0) ||
          pic30_dead_or_set_p(NEXT_INSN(insn), w0)) {
        /* preserve w1 */
        status.p_w0 = 0;
      } 
      if (dead_or_set_p(insn,w1) ||
          pic30_dead_or_set_p(NEXT_INSN(insn), w1)) {
        status.p_w1 = 0;
      } 
      if (find_reg_note(insn, REG_UNUSED, operands[3]))
      {
         status.q = 1;
      }
      else if (find_reg_note(insn, REG_UNUSED, operands[0]))
      {
         status.r = 1;
      }
      else
      {
         status.q = 1;
         status.r = 1;
      }
      if (status.p_w0 || status.p_w1) {
         /* is there an exchange register ? */
        if ((status.p_w0) && !(pic30_errata_mask & exch_errata)) {
          if ((rquo!=WR0_REGNO) && (rquo!=WR1_REGNO) && /* clobber by div */
              (rquo!=rnum) && (rquo!=rden) && status.q)          /* input */
             status.e_w0 = rquo;
        }
        if ((status.p_w1) && !(pic30_errata_mask & exch_errata)) {
          if ((rrem!=WR0_REGNO) && (rrem!=WR1_REGNO) && /* clobber by div */
              (rrem!=rnum) && (rrem!=rden) && status.r)          /* input */
             status.e_w1 = rrem;
        }
        if ((status.e_w0 == -1) && (status.e_w1 == -1) && 
            (status.p_w0) && (status.p_w1)) {
          f += sprintf(f, \"mov.d w0,[w15++]\;\");
        } else {
          if (status.p_w0) {
            if (status.e_w0 > 0) 
               f += sprintf(f, \"mov w0,%s\;\", reg_names[status.e_w0]);
            else
               f += sprintf(f, \"mov w0,[w15++]\;\");
          }
          if (status.p_w1) {
            if (status.e_w1 > 0) 
               f += sprintf(f, \"mov w1,%s\;\", reg_names[status.e_w1]);
            else
               f += sprintf(f, \"mov w1,[w15++]\;\");
          }
        }
      }
      f += sprintf(f, \"repeat #18-1\;div.sw %%1,%%2\");
      while (status.q || status.r) {
        if (status.q) {
          if (rquo != WR0_REGNO) {
            if (status.e_w0 > 0) {
              f += sprintf(f, \"\;exch w0,%s\", reg_names[status.e_w0]);
              status.q = 0;
            } else if ((rquo != WR1_REGNO) || (!status.r)) {
              f += sprintf(f, \"\;mov w0,%%0\");
              status.q = 0;
            }
          } else status.q = 0;
        }
        if (status.r) {
          if (rrem != WR1_REGNO) {
            if (status.e_w1 > 0) {
              f += sprintf(f, \"\;exch w1,%s\", reg_names[status.e_w1]);
              status.r = 0;
            } else if ((rrem != WR0_REGNO) || (!status.q)) {
              f += sprintf(f, \"\;mov w1,%%3\");
              status.r = 0;
            }
          } else status.r = 0;
        }
        if ((status.r) && (status.q)) {
          if ((rquo == WR1_REGNO) && (rrem == WR0_REGNO)) {
            if (pic30_errata_mask & exch_errata)
              f += sprintf(f, \"\;push w0\;mov w1,w0\;pop w1\");
            else 
              f += sprintf(f, \"\;exch w0,w1\");
            status.r = 0;
            status.q = 0;
          } else {
            internal_error(\"deadlock\");
          }
        }
      }
      if (status.p_w0 && status.p_w1 && (status.e_w0 == -1) && 
          (status.e_w1 == -1)) {
        f += sprintf(f, \"\;mov.d [--w15],w0\");
      } else {
        if (status.p_w1 && (status.e_w1 == -1)) {
          f += sprintf(f, \"\;mov [--w15],w1\");
        }
        if (status.p_w0 && (status.e_w0 == -1)) {
          f += sprintf(f, \"\;mov [--w15],w0\");
        }
      }
      return instr_sequence;
      break;
    case 0:
      return   (
       \"repeat #18-1\;\"
       \"div.sw %1,%2\"
      );
      break;
    case 1:
      if (pic30_errata_mask & exch_errata)
       return \"repeat #18-1\;\"
              \"div.sw %1,%2\;\"
              \"push w0\;\"
              \"mov w1,w0\;\"
              \"pop w1\;\";
      else
       return   (
         \"repeat #18-1\;\"
         \"div.sw %1,%2\;\"
         \"exch w0,w1\"
        );
      break;
   }
}"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "divmodhi4_w0"
  [(set (match_operand:HI 0 "pic30_wreg_operand"           "=a")
      (div:HI (match_operand:HI 1 "pic30_register_operand"       "r")
              (match_operand:HI 2 "pic30_ereg_operand"     "e")))
   (set (match_operand:HI 3 "pic30_breg_operand"           "=b")
      (mod:HI (match_dup 1) (match_dup 2)))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  "*
{
  return   (
         \"repeat #18-1\;\"
         \"div.sw %1,%2\"
         );
}"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "divmodhi4_w1"
  [(set (match_operand:HI 0 "pic30_breg_operand"           "=b")
      (div:HI (match_operand:HI 1 "pic30_register_operand"       "r")
              (match_operand:HI 2 "pic30_ereg_operand"     "e")))
   (set (match_operand:HI 3 "pic30_wreg_operand"           "=a")
      (mod:HI (match_dup 1) (match_dup 2)))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  "*
{
  if (pic30_errata_mask & exch_errata)
    return \"repeat #18-1\;\"
           \"div.sw %1,%2\;\"
           \"push w0\;\"
           \"mov w1,w0\;\"
           \"pop w1\;\";
  else
    return   (
         \"repeat #18-1\;\"
         \"div.sw %1,%2\"
         \"exch w0,w1\"
         );
}"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "udivmodhi4"
  [(set (match_operand:HI 0 "pic30_register_operand"             "=a,b,r")
      (udiv:HI (match_operand:HI 1 "pic30_register_operand"       "r,r,r")
              (match_operand:HI 2 "pic30_ereg_operand"     "e,e,e")))
   (set (match_operand:HI 3 "pic30_register_operand"             "=b,a,r")
      (umod:HI (match_dup 1) (match_dup 2)))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  "*
{
   int rquo = REGNO(operands[0]);
   int rnum = REGNO(operands[1]);
   int rden = REGNO(operands[2]);
   int rrem = REGNO(operands[3]);
   static char instr_sequence[120];
   char *f = instr_sequence;
   rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
   rtx w1 = gen_rtx_REG(HImode, WR1_REGNO);
   struct fred {
     unsigned int q:1;
     unsigned int r:1;
     unsigned int p_w0:1;
     unsigned int p_w1:1;
     int e_w0;
     int e_w1;
   } status = { 0 };

   switch (which_alternative) {
   case 2:
      /*
      ** wm/wn -> wq, wr
      */ 
      status.e_w0 = -1;
      status.e_w1 = -1;
      status.p_w0 = 1;
      status.p_w1 = 1;
      if (dead_or_set_p(insn,w0) ||
          pic30_dead_or_set_p(NEXT_INSN(insn), w0)) {
        /* preserve w1 */
        status.p_w0 = 0;
      } 
      if (dead_or_set_p(insn,w1) ||
          pic30_dead_or_set_p(NEXT_INSN(insn), w1)) {
        status.p_w1 = 0;
      } 
      if (find_reg_note(insn, REG_UNUSED, operands[3]))
      {
         status.q = 1;
      }
      else if (find_reg_note(insn, REG_UNUSED, operands[0]))
      {
         status.r = 1;
      }
      else
      {
         status.q = 1;
         status.r = 1;
      }
      if (status.p_w0 || status.p_w1) {
         /* is there an exchange register ? */
        if ((status.p_w0) && !(pic30_errata_mask & exch_errata)) {
          if ((rquo!=WR0_REGNO) && (rquo!=WR1_REGNO) && /* clobber by div */
              (rquo!=rnum) && (rquo!=rden) && status.q)          /* input */
             status.e_w0 = rquo;
        }
        if ((status.p_w1) && !(pic30_errata_mask & exch_errata)) {
          if ((rrem!=WR0_REGNO) && (rrem!=WR1_REGNO) && /* clobber by div */
              (rrem!=rnum) && (rrem!=rden) && status.r)          /* input */
             status.e_w1 = rrem;
        }
        if ((status.e_w0 == -1) && (status.e_w1 == -1) && 
            (status.p_w0) && (status.p_w1)) {
          f += sprintf(f, \"mov.d w0,[w15++]\;\");
        } else {
          if (status.p_w0) {
            if (status.e_w0 > 0) 
               f += sprintf(f, \"mov w0,%s\;\", reg_names[status.e_w0]);
            else
               f += sprintf(f, \"mov w0,[w15++]\;\");
          }
          if (status.p_w1) {
            if (status.e_w1 > 0) 
               f += sprintf(f, \"mov w1,%s\;\", reg_names[status.e_w1]);
            else
               f += sprintf(f, \"mov w1,[w15++]\;\");
          }
        }
      }
      f += sprintf(f, \"repeat #18-1\;div.uw %%1,%%2\");
      while (status.q || status.r) {
        if (status.q) {
          if (rquo != WR0_REGNO) {
            if (status.e_w0 > 0) {
              f += sprintf(f, \"\;exch w0,%s\", reg_names[status.e_w0]);
              status.q = 0;
            } else if ((rquo != WR1_REGNO) || (!status.r)) {
              f += sprintf(f, \"\;mov w0,%%0\");
              status.q = 0;
            }
          } else status.q = 0;
        }
        if (status.r) {
          if (rrem != WR1_REGNO) {
            if (status.e_w1 > 0) {
              f += sprintf(f, \"\;exch w1,%s\", reg_names[status.e_w1]);
              status.r = 0;
            } else if ((rrem != WR0_REGNO) || (!status.q)) {
              f += sprintf(f, \"\;mov w1,%%3\");
              status.r = 0;
            }
          } else status.r = 0;
        }
        if ((status.r) && (status.q)) {
          if ((rquo == WR1_REGNO) && (rrem == WR0_REGNO)) {
            if (pic30_errata_mask & exch_errata) 
              f += sprintf(f, \"\;push w0\;mov w1,w0\;pop w1\");
            else
              f += sprintf(f, \"\;exch w0,w1\");
            status.r = 0;
            status.q = 0;
          } else {
            internal_error(\"deadlock\");
          }
        }
      }
      if (status.p_w0 && status.p_w1 && (status.e_w0 == -1) && 
          (status.e_w1 == -1)) {
        f += sprintf(f, \"\;mov.d [--w15],w0\");
      } else {
        if (status.p_w1 && (status.e_w1 == -1)) {
          f += sprintf(f, \"\;mov [--w15],w1\");
        }
        if (status.p_w0 && (status.e_w0 == -1)) {
          f += sprintf(f, \"\;mov [--w15],w0\");
        }
      }
      return instr_sequence;
      break;
    case 0:
      return   (
       \"repeat #18-1\;\"
       \"div.uw %1,%2\"
      );
      break;
    case 1:
      if (pic30_errata_mask & exch_errata)
        return \"repeat #18-1\;\"
               \"div.uw %1,%2\;\"
               \"push w0\;\"
               \"mov w1,w0\;\"
               \"pop w1\;\";
      else
        return   (
         \"repeat #18-1\;\"
         \"div.uw %1,%2\;\"
         \"exch w0,w1\"
        );
      break;
   }
}"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "udivmodhi4_w0"
  [(set (match_operand:HI 0 "pic30_wreg_operand"           "=a")
      (div:HI (match_operand:HI 1 "pic30_register_operand"       "r")
              (match_operand:HI 2 "pic30_ereg_operand"     "e")))
   (set (match_operand:HI 3 "pic30_breg_operand"           "=b")
      (mod:HI (match_dup 1) (match_dup 2)))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  "*
{
  return   (
         \"repeat #18-1\;\"
         \"div.uw %1,%2\"
         );
}"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "udivmodhi4_w1"
  [(set (match_operand:HI 0 "pic30_breg_operand"           "=b")
      (div:HI (match_operand:HI 1 "pic30_register_operand"       "r")
              (match_operand:HI 2 "pic30_ereg_operand"     "e")))
   (set (match_operand:HI 3 "pic30_wreg_operand"           "=a")
      (mod:HI (match_dup 1) (match_dup 2)))
   (clobber (reg:HI RCOUNT))
  ]
  ""
  "*
{
  if (pic30_errata_mask & exch_errata)
    return \"repeat #18-1\;\"
           \"div.uw %1,%2\;\"
           \"push w0\;\"
           \"mov w1,w0\;\"
           \"pop w1\;\";
  else
    return   (
         \"repeat #18-1\;\"
         \"div.uw %1,%2\"
         \"exch w0,w1\"
         );
}"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; square root instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other arithmetic instructions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Absolute value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; Half integer
;;;;;;;;;;;;;;;;;;

;; (define_insn "abshi2"
;;   [(set (match_operand:HI 0 "pic30_register_operand"        "=r")
;;         (abs:HI (match_operand:HI 1 "pic30_register_operand" "r")))]
;;   ""
;;   "btsc %1,#15\;neg %1,%0"
;;   [(set_attr "cc" "clobber")])

;;;;;;;;;;;;;;;;;;
;; Single float
;;;;;;;;;;;;;;;;;;

(define_insn "abssf2"
  [(set (match_operand:SF 0 "pic30_register_operand"        "=r")
        (abs:SF (match_operand:SF 1 "pic30_register_operand" "0")))]
  ""
  "bclr %d0,#15"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;
;; Double float
;;;;;;;;;;;;;;;;;;

(define_insn "absdf2"
  [(set (match_operand:DF 0 "pic30_register_operand"        "=r")
        (abs:DF (match_operand:DF 1 "pic30_register_operand" "0")))]
  ""
  "bclr %q0,#15"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Negation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; Quarter integer 
;;;;;;;;;;;;;;;;;;

(define_insn "negqi2"
  [(set (match_operand:QI 0 "pic30_mode2_operand"        "=r<>,r<>,R,R")
        (neg:QI (match_operand:QI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")))]
  ""
  "neg.b %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*negqi2_sfr0"
  [(set (match_operand:QI 0 "pic30_wreg_operand"        "=a")
        (neg:QI (match_operand:QI 1 "pic30_near_operand" "U")))]
  ""
  "neg.b %1,WREG"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "*negqi2_sfr1"
  [(set (match_operand:QI 0 "pic30_near_operand"        "=U")
        (neg:QI (match_dup 0)))]
  ""
  "neg.b %0"
  [(set_attr "cc" "set")])

;;;;;;;;;;;;;;;;;;
;; Half integer 
;;;;;;;;;;;;;;;;;;

(define_insn "neghi2"
  [(set (match_operand:HI 0 "pic30_mode2_operand"        "=r<>,r<>,R,R")
        (neg:HI (match_operand:HI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")))]
  ""
  "neg %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*neghi2_sfr0"
  [(set (match_operand:HI 0 "pic30_wreg_operand"        "=a")
        (neg:HI (match_operand:HI 1 "pic30_near_operand" "U")))]
  ""
  "neg %1,WREG"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "*neghi2_sfr1"
  [(set (match_operand:HI 0 "pic30_near_operand"        "=U")
        (neg:HI (match_dup 0)))]
  ""
  "neg %0"
  [
   (set_attr "cc" "set")
  ]
)

;;;;;;;;;;;;;;;;;;
;; Single integer 
;;;;;;;;;;;;;;;;;;

(define_insn "negsi2"
  [(set (match_operand:SI 0 "pic30_register_operand"        "=r")
        (neg:SI (match_operand:SI 1 "pic30_register_operand" "r")))]
  ""
  "subr %1,#0,%0\;subbr %d1,#0,%d0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;
;; Single float
;;;;;;;;;;;;;;;;;;

(define_insn "negsf2"
  [(set (match_operand:SF 0        "pic30_register_operand" "=r")
        (neg:SF (match_operand:SF 1 "pic30_register_operand" "0")))]
  ""
  "btg %d0,#15"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "*negsf2sfr"
  [(set (match_operand:SF 0        "pic30_near_operand" "=U")
        (neg:SF (match_dup 0)))]
  ""
  "btg.b %0+3,#7"
  [(set_attr "cc" "clobber")])

;;;;;;;;;;;;;;;;;;
;; Double float
;;;;;;;;;;;;;;;;;;

(define_insn "negdf2"
  [(set (match_operand:DF 0        "pic30_register_operand" "=r")
        (neg:DF (match_operand:DF 1 "pic30_register_operand" "0")))]
  ""
  "btg %q0,#15"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bit-logical instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; Quarter integer 
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;
;; Set Bit ;;
;;;;;;;;;;;;;

(define_insn "bitsetqi"
  [(set (match_operand:QI 0 "pic30_mode2_operand"        "=r,R,<>")
        (ior:QI (match_operand:QI 1 "pic30_mode2_operand" "0,0,0")
                (match_operand 2 "const_int_operand"      "i,i,i")))]
  "(pic30_one_bit_set_p(INTVAL(operands[2])&0x00ff))"
  "bset.b %0,#%b2"
  [
   (set_attr "type" "def,use,defuse")
  ]
)

(define_insn "bitsetqi_sfr"
  [(set (match_operand:QI 0 "pic30_near_operand"        "=U")
        (ior:QI  (match_dup 0)
                 (match_operand 1 "const_int_operand" "i")))]
  "pic30_one_bit_set_p(INTVAL(operands[1]) & 0x00ff)"
  "bset.b %0,#%b1")

;;;;;;;;;;;;;;;
;; Reset Bit ;;
;;;;;;;;;;;;;;;

(define_insn "bitclrqi"
  [(set (match_operand:QI 0 "pic30_mode2_operand"        "=r,R,<>")
        (and:QI (match_operand:QI 1 "pic30_mode2_operand" "0,0,0")
                (match_operand 2 "const_int_operand"      "i,i,i")))]
  "(pic30_one_bit_set_p((~INTVAL(operands[2])&0xff)))"
  "bclr.b %0,#%B2"
  [
   (set_attr "type" "def,use,defuse")
  ]
)

(define_insn "bitclrqi_sfr"
  [(set (match_operand:QI 0 "pic30_near_operand"        "=U")
        (and:QI  (match_dup 0)
                 (match_operand 1 "const_int_operand" "i")))]
  "(pic30_one_bit_set_p((~INTVAL (operands[1])) & 0x00ff))"
  "bclr.b %0,#%B1")


;;;;;;;;;;;;;;;;
;; Toggle Bit ;;
;;;;;;;;;;;;;;;;

(define_insn "bittogqi"
  [(set (match_operand:QI 0 "pic30_mode2_operand"        "=r,R,<>")
        (xor:QI (match_operand:QI 1 "pic30_mode2_operand" "0,0,0")
                (match_operand 2 "const_int_operand"      "i,i,i")))]
  "(pic30_one_bit_set_p(INTVAL(operands[2])&0xffff))"
  "btg.b %0,#%b2"
  [
   (set_attr "type" "def,use,defuse")
  ]
)

(define_insn "bittogqi_sfr"
  [(set (match_operand:QI 0 "pic30_near_operand"        "=U")
        (xor:QI  (match_dup 0)
                 (match_operand 1 "const_int_operand" "i")))]
  "pic30_one_bit_set_p(INTVAL(operands[1]) & 0x00ff)"
  "btg.b %0,#%b1")

;;;;;;;;;;;;;;
;; Test Bit ;;
;;;;;;;;;;;;;;

(define_insn "*bittstqi_and"
  [(set (cc0)
        (and (match_operand:QI 0 "pic30_reg_or_near_operand" "r,U")
             (match_operand 1 "immediate_operand" "i,i")))]
  "(pic30_one_bit_set_p(INTVAL(operands[1])))"
  "@
   btst %0,#%b1
   btst.b %0,#%b1"
  [(set_attr "cc" "set")])
 
          
(define_insn "*bittstqi"
  [(set (cc0)
        (zero_extract (match_operand:QI 0 "pic30_reg_or_near_operand" "r,U")
		      (const_int 1)
		      (match_operand 1 "const_int_operand" "i,i")))]
  ""
  "@
   btst %0,#%1
   btst.b %0,#%1"
  [(set_attr "cc" "set")])

;;;;;;;;;;;;;;;;;;
;; Half integer ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;
;; Set Bit ;;
;;;;;;;;;;;;;

;; the pre/post modify modes have been removed due to errata in Rev A silicon
;;   the pre/post modify modes are valid for REV_A3

(define_insn "bitsethi"
  [(set (match_operand:HI 0 "pic30_mode2_operand"         "=r,R,<>")
        (ior:HI  (match_operand:HI 1 "pic30_mode2_operand" "0,0,0")
                 (match_operand 2 "const_int_operand"      "i,i,i")))]
  "(pic30_one_bit_set_p(INTVAL(operands[2])))"
  "bset %0,#%b2"
  [
   (set_attr "type" "def,use,defuse")
  ]
)

(define_insn "bitsethi_sfr"
  [(set (match_operand:HI 0 "pic30_near_operand"         "=U")
        (ior:HI  (match_dup 0)
                 (match_operand 1 "const_int_operand"  "i")))]
  "pic30_one_bit_set_p(INTVAL(operands[1]))"
  "*
{
	if (INTVAL(operands[1]) >= 256)
		return(\"bset.b %0+1,#%b1-8\");
	else
		return(\"bset.b %0,#%b1\");
}")

;;;;;;;;;;;;;;;
;; Reset Bit ;;
;;;;;;;;;;;;;;;

;; the pre/post modify modes have been removed due to errata in Rev A silicon
;;   the pre/post modify modes are valid for REV_A3

(define_insn "bitclrhi"
  [(set (match_operand:HI 0 "pic30_mode2_operand"         "=r,R,<>")
        (and:HI  (match_operand:HI 1 "pic30_mode2_operand" "0,0,0")
                 (match_operand 2 "const_int_operand"      "i,i,i")))]
  "(pic30_one_bit_set_p((~INTVAL(operands[2])) & 0xffff))"
  "bclr %0,#%B2"
  [
   (set_attr "type" "def,use,defuse")
  ]
)

;        (and:HI  (match_operand:HI 1 "pic30_near_operand"  "0")
(define_insn "bitclrhi_sfr"
  [(set (match_operand:HI 0 "pic30_near_operand"           "=U")
        (and:HI  (match_dup 0)
                 (match_operand 1 "const_int_operand" "i")))]
  "(pic30_one_bit_set_p((~INTVAL(operands[1])) & 0xffff))"
  "*
{
	if (pic30_which_bit(~INTVAL(operands[1])) > 7)
		return(\"bclr.b %0+1,#%B1-8\");
	else
		return(\"bclr.b %0,#%B1\");
}")

;;;;;;;;;;;;;;;;
;; Toggle Bit ;;
;;;;;;;;;;;;;;;;

;; the pre/post modify modes have been removed due to errata in Rev A silicon
;;   the pre/post modify modes are valid for REV_A3

(define_insn "bittoghi"
  [(set (match_operand:HI 0 "pic30_mode2_operand"         "=r,R,<>")
        (xor:HI  (match_operand:HI 1 "pic30_mode2_operand" "0,0,0")
                 (match_operand 2 "const_int_operand"      "i,i,i")))]
  "(pic30_one_bit_set_p(INTVAL(operands[2])))"
  "btg %0,#%b2"
  [
   (set_attr "type" "def,use,defuse")
  ]
)

(define_insn "bittoghi_sfr"
  [(set (match_operand:HI 0 "pic30_near_operand"           "=U")
        (xor:HI  (match_dup 0)
                 (match_operand 1 "const_int_operand" "i")))]
  "pic30_one_bit_set_p(INTVAL(operands[1]))"
  "*
{
	if (INTVAL(operands[1]) >= 256)
		return(\"btg.b %0+1,#%b1-8\");
	else
		return(\"btg.b %0,#%b1\");
}")

;;;;;;;;;;;;;;
;; Test Bit ;;
;;;;;;;;;;;;;;

(define_insn "*bittsthi_and"
  [(set (cc0)
        (and (match_operand:HI 0 "pic30_near_mode2_operand" "r,R,<>,U")
             (match_operand 1 "immediate_operand"           "i,i,i, i")))]
  "(pic30_one_bit_set_p(INTVAL(operands[1])))"
  "@
   btst %0,#%b1
   btst %0,#%b1
   btst %0,#%b1
   btst.b %0+%b1/8,#%b1%%8"
  [
   (set_attr "cc" "set")
   (set_attr "type" "etc,use,defuse,etc")
  ]
)

(define_insn "*bittsthi"
  [(set (cc0) (zero_extract:HI
                   (match_operand:HI 0 "pic30_near_mode2_operand" "r,R,<>,U")
		   (const_int 1)
		   (match_operand 1 "const_int_operand"           "i,i,i, i")))]
  ""
  "@
   btst %0,#%1
   btst %0,#%1
   btst %0,#%1
   btst.b %0+%1/8,#%1%%8"
  [
   (set_attr "cc" "set")
   (set_attr "type" "etc,use,defuse,etc")
  ]
)

;;;;;;;;;;;;;;;;;;;;
;; Single integer ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;
;; Set Bit ;;
;;;;;;;;;;;;;

(define_insn "*bitsetsiR"
  [(set (match_operand:SI 0 "pic30_reg_or_R_operand"         "=r,R")
        (ior:SI  (match_operand:SI 1 "pic30_reg_or_R_operand" "0,0")
                 (match_operand 2 "const_int_operand"         "i,i")))]
  "pic30_one_bit_set_p(INTVAL(operands[2]))"
  "*
{
	switch (which_alternative)
	{
	case 0:
		if (pic30_which_bit(INTVAL(operands[2])) < 16)
		{
			return(\"bset %0,#%b2\");
		}
		else
		{
			return(\"bset %d0,#%b2-16\");
		}
	case 1:
		if (pic30_which_bit(INTVAL(operands[2])) < 16)
		{
			return(\"bset %0,#%b2\");
		}
		else
		{
			return(\"btst %I0,#%b2-16\;bset %D0,#%b2-16\");
		}
	default:
		return(\";\");
	}
}"
  [
   (set_attr "type" "def,use")
  ]
)

(define_insn "*bitsetsir"
  [(set (match_operand:SI 0 "pic30_register_operand"         "=r")
        (ior:SI  (match_operand:SI 1 "pic30_register_operand" "0")
                 (match_operand 2 "const_int_operand"   "i")))]
  "pic30_one_bit_set_p(INTVAL(operands[2]))"
  "*
{
	if (pic30_which_bit(INTVAL(operands[2])) < 16)
	{
		return(\"bset %0,#%b2\");
	}
	else
	{
		return(\"bset %d0,#%b2-16\");
	}
}"
  [
   (set_attr "type" "def")
  ]
)

(define_insn "*bitsetsi_sfr"
  [(set (match_operand:SI 0 "pic30_near_operand"         "=U")
        (ior:SI  (match_dup 0)
                 (match_operand 1 "const_int_operand"  "i")))]
  "pic30_one_bit_set_p(INTVAL(operands[1]))"
  "*
{
	int n = INTVAL(operands[1]);

	if (n >= 0x1000000)
		return(\"bset.b %0+3,#%b1-24\");
	else if (n >= 0x10000)
		return(\"bset.b %0+2,#%b1-16\");
	else if (n >= 0x0100)
		return(\"bset.b %0+1,#%b1-8\");
	else
		return(\"bset.b %0+0,#%b1\");
}")

;;;;;;;;;;;;;;;
;; Reset Bit ;;
;;;;;;;;;;;;;;;

(define_insn "*bitclrsi_sfr"
  [(set (match_operand:SI 0 "pic30_near_operand"         "=U")
        (and:SI  (match_dup 0)
                 (match_operand 1 "const_int_operand"  "i")))]
  "pic30_one_bit_set_p((~INTVAL(operands[1])))"
  "*
{
	int n = pic30_which_bit(~INTVAL(operands[1]));

	if (n > 23)
		return(\"bclr.b %0+3,#%B1-24\");
	else if (n > 15)
		return(\"bclr.b %0+2,#%B1-16\");
	else if (n > 7)
		return(\"bclr.b %0+1,#%B1-8\");
	else
		return(\"bclr.b %0+0,#%B1\");
}")

(define_insn "*bitclrsi"
  [(set (match_operand:SI 0 "pic30_register_operand"         "=r")
        (and:SI  (match_operand:SI 1 "pic30_register_operand" "0")
                 (match_operand 2 "const_int_operand"   "i")))]
  "pic30_one_bit_set_p((~INTVAL(operands[2])))"
  "*
{
	int n = pic30_which_bit(~INTVAL(operands[2]));

	if (n > 15)
		return(\"bclr %d0,#%B2-16\");
	else
		return(\"bclr %0,#%B2\");
}"
  [
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;
;; Toggle Bit ;;
;;;;;;;;;;;;;;;;

(define_insn "bittogsi"
  [(set (match_operand:SI 0 "pic30_register_operand"         "=r")
        (xor:SI  (match_operand:SI 1 "pic30_register_operand" "0")
                 (match_operand 2 "const_int_operand"   "i")))]
  "pic30_one_bit_set_p(INTVAL(operands[2]))"
  "*
{
	unsigned int n = INTVAL(operands[2]);
	if (n >= 65536)
		return(\"btg %d0,#%b2-16\");
	else
		return(\"btg %0,#%b2\");
}"
  [
   (set_attr "type" "def")
  ]
)

(define_insn "bittogsi_sfr"
  [(set (match_operand:SI 0 "pic30_near_operand"         "=U")
        (xor:SI  (match_dup 0)
                 (match_operand 1 "const_int_operand"  "i")))]
  "pic30_one_bit_set_p(INTVAL(operands[1]))"
  "*
{
	unsigned int n = INTVAL(operands[1]);
	if (n >= 0x01000000)
		return(\"btg.b %0+3,#%b1-24\");
	else if (n >= 0x00010000)
		return(\"btg.b %0+2,#%b1-16\");
	else if (n >= 0x00000100)
		return(\"btg.b %0+1,#%b1-8\");
	else
		return(\"btg.b %0+0,#%b1-0\");
}")

;;;;;;;;;;;;;;
;; Test Bit ;;
;;;;;;;;;;;;;;

(define_insn "*bittstsir_and"
  [(set (cc0) 
        (and (match_operand:SI 0 "pic30_register_operand"  "r")
             (match_operand 1 "const_int_operand" "i")))]
  "pic30_one_bit_set_p(INTVAL(operands[1]))"
  "*
{
        if (INTVAL(operands[1]) < (1<<16))
        {
                return(\"btst %0,#%b1\");
        }
        else
        {
                return(\"btst %d0,#%b1-16\");
        }
}"
  [(set_attr "cc" "set")])

(define_insn "*bittstsi_sfr_and"
  [(set (cc0) 
        (and (match_operand:SI 0 "pic30_near_operand" "U")
             (match_operand 1 "const_int_operand"  "i")))]
  "pic30_one_bit_set_p(INTVAL(operands[1]))"
  "btst.b %0+%b1/8,#%b1%%8"
  [(set_attr "cc" "set")])

(define_insn "*bittstsiR"
  [(set (cc0) (zero_extract (match_operand:SI 0 "pic30_reg_or_R_operand" "r,R")
			    (const_int 1)
			    (match_operand 1 "const_int_operand" "i,i")))]
  "INTVAL(operands[1]) < 16"
  "btst %0,#%1"
  [
   (set_attr "cc" "set")
   (set_attr "type" "etc,use")
  ]
)

(define_insn "*bittstsiR_and"
  [(set (cc0)
        (and (match_operand:SI 0 "pic30_reg_or_R_operand"  "r,R")
             (match_operand 1 "const_int_operand"          "i,i")))]
  "pic30_one_bit_set_p(INTVAL(operands[1])) &&
   pic30_which_bit(INTVAL(operands[1])) < 16"
  "btst %0,#%b1"
  [
   (set_attr "cc" "set")
   (set_attr "type" "etc,use")
  ]
)

(define_insn "*bittstsir"
  [(set (cc0) (zero_extract (match_operand:SI 0 "pic30_register_operand" "r")
			    (const_int 1)
			    (match_operand 1 "const_int_operand" "i")))]
  ""
  "*
{
	if (INTVAL(operands[1]) < 16)
	{
		return(\"btst %0,#%1\");
	}
	else
	{
		return(\"btst %d0,#%1-16\");
	}
}"
  [(set_attr "cc" "set")])

(define_insn "*bittstsi_sfr"
  [(set (cc0) (zero_extract (match_operand:SI 0 "pic30_near_operand" "U")
			    (const_int 1)
			    (match_operand 1 "const_int_operand"  "i")))]
  ""
  "btst.b %0+%1/8,#%1%%8"
  [(set_attr "cc" "set")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; quarter integer
;;;;;;;;;;;;;;;;;;

(define_insn "andqi3"
  [(set (match_operand:QI 0 "pic30_mode2_operand"
	        "=r<>,r<>,R,R,  r<>,R,r<>,R,  r<>,R,r")
        (and:QI (match_operand:QI 1 "pic30_mode1J_operand"
		"%r,  r,  r,r,  r,  r,R<>,R<>,P,  P,0")
                (match_operand:QI 2 "pic30_mode1J_operand"
		 "r,  R<>,r,R<>,P,  P,r,  r,  r,  r,J")))]
  ""
  "@
   and.b %1,%2,%0
   and.b %1,%2,%0
   and.b %1,%2,%0
   and.b %1,%2,%0
   and.b %1,#%2,%0
   and.b %1,#%2,%0
   and.b %2,%1,%0
   and.b %2,%1,%0
   and.b %2,#%1,%0
   and.b %2,#%1,%0
   and.b #%2,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use,def,etc,defuse,use,def,etc,def")
  ]
)

(define_insn_and_split "*andqi3_sfr0"
  [(set (match_operand:QI 0 "pic30_register_operand"           "=a,a, d")
        (and:QI (match_operand:QI 1 "pic30_register_operand"   "%a,d, d")
                (match_operand:QI 2 "pic30_near_operand" " U,U, U")))
   (clobber (match_scratch:HI 3                          "=X,X,&r"))]
  ""
  "@
   and.b %2,WREG
   mov.b %1,w0\;and.b %2,WREG
   mov #%2,%3\;and.b %1,[%3],%0"
  "reload_completed"
  [
   (const_int 0)
  ]
{
  if (!pic30_wreg_operand(operands[0], QImode) &&
      !pic30_wreg_operand(operands[1], QImode))
  {
  	rtx pop = gen_rtx_MEM(QImode, operands[3]);
	emit_insn(gen_movhi_address(operands[3], XEXP(operands[2],0)));
	emit_insn(gen_andqi3(operands[0], operands[1], pop));
  	DONE;
  }
  else
  {
  	FAIL;
  }
}
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "*andqi3_sfr2"
  [(set (match_operand:QI 0 "pic30_near_operand"        "=U")
        (and:QI (match_dup 0)
                (match_operand:QI 1 "pic30_wreg_operand" "a")))]
  ""
  "and.b %0"
  [(set_attr "cc" "set")])

(define_insn "*andqi3_sfr3"
  [(set (match_operand:QI 0 "pic30_near_operand"        "=U")
        (and:QI (match_operand:QI 1 "pic30_wreg_operand" "a")
                (match_dup 0)))]
  ""
  "and.b %0"
  [(set_attr "cc" "set")])

;;;;;;;;;;;;;;;
;; half integer
;;;;;;;;;;;;;;;

(define_insn "andhi3"
  [(set (match_operand:HI 0 "pic30_mode2_operand"
	"=r<>,r<>,R,R,  R<>,r,R,  r<>,R,r<>")
        (and:HI (match_operand:HI 1 "pic30_mode1P_operand"
	"%r,  r,  r,r,  r,  r,R<>,R<>,P,P")
                (match_operand:HI 2 "pic30_mode1P_operand"
	 "r,  R<>,r,R<>,P,  P,r,  r,  r,r")))]
  ""
  "@
   and %1,%2,%0
   and %1,%2,%0
   and %1,%2,%0
   and %1,%2,%0
   and %1,#%2,%0
   and %1,#%2,%0
   and %2,%1,%0
   and %2,%1,%0
   and %2,#%1,%0
   and %2,#%1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use,def,etc,use,defuse,etc,def")
  ]
)

(define_insn "*andhi3_imm"
  [(set (match_operand:HI 0 "pic30_register_operand"         "=r")
        (and:HI (match_operand:HI 1 "pic30_register_operand" "%0")
                 (match_operand:HI 2 "pic30_J_operand"  "J")))]
  ""
  "and #%2,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "*andhi3_sfr0"
  [(set (match_operand:HI 0 "pic30_near_operand"         "=U,??U,??U")
        (and:HI (match_operand:HI 1 "pic30_register_operand"   "%a,??d,??r")
                (match_operand:HI 2 "pic30_near_operand"  "0,  0,??U")))
		(clobber (match_scratch:HI 3             "=X,  X, &r"))]
  ""
  "*
   switch (which_alternative) {
     case 0: return \"and %0\";
     case 1: if (pic30_errata_mask & exch_errata)
               return \"push w0\;mov %1,w0\;and %0\;pop w0\";
             else
               return \"exch w0,%1\;and %0\;exch w0,%1\";
     case 2: return \"mov %2,%3\;and %3,%1,%3\;mov %3,%0\";
   }
  "
  [(set_attr "cc" "set")])

(define_insn "*andhi3_sfr1"
  [(set (match_operand:HI 0 "pic30_near_operand"         "=U,??U,??U")
        (and:HI (match_operand:HI 1 "pic30_near_operand" "%0,  0,??U")
                (match_operand:HI 2 "pic30_register_operand"    "a,??d,??r")))
		(clobber (match_scratch:HI 3             "=X,  X, &r"))]
  ""
  "*
   switch (which_alternative) {
     case 0: return \"and %0\";
     case 1: if (pic30_errata_mask & exch_errata)
               return \"push w0\;mov %2,w0\;and %0\;pop w0\";
             else
               return \"exch w0,%2\;and %0\;exch w0,%2\";
     case 2: return \"mov %1,%3\;and %3,%2,%3\;mov %3,%0\";
   }
  "
  [(set_attr "cc" "set")])

(define_insn_and_split "*andhi3_sfr2"
  [(set (match_operand:HI 0 "pic30_register_operand"          "=a,  r, r")
        (and:HI (match_operand:HI 1 "pic30_register_operand"  " a,  r, r")
                (match_operand:HI 2 "pic30_reg_or_near_operand"
                                                              " U,  U, r")))
		(clobber (match_scratch:HI 3                  "=X,  &r,X"))]
  ""
  "@
   and %2,WREG
   mov %2,%3\;and %3,%1,%0
   and %1,%2,%0"
  "reload_completed"
  [
   (set (match_dup 3) (match_dup 2))
   (set (match_dup 0) (and:HI (match_dup 3) (match_dup 1)))
  ]
{
  if ((pic30_wreg_operand(operands[0], HImode) &&
      (pic30_wreg_operand(operands[1], HImode))) || 
      (pic30_register_operand(operands[2], HImode)))
  {
  	FAIL;
  }
}
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;
;; single integer
;;;;;;;;;;;;;;;;;

(define_insn "andsi3"
 [(set (match_operand:SI 0 "pic30_mode2_operand"
			"=r<>,r<>,&r<>,R,R,R,R, R, r<>")
       (and:SI (match_operand:SI 1 "pic30_mode1P_operand"
			"%r,  r,   r,  r,r,r,r, r, r")
               (match_operand:SI 2 "pic30_mode1P_operand"
			 "r  ,<>,  R,  0,R,r,<>,P, P")))]
 ""
 "@
  and %1,%2,%0\;and %d1,%d2,%d0
  and %1,%2,%0\;and %d1,%d2,%d0
  and %1,%I2,%0\;and %d1,%D2,%d0
  and %1,%2,%0\;and %d1,%P2,%D0
  and %1,%I2,%I0\;and %d1,%D2,%D0
  and %1,%2,%I0\;and %d1,%d2,%D0
  and %1,%2,%I0\;and %d1,%2,%D0
  and %1,#%2,%I0\;clr %D0
  and %1,#%2,%0\;clr %d0"
 [
  (set_attr "cc" "clobber")
  (set_attr "type" "def,defuse,defuse,use,use,etc,use,etc,def")
 ]
)

(define_insn_and_split "*andsi3_sfr0"
  [(set (match_operand:SI 0 "pic30_data_operand"      "=T")
        (and:SI (match_dup 0)
                (match_operand:SI 1 "pic30_register_operand" "r")))
		(clobber (match_scratch:HI 2         "=&r"))]
  ""
  "mov #%0,%2\;and %1,[%2],[%2]\;and %d1,[++%2],[%2]"
  "reload_completed"
  [
   (const_int 0)
  ]
  {
  	rtx pop = gen_rtx_MEM(SImode, operands[2]);
	emit_insn(gen_movhi_address(operands[2], XEXP(operands[0],0)));
	emit_insn(gen_andsi3(pop, operands[1], pop));
	DONE;
  }
  [
   (set_attr "cc" "clobber")
  ]
)

(define_insn_and_split "*andsi3_sfr1"
  [(set (match_operand:SI 0 "pic30_data_operand"      "=T")
        (and:SI (match_operand:SI 1 "pic30_register_operand" "r")
                (match_dup 0)))
		(clobber (match_scratch:HI 2         "=&r"))]
  ""
  "mov #%0,%2\;and %1,[%2],[%2]\;and %d1,[++%2],[%2]"
  "reload_completed"
  [
   (const_int 0)
  ]
  {
  	rtx pop = gen_rtx_MEM(SImode, operands[2]);
	emit_insn(gen_movhi_address(operands[2], XEXP(operands[0],0)));
	emit_insn(gen_andsi3(pop, operands[1], pop));
	DONE;
  }
  [
   (set_attr "cc" "clobber")
  ]
)

;;;;;;;;;;;;;;;;;
;; double integer
;;;;;;;;;;;;;;;;;

(define_insn "anddi3"
  [(set (match_operand:DI 0 "pic30_register_operand"         "=r")
        (and:DI (match_operand:DI 1 "pic30_register_operand" "%r")
                (match_operand:DI 2 "pic30_register_operand"  "r")))]
  ""
  "and %2,%1,%0\;and %d2,%d1,%d0\;and %t2,%t1,%t0\;and %q2,%q1,%q0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; quarter integer
;;;;;;;;;;;;;;;;;;

(define_insn "*iorqi3_imm"
  [(set (match_operand:QI 0 "pic30_register_operand"         "=r")
        (ior:QI (match_operand:QI 1 "pic30_register_operand" "%0")
                (match_operand:QI 2 "pic30_J_operand"   "J")))]
  ""
  "ior.b #%2,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "pic30_mode2_operand"
		"=r<>,r<>,R,R,  r<>,R,r<>,R,  r<>,R")
        (ior:QI (match_operand:QI 1 "pic30_mode1P_operand"
		"%r,  r,  r,r,  r,  r,R<>,R<>,P,  P")
                (match_operand:QI 2 "pic30_mode1P_operand"
		 "r,  R<>,r,R<>,P,  P,r,  r,  r,  r")))]
  ""
  "@
   ior.b %1,%2,%0
   ior.b %1,%2,%0
   ior.b %1,%2,%0
   ior.b %1,%2,%0
   ior.b %1,#%2,%0
   ior.b %1,#%2,%0
   ior.b %2,%1,%0
   ior.b %2,%1,%0
   ior.b %2,#%1,%0
   ior.b %2,#%1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use,def,etc,defuse,use,def,etc")
  ]
)

(define_insn_and_split "*iorqi3_sfr0"
  [(set (match_operand:QI 0 "pic30_register_operand"           "=a,a,r")
        (ior:QI (match_operand:QI 1 "pic30_register_operand"   "%a,d,r")
                (match_operand:QI 2 "pic30_near_operand" " U,U,U")))
   (clobber (match_scratch: HI 3                         "=X,X,&r"))]
  ""
  "@
   ior.b %2,WREG
   mov.b %1,w0\;ior.b %2,WREG
   mov #%2,%3\;ior.b %1,[%3],%0"
  "reload_completed"
  [
   (const_int 0)
  ]
{
  if (!pic30_wreg_operand(operands[0], QImode) &&
      !pic30_wreg_operand(operands[1], QImode))
  {
  	rtx pop = gen_rtx_MEM(QImode, operands[3]);
	emit_insn(gen_movhi_address(operands[3], XEXP(operands[2],0)));
	emit_insn(gen_iorqi3(operands[0], operands[1], pop));
  	DONE;
  }
  else
  {
  	FAIL;
  }
}
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

 (define_insn "*iorqi3_sfr1"
   [(set (match_operand:QI 0 "pic30_near_operand"      "=U, U")
         (ior:QI (match_dup 0)
                 (match_operand:QI 1 "pic30_register_operand" "a, !d")))
               (clobber (match_scratch:HI 2          "=X, &r"))]
   ""
   "@
    ior.b %0
    mov #%0,%2\;ior.b %1,[%2],[%2]"
   [(set_attr "cc" "set")])

;;;;;;;;;;;;;;;
;; half integer
;;;;;;;;;;;;;;;

(define_insn "iorhi3_sfr0"
  [(set (match_operand:HI 0 "pic30_near_operand"         "=U,??U")
        (ior:HI (match_operand:HI 1 "pic30_register_operand"   "a,!??r")
                (match_dup 0)))
                (clobber (match_scratch:HI 2             "=X, &d"))]
  ""
  "@
   ior %0
   mov %0,%2\;ior %2,%1,%2\;mov %2,%0"
  [(set_attr "cc" "set")])

(define_insn "iorhi3_sfr0a"
  [(set (match_operand:HI 0 "pic30_near_operand"         "=U,??U")
        (ior:HI (match_dup 0)
                (match_operand:HI 1 "pic30_register_operand"   "a,!??r")))]
  ""
  "*
 
   switch (which_alternative) {
     case 0: return \"ior %0\";
     case 1: if (pic30_errata_mask & exch_errata)
                  return \"push w0\;mov %1,w0\;ior %0\;pop w0\";
                else
                  return \"exch %1,w0\;ior %0\;exch %1,w0\";
   }
  "
  [(set_attr "cc" "set")])


(define_insn "*iorhi3_sfr1"
  [(set (match_operand:HI 0 "pic30_register_operand"          "=a, a, ?r")
        (ior:HI (match_operand:HI 1 "pic30_register_operand"  "0, r, r")
                (match_operand:HI 2 "pic30_near_operand" "U,  U, U")))
                (clobber (match_scratch:HI 3            "=X,  X, &r"))]
  ""
  "@
   ior %2,WREG
   mov %1,w0\;ior %2,WREG
   mov %2,%3\;ior %3,%1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "*iorhi3_sfr1a"
  [(set (match_operand:HI 0 "pic30_register_operand"          "=a, ?r")
        (ior:HI (match_operand:HI 1 "pic30_near_operand" "U,  U")
                (match_dup 0)))
                (clobber (match_scratch:HI 2            "=X,  &r"))]
  ""
  "@
   ior %1,WREG
   mov %1,%2\;ior %2,%0,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "*iorhi3_imm"
  [(set (match_operand:HI 0 "pic30_register_operand"          "=r")
        (ior:HI (match_operand:HI 1 "pic30_register_operand" "0")
                (match_operand:HI 2 "pic30_J_operand" "J")))]
  ""
  "ior #%2,%0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "*iorhi3_imma"
  [(set (match_operand:HI 0 "pic30_register_operand"          "=r")
        (ior:HI (match_operand:HI 1 "pic30_J_operand" "J")
                (match_operand:HI 2 "pic30_register_operand" "0")))]
  ""
  "ior #%1,%0"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "pic30_mode2_operand"
	"=r<>,r<>,R,  R,r<>,R,r<>,R,  r<>,R")
        (ior:HI (match_operand:HI 1 "pic30_mode1P_operand"
	"%r,  r,  r,  r,r,  r,R<>,R<>,P,  P")
                (match_operand:HI 2 "pic30_mode1P_operand"
	 "R<>,r,  R<>,r,P,  P,r,  r,  r,  r")))
  ]
  ""
  "@
   ior %1,%2,%0
   ior %1,%2,%0
   ior %1,%2,%0
   ior %1,%2,%0
   ior %1,#%2,%0
   ior %1,#%2,%0
   ior %2,%1,%0
   ior %2,%1,%0
   ior %2,#%1,%0
   ior %2,#%1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "defuse,def,use,etc,def,etc,defuse,use,def,etc")
  ]
)

;;;;;;;;;;;;;;;;;
;; single integer
;;;;;;;;;;;;;;;;;

(define_insn "iorsi3"
 [(set (match_operand:SI 0 "pic30_mode2_operand"
				"=r<>,&r<>,R,R,R,  R,r, r,<>")
       (ior:SI (match_operand:SI 1 "pic30_mode1P_operand"
				"%r,   r,  r,r,r,  r,0, r,r")
               (match_operand:SI 2 "pic30_mode1P_operand"
				 "r<>, R,  0,R,r<>,P,P, P,P")))]
 ""
 "@
  ior %1,%2,%0\;ior %d1,%d2,%d0
  ior %1,%I2,%0\;ior %d1,%D2,%d0
  ior %1,%2,%I0\;ior %d1,%2,%D0
  ior %1,%I2,%I0\;ior %d1,%D2,%D0
  ior %1,%2,%I0\;ior %d1,%d2,%D0
  ior %1,#%2,%I0\;ior %d1,#0,%D0
  ior %1,#%2,%0
  ior %1,#%2,%0\;ior %d1,#0,%d0
  ior %1,#%2,%0\;ior %d1,#0,%0"
 [
  (set_attr "cc" "clobber")
  (set_attr "type" "defuse,defuse,etc,use,use,etc,def,def,def")
 ]
)

(define_insn "*iorsi3_sfr0"
  [(set (match_operand:SI 0 "pic30_data_operand"         "=T")
        (ior:SI (match_dup 0)
                (match_operand:SI 1 "pic30_register_operand"    "r")))
		(clobber (match_scratch:HI 2             "=&r"))]
  ""
  "mov #%0,%2\;ior %1,[%2],[%2]\;ior %d1,[++%2],[%2]"
  [(set_attr "cc" "clobber")])

(define_insn "*iorsi3_sfr1"
  [(set (match_operand:SI 0 "pic30_data_operand"        "=T")
        (ior:SI (match_operand:SI 1 "pic30_register_operand"  "r")
                (match_dup 0)))
   (clobber (match_scratch:HI 2            "=&r"))]
  ""
  "
   mov #%0,%2\;ior %1,[%2],[%2]\;ior %d1,[++%2],[%2]"
  [(set_attr "cc" "clobber")])

;;;;;;;;;;;;;;;;;
;; double integer
;;;;;;;;;;;;;;;;;

(define_insn "iordi3"
  [(set (match_operand:DI 0 "pic30_register_operand"         "=r")
        (ior:DI (match_operand:DI 1 "pic30_register_operand" "%r")
                (match_operand:DI 2 "pic30_register_operand"  "r")))]
  ""
  "ior %2,%1,%0\;ior %d2,%d1,%d0\;ior %t2,%t1,%t0\;ior %q2,%q1,%q0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; quarter integer
;;;;;;;;;;;;;;;;;;

(define_insn "*xorqi3_imm"
  [(set (match_operand:QI 0 "pic30_register_operand"         "=r")
        (xor:QI (match_operand:QI 1 "pic30_register_operand" "%0")
                (match_operand:QI 2 "pic30_J_operand"   "J")))]
  ""
  "xor.b #%2,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "pic30_mode2_operand"
		"=r<>,r<>,R,R,  r<>,R,r<>,R,  r<>,R")
        (xor:QI (match_operand:QI 1 "pic30_mode1P_operand"
		"%r,  r,  r,r,  r,  r,R<>,R<>,P,  P")
                (match_operand:QI 2 "pic30_mode1P_operand"
		" r,  R<>,r,R<>,P,  P,r,  r,  r,  r")))]
  ""
  "@
   xor.b %1,%2,%0
   xor.b %1,%2,%0
   xor.b %1,%2,%0
   xor.b %1,%2,%0
   xor.b %1,#%2,%0
   xor.b %1,#%2,%0
   xor.b %2,%1,%0
   xor.b %2,%1,%0
   xor.b %2,#%1,%0
   xor.b %2,#%1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use,def,etc,defuse,use,def,etc")
  ]
)

(define_insn_and_split "*xorqi3_sfr0"
  [(set (match_operand:QI 0 "pic30_register_operand"           "=a,a, r")
        (xor:QI (match_operand:QI 1 "pic30_register_operand"   "%a,d, r")
                (match_operand:QI 2 "pic30_near_operand" " U,U, U")))
   (clobber (match_scratch:HI 3                          "=X,X,&r"))]
  ""
  "@
   xor.b %2,WREG
   mov.b %1,w0\;xor.b %2,WREG
   mov #%2,%3\;xor.b %1,[%3],%0"
  "reload_completed"
  [
   (const_int 0)
  ]
{
  if (!pic30_wreg_operand(operands[0], QImode) &&
      !pic30_wreg_operand(operands[1], QImode))
  {
  	rtx pop = gen_rtx_MEM(QImode, operands[3]);
	emit_insn(gen_movhi_address(operands[3], XEXP(operands[2],0)));
	emit_insn(gen_xorqi3(operands[0], operands[1], pop));
  	DONE;
  }
  else
  {
  	FAIL;
  }
}
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "*xorqi3_sfr1"
  [(set (match_operand:QI 0 "pic30_near_operand"       "=U,!???U")
        (xor:QI (match_dup 0)
                (match_operand:QI 1 "pic30_register_operand"  "a,!???d")))]
  ""
  "*
   switch (which_alternative) {
     case 0: return \"xor.b %0\";
     case 1: if (pic30_errata_mask & exch_errata) 
               return \"push w0\;mov %1,w0\;xor.b %0\;pop w0\";
             else 
               return \"exch w0,%1\;xor.b %0\;exch w0,%1\";
   }
  "
  [(set_attr "cc" "set")])

;;;;;;;;;;;;;;;
;; half integer
;;;;;;;;;;;;;;;

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "pic30_mode2_operand"
	                         "=r<>,r<>, R,R,  r<>,R,r<>,R,  r<>,R")
        (xor:HI (match_operand:HI 1 "pic30_mode1P_operand"
	                                 "%r,  r,   r,r,  r,  r,R<>,R<>,P,  P")
                (match_operand:HI 2 "pic30_mode1P_operand"
	                                 "r,  R<>, r,R<>,P,  P,r,  r,  r,  r")))]
  ""
  "@
   xor %1,%2,%0
   xor %1,%2,%0
   xor %1,%2,%0
   xor %1,%2,%0
   xor %1,#%2,%0
   xor %1,#%2,%0
   xor %2,%1,%0
   xor %2,%1,%0
   xor %2,#%1,%0
   xor %2,#%1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use,def,etc,defuse,use,def,etc")
  ]
)

(define_insn "xorhi3_sfr2"
  [(set (match_operand:HI 0 "pic30_near_operand" "=U")
        (xor:HI (match_dup 0)
                (match_operand:HI 1 "pic30_immediate_1bit_operand" "i")))]
  ""
  "*
{  unsigned int bit_num;
   static char insn[48];

   if (GET_CODE(operands[1]) == CONST_INT) {
     int i;
     i = INTVAL(operands[1]);
 
     i = i & 0xFFFF;
     /* apparantly I can't use log here */
     /* bit_num = log(bit_num) / log(2.0);
     /* if (bit_num > 15) abort(); */
     for (bit_num = 0 ; bit_num <= 16; bit_num++)
       if (((1 << bit_num) & i) == i) break;
     if (bit_num == 16) abort();
     sprintf(insn, \"btg %%0,#%d\",bit_num);
     return insn;
   } else abort();
}
"
 [(set_attr "cc" "set")])

(define_insn "xorhi3_sfr3"
  [(set (match_operand:HI 0 "pic30_near_operand" "=U")
        (xor:HI (match_operand:HI 1 "pic30_immediate_1bit_operand" "i")
                (match_dup 0)))]
  ""
  "*
{  unsigned int bit_num;
   static char insn[48];

   if (GET_CODE(operands[1]) == CONST_INT) {
     int i;
     i = INTVAL(operands[1]);

     i = i & 0xFFFF;
     /* apparantly I can't use log here */
     /* bit_num = log(bit_num) / log(2.0);
     /* if (bit_num > 15) abort(); */
     for (bit_num = 0 ; bit_num <= 16; bit_num++)
       if (((1 << bit_num) & i) == i) break;
     if (bit_num == 16) abort();
     sprintf(insn, \"btg %%0,#%d\",bit_num);
     return insn;
   } else abort();
}
"
 [(set_attr "cc" "set")])

(define_insn "*xorhi3_imm"
  [(set (match_operand:HI 0 "pic30_register_operand"         "=r")
        (xor:HI (match_operand:HI 1 "pic30_register_operand" "%0")
                (match_operand:HI 2 "pic30_J_operand"   "J")))]
  ""
  "xor #%2,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "*xorhi3_sfr0"
  [(set (match_operand:HI 0 "pic30_reg_or_near_operand"
						          "=a,U,!???d,!???U")
        (xor:HI (match_dup 0)
                (match_operand:HI 1 "pic30_reg_or_near_operand"
						                  "U,a,!???U,!???d")))]
  ""
  "*
   
   switch (which_alternative) {
     case 0: return \"xor %1,WREG\";
     case 1: return \"xor %0\";
     case 2: if (pic30_errata_mask & exch_errata) 
               return \"push w0\;mov w0,%0\;xor %1,WREG\;mov w0,%0\;pop w0\";
             else
               return \"exch w0,%0\;xor %1,WREG\;exch w0,%0\";
     case 3: if (pic30_errata_mask & exch_errata) 
               return \"push w0\;mov %1,w0\;xor %0\;pop w0\";
             else
               return \"exch w0,%1\;xor %0\;exch w0,%1\";
   }
  "
  [(set_attr "cc" "set")])

(define_insn "*xorhi3_sfr1"
  [(set (match_operand:HI 0 "pic30_reg_or_near_operand" "=a,U,!???d,!???U")
        (xor:HI (match_operand:HI 1 "pic30_reg_or_near_operand"
						                  "U,a,!???U,!???d")
		  (match_dup 0)))]
  ""
  "*

   switch (which_alternative) {
     case 0: return \"xor %1,WREG\";
     case 1: return \"xor %0\";
     case 2: if (pic30_errata_mask & exch_errata)
               return \"push w0\;mov %0,w0\;xor %1,WREG\;mov w0,%0\;pop w0\";
             else 
               return \"exch w0,%0\;xor %1,WREG\;exch w0,%0\";
     case 3: if (pic30_errata_mask & exch_errata) 
               return \"push w0\;mov %1,w0\;xor %0\;pop w0\";
             else
               return \"exch w0,%1\;xor %0\;exch w0,%1\";
   }
  "
  [(set_attr "cc" "set")])


;;;;;;;;;;;;;;;;;
;; single integer
;;;;;;;;;;;;;;;;;

(define_insn "xorsi3"
 [(set (match_operand:SI 0 "pic30_mode2_operand"
					"=r<>,&r<>,R,R,R,  R,r,&r,<>")
       (xor:SI (match_operand:SI 1 "pic30_mode1P_operand"
					"%r,   r,  r,r,r,  r,0, r,r")
               (match_operand:SI 2 "pic30_mode1P_operand"
					 "r<>, R,  0,R,r<>,P,P, P,P")))]
 ""
 "@
  xor %1,%2,%0\;xor %d1,%d2,%d0
  xor %1,%I2,%0\;xor %d1,%D2,%d0
  xor %1,%2,%I0\;xor %d1,%2,%D0
  xor %1,%I2,%I0\;xor %d1,%D2,%D0
  xor %1,%2,%I0\;xor %d1,%d2,%D0
  xor %1,#%2,%I0\;xor %d1,#0,%D0
  xor %1,#%2,%0
  xor %1,#%2,%0\;xor %d1,#0,%d0
  xor %1,#%2,%0\;xor %d1,#0,%0"
 [
  (set_attr "cc" "clobber")
  (set_attr "type" "defuse,defuse,etc,use,use,etc,def,def,def")
 ]
)

;;;;;;;;;;;;;;;;;
;; double integer
;;;;;;;;;;;;;;;;;

(define_insn "xordi3"
  [(set (match_operand:DI 0 "pic30_register_operand"         "=r")
        (xor:DI (match_operand:DI 1 "pic30_register_operand" "%r")
                (match_operand:DI 2 "pic30_register_operand"  "r")))]
  ""
  "xor %2,%1,%0\;xor %d2,%d1,%d0\;xor %t2,%t1,%t0\;xor %q2,%q1,%q0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; QImode ;;
;;;;;;;;;;;;

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "pic30_mode2_operand"        "=r<>,r<>,R,R")
        (not:QI (match_operand:QI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")))]
  ""
  "com.b %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*one_cmplqi2_sfr"
  [(set (match_operand:QI 0 "pic30_near_operand"        "=U")
        (not:QI (match_dup 0)))]
  ""
  "com.b %0"
  [(set_attr "cc" "set")])

;;;;;;;;;;;;
;; HImode ;;
;;;;;;;;;;;;

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "pic30_mode2_operand"        "=r<>,r<>,R,R")
        (not:HI (match_operand:HI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")))]
  ""
  "com %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*one_cmplhi2_sfr"
  [(set (match_operand:HI 0 "pic30_near_operand"        "=U")
        (not:HI (match_dup 0)))]
  ""
  "com %0"
  [(set_attr "cc" "set")])

;;;;;;;;;;;;
;; SImode ;;
;;;;;;;;;;;;

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "pic30_register_operand"        "=r")
        (not:SI (match_operand:SI 1 "pic30_register_operand" "r")))]
  ""
  "com %1,%0\;com %d1,%d0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "*one_cmplsi2_sfr"
  [(set (match_operand:SI 0 "pic30_near_operand"        "=U")
        (not:SI (match_dup 0)))]
  ""
  "com %0\;com %0+2"
  [(set_attr "cc" "clobber")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find first one
;;
;; Represents one plus the index of the least significant 1-bit in X,
;; represented as an integer of mode M.  (The value is zero if X is
;; zero.)  The mode of X need not be M; depending on the target
;; machine, various mode combinations may be valid.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "ffshi2"
  [(set (match_operand:HI 0 "pic30_register_operand"           "=r,r")
        (ffs:HI (match_operand:HI 1 "pic30_mode2_operand" "r,R<>")))]
  ""
  "*
{
  return \"ff1r %1,%0\";
}"
  [(set_attr "type" "def,defuse")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shift instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arithmetic Shift Left instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; QImode ;;
;;;;;;;;;;;;

(define_insn "*ashlqi3_gen_1"
  [(set (match_operand:QI 0 "pic30_mode2_operand"           "=r<>,r<>,R,R")
        (ashift:QI (match_operand:QI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")
                   (match_operand:QI 2 "pic30_I_operand"     "I,  I,  I,I")))]
  ""
  "sl.b %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*ashlqi3_sfr_1"
  [(set (match_operand:QI 0 "pic30_wreg_or_near_operand"   "=a,U")
        (ashift:QI (match_operand:QI 1 "pic30_near_operand" "U,0")
                   (match_operand:QI 2 "pic30_I_operand"    "I,I")))]
  ""
  "@
    sl.b %1,WREG
    sl.b %0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,etc")
  ]
)

;; For shift by n, we operate in word mode.
;; This is ok, since we are shifting left
;; (zeroes are inserted from the right).
;; However, the condition codes are not useful.

(define_insn "ashlqi3"
  [(set (match_operand:QI 0 "pic30_register_operand"                 "=r,r")
        (ashift:QI (match_operand:QI 1 "pic30_register_operand"       "r,r")
                   (match_operand:QI 2 "pic30_reg_or_P_operand" "r,P")))]
  ""
  "@
    sl %1,%2,%0
    sl %1,#%2,%0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;
;; HImode ;;
;;;;;;;;;;;;

(define_insn "*ashlhi3_gen_1"
  [(set (match_operand:HI 0 "pic30_mode2_operand"           "=r<>,r<>,R,R")
        (ashift:HI (match_operand:HI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")
                   (match_operand:HI 2 "pic30_I_operand"     "I,  I,  I,I")
        )
   )
  ]
  ""
  "@
    sl %1,%0
    sl %1,%0
    sl %1,%0
    sl %1,%0"
 [
  (set_attr "cc" "set")
  (set_attr "type" "def,defuse,etc,use")
 ]
)

(define_insn_and_split "*ashlhi3_sfr0"
  [(set (match_operand:HI 0 "pic30_register_operand"             "=a,?d")
        (ashift:HI (match_operand:HI 1 "pic30_near_operand" "U, U")
                   (match_operand:HI 2 "pic30_I_operand"    "I, I")))]
  ""
  "@
   sl %1,WREG
   mov %1,%0\;sl %0,%0"
  "reload_completed"
  [
   (set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (ashift:HI (match_dup 0) (const_int 1)))
  ]
{
  if (!pic30_wreg_operand(operands[0], HImode))
  {
  }
  else
  {
  	FAIL;
  }
}
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "*ashlhi3_sfr1"
  [(set (match_operand:HI 0 "pic30_near_operand"           "=U")
        (ashift:HI (match_dup 0)
                   (match_operand:HI 1 "pic30_I_operand"    "I")))]
  ""
  "sl %0"
  [(set_attr "cc" "set")])

(define_insn "ashlhi3"
  [(set (match_operand:HI 0 "pic30_register_operand"                   "=r,r,r")
        (ashift:HI (match_operand:HI 1 "pic30_register_operand"         "r,r,r")
                   (match_operand:HI 2 "pic30_reg_or_imm_operand" "K,i,r")))]
  ""
  "*
{
	switch (which_alternative)
	{
	case 0:
    		return(\"sl %1,#%2,%0\");
	case 1:
		if (INTVAL(operands[2]) < 0)
    			return(\"asr %1,#%J2%%16,%0\");
		else
    			return(\"sl %1,#%2%%16,%0\");
	case 2:
    		return(\"sl %1,%2,%0\");
	default:
		return(\"#\");
	}
}"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;
;; SImode ;;
;;;;;;;;;;;;

(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "pic30_register_operand" "")
        (ashift:SI (match_operand:SI 1 "pic30_register_operand" "")
                   (match_operand:HI 2 "pic30_reg_or_imm_operand" "")))]
  ""
  "
{
	if (GET_CODE(operands[2]) == CONST_INT)
	{
		switch (INTVAL(operands[2]))
		{
		case 0:
			emit_insn(gen_movsi(operands[0], operands[1]));
			break;
		case 1:
			emit_insn(gen_ashlsi3_imm1(operands[0],
						operands[1], operands[2]));
			break;
		case 8:
			emit_insn(gen_ashlsi3_imm8(operands[0],
						operands[1], operands[2]));
			break;
		case 2 ... 7:
		case 9 ... 15:
			emit_insn(gen_ashlsi3_imm2to15(operands[0],
						operands[1], operands[2]));
			break;
		case 16:
			emit_insn(gen_ashlsi3_imm16plus(operands[0],
						operands[1], operands[2]));
			break;
		case 17 ... 31:
			emit_insn(gen_ashlsi3_imm16plus(operands[0],
						operands[1], operands[2]));
			break;
		default:
			emit_insn(gen_movsi(operands[0], const0_rtx));
			break;
		}
	}
	else
	{
		emit_insn(gen_ashlsi3_reg(operands[0],operands[1],operands[2]));
	}
	DONE;
}")

(define_insn "ashlsi3_imm1"
  [(set (match_operand:SI 0            "pic30_register_operand" "=r")
        (ashift:SI (match_operand:SI 1 "pic30_register_operand"  "r")
                   (match_operand:HI 2 "pic30_I_operand"   "I")))]
  ""
  "add %1,%1,%0\;addc %d1,%d1,%d0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashlsi3_imm8"
  [(set (match_operand:SI 0            "pic30_register_operand"  "=r")
        (ashift:SI (match_operand:SI 1 "pic30_register_operand"   "r")
                   (match_operand:HI 2 "pic30_imm8_operand" "i")))]
  ""
  "*
{
	int idDst = REGNO(operands[0]);
	int idSrc = REGNO(operands[1]);
	if (idDst == idSrc)
	{
		return(	\"sl %d1,#%2,%d0\;\"
			\"swap %1\;\"
			\"mov.b %0,%d0\;\"
			\"clr.b %0\");
	}
	else
	{
		return(	\"sl %d1,#%2,%0\;\"
			\"lsr %1,#%k2,%d0\;\"
			\"ior %0,%d0,%d0\;\"
			\"sl %1,#%2,%0\");
	}
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashlsi3_imm16plus"
  [(set (match_operand:SI 0            "pic30_register_operand"       "=r")
        (ashift:SI (match_operand:SI 1 "pic30_register_operand"        "r")
                   (match_operand:HI 2 "pic30_imm16plus_operand" "i")))]
  ""
  "sl %1,#%K2,%d0\;mov #0,%0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashlsi3_imm2to15"
  [(set (match_operand:SI 0            "pic30_register_operand"      "=r,&r")
        (ashift:SI (match_operand:SI 1 "pic30_register_operand"       "r, r")
                   (match_operand:HI 2 "pic30_imm2to15_operand" "i, i")))
		   (clobber (match_scratch:HI 3               "=&r, X"))]
  ""
  "*
{
	int idDst, idSrc;

	switch (which_alternative)
	{
	case 0:
		/*
		** Take care that the source and dest don't overlap
		*/
		idDst = REGNO(operands[0]);
		idSrc = REGNO(operands[1]);
		if (idDst >= idSrc)
		{
			return(	\"sl %d1,#%2,%3\;\"
				\"lsr %1,#%k2,%d0\;\"
				\"ior %3,%d0,%d0\;\"
				\"sl %1,#%2,%0\");
		}
		else
		{
			return(	\"sl %1,#%2,%0\;\"
				\"sl %d1,#%2,%3\;\"
				\"lsr %1,#%k2,%d0\;\"
				\"ior %3,%d0,%d0\");
		}
	default:
		/*
		** The dest and source don't overlap
		** so use dest lsw as a temporary
		*/
		return(	\"sl %d1,#%2,%0\;\"
			\"lsr %1,#%k2,%d0\;\"
			\"ior %0,%d0,%d0\;\"
			\"sl %1,#%2,%0\");
	}
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashlsi3_reg"
  [(set (match_operand:SI 0            "pic30_register_operand" "=r")
        (ashift:SI (match_operand:SI 1 "pic30_register_operand"  "0")
                   (match_operand:HI 2 "pic30_register_operand"  "r")))
		   (clobber (match_scratch:HI 3           "=2"))]
  ""
  "*
{
      return( 
#if (1)
      	      \".set ___BP___,0\n\"
#endif
              \".LB%=:\;\"
              \"dec %2,%2\;\"
              \"bra n,.LE%=\;\"
              \"add %1,%1,%0\;\"
              \"addc %d1,%d1,%d0\;\"
              \"bra .LB%=\n\"
              \".LE%=:\");
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DImode arithmetic shift left.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "ashldi3"
  [(set (match_operand:DI 0 "pic30_register_operand"            "")
        (ashift:DI (match_operand:DI 1 "pic30_register_operand"  "")
                   (match_operand:HI 2 "pic30_reg_or_imm_operand" "")))]
  ""
  "
{
	if (GET_CODE(operands[2]) == CONST_INT)
	{
		switch (INTVAL(operands[2]))
		{
		case 0:
			emit_insn(gen_movdi(operands[0], operands[1]));
			break;
		case 1:
			emit_insn(gen_ashldi3_imm1(operands[0],
						    operands[1], operands[2]));
			break;
		case 2 ... 15:
			emit_insn(gen_ashldi3_immn(operands[0],
						   operands[1], operands[2]));
			break;
		case 16:
			emit_insn(gen_ashldi3_imm16(operands[0],
						    operands[1], operands[2]));
			break;
		case 17 ... 31:
			emit_insn(gen_ashldi3_immn(operands[0],
						   operands[1], operands[2]));
			break;
		case 32:
			emit_insn(gen_ashldi3_imm32(operands[0],
						    operands[1], operands[2]));
			break;
		case 33 ... 47:
			emit_insn(gen_ashldi3_immn(operands[0],
						   operands[1], operands[2]));
			break;
		case 48:
			emit_insn(gen_ashldi3_imm48(operands[0],
						    operands[1], operands[2]));
			break;
		case 49 ... 63:
			emit_insn(gen_ashldi3_immn(operands[0],
						   operands[1], operands[2]));
			break;
		default:
			emit_insn(gen_movdi(operands[0], const0_rtx));
			break;
		}
	}
	else
	{
		emit_insn(gen_ashldi3_reg(operands[0],operands[1],operands[2]));
	}
	DONE;
}")

(define_insn "ashldi3_imm1"
  [(set (match_operand:DI 0            "pic30_register_operand" "=r")
        (ashift:DI (match_operand:DI 1 "pic30_register_operand"  "r")
                   (match_operand:HI 2 "immediate_operand" "i")))]
  "INTVAL(operands[2])==1"
  "sl %1,%0\;rlc %d1,%d0\;rlc %t1,%t0\;rlc %q1,%q0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashldi3_imm16"
  [(set (match_operand:DI 0            "pic30_register_operand" "=r")
        (ashift:DI (match_operand:DI 1 "pic30_register_operand"  "r")
                   (match_operand:HI 2 "immediate_operand" "i")))]
  "INTVAL(operands[2])==16"
  "mov %t1,%q0\;mov %d1,%t0\;mov %1,%d0\;mov #0,%0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashldi3_imm32"
  [(set (match_operand:DI 0            "pic30_register_operand" "=r")
        (ashift:DI (match_operand:DI 1 "pic30_register_operand"  "r")
                   (match_operand:HI 2 "immediate_operand" "i")))]
  "INTVAL(operands[2])==32"
  "mov.d %1,%t0\;mul.su %0,#0,%0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashldi3_imm48"
  [(set (match_operand:DI 0            "pic30_register_operand" "=r")
        (ashift:DI (match_operand:DI 1 "pic30_register_operand"  "r")
                   (match_operand:HI 2 "immediate_operand" "i")))]
  "INTVAL(operands[2])==48"
  "mov %1,%q0\;mov #0,%t0\;mul.su %0,#0,%0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashldi3_immn"
  [(set (match_operand:DI 0 "pic30_register_operand"            "=r")
        (ashift:DI (match_operand:DI 1 "pic30_register_operand"  "0")
                   (match_operand:HI 2 "immediate_operand" "i")))
		   (clobber (match_scratch:HI 3           "=&r"))]
  ""
  "*
{
      return( 
#if (1)
      	      \".set ___BP___,0\;\"
#endif
              \"mov #%2,%3\n\"
              \".LB%=:\;\"
              \"sl %1,%0\;\"
              \"rlc %d1,%d0\;\"
              \"rlc %t1,%t0\;\"
              \"rlc %q1,%q0\;\"
              \"dec %3,%3\;\"
              \"bra nz,.LB%=\");
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashldi3_reg"
  [(set (match_operand:DI 0 "pic30_register_operand"           "=r")
        (ashift:DI (match_operand:DI 1 "pic30_register_operand" "0")
                   (match_operand:HI 2 "pic30_register_operand" "r")))
		   (clobber (match_scratch:HI 3          "=2"))]
  ""
  "*
{
      return( 
#if (1)
      	      \".set ___BP___,0\n\"
#endif
              \".LB%=:\;\"
              \"dec %2,%2\;\"
              \"bra n,.LE%=\;\"
              \"sl %1,%0\;\"
              \"rlc %d1,%d0\;\"
              \"rlc %t1,%t0\;\"
              \"rlc %q1,%q0\;\"
              \"bra .LB%=\n\"
              \".LE%=:\");
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logical Shift Right instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; QImode ;;
;;;;;;;;;;;;

(define_insn "*lshrqi3_one"
  [(set (match_operand:QI 0 "pic30_mode2_operand"             "=r<>,r<>,R,R")
        (lshiftrt:QI (match_operand:QI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")
                     (match_operand:QI 2 "pic30_I_operand"     "I,  I,  I,I")))]
  ""
  "lsr.b %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*lshrqi3_sfr0"
  [(set (match_operand:QI 0 "pic30_register_operand"               "=a,r")
        (lshiftrt:QI (match_operand:QI 1 "pic30_near_operand" "U,U")
                     (match_operand:QI 2 "pic30_I_operand"    "I,I")))]
  ""
  "@
    lsr.b %1,WREG
    mov #%1,%0\;lsr.b [%0],%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "*lshrqi3_sfr1"
  [(set (match_operand:QI 0 "pic30_near_operand"          "=U")
        (lshiftrt:QI (match_dup 0)
                     (match_operand:QI 1 "pic30_I_operand" "I")))]
  ""
  "lsr %0"
  [(set_attr "cc" "set")])

;;;;;;;;;;;;
;; HImode ;;
;;;;;;;;;;;;

(define_insn "*lshrhi3_one"
  [(set (match_operand:HI 0 "pic30_mode2_operand"             "=r<>,r<>,R,R")
        (lshiftrt:HI (match_operand:HI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")
                     (match_operand:HI 2 "pic30_I_operand"     "I,  I,  I,I")))]
  ""
  "lsr %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*lshrhi3_sfr0"
  [(set (match_operand:HI 0 "pic30_register_operand"               "=a,?r")
        (lshiftrt:HI (match_operand:HI 1 "pic30_data_operand" "U, T")
                     (match_operand:HI 2 "pic30_I_operand"    "I, I")))]
  ""
  "@
   lsr %1,WREG
   mov %1,%0\;lsr %0,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def")
  ]
)

(define_insn "*lshrhi3_sfr1"
  [(set (match_operand:HI 0 "pic30_near_operand"          "=U")
        (lshiftrt:HI (match_dup 0)
                     (match_operand:HI 1 "pic30_I_operand" "I")))]
  ""
  "lsr %0"
  [(set_attr "cc" "set")])

(define_insn "lshrhi3"
  [(set (match_operand:HI 0 "pic30_register_operand"                     "=r,r,r")
        (lshiftrt:HI (match_operand:HI 1 "pic30_register_operand"         "r,r,r")
                     (match_operand:HI 2 "pic30_reg_or_imm_operand" "K,i,r")))]
  ""
  "@
    lsr %1,#%2,%0
    clr %0
    lsr %1,%2,%0"
  [
   (set_attr "cc" "math,change0,math")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;
;; SImode ;;
;;;;;;;;;;;;

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "pic30_register_operand"              "")
        (lshiftrt:SI (match_operand:SI 1 "pic30_register_operand"  "")
                     (match_operand:HI 2 "pic30_reg_or_imm_operand" "")))]
  ""
  "
{
	if (GET_CODE(operands[2]) == CONST_INT)
	{
		switch (INTVAL(operands[2]))
		{
		case 0:
			emit_insn(gen_movsi(operands[0], operands[1]));
			break;
		case 1:
			emit_insn(gen_lshrsi3_imm1(operands[0],
						operands[1], operands[2]));
			break;
		case 2 ... 15:
			emit_insn(gen_lshrsi3_imm2to15(operands[0],
						operands[1], operands[2]));
			break;
		case 16:
			emit_insn(gen_lshrsi3_imm16plus(operands[0],
						operands[1], operands[2]));
			break;
		case 17 ... 31:
			emit_insn(gen_lshrsi3_imm16plus(operands[0],
						operands[1], operands[2]));
			break;
		default:
			emit_insn(gen_movsi(operands[0], const0_rtx));
			break;
		}
	}
	else
	{
		emit_insn(gen_lshrsi3_reg(operands[0],operands[1],operands[2]));
	}
	DONE;
}")

(define_insn "lshrsi3_imm1"
  [(set (match_operand:SI 0 "pic30_register_operand"             "=r")
        (lshiftrt:SI (match_operand:SI 1 "pic30_register_operand" "r")
                     (match_operand:HI 2 "pic30_I_operand"  "I")))]
  ""
  "
    lsr %d1,%d0\;rrc %1,%0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "lshrsi3_imm16plus"
  [(set (match_operand:SI 0            "pic30_register_operand"         "=r")
        (lshiftrt:SI (match_operand:SI 1 "pic30_register_operand"        "r")
                     (match_operand:HI 2 "pic30_imm16plus_operand" "i")))]
  ""
  "lsr %d1,#%K2,%0\;mov #0,%d0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "lshrsi3_imm2to15"
  [(set (match_operand:SI 0            "pic30_register_operand"        "=r")
        (lshiftrt:SI (match_operand:SI 1 "pic30_register_operand"       "r")
                     (match_operand:HI 2 "pic30_imm2to15_operand" "i")))
		     (clobber (match_scratch:HI 3               "=&r"))]
  ""
  "*
{
	/*
	** Take care that the source and dest don't overlap
	*/
	if (REGNO(operands[0]) <= REGNO(operands[1]))
	{
		return(	\"sl %d1,#%k2,%3\;\"
			\"lsr %1,#%2,%0\;\"
			\"ior %3,%0,%0\;\"
			\"lsr %d1,#%2,%d0\");
	}
	else
	{
		return(	\"lsr %d1,#%2,%d0\;\"
			\"sl %d1,#%k2,%3\;\"
			\"lsr %1,#%2,%0\;\"
			\"ior %3,%0,%0\");
	}
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "lshrsi3_reg"
  [(set (match_operand:SI 0 "pic30_register_operand"             "=r")
        (lshiftrt:SI (match_operand:SI 1 "pic30_register_operand" "0")
                     (match_operand:HI 2 "pic30_register_operand" "r")))
		     (clobber (match_scratch:HI 3          "=2"))]
  ""
  "*
{
      return( 
#if (1)
      	      \".set ___BP___,0\n\"
#endif
              \".LB%=:\;\"
              \"dec %2,%2\;\"
              \"bra n,.LE%=\;\"
              \"lsr %d1,%d0\;\"
              \"rrc %1,%0\;\"
              \"bra .LB%=\n\"
              \".LE%=:\");
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DImode arithmetic shift left.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "lshrdi3"
  [(set (match_operand:DI 0 "pic30_register_operand"              "")
        (lshiftrt:DI (match_operand:DI 1 "pic30_register_operand"  "")
                     (match_operand:HI 2 "pic30_reg_or_imm_operand" "")))]
  ""
  "
{
	if (GET_CODE(operands[2]) == CONST_INT)
	{
		switch (INTVAL(operands[2]))
		{
		case 0:
			emit_insn(gen_movdi(operands[0], operands[1]));
			break;
		case 1:
			emit_insn(gen_lshrdi3_imm1(operands[0],
						    operands[1], operands[2]));
			break;
		case 2 ... 15:
			emit_insn(gen_lshrdi3_immn(operands[0],
						   operands[1], operands[2]));
			break;
		case 16:
			emit_insn(gen_lshrdi3_imm16(operands[0],
						    operands[1], operands[2]));
			break;
		case 17 ... 31:
			emit_insn(gen_lshrdi3_immn(operands[0],
						   operands[1], operands[2]));
			break;
		case 32:
			emit_insn(gen_lshrdi3_imm32(operands[0],
						    operands[1], operands[2]));
			break;
		case 33 ... 47:
			emit_insn(gen_lshrdi3_immn(operands[0],
						   operands[1], operands[2]));
			break;
		case 48:
			emit_insn(gen_lshrdi3_imm48(operands[0],
						    operands[1], operands[2]));
			break;
		case 49 ... 63:
			emit_insn(gen_lshrdi3_immn(operands[0],
						   operands[1], operands[2]));
			break;
		default:
			emit_insn(gen_movdi(operands[0], const0_rtx));
			break;
		}
	}
	else
	{
		emit_insn(gen_lshrdi3_reg(operands[0],operands[1],operands[2]));
	}
	DONE;
}")

(define_insn "lshrdi3_imm1"
  [(set (match_operand:DI 0            "pic30_register_operand"   "=r")
        (lshiftrt:DI (match_operand:DI 1 "pic30_register_operand"  "r")
                     (match_operand:HI 2 "immediate_operand" "i")))]
  "INTVAL(operands[2])==1"
  "lsr %q1,%q0\;rrc %t1,%t0\;rrc %d1,%d0\;rrc %1,%0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "lshrdi3_imm16"
  [(set (match_operand:DI 0            "pic30_register_operand"   "=r")
        (lshiftrt:DI (match_operand:DI 1 "pic30_register_operand"  "r")
                     (match_operand:HI 2 "immediate_operand" "i")))]
  "INTVAL(operands[2])==16"
  "mov %d1,%0\;mov %t1,%d0\;mul.uu %q1,#1,%t0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "lshrdi3_imm32"
  [(set (match_operand:DI 0            "pic30_register_operand"   "=r")
        (lshiftrt:DI (match_operand:DI 1 "pic30_register_operand"  "r")
                     (match_operand:HI 2 "immediate_operand" "i")))]
  "INTVAL(operands[2])==32"
  "mov.d %t1,%0\;mul.uu %t0,#0,%t0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "lshrdi3_imm48"
  [(set (match_operand:DI 0            "pic30_register_operand"   "=r")
        (lshiftrt:DI (match_operand:DI 1 "pic30_register_operand"  "r")
                     (match_operand:HI 2 "immediate_operand" "i")))]
  "INTVAL(operands[2])==48"
  "mul.uu %q1,#1,%0\;mul.uu %t0,#0,%t0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "lshrdi3_immn"
  [(set (match_operand:DI 0 "pic30_register_operand"              "=r")
        (lshiftrt:DI (match_operand:DI 1 "pic30_register_operand"  "0")
                     (match_operand:HI 2 "immediate_operand" "i")))
		     (clobber (match_scratch:HI 3           "=&r"))]
  ""
  "*
{
      return( 
#if (1)
      	      \".set ___BP___,0\;\"
#endif
              \"mov #%2,%3\n\"
              \".LB%=:\;\"
              \"lsr %q1,%q0\;\"
              \"rrc %t1,%t0\;\"
              \"rrc %d1,%d0\;\"
              \"rrc %1,%0\;\"
              \"dec %3,%3\;\"
              \"bra nz,.LB%=\");
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "lshrdi3_reg"
  [(set (match_operand:DI 0 "pic30_register_operand"             "=r")
        (lshiftrt:DI (match_operand:DI 1 "pic30_register_operand" "0")
                     (match_operand:HI 2 "pic30_register_operand" "r")))
		     (clobber (match_scratch:HI 3          "=2"))]
  ""
  "*
{
      return( 
#if (1)
      	      \".set ___BP___,0\n\"
#endif
              \".LB%=:\;\"
              \"dec %2,%2\;\"
              \"bra n,.LE%=\;\"
              \"lsr %q1,%q0\;\"
              \"rrc %t1,%t0\;\"
              \"rrc %d1,%d0\;\"
              \"rrc %1,%0\;\"
              \"bra .LB%=\n\"
              \".LE%=:\");
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arithmetic shift right.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QImode arithmetic shift right.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "*ashrqi3_one"
  [(set (match_operand:QI 0 "pic30_mode2_operand"             "=r<>,r<>,R,R")
        (ashiftrt:QI (match_operand:QI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")
                   (match_operand:QI 2 "pic30_I_operand"       "I,  I,  I,I")))]
  ""
  "asr.b %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*ashrqi3_sfr"
  [(set (match_operand:QI 0 "pic30_wreg_or_near_operand"     "=a,U")
        (ashiftrt:QI (match_operand:QI 1 "pic30_near_operand" "U,0")
                   (match_operand:QI 2 "pic30_I_operand"      "I,I")))]
  ""
  "@
    asr.b %1,%0
    asr.b %0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,etc")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HImode arithmetic shift right.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "*ashrhi3_one"
  [(set (match_operand:HI 0 "pic30_mode2_operand"             "=r<>,r<>,R,R")
        (ashiftrt:HI (match_operand:HI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")
                     (match_operand:HI 2 "pic30_I_operand"     "I,  I,  I,I")))]
  ""
  "asr %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*ashrhi3_sfr"
  [(set (match_operand:HI 0 "pic30_wreg_or_near_operand"     "=a,U")
        (ashiftrt:HI (match_operand:HI 1 "pic30_near_operand" "U,0")
                     (match_operand:HI 2 "pic30_I_operand"    "I,I")))]
  ""
  "@
    asr %1,WREG
    asr %0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,etc")
  ]
)

(define_insn "ashrhi3"
  [(set (match_operand:HI 0 "pic30_register_operand"                     "=r,r,r")
        (ashiftrt:HI (match_operand:HI 1 "pic30_register_operand"         "r,r,r")
                     (match_operand:HI 2 "pic30_reg_or_imm_operand" "K,i,r")))]
  ""
  "*
{
	switch (which_alternative)
	{
	case 0:
    		return(\"asr %1,#%2,%0\");
	case 1:
		if (INTVAL(operands[2]) < 0)
    			return(\"sl %1,#%J2%%16,%0\");
		else
    			return(\"asr %1,#%2%%16,%0\");
	default:
    		return(\"asr %1,%2,%0\");
	}
}"
  [
   (set_attr "cc" "math")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SImode arithmetic shift right.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "pic30_register_operand"              "")
        (ashiftrt:SI (match_operand:SI 1 "pic30_register_operand"  "")
                     (match_operand:HI 2 "pic30_reg_or_imm_operand" "")))]
  ""
  "
{
	if (GET_CODE(operands[2]) == CONST_INT)
	{
		switch (INTVAL(operands[2]))
		{
		case 0:
			emit_insn(gen_movsi(operands[0], operands[1]));
			break;
		case 1:
			emit_insn(gen_ashrsi3_imm1(operands[0],
						operands[1], operands[2]));
			break;
		case 2 ... 15:
			emit_insn(gen_ashrsi3_imm2to15(operands[0],
						operands[1], operands[2]));
			break;
		case 16:
			emit_insn(gen_ashrsi3_imm16plus(operands[0],
						operands[1], operands[2]));
			break;
		case 17 ... 31:
			emit_insn(gen_ashrsi3_imm16plus(operands[0],
						operands[1], operands[2]));
			break;
		default:
			emit_insn(gen_movsi(operands[0], const0_rtx));
			break;
		}
	}
	else
	{
		emit_insn(gen_ashrsi3_reg(operands[0],operands[1],operands[2]));
	}
	DONE;
}")
(define_insn "ashrsi3_imm1"
  [(set (match_operand:SI 0 "pic30_register_operand"             "=r")
        (ashiftrt:SI (match_operand:SI 1 "pic30_register_operand" "r")
                     (match_operand:HI 2 "pic30_I_operand"  "I")))]
  ""
  "
    asr %d1,%d0\;rrc %1,%0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashrsi3_imm2to15"
  [(set (match_operand:SI 0            "pic30_register_operand"        "=r")
        (ashiftrt:SI (match_operand:SI 1 "pic30_register_operand"       "r")
                     (match_operand:HI 2 "pic30_imm2to15_operand" "i")))
		     (clobber (match_scratch:HI 3                "=&r"))]
  ""
  "*
{
	/*
	** Take care that the source and dest don't overlap
	*/
	if (REGNO(operands[0]) <= REGNO(operands[1]))
	{
		return(	\"sl %d1,#%k2,%3\;\"
			\"lsr %1,#%2,%0\;\"
			\"ior %3,%0,%0\;\"
			\"asr %d1,#%2,%d0\");
	}
	else
	{
		return(	\"asr %d1,#%2,%d0\;\"
			\"sl %d1,#%k2,%3\;\"
			\"lsr %1,#%2,%0\;\"
			\"ior %3,%0,%0\");
	}
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashrsi3_imm16plus"
  [(set (match_operand:SI 0            "pic30_register_operand"         "=r")
        (ashiftrt:SI (match_operand:SI 1 "pic30_register_operand"        "r")
                     (match_operand:HI 2 "pic30_imm16plus_operand" "i")))]
  ""
  "asr %d1,#%K2,%0\;asr %0,#15,%d0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashrsi3_reg"
  [(set (match_operand:SI 0 "pic30_register_operand"             "=r")
        (ashiftrt:SI (match_operand:SI 1 "pic30_register_operand" "0")
                     (match_operand:HI 2 "pic30_register_operand" "r")))
		     (clobber (match_scratch:HI 3          "=2"))]
  ""
  "*
{
      return( 
#if (1)
      	      \".set ___BP___,0\n\"
#endif
              \".LB%=:\;\"
              \"dec %2,%2\;\"
              \"bra n,.LE%=\;\"
              \"asr %d1,%d0\;\"
              \"rrc %1,%0\;\"
              \"bra .LB%=\n\"
              \".LE%=:\");
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DImode arithmetic shift right.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "ashrdi3"
  [(set (match_operand:DI 0 "pic30_register_operand"              "")
        (ashiftrt:DI (match_operand:DI 1 "pic30_register_operand"  "")
                     (match_operand:HI 2 "pic30_reg_or_imm_operand" "")))]
  ""
  "
{
	if (GET_CODE(operands[2]) == CONST_INT)
	{
		switch (INTVAL(operands[2]))
		{
		case 0:
			emit_insn(gen_movdi(operands[0], operands[1]));
			break;
		case 1:
			emit_insn(gen_ashrdi3_imm1(operands[0],
						    operands[1], operands[2]));
			break;
		case 2 ... 15:
			emit_insn(gen_ashrdi3_immn(operands[0],
						   operands[1], operands[2]));
			break;
		case 16:
			emit_insn(gen_ashrdi3_imm16(operands[0],
						    operands[1], operands[2]));
			break;
		case 17 ... 31:
			emit_insn(gen_ashrdi3_immn(operands[0],
						   operands[1], operands[2]));
			break;
		case 32:
			emit_insn(gen_ashrdi3_imm32(operands[0],
						    operands[1], operands[2]));
			break;
		case 33 ... 47:
			emit_insn(gen_ashrdi3_immn(operands[0],
						   operands[1], operands[2]));
			break;
		case 48:
			emit_insn(gen_ashrdi3_imm48(operands[0],
						    operands[1], operands[2]));
			break;
		case 49 ... 63:
			emit_insn(gen_ashrdi3_immn(operands[0],
						   operands[1], operands[2]));
			break;
		default:
			emit_insn(gen_movdi(operands[0], const0_rtx));
			break;
		}
	}
	else
	{
		emit_insn(gen_ashrdi3_reg(operands[0],operands[1],operands[2]));
	}
	DONE;
}")

(define_insn "ashrdi3_imm1"
  [(set (match_operand:DI 0            "pic30_register_operand"   "=r")
        (ashiftrt:DI (match_operand:DI 1 "pic30_register_operand"  "r")
                     (match_operand:HI 2 "immediate_operand" "i")))]
  "INTVAL(operands[2])==1"
  "asr %q1,%q0\;rrc %t1,%t0\;rrc %d1,%d0\;rrc %1,%0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashrdi3_imm16"
  [(set (match_operand:DI 0            "pic30_register_operand"   "=r")
        (ashiftrt:DI (match_operand:DI 1 "pic30_register_operand"  "r")
                     (match_operand:HI 2 "immediate_operand" "i")))]
  "INTVAL(operands[2])==16"
  "mov %d1,%0\;mov %t1,%d0\;mul.su %q1,#1,%t0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashrdi3_imm32"
  [(set (match_operand:DI 0            "pic30_register_operand"   "=r")
        (ashiftrt:DI (match_operand:DI 1 "pic30_register_operand"  "r")
                     (match_operand:HI 2 "immediate_operand" "i")))]
  "INTVAL(operands[2])==32"
  "mov.d %t1,%0\;asr %d1,#15,%t0\;mov %t0,%q0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashrdi3_imm48"
  [(set (match_operand:DI 0            "pic30_register_operand"   "=r")
        (ashiftrt:DI (match_operand:DI 1 "pic30_register_operand"  "r")
                     (match_operand:HI 2 "immediate_operand" "i")))]
  "INTVAL(operands[2])==48"
  "mul.su %q1,#1,%0\;mul.su %d0,#1,%t0"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashrdi3_immn"
  [(set (match_operand:DI 0 "pic30_register_operand"              "=r")
        (ashiftrt:DI (match_operand:DI 1 "pic30_register_operand"  "0")
                     (match_operand:HI 2 "immediate_operand" "i")))
		     (clobber (match_scratch:HI 3           "=&r"))]
  ""
  "*
{
      return( 
#if (1)
      	      \".set ___BP___,0\;\"
#endif
              \"mov #%2,%3\n\"
              \".LB%=:\;\"
              \"asr %q1,%q0\;\"
              \"rrc %t1,%t0\;\"
              \"rrc %d1,%d0\;\"
              \"rrc %1,%0\;\"
              \"dec %3,%3\;\"
              \"bra nz,.LB%=\");
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

(define_insn "ashrdi3_reg"
  [(set (match_operand:DI 0 "pic30_register_operand"             "=r")
        (ashiftrt:DI (match_operand:DI 1 "pic30_register_operand" "0")
                     (match_operand:HI 2 "pic30_register_operand" "r")))
		     (clobber (match_scratch:HI 3          "=2"))]
  ""
  "*
{
      return( 
#if (1)
      	      \".set ___BP___,0\n\"
#endif
              \".LB%=:\;\"
              \"dec %2,%2\;\"
              \"bra n,.LE%=\;\"
              \"asr %q1,%q0\;\"
              \"rrc %t1,%t0\;\"
              \"rrc %d1,%d0\;\"
              \"rrc %1,%0\;\"
              \"bra .LB%=\n\"
              \".LE%=:\");
}"
  [
   (set_attr "cc" "clobber")
   (set_attr "type" "def")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rotate instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rotate left
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; QImode ;;
;;;;;;;;;;;;

(define_insn "*rotlqi3_one"
  [(set (match_operand:QI 0 "pic30_mode2_operand"           "=r<>,r<>,R,R")
        (rotate:QI (match_operand:QI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")
                   (match_operand:QI 2 "pic30_I_operand"     "I,  I,  I,I")))]
  ""
  "rlnc.b %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*rotlqi3_sfr"
  [(set (match_operand:QI 0 "pic30_wreg_or_near_operand"   "=a,U")
        (rotate:QI (match_operand:QI 1 "pic30_near_operand" "U,0")
                   (match_operand:QI 2 "pic30_I_operand"    "I,I")))]
  ""
  "@
    rlnc.b %1,%0
    rlnc.b %1"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,etc")
  ]
)

;;;;;;;;;;;;
;; HImode ;;
;;;;;;;;;;;;

(define_insn "*rotlhi3_one"
  [(set (match_operand:HI 0 "pic30_mode2_operand"           "=r<>,r<>,R,R")
        (rotate:HI (match_operand:HI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")
                   (match_operand:HI 2 "pic30_I_operand"     "I,  I,  I,I")))]
  ""
  "rlnc %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*rotlhi3_sfr"
  [(set (match_operand:HI 0 "pic30_wreg_or_near_operand"   "=a,U")
        (rotate:HI (match_operand:HI 1 "pic30_near_operand" "U,0")
                   (match_operand:HI 2 "pic30_I_operand"    "I,I")))]
  ""
  "@
    rlnc %1,%0
    rlnc %1"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,etc")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rotate right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; QImode ;;
;;;;;;;;;;;;

(define_insn "*rotrqi3_one"
  [(set (match_operand:QI 0 "pic30_mode2_operand"             "=r<>,r<>,R,R")
        (rotatert:QI (match_operand:QI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")
                   (match_operand:QI 2 "pic30_I_operand"       "I,  I,  I,I")))]
  ""
  "rrnc.b %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*rotrqi3_sfr"
  [(set (match_operand:QI 0 "pic30_wreg_or_near_operand"     "=a,U")
        (rotatert:QI (match_operand:QI 1 "pic30_near_operand" "U,0")
                   (match_operand:QI 2 "pic30_I_operand"      "I,I")))]
  ""
  "@
    rrnc.b %1,%0
    rrnc.b %1"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,etc")
  ]
)

;;;;;;;;;;;;
;; HImode ;;
;;;;;;;;;;;;

(define_insn "*rotrhi3_one"
  [(set (match_operand:HI 0 "pic30_mode2_operand"             "=r<>,r<>,R,R")
        (rotatert:HI (match_operand:HI 1 "pic30_mode2_operand" "r,  R<>,r,R<>")
                     (match_operand:HI 2 "pic30_I_operand"     "I,  I,  I,I")))]
  ""
  "rrnc %1,%0"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,defuse,etc,use")
  ]
)

(define_insn "*rotrhi3_sfr"
  [(set (match_operand:HI 0 "pic30_wreg_or_near_operand"     "=a,U")
        (rotatert:HI (match_operand:HI 1 "pic30_near_operand" "U,0")
                   (match_operand:HI 2 "pic30_I_operand"      "I,I")))]
  ""
  "@
    rrnc %1,%0
    rrnc %1"
  [
   (set_attr "cc" "set")
   (set_attr "type" "def,etc")
  ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special cases of bit-field insns which we should
;; recognize in preference to the general case.
;; These handle aligned 8-bit and 16-bit fields,
;; which can usually be done with move instructions.
;    dsPIC30: t.b.d.
;********************

;; Bit field instructions, general cases.
;; "o,d" constraint causes a nonoffsettable memref to match the "o"
;; so that its address is reloaded.

;; (define_insn "extv" ...

;; (define_insn "extzv" ...

;; (define_insn "insv" ...

(define_expand "insv"
  [(set (zero_extract: HI (match_operand    0 "pic30_reg_or_near_operand" "+rU")
                          (match_operand:HI 1 "immediate_operand" "i")
                          (match_operand:HI 2 "immediate_operand" "i"))
        (match_operand:HI 3 "immediate_operand" "ii"))]
  ""
  "
{ int n;
  int mode;

  n = 4;
  mode = GET_MODE(operands[0]);
  if (pic30_volatile_operand(operands[0], mode)) n = 1;
  if ((INTVAL(operands[1]) <= n) && (GET_CODE(operands[3]) == CONST_INT) &&
      /* pic30_reg_or_near_operand is already called, but if optimization is
         on, constant addresses are forced into a register so that 'cse can
         get a chance to see them' (in fn memory_address)... thats okay, cse 
         will turn them back to constants we hope */
      (pic30_reg_or_near_operand(operands[0],VOIDmode)) && 
      ((mode == HImode) || (mode == QImode)))
  {  int mask;
     rtx mask_rtx;
     int i;
     rtx clobber = 0;
     rtx modifybit = operands[0];
     
     /* actually operand [3] does not necessarily fit into our bitfield...
        *SIGH* */
     
     for (i = 0; i < INTVAL(operands[1]); i++) {
       mask = 1 << i;  
       if ((INTVAL(operands[3]) & mask) == 0) {
          if (REG_P(operands[0]) || (GET_CODE(operands[0]) == SUBREG)) {
            mask =  ~(1 << (INTVAL(operands[2]) + i));
            mask_rtx = gen_rtx_CONST_INT(HImode, mask);
            emit(gen_bitclrhi(operands[0], operands[0], mask_rtx));
          } else {
            mask =  ~(1 << (INTVAL(operands[2]) +i));
            if (mode == HImode) {
              mask_rtx = gen_rtx_CONST_INT(HImode, mask);
              emit(gen_bitclrhi_sfr(operands[0], mask_rtx));
            } else if (mode == QImode) {
              if (mask > 0x80) {
                FAIL;
                break;
              }
              mask_rtx = gen_rtx_CONST_INT(HImode, mask);
              emit(gen_bitclrqi_sfr(modifybit, mask_rtx));
            }
          }
       } else {
          if (REG_P(operands[0]) || (GET_CODE(operands[0]) == SUBREG)) {
            mask =  (1 << (INTVAL(operands[2]) +i));
            mask_rtx = gen_rtx_CONST_INT(HImode, mask);
            emit(gen_bitsethi(operands[0], operands[0], mask_rtx));
          } else {
            mask =  (1 << (INTVAL(operands[2]) +i));
            if (mode == HImode) {
              mask_rtx = gen_rtx_CONST_INT(HImode, mask);
              emit(gen_bitsethi_sfr(operands[0], mask_rtx));
            } else {
              if (mask > 0x80) {
                FAIL;
                break;
              }
              mask_rtx = gen_rtx_CONST_INT(HImode, mask);
              emit(gen_bitsetqi_sfr(modifybit, mask_rtx));
            }
          }
       }
     }
     DONE;
  } else  {
    FAIL;
  }  

}
")

;; Now recognize bit field insns that operate on registers
;; (or at least were intended to do so).
;[unnamed only]

;; Special patterns for optimizing bit-field instructions.
;**************************************

; cc status test ops n.a. on dsPIC30 ......... e.g. "sleu" on 68k:
;  [(set (match_operand:QI 0 "general_operand" "=d")
;        (leu (cc0) (const_int 0)))]
;  ""
;  "* cc_status = cc_prev_status;
;     return \"sls %0\"; ")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic conditional jump instructions.
;; Every machine description must have a named pattern for each of the
;; conditional branch names "bcond"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "beq"
  [(set (pc)
        (if_then_else (eq (cc0)
                          (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "* return(pic30_conditional_branch(EQ, operands[0]));"
  [(set_attr "cc" "unchanged")])

(define_insn "bne"
  [(set (pc)
        (if_then_else (ne (cc0)
                          (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "* return(pic30_conditional_branch(NE, operands[0]));"
  [(set_attr "cc" "unchanged")])

(define_insn "bgt"
  [(set (pc)
        (if_then_else (gt (cc0)
                          (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "* return(pic30_conditional_branch(GT, operands[0]));"
  [(set_attr "cc" "unchanged")])

(define_insn "blt"
  [(set (pc)
        (if_then_else (lt (cc0)
                          (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "* return(pic30_conditional_branch(LT, operands[0]));"
  [(set_attr "cc" "unchanged")])

(define_insn "bge"
  [(set (pc)
        (if_then_else (ge (cc0)
                          (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "* return(pic30_conditional_branch(GE, operands[0]));"
  [(set_attr "cc" "unchanged")])

(define_insn "ble"
  [(set (pc)
        (if_then_else (le (cc0)
                          (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "* return(pic30_conditional_branch(LE, operands[0]));"
  [(set_attr "cc" "unchanged")])


; unsigned branches:

(define_insn "bgtu"
  [(set (pc)
        (if_then_else (gtu (cc0)
                          (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "* return(pic30_conditional_branch(GTU, operands[0]));"
  [(set_attr "cc" "unchanged")])

(define_insn "bltu"
  [(set (pc)
        (if_then_else (ltu (cc0)
                          (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "* return(pic30_conditional_branch(LTU, operands[0]));"
  [(set_attr "cc" "unchanged")])

(define_insn "bgeu"
  [(set (pc)
        (if_then_else (geu (cc0)
                          (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "* return(pic30_conditional_branch(GEU, operands[0]));"
  [(set_attr "cc" "unchanged")])

(define_insn "bleu"
  [(set (pc)
        (if_then_else (leu (cc0)
                          (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "* return(pic30_conditional_branch(LEU, operands[0]));"
  [(set_attr "cc" "unchanged")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Every machine description must have an anonymous pattern for each of
;; the possible reverse-conditional branches.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "*bccreverse"
  [(set (pc)
        (if_then_else (match_operator 1 "comparison_operator" 
			[(cc0) (const_int 0)])
                      (pc)
                      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
	return(pic30_conditional_branch(
		reverse_condition(GET_CODE(operands[1])), operands[0]));
}"
  [(set_attr "cc" "unchanged")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An instruction to jump to an address which is operand zero.
;; This pattern name is mandatory on all machines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "indirect_jump"
 [(set (pc) (match_operand:HI 0 "pic30_register_operand" "r"))]
 ""
 "goto %0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction to jump to a variable address. This is a low-level capability
;; which can be used to implement a dispatch table when there is no 'casesi'
;; pattern. This pattern requires two operands: the address or offset, and a
;; label which should immediately precede the jump table. If the macro
;; CASE_VECTOR_PC_RELATIVE evaluates to a nonzero value then the first operand
;; is an offset which counts from the address of the table; otherwise, it is
;; an absolute address to jump to. In either case, the first operand has mode
;; Pmode. The 'tablejump' insn is always the last insn before the jump table
;; it uses. Its assembler code normally has no need to use the second operand,
;; but you should incorporate it in the RTL pattern so that the jump optimizer
;; will not delete the table as unreachable code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define_insn "tablejump"
  [(set (pc) (match_operand:HI 0 "pic30_register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "bra %0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines of "casesi".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Operand 0 is index
;; operand 1 is the minimum bound
;; operand 2 is the maximum bound - minimum bound + 1
;; operand 3 is CODE_LABEL for the table;
;; operand 4 is the CODE_LABEL to go to if index out of range.

(define_expand "casesi"
  [(match_operand:SI 0 "general_operand" "")
   (match_operand:SI 1 "immediate_operand" "")
   (match_operand:SI 2 "immediate_operand" "")
   (match_operand 3 "" "")
   (match_operand 4 "" "")]
  ""
  "
{
  if (INTVAL(operands[2]) >= 32767) {
    error(\"Too many case statements in switch table\");
    FAIL;
  }
  if (operands[1] != const0_rtx) {
    rtx reg = gen_reg_rtx(SImode);

    operands[1] = GEN_INT(-INTVAL(operands[1]));
    operands[1] = force_reg(SImode, operands[1]);
    emit_insn(gen_addsi3(reg, operands[0], operands[1]));
    operands[0] = reg;
  }
  operands[2] = force_reg(SImode, operands[2]);
  emit_insn(gen_cmpsi(operands[0], operands[2]));
  emit_jump_insn(gen_bgtu(operands[4]));
  emit_jump_insn(gen_casesi0(operands[0], operands[3]));
  DONE;
}")

(define_insn "casesi0"
  [(set (pc) (plus:SI
     (mem:SI (plus:SI (pc)
               (match_operand:SI 0 "pic30_mode2_operand" "r,R")))
     (label_ref (match_operand 1 "" ""))))
     (clobber (match_scratch:HI 2  "=X,r"))
     (clobber (match_dup 0))]
  ""
  "*
{
  /*
  ** See if there is anything between us and the jump table
  ** If we could be sure there never was, then the 'clobber'
  ** of operand[0] could be removed.
  */
  register rtx p;
  int fDisjoint = FALSE;
  for (p = NEXT_INSN (insn); p != operands[1]; p = NEXT_INSN (p))
  {
    fDisjoint = TRUE;
    break;
  }
  if (fDisjoint)
  {
    
    if (which_alternative == 0) 
      return(\"add #(%1-$)/4,%0\;\"
             \"bra %0\");
    else 
      return(\"mov [%0], %2\;\"
             \"add #(%1-$)/4,%2\;\"
             \"bra %2\");
  }
  else
  {
    if (which_alternative == 0) 
      return(\"bra %0\");
    else
      return(\"mov %0, %2\;\"
             \"bra %2\");
  }
}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unconditional jump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
  "bra %0"
  [(set_attr "cc" "unchanged")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call subroutine, returning value in operand 0
;; (which must be a hard register).
;; Operand 1 is the function to call
;; Operand 2 is the number of bytes of arguments pushed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define_expand "call_value"
  [(set (match_operand 0 "pic30_register_operand"        "=r,r ,r")
        (call (match_operand:QI 1 "memory_operand"  "R,QS,m")
              (match_operand:HI 2 "general_operand" "")))]
  ;; Operand 2 not really used for dsPIC30.
  ""
  "
{  
     emit(gen_call_value_helper(operands[0], operands[1], operands[2]));
     DONE;
}
  ")

(define_insn "call_value_helper"
  [(set (match_operand 0 "pic30_register_operand"        "=r,r ,r")
        (call (match_operand:QI 1 "memory_operand"  "R,QS,m")
              (match_operand:HI 2 "general_operand" "")))]
  ;; Operand 2 not really used for dsPIC30.
  "(pic30_check_for_conversion(insn))"
  "*
	static char szInsn[48];

        pic30_clear_fn_list = 1;
        pic30_valid_call_address_operand(operands[0], Pmode);
	switch (which_alternative)
	{
	case 0:
		sprintf(szInsn, \"call %s\",
				reg_names[REGNO(XEXP(operands[1],0))]);
		break;
	case 1:
		/*
		** Casts of &(int x) to function ptrs, etc.
		*/
		error(\"invalid function call\");
	default:
		sprintf(szInsn, \"%s %%1\",
				pic30_near_function_p(operands[1])
						? \"rcall\" : \"call\");
		break;
	}
	return(szInsn);
")

;; Call subroutine with no return value.

(define_insn "*call_void"
  [(call (match_operand:QI 0 "memory_operand" "R,QS,m")
         (match_operand:HI 1 "general_operand" ""))]
  "(pic30_check_for_conversion(insn))"
  "*
{
	static char szInsn[48];

        pic30_clear_fn_list = 1;
        pic30_valid_call_address_operand(operands[0], Pmode);
	switch (which_alternative)
	{
	case 0:
		sprintf(szInsn, \"call %s\",
				reg_names[REGNO(XEXP(operands[0],0))]);
		break;
	case 1:
		/*
		** Casts of &(int x) to function ptrs, etc.
		*/
		error(\"invalid function call\");
	default:
		sprintf(szInsn, \"%s %%0\", pic30_near_function_p(operands[0])
						? \"rcall\" : \"call\");
		break;
	}
	return(szInsn);
}")

;; Call subroutine with no return value.
;; This handles intrinsics, such as bcopy.

(define_expand "call"
 [(call (match_operand:QI 0 "" "")
        (match_operand:HI 1 "" ""))]
 ""
 "
{
  if (GET_CODE (operands[0]) == MEM
      && ! pic30_call_address_operand (XEXP (operands[0], 0), Pmode))
    operands[0] = gen_rtx_MEM (GET_MODE (operands[0]),
			       force_reg (Pmode, XEXP (operands[0], 0)));
}")

(define_insn "*call"
  [(call (mem:QI (match_operand:QI 0 "pic30_call_address_operand" ""))
         (match_operand:HI 1 "general_operand" ""))]
  "(pic30_check_for_conversion(insn))"
  "*
  {
    pic30_clear_fn_list = 1;
    pic30_valid_call_address_operand(operands[0], Pmode);
    return (\"call %0\");
  }"
)

;;
;; return
;;
(define_insn "return"
  [(return)]
  "pic30_null_epilogue_p()"
  "*
{
	pic30_set_function_return(TRUE);

	return(\"return\");
}"
  [(set_attr "cc" "clobber")])

(define_insn "return_from_epilogue"
  [(return)]
  "!pic30_null_epilogue_p() && reload_completed"
  "*
  { extern tree current_function_decl;
    if (pic30_interrupt_function_p(current_function_decl))
    {
      return(\"retfie\");
    }
    else if (pic30_noreturn_function(current_function_decl))
    {
      return(\"reset\");
    }
    else 
    {
      return(\"return\");
    }
  }"
  [(set_attr "cc" "clobber")]
)

;;
;; link
;;
(define_insn "lnk"
  [(set (reg:HI SPREG)
        (match_operand 0 "immediate_operand" "i"))
   (clobber (reg:HI FPREG))
   (use (reg:HI FPREG))
   (use (reg:HI SPREG))
  ]
  "reload_completed"
  "lnk #%0"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def")
  ]
)

;;
;; unlink
;;
(define_insn "ulnk"
  [(set (reg:HI SPREG)
        (reg:HI FPREG))
   (clobber (reg:HI FPREG))]
  "reload_completed"
  "ulnk"
  [
   (set_attr "cc" "change0")
   (set_attr "type" "def")
  ]
)

;;
;; disi
;;
(define_insn "disi"
  [(unspec_volatile [(match_operand 0 "immediate_operand" "i")] UNSPECV_DISI)]
  ""
  "disi #%0")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prologue/epilogue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "prologue"
  [(const_int 1)]
  ""                           
  "
  {
  	pic30_expand_prologue();
	DONE;
  }"
)

(define_expand "epilogue"
  [(const_int 1)]
  ""
  "
  {
  	pic30_expand_epilogue();
	DONE;
  }"
)

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  "")

(define_insn "pa"
  [(unspec_volatile [(match_operand 0 "immediate_operand" "i")] UNSPECV_PA)]
  ""
  ".set ___PA___,%0")

(define_insn "iv"
  [(unspec_volatile [(match_operand 0 "immediate_operand" "i")] UNSPECV_IV)]
  ""
  {
static	char szInsn[96];
	char szVector[32];
	int nVectorID;
			
	nVectorID = INTVAL(operands[0]);
	if (nVectorID < 0)
	{
		nVectorID = -nVectorID-1;
		sprintf(szVector,"__AltInterrupt%d",nVectorID);
	}
	else
	{
		sprintf(szVector, "__Interrupt%d", nVectorID);
	}
	sprintf(szInsn, ".global\t%s\n%s:", szVector, szVector);

	return(szInsn);
  }
)
(define_insn "pp"
  [(unspec_volatile [(const_int 0)] UNSPECV_PP)]
  ""
  {
  	return(pic30_interrupt_preprologue());
  }
)

(define_insn "write_oscconl"
  [(unspec_volatile [(match_operand 0 "pic30_register_operand" "=&r")
                     (match_operand 1 "pic30_register_operand" "r")
                     (match_operand 2 "pic30_register_operand" "r")
                     (match_operand 3 "pic30_register_operand" "r")] 
    UNSPECV_WRITEOSCCONL)]
  ""
  "mov #_OSCCON,%0\;mov.b %1,[%0]\;mov.b %2,[%0]\;mov.b %3,[%0]"
)

(define_insn "write_oscconh"
  [(unspec_volatile [(match_operand 0 "pic30_register_operand" "=&r")
                     (match_operand 1 "pic30_register_operand" "r")
                     (match_operand 2 "pic30_register_operand" "r")
                     (match_operand 3 "pic30_register_operand" "r")] 
    UNSPECV_WRITEOSCCONH)]
  ""
  "mov #_OSCCON+1,%0\;mov.b %1,[%0]\;mov.b %2,[%0]\;mov.b %3,[%0]"
)

(define_insn "write_nvm"
  [(unspec_volatile [(match_operand 0 "pic30_wreg_operand" "=a")]
   UNSPECV_WRITEONVM)]
  ""
  "mov #0x55,%0\;mov %0,_NVMKEY\;mov #0xAA,%0\;mov %0,_NVMKEY\;bset _NVMCON,#15\;nop\;nop"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "bifnop"
  [(unspec_volatile [(const_int 0)] UNSPECV_NOP)]
  ""
  "nop"
  [(set_attr "cc" "unchanged")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "cc" "unchanged")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Peephole
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sequential assignments of 0 to a register
(define_peephole
  [
   (set (match_operand:HI 0 "pic30_register_operand" "")
        (const_int 0))
   (set (match_operand:HI 1 "pic30_register_operand" "")
        (const_int 0))
  ]
  "((REGNO(operands[0]) + 1 == REGNO(operands[1])) && 
    ((REGNO(operands[0]) & 1) == 0))"
  "mul.uu %0, #0, %0"
  [(set_attr "cc" "unchanged")]
)

(define_peephole
  [
   (set (match_operand:HI 0 "pic30_register_operand" "")
        (const_int 0))
   (set (match_operand:HI 1 "pic30_register_operand" "")
        (const_int 0))
  ]
  "((REGNO(operands[1]) + 1 == REGNO(operands[0])) && 
    ((REGNO(operands[1]) & 1) == 0))"
  "mul.uu %1, #0, %1"
  [(set_attr "cc" "unchanged")]
)


;;
;; 16-bit shift right SI followed by truncate to HI.
;; Simplify to most-significant subreg.
;; Unsigned shift.
;;
(define_peephole2
  [(set (match_operand:SI 0            "pic30_register_operand"    "")
        (lshiftrt:SI (match_operand:SI 1 "pic30_register_operand"  "")
                     (match_operand 2 "immediate_operand" "")))
   (set (match_operand:HI 3 "pic30_move_operand" "")
        (match_operand:HI 4 "pic30_register_operand" ""))
  ]
  "(INTVAL(operands[2]) == 16) &&
   (REGNO(operands[0]) == REGNO(operands[4])) &&
   (((REGNO(operands[0]) <= REGNO(operands[3])) && 
     (REGNO(operands[3]) < REGNO(operands[0]) + 
                    HARD_REGNO_NREGS(REGNO(operands[0]), GET_MODE (operands[0]))
     )) || peep2_reg_dead_p(2, operands[0]))"
  [(set (match_dup 3)
        (subreg:HI (match_dup 1) 2))]
  ""
  )

(define_peephole2
  [(set (match_operand:SI 0            "pic30_register_operand"    "")
        (lshiftrt:SI (match_operand:SI 1 "pic30_register_operand"  "")
                     (match_operand 2 "immediate_operand" "")))
   (set (match_operand:HI 3 "pic30_register_operand" "")
        (match_operator:HI 6 "pic30_valid_operator"
           [(match_operand:HI 4 "pic30_register_operand" "")
            (match_operand:HI 5 "" "")]))
  ]
  "(INTVAL(operands[2]) == 16) &&
   (REGNO(operands[0]) == REGNO(operands[4])) &&
   (((REGNO(operands[0]) <= REGNO(operands[3])) &&
     (REGNO(operands[3]) < REGNO(operands[0]) + 
                    HARD_REGNO_NREGS(REGNO(operands[0]), GET_MODE (operands[0]))     )) || peep2_reg_dead_p(2, operands[0]))"
  [ 
   (set (match_dup 4) (subreg:HI (match_dup 1) 2))
   (set (match_dup 3)
        (match_op_dup 6 [(match_dup 4) (match_dup 5)]))
  ]
  ""
  )

;; Ditto for signed shift.
(define_peephole2
  [(set (match_operand:SI 0            "pic30_register_operand"    "")
        (ashiftrt:SI (match_operand:SI 1 "pic30_register_operand"  "")
                     (match_operand 2 "immediate_operand" "")))
   (set (match_operand:HI 3 "pic30_move_operand" "")
        (match_operand:HI 4 "pic30_register_operand" ""))
  ]
  "(INTVAL(operands[2]) == 16) &&
   (REGNO(operands[0]) == REGNO(operands[4])) &&
   (((REGNO(operands[0]) <= REGNO(operands[3])) &&
     (REGNO(operands[3]) < REGNO(operands[0]) + 
                    HARD_REGNO_NREGS(REGNO(operands[0]), GET_MODE (operands[0]))     )) || peep2_reg_dead_p(2, operands[0]))"

  [(set (match_dup 3)
        (subreg:HI (match_dup 1) 2))]
  ""
  )

(define_peephole2
  [(set (match_operand:SI 0            "pic30_register_operand"    "")
        (ashiftrt:SI (match_operand:SI 1 "pic30_register_operand"  "")
                     (match_operand 2 "immediate_operand" "")))
   (set (match_operand:HI 3 "pic30_register_operand" "")
        (match_operator:HI 6 "pic30_valid_operator"
           [(match_operand:HI 4 "pic30_register_operand" "")
            (match_operand:HI 5 "" "")]))
  ]
  "(INTVAL(operands[2]) == 16) &&
   (REGNO(operands[0]) == REGNO(operands[4])) &&
   (((REGNO(operands[0]) <= REGNO(operands[3])) &&
     (REGNO(operands[3]) < REGNO(operands[0]) +
                    HARD_REGNO_NREGS(REGNO(operands[0]), GET_MODE (operands[0]))     )) || peep2_reg_dead_p(2, operands[0]))"
  [
   (set (match_dup 4) (subreg:HI (match_dup 1) 2))
   (set (match_dup 3)
        (match_op_dup 6 [(match_dup 4) (match_dup 5)]))
  ]
  ""
  )


;; Move [Base+Index] to [Base+Index] where Base or Index is common
(define_peephole
  [(set (match_operand:HI 0 "pic30_register_operand" "=r")
	(match_operand:HI 1 "pic30_mode3_operand" "RS<>r"))
   (set (match_operand:HI 2 "pic30_mode3_operand" "=RS<>r")
	(match_dup 0))
  ]
 "pic30_IndexEqual(operands[1], operands[2]) &&
	dead_or_set_p(insn, operands[0])"
 "mov %1,%2"
  [(set_attr "cc" "unchanged")])

(define_peephole
  [(set (match_operand:QI 0 "pic30_register_operand" "=r")
	(match_operand:QI 1 "pic30_mode3_operand" "RS<>r"))
   (set (match_operand:QI 2 "pic30_mode3_operand" "=RS<>r")
	(match_dup 0))
  ]
 "pic30_IndexEqual(operands[1], operands[2]) &&
	dead_or_set_p(insn, operands[0])"
 "mov.b %1,%2"
  [(set_attr "cc" "unchanged")])

;; add a,b,c;  mov [c], d => mov [a+b], d
(define_peephole2
  [(set (match_operand:HI 0 "pic30_register_operand" "")
        (plus:HI (match_operand:HI 1 "pic30_register_operand" "")
                 (match_operand:HI 2 "pic30_register_operand" "")))
   (set (match_operand:HI 3 "pic30_register_operand" "")
        (mem:HI (match_dup 0)))]
  "(peep2_reg_dead_p(2, operands[0]) || (REGNO(operands[0]) == REGNO(operands[3])))"
  [(set (match_dup 3)
        (mem:HI (plus:HI (match_dup 1) (match_dup 2))))]
)

(define_peephole
 [(set (match_operand:HI 0 "pic30_register_operand" "=r")
       (match_dup 0))]
 ""
 "; mov %0, %0"
)

(define_peephole
 [(set (match_operand:HI 0 "pic30_register_operand" "=r")
       (match_operand:HI 1 "pic30_move_operand" "RS<>rTQ"))
  (set (match_operand:HI 2 "pic30_register_operand" "=r")
       (match_dup 0))]
 "dead_or_set_p(insn, operands[0])"
 "mov %1, %2"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GCC often loads a function parameter into an arbitrary register,
;; then moves that register to one appropriate for the function call.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mov #addr,r0; mov r0,r1 becomes mov #addr,r1

(define_peephole
  [(set (match_operand:HI 0 "pic30_register_operand"              "=r")
        (match_operand:HI 1 "pic30_symbolic_address_operand" "g"))
   (set (match_operand:HI 2 "pic30_register_operand"              "=r")
        (match_dup 0))]
 "dead_or_set_p(insn, operands[0])"
  "*
{
	if (pic30_program_space_operand_p(operands[1]))
	{
		return(\"mov #handle(%1),%2\");
	}
	else
	{
		return(\"mov #%1,%2\");
	}
}"
  [(set_attr "cc" "clobber")])

;; mov.d [wn+k],r0; mov.d r0,r1 becomes mov.d [wn+k],r1

(define_peephole
  [(set (match_operand:SF 0 "pic30_register_operand" "=r")
        (match_operand:SF 1 "pic30_Q_operand"   "Q"))
   (set (match_operand:SF 2 "pic30_register_operand" "=r")
        (match_dup 0))
  ]
 "dead_or_set_p(insn, operands[0])"
 "*
{
	int idSrc, idDst;

	idDst = REGNO(operands[2]);
	idSrc = REGNO(XEXP(XEXP(operands[1],0),0));
	if (idDst == idSrc)
	{
		return \"mov %Q1,%d2\;mov %1,%2\";
	}
	else
	{
		return \"mov %1,%2\;mov %Q1,%d2\";
	}
}"
  [(set_attr "cc" "clobber")])

(define_peephole
  [(set (match_operand:SI 0 "pic30_register_operand" "=r")
        (match_operand:SI 1 "pic30_Q_operand"   "Q"))
   (set (match_operand:SI 2 "pic30_register_operand" "=r")
        (match_dup 0))
  ]
 "dead_or_set_p(insn, operands[0])"
 "*
{
	int idSrc, idDst;

	idDst = REGNO(operands[2]);
	idSrc = REGNO(XEXP(XEXP(operands[1],0),0));
	if (idDst == idSrc)
	{
		return \"mov %Q1,%d2\;mov %1,%2\";
	}
	else
	{
		return \"mov %1,%2\;mov %Q1,%d2\";
	}
}"
  [(set_attr "cc" "clobber")])

;; mov.d a,r0; mov.d r0,r1 becomes mov.d a,r1

(define_peephole
  [(set (match_operand:SF 0 "pic30_register_operand" "=r")
        (match_operand:SF 1 "pic30_T_operand"   "T"))
   (set (match_operand:SF 2 "pic30_register_operand" "=r")
        (match_dup 0))
  ]
 "dead_or_set_p(insn, operands[0])"
 "*
{
	return \"mov %1,%2\;mov %Q1,%d2\";
}"
  [(set_attr "cc" "clobber")])

(define_peephole
  [(set (match_operand:SI 0 "pic30_register_operand" "=r")
        (match_operand:SI 1 "pic30_T_operand"   "T"))
   (set (match_operand:SI 2 "pic30_register_operand" "=r")
        (match_dup 0))
  ]
 "dead_or_set_p(insn, operands[0])"
 "*
{
	return \"mov %1,%2\;mov %Q1,%d2\";
}"
  [(set_attr "cc" "clobber")])

;; mov.q #k,r0; mov.q r0,r1 becomes mov.q #k,r1

(define_peephole
  [(set (match_operand:DF 0 "pic30_register_operand" "=r,r")
        (match_operand:DF 1 "immediate_operand" "G,i"))
   (set (match_operand:DF 2 "pic30_register_operand" "=r,r")
        (match_dup 0))
  ]
 "dead_or_set_p(insn, operands[0])"
 "*
{
	REAL_VALUE_TYPE r;
	long l[4] = { 0 };

	switch (which_alternative)
	{
	case 0:
		return(	\"mul.uu %2,#0,%2\;\"
			\"mul.uu %t2,#0,%t2\");
	default:
		REAL_VALUE_FROM_CONST_DOUBLE(r, operands[1]);
		REAL_VALUE_TO_TARGET_DOUBLE(r, l);
		if (l[0] == 0)
		{
			return( \"mul.uu %0,#0,%2\;\"
				\"mov #%x1,%t2\;\"
				\"mov #%w1,%q2\");
		}
		else
		{
			return( \"mov #%z1,%2\;\"
				\"mov #%y1,%d2\;\"
				\"mov #%x1,%t2\;\"
				\"mov #%w1,%q2\");
		}
	}
}"
  [(set_attr "cc" "clobber")])

;; mov.q [wn+k],r0; mov.q r0,r1 becomes mov.q [wn+k],r1

(define_peephole
  [(set (match_operand:DF 0 "pic30_register_operand" "=r")
        (match_operand:DF 1 "pic30_Q_operand"   "Q"))
   (set (match_operand:DF 2 "pic30_register_operand" "=r")
        (match_dup 0))
  ]
 "dead_or_set_p(insn, operands[0])"
 "*
{
	int idSrc, idDst;
	char temp[48];
	char save[48];
static	char szInsn[48];

	szInsn[0] = 0;
	temp[0] = 0;
	save[0] = 0;

	idDst =	REGNO(operands[2]);
	idSrc =	REGNO(XEXP(XEXP(operands[1],0),0));
 	strcpy(temp, \"mov %1,%2\;\");
 	if (idDst != idSrc)
 		strcat(szInsn, temp);
 	else
 		strcat(save, temp);
 	idDst++;
 	strcpy(temp, \"mov %Q1,%d2\;\");
 	if (idDst != idSrc)
 		strcat(szInsn, temp);
 	else
 		strcat(save, temp);
 	idDst++;
 	strcpy(temp, \"mov %R1,%t2\;\");
 	if (idDst != idSrc)
 		strcat(szInsn, temp);
 	else
 		strcat(save, temp);
 	idDst++;
 	strcpy(temp, \"mov %S1,%q2\;\");
 	if (idDst != idSrc)
 		strcat(szInsn, temp);
 	else
 		strcat(save, temp);
 	idDst++;
 	strcat(szInsn, save);

	return(szInsn);
}"
  [(set_attr "cc" "clobber")])

;;
;; Substitute RETLW #k,Wn for MOV #k,Wn; RETURN
;;
(define_peephole
  [(set (match_operand:HI 0 "pic30_register_operand" "=r")
	(match_operand:HI 1 "pic30_J_operand"   "J"))
   (return)
  ]
 "pic30_null_epilogue_p()"
 "*
{
	pic30_set_function_return(TRUE);
	if (REGNO(operands[0]) == WR0_REGNO)
	{
		return(\"retlw #%1,%0\");
	}
	else
	{
		return(\"mov #%1,%0\;return\");
	}
}"
  [(set_attr "cc" "clobber")])

(define_peephole
  [(set (match_operand:SI 0 "pic30_register_operand" "=r")
	(match_operand:SI 1 "pic30_J_operand"   "J"))
   (return)
  ]
 "pic30_null_epilogue_p()"
 "*
{
	pic30_set_function_return(TRUE);
	if (REGNO(operands[0]) == WR0_REGNO)
	{
		return(\"mov #0,%d0\;retlw #%1,%0\");
	}
	else
	{
		return(\"mov #0,%d0\;mov #%1,%0\;return\");
	}
}"
  [(set_attr "cc" "clobber")])

;; Substitute bra/goto f for rcall/call f; ret

(define_peephole
  [(call (match_operand:QI 0 "memory_operand" "R,mp")
         (match_operand:HI 1 "general_operand" ""))
   (return)
  ]
  "pic30_null_epilogue_p()"
  "*
{
	pic30_set_function_return(TRUE);
	switch (which_alternative)
	{
	static char szInsn[48];
	case 0:
		sprintf(szInsn, \"goto %s\",
				reg_names[REGNO(XEXP(operands[0],0))]);
		return(szInsn);
	case 1:
		if (pic30_near_function_p(operands[0]))
			return(\"bra %0\");
		else
			return(\"goto %0\");
	default:
		return(\";\");
	}
}")

(define_peephole
  [(set (match_operand 0 "pic30_register_operand"         "r,r")
        (call (match_operand:QI 1 "memory_operand"  "R,mp")
              (match_operand:HI 2 "general_operand" "")))
   (return)
  ]
  "pic30_null_epilogue_p()"
  "*
{
	pic30_set_function_return(TRUE);
	switch (which_alternative)
	{
	static char szInsn[48];
	case 0:
		sprintf(szInsn, \"goto %s\",
				reg_names[REGNO(XEXP(operands[1],0))]);
		return(szInsn);
	case 1:
		if (pic30_near_function_p(operands[1]))
			return(\"bra %1\");
		else
			return(\"goto %1\");
	default:
		return(\";\");
	}
}")

;; Combine mov.w pairs to mov.d

(define_peephole
  [(set (match_operand:HI 0 "pic30_register_operand" "=r")
        (match_operand:HI 1 "pic30_register_operand"  "r"))
   (set (match_operand:HI 2 "pic30_register_operand" "=r")
        (match_operand:HI 3 "pic30_register_operand"  "r"))
  ]
  "pic30_registerpairs_p(operands[0],operands[2],operands[1],operands[3])"
  "*
{
	if (REGNO(operands[0]) < REGNO(operands[2]))
	{
		return(\"mov.d %1,%0\");
	}
	else
	{
		return(\"mov.d %3,%2\");
	}
}"
  [(set_attr "cc" "clobber")])
  
;; Combine mov.w [Wn],Wm; mov.w [Wn+2],Wm+1 to mov.d [Wn],Wm

(define_peephole
  [(set (match_operand:HI 0 "pic30_register_operand" "=r")
        (match_operand:HI 1 "pic30_R_operand"   "R"))
   (set (match_operand:HI 2 "pic30_register_operand" "=r")
        (match_operand:HI 3 "pic30_Q_operand"   "Q"))
  ]
  "(REGNO(operands[0]) == (REGNO(operands[2]) - 1)) &&
   (IS_EVEN_REG(REGNO(operands[0]))) &&
   (pic30_Q_base(operands[3]) == REGNO(operands[1])) &&
   (pic30_Q_displacement(operands[3]) == 2)"
  "mov.d %1,%0"
  [(set_attr "cc" "clobber")])

;; Zero-extend followed by shift

(define_peephole
  [(set (match_operand:SI 0 "pic30_register_operand"                  "=r")
        (zero_extend:SI (match_operand:QI 1 "pic30_register_operand"   "r")) )
   (set (match_dup 0)
        (ashift:SI (match_dup 0)
                   (match_operand:HI 2 "pic30_imm16plus_operand" "i")))
  ]
  ""
  "*
{
	int n = INTVAL(operands[2]);
	if (n == 16)
	{
		return(\"ze %1,%d0\;mov #0,%0\");
	}
	else if (n >= 24)
	{
		return(\"sl %1,#%K2,%d0\;mov #0,%0\");
	}
	else
	{
		return(\"ze %1,%0\;sl %0,#%K2,%d0\;mov #0,%0\");
	}
}"
  [(set_attr "cc" "clobber")])


(define_peephole2
  [(set (match_operand:HI 0 "pic30_register_operand" "")
        (match_operand:HI 1 "pic30_near_operand"  "") )
   (set (match_operand:SI 2 "pic30_wreg_operand" "")
        (mult:SI (zero_extend:SI (match_dup 0))
                 (zero_extend:SI (match_operand 3 "immediate_operand"   ""))))]
  "((INTVAL(operands[3]) == 2) && (peep2_reg_dead_p(2, operands[0]) || (REGNO(operands[0]) == REGNO(operands[2]))))"
  [(set (subreg:HI (match_dup 2) 0)
        (ashift:HI (match_dup 1)
                   (const_int 1)))]
  ""
  )

(define_peephole2
  [(set (match_operand:HI 0 "pic30_register_operand" "")
        (match_operand:HI 1 "pic30_near_operand"  "") )
   (set (match_operand:SI 2 "pic30_register_operand" "")
        (mult:SI (zero_extend:SI (match_dup 0))
                 (match_operand:SI 3 "immediate_operand"   "")))
   (set (match_operand:HI 4 "pic30_wreg_operand" "" )
        (match_operand:HI 5 "pic30_register_operand" ""))
]
  "((INTVAL(operands[3]) == 2) && (REGNO(operands[2]) == REGNO(operands[5])) &&     (peep2_reg_dead_p(3, operands[0]) || (REGNO(operands[0]) == REGNO(operands[4]))))"
  [(set (subreg:HI (match_dup 2) 0)
        (ashift:HI (match_dup 1)
                   (const_int 1)))]
  ""
  )

(define_peephole2
  [(match_scratch:HI 5 "a") 
   (set (match_operand:HI 0 "pic30_register_operand" "")
        (match_operand:HI 1 "pic30_near_operand"  ""))
   (set (match_operand:SI 2 "pic30_register_operand" "")
        (mult:SI (zero_extend:SI (match_dup 0))
                 (match_operand:SI 3 "immediate_operand"   "")))
   (set (match_operand:HI 6 "pic30_register_operand" "")
        (plus:HI (match_operand:HI 7 "pic30_register_operand" "")
                 (match_operand:HI 4 "pic30_math_operand" "")))]
  "((INTVAL(operands[3]) == 2) && (REGNO(operands[7]) == REGNO(operands[2])) &&     (peep2_reg_dead_p(3, operands[0]) ||                                              (REGNO(operands[0]) == REGNO(operands[6])) ||                                   (REGNO(operands[0]) == REGNO(operands[2]))) &&                                (peep2_reg_dead_p(3, operands[2]) ||                                              (REGNO(operands[2]) == REGNO(operands[6]))))"
  [(set (match_dup 5)
        (ashift:HI (match_dup 1)
                   (const_int 1)))
   (set (match_dup 6) 
        (plus:HI (match_dup 4) (match_dup 5)))]
  ""
  )

(define_peephole2
  [(match_scratch:HI 5 "a") 
   (set (match_operand:HI 0 "pic30_register_operand" "")
        (match_operand:HI 1 "pic30_near_operand"  ""))
   (set (match_operand:SI 2 "pic30_register_operand" "")
        (mult:SI (zero_extend:SI (match_dup 0))
                 (match_operand:SI 3 "immediate_operand"   "")))
   (set (match_operand:HI 6 "pic30_register_operand" "")
        (plus:HI (match_operand:HI 4 "pic30_math_operand" "")
                 (match_operand:HI 7 "pic30_register_operand" "")))]
  "((INTVAL(operands[3]) == 2) && (REGNO(operands[7]) == REGNO(operands[2])) &&     (peep2_reg_dead_p(3, operands[0]) ||                                              (REGNO(operands[0]) == REGNO(operands[6])) ||                                   (REGNO(operands[0]) == REGNO(operands[2]))) &&                                (peep2_reg_dead_p(3, operands[2]) ||                                              (REGNO(operands[2]) == REGNO(operands[6]))))"
  [(set (match_dup 5)
        (ashift:HI (match_dup 1)
                   (const_int 1)))
   (set (match_dup 6) 
        (plus:HI (match_dup 4) (match_dup 5)))]
  ""
  )

(define_peephole
  [(set (match_operand:SI 0 "pic30_register_operand"                "=r")
        (zero_extend:SI (match_operand:QI 1 "pic30_register_operand" "r")) )
   (set (match_dup 0)
        (ashift:SI (match_dup 0)
                   (match_operand:HI 2 "pic30_imm8_operand"    "i")))
  ]
  ""
  "*
{
	return(	\"sl %1,#%2,%0\;\"
		\"mov #0,%d0\");
}"
  [(set_attr "cc" "clobber")])

;; bit set optimizations

(define_peephole
  [(set (match_operand:SI 0 "pic30_register_operand" "=r")
	(match_operand:SI 1 "pic30_R_operand"   "R"))
   (set (match_dup 0)
        (ior:SI  (match_dup 0)
                 (match_operand:SI 2 "const_int_operand"   "i")))
   (set (match_dup 0)
        (ior:SI  (match_dup 0)
                 (match_operand:SI 3 "const_int_operand"   "i")))
   (set (match_dup 1)
	(match_dup 0))
  ]
 "dead_or_set_p(insn, operands[0]) &&
  pic30_one_bit_set_p(INTVAL(operands[2])) &&
  (pic30_which_bit(INTVAL(operands[2]))<16) &&
  pic30_one_bit_set_p(INTVAL(operands[3])) &&
  (pic30_which_bit(INTVAL(operands[2]))<16)"
 "bset %1,#%b2\;bset %1,#%b3"
  [(set_attr "cc" "clobber")])

;; Improve mov.b Wd,sfr

(define_peephole
  [(set (match_operand:QI 0 "pic30_near_operand" "=U,U")
	(match_operand:QI 1 "pic30_register_operand"    "d,a"))
   (set (match_operand:QI 2 "pic30_near_operand" "=U,U")
	(match_dup 1))
   (set (match_operand:QI 3 "pic30_near_operand" "=U,U")
	(match_dup 1))
   (set (match_operand:QI 4 "pic30_near_operand" "=U,U")
	(match_dup 1))
   (set (match_operand:QI 5 "pic30_near_operand" "=U,U")
	(match_dup 1))
  ]
 "!(pic30_errata_mask & exch_errata)"
 "@
  exch w0,%1\;mov.b WREG,%0\;mov.b WREG,%2\;mov.b WREG,%3\;mov.b WREG,%4\;mov.b WREG,%5\;exch w0,%1
  mov.b WREG,%0\;mov.b WREG,%2\;mov.b WREG,%3\;mov.b WREG,%4\;mov.b WREG,%5"
  [(set_attr "cc" "unchanged")])

(define_peephole
  [(set (match_operand:QI 0 "pic30_near_operand" "=U,U")
	(match_operand:QI 1 "pic30_register_operand"    "d,a"))
   (set (match_operand:QI 2 "pic30_near_operand" "=U,U")
	(match_dup 1))
   (set (match_operand:QI 3 "pic30_near_operand" "=U,U")
	(match_dup 1))
   (set (match_operand:QI 4 "pic30_near_operand" "=U,U")
	(match_dup 1))
  ]
 "!(pic30_errata_mask & exch_errata)"
 "@
  exch w0,%1\;mov.b WREG,%0\;mov.b WREG,%2\;mov.b WREG,%3\;mov.b WREG,%4\;exch w0,%1
  mov.b WREG,%0\;mov.b WREG,%2\;mov.b WREG,%3\;mov.b WREG,%4"
  [(set_attr "cc" "unchanged")])

(define_peephole
  [(set (match_operand:QI 0 "pic30_near_operand" "=U,U")
	(match_operand:QI 1 "pic30_register_operand"    "d,a"))
   (set (match_operand:QI 2 "pic30_near_operand" "=U,U")
	(match_dup 1))
   (set (match_operand:QI 3 "pic30_near_operand" "=U,U")
	(match_dup 1))
  ]
 "!(pic30_errata_mask & exch_errata)"
 "@
  exch w0,%1\;mov.b WREG,%0\;mov.b WREG,%2\;mov.b WREG,%3\;exch w0,%1
  mov.b WREG,%0\;mov.b WREG,%2\;mov.b WREG,%3"
  [(set_attr "cc" "unchanged")])

(define_peephole
  [(set (match_operand:QI 0 "pic30_near_operand" "=U,U")
	(match_operand:QI 1 "pic30_register_operand"    "d,a"))
   (set (match_operand:QI 2 "pic30_near_operand" "=U,U")
	(match_dup 1))
  ]
 "!(pic30_errata_mask & exch_errata)"
 "@
  exch w0,%1\;mov.b WREG,%0\;mov.b WREG,%2\;exch w0,%1
  mov.b WREG,%0\;mov.b WREG,%2"
  [(set_attr "cc" "unchanged")])

;
;  mov val, reg
;  mov reg, reg2
;
;  -> mov val, reg2
;
;  don't know why we need them, seems the register allocator should do better
;

(define_peephole
  [(set (match_operand:QI 0 "pic30_register_operand" "=r")
        (match_operand:QI 1 "immediate_operand" "i"))
   (set (match_operand:QI 2 "pic30_register_operand" "=r")
        (match_dup 0))]
  "dead_or_set_p(insn,operands[0])"
  "mov.b #%1,%2"
  [(set_attr "cc" "unchanged")])

(define_peephole
  [(set (match_operand:QI 0 "pic30_register_operand" "=r")
        (match_operand:QI 1 "pic30_near_operand" "U"))
   (set (match_operand:QI 2 "pic30_register_operand" "=r")
        (match_dup 0))]
  "dead_or_set_p(insn,operands[0])"
  "mov #%1,%2\;mov.b [%2],%2"
  [(set_attr "cc" "unchanged")])

(define_peephole
  [(set (match_operand:QI 0 "pic30_register_operand" "=r")
        (match_operand:QI 1 "general_operand" "g"))
   (set (match_operand:QI 2 "pic30_register_operand" "=r")
        (match_dup 0))]
  "dead_or_set_p(insn,operands[0])"
  "mov.b %1,%2"
  [(set_attr "cc" "unchanged")])
 
(define_peephole
  [(set (match_operand:HI 0 "pic30_register_operand" "=r")
        (match_operand:HI 1 "immediate_operand" "i"))
   (set (match_operand:HI 2 "pic30_register_operand" "=r")
        (match_dup 0))]
  "dead_or_set_p(insn,operands[0])"
  "mov.w #%1,%2"
  [(set_attr "cc" "unchanged")])

(define_peephole
  [(set (match_operand:HI 0 "pic30_register_operand" "=r")
        (match_operand:HI 1 "general_operand" "g"))
   (set (match_operand:HI 2 "pic30_register_operand" "=r")
        (match_dup 0))]
  "dead_or_set_p(insn,operands[0])"
  "mov.w %1,%2"
  [(set_attr "cc" "unchanged")])

(define_peephole
  [(set (match_operand:HI 0 "pic30_register_operand" "=r")
        (match_operand:HI 1 "general_operand" "g"))
   (set (match_dup 1)
        (match_dup 0))]
  "pic30_dead_or_set_p(NEXT_INSN(insn),operands[0])"
  "; move deleted"
  [(set_attr "cc" "unchanged")])

(define_peephole
  [(set (match_operand:QI 0 "pic30_register_operand" "=r")
        (match_operand:QI 1 "general_operand" "g"))
   (set (match_dup 1)
        (match_dup 0))]
  "pic30_dead_or_set_p(NEXT_INSN(insn),operands[0])"
  "; move deleted"
  [(set_attr "cc" "unchanged")])

(define_peephole
  [(set (match_operand:SI 0 "pic30_register_operand" "=r")
        (match_operand:SI 1 "general_operand" "g"))
   (set (match_dup 1)
        (match_dup 0))]
  "pic30_dead_or_set_p(NEXT_INSN(insn),operands[0])"
  "; move deleted"
  [(set_attr "cc" "unchanged")])

;
; some general const int optimizations
;

(define_peephole2
 [(set (match_operand:HI 0 "pic30_register_operand" "")
       (match_operand:HI 1 "immediate_operand" ""))
  (set (match_operand:HI 2 "pic30_near_operand" "")
       (match_dup 0))]
 "((INTVAL(operands[1]) == 255) && 
   (!pic30_volatile_operand(operands[2],HImode)) &&
   (pic30_dead_or_set_p(peep2_next_insn(2), operands[0])))"
 [(set (match_dup 2) (const_int 0))
  (set (subreg:QI (match_dup 2) 0) (const_int -1))]
 ""
)

(define_peephole2
 [(set (match_operand:HI 0 "pic30_register_operand" "")
       (match_operand:HI 1 "immediate_operand" ""))
  (set (match_operand:HI 2 "pic30_near_operand" "")
       (match_dup 0))]
 "((INTVAL(operands[1]) == -256) && 
   (!pic30_volatile_operand(operands[2],HImode)) &&
   (pic30_dead_or_set_p(peep2_next_insn(2), operands[0])))"
 [(set (match_dup 2) (const_int 0))
  (set (subreg:QI (match_dup 2) 1) (const_int -1))]
 ""
)

;
; add become subtract
;
(define_peephole2
 [(set (match_operand:HI 0 "pic30_register_operand" "")
       (match_operand:HI 1 "immediate_operand" ""))
  (set (match_operand:HI 2 "pic30_register_operand" "")
       (plus:HI (match_dup 2)
                (match_dup 0)))]
 "((INTVAL(operands[1]) <= 0) && (INTVAL(operands[1]) >= -1023) && pic30_dead_or_set_p(peep2_next_insn(2),operands[0]))"
 [ (set (match_dup 2)
        (minus:HI (match_dup 2) (match_dup 1)))]
 "{ operands[1] = gen_rtx_CONST_INT(HImode,-INTVAL(operands[1])); }"
)

(define_peephole2
 [(set (match_operand:HI 0 "pic30_register_operand" "")
       (match_operand:HI 1 "immediate_operand" ""))
  (set (match_operand:HI 2 "pic30_register_operand" "")
       (plus:HI (match_operand:HI 3 "pic30_register_operand" "")
                (match_dup 0)))]
 "((INTVAL(operands[1]) <= 0) && (INTVAL(operands[1]) >= -31) && pic30_dead_or_set_p(peep2_next_insn(2),operands[0]))"
 [ (set (match_dup 2)
        (minus:HI (match_dup 3) (match_dup 1)))]
 "{ operands[1] = gen_rtx_CONST_INT(HImode,-INTVAL(operands[1])); }"
)

;
; combining two HI's into an SI
;

(define_peephole2
 [(set (match_operand:SI 0 "pic30_register_operand" "")
       (zero_extend:SI (match_operand:HI 1 "pic30_register_operand" "")))
  (set (match_operand:SI 2 "pic30_register_operand" "")
       (ashift:SI (match_dup 0) (const_int 16)))
  (set (match_operand:SI 3 "pic30_register_operand" "")
       (zero_extend:SI (match_operand:HI 4 "pic30_register_operand" "")))
  (set (match_operand:SI 5 "pic30_register_operand" "")
       (ior:SI (match_dup 2)
               (match_dup 3)))]
 "(REGNO(operands[5]) != REGNO(operands[1]))"
 [(set (match_dup 5) (match_dup 4))
  (set (match_dup 0) (match_dup 1))]
 "{ int reg = REGNO(operands[5]);
    operands[5] = gen_rtx_REG(HImode,reg);
    operands[0] = gen_rtx_REG(HImode,reg+1); }"
)

; [(set (subreg:HI (match_dup 5) 0) (match_dup 4))
;  (set (subreg:HI (match_dup 5) 2) (match_dup 1))]

(define_peephole2
 [(set (match_operand:SI 0 "pic30_register_operand" "")
       (zero_extend:SI (match_operand:HI 1 "pic30_register_operand" "")))
  (set (match_operand:SI 2 "pic30_register_operand" "")
       (ashift:SI (match_dup 0) (const_int 16)))
  (set (match_operand:SI 3 "pic30_register_operand" "")
       (zero_extend:SI (match_operand:HI 4 "pic30_register_operand" "")))
  (set (match_operand:SI 5 "pic30_register_operand" "")
       (ior:SI (match_dup 2)
               (match_dup 3)))]
 "((REGNO(operands[5])+1) != REGNO(operands[4]))"
 [(set (match_dup 5) (match_dup 1))
  (set (match_dup 0) (match_dup 4))]
 "{ int reg = REGNO(operands[5]);
    operands[5] = gen_rtx_REG(HImode,reg+1);
    operands[0] = gen_rtx_REG(HImode,reg); }"
)

(define_peephole2
 [(set (match_operand:SI 0 "pic30_register_operand" "")
       (zero_extend:SI (match_operand:HI 1 "pic30_register_operand" "")))
  (set (match_operand:SI 2 "pic30_register_operand" "")
       (ashift:SI (match_dup 0) (const_int 16)))
  (set (match_operand:SI 3 "pic30_register_operand" "")
       (zero_extend:SI (match_operand:HI 4 "pic30_register_operand" "")))
  (set (match_operand:SI 5 "pic30_register_operand" "")
       (ior:SI (match_dup 2)
               (match_dup 3)))]
 "(REGNO(operands[0])+1 != REGNO(operands[4]))"
 [
  (set (match_dup 2) (match_dup 1))
  (set (match_dup 3) (match_dup 4))
  (set (match_dup 5) (match_dup 0))
 ] 
 "{ int reg = REGNO(operands[0]);
    operands[2] = gen_rtx_REG(HImode, reg+1);
    operands[3] = gen_rtx_REG(HImode, reg);  }"
)

(define_peephole2
 [(set (match_operand:SI 0 "pic30_register_operand" "")
       (match_operand:SI 1 "pic30_move_operand" ""))
  (set (match_operand:SI 2 "pic30_register_operand" "")
       (match_dup 0))]
 "peep2_reg_dead_p(2, operands[0])"
 [(set (match_dup 2) (match_dup 1))]
)

(define_peephole2
 [(set (match_operand:SI 0 "pic30_register_operand" "")
       (match_operand:SI 1 "pic30_register_operand" ""))
  (set (match_operand:SI 2 "pic30_move_operand" "")
       (match_dup 0))]
 "peep2_reg_dead_p(2, operands[0])"
 [(set (match_dup 2) (match_dup 1))]
)

(define_peephole2
 [(set (match_operand:SI 0 "pic30_move_operand" "")
       (match_operand:SI 1 "pic30_register_operand" ""))
  (set (match_operand:SI 2 "pic30_register_operand" "")
       (match_dup 0))]
 "peep2_reg_dead_p(2, operands[1])"
 [(set (match_dup 0) (match_dup 1))
  (set (match_dup 2) (match_dup 1))]
)



; [(set (subreg:HI (match_dup 5) 2) (match_dup 1))
;  (set (subreg:HI (match_dup 5) 0) (match_dup 4))]

;; (define_peephole
;;   [(set (match_operand:SI 0 "pic30_register_operand"                "=r")
;;         (sign_extend:SI (match_operand:HI 1 "pic30_register_operand" "r")) )
;;    (set (match_operand:SF 2 "pic30_register_operand" "=r")
;;         (call (match_operand:QI 3 "memory_operand"  "m")
;;               (match_operand:HI 4 "general_operand" "g")))
;;   ]
;;   "isfloatsisf(operands[3])"
;;   "*
;; {
;; 	if (TARGET_SMALL_CODE)
;; 		return(\"rcall __floathisf\");
;; 	else
;; 		return(\"call __floathisf\");
;; }")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;End.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
