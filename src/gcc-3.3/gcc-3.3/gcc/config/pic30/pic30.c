/* Subroutines for insn output for Microchip dsPIC30.
   Copyright (C) 1994, 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
   Contributed by John Elliott (john.elliott@microchip.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    This is the helper module for the Microchip dsPIC30 port.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#include "config.h"
#include "system.h"
#include "toplev.h"
#include "rtl.h"
#include "insn-config.h"
#include "regs.h"
#include "tree.h"
#include "c-tree.h"
#include "function.h"
#include "expr.h"
#include "recog.h"
#if !defined(HAVE_cc0)
#define HAVE_cc0
#endif
#include "conditions.h"
#include "real.h"
#include "hard-reg-set.h"
#include "insn-attr.h"
#include "output.h"
#include "flags.h"
/* #include "ggc.h" */
#include "c-pragma.h"
/* #include "c-lex.h"  this stuff has been moved to c-praghma.h */
#include "cpplib.h"
#include "pic30-protos.h"
#include "target.h"
#include "target-def.h"
#include "insn-codes.h"
#include "basic-block.h"
#include "version.h"
#include "../../../../../c30_resource/src/c30/resource_info.h"

#define    NELEMENTS(a)    (sizeof(a)/sizeof(a[0]))
/*----------------------------------------------------------------------*/
/*    G L O B A L    V A R I A B L E S                                  */
/*----------------------------------------------------------------------*/
int pic30_compiler_version = 0;
const char *    pic30_text_scn = NULL;      /* -mtext=name cmd-line option  */
const char *    pic30_pa_level = NULL;      /* -mpa=level cmd-line option   */
const char *    pic30_target_family = NULL;
const char *    pic30_target_cpu = NULL;    /* -mcpu=name cmd-line option   */
const char *    pic30_io_size = NULL;       /* -msmart-io= cmd-line option  */
const char *    pic30_errata = NULL;        /* -merrata= cmd-line option    */
      char *    pic30_resource_file = NULL; 
int             pic30_io_size_val = 0;
const char *    pic30_it_option = NULL;
const char *    pic30_it_option_arg = NULL;
rtx    rtxCmpOperands[2] = { NULL_RTX, NULL_RTX };
/*----------------------------------------------------------------------*/
/*    L O C A L    V A R I A B L E S                                    */
/*----------------------------------------------------------------------*/
#define SECTION_FLAG_EXEC       "x"
#define SECTION_FLAG_DATA       "d"
#define SECTION_FLAG_BSS        "b"
#define SECTION_FLAG_READONLY   "r"

#define SECTION_NAME_BSS        ".bss"
#define SECTION_NAME_NBSS       ".nbss"
#define SECTION_NAME_DATA       ".data"
#define SECTION_NAME_NDATA      ".ndata"
#define SECTION_NAME_DCONST     ".dconst"
#define SECTION_NAME_NDCONST    ".ndconst"
#define SECTION_NAME_CONST      ".const"

                                /* 0x100000 */
#define JOIN(X,Y) (X ## Y)
#define PIC30_LL(X) JOIN(X,LL)

#define SECTION_READ_ONLY       (PIC30_LL(SECTION_MACH_DEP))
#define SECTION_XMEMORY         (PIC30_LL(SECTION_MACH_DEP) << 1)
#define SECTION_YMEMORY         (PIC30_LL(SECTION_MACH_DEP) << 2)
#define SECTION_NEAR            (PIC30_LL(SECTION_MACH_DEP) << 3)
#define SECTION_PERSIST         (PIC30_LL(SECTION_MACH_DEP) << 4)
#define SECTION_PSV             (PIC30_LL(SECTION_MACH_DEP) << 5)
#define SECTION_EEDATA          (PIC30_LL(SECTION_MACH_DEP) << 6)
#define SECTION_NOLOAD          (PIC30_LL(SECTION_MACH_DEP) << 7)
#define SECTION_REVERSE         (PIC30_LL(SECTION_MACH_DEP) << 8)
#define SECTION_INFO            (PIC30_LL(SECTION_MACH_DEP) << 9)
#define SECTION_ADDRESS         (PIC30_LL(SECTION_MACH_DEP) << 10)
#define SECTION_ALIGN           (PIC30_LL(SECTION_MACH_DEP) << 11)
#define SECTION_DMA             (PIC30_LL(SECTION_MACH_DEP) << 12)

/* the attribute names from the assemblers point of view */
#define SECTION_ATTR_ADDRESS "address"
#define SECTION_ATTR_ALIGN   "align"
#define SECTION_ATTR_BSS     "bss"
#define SECTION_ATTR_CODE    "code"
#define SECTION_ATTR_CONST   "psv"
#define SECTION_ATTR_DATA    "data"
#define SECTION_ATTR_DMA     "dma"
#define SECTION_ATTR_EEDATA  "eedata"
#define SECTION_ATTR_INFO    "info"
#define SECTION_ATTR_MERGE   "merge"
#define SECTION_ATTR_NEAR    "near"
#define SECTION_ATTR_NOLOAD  "noload"
#define SECTION_ATTR_PERSIST "persist"
#define SECTION_ATTR_PSV     "psv"
#define SECTION_ATTR_REVERSE "reverse"
#define SECTION_ATTR_XMEMORY "xmemory"
#define SECTION_ATTR_YMEMORY "ymemory"

struct valid_section_flags_ {
  const char *flag_name;
  char single_letter_equiv;
  SECTION_FLAGS_INT flag_mask;
  SECTION_FLAGS_INT incompatable_with;
} valid_section_flags[] = {
  { SECTION_ATTR_ADDRESS, 0, 
              SECTION_ADDRESS, SECTION_REVERSE | SECTION_ALIGN | SECTION_INFO },
  { SECTION_ATTR_ALIGN, 0, 
              SECTION_ALIGN,   SECTION_ADDRESS | SECTION_REVERSE |
                               SECTION_INFO },
  { SECTION_ATTR_BSS, 'b', 
              SECTION_BSS,     SECTION_CODE | SECTION_WRITE | SECTION_PERSIST |
                               SECTION_PSV | SECTION_READ_ONLY | 
                               SECTION_EEDATA },
  { SECTION_ATTR_CODE, 'x', 
             SECTION_CODE,     SECTION_WRITE | SECTION_XMEMORY | SECTION_BSS |
                               SECTION_YMEMORY | SECTION_NEAR | SECTION_PSV |
                               SECTION_PERSIST | SECTION_EEDATA | 
                               SECTION_READ_ONLY },
  { SECTION_ATTR_CONST, 'r', 
             SECTION_READ_ONLY,SECTION_CODE | SECTION_WRITE | SECTION_BSS |
                               SECTION_EEDATA | SECTION_NEAR | SECTION_XMEMORY |
                               SECTION_YMEMORY | SECTION_INFO | SECTION_PSV },
  { SECTION_ATTR_DATA, 'd', 
             SECTION_WRITE,    SECTION_BSS | SECTION_PSV | SECTION_PERSIST |
                               SECTION_EEDATA | SECTION_READ_ONLY },
  { SECTION_ATTR_DMA, 0 , 
             SECTION_DMA,      SECTION_PSV | SECTION_INFO |
                               SECTION_EEDATA | SECTION_READ_ONLY | 
                               SECTION_XMEMORY | SECTION_YMEMORY | 
                               SECTION_NEAR },
  { SECTION_ATTR_EEDATA, 0, 
             SECTION_EEDATA,   SECTION_CODE | SECTION_WRITE | SECTION_BSS |
                               SECTION_PSV | SECTION_NEAR | SECTION_XMEMORY |
                               SECTION_YMEMORY | SECTION_INFO | 
                               SECTION_READ_ONLY },
  { SECTION_ATTR_INFO, 0, 
             SECTION_INFO,     SECTION_PERSIST | SECTION_PSV | SECTION_EEDATA |
                               SECTION_ADDRESS | SECTION_NEAR | 
                               SECTION_XMEMORY | SECTION_YMEMORY | 
                               SECTION_REVERSE | SECTION_ALIGN | 
                               SECTION_NOLOAD | SECTION_MERGE | 
                               SECTION_READ_ONLY },
  { SECTION_ATTR_MERGE, 0, 
             SECTION_MERGE,    SECTION_BSS | SECTION_PERSIST | SECTION_INFO },
  { SECTION_ATTR_NEAR, 0, 
              SECTION_NEAR,    SECTION_CODE | SECTION_PSV | SECTION_EEDATA |
                               SECTION_INFO | SECTION_READ_ONLY },
  { SECTION_ATTR_NOLOAD, 0, 
             SECTION_NOLOAD,   SECTION_MERGE | SECTION_INFO },
  { SECTION_ATTR_PERSIST, 'b', 
             SECTION_PERSIST,  SECTION_CODE | SECTION_WRITE | SECTION_BSS |
                               SECTION_PSV | SECTION_EEDATA | SECTION_MERGE |
                               SECTION_INFO | SECTION_READ_ONLY },
  { SECTION_ATTR_PSV, 0, 
             SECTION_PSV,      SECTION_CODE | SECTION_WRITE | SECTION_BSS |
                               SECTION_EEDATA | SECTION_NEAR | SECTION_XMEMORY |
                               SECTION_YMEMORY | SECTION_INFO | 
                               SECTION_READ_ONLY },
  { SECTION_ATTR_REVERSE, 0,
             SECTION_REVERSE,  SECTION_CODE | SECTION_ADDRESS | SECTION_ALIGN |
                               SECTION_INFO },
  { SECTION_ATTR_XMEMORY, 0, 
             SECTION_XMEMORY,  SECTION_CODE | SECTION_PSV | SECTION_EEDATA |
                               SECTION_YMEMORY | SECTION_INFO |
                               SECTION_READ_ONLY },
  { SECTION_ATTR_YMEMORY, 0, 
             SECTION_YMEMORY,  SECTION_CODE | SECTION_PSV | SECTION_EEDATA |
                               SECTION_XMEMORY | SECTION_INFO |
                               SECTION_READ_ONLY },
  { 0, 0, 0, 0},
};

struct reserved_section_names_ {
  const char *section_name;
  SECTION_FLAGS_INT mask;
} reserved_section_names[] = {
  { ".bss",    SECTION_BSS },
  { ".const",  SECTION_READ_ONLY },
  { ".data",   SECTION_WRITE },
  { ".dconst", SECTION_WRITE },
  { ".eedata", SECTION_EEDATA },
  { ".nbss",   SECTION_BSS | SECTION_NEAR },
  { ".ndata",  SECTION_WRITE | SECTION_NEAR },
  { ".ndconst",SECTION_WRITE | SECTION_NEAR },
  { ".pbss",   SECTION_PERSIST },
  { ".text",   SECTION_CODE },
  { ".xbss",   SECTION_BSS | SECTION_XMEMORY },
  { ".xdata",  SECTION_WRITE | SECTION_XMEMORY },
  { ".ybss",   SECTION_BSS | SECTION_YMEMORY },
  { ".ydata",  SECTION_WRITE | SECTION_YMEMORY },
  { 0, 0},
};

#define MAKE_FRAME_RELATED 0

static const char *pic30_default_section = "*";

typedef struct tagSFR
{
    struct tagSFR    *pNext;
    const char    *pName;
    int        address;
}
    SFR, *PSFR;
static PSFR lpSFRs = NULL;

static int pic30_smart_io_warning = 0;
static int lbFunctionHasReturn = FALSE;
static int lfInExecutableSection = FALSE;
extern int flag_gcse, flag_rerun_cse_after_loop, 
           flag_delete_null_pointer_checks;

enum {
  ss_pushed = 0,     /* section stack was pushed */
  ss_set = 1,        /* section stack was set */
  ss_should_pop = 2  /* popped, but wait til we see what is pushed next */
};

typedef struct sectionStack_ {
  const char * pszName;
  unsigned int pszFlag;
  struct sectionStack_ *pop;
  SECTION_FLAGS_INT flags; 
} sectionStack;

static sectionStack default_section = {
  ".text", SECTION_CODE , 0x0, 0x0
};

static sectionStack *lSectionStack = &default_section;
static sectionStack *freeSectionStack;

static tree lTreeInterrupt = NULL_TREE;        /* #pragma interrupt    */
static tree lTreeShadow = NULL_TREE;        /* #pragma shadow    */
static tree lTreeTextScnName = NULL_TREE;    /* #pragma code        */
static tree lTreeIDataScnName = NULL_TREE;    /* #pragma idata    */
static tree lTreeUDataScnName = NULL_TREE;    /* #pragma udata    */

#define    SAVE_SWORD    1    /* single word */
#define    SAVE_DWORD    2    /* double word */
#define    SAVE_QWORD    4    /* quad word */
static int    lCalleeSaveRegs[SP_REGNO];
static unsigned int l_RAWregdefmask = 0;

#define IDENT_INTERRUPT(t) \
    ((t)==pic30_identInterrupt[0]||(t)==pic30_identInterrupt[1])
#define IDENT_SHADOW(t) \
    ((t) == pic30_identShadow[0] || (t) == pic30_identShadow[1])
#define IDENT_IRQ(t) \
    ((t) == pic30_identIRQ[0] || (t) == pic30_identIRQ[1])
#define IDENT_ALTIRQ(t) \
    ((t) == pic30_identAltIRQ[0] || (t) == pic30_identAltIRQ[1])
#define IDENT_SAVE(t) \
    ((t) == pic30_identSave[0] || (t) == pic30_identSave[1])
#define IDENT_PREPROLOGUE(t) \
        ((t) == pic30_identPreprologue[0] || (t) == pic30_identPreprologue[1])
#define IDENT_SFR(t) \
    (((t)==pic30_identSFR[0]) || ((t)==pic30_identSFR[1]))
#define IDENT_NEAR(t) \
    ((t)==pic30_identNear[0] || (t)==pic30_identNear[1])
#define IDENT_FAR(t) \
    ((t)==pic30_identFar[0] || (t)==pic30_identFar[1])
#define IDENT_SPACE(t) \
    ((t) == pic30_identSpace[0] || (t) == pic30_identSpace[1])
#define IDENT_PROG(t) \
    ((t) == pic30_identProg[0] || (t) == pic30_identProg[1])
#define IDENT_DATA(t) \
    ((t) == pic30_identData[0] || (t) == pic30_identData[1])
#if 0
#define IDENT_XXX(t) \
   ((t) == pic30_identYYY[0] || (t) == pic30_identYYY[1])
#endif
#define IDENT_XMEMORY(t) \
   ((t) == pic30_identXmemory[0] || (t) == pic30_identXmemory[1])
#define IDENT_YMEMORY(t) \
   ((t) == pic30_identYmemory[0] || (t) == pic30_identYmemory[1])
#define IDENT_PSV(t) \
   ((t) == pic30_identPsv[0] || (t) == pic30_identPsv[1])
#define IDENT_EEDATA(t) \
   ((t) == pic30_identEedata[0] || (t) == pic30_identEedata[1])
#define IDENT_CONST(t) \
   ((t) == pic30_identConst[0] || (t) == pic30_identConst[1])
#define IDENT_PERSISTENT(t) \
   ((t) == pic30_identPersistent[0] || (t) == pic30_identPersistent[1])
#define IDENT_ADDRESS(t) \
   ((t) == pic30_identAddress[0] || (t) == pic30_identAddress[1])
#define IDENT_REVERSE(t) \
   ((t) == pic30_identReverse[0] || (t) == pic30_identReverse[1])
#define IDENT_NOLOAD(t) \
   ((t) == pic30_identNoload[0] || (t) == pic30_identNoload[1])
#define IDENT_MERGE(t) \
   ((t) == pic30_identMerge[0] || (t) == pic30_identMerge[1])
#define IDENT_UNORDERED(t) \
   ((t) == pic30_identUnordered[0] || (t) == pic30_identUnordered[1])
#define IDENT_UNSAFE(t) \
   ((t) == pic30_identUnsafe[0] || (t) == pic30_identUnsafe[1])
#define IDENT_DMA(t) \
   ((t) == pic30_identDma[0] || (t) == pic30_identDma[1])

static tree pic30_identInterrupt[2];
static tree pic30_identIRQ[2];
static tree pic30_identAltIRQ[2];
static tree pic30_identSave[2];
static tree pic30_identPreprologue[2];

static tree pic30_identShadow[2];

static tree pic30_identNear[2];
static tree pic30_identFar[2];
static tree pic30_identSFR[2];

static tree pic30_identSpace[2];
static tree pic30_identProg[2];
static tree pic30_identData[2];
static tree pic30_identXmemory[2];
static tree pic30_identYmemory[2];
static tree pic30_identConst[2];
static tree pic30_identPsv[2];
static tree pic30_identEedata[2];
static tree pic30_identDma[2];

static tree pic30_identPersistent[2];
static tree pic30_identAddress[2];
static tree pic30_identReverse[2];
static tree pic30_identNoload[2];
static tree pic30_identMerge[2];
static tree pic30_identUnordered[2];
static tree pic30_identAligned[2];

tree pic30_identUnsafe[2];

typedef enum pic30_interesting_fn_info_ {
  info_invalid,
  info_I,
  info_O,
  info_O_v,
  info_dbl
} pic30_interesting_fn_info;

typedef enum pic30_conversion_status_ {
  conv_state_unknown,
  conv_possible,
  conv_indeterminate,
  conv_not_possible
} pic30_conversion_status;

typedef struct pic30_intersting_fn_ {
  const char *name;
  const char *map_to;
  pic30_interesting_fn_info conversion_style;
  unsigned int interesting_arg;
  unsigned int function_convertable;
} pic30_interesting_fn;

static pic30_interesting_fn pic30_fn_list[] = {
/*  name         map_to         style        arg        c */
  { "fprintf",   "_ifprintf",   info_O,      -1,        0 },
  { "fprintf",   "_dfprintf",   info_dbl,    -1,        0 },
  { "fscanf",    "_ifscanf",    info_I,      -1,        0 },
  { "fscanf",    "_dfscanf",    info_dbl,    -1,        0 },
  { "printf",    "_iprintf",    info_O,      -1,        0 },
  { "printf",    "_dprintf",    info_dbl,    -1,        0 },
  { "scanf",     "_iscanf",     info_I,      -1,        0 },
  { "scanf",     "_dscanf",     info_dbl,    -1,        0 },
  { "snprintf",  "_isnprintf",  info_O,      -1,        0 },
  { "snprintf",  "_dsnprintf",  info_dbl,    -1,        0 },
  { "sprintf",   "_isprintf",   info_O,      -1,        0 },
  { "sprintf",   "_dsprintf",   info_dbl,    -1,        0 },
  { "sscanf",    "_isscanf",    info_I,      -1,        0 },
  { "sscanf",    "_dsscanf",    info_dbl,    -1,        0 },
  { "vfprintf",  "_ivfprintf",  info_O_v,    WR1_REGNO, 0 },
  { "vfprintf",  "_dvfprintf",  info_dbl,    WR1_REGNO, 0 },
  { "vprintf",   "_ivprintf",   info_O_v,    WR0_REGNO, 0 },
  { "vprintf",   "_dvprintf",   info_dbl,    WR0_REGNO, 0 },
  { "vsnprintf", "_ivsnprintf", info_O_v,    WR2_REGNO, 0 },
  { "vsnprintf", "_dvsnprintf", info_dbl,    WR2_REGNO, 0 },
  { "vsprintf",  "_ivsprintf",  info_O_v,    WR1_REGNO, 0 },
  { "vsprintf",  "_dvsprintf",  info_dbl,    WR1_REGNO, 0 },
  { 0,           0,             0,           -1,        0 }
};

/*
 *  strings values are thrown away after they are generated, but the
 *    a reference to the string will always return the same rtx... keep
 *    track of them here and they're conversion state
 */
enum { status_output = 0,
       status_input = 1 };

typedef struct pic30_conversion_cache_ {
  rtx rtl;
  pic30_conversion_status valid[2];
  struct pic30_conversion_cache_ *l,*r;
} pic30_conversion_cache;

static pic30_conversion_cache *pic30_saved_conversion_info;

int pic30_clear_fn_list=1;
int pic30_errata_mask = 0;

typedef struct pic30_errata_map_ {
  const char *name;
  int mask;
  const char *description;
  int conflicts_with;
} pic30_errata_map;

pic30_errata_map errata_map[] = {
  { "retfie", retfie_errata, "A retfie interrupted while in the process of\n"
                          "\t\treturning to a repeat instruction can cause an\n"
                          "\t\tAddressError.\n",
    retfie_errata_disi
  },
  { "retfie_disi", retfie_errata_disi,
                          "A retfie interrupted while in the process of\n"
                          "\t\treturning to a repeat instruction can cause an\n"
                          "\t\tAddressError. Disable using a disi instruction\n"
                          "\t\twhich will not prevent level 7 interrupts from\n"
                          "\t\toccurring.\n",
     retfie_errata
  },
  { "psv",         psv_errata,
                          "\tIndirect access to PSV data may cause CPU status\n"
                          "\t\tregisters to be corrupted producing incorrect\n"
                          "\t\tresults.\n",
    0
  },
  { "exch",        exch_errata,
                          "\tUse of the the exch instruction on certain devices\n"
                          "\t\tsupporting the DMA peripheral can cause\n"
                          "\t\tcorruption in the exchanged data.\n",
    0
  },
  { 0, 0, 0, 0 }
};

static const char *pic30_target_cpu_id;


/*----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*/
/*    L O C A L    F U N C T I O N    P R O T O T Y P E S        */
/*----------------------------------------------------------------------*/
static inline int pic30_obj_elf_p(void);
static void    pic30_SortLibcallNames(void);
static void    pic30_init_idents(void);
static void    pic30_push_section_name(const char *, SECTION_FLAGS_INT);
static void    pic30_pop_section_name(void);
static tree    pic30_pop_save_variable(void);
static tree    pic30_get_save_variable_list(void);
static int    pic30_interrupt_vector_id(tree);
static tree    pic30_lookup_vardecl(const char *pszName);
static int    pic30_valid_machine_decl_save(tree);
static int      pic30_valid_machine_decl_preprologue(tree);
static void    pic30_check_type_attribute(tree, tree, tree *);
static int    pic30_check_decl_attribute(tree, tree, tree, tree *);
static int    pic30_mode1MinMax_operand(rtx, enum machine_mode, int, int);
static int    pic30_frame_pointer_needed_p(int size);
static void    pic30_expand_prologue_frame(int);
static double    pic30_get_double(rtx);
static const char *    pic30_condition_code_name(enum rtx_code);
static char *    pic30_conditional_branchHI(enum rtx_code, char *);
static char *    pic30_conditional_branchSI(enum rtx_code, rtx, char *);
static void    pic30_handle_section_pragma(cpp_reader *, tree *pvalue);
static int     pic30_parse_pragma_option(int pc);
static void    pic30_asm_named_section(const char *name,SECTION_FLAGS_INT flags);
static void    pic30_merged_asm_named_section(const char *name, 
                                              SECTION_FLAGS_INT flags);
SECTION_FLAGS_INT pic30_section_type_flags(tree decl,const char *name,int reloc);
static bool     pic30_assemble_integer(rtx x, unsigned int size, int aligned_p);
static tree pic30_valid_machine_attribute(tree *, tree, tree, int, bool *);
static tree pic30_valid_machine_decl_attribute(tree *, tree, tree, bool *, const char *);
static tree pic30_valid_machine_type_attribute(tree *, tree, tree, bool *);
static void pic30_set_default_decl_attributes(tree , tree *);
static void pic30_globalize_label(FILE *, const char *);
static pic30_interesting_fn *pic30_match_conversion_fn(const char *name);
static void pic30_init_builtins(void);
static int  pic30_sched_adjust_cost(rtx, rtx, rtx, int);
static int  pic30_sched_use_dfa_interface(void);
static int set_section_stack(const char *pszSectionName, 
                             SECTION_FLAGS_INT pszSectionFlag);
void pic30_no_section(void);
static void pic30_push_pop_constant_section(tree decl, int push);

/*----------------------------------------------------------------------*/

/* Initialize the GCC target structure.  */
/* new way of defining things */

#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER pic30_assemble_integer

/* CHANGE TO NAMED SECTION */
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION pic30_asm_named_section

/* GET SECTION TYPE FLAGS */
#undef TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS pic30_section_type_flags

/*
** Instruction scheduler
*/
#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST pic30_sched_adjust_cost

#undef TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE 
#define TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE pic30_sched_use_dfa_interface

#undef TARGET_HAVE_NAMED_SECTIONS
#define TARGET_HAVE_NAMED_SECTIONS true

#undef TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES pic30_set_default_decl_attributes

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN pic30_expand_builtin

#undef TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING pic30_strip_name_encoding

#undef TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION pic30_select_section

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO pic30_encode_section_info

#undef TARGET_ASM_GLOBALIZE_LABEL
#define TARGET_ASM_GLOBALIZE_LABEL pic30_globalize_label

/* Initialize new attribute table structure */
/* -1 for max args implies no upper limit */

const struct attribute_spec pic30_attribute_table[] = {
  /* name|      min| max| decl| type| fn| handler */
  /*           args|args|                         */
  { "interrupt",  0,   3,    0,    0,  0, pic30_valid_machine_attribute },
  { "shadow",     0,   0,    0,    0,  0, pic30_valid_machine_attribute },
  { "sfr",        0,   1,    1,    0,  0, pic30_valid_machine_attribute },
  { "near",       0,   0,    0,    0,  0, pic30_valid_machine_attribute },
  { "far",        0,   0,    0,    0,  0, pic30_valid_machine_attribute },
  { "space",      1,   1,    0,    0,  0, pic30_valid_machine_attribute },
  { "address",    1,   1,    0,    0,  0, pic30_valid_machine_attribute },
  { "unordered",  0,   0,    0,    0,  0, pic30_valid_machine_attribute },
  { "noload",     0,   0,    0,    0,  0, pic30_valid_machine_attribute },
  { "persistent", 0,   0,    0,    0,  0, pic30_valid_machine_attribute },
  { "reverse",    1,   1,    0,    0,  0, pic30_valid_machine_attribute },
  { "unsafe",     0,   0,    0,    0,  0, pic30_valid_machine_attribute },
  { 0,            0,   0,    0,    0,  0, NULL },
};
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE pic30_attribute_table

/*
 *  Init target builtins and give proto-types for attributes to avoid
 *    spurious warning messages
 */
#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS pic30_init_builtins

struct gcc_target targetm = TARGET_INITIALIZER;

#if 0
/*
 *  Verify that any -mcpu architecture is valid
 */
static struct target_info {
  const char *id; 
  int mask; 
} valid_targets[] = {
  { "30F2005", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F2010", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F2010A", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F2011", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F2012", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F2020", TARGET_MASK_ARCH_PIC30F202X },
  { "30F2021", TARGET_MASK_ARCH_PIC30F202X },
  { "30F2022", TARGET_MASK_ARCH_PIC30F202X },
  { "30F2023", TARGET_MASK_ARCH_PIC30F202X },
  { "30F3010", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F3011", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F3012", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F3013", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F3014", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F4011", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F4012", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F4013", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F5011", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F5013", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F5015", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F5016", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F6010", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F6010A", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F6011", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F6011A", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F6012", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F6012A", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F6013", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F6013A", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F6014", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F6014A", TARGET_MASK_ARCH_PIC30FXXXX },
  { "30F6015", TARGET_MASK_ARCH_PIC30FXXXX },
  { "24FJ64GA006", TARGET_MASK_ARCH_PIC24F },
  { "24FJ96GA006", TARGET_MASK_ARCH_PIC24F },
  { "24FJ128GA006", TARGET_MASK_ARCH_PIC24F },
  { "24FJ64GA008", TARGET_MASK_ARCH_PIC24F },
  { "24FJ96GA008", TARGET_MASK_ARCH_PIC24F },
  { "24FJ128GA008", TARGET_MASK_ARCH_PIC24F },
  { "24FJ64GA010", TARGET_MASK_ARCH_PIC24F },
  { "24FJ96GA010", TARGET_MASK_ARCH_PIC24F },
  { "24FJ128GA010", TARGET_MASK_ARCH_PIC24F },
  { "24HJ64GP206", TARGET_MASK_ARCH_PIC24H },
  { "24HJ64GP210", TARGET_MASK_ARCH_PIC24H },
  { "24HJ64GP506", TARGET_MASK_ARCH_PIC24H },
  { "24HJ64GP510", TARGET_MASK_ARCH_PIC24H },
  { "24HJ128GP206", TARGET_MASK_ARCH_PIC24H },
  { "24HJ128GP306", TARGET_MASK_ARCH_PIC24H },
  { "24HJ128GP310", TARGET_MASK_ARCH_PIC24H },
  { "24HJ128GP506", TARGET_MASK_ARCH_PIC24H },
  { "24HJ128GP510", TARGET_MASK_ARCH_PIC24H },
  { "24HJ256GP206", TARGET_MASK_ARCH_PIC24H },
  { "24HJ256GP210", TARGET_MASK_ARCH_PIC24H },
  { "24HJ256GP610", TARGET_MASK_ARCH_PIC24H },
  { "33FJ64GP206", TARGET_MASK_ARCH_PIC33 },
  { "33FJ64GP306", TARGET_MASK_ARCH_PIC33 },
#if 0
  { "33FJ64GP308", TARGET_MASK_ARCH_PIC33 },
#endif
  { "33FJ64GP310", TARGET_MASK_ARCH_PIC33 },
#if 0
  { "33FJ64GP506", TARGET_MASK_ARCH_PIC33 },
  { "33FJ64GP508", TARGET_MASK_ARCH_PIC33 },
  { "33FJ64GP510", TARGET_MASK_ARCH_PIC33 },
#endif
  { "33FJ64GP706", TARGET_MASK_ARCH_PIC33 },
  { "33FJ64GP708", TARGET_MASK_ARCH_PIC33 },
  { "33FJ64GP710", TARGET_MASK_ARCH_PIC33 },
  { "33FJ128GP306", TARGET_MASK_ARCH_PIC33 },
#if 0
  { "33FJ128GP308", TARGET_MASK_ARCH_PIC33 },
#endif
  { "33FJ128GP310", TARGET_MASK_ARCH_PIC33 },
#if 0
  { "33FJ128GP506", TARGET_MASK_ARCH_PIC33 },
  { "33FJ128GP508", TARGET_MASK_ARCH_PIC33 },
#endif
  { "33FJ128GP510", TARGET_MASK_ARCH_PIC33 },
  { "33FJ128GP706", TARGET_MASK_ARCH_PIC33 },
  { "33FJ128GP708", TARGET_MASK_ARCH_PIC33 },
  { "33FJ128GP710", TARGET_MASK_ARCH_PIC33 },
  { "33FJ256GP506", TARGET_MASK_ARCH_PIC33 },
#if 0
  { "33FJ256GP508", TARGET_MASK_ARCH_PIC33 },
  { "33FJ256GP510", TARGET_MASK_ARCH_PIC33 },
#endif
  { "33FJ256GP710", TARGET_MASK_ARCH_PIC33 },
#if 0
  { "33FJ64MC306", TARGET_MASK_ARCH_PIC33 },
  { "33FJ64MC308", TARGET_MASK_ARCH_PIC33 },
  { "33FJ64MC310", TARGET_MASK_ARCH_PIC33 },
#endif
  { "33FJ64MC506", TARGET_MASK_ARCH_PIC33 },
  { "33FJ64MC508", TARGET_MASK_ARCH_PIC33 },
  { "33FJ64MC510", TARGET_MASK_ARCH_PIC33 },
  { "33FJ64MC706", TARGET_MASK_ARCH_PIC33 },
  { "33FJ64MC710", TARGET_MASK_ARCH_PIC33 },
#if 0
  { "33FJ128MC306", TARGET_MASK_ARCH_PIC33 },
  { "33FJ128MC308", TARGET_MASK_ARCH_PIC33 },
  { "33FJ128MC310", TARGET_MASK_ARCH_PIC33 },
#endif
  { "33FJ128MC506", TARGET_MASK_ARCH_PIC33 },
#if 0
  { "33FJ128MC508", TARGET_MASK_ARCH_PIC33 },
#endif
  { "33FJ128MC510", TARGET_MASK_ARCH_PIC33 },
  { "33FJ128MC706", TARGET_MASK_ARCH_PIC33 },
  { "33FJ128MC708", TARGET_MASK_ARCH_PIC33 },
  { "33FJ128MC710", TARGET_MASK_ARCH_PIC33 },
#if 0
  { "33FJ256MC506", TARGET_MASK_ARCH_PIC33 },
  { "33FJ256MC508", TARGET_MASK_ARCH_PIC33 },
#endif
  { "33FJ256MC510", TARGET_MASK_ARCH_PIC33 },
  { "33FJ256MC710", TARGET_MASK_ARCH_PIC33 },
  { "GENERIC-16BIT",TARGET_MASK_ARCH_GENERIC },
  { 0,    0 }
};
#endif

static struct isr_info {
  const char *id;
  int mask;
} *valid_isr_names;

static int valid_isr_names_cnt;

/* stupid prototype */
unsigned int validate_target_id(char *id, char **matched_id);
unsigned int validate_target_id(char *id, char **matched_id) {
  struct resource_introduction_block *rib;
  struct resource_data d;
  int version;
  int mask;
  char *Microchip;
  char *new_version = xstrdup(version_string);
  char *version_part1;
  char *version_part2;
  int mismatch=0;
  
  mask = 0;
  if (strcmp(id, "GENERIC-16BIT") == 0) {
    *matched_id = "GENERIC-16BIT";
    return TARGET_MASK_ARCH_GENERIC;
  }
  if (pic30_resource_file == 0) {
    warning("Provide a resource file");
    return 0;
  }
  rib = read_rib(pic30_resource_file);
  if (rib == 0) {
    warning("Could not open resource file: %s", pic30_resource_file);
    return 0;
  }
  if (strcmp(rib->tool_chain,"C30")) {
    warning("Invalid resource resource file");
    close_rib();
    return 0;
  }
  version = rib->version.major * 100 + rib->version.minor;
  if ((version != pic30_compiler_version) && (flag_preprocess_only == 0)) {
    char buffer[16];

    sprintf(buffer,"%d.%.2d", rib->version.major, rib->version.minor);
    warning("Resource version (%s) does not match compiler!", buffer);
    mismatch=1;
  }
  /* modify version number */
  Microchip = strstr(new_version,"Microchip");
  if (Microchip) {
    int i;

    for (; (*Microchip) && ((*Microchip <= '0') || (*Microchip >= '9'));
         Microchip++);
    if (*Microchip) {
      i = strtol(Microchip, &Microchip, 0);
      if ((*Microchip) && ((*Microchip == '_') || (*Microchip == '.'))) {
         Microchip++;
         i = strtol(Microchip, &Microchip, 0);
         for (; *Microchip && *Microchip != ' '; Microchip++);
      }
      version_part1 = new_version;
      *Microchip = 0;
      version_part2 = Microchip+1;
      /* version part located */
      version_string = xmalloc(strlen(version_part1) + strlen(version_part2) +
                               40);
      if (mismatch) {
        sprintf(version_string,"%s, resource version %d.%.2d (%c), %s",
                version_part1, rib->version.major, rib->version.minor,
                rib->resource_version_increment,version_part2);
      } else {
        sprintf(version_string,"%s (%c) %s",
                version_part1, rib->resource_version_increment,version_part2);
      }
    }
  }
  free(new_version);
  if (rib->field_count >= 3) {
    int record;
    int device_mask=0;
    int device_id=0;
    int isr_names_max=0;
    int isr_names_idx=0;

    for (record = 0; move_to_record(record); record++) {
      read_value(rik_string, &d);
      if (strcmp(d.v.s, id) == 0) {
        /* match */ 
        *matched_id = d.v.s;
        read_value(rik_int, &d);
        if (d.v.i & IS_DEVICE_ID) {
          device_mask = (d.v.i & (~IS_DEVICE_ID));
          if (d.v.i & P30F) mask = TARGET_MASK_ARCH_PIC30;
          if (d.v.i & P33F) mask = TARGET_MASK_ARCH_PIC33;
          if (d.v.i & P30FSMPS) mask = TARGET_MASK_ARCH_PIC30F202X;
          if (d.v.i & P24F) mask = TARGET_MASK_ARCH_PIC24F;
          if (d.v.i & P24H) mask = TARGET_MASK_ARCH_PIC24H;
          read_value(rik_int, &d);
          device_id = d.v.i;
          break;
        }
      }
      free(d.v.s);
    }

    for (record = 0; move_to_record(record); record++) {
      read_value(rik_string, &d);
      {
        char *match; 
        int flags;
        int device;

        match = d.v.s;
        read_value(rik_int, &d);
        if (d.v.i & IS_DEVICE_ID) {
          free(match);
        } else if (d.v.i & IS_VECTOR_ID) {
          flags = d.v.i;
          read_value(rik_int,&d);
          device = d.v.i;
          if (((device) && (device == device_id)) ||
              ((device == 0) && (flags & device_mask))) {
            /* vector for this device or family */
            if ((valid_isr_names == 0) || (isr_names_idx >= isr_names_max)) {
              struct isr_info *new;

              isr_names_max += 256;
              new = xmalloc(isr_names_max * sizeof(struct isr_info));
              if (valid_isr_names) {
                memcpy(new, valid_isr_names, 
                       isr_names_idx * sizeof(struct isr_info));
                free(valid_isr_names);
              }
              valid_isr_names = new;
            }
            valid_isr_names[isr_names_idx].id = match;
            valid_isr_names[isr_names_idx++].mask = mask;
          }
        }
      }
    }
    valid_isr_names_cnt = isr_names_idx;
  }
  close_rib();
  return mask;
}

static void validate_ordered_tables(void) {
  int i;

  for (i = 1; i < valid_isr_names_cnt; i++) {
    if (strcmp(valid_isr_names[i-1].id, valid_isr_names[i].id) > 0) {
      fprintf(stderr,"internal warning: %s and %s are mis-ordered\n",
              valid_isr_names[i-1].id, valid_isr_names[i].id);
    }
  }
}

static int
pic30_bsearch_isr_compare(const void *va, const void *vb)
{
  const char *a = (const char *)va;
  const struct isr_info *b = vb;
  int result = strcmp(a,b->id);

  if (b->mask & target_flags) return result;
  else if (result == 0) return 1;
  else return result;
}

static int
pic30_bsearch_rsn_compare(const void *va, const void *vb) {
  const char *a = (const char *)va;
  const struct reserved_section_names_ *n = vb;

  if (n) return strncmp(a, n->section_name, strlen(n->section_name));
  else return 0;
}

static int
pic30_bsearch_vsf_compare(const void *va, const void *vb) {
  const char *a = (const char *)va;
  const struct valid_section_flags_ *f = vb;

  if (f) return strncmp(a, f->flag_name, strlen(f->flag_name));
  else return 0;
}

/* validates a section declaration based on its name and any flags */
static SECTION_FLAGS_INT validate_section_flags(const char *name, 
                                                 SECTION_FLAGS_INT attr_flags){
  SECTION_FLAGS_INT set_flags = attr_flags;
  struct reserved_section_names_ *r_section = 0;
  struct valid_section_flags_ *v_flags = 0;
  char *f,*fe,comma=0;
  char *flags;
  int first_flag = 1;

  f = 0;
  flags = strchr(name, ',');
  if (flags) {
    *flags = 0;
    f = flags+1;
    comma = ',';
  }
  if (name) r_section = bsearch(name, reserved_section_names, 
                                (sizeof(reserved_section_names) / 
                                 sizeof(struct reserved_section_names_)) - 1, 
                                sizeof(struct reserved_section_names_), 
                                pic30_bsearch_rsn_compare);
  if (r_section) {
    set_flags |= r_section->mask;
  }
  if (f) do {
    fe = strchr(f, ','); 
    if (fe) {
      *fe = 0;
    }
    /* nasty safe-ctype.h means that we can't use isspace */
    while (*f && ISSPACE(*f)) f++;
    if (*f) {
      v_flags = bsearch(f, valid_section_flags, 
                        (sizeof(valid_section_flags) /
                         sizeof(struct valid_section_flags_)) -1,
                        sizeof(struct valid_section_flags_),
                        pic30_bsearch_vsf_compare);
      if (!v_flags) {
        if (first_flag) {
          char *s;

          for (s = f; *s; s++) {
            for (v_flags = valid_section_flags; v_flags->flag_name; v_flags++) {
              if (*s == v_flags->single_letter_equiv) {
                if (v_flags->single_letter_equiv == 'b') {
                  /* this may be .pbss,b for peristent */
                  if ((strncmp(name, ".pbss", 5) == 0) && 
                      ((v_flags->flag_mask & SECTION_PERSIST) == 0)) continue;
                } else if (v_flags->single_letter_equiv == 'r') {
                  /* 'r' used to be used for .eedata - don't set READ_ONLY for
                         .eedata section */
                  if (strncmp(name, ".eedata", 7) == 0) break;
                }
                set_flags |= v_flags->flag_mask;
                break;
              }
            }
            if (!v_flags->flag_name) {
              warning("'%c': unrecognized old-style section flag", *s);
              break;
            }
            *s = ' ';
            comma=' ';
          }
          first_flag = 0;
        } else warning("'%s': unrecognized section flag", f);
      } else {
        set_flags |= v_flags->flag_mask;
      }
      if (fe) {
        *fe = ',';
        f = fe+1;
      } else break;
    } else break;
  } while(1);
  if (flags) *flags = comma;
  for (v_flags = valid_section_flags; v_flags->flag_name; v_flags++) {
    if ((set_flags & v_flags->flag_mask) && 
        (set_flags & v_flags->incompatable_with)) {
      error("incompatible section flags for section '%s'", name);
      return set_flags;
    }
  }
  return set_flags;
}

/* validate prefix before an identifier */
static SECTION_FLAGS_INT validate_identifier_flags(const char *id) {
  const char *f = id;
  SECTION_FLAGS_INT flags = 0;
  struct valid_section_flags_ *v_flags = 0;

  while (*f == PIC30_EXTENDED_FLAG[0]) {
    if (strncmp(f, PIC30_PROG_FLAG, sizeof(PIC30_PROG_FLAG)-1) == 0) {
      flags |= SECTION_CODE;
      f += sizeof(PIC30_PROG_FLAG)-1;
    } else if (strncmp(f, PIC30_FCNN_FLAG, sizeof(PIC30_FCNN_FLAG)-1) == 0) {
      flags |= SECTION_CODE;
      f += sizeof(PIC30_FCNN_FLAG)-1;
    } else if (strncmp(f, PIC30_FCNS_FLAG, sizeof(PIC30_FCNS_FLAG)-1) == 0) {
      flags |= SECTION_CODE;
      f += sizeof(PIC30_FCNS_FLAG)-1;
    } else if (strncmp(f, PIC30_DATA_FLAG, sizeof(PIC30_DATA_FLAG)-1) == 0) {
      flags |= SECTION_WRITE;
      f += sizeof(PIC30_DATA_FLAG)-1;
    } else if (strncmp(f, PIC30_X_FLAG, sizeof(PIC30_X_FLAG)-1) == 0) {
      flags |= SECTION_XMEMORY;
      f += sizeof(PIC30_X_FLAG)-1;
    } else if (strncmp(f, PIC30_Y_FLAG, sizeof(PIC30_Y_FLAG)-1) == 0) {
      flags |= SECTION_YMEMORY;
      f += sizeof(PIC30_Y_FLAG)-1;
    } else if (strncmp(f, PIC30_APSV_FLAG, sizeof(PIC30_APSV_FLAG)-1) == 0) {
      flags |= SECTION_READ_ONLY;
      f += sizeof(PIC30_APSV_FLAG)-1;
    } else if (strncmp(f, PIC30_PRST_FLAG, sizeof(PIC30_PRST_FLAG)-1) == 0) {
      flags |= SECTION_PERSIST;
      f += sizeof(PIC30_PRST_FLAG)-1;
    } else if (strncmp(f, PIC30_DMA_FLAG, sizeof(PIC30_DMA_FLAG)-1) == 0) {
      flags |= SECTION_DMA;
      f += sizeof(PIC30_DMA_FLAG)-1;
    } else if (strncmp(f, PIC30_PSV_FLAG, sizeof(PIC30_PSV_FLAG)-1) == 0) {
      flags |= SECTION_PSV;
      f += sizeof(PIC30_PSV_FLAG)-1;
    } else  if (strncmp(f, PIC30_EE_FLAG, sizeof(PIC30_EE_FLAG)-1) == 0) {
      flags |= SECTION_EEDATA;
      f += sizeof(PIC30_EE_FLAG)-1;
    } else if (strncmp(f, PIC30_NEAR_FLAG, sizeof(PIC30_NEAR_FLAG)-1) == 0) {
      flags |= SECTION_NEAR;
      f += sizeof(PIC30_NEAR_FLAG)-1;
    } else if (strncmp(f, PIC30_ADDR_FLAG, sizeof(PIC30_ADDR_FLAG)-1) == 0) {
      flags |= SECTION_ADDRESS;
      f += sizeof(PIC30_ADDR_FLAG)-1;
    } else if (strncmp(f, PIC30_MERGE_FLAG, sizeof(PIC30_MERGE_FLAG)-1) == 0) {
      flags |= SECTION_MERGE;
      f += sizeof(PIC30_MERGE_FLAG)-1;
    } else if (strncmp(f, PIC30_NOLOAD_FLAG, sizeof(PIC30_NOLOAD_FLAG)-1) == 0){
      flags |= SECTION_NOLOAD;
      f += sizeof(PIC30_NOLOAD_FLAG)-1;
    } else if (strncmp(f, PIC30_ALGN_FLAG, sizeof(PIC30_ALGN_FLAG)-1) == 0) {
      flags |= SECTION_ALIGN;
      f += sizeof(PIC30_ALGN_FLAG)-1;
    } else if (strncmp(f, PIC30_RALGN_FLAG, sizeof(PIC30_RALGN_FLAG)-1) == 0) {
      flags |= SECTION_REVERSE;
      f += sizeof(PIC30_RALGN_FLAG)-1;
    } else if (strncmp(f, PIC30_BSS_FLAG, sizeof(PIC30_BSS_FLAG)-1) == 0) {
      flags |= SECTION_BSS;
      f += sizeof(PIC30_BSS_FLAG)-1;
    } else if (strncmp(f, PIC30_FCNN_FLAG, sizeof(PIC30_FCNN_FLAG)-1) == 0) {
      f += sizeof(PIC30_FCNN_FLAG)-1;
    } else if (strncmp(f, PIC30_FCNS_FLAG, sizeof(PIC30_FCNS_FLAG)-1) == 0) {
      f += sizeof(PIC30_FCNS_FLAG)-1;
    } else {
      error("Could not determine flags for: '%s'", id);
      return flags;
    }
  }
  for (v_flags = valid_section_flags; v_flags->flag_name; v_flags++) {
    if ((flags & v_flags->flag_mask) && (flags & v_flags->incompatable_with)) {
      error("incompatible section flags for identifier '%s'", 
              pic30_strip_name_encoding(id));
      return flags;
    }
  }
  return flags;
}

/* determine complete prefix for declaration */
static int pic30_build_prefix(tree decl, int fnear, char *prefix) {
  char *f = prefix;
  tree attr;
  tree address_attr = 0;
  tree reverse_attr = 0;
  tree space_attr = 0;
  tree near_attr = 0;
  tree sfr_attr = 0;
  SECTION_FLAGS_INT flags = 0;
  const char *ident;
  int section_type_set = 0;
  int auto_psv=0;

  if (fnear == -1) {
    section_type_set = 1;
    fnear = 0;
  }
  reverse_attr = lookup_attribute(IDENTIFIER_POINTER(pic30_identReverse[0]),
                                  DECL_ATTRIBUTES(decl));
  address_attr = lookup_attribute(IDENTIFIER_POINTER(pic30_identAddress[0]),
                                  DECL_ATTRIBUTES(decl));
  space_attr = lookup_attribute(IDENTIFIER_POINTER(pic30_identSpace[0]),
                                DECL_ATTRIBUTES(decl));
  near_attr = lookup_attribute(IDENTIFIER_POINTER(pic30_identNear[0]),
                               DECL_ATTRIBUTES(decl));
  sfr_attr = lookup_attribute(IDENTIFIER_POINTER(pic30_identSFR[0]),
                              DECL_ATTRIBUTES(decl));
  auto_psv = (space_attr && IDENT_CONST(TREE_VALUE(TREE_VALUE(space_attr))));
  ident = IDENTIFIER_POINTER(DECL_NAME(decl));
  if (DECL_SECTION_NAME(decl)) {
    const char *name = TREE_STRING_POINTER(DECL_SECTION_NAME(decl));

    flags = pic30_section_type_flags(0, name, 1);
  } else {
    if ((address_attr) || (reverse_attr)) {
      DECL_SECTION_NAME(decl) = build_string(1,"*");
    }
    if (TARGET_CONST_IN_CODE) {
      if (TREE_CODE(decl) == STRING_CST) {
        if (!flag_writable_strings) {
          flags |= SECTION_READ_ONLY;
        }
      }
      if (TREE_CODE(decl) == VAR_DECL) {
        
        if (!space_attr && TREE_READONLY(decl) && DECL_INITIAL(decl) && 
            TREE_CONSTANT(DECL_INITIAL(decl)))
          flags |= SECTION_READ_ONLY;
      }
    }
  }
  if (address_attr) {
    if (flags & SECTION_ADDRESS)
      warning_with_decl(decl, 
        "address attribute conflicts with section attribute for '%s'", ident);
    else f += sprintf(f, PIC30_ADDR_FLAG);
  }
  attr = lookup_attribute(IDENTIFIER_POINTER(pic30_identNoload[0]),
                          DECL_ATTRIBUTES(decl));
  if (attr) {
    if (auto_psv) {
      warning_with_decl(decl, "Ignoring noload attribute for '%s'", ident);
    } else {
      f += sprintf(f, PIC30_NOLOAD_FLAG);
      if ((TREE_CODE(decl) != FUNCTION_DECL) && (DECL_INITIAL(decl)))
        warning_with_decl(decl,
                          "Noload variable '%s' will not be initialized",
                          ident);
    }
  }
  attr = lookup_attribute(IDENTIFIER_POINTER(pic30_identMerge[0]),
                          DECL_ATTRIBUTES(decl));
  if (attr) {
    if (auto_psv) {
       warning_with_decl(decl, "Ignoring merge attribute for '%s'", ident);
    } else f += sprintf(f, PIC30_MERGE_FLAG);
  }
#if 0
  attr = lookup_attribute(IDENTIFIER_POINTER(pic30_identAlign[0]),
                          DECL_ATTRIBUTES(decl));
  if (attr) {
    if (flags & SECTION_ALIGN)
      warning_with_decl(decl, 
        "align attribute conflicts with section attribute for '%s'", ident);
    else f += sprintf(f, PIC30_ALGN_FLAG);
  }
#endif
  attr = lookup_attribute(IDENTIFIER_POINTER(pic30_identReverse[0]),
                          DECL_ATTRIBUTES(decl));
  if (attr) {
    if (flags & SECTION_REVERSE)
      warning_with_decl(decl, 
        "reverse attribute conflicts with section attribute for '%s'", ident);
    else f += sprintf(f, PIC30_RALGN_FLAG);
  }
  attr = lookup_attribute(IDENTIFIER_POINTER(pic30_identPersistent[0]),
                           DECL_ATTRIBUTES(decl));
  if ((flags & SECTION_PERSIST) || (attr)) {
    if (auto_psv) {
      if (attr) {
        warning_with_decl(decl,"Ignoring persistent attribute for '%s'", ident);
      }
    } 
    else {
      f += sprintf(f, PIC30_PRST_FLAG);
      section_type_set = 1;
      if (DECL_INITIAL(decl)) {
        warning_with_decl(decl,
                          "Persistent variable '%s' will not be initialized",
                          ident);
      } 
    }
  } 
  fnear |= (flags & SECTION_NEAR);
  attr = space_attr;
  if ((flags & SECTION_CODE) || 
      (attr && (IDENT_PROG(TREE_VALUE(TREE_VALUE(attr)))))) {
    f += sprintf(f, PIC30_PROG_FLAG);
    fnear = 0;
    section_type_set = 1;
  } 
  if ((flags & SECTION_WRITE) || 
      (attr && (IDENT_DATA(TREE_VALUE(TREE_VALUE(attr)))))) {
    f += sprintf(f, PIC30_DATA_FLAG);
    section_type_set = 1;
  } 
  /* we can't ask for a BSS section apart from by using the old naming
     convention or flags in a section directive - but if it is there, we
     should honour it */
  if (flags & SECTION_BSS) {
    f += sprintf(f, PIC30_BSS_FLAG);
    section_type_set = 1;
  }
  if ((flags & SECTION_READ_ONLY) || (auto_psv)) {
    
    if (address_attr || reverse_attr) {
      f += sprintf(f, PIC30_PSV_FLAG);
      if (auto_psv) {
        if (reverse_attr) 
          warning_with_decl(decl, 
            "Ignoring space(auto_psv) attribute due to reverse(); "
            "assuming space(psv) for '%s'.", ident); 
        else
          warning_with_decl(decl, 
            "Ignoring space(auto_psv) attribute due to address(); "
            "assuming space(psv) for '%s'.", ident); 
      } else {
        if (reverse_attr)
          warning_with_decl(decl,
            "Not placing into compiler managed PSV section due to "
            "reverse(); assuming space(psv) for '%s'.", ident); 
        else
          warning_with_decl(decl,
            "Not placing into compiler managed PSV section due to "
            "address(); assuming space(psv) for '%s'.", ident); 
      }
    } else f += sprintf(f, PIC30_APSV_FLAG);
    fnear = 0;
    section_type_set = 1;
    if (near_attr) {
      warning_with_decl(decl,
        "A PSV variable cannot be near; ignoring near for '%s'", ident);
    }
  } 
  if ((flags & SECTION_PSV) || 
      (attr && (IDENT_PSV(TREE_VALUE(TREE_VALUE(attr)))))) {
    f += sprintf(f, PIC30_PSV_FLAG);
    fnear = 0;
    section_type_set = 1;
    if (near_attr) {
      warning_with_decl(decl,
        "A PSV variable cannot be near; ignoring near for '%s'", ident);
    }
  } 
  if ((flags & SECTION_EEDATA) || 
      (attr && (IDENT_EEDATA(TREE_VALUE(TREE_VALUE(attr)))))) {
    f += sprintf(f, PIC30_EE_FLAG);
    fnear = 0;
    section_type_set = 1;
  } 
  if (!section_type_set) {
    f += sprintf(f, PIC30_DATA_FLAG);
  }
  if ((flags & SECTION_XMEMORY) || 
      (attr && (IDENT_XMEMORY(TREE_VALUE(TREE_VALUE(attr))))))
    f += sprintf(f, PIC30_X_FLAG);
  if ((flags & SECTION_YMEMORY) || 
      (attr && (IDENT_YMEMORY(TREE_VALUE(TREE_VALUE(attr))))))
    f += sprintf(f, PIC30_Y_FLAG);
  if ((flags & SECTION_DMA) || 
      (attr && (IDENT_DMA(TREE_VALUE(TREE_VALUE(attr)))))) {
    f += sprintf(f, PIC30_DMA_FLAG);
    if ((flags & SECTION_NEAR) == 0) fnear = 0;
  }
  if (fnear && address_attr) {
    if (TREE_INT_CST_LOW(TREE_VALUE(TREE_VALUE(address_attr))) > 0x1FFF) {
      fnear = 0;
      if (near_attr)
         warning_with_decl(decl, 
           "address() attribute over-rides near attribute for '%s'", ident);
    }
  }
  if ((fnear) && !(flags & (SECTION_INFO))) {
    if (sfr_attr) f += sprintf(f, PIC30_SFR_FLAG);
    f += sprintf(f, PIC30_NEAR_FLAG);
  }
  return fnear;
}

static const char *default_section_name(tree decl, SECTION_FLAGS_INT flags) {
  static char result[80];
  char *f;
  int i,psv=0,implied_psv=0;
  tree a,r,u,p,is_aligned;
  const char *pszSectionName=0;

  f = result;
  *f = 0;
  implied_psv = ((flags & SECTION_READ_ONLY) && (TARGET_CONST_IN_CODE));
  if (decl) {
    is_aligned = lookup_attribute(IDENTIFIER_POINTER(pic30_identAligned[0]),
                       DECL_ATTRIBUTES(decl)); 
    a = lookup_attribute(IDENTIFIER_POINTER(pic30_identAddress[0]),
                       DECL_ATTRIBUTES(decl)); 
    r = lookup_attribute(IDENTIFIER_POINTER(pic30_identReverse[0]),
                       DECL_ATTRIBUTES(decl)); 
    u = lookup_attribute(IDENTIFIER_POINTER(pic30_identUnordered[0]),
                         DECL_ATTRIBUTES(decl));
    p = lookup_attribute(IDENTIFIER_POINTER(pic30_identSpace[0]),
                         DECL_ATTRIBUTES(decl));
    if (p) {
      psv = IDENT_CONST(TREE_VALUE(TREE_VALUE(p)));
    }
    psv = psv | implied_psv;
    if (DECL_SECTION_NAME (decl)) {
  
      pszSectionName = TREE_STRING_POINTER(DECL_SECTION_NAME(decl));
    }
    if (r || u || psv) {
      if (pszSectionName && strcmp(pszSectionName,pic30_default_section)) {
        warning_with_decl(decl, "Ignoring explicit section name for '%s'", 
                          IDENTIFIER_POINTER(DECL_NAME(decl)));
      }
      if (u && psv) {
        warning_with_decl(decl, "Ignoring unordered for '%s'",
                          IDENTIFIER_POINTER(DECL_NAME(decl)));
        u = 0;
      }
    }
    if (a) {
      if (!pszSectionName||(strcmp(pszSectionName,pic30_default_section) == 0))
        f += sprintf(result, "%s_0x%x,%s(%d)", 
                     flags & SECTION_CODE ? ".prog" : ".data",
                     TREE_INT_CST_LOW(TREE_VALUE(TREE_VALUE(a))),
                     SECTION_ATTR_ADDRESS,
                     TREE_INT_CST_LOW(TREE_VALUE(TREE_VALUE(a))));
      else 
        f += sprintf(result, "%s,%s(%d)", 
                     pszSectionName,
                     SECTION_ATTR_ADDRESS,
                     TREE_INT_CST_LOW(TREE_VALUE(TREE_VALUE(a))));
    } else if (r) {
      f += sprintf(result, "*,%s(%d)", 
              SECTION_ATTR_REVERSE, 
              TREE_INT_CST_LOW(TREE_VALUE(TREE_VALUE(r))));
    }
    if (f != result) return result;
    if (psv) return SECTION_NAME_CONST;
    if (u) return pic30_default_section;
    if (pszSectionName) return pszSectionName;
    if (is_aligned) return pic30_default_section;
  }
  if (flags) {
    i = 0;
    if ((flags & SECTION_READ_ONLY) && (!TARGET_CONST_IN_CODE)) {
      if ((TREE_CODE(decl) == STRING_CST) ||
          (DECL_INITIAL(decl) && TREE_CONSTANT(DECL_INITIAL(decl)))) {
        const char *name;

        if (flags & SECTION_NEAR) name = SECTION_NAME_NDCONST;
        else name = SECTION_NAME_DCONST;
        while (reserved_section_names[i].section_name && 
               strcmp(name, reserved_section_names[i].section_name) == 0) {
          if ((flags ^ reserved_section_names[i].mask) == 0) return name;
        }
      }
    }
    while (reserved_section_names[i].section_name) {
      if ((flags ^ reserved_section_names[i].mask) == 0) {
        if ((pic30_text_scn) && 
            (strcmp(reserved_section_names[i].section_name, ".text") == 0))
            return pic30_text_scn;
        else return reserved_section_names[i].section_name;
      }
      i++;
    }
  }
  return pic30_default_section;
}

int pic30_data_alignment(tree type, int align) {

  if ((TREE_CODE(type) == RECORD_TYPE) && (align < INT_TYPE_SIZE)) {
    align = INT_TYPE_SIZE;
  }
  return align;
}
 
/*
** Determine if the output format is ELF/DWARF or COFF
*/
static inline int
pic30_obj_elf_p(void)
{
#if defined(OBJECT_FORMAT_ELF)
    return(1);
#else
    int fObjELF;

    fObjELF = (write_symbols == DWARF2_DEBUG);

    return(fObjELF);
#endif
}

/*
 *  Called after options are processed.  Verifies the option values are
 *   valid for the -m<target> options
 */
void pic30_override_options() {
  int i;
  int pic30_license_valid = 1;

  /*
   *  On systems where we have a licence manager, call it
   */
#ifdef LICENSE_MANAGER
  pic30_license_valid = 0;
  { char *path;
    char *exec;
    char *args[] = { 0, "-k", 0 };
    char *c;
    char *err_msg, *err_arg;
    int pid;
    int status;
    extern char **save_argv;

    pic30_license_valid = 0;
    path = make_relative_prefix(save_argv[0], STANDARD_BINDIR_PREFIX,
                                         STANDARD_EXEC_PREFIX);

    if (!path) fatal_error("Could not locate `%s`\n", save_argv[0]);
    exec = xmalloc(strlen(path)+sizeof("pic30-lm.exe") + 1);
    sprintf(exec, "%spic30-lm.exe", path);
    args[0] = exec;
    pid = pexecute(exec, args, "foobar", 0, &err_msg, &err_arg,
                   PEXECUTE_FIRST | PEXECUTE_LAST);
    if (pid == -1) fatal_error (err_msg, exec);
    pid = pwait(pid, &status, 0);
    if (pid < 0) abort();
    if (WIFEXITED(status) && (WEXITSTATUS(status) == 0)) {
      pic30_license_valid=1;
    } else if (WIFEXITED(status)) {
      pic30_license_valid=WEXITSTATUS(status) - 256;
    }
    free(exec);
  }
#endif

  validate_ordered_tables();
  /* disable scheduling */
  flag_schedule_insns = 0;
  flag_schedule_insns_after_reload = 0;
  /* disable register renameing - it conflicts with our RTL generation of 
                                  prologue and epilogue code */
  flag_rename_registers=0; 
  SET_MCHP_VERSION(pic30_compiler_version);
  if (pic30_target_cpu) {
    char *copy, *copy2, *c;
    int mask;

    copy = xmalloc(80);
    sprintf(copy, "__dsPIC%s", pic30_target_cpu);
    copy2 = "__dsPIC30F__";
    /* nasty safe-ctype.h means that we can't use toupper */
    for (c = copy + 7; *c; c++) *c = TOUPPER(*c);
    mask = validate_target_id(copy+7, &pic30_target_cpu_id);
    if (mask) {
      target_flags |= mask;
      strcat(copy, "__");
      pic30_target_cpu = copy;
      if (TARGET_ARCH(PIC24F) || TARGET_ARCH(PIC24H)) {
        copy2 = xmalloc(80);
        copy[2] = '_';
        copy[3] = '_';
        sprintf(copy2, "__PIC24%c__", TARGET_ARCH(PIC24F) ? 'F' : 'H');
        pic30_target_cpu = copy+2;
      } else if (TARGET_ARCH(PIC33)) {
        copy2 = "__dsPIC33F__";
      }
      pic30_target_family = copy2;
      if (TARGET_ARCH(GENERIC)) {
        pic30_target_cpu = 0;
        pic30_target_family = 0;
      }
    } else {
      error("Invalid -mcpu option.  CPU %s not recognized.\n", 
            pic30_target_cpu);
      pic30_target_cpu = 0;
      pic30_target_cpu_id = "invalid target";
    }
  }
  else { 
    /* set the default architecture */
    target_flags |= TARGET_MASK_ARCH_PIC30;
    pic30_target_cpu_id = "unspecified dsPIC30F";
  }
  if (pic30_io_size) {
    pic30_io_size_val = -1;
    if (*pic30_io_size == '0') {
      pic30_io_size_val = 0;
      target_flags &= ~TARGET_MASK_SMART_IO;
    } else if (*pic30_io_size == '1') {
      pic30_io_size_val = 1;
      target_flags |= TARGET_MASK_SMART_IO;
    } else if (*pic30_io_size == '2') {
      pic30_io_size_val = 2;
      target_flags |= TARGET_MASK_SMART_IO;
    } 
    if (pic30_io_size_val > 0) pic30_smart_io_warning=1;
    if ((pic30_io_size_val == -1) || (pic30_io_size[1] != 0)) {
      warning("-msmart-io=%s invalid; defaulting to -msmart-io=1",
              pic30_io_size);
      pic30_io_size_val = 1;
      target_flags |= TARGET_MASK_SMART_IO;
    }
  } else {
    /* set the default smartness level */
    pic30_io_size_val = 1;
    target_flags |= TARGET_MASK_SMART_IO;
  }
  if (pic30_errata) {
    const char *errata = pic30_errata;
    char save;
    char *c;
    int errata_num;

    while (*errata) {
      for (c = (char *) errata; *c && *c != ' ' && *c != ','; c++);
      save = *c;
      *c = 0;
      if (strcmp(errata, "list") == 0) {
        fprintf(stderr,"Supported -merrata= errata\n");
        for (errata_num = 0; errata_map[errata_num].name; errata_num++) {
          fprintf(stderr,"  %s\t%s", errata_map[errata_num].name,
                                     errata_map[errata_num].description);
        }
      } else if (strcmp(errata, "all") == 0) {
        for (errata_num = 0; errata_map[errata_num].name; errata_num++) {
          if (errata_map[errata_num].conflicts_with & pic30_errata_mask) {
            warning("Not enabling errata '%s' due to prior conflict\n",
                     (errata_map[errata_num].name));
          } else pic30_errata_mask |= errata_map[errata_num].mask;
        }
        break;
      } else {
        for (errata_num = 0; errata_map[errata_num].name; errata_num++) {
          if (strcmp(errata_map[errata_num].name, errata) == 0) {
            if (errata_map[errata_num].conflicts_with & pic30_errata_mask) {
              warning("Not enabling errata '%s' due to prior conflict\n",
                       (errata_map[errata_num].name));
            } else pic30_errata_mask |= errata_map[errata_num].mask;
            break;
          }
        }
        if (errata_map[errata_num].name == 0) {
          warning("-merrata=%s invalid; unrecognized errata name - ignoring",
                  errata);
        }
      }
      *c = save;
      errata = c;
      for (; *errata && (*errata == ' ' || *errata == ','); errata++);
    }
  }
  if (pic30_license_valid < 0) {
    /* an invalid license, 
       disable those optimizations turned on by -O2,-O3,-Os */
    static int message_displayed;
    char *invalid = "invalid";

    #define NULLIFY(X) \
    if ((X) && (message_displayed++ == 0)) \
      warning("Options have been disabled due to %s license", invalid); \
    X

    if ((pic30_license_valid == -253) ||
        (pic30_license_valid == -252))  invalid = "missing";
    else if (pic30_license_valid == -248) invalid = "expired";
    NULLIFY(flag_optimize_sibling_calls) = 0;
    NULLIFY(flag_cse_follow_jumps) = 0;
    NULLIFY(flag_cse_skip_blocks) = 0;
    NULLIFY(flag_gcse) = 0;
    NULLIFY(flag_expensive_optimizations) = 0;
    NULLIFY(flag_strength_reduce) = 0;
    NULLIFY(flag_rerun_cse_after_loop) = 0;
    NULLIFY(flag_rerun_loop_opt) = 0;
    NULLIFY(flag_caller_saves) = 0;
    NULLIFY(flag_force_mem) = 0;
    NULLIFY(flag_peephole2) = 0;
    NULLIFY(flag_schedule_insns) = 0;
    NULLIFY(flag_schedule_insns_after_reload) = 0;
    NULLIFY(flag_regmove) = 0;
    NULLIFY(flag_strict_aliasing) = 0;
    NULLIFY(flag_delete_null_pointer_checks) = 0;
    NULLIFY(flag_reorder_blocks) = 0;
    NULLIFY(flag_reorder_functions) = 0;
    NULLIFY(flag_inline_functions) = 0;
    NULLIFY(flag_rename_registers) = 0;
    NULLIFY(align_loops) = 0;
    NULLIFY(align_jumps) = 0;
    NULLIFY(align_labels) = 0;
    NULLIFY(align_functions) = 0;

    #undef NULLIFY
  }
  if (pic30_it_option) {
    /* enable instrumented trace */
    char *option,*c;
    char *option_arg=0;

    option = xstrdup(pic30_it_option);
#define LPAREN 0x28
#define RPAREN 0x29
    for (c = option; (*c) && (*c != LPAREN); c++);
    if (*c) {
      *c++ = 0;
      option_arg = c;
      for (; (*c) && (*c != RPAREN); c++);
      *c = 0;
    }
    pic30_it_option = option;
    pic30_it_option_arg = option_arg;
  }
}

/*
 *  in 2.95 this fn appeared in toplev.c, not in 3.xx
 */

#if !defined(__CYGWIN__)
static void
pfatal_with_name (const char *name)
{
  fprintf (stderr, "%s: ", progname);
  perror (name);
  exit (FATAL_EXIT_CODE);
}
#endif

/* 
 *  Sometimes the oldways are the best; stolen from varasm.c in 2.95
 */

static bool
pic30_assemble_integer(rtx x, unsigned int size, int aligned_p)
{

  /* First try to use the standard 1, 2, 4, 8, and 16 byte
     ASM_OUTPUT... macros.  */

    switch (size)
    {
#ifdef ASM_OUTPUT_CHAR
    case 1:
      ASM_OUTPUT_CHAR (asm_out_file, x);
      return 1;
#endif

    case 2:
    if (pic30_obj_elf_p() && !aligned_p)
    {
        fprintf(asm_out_file, "\t.2byte\t");
        output_addr_const(asm_out_file, x);
        fprintf(asm_out_file, "\n");
              return(1);
    }
#ifdef ASM_OUTPUT_SHORT
          ASM_OUTPUT_SHORT(asm_out_file, x);
          return(1);
#endif
          break;

    case 4:
    if (pic30_obj_elf_p() && !aligned_p)
    {
        fprintf(asm_out_file, "\t.4byte\t");
        output_addr_const(asm_out_file, x);
        fprintf(asm_out_file, "\n");
              return(1);
    }
#ifdef ASM_OUTPUT_INT
          ASM_OUTPUT_INT(asm_out_file, x);
          return(1);
#endif
          break;

#ifdef ASM_OUTPUT_DOUBLE_INT
    case 8:
      ASM_OUTPUT_DOUBLE_INT (asm_out_file, x);
      return 1;
#endif

#ifdef ASM_OUTPUT_QUADRUPLE_INT
    case 16:
      ASM_OUTPUT_QUADRUPLE_INT (asm_out_file, x);
      return 1;
#endif
    }
    return(0);
}

/* formerly macro STRIP_NAME_ENCODING;
   GCC is slowly moving away from using macro's to define target behaviour
    to using different macros to define behaviour.  The difference of course
    is that the different macros only pollute a small name space. (ha!)
*/

static const char *pic30_strip_name_encoding_helper(const char *symbol_name) {
  const char *var;
  int sz = 0;

  var = symbol_name;
  while ((sz = ENCODED_NAME_P(var))) {
    var = var + ENCODED_NAME_P(symbol_name);
    var += (*var == '*');
  }
  return var;
}

const char *pic30_strip_name_encoding(const char *symbol_name) {
  const char *var;
  char *sfr_match;
  pic30_interesting_fn *match;

  var = pic30_strip_name_encoding_helper(symbol_name);
  if (TARGET_ARCH(GENERIC) && 
      ((sfr_match = PIC30_HAS_NAME_P(symbol_name, PIC30_SFR_FLAG)) != 0)) {
    /* clear the SFR designator (it is also marked NEAR) so that this message
       is only displayed once per symbol */
    for (sfr_match++; *sfr_match != PIC30_EXTENDED_FLAG[0]; sfr_match++) 
      *sfr_match=' ';
    warning("'%s' represents a symbol in SFR space\n\t and may be "
            "in-appropriate for the generic cpu\n", var);
  }
  if (TARGET_SMART_IO) {
    match = pic30_match_conversion_fn(var);
    while (match) {
      if (match->function_convertable) {
        return match->map_to;
      }
      if (match[1].name && 
          (strcmp(match[1].name,var) == 0)) match++; else match = 0;
    }
  }
  return var;
}

/* formerly macro ASM_GLOBALIZE_LABEL */
static void
pic30_globalize_label(FILE *f, const char *l)
{
   fputs("\t.global\t", f);
   assemble_name(f, l);
   fputs("\t; export\n", f);
}

/*
 *  Initialize symbol names for any builtin function and other symbols that
 *    that we may need
 */
static void
pic30_init_builtins(void)
{
  tree fn_type;
  tree argtype;
  tree p0_type;
  tree p1_type;
  tree p2_type;

  fn_type = build_function_type(void_type_node, void_type_node);
  builtin_function("save", fn_type, CODE_FOR_nop, NOT_BUILT_IN, 0, 0);
  builtin_function("__save__", fn_type, CODE_FOR_nop, NOT_BUILT_IN, 0, 0);
  builtin_function("irq", fn_type, CODE_FOR_nop, NOT_BUILT_IN, 0, 0);
  builtin_function("__irq__", fn_type, CODE_FOR_nop, NOT_BUILT_IN, 0, 0);
  builtin_function("preprologue", fn_type, CODE_FOR_nop, NOT_BUILT_IN, 0,0);
  builtin_function("__preprologue__", fn_type, CODE_FOR_nop, NOT_BUILT_IN, 0,0);
  builtin_function("altirq", fn_type, CODE_FOR_nop, NOT_BUILT_IN, 0, 0);
  builtin_function("__altirq__", fn_type, CODE_FOR_nop, NOT_BUILT_IN, 0, 0);

  fn_type = build_function_type(unsigned_type_node, void_type_node);
  builtin_function("__builtin_ittype", fn_type, PIC30_BUILTIN_ITTYPE, 
                   BUILT_IN_MD, NULL, NULL_TREE);

  builtin_function("__builtin_unique_id", fn_type, PIC30_BUILTIN_UNIQUEID,
                   BUILT_IN_MD, NULL, NULL_TREE);

  fn_type = build_function_type_list(void_type_node, unsigned_type_node, 
                                     NULL_TREE);
  builtin_function("__builtin_write_OSCCONL", fn_type, 
                   PIC30_BUILTIN_WRITEOSCCONL, BUILT_IN_MD, NULL, NULL_TREE);
  builtin_function("__builtin_write_OSCCONH", fn_type, 
                   PIC30_BUILTIN_WRITEOSCCONH, BUILT_IN_MD, NULL, NULL_TREE);

  fn_type = build_function_type_list(void_type_node, void_type_node, NULL_TREE);
  builtin_function("__builtin_write_NVM", fn_type, 
                   PIC30_BUILTIN_WRITENVM, BUILT_IN_MD, NULL, NULL_TREE);

  /*
  ** builtin for safe reading of an SFR.
  ** This compensates for a silicon erratum.
  */
  argtype = build_qualified_type(void_type_node, TYPE_QUAL_VOLATILE);
  argtype = build_pointer_type(argtype);
  fn_type = build_function_type(unsigned_type_node,
             tree_cons(NULL_TREE, argtype, void_list_node));
  builtin_function("__builtin_readsfr", fn_type,
          PIC30_BUILTIN_READSFR, BUILT_IN_MD, NULL, NULL_TREE);

  /*
  ** builtin for safe reading of an SFR.
  ** This compensates for a silicon erratum.
  */
  p0_type = build_qualified_type(void_type_node, TYPE_QUAL_VOLATILE);
  p0_type = build_pointer_type(p0_type);
  fn_type = build_function_type_list(void_type_node, p0_type, 
                                     unsigned_type_node, NULL_TREE);
  builtin_function("__builtin_writesfr", fn_type,
        PIC30_BUILTIN_WRITESFR, BUILT_IN_MD, NULL, NULL_TREE);

  /*
  ** builtins for tblpage(), psvpage(), tbloffset(), psvoffset(), dmaoffset()
  */
  argtype = build_qualified_type(void_type_node, TYPE_QUAL_CONST);
  argtype = build_pointer_type(argtype);
  fn_type = build_function_type(unsigned_type_node,
             tree_cons(NULL_TREE, argtype, void_list_node));

  builtin_function("__builtin_tblpage", fn_type,
          PIC30_BUILTIN_TBLPAGE, BUILT_IN_MD, NULL, NULL_TREE);
  builtin_function("__builtin_tbloffset", fn_type,
          PIC30_BUILTIN_TBLOFFSET, BUILT_IN_MD, NULL, NULL_TREE);
  builtin_function("__builtin_psvpage", fn_type,
          PIC30_BUILTIN_PSVPAGE, BUILT_IN_MD, NULL, NULL_TREE);
  builtin_function("__builtin_psvoffset", fn_type,
          PIC30_BUILTIN_PSVOFFSET, BUILT_IN_MD, NULL, NULL_TREE);
  builtin_function("__builtin_dmaoffset", fn_type,
          PIC30_BUILTIN_DMAOFFSET, BUILT_IN_MD, NULL, NULL_TREE);

  /*
  ** builtins for zero-operand machine instructions
  */
  fn_type = build_function_type(void_type_node, void_list_node);
  builtin_function("__builtin_nop", fn_type,
          PIC30_BUILTIN_NOP, BUILT_IN_MD, NULL, NULL_TREE);

  /*
  ** builtins for 32/16
  */

  /*
  ** int
  ** __builtin_divsd(const long num, const int den);
  ** 
  ** and friends
  */
  p0_type = build_qualified_type(long_integer_type_node, TYPE_QUAL_CONST);
  p1_type = build_qualified_type(integer_type_node, TYPE_QUAL_CONST);
  fn_type = build_function_type_list(integer_type_node,
                      p0_type, p1_type, NULL_TREE);
  builtin_function("__builtin_divsd", fn_type,
          PIC30_BUILTIN_DIVSD, BUILT_IN_MD, NULL, NULL_TREE);
  builtin_function("__builtin_modsd", fn_type,
          PIC30_BUILTIN_MODSD, BUILT_IN_MD, NULL, NULL_TREE);

  p0_type = build_qualified_type(long_integer_type_node, TYPE_QUAL_CONST);
  p1_type = build_qualified_type(integer_type_node, TYPE_QUAL_CONST);
  p2_type = build_pointer_type(integer_type_node);

  fn_type = build_function_type_list(integer_type_node,
                      p0_type, p1_type, p2_type, NULL_TREE);
  builtin_function("__builtin_divmodsd", fn_type,
          PIC30_BUILTIN_DIVMODSD, BUILT_IN_MD, NULL, NULL_TREE);

  /*
  ** unsigned int
  ** __builtin_divud(const unsigned long num, const unsigned int den);
  ** 
  ** and friends
  */
  p0_type = build_qualified_type(long_unsigned_type_node, TYPE_QUAL_CONST);
  p1_type = build_qualified_type(unsigned_type_node, TYPE_QUAL_CONST);
  fn_type = build_function_type_list(unsigned_type_node,
                      p0_type, p1_type, NULL_TREE);
  builtin_function("__builtin_divud", fn_type,
          PIC30_BUILTIN_DIVUD, BUILT_IN_MD, NULL, NULL_TREE);
  builtin_function("__builtin_modud", fn_type,
          PIC30_BUILTIN_MODUD, BUILT_IN_MD, NULL, NULL_TREE);

  p0_type = build_qualified_type(long_unsigned_type_node, TYPE_QUAL_CONST);
  p1_type = build_qualified_type(unsigned_type_node, TYPE_QUAL_CONST);
  p2_type = build_pointer_type(unsigned_type_node);
  fn_type = build_function_type_list(unsigned_type_node,
                      p0_type, p1_type, p2_type, NULL_TREE);
  builtin_function("__builtin_divmodud", fn_type,
          PIC30_BUILTIN_DIVMODUD, BUILT_IN_MD, NULL, NULL_TREE);


  /*****************************
  ** builtins for 16/16 -> 32 **
  *****************************/

  /*
  ** signed long
  ** __builtin_mulss(const signed int p0, const signed int p1);
  */
  p0_type = build_qualified_type(integer_type_node, TYPE_QUAL_CONST);
  p1_type = build_qualified_type(integer_type_node, TYPE_QUAL_CONST);
  fn_type = build_function_type_list(long_integer_type_node,
                      p0_type, p1_type, NULL_TREE);
  builtin_function("__builtin_mulss", fn_type,
          PIC30_BUILTIN_MULSS, BUILT_IN_MD, NULL, NULL_TREE);
  /*
  ** unsigned long
  ** __builtin_muluu(const unsigned int p0, const unsigned int p1);
  */
  p0_type = build_qualified_type(unsigned_type_node, TYPE_QUAL_CONST);
  p1_type = build_qualified_type(unsigned_type_node, TYPE_QUAL_CONST);
  fn_type = build_function_type_list(long_unsigned_type_node,
                      p0_type, p1_type, NULL_TREE);
  builtin_function("__builtin_muluu", fn_type,
          PIC30_BUILTIN_MULUU, BUILT_IN_MD, NULL, NULL_TREE);
  /*
  ** signed long
  ** __builtin_mulsu(const signed int p0, const unsigned int p1);
  */
  p0_type = build_qualified_type(integer_type_node, TYPE_QUAL_CONST);
  p1_type = build_qualified_type(unsigned_type_node, TYPE_QUAL_CONST);
  fn_type = build_function_type_list(long_integer_type_node,
                      p0_type, p1_type, NULL_TREE);
  builtin_function("__builtin_mulsu", fn_type,
          PIC30_BUILTIN_MULSU, BUILT_IN_MD, NULL, NULL_TREE);
  /*
  ** signed long
  ** __builtin_mulus(const unsigned int p0, const signed int p1);
  */
  p0_type = build_qualified_type(unsigned_type_node, TYPE_QUAL_CONST);
  p1_type = build_qualified_type(integer_type_node, TYPE_QUAL_CONST);
  fn_type = build_function_type_list(long_integer_type_node,
                      p0_type, p1_type, NULL_TREE);
  builtin_function("__builtin_mulus", fn_type,
          PIC30_BUILTIN_MULUS, BUILT_IN_MD, NULL, NULL_TREE);

  /*
   *  builtin_bit_toggle
   */
  p0_type = build_pointer_type(unsigned_type_node);
  p1_type = build_qualified_type(unsigned_type_node, TYPE_QUAL_CONST);
  fn_type = build_function_type_list(void_type_node, p0_type, 
                                     p1_type, NULL_TREE);
  builtin_function("__builtin_btg", fn_type, PIC30_BUILTIN_BTG, 
                   BUILT_IN_MD, NULL, NULL_TREE);

  /*
   * DSP builtins
   */
  fn_type = build_function_type_list(integer_type_node,
                                     void_type_node, NULL_TREE);
  builtin_function("__builtin_addab", fn_type,
                  PIC30_BUILTIN_ADDAB, BUILT_IN_MD, NULL, NULL_TREE);

  fn_type = build_function_type_list(integer_type_node,
                                     integer_type_node, integer_type_node,
                                     NULL_TREE);
  builtin_function("__builtin_add", fn_type,
                   PIC30_BUILTIN_ADD, BUILT_IN_MD, NULL, NULL_TREE);


  fn_type = build_function_type_list(integer_type_node,
                                     void_type_node, NULL_TREE);
  builtin_function("__builtin_clr", fn_type,
                 PIC30_BUILTIN_CLR, BUILT_IN_MD, NULL, NULL_TREE);
 
  p0_type = build_pointer_type(integer_type_node);
  p1_type = build_pointer_type(p0_type);
  fn_type = build_function_type_list(integer_type_node, 
                                     p1_type, p0_type, integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     p0_type, NULL_TREE);
  builtin_function("__builtin_clr_prefetch", fn_type, 
                   PIC30_BUILTIN_CLR_PREFETCH, BUILT_IN_MD, NULL, NULL_TREE);

  p0_type = build_pointer_type(integer_type_node);
  p1_type = build_pointer_type(p0_type);
  fn_type = build_function_type_list(integer_type_node,
                                     integer_type_node, 
                                     p1_type, integer_type_node,
                                     p1_type, integer_type_node,
                                     p0_type, NULL_TREE);
  builtin_function("__builtin_ed", fn_type,
                   PIC30_BUILTIN_ED, BUILT_IN_MD, NULL, NULL_TREE);

  /* same type as __builtin_ed */
  builtin_function("__builtin_edac", fn_type,
                   PIC30_BUILTIN_EDAC, BUILT_IN_MD, NULL, NULL_TREE);

  fn_type = build_function_type_list(integer_type_node,
                                     integer_type_node, NULL_TREE);
  builtin_function("__builtin_fbcl", fn_type,
                   PIC30_BUILTIN_FBCL, BUILT_IN_MD, NULL, NULL_TREE);

  fn_type = build_function_type_list(integer_type_node,
                                     integer_type_node, integer_type_node,
                                     NULL_TREE);
  builtin_function("__builtin_lac", fn_type,
                   PIC30_BUILTIN_LAC, BUILT_IN_MD, NULL, NULL_TREE);

  p0_type = build_pointer_type(integer_type_node);
  p1_type = build_pointer_type(p0_type);
  fn_type = build_function_type_list(integer_type_node,
                                     integer_type_node, integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     p0_type, NULL_TREE);
  builtin_function("__builtin_mac", fn_type, 
                   PIC30_BUILTIN_MAC, BUILT_IN_MD, NULL, NULL_TREE);

  p0_type = build_pointer_type(integer_type_node);
  p1_type = build_pointer_type(p0_type);
  fn_type = build_function_type_list(integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     p0_type, NULL_TREE);
  builtin_function("__builtin_movsac", fn_type,
                   PIC30_BUILTIN_MOVSAC, BUILT_IN_MD, NULL, NULL_TREE);

  p0_type = build_pointer_type(integer_type_node);
  p1_type = build_pointer_type(p0_type);
  fn_type = build_function_type_list(integer_type_node,
                                     integer_type_node, integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     NULL_TREE);
  builtin_function("__builtin_mpy", fn_type,
                   PIC30_BUILTIN_MPY, BUILT_IN_MD, NULL, NULL_TREE);

  p0_type = build_pointer_type(integer_type_node);
  p1_type = build_pointer_type(p0_type);
  fn_type = build_function_type_list(integer_type_node,
                                     integer_type_node, integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     NULL_TREE);
  builtin_function("__builtin_mpyn", fn_type,
                   PIC30_BUILTIN_MPYN, BUILT_IN_MD, NULL, NULL_TREE);

  p0_type = build_pointer_type(integer_type_node); 
  p1_type = build_pointer_type(p0_type);
  fn_type = build_function_type_list(integer_type_node,
                                     integer_type_node, integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     NULL_TREE); 

  p0_type = build_pointer_type(integer_type_node);
  p1_type = build_pointer_type(p0_type);
  fn_type = build_function_type_list(integer_type_node,
                                     integer_type_node, integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     p1_type, p0_type, integer_type_node,
                                     p0_type, NULL_TREE);
  builtin_function("__builtin_msc", fn_type,
                   PIC30_BUILTIN_MSC, BUILT_IN_MD, NULL, NULL_TREE);

  fn_type = build_function_type_list(integer_type_node,
                                     integer_type_node,
                                     integer_type_node, NULL_TREE);
  builtin_function("__builtin_sac", fn_type,
                   PIC30_BUILTIN_SAC, BUILT_IN_MD, NULL, NULL_TREE);

  fn_type = build_function_type_list(integer_type_node,
                                     integer_type_node,
                                     integer_type_node, NULL_TREE);
  builtin_function("__builtin_sacr", fn_type,
                   PIC30_BUILTIN_SACR, BUILT_IN_MD, NULL, NULL_TREE);

  fn_type = build_function_type_list(integer_type_node,
                                     integer_type_node, NULL_TREE);
  builtin_function("__builtin_sftac", fn_type,
                   PIC30_BUILTIN_SFTAC, BUILT_IN_MD, NULL, NULL_TREE);

  fn_type = build_function_type_list(integer_type_node,
                                     void_type_node, NULL_TREE);
  builtin_function("__builtin_subab", fn_type,
                  PIC30_BUILTIN_SUBAB, BUILT_IN_MD, NULL, NULL_TREE);

  fn_type = build_function_type_list(void_type_node, integer_type_node, 
                                     NULL_TREE);
  builtin_function("__builtin_disi", fn_type, 
                   PIC30_BUILTIN_DISI, BUILT_IN_MD, NULL, NULL_TREE);
}

/*
** Determine the section name of an operand
*/
static const char *
pic30_section_name(rtx op)
{
  const char *pszSectionName = NULL;
  const char *real_name;
  tree sym;
  tree sectname = 0;

  switch (GET_CODE (op))
  {
    case LABEL_REF:
    case SYMBOL_REF:
      real_name = pic30_strip_name_encoding_helper(XSTR(op,0));
      sym = maybe_get_identifier(real_name);
      if (sym == 0)
      {
        return(0);
      }
      sym = lookup_name(sym);
      if (sym == 0)
      {
        return(0);
      }
      switch (TREE_CODE(sym))
      {
        case FUNCTION_DECL:
        case VAR_DECL:
          sectname = DECL_SECTION_NAME(sym);
          if (sectname && STRING_CST_CHECK(sectname))
          {
            pszSectionName = TREE_STRING_POINTER(sectname);
          }
          break;
        default: break;
      }
      break;
    case PLUS:
      /* Assume canonical format of symbol + constant.  Fall through.  */
    case CONST:
      return(pic30_section_name(XEXP(op, 0)));
    default: break;
  }
  return(pszSectionName);
}

static tree pic30_pointer_expr(tree arg) {
  int i;

  switch (TREE_CODE(arg)) {
    case INTEGER_CST:
    case REAL_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
    case STRING_CST:
      return 0;
    case ADDR_EXPR:
      if (TREE_CODE(TREE_OPERAND(arg,0)) == VAR_DECL) 
        return TREE_OPERAND(arg,0);
    default:
      if (TREE_CODE(TREE_TYPE(arg)) == POINTER_TYPE) return arg;
      break;
  }
  for (i = 0; i < TREE_CODE_LENGTH(TREE_CODE(arg)); i++) {
    tree parg = pic30_pointer_expr(TREE_OPERAND(arg,i));
    if (parg) return parg;
  }
  return 0;
}

/*
** Determine if a parameter is suitable as an argument to
** the builtin table and psv instructions.
*/
static int
pic30_builtin_tblpsv_arg_p(tree arg0 ATTRIBUTE_UNUSED, rtx r0)
{ int p = 0;

  if ((pic30_program_space_operand_p(r0)) ||
      (pic30_has_space_operand_p(r0, (char *) PIC30_APSV_FLAG)) ||
      (pic30_has_space_operand_p(r0, (char *) PIC30_PSV_FLAG)) ||
      (pic30_has_space_operand_p(r0, (char *) PIC30_EE_FLAG)))
  {
    p = 1;
  }
  if (!p) {
    /*
     ** Check for explicit 'read-only' sections
     */
    const char *pszSectionName;

    pszSectionName = pic30_section_name(r0);
    if (pszSectionName)
    { SECTION_FLAGS_INT f;

      f = pic30_section_type_flags(NULL, pszSectionName, 1);
      if ((f & SECTION_EEDATA) ||
          (f & SECTION_PSV) ||
          (f & SECTION_READ_ONLY)) {
          p = 1;
      }
    }
  }
  return(p);
}

/*
** Determine if a parameter is suitable as an argument to
** the builtin dma instrucitons
*/
static int
pic30_builtin_dma_arg_p(tree arg0 ATTRIBUTE_UNUSED, rtx r0)
{ int p = 0;

  if (pic30_has_space_operand_p(r0, (char *) PIC30_DMA_FLAG))
  {
    p = 1;
  }
  if (!p) {
    /*
     ** Check for explicit 'dma' sections
     */
    const char *pszSectionName;

    pszSectionName = pic30_section_name(r0);
    if (pszSectionName)
    { SECTION_FLAGS_INT f;

      f = pic30_section_type_flags(NULL, pszSectionName, 1);
      if (f & SECTION_DMA) {
          p = 1;
      }
    }
  }
  return(p);
}

/* return true if reg is a reg suitable for a builtin that will require
   a machine register */
static int pic30_reg_for_builtin(rtx reg) {

  if (GET_CODE(reg) == REG) {
    int regno = REGNO(reg);
    
    if ((regno < FIRST_VIRTUAL_REGISTER) || (regno > LAST_VIRTUAL_REGISTER))
      return 1;
  }
  return 0;
}

/*
** Expand a call to a machine specific built-in function that was set up by
** TARGET_INIT_BUILTINS.
** <exp> is the expression for the function call; the result should go to
** <target> if that is convenient, and have mode <mode> if that is convenient.
** <subtarget> may be used as the target for computing one of exp's operands.
** <ignore> is nonzero if the value is to be ignored.
** This function should return the result of the call to the built-in function.
*/
rtx
pic30_expand_builtin (exp, target, subtarget, mode, ignore)
     tree exp;
     rtx target;
     rtx subtarget ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     int ignore ATTRIBUTE_UNUSED;
{
  tree fndecl = TREE_OPERAND(TREE_OPERAND(exp, 0), 0);
  unsigned int fcode = DECL_FUNCTION_CODE(fndecl);
  tree arglist = TREE_OPERAND(exp, 1);
  tree arg0;
  tree arg1;
  tree arg2;
  tree arg3;
  tree arg4;
  tree arg5;
  tree arg6;
  tree arg7;
  tree arg8;
  rtx r0;
  rtx r1;
  rtx r2;
  rtx r3;
  rtx r4;
  rtx r5;
  rtx r6;
  rtx r7;
  rtx r8;
  char *id = 0;

  switch (fcode)
  {
    case PIC30_BUILTIN_WRITEOSCCONL:
    case PIC30_BUILTIN_WRITEOSCCONH: {
      arg0 = TREE_VALUE(arglist);

      r0 = gen_reg_rtx(HImode);
      r1 = gen_reg_rtx(HImode);
      r2 = expand_expr(arg0, NULL_RTX, QImode, EXPAND_NORMAL);
      r2 = protect_from_queue(r2,0);
      if (!pic30_register_operand(r2,HImode)) {
        r3 = r2;
        r2 = gen_reg_rtx(HImode);
        emit_move_insn(r2,r3);
      }
      r3 = gen_reg_rtx(HImode);
      if (fcode == PIC30_BUILTIN_WRITEOSCCONL) {
        emit_move_insn(r1,GEN_INT(0x46));
        emit_move_insn(r3,GEN_INT(0x57));
        emit_insn(gen_write_oscconl(r0,r1,r3,r2));
      } else {
        emit_move_insn(r1,GEN_INT(0x78));
        emit_move_insn(r3,GEN_INT(0x9a));
        emit_insn(gen_write_oscconh(r0,r1,r3,r2));
      }
      break;
    }

    case PIC30_BUILTIN_WRITENVM:
      r0 = gen_reg_rtx(HImode);
      emit_insn(gen_write_nvm(r0));
      break;

    case PIC30_BUILTIN_UNIQUEID: {
      char *label_name = 0;
      char *fmt = 0;

      arg0 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      if (TREE_CODE(arg0) == NOP_EXPR) {
        tree sub_arg0 = TREE_OPERAND(arg0,0);

        if (TREE_CODE(sub_arg0) == ADDR_EXPR) {
          sub_arg0 = TREE_OPERAND(sub_arg0,0);

          if (TREE_CODE(sub_arg0) == STRING_CST) {
            if (TREE_STRING_LENGTH(sub_arg0) > 20) {
               error("__builtin_unique_id arguement 0 exceeds maximum length");
            }
            label_name = xmalloc(TREE_STRING_LENGTH(sub_arg0) + 5);
            fmt = TREE_STRING_POINTER(sub_arg0);
          }
        }
      }
      if (fmt == 0) {
        error("__builtin_unique_id requires string literal for argument 0");
        return GEN_INT(0);
      }
      r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
      r1 = protect_from_queue(r1, 0);
      if (GET_CODE(r1) != CONST_INT) {
        error("__builtin_unique_id requires integer literal for argument 1");
        return GEN_INT(0);
      }
      sprintf(label_name,"%s_%d", fmt, INTVAL(r1));
      if (!target || !register_operand(target, HImode)) {
        target = gen_reg_rtx(HImode);
      }
      emit_insn(gen_unique_id(target,GEN_INT(label_name), r1));
      return target;
    }

    case PIC30_BUILTIN_ITTYPE: {
      int result = 0;
      tree type;

      arg0 = TREE_VALUE(arglist);
      type = TREE_TYPE(arg0);
      switch (TREE_CODE(type)) {
        case INTEGER_TYPE:
          if (TREE_CODE(arg0) == NOP_EXPR) {
            /* a conversion */
            tree sub_arg0 = TREE_OPERAND(arg0,0);
 
            type = TREE_TYPE(sub_arg0);
          }
          if (TREE_UNSIGNED(type)) 
            result |= (1 << 4);
          break;
        case REAL_TYPE:
          result |= (1 << 3);
          break;
        case CHAR_TYPE:
          if (TREE_UNSIGNED(type)) 
            result |= (1 << 4);
          break;
        case POINTER_TYPE:
          result |= (1 << 5);
          break;
        default:  error("__builtin_ittype() cannot accept an aggregate type");
      }
      result |= (TYPE_PRECISION(type) / BITS_PER_UNIT) - 1;
      return GEN_INT(result);
    }

    case PIC30_BUILTIN_READSFR:
    /*
    ** unsigned int __builtin_readsfr(volatile void *);
    */

      arg0 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      if (!register_operand(r0, HImode))
      { 
        r0 = copy_to_mode_reg(HImode, r0);
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      emit_insn(gen_readsfr(target, r0));
      return(target);

    case PIC30_BUILTIN_WRITESFR:
    /*
    ** void __builtin_writesfr(volatile void *, unsigned int);
    */

      arg0 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      if (!register_operand(r0, HImode))
      { 
        r0 = copy_to_mode_reg(HImode, r0);
      }
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
      r1 = protect_from_queue(r1, 0);
      if (!register_operand(r1, HImode))
      { 
        r1 = copy_to_mode_reg(HImode, r1);
      }
      emit_insn(gen_writesfr(r0, r1));
      return NULL_RTX;

    case PIC30_BUILTIN_TBLPAGE:
      arg0 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      if (!pic30_builtin_tblpsv_arg_p(arg0, r0))
      {
        error("Argument to __builtin_tblpage() is not the address of an object"
              " in a code, psv, or eedata section");
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      emit_insn(gen_tblpage(target, r0));
      return(target);

    case PIC30_BUILTIN_TBLOFFSET:
      arg0 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      if (!pic30_builtin_tblpsv_arg_p(arg0, r0))
      {
        error("Argument to __builtin_tbloffset() is not the address of an"
              " object in a code, psv, or eedata section");
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      emit_insn(gen_tbloffset(target, r0));
      return(target);

    case PIC30_BUILTIN_PSVPAGE:
      arg0 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      if (!pic30_builtin_tblpsv_arg_p(arg0, r0))
      {
        error("Argument to __builtin_psvpage() is not the address of an object"
              " in a code, psv, or eedata section");
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      emit_insn(gen_psvpage(target, r0));
      return(target);

    case PIC30_BUILTIN_PSVOFFSET:
      arg0 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      if (!pic30_builtin_tblpsv_arg_p(arg0, r0))
      {
        error("Argument to __builtin_psvoffset()  is not the address of an"
              " object in a code, psv, or eedata section");
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      emit_insn(gen_psvoffset(target, r0));
      return(target);

    case PIC30_BUILTIN_DMAOFFSET:
      if (!((target_flags & TARGET_MASK_ARCH_PIC24H) ||
           (target_flags & TARGET_MASK_ARCH_PIC33))) {
        error("__builtin_dmaoffset() is not supported on this target");
        return NULL_RTX;
      }
      arg0 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      if (!pic30_builtin_dma_arg_p(arg0, r0))
      {
        error("Argument to __builtin_dmaoffset()  is not the address of an"
              " object in a dma section");
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      emit_insn(gen_dmaoffset(target, r0));
      return(target);

    case PIC30_BUILTIN_NOP:
      emit_insn(gen_bifnop());
      return(NULL_RTX);

    case PIC30_BUILTIN_MODSD:
    case PIC30_BUILTIN_DIVMODSD:
    case PIC30_BUILTIN_MODUD:
    case PIC30_BUILTIN_DIVMODUD:
    case PIC30_BUILTIN_DIVSD:
      /*
      ** int
      ** __builtin_divsd(const long num, const int den);
      */

    case PIC30_BUILTIN_DIVUD:
      /*
      ** unsigned int
      ** __builtin_divud(const unsigned long num, const unsigned int den);
      */

      arg0 = TREE_VALUE(arglist);
      if (arg0 == error_mark_node)
      {
        return(const0_rtx);
      }
      r0 = expand_expr(arg0, NULL_RTX, SImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      if (!register_operand(r0, SImode))
      {
        r0 = copy_to_mode_reg(SImode, r0);
      }
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      if (arg1 == error_mark_node)
      {
        return(const0_rtx);
      }
      r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
      r1 = protect_from_queue(r1, 0);
      if (!register_operand(r1, HImode))
      {
        r1 = copy_to_mode_reg(HImode, r1);
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      if ((fcode == PIC30_BUILTIN_DIVMODSD) || 
          (fcode == PIC30_BUILTIN_DIVMODUD)) {
        arglist = TREE_CHAIN(arglist);
        arg2 = TREE_VALUE(arglist);
        if (arg2 == error_mark_node) return const0_rtx;
        r2 = expand_expr(arg2, NULL_RTX, HImode, EXPAND_NORMAL);
        r2 = protect_from_queue(r2,0);
        r3 = gen_reg_rtx(Pmode);
        emit_move_insn(r3,r2);
        r2 = r3;
        if (fcode == PIC30_BUILTIN_DIVMODSD) {
          emit_insn(gen_divmodsd(target, r0, r1, r2));
        } else {
          emit_insn(gen_divmodud(target, r0, r1, r2));
        }
      } else if (fcode == PIC30_BUILTIN_DIVSD)
      {
        emit_insn(gen_divsd(target, r0, r1));
      }
      else if (fcode == PIC30_BUILTIN_DIVUD)
      {
        emit_insn(gen_divud(target, r0, r1));
      } else if (fcode == PIC30_BUILTIN_MODSD) {
        emit_insn(gen_modsd(target, r0, r1));
      } else if (fcode == PIC30_BUILTIN_MODUD) {
        emit_insn(gen_modud(target, r0, r1));
      } else error("Invalid __builtin\n");
      return(target);

    case PIC30_BUILTIN_MULSS:
      /*
      ** signed long
      ** __builtin_mulss(const signed int p0, const signed int p1);
      */

    case PIC30_BUILTIN_MULUU:
      /*
      ** unsigned long
      ** __builtin_muluu(const unsigned int p0, const unsigned int p1);
      */

    case PIC30_BUILTIN_MULSU:
      /*
      ** signed long
      ** __builtin_mulsu(const signed int p0, const unsigned int p1);
      */

    case PIC30_BUILTIN_MULUS:
      /*
      ** signed long
      ** __builtin_mulus(const unsigned int p0, const signed int p1);
      */
      if (!target || !register_operand(target, SImode))
      {
        target = gen_reg_rtx(SImode);
      }
      arg0 = TREE_VALUE(arglist);
      if (arg0 == error_mark_node)
      {
        return(const0_rtx);
      }
      r0 = expand_expr(arg0, NULL_RTX, SImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      if (!register_operand(r0, HImode))
      {
        r0 = copy_to_mode_reg(HImode, r0);
      }
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      if (arg1 == error_mark_node)
      {
        return(const0_rtx);
      }
      r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
      r1 = protect_from_queue(r1, 0);
      switch (fcode)
      {
        case PIC30_BUILTIN_MULSU:
          if (!pic30_mode2_or_P_operand(r1, HImode))
          {
            r1 = copy_to_mode_reg(HImode, r1);
          }
          emit_insn(gen_mulsu(target, r0, r1));
          break;

        case PIC30_BUILTIN_MULUU:
          if (!pic30_mode2_or_P_operand(r1, HImode))
          {
            r1 = copy_to_mode_reg(HImode, r1);
          }
          emit_insn(gen_muluu(target, r0, r1));
          break;

        case PIC30_BUILTIN_MULSS:
          if (!pic30_mode2_operand(r1, HImode))
          {
            r1 = copy_to_mode_reg(HImode, r1);
          }
          emit_insn(gen_mulss(target, r0, r1));
          break;

        case PIC30_BUILTIN_MULUS:
          if (!pic30_mode2_operand(r1, HImode))
          {
            r1 = copy_to_mode_reg(HImode, r1);
          }
          emit_insn(gen_mulus(target, r0, r1));
          break;

        default:
          break;
      }
      return(target);

    case PIC30_BUILTIN_BTG: {
      int pointer = 0;
      int mode = HImode;
      rtx (*gen_mov)(rtx,rtx) = gen_movhi;
      rtx (*gen_bittog)(rtx,rtx,rtx) = gen_bittoghi;
      rtx (*gen_bittog_sfr)(rtx,rtx) = gen_bittoghi_sfr;

#if 0
      /* arg0 is the address of an expression - lets see if this is an address
         of a VAR_DECL which has been given an register */
#endif
      r0 = NULL_RTX;
      arg0 = TREE_VALUE(arglist);
      if (arg0 == error_mark_node) return NULL_RTX;
      if (TREE_CODE(arg0) == NOP_EXPR) {
        tree sub_arg0 = TREE_OPERAND(arg0, 0);

        if (TREE_CODE(sub_arg0) == ADDR_EXPR) {
          sub_arg0 = TREE_OPERAND(sub_arg0,0);

          if (TREE_CODE(sub_arg0) == VAR_DECL) {
            r0 = DECL_RTL(sub_arg0);
          }
        } else {
          pointer = (pic30_pointer_expr(sub_arg0) != 0);
        } 
      } else pointer = (pic30_pointer_expr(arg0) != 0); 
      if (!r0) r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      mode = GET_MODE(r0);
      if (arg1 == error_mark_node) return NULL_RTX;
      if ((TREE_CODE(arg1) == NOP_EXPR) || (TREE_CODE(arg1) == COMPONENT_REF)) {
        /* allow param 2 to be a field value, get the address from that ie:
           btg(&foo, foo.bit15); */
        if (TREE_CODE(arg1) == NOP_EXPR) arg1 = TREE_OPERAND(arg1, 0);
        if (TREE_CODE(arg1) == COMPONENT_REF) {
          arg1 = TREE_OPERAND(arg1, 1);
          if (TREE_CODE(arg1) != FIELD_DECL) return NULL_RTX;
          r1 = gen_rtx_CONST_INT(HImode, 1 << int_bit_position(arg1));
        } else {
          error("Unexpected second argument to __builtin_btg()");
          return NULL_RTX;
        }
      } else if (TREE_CODE(arg1) == INTEGER_CST) {
        /* allow param 2 to be an integer constant representing a bit position 
           btg(&foo,15); */
        unsigned int max = 15;

        if (mode == QImode) max = 7;
        else if (mode == SImode) max = 31;
        if ((TREE_INT_CST_HIGH(arg1) < 0) || (TREE_INT_CST_LOW(arg1) > max)) {
           error("Invalid range for second argument to __builtin_btg()");
        }
        r1 = gen_rtx_CONST_INT(HImode, 1 << TREE_INT_CST_LOW(arg1));
      } else {
        error("Unexpected second argument to __builtin_btg()");
        return NULL_RTX;
      }
      if (mode == QImode) {
        gen_mov = gen_movqi;
        gen_bittog = gen_bittogqi;
        gen_bittog_sfr = gen_bittogqi_sfr;
      } else if (mode == SImode) {
        gen_mov = gen_movsi;
        gen_bittog = gen_bittogsi;
        gen_bittog_sfr = gen_bittogsi_sfr;
      } else if (mode != HImode) {
        error("Invalid pointer type, must be pointer to char, int, or long");
      }
      if (pointer) {
        /* a pointer - copy its value into a register so that we can toggle 
           the contents  */
        rtx reg = gen_reg_rtx(mode);

        emit_insn(gen_mov(reg, r0));
        emit_insn(gen_bittog(gen_rtx_MEM(mode,reg),gen_rtx_MEM(mode,reg), r1));
        return NULL_RTX;
      } 
      if (GET_CODE(r0) == SYMBOL_REF) {
         if (pic30_neardata_space_operand_p(r0))
           emit_insn(gen_bittog_sfr(gen_rtx_MEM(mode,r0),r1));
         else {
           rtx reg = gen_reg_rtx(HImode);

           emit_insn(gen_movhi_address(reg, r0));
           emit_insn(gen_bittog(gen_rtx_MEM(mode,reg),
                     gen_rtx_MEM(mode,reg), r1));
         }
      }
      else if (GET_CODE(r0) == REG) {
         if (r0 == virtual_stack_vars_rtx) {
            /* btg cannot use indexed indirect addressing */
            rtx reg = gen_reg_rtx(mode);
            emit_insn(gen_mov(reg, r0));
            emit_insn(gen_bittog(reg,gen_rtx_MEM(mode,reg),r1));
            emit_insn(gen_mov(gen_rtx_MEM(mode,r0), reg));
         } else emit_insn(gen_bittog(r0,r0,r1));
      } else if (GET_CODE(r0) == MEM) {
        rtx inner = XEXP(r0,0);

        if (GET_CODE(inner) == SYMBOL_REF) {
          if (pic30_neardata_space_operand_p(inner))
            emit_insn(gen_bittog_sfr(r0,r1));
          else {
            rtx reg = gen_reg_rtx(HImode);

            emit_insn(gen_movhi_address(reg,r0));
            emit_insn(gen_bittog(gen_rtx_MEM(mode,reg),
                      gen_rtx_MEM(mode,reg),r1));
          }
        } else {
          emit_insn(gen_bittog(r0, r0, r1));
        }
      } else if (GET_CODE(r0) == ADDRESSOF) {
        emit_insn(gen_bittog(XEXP(r0,0),XEXP(r0,0),r1));
      }
      break;
    }

    case PIC30_BUILTIN_SUBAB:
      id = "subab";
    case PIC30_BUILTIN_ADDAB: {
      rtx (*gen)(rtx,rtx);
      rtx other_accumulator = NULL_RTX;

      if (id == 0) id = "addab";
      if (!((target_flags & TARGET_MASK_ARCH_PIC30) ||
           (target_flags & TARGET_MASK_ARCH_PIC33) ||
           (target_flags & TARGET_MASK_ARCH_PIC30F202X))) {
        error("__builtin_%s is not supported on this target", id);
        return NULL_RTX;
      }
      if (fcode == PIC30_BUILTIN_SUBAB) gen = gen_subac_hi; 
      else gen =gen_addac_hi;
      if (!target ||
          (GET_CODE(target) != REG) || (!IS_ACCUM_REG(REGNO(target)))) {
        error("result for __builtin_%s should be an accumulator register", id);
        return NULL_RTX;
      } else {
        other_accumulator = gen_raw_REG(HImode,OTHER_ACCUM_REG(REGNO(target)));
      }
      emit_insn(
        gen(target, other_accumulator)
      );
      return target;
    }

    case PIC30_BUILTIN_ADD: {
      rtx reg = NULL_RTX;

      id = "add";
      if (!((target_flags & TARGET_MASK_ARCH_PIC30) ||
           (target_flags & TARGET_MASK_ARCH_PIC33) ||
           (target_flags & TARGET_MASK_ARCH_PIC30F202X))) {
        error("__builtin_%s is not supported on this target", id);
        return NULL_RTX;
      }
      if (!target ||
          (GET_CODE(target) != REG) || (!IS_ACCUM_REG(REGNO(target)))) {
        error("result for __builtin_add should be an accumulator register");
        return NULL_RTX;
      }
      arg0 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0,0);
      if (!pic30_reg_for_builtin(r0)) {

        reg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(reg, r0)
        );
        r0 = reg;
      }
      r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
      r1 = protect_from_queue(r1,0);
      if ((GET_CODE(r1) != CONST_INT) || 
          (!CONST_OK_FOR_LETTER_P(INTVAL(r1),'Z'))) {
        error("parameter 2 for __builtin_add should fall in the literal "
              "range -8:7");
        return NULL_RTX;
      }
      emit_insn(
        (INTVAL(r1) < 0) ? gen_addacr_shiftlt_hi(target, r0, r1) :
        (INTVAL(r1) > 0) ? gen_addacr_shiftrt_hi(target, r0, r1) :
                           gen_addacr_noshift_hi(target, r0)
      );
      return target;
      break;
    }

    case PIC30_BUILTIN_CLR: {
      rtx scratch = gen_rtx_CONST_INT(HImode,0);

      if (!target ||
          (GET_CODE(target) != REG) || (!IS_ACCUM_REG(REGNO(target)))) {
        error("result for __builtin_clr should be an accumulator register");
      }
      emit_insn(
        gen_clrac_gen_hi(target,
                         scratch,
                         scratch,
                         scratch,
                         scratch,
                         scratch,
                         scratch,
                         scratch,
                         scratch)
      );
      return target;
      break;
    }

    case PIC30_BUILTIN_CLR_PREFETCH: {
      rtx scratch = gen_rtx_CONST_INT(HImode,0);
      rtx p_xreg = NULL_RTX;
      rtx p_yreg = NULL_RTX;
      rtx d_xreg = NULL_RTX;
      rtx d_yreg = NULL_RTX;
      rtx awb = NULL_RTX;
      rtx awb_to;

      id = "clr_prefetch";
      if (!((target_flags & TARGET_MASK_ARCH_PIC30) ||
           (target_flags & TARGET_MASK_ARCH_PIC33) ||
           (target_flags & TARGET_MASK_ARCH_PIC30F202X))) {
        error("__builtin_%s is not supported on this target", id);
        return NULL_RTX;
      }
      arg0 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg2 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg3 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg4 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg5 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg6 = TREE_VALUE(arglist);
      r2 = NULL_RTX;
      r5 = NULL_RTX;

      if (!target ||
          (GET_CODE(target) != REG) || (!IS_ACCUM_REG(REGNO(target)))) {
        error("result for __builtin_clr_prefetch should be "
              "an accumulator register");
        awb_to = scratch;
      } else {
        awb_to = gen_raw_REG(HImode,OTHER_ACCUM_REG(REGNO(target)));
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r0) != CONST_INT) || (INTVAL(r0) != 0)) {
        /* contains an X prefetch */
        r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
        if ((GET_CODE(r1) == CONST_INT) && (INTVAL(r1) == 0)) {
          error("X prefetch destination to __builtin_clr_prefetch should not "
                "be NULL");
          return NULL_RTX;
        }
        r2 = expand_expr(arg2, NULL_RTX, HImode, EXPAND_NORMAL);
        r0 = protect_from_queue(r0, 0);
        r1 = protect_from_queue(r1, 0);
        p_xreg = gen_reg_rtx(Pmode);
        d_xreg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(p_xreg, gen_rtx_MEM(Pmode, r0))
        );
        if (GET_CODE(r2) != CONST_INT) {
          error("X increment to __builtin_clr_prefetch should be a literal");
          return NULL_RTX;
        }
        switch (INTVAL(r2)) {
          case 0:  r2 = NULL_RTX;
          case -6:
          case -4:
          case -2:
          case 2:
          case 4:
          case 6: break;
          default:
            error("X increment to __builtin_clr_prefetch is out of range");
            return NULL_RTX;
        }
      }
      r3 = expand_expr(arg3, NULL_RTX, HImode, EXPAND_NORMAL); 
      if ((GET_CODE(r3) != CONST_INT) || (INTVAL(r3) != 0)) {
        /* contains an Y prefetch */
        r4 = expand_expr(arg4, NULL_RTX, HImode, EXPAND_NORMAL);
        if ((GET_CODE(r4) == CONST_INT) && (INTVAL(r4) == 0)) {
          error("Y prefetch destination to __builtin_clr_prefetch should not "
                "be NULL");
          return NULL_RTX;
        }
        r5 = expand_expr(arg5, NULL_RTX, HImode, EXPAND_NORMAL);
        r4 = protect_from_queue(r4, 0);
        r5 = protect_from_queue(r5, 0);
        p_yreg = gen_reg_rtx(Pmode);
        d_yreg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(p_yreg, gen_rtx_MEM(Pmode, r3))
        );
        if (GET_CODE(r5) != CONST_INT) {
          error("Y increment to __builtin_clr_prefetch should be a literal");
          return NULL_RTX;
        }
        switch (INTVAL(r5)) {
          case 0:  r5 = NULL_RTX;
          case -6:
          case -4:
          case -2:
          case 2:
          case 4:
          case 6: break;
          default:
            error("Y increment to __builtin_clr_prefetch is out of range");
            return NULL_RTX;
        }
      }
      r6 = expand_expr(arg6, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r6) != CONST_INT) || (INTVAL(r6) != 0)) {
        r6 = protect_from_queue(r6,0);
        awb = gen_reg_rtx(HImode);
      } else awb_to = scratch;
      if (awb) {
        emit_insn(
          gen_clracawb_gen_hi(target,
                           d_xreg ? d_xreg : scratch,
                           p_xreg ? p_xreg : scratch,
                           p_xreg && (r2) ? p_xreg : scratch,
                           (r2)   ? r2 : gen_rtx_CONST_INT(HImode,0),
                           d_yreg ? d_yreg : scratch,
                           p_yreg ? p_yreg : scratch,
                           p_yreg && (r5) ? p_yreg : scratch,
                           (r5)   ? r5 : gen_rtx_CONST_INT(HImode,0),
                           awb,
                           awb_to)
        );
      } else emit_insn(
        gen_clrac_gen_hi(target, 
                         d_xreg ? d_xreg : scratch,
                         p_xreg ? p_xreg : scratch,
                         p_xreg && (r2) ? p_xreg : scratch,
                         (r2)   ? r2 : gen_rtx_CONST_INT(HImode,0),
                         d_yreg ? d_yreg : scratch,
                         p_yreg ? p_yreg : scratch,
                         p_yreg && (r5) ? p_yreg : scratch,
                         (r5)   ? r5 : gen_rtx_CONST_INT(HImode,0))
      );
      if (p_xreg && r2) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r0), p_xreg)
        );
      }
      if (p_yreg && r5) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r3), p_yreg)
        );
      }
      if (d_xreg) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r1), d_xreg)
        );
      }
      if (d_yreg) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r4), d_yreg)
        );
      }
      if (awb) {
        rtx mem_of = r6;

        if (GET_CODE(r6) == MEM) {
          rtx in1 = XEXP(r6,0);

          if (GET_CODE(in1) == SYMBOL_REF) {
            mem_of = gen_reg_rtx(Pmode);

            emit_insn(
               gen_movhi_address(mem_of, r6)
            );
          }
        }
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,mem_of), awb)
        );
      }
      return target;
      break;
    }

    case PIC30_BUILTIN_EDAC:
      /* FALLSTHROUGH */
      id = "edac";
    case PIC30_BUILTIN_ED: {
      rtx scratch = gen_rtx_CONST_INT(HImode,0);
      rtx distance = NULL_RTX;
      rtx p_xreg = NULL_RTX;
      rtx p_yreg = NULL_RTX;
      rtx (*gen_an_ed)(rtx, rtx, rtx, rtx, rtx, rtx, rtx, rtx, rtx) =
         (fcode == PIC30_BUILTIN_EDAC ? gen_edac_hi : gen_ed_hi);

      if (id == 0) id = "ed";
      if (!((target_flags & TARGET_MASK_ARCH_PIC30) ||
           (target_flags & TARGET_MASK_ARCH_PIC33) ||
           (target_flags & TARGET_MASK_ARCH_PIC30F202X))) {
        error("__builtin_%s is not supported on this target", id);
        return NULL_RTX;
      }
      if (!target ||
          (GET_CODE(target) != REG) || (!IS_ACCUM_REG(REGNO(target)))) {
        error("result for __builtin_%s should be an accumulator register",id);
      }
      arg0 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg2 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg3 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg4 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg5 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0,0);
      if (!pic30_reg_for_builtin(r0)) {
        rtx sqr = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(sqr, r0)
        );
        r0 = sqr;
      }
      r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
      r1 = protect_from_queue(r1,0);
      if ((GET_CODE(r1) == CONST_INT) && (INTVAL(r1) == 0)) {
        error("parameter 2 to __builtin_%s cannot be a NULL pointer",id);
        return NULL_RTX;
      }
      r3 = expand_expr(arg3, NULL_RTX, HImode, EXPAND_NORMAL);
      r3 = protect_from_queue(r3,0);
      if ((GET_CODE(r3) == CONST_INT) && (INTVAL(r3) == 0)) {
        error("parameter 4 to __builtin_%s cannot be a NULL pointer",id);
        return NULL_RTX;
      }
      p_xreg = gen_reg_rtx(Pmode);
      emit_insn(
        gen_movhi(p_xreg, gen_rtx_MEM(Pmode, r1))
      );
      p_yreg = gen_reg_rtx(Pmode);
      emit_insn(
        gen_movhi(p_yreg, gen_rtx_MEM(Pmode, r3))
      );
      r2 = expand_expr(arg2, NULL_RTX, HImode, EXPAND_NORMAL);
      r4 = expand_expr(arg4, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r2) != CONST_INT) ||
          ((INTVAL(r2) != 0) && (!CONST_OK_FOR_LETTER_P(INTVAL(r2),'Y')))) {
        error("parameter 3 for __builtin_%s is out of range",id);
      }
      if ((GET_CODE(r4) != CONST_INT) ||
          ((INTVAL(r4) != 0) && (!CONST_OK_FOR_LETTER_P(INTVAL(r4),'Y')))) {
        error("parameter 5 for __builtin_%s is out of range",id);
      }
      r5 = expand_expr(arg5, NULL_RTX, HImode, EXPAND_NORMAL);
      r5 = protect_from_queue(r5,0);
      if ((GET_CODE(r5) == CONST_INT) && (INTVAL(r5) == 0)) {
        error("parameter 6 to __builtin_%s cannot be a NULL pointer",id);
        return NULL_RTX;
      }
      distance = gen_reg_rtx(HImode);
      emit_insn(
          gen_an_ed(
                  target, 
                  r0,
                  distance,
                  p_xreg,
                  p_yreg,
                  INTVAL(r2) != 0 ? p_xreg : scratch,
                  r2,
                  INTVAL(r4) != 0 ? p_yreg : scratch,
                  r4)
      );
      if (p_xreg && (INTVAL(r2) != 0)) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r1), p_xreg)
        );
      }
      if (p_yreg && (INTVAL(r4) != 0)) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r3), p_yreg)
        );
      }
      emit_insn(
        gen_movhi(gen_rtx_MEM(HImode,r5), distance)
      );
      return target;
    }

    case PIC30_BUILTIN_FBCL:
      id = "fbcl";
      arg0 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      if (!pic30_reg_for_builtin(r0)) {
        rtx reg;

        reg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(reg, r0)
        );
        r0 = reg;
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }

      emit_insn(
        gen_fbcl_hi(target, r0)
      );
      return target;
  
    case PIC30_BUILTIN_LAC:
      id = "lac";
      if (!((target_flags & TARGET_MASK_ARCH_PIC30) ||
           (target_flags & TARGET_MASK_ARCH_PIC33) ||
           (target_flags & TARGET_MASK_ARCH_PIC30F202X))) {
        error("__builtin_%s is not supported on this target", id);
        return NULL_RTX;
      }
      arg0 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
      if (!pic30_reg_for_builtin(r0)) {
        rtx reg;

        reg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(reg, r0)
        );
        r0 = reg;
      }
      if (!target || (GET_CODE(target) != REG) || 
          (!IS_ACCUM_REG(REGNO(target)))) {
        error("result of __builtin_lac should be an accumulator register");
        return NULL_RTX;
      }
      if ((GET_CODE(r1) != CONST_INT) ||
          (!CONST_OK_FOR_LETTER_P(INTVAL(r1),'Z'))) {
        error("parameter 2 for __builtin_add should fall in the literal "
              "range -8:7");
        return NULL_RTX;
      }
      emit_insn(
        gen_lac_hi(target, r0, r1)
      );
      return target;

    case PIC30_BUILTIN_MAC: {
      rtx scratch = gen_rtx_CONST_INT(HImode,0);
      rtx p_xreg = NULL_RTX;
      rtx p_yreg = NULL_RTX;
      rtx d_xreg = NULL_RTX;
      rtx d_yreg = NULL_RTX;
      rtx awb = NULL_RTX;
      rtx awb_to;

      id = "mac";
      if (!((target_flags & TARGET_MASK_ARCH_PIC30) ||
           (target_flags & TARGET_MASK_ARCH_PIC33) ||
           (target_flags & TARGET_MASK_ARCH_PIC30F202X))) {
        error("__builtin_%s is not supported on this target", id);
        return NULL_RTX;
      }
      arg0 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg2 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg3 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg4 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg5 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg6 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg7 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg8 = TREE_VALUE(arglist);
      r4 = NULL_RTX;
      r7 = NULL_RTX;

      if (!target ||
          (GET_CODE(target) != REG) || (!IS_ACCUM_REG(REGNO(target)))) {
        error("result for __builtin_mac should be an accumulator register");
        awb_to = scratch;
      } else {
        awb_to = gen_raw_REG(HImode,OTHER_ACCUM_REG(REGNO(target)));
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      r1 = protect_from_queue(r1, 0);
      if (!pic30_reg_for_builtin(r0)) {
        rtx reg;

        reg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(reg, r0)
        );
        r0 = reg;
      }
      if (!pic30_reg_for_builtin(r1)) {
        rtx reg;

        reg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(reg, r1)
        );
        r1 = reg;
      }
      r2 = expand_expr(arg2, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r2) != CONST_INT) || (INTVAL(r2) != 0)) {
        /* contains an X prefetch */
        r3 = expand_expr(arg3, NULL_RTX, HImode, EXPAND_NORMAL);
        if ((GET_CODE(r3) == CONST_INT) && (INTVAL(r3) == 0)) {
          error("X prefetch destination to __builtin_mac should not be NULL");
          return NULL_RTX;
        }
        r4 = expand_expr(arg4, NULL_RTX, HImode, EXPAND_NORMAL);
        r2 = protect_from_queue(r2, 0);
        r3 = protect_from_queue(r3, 0);
        p_xreg = gen_reg_rtx(Pmode);
        d_xreg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(p_xreg, gen_rtx_MEM(Pmode, r2))
        );
        if (GET_CODE(r4) != CONST_INT) {
          error("X increment to __builtin_mac should be a literal");
          return NULL_RTX;
        }
        switch (INTVAL(r4)) {
          case 0:  r4 = NULL_RTX;
          case -6:
          case -4:
          case -2:
          case 2:
          case 4:
          case 6: break;
          default:
            error("X increment to __builtin_mac is out of range");
            return NULL_RTX;
        }
      }
      r5 = expand_expr(arg5, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r5) != CONST_INT) || (INTVAL(r5) != 0)) {
        /* contains an Y prefetch */
        r6 = expand_expr(arg6, NULL_RTX, HImode, EXPAND_NORMAL);
        if ((GET_CODE(r6) == CONST_INT) && (INTVAL(r6) == 0)) {
          error("Y prefetch destination to __builtin_mac should not be NULL");
          return NULL_RTX;
        }
        r7 = expand_expr(arg7, NULL_RTX, HImode, EXPAND_NORMAL);
        r6 = protect_from_queue(r6, 0);
        r7 = protect_from_queue(r7, 0);
        p_yreg = gen_reg_rtx(Pmode);
        d_yreg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(p_yreg, gen_rtx_MEM(Pmode, r5))
        );
        if (GET_CODE(r7) != CONST_INT) {
          error("Y increment to __builtin_mac should be a literal");
          return NULL_RTX;
        }
        switch (INTVAL(r7)) {
          case 0:  r7 = NULL_RTX;
          case -6:
          case -4:
          case -2:
          case 2:
          case 4:
          case 6: break;
          default:
            error("Y increment to __builtin_mac is out of range");
            return NULL_RTX;
        }
      }
      r8 = expand_expr(arg8, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r8) != CONST_INT) || (INTVAL(r8) != 0)) {
        r8 = protect_from_queue(r8,0);
        awb = gen_reg_rtx(HImode);
      } else awb_to = scratch;
      if (awb) {
        emit_insn(
          gen_macawb_gen_hi(target,
                           r0,
                           r1,
                           d_xreg ? d_xreg : scratch,
                           p_xreg ? p_xreg : scratch,
                           p_xreg && (r4) ? p_xreg : scratch,
                           (r4)   ? r4 : gen_rtx_CONST_INT(HImode,0),
                           d_yreg ? d_yreg : scratch,
                           p_yreg ? p_yreg : scratch,
                           p_yreg && (r7) ? p_yreg : scratch,
                           (r7)   ? r7 : gen_rtx_CONST_INT(HImode,0),
                           awb,
                           awb_to)
        );
      } else emit_insn(
        gen_mac_gen_hi(target,
                         r0,
                         r1,
                         d_xreg ? d_xreg : scratch,
                         p_xreg ? p_xreg : scratch,
                         p_xreg && (r4) ? p_xreg : scratch,
                         (r4)   ? r4 : gen_rtx_CONST_INT(HImode,0),
                         d_yreg ? d_yreg : scratch,
                         p_yreg ? p_yreg : scratch,
                         p_yreg && (r7) ? p_yreg : scratch,
                         (r7)   ? r7 : gen_rtx_CONST_INT(HImode,0))
      );
      if (p_xreg && r4) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r2), p_xreg)
        );
      }
      if (p_yreg && r7) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r5), p_yreg)
        );
      }
      if (d_xreg) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r3), d_xreg)
        );
      }
      if (d_yreg) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r6), d_yreg)
        );
      }
      if (awb) {
        rtx mem_of = r8;

        if (GET_CODE(r8) == MEM) {
          rtx in1 = XEXP(r8,0);

          if (GET_CODE(in1) == SYMBOL_REF) {
            mem_of = gen_reg_rtx(Pmode);

            emit_insn(
               gen_movhi_address(mem_of, r8)
            );
          }
        }
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,mem_of), awb)
        );
      }
      return target;
      break;
    }

    case PIC30_BUILTIN_MOVSAC: {
      rtx scratch = gen_rtx_CONST_INT(HImode,0);
      rtx p_xreg = NULL_RTX;
      rtx p_yreg = NULL_RTX;
      rtx d_xreg = NULL_RTX;
      rtx d_yreg = NULL_RTX;
      rtx awb = NULL_RTX;
      rtx awb_to;

      id = "movsac";
      if (!((target_flags & TARGET_MASK_ARCH_PIC30) ||
           (target_flags & TARGET_MASK_ARCH_PIC33) ||
           (target_flags & TARGET_MASK_ARCH_PIC30F202X))) {
        error("__builtin_%s is not supported on this target", id);
        return NULL_RTX;
      }
      arg0 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg2 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg3 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg4 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg5 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg6 = TREE_VALUE(arglist);
      r2 = NULL_RTX;
      r5 = NULL_RTX;

      if (!target ||
          (GET_CODE(target) != REG) || (!IS_ACCUM_REG(REGNO(target)))) {
        error("result for __builtin_movsac should be an accumulator register");
        awb_to = scratch;
      } else {
        awb_to = gen_raw_REG(HImode,OTHER_ACCUM_REG(REGNO(target)));
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r0) != CONST_INT) || (INTVAL(r0) != 0)) {
        /* contains an X prefetch */
        r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
        if ((GET_CODE(r1) == CONST_INT) && (INTVAL(r1) == 0)) {
          error("X prefetch destination to __builtin_movsac should not "
                "be NULL");
          return NULL_RTX;
        }
        r2 = expand_expr(arg2, NULL_RTX, HImode, EXPAND_NORMAL);
        r0 = protect_from_queue(r0, 0);
        r1 = protect_from_queue(r1, 0);
        p_xreg = gen_reg_rtx(Pmode);
        d_xreg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(p_xreg, gen_rtx_MEM(Pmode, r0))
        );
        if (GET_CODE(r2) != CONST_INT) {
          error("X increment to __builtin_movsac should be a literal");
          return NULL_RTX;
        }
        switch (INTVAL(r2)) {
          case 0:  r2 = NULL_RTX;
          case -6:
          case -4:
          case -2:
          case 2:
          case 4:
          case 6: break;
          default:
            error("X increment to __builtin_movsac is out of range");
            return NULL_RTX;
        }
      }
      r3 = expand_expr(arg3, NULL_RTX, HImode, EXPAND_NORMAL); 
      if ((GET_CODE(r3) != CONST_INT) || (INTVAL(r3) != 0)) {
        /* contains an Y prefetch */
        r4 = expand_expr(arg4, NULL_RTX, HImode, EXPAND_NORMAL);
        if ((GET_CODE(r4) == CONST_INT) && (INTVAL(r4) == 0)) {
          error("Y prefetch destination to __builtin_movsac should not "
                "be NULL");
          return NULL_RTX;
        }
        r5 = expand_expr(arg5, NULL_RTX, HImode, EXPAND_NORMAL);
        r4 = protect_from_queue(r4, 0);
        r5 = protect_from_queue(r5, 0);
        p_yreg = gen_reg_rtx(Pmode);
        d_yreg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(p_yreg, gen_rtx_MEM(Pmode, r3))
        );
        if (GET_CODE(r5) != CONST_INT) {
          error("Y increment to __builtin_movsac should be a literal");
          return NULL_RTX;
        }
        switch (INTVAL(r5)) {
          case 0:  r5 = NULL_RTX;
          case -6:
          case -4:
          case -2:
          case 2:
          case 4:
          case 6: break;
          default:
            error("Y increment to __builtin_movsac is out of range");
            return NULL_RTX;
        }
      }
      r6 = expand_expr(arg6, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r6) != CONST_INT) || (INTVAL(r6) != 0)) {
        r6 = protect_from_queue(r6,0);
        awb = gen_reg_rtx(HImode);
      } else awb_to = scratch;
      if (awb) {
        emit_insn(
          gen_movsacawb_gen_hi(target,
                           d_xreg ? d_xreg : scratch,
                           p_xreg ? p_xreg : scratch,
                           p_xreg && (r2) ? p_xreg : scratch,
                           (r2)   ? r2 : gen_rtx_CONST_INT(HImode,0),
                           d_yreg ? d_yreg : scratch,
                           p_yreg ? p_yreg : scratch,
                           p_yreg && (r5) ? p_yreg : scratch,
                           (r5)   ? r5 : gen_rtx_CONST_INT(HImode,0),
                           awb,
                           awb_to)
        );
      } else emit_insn(
        gen_movsac_gen_hi(target, 
                         d_xreg ? d_xreg : scratch,
                         p_xreg ? p_xreg : scratch,
                         p_xreg && (r2) ? p_xreg : scratch,
                         (r2)   ? r2 : gen_rtx_CONST_INT(HImode,0),
                         d_yreg ? d_yreg : scratch,
                         p_yreg ? p_yreg : scratch,
                         p_yreg && (r5) ? p_yreg : scratch,
                         (r5)   ? r5 : gen_rtx_CONST_INT(HImode,0))
      );
      if (p_xreg && r2) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r0), p_xreg)
        );
      }
      if (p_yreg && r5) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r3), p_yreg)
        );
      }
      if (d_xreg) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r1), d_xreg)
        );
      }
      if (d_yreg) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r4), d_yreg)
        );
      }
      if (awb) {
        rtx mem_of = r6;

        if (GET_CODE(r6) == MEM) {
          rtx in1 = XEXP(r6,0);

          if (GET_CODE(in1) == SYMBOL_REF) {
            mem_of = gen_reg_rtx(Pmode);

            emit_insn(
               gen_movhi_address(mem_of, r6)
            );
          }
        }
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,mem_of), awb)
        );
      }
      return target;
      break;
    }

    case PIC30_BUILTIN_MPYN:
      id = "mpyn";
    case PIC30_BUILTIN_MPY: {
      rtx scratch = gen_rtx_CONST_INT(HImode,0);
      rtx p_xreg = NULL_RTX;
      rtx p_yreg = NULL_RTX;
      rtx d_xreg = NULL_RTX;
      rtx d_yreg = NULL_RTX;

      if (id == 0) id = "mpy";
      if (!((target_flags & TARGET_MASK_ARCH_PIC30) ||
           (target_flags & TARGET_MASK_ARCH_PIC33) ||
           (target_flags & TARGET_MASK_ARCH_PIC30F202X))) {
        error("__builtin_%s is not supported on this target", id);
        return NULL_RTX;
      }
      arg0 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg2 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg3 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg4 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg5 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg6 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg7 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg8 = TREE_VALUE(arglist);
      r4 = NULL_RTX;
      r7 = NULL_RTX;

      if (!target ||
          (GET_CODE(target) != REG) || (!IS_ACCUM_REG(REGNO(target)))) {
        error("result for __builtin_%s should be an accumulator register",id);
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      r1 = protect_from_queue(r1, 0);
      if (!pic30_reg_for_builtin(r0)) {
        rtx reg;

        reg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(reg, r0)
        );
        r0 = reg;
      }
      if (!pic30_reg_for_builtin(r1) != REG) {
        rtx reg;

        reg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(reg, r1)
        );
        r1 = reg;
      }
      r2 = expand_expr(arg2, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r2) != CONST_INT) || (INTVAL(r2) != 0)) {
        /* contains an X prefetch */
        r3 = expand_expr(arg3, NULL_RTX, HImode, EXPAND_NORMAL);
        if ((GET_CODE(r3) == CONST_INT) && (INTVAL(r3) == 0)) {
          error("X prefetch destination to __builtin_%s should not be NULL",id);
          return NULL_RTX;
        }
        r4 = expand_expr(arg4, NULL_RTX, HImode, EXPAND_NORMAL);
        r2 = protect_from_queue(r2, 0);
        r3 = protect_from_queue(r3, 0);
        p_xreg = gen_reg_rtx(Pmode);
        d_xreg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(p_xreg, gen_rtx_MEM(Pmode, r2))
        );
        if (GET_CODE(r4) != CONST_INT) {
          error("X increment to __builtin_%s should be a literal",id);
          return NULL_RTX;
        }
        switch (INTVAL(r4)) {
          case 0:  r4 = NULL_RTX;
          case -6:
          case -4:
          case -2:
          case 2:
          case 4:
          case 6: break;
          default:
            error("X increment to __builtin_%s is out of range",id);
            return NULL_RTX;
        }
      }
      r5 = expand_expr(arg5, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r5) != CONST_INT) || (INTVAL(r5) != 0)) {
        /* contains an Y prefetch */
        r6 = expand_expr(arg6, NULL_RTX, HImode, EXPAND_NORMAL);
        if ((GET_CODE(r6) == CONST_INT) && (INTVAL(r6) == 0)) {
          error("Y prefetch destination to __builtin_%s should not be NULL",id);
          return NULL_RTX;
        }
        r7 = expand_expr(arg7, NULL_RTX, HImode, EXPAND_NORMAL);
        r6 = protect_from_queue(r6, 0);
        r7 = protect_from_queue(r7, 0);
        p_yreg = gen_reg_rtx(Pmode);
        d_yreg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(p_yreg, gen_rtx_MEM(Pmode, r5))
        );
        if (GET_CODE(r7) != CONST_INT) {
          error("Y increment to __builtin_%s should be a literal",id);
          return NULL_RTX;
        }
        switch (INTVAL(r7)) {
          case 0:  r7 = NULL_RTX;
          case -6:
          case -4:
          case -2:
          case 2:
          case 4:
          case 6: break;
          default:
            error("Y increment to __builtin_%s is out of range",id);
            return NULL_RTX;
        }
      }
      emit_insn(
        (fcode == PIC30_BUILTIN_MPYN ? 
         gen_mpyn_gen_hi : 
         gen_mpy_gen_hi)(target,
                         r0,
                         r1,
                         d_xreg ? d_xreg : scratch,
                         p_xreg ? p_xreg : scratch,
                         p_xreg && (r4) ? p_xreg : scratch,
                         (r4)   ? r4 : gen_rtx_CONST_INT(HImode,0),
                         d_yreg ? d_yreg : scratch,
                         p_yreg ? p_yreg : scratch,
                         p_yreg && (r7) ? p_yreg : scratch,
                         (r7)   ? r7 : gen_rtx_CONST_INT(HImode,0))
      );
      if (p_xreg && r4) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r2), p_xreg)
        );
      }
      if (p_yreg && r7) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r5), p_yreg)
        );
      }
      if (d_xreg) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r3), d_xreg)
        );
      }
      if (d_yreg) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r6), d_yreg)
        );
      }
      return target;
      break;
    }

    case PIC30_BUILTIN_MSC: {
      rtx scratch = gen_rtx_CONST_INT(HImode,0);
      rtx p_xreg = NULL_RTX;
      rtx p_yreg = NULL_RTX;
      rtx d_xreg = NULL_RTX;
      rtx d_yreg = NULL_RTX;
      rtx awb = NULL_RTX;
      rtx awb_to;

      id = "msc";
      if (!((target_flags & TARGET_MASK_ARCH_PIC30) ||
           (target_flags & TARGET_MASK_ARCH_PIC33) ||
           (target_flags & TARGET_MASK_ARCH_PIC30F202X))) {
        error("__builtin_%s is not supported on this target", id);
        return NULL_RTX;
      }
      arg0 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg2 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg3 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg4 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg5 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg6 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg7 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg8 = TREE_VALUE(arglist);
      r4 = NULL_RTX;
      r7 = NULL_RTX;

      if (!target ||
          (GET_CODE(target) != REG) || (!IS_ACCUM_REG(REGNO(target)))) {
        error("result for __builtin_msc should be an accumulator register");
        awb_to = scratch;
      } else {
        awb_to = gen_raw_REG(HImode,OTHER_ACCUM_REG(REGNO(target)));
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      r1 = protect_from_queue(r1, 0);
      if (!pic30_reg_for_builtin(r0)) {
        rtx reg;

        reg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(reg, r0)
        );
        r0 = reg;
      }
      if (!pic30_reg_for_builtin(r1)) {
        rtx reg;

        reg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(reg, r1)
        );
        r1 = reg;
      }
      r2 = expand_expr(arg2, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r2) != CONST_INT) || (INTVAL(r2) != 0)) {
        /* contains an X prefetch */
        r3 = expand_expr(arg3, NULL_RTX, HImode, EXPAND_NORMAL);
        if ((GET_CODE(r3) == CONST_INT) && (INTVAL(r3) == 0)) {
          error("X prefetch destination to __builtin_msc should not be NULL");
          return NULL_RTX;
        }
        r4 = expand_expr(arg4, NULL_RTX, HImode, EXPAND_NORMAL);
        r2 = protect_from_queue(r2, 0);
        r3 = protect_from_queue(r3, 0);
        p_xreg = gen_reg_rtx(Pmode);
        d_xreg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(p_xreg, gen_rtx_MEM(Pmode, r2))
        );
        if (GET_CODE(r4) != CONST_INT) {
          error("X increment to __builtin_msc should be a literal");
          return NULL_RTX;
        }
        switch (INTVAL(r4)) {
          case 0:  r4 = NULL_RTX;
          case -6:
          case -4:
          case -2:
          case 2:
          case 4:
          case 6: break;
          default:
            error("X increment to __builtin_msc is out of range");
            return NULL_RTX;
        }
      }
      r5 = expand_expr(arg5, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r5) != CONST_INT) || (INTVAL(r5) != 0)) {
        /* contains an Y prefetch */
        r6 = expand_expr(arg6, NULL_RTX, HImode, EXPAND_NORMAL);
        if ((GET_CODE(r6) == CONST_INT) && (INTVAL(r6) == 0)) {
          error("Y prefetch destination to __builtin_msc should not be NULL");
          return NULL_RTX;
        }
        r7 = expand_expr(arg7, NULL_RTX, HImode, EXPAND_NORMAL);
        r6 = protect_from_queue(r6, 0);
        r7 = protect_from_queue(r7, 0);
        p_yreg = gen_reg_rtx(Pmode);
        d_yreg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(p_yreg, gen_rtx_MEM(Pmode, r5))
        );
        if (GET_CODE(r7) != CONST_INT) {
          error("Y increment to __builtin_msc should be a literal");
          return NULL_RTX;
        }
        switch (INTVAL(r7)) {
          case 0:  r7 = NULL_RTX;
          case -6:
          case -4:
          case -2:
          case 2:
          case 4:
          case 6: break;
          default:
            error("Y increment to __builtin_msc is out of range");
            return NULL_RTX;
        }
      }
      r8 = expand_expr(arg8, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r8) != CONST_INT) || (INTVAL(r8) != 0)) {
        r8 = protect_from_queue(r8,0);
        awb = gen_reg_rtx(HImode);
      } else awb_to = scratch;
      if (awb) {
        emit_insn(
          gen_mscawb_gen_hi(target,
                           r0,
                           r1,
                           d_xreg ? d_xreg : scratch,
                           p_xreg ? p_xreg : scratch,
                           p_xreg && (r4) ? p_xreg : scratch,
                           (r4)   ? r4 : gen_rtx_CONST_INT(HImode,0),
                           d_yreg ? d_yreg : scratch,
                           p_yreg ? p_yreg : scratch,
                           p_yreg && (r7) ? p_yreg : scratch,
                           (r7)   ? r7 : gen_rtx_CONST_INT(HImode,0),
                           awb,
                           awb_to)
        );
      } else emit_insn(
        gen_msc_gen_hi(target,
                         r0,
                         r1,
                         d_xreg ? d_xreg : scratch,
                         p_xreg ? p_xreg : scratch,
                         p_xreg && (r4) ? p_xreg : scratch,
                         (r4)   ? r4 : gen_rtx_CONST_INT(HImode,0),
                         d_yreg ? d_yreg : scratch,
                         p_yreg ? p_yreg : scratch,
                         p_yreg && (r7) ? p_yreg : scratch,
                         (r7)   ? r7 : gen_rtx_CONST_INT(HImode,0))
      );
      if (p_xreg && r4) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r2), p_xreg)
        );
      }
      if (p_yreg && r7) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r5), p_yreg)
        );
      }
      if (d_xreg) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r3), d_xreg)
        );
      }
      if (d_yreg) {
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,r6), d_yreg)
        );
      }
      if (awb) {
        rtx mem_of = r8;

        if (GET_CODE(r8) == MEM) {
          rtx in1 = XEXP(r8,0);

          if (GET_CODE(in1) == SYMBOL_REF) {
            mem_of = gen_reg_rtx(Pmode);
            
            emit_insn(
               gen_movhi_address(mem_of, r8)
            );
          }
        }
        emit_insn(
          gen_movhi(gen_rtx_MEM(HImode,mem_of), awb)
        );
      }
      return target;
      break;
    }
   
    case PIC30_BUILTIN_SAC:
      id = "msc";
      if (!((target_flags & TARGET_MASK_ARCH_PIC30) ||
           (target_flags & TARGET_MASK_ARCH_PIC33) ||
           (target_flags & TARGET_MASK_ARCH_PIC30F202X))) {
        error("__builtin_%s is not supported on this target", id);
        return NULL_RTX;
      }
      arg0 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r0) != REG) || (!IS_ACCUM_REG(REGNO(r0)))) {
        error("parameter 1 for __builtin_sac should be "
              "an accumulator register");
        return NULL_RTX;
      }
      if ((GET_CODE(r1) != CONST_INT) || 
          (!CONST_OK_FOR_LETTER_P(INTVAL(r1),'Z'))) {
        error("parameter 2 for __builtin_add should fall in the literal "
              "range -8:7");
        return NULL_RTX;
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode); 
      }
      emit_insn(
        gen_sac_gen_hi(target, r0, r1)
      );
      return target;
      break;

    case PIC30_BUILTIN_SACR:
      id = "sacr";
      if (!((target_flags & TARGET_MASK_ARCH_PIC30) ||
           (target_flags & TARGET_MASK_ARCH_PIC33) ||
           (target_flags & TARGET_MASK_ARCH_PIC30F202X))) {
        error("__builtin_%s is not supported on this target", id);
        return NULL_RTX;
      }
      arg0 = TREE_VALUE(arglist);
      arglist = TREE_CHAIN(arglist);
      arg1 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      r1 = expand_expr(arg1, NULL_RTX, HImode, EXPAND_NORMAL);
      if ((GET_CODE(r0) != REG) || (!IS_ACCUM_REG(REGNO(r0)))) {
        error("parameter 1 for __builtin_sacr should be "
              "an accumulator register");
        return NULL_RTX;
      }
      if ((GET_CODE(r1) != CONST_INT) || 
          (!CONST_OK_FOR_LETTER_P(INTVAL(r1),'Z'))) {
        error("parameter 2 for __builtin_add should fall in the literal "
              "range -8:7");
        return NULL_RTX;
      }
      if (!target || !register_operand(target, HImode))
      {
        target = gen_reg_rtx(HImode);
      }
      emit_insn(
        gen_sacr_gen_hi(target, r0, r1)
      );
      return target;
      break;

    case PIC30_BUILTIN_SFTAC: {

      id = "sftac";
      if (!((target_flags & TARGET_MASK_ARCH_PIC30) ||
           (target_flags & TARGET_MASK_ARCH_PIC33) ||
           (target_flags & TARGET_MASK_ARCH_PIC30F202X))) {
        error("__builtin_%s is not supported on this target", id);
        return NULL_RTX;
      }
      if (!target ||
          (GET_CODE(target) != REG) || (!IS_ACCUM_REG(REGNO(target)))) {
        error("result for __builtin_sftac should be an accumulator register");
      }
      arg0 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      r0 = protect_from_queue(r0, 0);
      if ((GET_CODE(r0) == CONST_INT) && 
          !CONST_OK_FOR_LETTER_P(INTVAL(r0),'W')) {
        error("parameter 1 is out of range for __builtin_sftac");
        return NULL_RTX;
      } else if (!pic30_reg_for_builtin(r0)) {
        rtx reg;

        reg = gen_reg_rtx(HImode);
        emit_insn(
          gen_movhi(reg, r0)
        );
        r0 = reg;
      }
      emit_insn(
        gen_sftac_gen_hi(target, r0)
      );
      return target;
      break;
    }

    case PIC30_BUILTIN_DISI: {
      arg0 = TREE_VALUE(arglist);
      r0 = expand_expr(arg0, NULL_RTX, HImode, EXPAND_NORMAL);
      if (GET_CODE(r0) != CONST_INT) {
        error("__builtin_disi requires a literal value");
        return NULL_RTX;
      }
      if ((INTVAL(r0) < 0) || (INTVAL(r0) > 16383)) {
        error("__builtin_disi value out of range: 0..16383 expected");
        return NULL_RTX;
      }
      emit_insn(gen_disi(r0));
      return NULL_RTX;
    }
  }
    
  return(NULL_RTX);
}

/************************************************************************/
/*
** Store in cc_status the expressions that the condition codes will
** describe after execution of an instruction whose pattern is EXP.
** Do not alter them if the instruction would not alter the cc's.
**
** On the dsPIC30, all the insns to store in an address register fail to
** set the cc's.  However, in some cases these instructions can make it
** possibly invalid to use the saved cc's.  In those cases we clear out
** some or all of the saved cc's so they won't be used.
*/
/************************************************************************/
void
pic30_notice_update_cc(exp, insn)
     rtx exp;
     rtx insn;
{
    switch (get_attr_cc(insn))
    {
    case CC_SET:
        /*
        ** The insn sets the cc.
        */
        CC_STATUS_INIT;
        if (GET_CODE(exp) == PARALLEL)
        {
            /*
            ** insns with scratch registers
            */
               exp = XVECEXP(exp, 0, 0);
               if (GET_CODE(exp) != SET)
            {
                break;
            }
        }
        cc_status.value1 = SET_DEST(exp);
        cc_status.value2 = SET_SRC(exp);
        break;
    case CC_MATH:
        /*
        ** The insn leaves the C and OV flags incorrect.
        */
        CC_STATUS_INIT;
        cc_status.flags |= CC_NO_OVERFLOW;    /* Really C flag */
        cc_status.mdep = TRUE;            /* Indicates OV bad */
        cc_status.value1 = SET_DEST(exp);
        break;
    case CC_MOVE:
        /*
        ** The insn leaves the C and OV flags incorrect.
        */
        CC_STATUS_INIT;
        cc_status.flags |= CC_NO_OVERFLOW;    /* Really C flag */
        cc_status.mdep = TRUE;            /* Indicates OV bad */
        cc_status.value1 = SET_DEST(exp);
        cc_status.value2 = SET_SRC(exp);
        break;
    case CC_CHANGE0:
        /*
        ** Insn does not change CC,
        ** but the 0'th operand has been changed.
        */
        if (cc_status.value1 && modified_in_p(cc_status.value1, insn))
        {
            cc_status.value1 = NULL_RTX;
        }
        if (cc_status.value2 && modified_in_p(cc_status.value2, insn))
        {
            cc_status.value2 = NULL_RTX; 
        }
        break;
    case CC_CLOBBER:
        /*
        ** Insn doesn't leave CC in a usable state.
        */
        CC_STATUS_INIT;
        break;
    case CC_UNCHANGED:
        /*
        ** Insn does not affect CC at all.
        */
        break;
    }
}
/*************************************************************************
**
** On some machines, double-precision values must be kept in even/odd
** register pairs. You can implement that by defining this macro to reject
** odd register numbers for such modes. 
**
*************************************************************************/
int
pic30_hard_regno_mode_ok (regno, mode)
     int regno;
     enum machine_mode mode;
{ int fOkay;

  switch (mode)
  {
    case SImode:        /* Integer (32 bits) */
    case SFmode:        /* Float (32 bits) */
      fOkay = (regno < SP_REGNO) && IS_EVEN_REG(regno);
      break;

    case DImode:        /* Integer (64 bits) */
    case DFmode:        /* Float (64 bits) */
      fOkay = (regno < SP_REGNO) && IS_QUAD_REG(regno);
      break;

    default:
      fOkay = (regno <= SP_REGNO) || (regno >= A_REGNO && regno <= B_REGNO);
      break;
  }
  return(fOkay);
}


int pic30_pp_modify_valid(rtx opnd ATTRIBUTE_UNUSED) {
  int pre=0;

#if 0
  rtx inner;

  if (TARGET_ARCH(REV_A2) && TARGET_CONST_IN_CODE) {
    pre = 1; /* invalid */
    if (opnd) {
      if (GET_CODE(opnd) != MEM) abort();
      inner = XEXP(opnd,0);
      switch (GET_CODE(inner)) {
        case PRE_INC:
        case PRE_DEC: pre = -1;
             break;
        case POST_INC:
        case POST_DEC: pre = 1;
             break;
    default: abort();
      }
    }
  }
#endif
  return pre;
}
/************************************************************************/
/*
** Preserve the operands of a compare instruction.
*/
/************************************************************************/
const char *
pic30_compare(operands)
     rtx *operands;
{
    rtxCmpOperands[0] = operands[0];
    rtxCmpOperands[1] = operands[1];

    if (flag_verbose_asm)
    {
        return(";");
    }
    else
    {
        return("");
    }
}
/************************************************************************/
/*    Convert a condition code id to a name.                */
/************************************************************************/
static const char *
pic30_condition_code_name(cond)
    enum rtx_code cond;
{
    const char *pszCondition = "";

    switch (cond)
    {
    case EQ:
        pszCondition = "z";
        break;
    case NE:
        pszCondition = "nz";
        break;
    case LE:
        pszCondition = "le";
        break;
    case LT:
        pszCondition = "lt";
        break;
    case GE:
        pszCondition = "ge";
        break;
    case GT:
        pszCondition = "gt";
        break;
    case LEU:
        pszCondition = "leu";
        break;
    case LTU:
        pszCondition = "ltu";
        break;
    case GEU:
        pszCondition = "geu";
        break;
    case GTU:
        pszCondition = "gtu";
        break;
    default:
        break;
    }
    return(pszCondition);
}
/************************************************************************/
/*
** Construct an SImode conditional branch instruction.
**
** cond    specifies the condition to test.
** rtxThenLabel    specifies the label to branch to if the condition is true.
** If the condition is false, fall through to the next sequential insn.
*/
/************************************************************************/
static char *
pic30_conditional_branchSI(cond, rtxThenLabel, buf)
    enum rtx_code cond;
    rtx rtxThenLabel;
    char *buf;
{
        rtx rtxElseLabel = NULL_RTX;

    switch (cond)
    {
    case EQ:
        if (    (GET_CODE(rtxCmpOperands[1]) == CONST_INT) &&
            (INTVAL(rtxCmpOperands[1]) == 0))
        {
            output_asm_insn("ior %d0,%0,[w15]", rtxCmpOperands);
            output_asm_insn("bra z,%0", &rtxThenLabel);
        }
        else
        {
            output_asm_insn("sub %1,%0,[w15]", rtxCmpOperands);
            output_asm_insn("subb %d1,%d0,[w15]",rtxCmpOperands);
            output_asm_insn("bra z,%0", &rtxThenLabel);
        }
        break;
    case NE:
        if (    (GET_CODE(rtxCmpOperands[1]) == CONST_INT) &&
            (INTVAL(rtxCmpOperands[1]) == 0))
        {
            output_asm_insn("ior %d0,%0,[w15]", rtxCmpOperands);
            output_asm_insn("bra nz,%0", &rtxThenLabel);
        }
        else
        {
            output_asm_insn("sub %1,%0,[w15]", rtxCmpOperands);
            output_asm_insn("subb %d1,%d0,[w15]",rtxCmpOperands);
            output_asm_insn("bra nz,%0", &rtxThenLabel);
        }
        break;
    case GT:
        if (    (GET_CODE(rtxCmpOperands[1]) == CONST_INT) &&
            (INTVAL(rtxCmpOperands[1]) == 0))
        {
             output_asm_insn("sub %0,#0,[w15]",
                            rtxCmpOperands);
             output_asm_insn("subb %d0,#0,[w15]",
                            rtxCmpOperands);
             output_asm_insn("bra n,%0", &rtxThenLabel);
        }
        else
        {
             output_asm_insn("sub %1,%0,[w15]",
                            rtxCmpOperands);
             output_asm_insn("subb %d1,%d0,[w15]",
                            rtxCmpOperands);
             output_asm_insn("bra n,%0", &rtxThenLabel);
        }
        break;
    case LT:
        if (    (GET_CODE(rtxCmpOperands[1]) == CONST_INT) &&
            (INTVAL(rtxCmpOperands[1]) == 0))
        {
            output_asm_insn("cp0 %d0", rtxCmpOperands);
            output_asm_insn("bra lt,%0", &rtxThenLabel);
        }
        else
        {
             output_asm_insn("sub %0,%1,[w15]",
                             rtxCmpOperands);
             output_asm_insn("subb %d0,%d1,[w15]",
                             rtxCmpOperands);
             output_asm_insn("bra n,%0", &rtxThenLabel);
        }
        break;
    case GE:
        if (    (GET_CODE(rtxCmpOperands[1]) == CONST_INT) &&
            (INTVAL(rtxCmpOperands[1]) == 0))
        {
            output_asm_insn("cp0 %d0", rtxCmpOperands);
            output_asm_insn("bra nn,%0", &rtxThenLabel);
        }
        else
        {
             output_asm_insn("sub %0,%1,[w15]",
                             rtxCmpOperands);
             output_asm_insn("subb %d0,%d1,[w15]",
                             rtxCmpOperands);
             output_asm_insn("bra nn,%0", &rtxThenLabel);
        }
        break;
    case LE:
        if (    (GET_CODE(rtxCmpOperands[1]) == CONST_INT) &&
            (INTVAL(rtxCmpOperands[1]) == 0))
        {
            output_asm_insn("sub %0,#0,[w15]", rtxCmpOperands);
            output_asm_insn("subb %d0,#0,[w15]",rtxCmpOperands);
            output_asm_insn("bra le,%0", &rtxThenLabel);
        }
        else
        {
             output_asm_insn("sub %1,%0,[w15]",
                            rtxCmpOperands);
             output_asm_insn("subb %d1,%d0,[w15]",
                            rtxCmpOperands);
             output_asm_insn("bra nn,%0", &rtxThenLabel);
        }
        break;
    case GTU:
        output_asm_insn("sub %1,%0,[w15]", rtxCmpOperands);
        output_asm_insn("subb %d1,%d0,[w15]", rtxCmpOperands);
        output_asm_insn("bra ltu,%0", &rtxThenLabel);
        break;
    case LTU:
        output_asm_insn("sub %0,%1,[w15]", rtxCmpOperands);
        output_asm_insn("subb %d0,%d1,[w15]", rtxCmpOperands);
        output_asm_insn("bra ltu,%0", &rtxThenLabel);
        break;
    case GEU:
        output_asm_insn("sub %0,%1,[w15]", rtxCmpOperands);
        output_asm_insn("subb %d0,%d1,[w15]", rtxCmpOperands);
        output_asm_insn("bra geu,%0", &rtxThenLabel);
        break;
    case LEU:
        output_asm_insn("sub %1,%0,[w15]", rtxCmpOperands);
        output_asm_insn("subb %d1,%d0,[w15]", rtxCmpOperands);
        output_asm_insn("bra geu,%0", &rtxThenLabel);
        break;
    default:
        break;
    }
    if (rtxElseLabel != NULL_RTX)
    {
        asm_fprintf(asm_out_file, "%s%d:\n",
                    LOCAL_LABEL_PREFIX,
                        CODE_LABEL_NUMBER(rtxElseLabel));
    }
    buf[0] = 0;
    rtxCmpOperands[0] = NULL_RTX;

    return(buf);
}
/************************************************************************/
/*
** Construct an HImode conditional branch instruction.
*/
/************************************************************************/
#define    SR_OV    "2"            /* Status Register OV bit #    */
static char *
pic30_conditional_branchHI(cond, buf)
    enum rtx_code cond;
    char *buf;
{
    char *obuf = buf;
    const char *pszCondition = pic30_condition_code_name(cond);

    switch (cond)
    {
    case GT:
    case LE:
        if (cc_status.mdep)
        {
            strcpy(buf, "bclr.b _SR,#" SR_OV "\n\t");
            buf += strlen(buf);
            cc_status.mdep = FALSE;
        }
        break;
    case LT:
        if (cc_status.mdep)
        {
            pszCondition = "n";
        }
        break;
    case GE:
        if (cc_status.mdep)
        {
            pszCondition = "nn";
        }
        break;
    case EQ:
    case NE:
    case LTU:
    case GTU:
    case LEU:
    case GEU:
        break;
    default:
        break;
    }
    sprintf(buf, "bra %s,%%0", pszCondition);

    return(obuf);
}
/************************************************************************/
/*
** Construct a conditional branch instruction.
*/
/************************************************************************/
char *
pic30_conditional_branch(cond, rtxThenLabel)
    enum rtx_code cond;
    rtx rtxThenLabel;
{
   static char buf[30];
#if (1)
    int pred_val = 0;
    double pr;
    rtx p = find_reg_note(current_output_insn, REG_BR_PROB, 0);
    if (p)
    {
        pred_val = INTVAL(XEXP(p, 0));
    }
    pr = pred_val * 100.0 / REG_BR_PROB_BASE;

     asm_fprintf(asm_out_file, "\t.set ___BP___,%d\n", (int)pr);
#endif
    if (rtxCmpOperands[0] != NULL_RTX)
    {
        return(pic30_conditional_branchSI(cond, rtxThenLabel, buf));
    }
    else
    {
        return(pic30_conditional_branchHI(cond, buf));
    }
}
/*
** For an arg passed partly in registers and partly on the stack,
** this is the number of registers used.
** For args passed entirely in registers or entirely on the stack, zero.
*/
int
pic30_function_arg_partial_nregs(cum, mode, type, named)
     CUMULATIVE_ARGS cum ATTRIBUTE_UNUSED;    /* current arg information */
     enum machine_mode mode ATTRIBUTE_UNUSED;    /* current arg mode */
     tree type ATTRIBUTE_UNUSED; /* type of the argument or 0 if lib support */
     int named ATTRIBUTE_UNUSED; /* whether or not the argument was named */
{
    int nregs = 0;

    /*
    ** To pass parameters partially on the stack, and
    ** partially in registers, I think we need to change
    ** FUNCTION_ARG to return a parallel RTX.
    ** Later ...
    */
    return(nregs);
}

/*
** Initialize a variable CUM of type CUMULATIVE_ARGS for a call to a
** function whose data type is FNTYPE.
** For a library call, FNTYPE is  0.
*/
void
pic30_init_cumulative_args (cum, fntype, libname)
     CUMULATIVE_ARGS *cum;    /* argument info to initialize */
     tree fntype ATTRIBUTE_UNUSED;    /* tree ptr for function decl */
     rtx libname ATTRIBUTE_UNUSED;    /* SYMBOL_REF of library name or 0 */
{
    int i;

    for (i = 0; i < (int)NELEMENTS(cum->parmregs); ++i)
    {
        cum->parmregs[i] = -1;
    }
}
/*************************************************************************
**
** A C function that controls whether a function argument is passed
** in a register, and if so which register.
** Define where to put the arguments to a function.  Value is zero to
** push the argument on the stack, or a hard register in which to
** store the argument.
**
** MODE is the argument's machine mode.
** TYPE is the data type of the argument (as a tree).
** This is null for libcalls where that information may
** not be available.
** CUM is a variable of type CUMULATIVE_ARGS which gives info about
** the preceding args and about the function being called.
** NAMED is nonzero if this argument is a named parameter
** (otherwise it is an extra parameter matching an ellipsis).
**
*************************************************************************/
rtx
pic30_function_arg(cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
    rtx arg = NULL_RTX;

        /* see Changelog.3 for precedent, look for void_type_node.
           void_type_node no longer has a size field.  In the past the size
           was set to zero_type_size but now a null pointer is dereferenced
           in the macro MUST_PASS_IN_STACK causing sigsegv.  void types
           won't be in a register. (CW) */
    if (named && type != void_type_node && !MUST_PASS_IN_STACK(mode, type))
    {
        int i;
        int nWords;

        if (mode == BLKmode)
        {
            /*
            ** Structure containing an array
            */
            nWords = (int_size_in_bytes(type) + UNITS_PER_WORD-1)
                            / UNITS_PER_WORD;
        }
        else
        {
               nWords = (GET_MODE_SIZE(mode) + UNITS_PER_WORD-1)
                            / UNITS_PER_WORD;
        }
        for (i = 0; i < (int)NELEMENTS(cum->parmregs); ++i)
        {
            if (    (cum->parmregs[i] == -1) &&
                ((i + nWords) <= (PIC30_LAST_PARAM_REG+1)) &&
                ((i & (nWords-1)) == 0)    )
            {
                arg = gen_rtx_REG(mode, i);
                break;
            }
        }
    }
    return(arg);
}
/*
** Update the data in CUM to advance over an argument
** of mode MODE and data type TYPE.
** (TYPE is null for libcalls where that information may not be available.)
*/
void
pic30_function_arg_advance(cum, mode, type, named)
     CUMULATIVE_ARGS *cum;    /* current arg information */
     enum machine_mode mode;    /* current arg mode */
     tree type;            /* type of the argument or 0 if lib support */
     int named;            /* whether or not the argument was named */
{
    if (named && !MUST_PASS_IN_STACK(mode, type))
    {
        int i;
        int nWords;

        if (mode == BLKmode)
        {
            /*
            ** Structure containing an array
            */
            nWords = (int_size_in_bytes(type) + UNITS_PER_WORD-1)
                            / UNITS_PER_WORD;
        }
        else
        {
               nWords = (GET_MODE_SIZE(mode)+UNITS_PER_WORD-1)
                    / UNITS_PER_WORD;
        }
        for (i = 0; i < (int)NELEMENTS(cum->parmregs); ++i)
        {
            if (    (cum->parmregs[i] == -1) &&
                ((i + nWords) <= (PIC30_LAST_PARAM_REG+1)) &&
                ((i & (nWords-1)) == 0)    )
            {
                int j;
                
                for (j = 0; j < nWords; ++j)
                {
                    cum->parmregs[i+j] = mode;
                }
                break;
            }
        }
    }
}

/*
** Convert and RTX double to a C double
*/
static double
pic30_get_double (x)
     rtx x;
{
    union
    {
        double d;
        long i[2];
    }
    du;

    du.i[0] = CONST_DOUBLE_LOW (x);
    du.i[1] = CONST_DOUBLE_HIGH (x);

    return du.d;
}

/*
** Check whether <reg> is dead at insn <first>.
** This is done by searching ahead for either the next use
** (i.e., reg is live), a death note, or a set of <reg>.
*/
int
pic30_dead_or_set_p(rtx first, rtx reg)
{
  rtx insn;

  if (first == 0) return 1;
#if 0
  /*
  ** Dies immediately.
  */
  if (dead_or_set_p(first, reg))
  {
    return(1);
  }
#endif

  /*
  ** Look for conclusive evidence of live/death, otherwise we have
  ** to assume that it is live.
  */
  for (insn = first; insn; insn = NEXT_INSN(insn))
  {
#if 0
    if (GET_CODE(insn) == JUMP_INSN)
    {
       return(0);    /* We lose track, assume it is alive.  */
    }
    else if (GET_CODE(insn) == CALL_INSN)
    {
      return(0);    /* We lose track, assume it is alive.  */
    }
#else
    if (GET_CODE(insn) == JUMP_INSN) {
      rtx block_start = first;

      for (; block_start; block_start = PREV_INSN(block_start)) {
         if  (GET_CODE(block_start) == NOTE) {
           if (NOTE_INSN_BASIC_BLOCK_P(block_start)) break; 
           if (NOTE_LINE_NUMBER(block_start) == NOTE_INSN_BLOCK_END) break;
         }
      }
      if (block_start && NOTE_INSN_BASIC_BLOCK_P(block_start)) {
         /* found the beginning of the block */
         /* if the register is not valid going in, then its dead */
         basic_block bb = NOTE_BASIC_BLOCK(block_start);

         if (!REGNO_REG_SET_P(bb->global_live_at_end,reg->fld[0].rtint)) 
           return 1;
      }
      /* otherwise we are lost and assume the worst */
      return 0;
    }
    /* a CALL_INSN should not affect register usage - the call will restore
       the registers - but this fn doesn't know what is currently live its
       dumb */
    else if (GET_CODE(insn) == CALL_INSN) {
      return 0;
    }
#endif
    else if (GET_CODE(insn) == INSN) 
    { rtx pattern;

      pattern = PATTERN(insn);

      if (GET_CODE(pattern) == CLOBBER) {
        rtx clobber = XEXP(pattern, 0);
  
        if ((GET_CODE(clobber) == REG) &&
            (REGNO(clobber) < FIRST_PSEUDO_REGISTER) && 
            (REGNO(reg) < FIRST_PSEUDO_REGISTER)) {
            int regno = REGNO(reg);

          if ((REGNO(clobber) <= regno) &&
              (REGNO(clobber) +
               HARD_REGNO_NREGS(0,GET_MODE(clobber)) > regno))
             return 1;
        } else if ((GET_CODE(clobber) == REG) && (REGNO(clobber) == REGNO(reg)))
          return 1;
      } 
      if (reg_referenced_p(reg, PATTERN(insn)))
      {
         return(0);
      }
      else if (dead_or_set_p(insn, reg))
      {
         return(1);
      }
    }
  }

  /*
  ** No conclusive evidence either way, we can not take the chance
  ** that control flow hid the use from us -- "I'm not dead yet".
  */
  return(0);
}
/*
** See if a register (reg) is dereferenced in an RTL (in).
*/
static int
pic30_reg_dereferenced_p(rtx reg, rtx in)
{
    register const char *fmt;
    register int i;
    register enum rtx_code code;
    rtx regb;
    rtx regi;
    rtx rtxInner;
    rtx rtxPlusOp0;
    rtx rtxPlusOp1;

    if (in == 0)
    {
        return(0);
    }
    code = GET_CODE(in);

    switch (code)
    {
    case MEM:
        rtxInner = XEXP(in, 0);
        regb = NULL_RTX;
        regi = NULL_RTX;
        switch (GET_CODE(rtxInner))
        {
        case REG:
            regb = rtxInner;
            break;
        case POST_INC:
        case POST_DEC:
        case PRE_INC:
        case PRE_DEC:
            regb = XEXP(rtxInner, 0);
            if (GET_CODE(regb) != REG)
            {
                regb = NULL_RTX;
            }
            break;
        case PLUS:
            /*
            ** Base with index/displacement.
            */
              rtxPlusOp0 = XEXP(rtxInner, 0);
              switch (GET_CODE(rtxPlusOp0))
            {
            case SUBREG:
                if (!register_operand(rtxPlusOp0, Pmode))
                {
                    break;
                }
                /*
                ** Fall thru
                */
            case REG:
                regb = rtxPlusOp0;
                rtxPlusOp1 = XEXP(rtxInner, 1);
                switch (GET_CODE(rtxPlusOp1))
                {
                case SUBREG:
                    if (!register_operand(rtxPlusOp1,Pmode))
                    {
                        break;
                    }
                    /*
                    ** Fall thru
                    */
                case REG:
                    /*
                    ** Base with index
                    */
                    regi = rtxPlusOp1;
                    break;
                default:
                    break;
                }
                break;
            default:
                break;
            }
            break;
        default:
            break;
        }
        if ((regb != NULL_RTX) && (REGNO(regb) == REGNO(reg)))
        {
            return(1);
        }
        if ((regi != NULL_RTX) && (REGNO(regi) == REGNO(reg)))
        {
            return(1);
        }
        return(0);

    case REG:
        return(0);

    case SCRATCH:
    case CC0:
    case PC:
        return(0);

    case CONST_INT:
        return(0);
      
    case CONST_DOUBLE:
        return(0);
      
    default:
        break;
    }
    /*
    ** Abstract expression: get the format code.
    ** For each expression code, the rtx format specifies the number of
    ** contained objects and their kinds using a sequence of characters
    ** called the 'format' of the expression code.
    ** We are interested in expressions (format 'e') and vectors of
    ** expressions (format 'E').
    */
    fmt = GET_RTX_FORMAT(code);

    for (i = GET_RTX_LENGTH(code) - 1; i >= 0; i--)
    {
        int j;

        switch (fmt[i])
        {
        case 'E':
            /*
            ** A vector of expressions.
            */
            for (j = XVECLEN(in, i) - 1; j >= 0; j--)
            {
                rtx inv = XVECEXP(in, i, j);
                if (pic30_reg_dereferenced_p(reg, inv))
                {
                    return(1);
                }
            }
            break;
        case 'e':
            /*
            ** An expression (actually a pointer to an expression).
            */
            if (pic30_reg_dereferenced_p(reg, XEXP(in, i)))
            {
                return(1);
            }
            break;
        default:
            break;
        }
    }
    return(0);
}
/*
** This function corrects the value of COST based on the relationship between
** INSN and DEP_INSN through the dependence LINK. It should return the new
** value. The default is to make no adjustment to cost. This can be used for
** example to specify to the scheduler using the traditional pipeline
** description that an output- or anti-dependence does not incur the same cost
** as a data-dependence. If the scheduler using the automaton based pipeline
** description, the cost of anti-dependence is zero and the cost of
** output-dependence is maximum of one and the difference of latency times of
** the first and the second insns. If these values are not acceptable,
** you could use the hook to modify them too. 
*/
#define    DEF_USE_COST    2
#define    DEBUG_SCHED    0

static int
pic30_sched_adjust_cost(rtx insn, rtx link, rtx dep_insn, int cost)
{
    enum attr_type tyd;
    enum attr_type tyi;
    enum rtx_code code;
    rtx regd;
    rtx pati;
    rtx patd;

    cost = 0;
    /*
    ** Don't worry about this until we know what registers have been
    ** assigned.
    if ((flag_schedule_insns == 0) && !reload_completed)
    {
        return(cost);
    }
    */
    /*
    ** Reload sometimes generates a CLOBBER of a stack slot,
    ** so only deal with insns we know about.
    */
    if (recog_memoized(dep_insn) < 0)
    {
        return(cost);
    }
    if (REG_NOTE_KIND(link) == 0)
    {
        /*
        ** Data dependency:
        ** DEP_INSN writes a register that INSN reads some cycles later.
        */
        tyd = get_attr_type(dep_insn);
        tyi = get_attr_type(insn);
#if (DEBUG_SCHED)
        switch (tyd)
        {
        case TYPE_DEF:
            printf("D[%02d]=DEF ", INSN_UID(dep_insn));
            break;
        case TYPE_USE:
            printf("D[%02d]=USE ", INSN_UID(dep_insn));
            break;
        case TYPE_DEFUSE:
            printf("D[%02d]=D+U ", INSN_UID(dep_insn));
            break;
        case TYPE_ETC:
            printf("D[%02d]=ETC ", INSN_UID(dep_insn));
            break;
        }
        switch (tyi)
        {
        case TYPE_DEF:
            printf("U[%02d]=DEF ", INSN_UID(insn));
            break;
        case TYPE_USE:
            printf("U[%02d]=USE ", INSN_UID(insn));
            break;
        case TYPE_DEFUSE:
            printf("U[%02d]=D+U ", INSN_UID(insn));
            break;
        case TYPE_ETC:
            printf("U[%02d]=ETC ", INSN_UID(insn));
            break;
        }
        printf("\n");
#endif
        if ((tyi != TYPE_USE) && (tyi != TYPE_DEFUSE))
        {
            return(cost);
        }
        pati = PATTERN(insn);
        patd = PATTERN(dep_insn);
        
        if ((GET_CODE(pati) != SET) || (GET_CODE(patd) != SET))
        {
            /*
            ** probably a parallel with a clobber.
            */
#if (DEBUG_SCHED)
            printf("early exit (1)\n");
#endif
            return(cost);
        }
        if (SET_DEST(patd) == 0)
        {
            return(cost);
        }
        patd = SET_DEST(patd);
        code = GET_CODE(patd);
        regd = NULL_RTX;
        switch (code)
        {
        case REG:
            regd = patd;
            break;
        case SUBREG:
            regd = SUBREG_REG(patd);
            if (GET_CODE(regd) != REG)
            {
                regd = NULL_RTX;
            }
            break;
        case MEM:
            code = GET_CODE(XEXP(patd, 0));
            switch (code)
            {
            case REG:
                regd = XEXP(patd, 0);
                break;
            case POST_INC:
            case POST_DEC:
            case PRE_INC:
            case PRE_DEC:
                regd = XEXP(XEXP(patd, 0),0);
                if (GET_CODE(regd) != REG)
                {
                    regd = NULL_RTX;
                }
                break;
            default:
                break;
            }
            break;
        default:
            break;
        }
        if (regd == NULL_RTX)
        {
#if (DEBUG_SCHED)
            printf("early exit (2)\n");
#endif
            return(cost);
        }
        if (pic30_reg_dereferenced_p(regd, SET_SRC(pati)))
        {
#if (DEBUG_SCHED)
            printf("regnod=%s:%d used\n",
                    mode_name[GET_MODE(regd)],
                    REGNO(regd));
#endif
            cost = DEF_USE_COST;
        }
#if (DEBUG_SCHED)
        else
        {
            printf("regnod=%s:%d unused\n",
                    mode_name[GET_MODE(regd)],
                    REGNO(regd));
        }
#endif
    }
    else if (REG_NOTE_KIND(link) == REG_DEP_ANTI)
    {
        /*
        ** Anti dependency:
        ** DEP_INSN reads a register that INSN writes
        ** some cycles later.
        */
        cost = 0;
    }
    else if (REG_NOTE_KIND(link) == REG_DEP_OUTPUT)
    {
        /*
        ** Output dependency:
        ** DEP_INSN writes a register that INSN writes some
        ** cycles later.
        */
        cost = 0;
    }
    return(cost);
}
/*
** This Function returns nonzero if the DFA based scheduler interface
** is to be used.
*/
static int
pic30_sched_use_dfa_interface(void)
{
    return(1);
}

/*
** Provide the costs of an addressing mode that contains ADDR.
** If ADDR is not a valid address, its cost is irrelevant.  
** This is used in cse and loop optimisation to determine
** if it is worthwhile storing a common address into a register. 
*/
int
pic30_address_cost(op)
     rtx op;
{
    static int recurred = 0;
    int nCost = 3;

    switch (GET_MODE (op))
    {
#if Pmode != HImode
    case Pmode:
#endif
    case VOIDmode:
    case HImode:
    case QImode:
    case SImode:
        break;
    default:
        return(10);
    }
    switch (GET_CODE (op))
    {
    case MEM:
        if (!recurred)
        {
            switch (GET_CODE(XEXP(op, 0)))
            {
            case REG:
                nCost = 1;
                break;
            case SYMBOL_REF:
                if (SYMBOL_REF_FLAG(op))
                {
                    nCost = 0;
                }
                break;
            default:
                break;
            }
        }
        break;
    case PRE_DEC:
    case PRE_INC:
        break;
    case POST_DEC:
    case POST_INC:
        break;
    case MINUS:
    case MULT:
    case DIV:
    case IOR:
        nCost = 10;
        break;
    case PLUS:
        recurred = 1;
        nCost += pic30_address_cost(XEXP(op, 0));
        if (nCost < 10)
            nCost += pic30_address_cost(XEXP(op, 1));
        recurred = 0;
        break;
    case REG:
        nCost = 1;
        break;
    case PC:
    case CONST:
    case CONST_INT:
    case SUBREG:
    case ADDRESSOF:
    case ASM_OPERANDS:
        break;
    case SYMBOL_REF:
        if (SYMBOL_REF_FLAG(op))
        {
            nCost = 0;
        }
        break;
    default:
        printf ("pic30_address_cost: code=%d (MEM=%d)\n",
                    (int) GET_CODE (op), MEM);
        break;
    }
    return(nCost);
}

/*
 * returns ture if opnd is volatile
 *
 */
int pic30_volatile_operand(rtx opnd, enum machine_mode mode) {

  switch (GET_CODE(opnd)) {
    case SUBREG: return pic30_volatile_operand(SUBREG_REG(opnd), mode);
    case MEM:    return opnd->volatil;
    default: break;
  }
  return 0;
}


/************************************************************************/
/*                                    */
/*   Predicate for the Mode1 addressing modes.                */
/*   Mode1 addressing modes allow for:                    */
/*      Wn    Register to Register                    */
/*      [Wn]    Indirect                        */
/*      [Wn++]    Indirect with post-increment                */
/*      [Wn--]    Indirect with post-decrement                */
/*      [++Wn]    Indirect with pre-increment                */
/*      [--Wn]    Indirect with pre-decrement                */
/*      +k    Small literal, positive                    */
/*      -k    Small literal, negative                    */
/*                                    */
/************************************************************************/
static int
pic30_mode1MinMax_operand(op, mode, nMin, nMax)
   rtx op;
   enum machine_mode mode;
    int nMin, nMax;
{
    int nLiteral;
    int fMode1Operand;
    enum rtx_code code;

    fMode1Operand = 0;
    code = GET_CODE(op);
    switch (code)
    {
    case SUBREG:
        fMode1Operand = register_operand(op, mode);
        break;
    case REG:
        /*
        ** Register to register
        */
#if 1
        fMode1Operand = ((GET_MODE(op) == mode) &&
                         ((REGNO(op) <= WR15_REGNO) ||
                          (REGNO(op) >= FIRST_PSEUDO_REGISTER)));
#else
        fMode1Operand = (GET_MODE(op) == mode);
#endif
        break;
    case MEM:
        /*
        ** One of:
        ** Indirect: [Wn]
        ** Indirect with post-increment: [Wn++]
        ** Indirect with post-decrement: [Wn--]
        ** Indirect with pre-increment: [++Wn]
        ** Indirect with pre-decrement: [--Wn]
        */
        code = GET_CODE(XEXP(op, 0));
        switch (code)
        {
        case REG:
            /*
            ** Indirect
            */
            fMode1Operand = (GET_MODE(op) == mode);
            break;
        case POST_INC:
            /*
            ** Indirect with post-increment: [Wn++]
            */
        case POST_DEC:
            /*
            ** Indirect with post-decrement: [Wn--]
            */
        case PRE_INC:
            /*
            ** Indirect with pre-increment: [++Wn]
            */
        case PRE_DEC:
            /*
            ** Indirect with pre-decrement: [--Wn]
            */
            fMode1Operand = (GET_MODE(op) == mode) &&
                    ((mode == HImode) || (mode == QImode));
            break;
        default:
            break;
        }
        break;

    case CONST_INT:
        /*
        ** Literal: signed 5 bit
        */
        nLiteral = INTVAL(op);
        fMode1Operand = (nMin <= nLiteral) && (nLiteral <= nMax);
        break;
    default:
        break;
    }
    return(fMode1Operand);
}

int pic30_immediate_1bit_operand(rtx op, 
                                 enum machine_mode mode ATTRIBUTE_UNUSED) {

  if (GET_CODE(op) == CONST_INT) {
    unsigned int literal = INTVAL(op);
    
    literal = literal & 0xFFFF;
    return ((literal & (literal -1)) == 0);
  }
  return 0;
}

/*
** Canonical mode1: literal range is [-16,15]
*/
int
pic30_mode1_operand(op, mode)
   rtx op;
   enum machine_mode mode;
{
    return(pic30_mode1MinMax_operand(op, mode, -16, 15));
}
/*
** Special mode1: literal range is [0,31]
*/
int
pic30_mode1P_operand(op, mode)
   rtx op;
   enum machine_mode mode;
{
    return(pic30_mode1MinMax_operand(op, mode, 0, 31));
}
/*
** Special mode1: literal range is [-31,31]
*/
int
pic30_mode1PN_operand(op, mode)
   rtx op;
   enum machine_mode mode;
{
    return(pic30_mode1MinMax_operand(op, mode, -31, 31));
}
/*
** Special mode1: literal range is [-31,1023]
*/
int
pic30_mode1JN_operand(op, mode)
   rtx op;
   enum machine_mode mode;
{
    return(pic30_mode1MinMax_operand(op, mode, -31, 1023));
}
/*
** Special mode1: literal range is [0,1023]
*/
int
pic30_mode1J_operand(op, mode)
   rtx op;
   enum machine_mode mode;
{
    return(pic30_mode1MinMax_operand(op, mode, 0, 1023));
}

/*
 *      [Wn++]  Indirect with post-increment
 *      [Wn--]  Indirect with post-decrement
 *      [++Wn]  Indirect with pre-increment
 *      [--Wn]  Indirect with pre-decrement
 */

int
pic30_indirect_mem_operand_modify(rtx op, enum machine_mode mode) {
        if (GET_CODE(op) == MEM)
        switch (GET_CODE(XEXP(op,0)))
        {
        case POST_INC:
                /*
                ** Indirect with post-increment: [Wn++]
                */
        case POST_DEC:
                /*
                ** Indirect with post-decrement: [Wn--]
                */
        case PRE_INC:
                /*
                ** Indirect with pre-increment: [++Wn]
                */
        case PRE_DEC:
                /*
                ** Indirect with pre-decrement: [--Wn]
                */
                return (GET_MODE(op) == mode);
        default:
                break;
        }
        return FALSE;
}

/*
 *    [Wn]    Indirect
 *    [Wn++]    Indirect with post-increment
 *    [Wn--]    Indirect with post-decrement
 *    [++Wn]    Indirect with pre-increment
 *    [--Wn]    Indirect with pre-decrement
 */
int
pic30_indirect_mem_operand(rtx op, enum machine_mode mode) {

        if (GET_CODE(op) == MEM) {

          if (pic30_indirect_mem_operand_modify(op, mode)) return TRUE;
      switch (GET_CODE(XEXP(op,0)))
      {
      case REG:
          /*
          ** Indirect: [Wn]
          */
               return (GET_MODE(op) == mode);
      default:
          break;
      }
        }
    return FALSE;
}

/************************************************************************/
/* pic30_mode2_operand(): predicate for the mode2 addressing modes    */
/*                                    */
/* MODE2 addressing modifiers are used for:                */
/*    1) destination operands of 3 operand instructions        */
/*    2) source and destination of 2 operand instructions        */
/* The encoding is as follows:                        */
/*    Wn    Register direct                        */
/*    [Wn]    Indirect                        */
/*    [Wn++]    Indirect with post-increment                */
/*    [Wn--]    Indirect with post-decrement                */
/*    [++Wn]    Indirect with pre-increment                */
/*    [--Wn]    Indirect with pre-decrement                */
/************************************************************************/
int
pic30_mode2_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fMode2Operand;
    enum rtx_code code;

    fMode2Operand = FALSE;
    code = GET_CODE(op);
    switch (code)
    {
    case SUBREG:
        fMode2Operand = register_operand(op, mode);
        break;
    case REG:
        /*
        ** Register to register
        */
#if 1
        fMode2Operand = ((GET_MODE(op) == mode) &&
                         ((REGNO(op) <= WR15_REGNO) ||
                          (REGNO(op) >= FIRST_PSEUDO_REGISTER)));
#else
        fMode2Operand = (GET_MODE(op) == mode);
#endif

        break;
    case MEM:
        fMode2Operand = pic30_indirect_mem_operand(op,mode);
        break;
    default:
        break;
    }
    return(fMode2Operand);
}

/************************************************************************/
/* pic30_mode2res_operand(): predicate for restricted mode2 addr modes    */
/************************************************************************/
int
pic30_mode2res_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fMode2Operand;
    fMode2Operand = pic30_mode2_operand(op, mode);
    if (fMode2Operand)
    {
        if (GET_CODE(op) == MEM)
        {
            switch (GET_CODE(XEXP(op, 0)))
            {
            case PRE_INC:
                /*
                ** Indirect with pre-increment: [++Wn]
                */
            case POST_DEC:
                /*
                ** Indirect with post-decrement: [Wn--]
                */
                fMode2Operand = FALSE;
                break;
            default:
                break;
            }
        }
    }
    return(fMode2Operand);
}
/************************************************************************/
/* pic30_mode2mres_operand(): predicate for restricted mode2 addr modes    */
/************************************************************************/
int
pic30_mode2mres_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fMode2Operand;
    fMode2Operand = pic30_mode2_operand(op, mode);
    if (fMode2Operand)
    {
        if (GET_CODE(op) == MEM)
        {
            switch (GET_CODE(XEXP(op, 0)))
            {
            case PRE_INC:
                /*
                ** Indirect with pre-increment: [++Wn]
                */
            case PRE_DEC:
                /*
                ** Indirect with pre-decrement: [--Wn]
                */
            case POST_DEC:
                /*
                ** Indirect with post-decrement: [Wn--]
                */
                fMode2Operand = FALSE;
                break;
            default:
                break;
            }
        }
    }
    return(fMode2Operand);
}
/************************************************************************/
/* pic30_modek_operand(): predicate for the modek addressing modes    */
/*                                    */
/* MODEk addressing modifiers are used for source and destination of    */
/* move instructions (excluding TABLE & DSP single operand instructions)*/
/*                                    */
/* The encoding is as follows:                        */
/*    [Wn+k]    Indirect + displacement                    */
/************************************************************************/
int
pic30_modek_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fModekOperand;
    rtx rtxInner;
    rtx rtxPlusOp0;
    rtx rtxPlusOp1;
    int nMin, nMax, nDisp;

    fModekOperand = FALSE;
    switch (GET_CODE(op))
    {
    case SUBREG:
        /*
        ** Either a register, or a memory reference
        */
        if (GET_CODE(SUBREG_REG(op)) == MEM)
        {
            if (GET_MODE(op) == mode)
            {
                rtx subop;
                     enum machine_mode submd;

                subop = SUBREG_REG(op);
                submd = GET_MODE(subop);
                fModekOperand =
                    pic30_modek_operand(subop, submd);
            }
        }
        break;
    case MEM:
        rtxInner = XEXP(op, 0);
        switch (GET_CODE(rtxInner))
        {
        case PLUS:
            /*
            ** Base with displacement.
            */
              rtxPlusOp0 = XEXP(rtxInner, 0);
              switch (GET_CODE(rtxPlusOp0))
            {
            case SUBREG:
                if (!register_operand(rtxPlusOp0, Pmode))
                {
                    break;
                }
                /*
                ** Fall thru
                */
            case REG:
                rtxPlusOp1 = XEXP(rtxInner, 1);
                switch (GET_CODE(rtxPlusOp1))
                {
                case CONST_INT:
                    /*
                    ** Base with displacement.
                    */
                    switch (mode)
                    {
                    case QImode:
                        nMin = PIC30_DISP_MIN;
                        nMax = PIC30_DISP_MAX;
                        break;
                    case SFmode:
                    case DFmode:
                    case HImode:
                    case SImode:
                    case DImode:
                        nMin = PIC30_DISP_MIN*2;
                        nMax = PIC30_DISP_MAX*2 +
                            UNITS_PER_WORD -
                            GET_MODE_SIZE(mode);
                        break;
                    default:
                        nMin = 0;
                        nMax = 0;
                        break;
                    }
                    nDisp = INTVAL(rtxPlusOp1);
                    fModekOperand = (GET_MODE(op)==mode) &&
                       ((nMin <= nDisp) && (nDisp <= nMax));
                    break;
                default:
                    break;
                }
            default:
                break;
            }
        default:
            break;
        }
    default:
        break;
    }
    return(fModekOperand);
}
/************************************************************************/
/* pic30_mode3_operand(): predicate for the mode3 addressing modes    */
/*                                    */
/* MODE3 addressing modifiers are used for source and destination of    */
/* move instructions (including TABLE & DSP single operand instructions)*/
/*                                    */
/* The encoding is as follows:                        */
/*    Wn    Register direct                        */
/*    [Wn]    Indirect                        */
/*    [Wn++]    Indirect with post-increment                */
/*    [Wn--]    Indirect with post-decrement                */
/*    [--Wn]    Indirect with pre-decrement                */
/*    [++Wn]    Indirect with pre-increment                */
/*    [Wn+Wb]    Indirect with base                    */
/************************************************************************/
int
pic30_mode3_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fMode3Operand;
    rtx rtxInner;
    rtx rtxPlusOp0;
    rtx rtxPlusOp1;

    fMode3Operand = FALSE;
    switch (GET_CODE(op))
    {
    case ADDRESSOF:
        /*
        ** (addressof:m REG) 
        ** This RTX represents a request for the address of register
        ** REG. Its mode is always Pmode. If there are any addressof
        ** expressions left in the function after CSE, REG is forced
        ** into the stack and the addressof expression is replaced with
        ** a plus expression for the address of its stack slot.
        ** We accept this as a valid operand, otherwise gcc crashes,
        ** since it needs a rule that can move this kind of operand.
        ** It's safe to do this, since it will be eliminated by the
        ** time we get to code generation.
        */
        fMode3Operand = TRUE;
        break;
    case SUBREG:
        /*
        ** Either a register, or a memory reference
        */
        if (GET_CODE(SUBREG_REG(op)) == MEM)
        {
            if (GET_MODE(op) == mode)
            {
                rtx subop;
                     enum machine_mode submd;

                subop = SUBREG_REG(op);
                submd = GET_MODE(subop);
                fMode3Operand =
                    pic30_mode3_operand(subop, submd);
            }
        }
        else
        {
            fMode3Operand = register_operand(op, mode);
        }
        break;
    case REG:
        /*
        ** Register to register
        */
        fMode3Operand = ((GET_MODE(op) == mode) && 
                         ((REGNO(op) <= WR15_REGNO) || 
                          (REGNO(op) >= FIRST_PSEUDO_REGISTER)));
        break;
    case MEM:
        rtxInner = XEXP(op, 0);
        switch (GET_CODE(rtxInner))
        {
        case ADDRESSOF:
            /*
            ** See comment above.
            */
            fMode3Operand = TRUE;
            break;
        case SUBREG:
            fMode3Operand = (GET_MODE(op) == mode);
            break;
        case REG:
            fMode3Operand = (GET_MODE(op) == mode);
            break;
        case POST_INC:
            fMode3Operand = (GET_MODE(op) == mode);
            break;
        case PRE_DEC:
            fMode3Operand = (GET_MODE(op) == mode);
            break;
        case PRE_INC:
        case POST_DEC:
            switch (mode)
            {
            case DFmode:
            case DImode:
                /*
                ** mov.q in these modes can't
                ** be synthesized from mov.d
                */
                fMode3Operand = FALSE;
                break;
            default:
                fMode3Operand = (GET_MODE(op) == mode);
                break;
            }
            break;
        case PRE_MODIFY:
        case POST_MODIFY:
            /*
            ** Not yet implemented in gcc.
            */
            break;
        case PLUS:
            /*
            ** Base with index.
            */
              rtxPlusOp0 = XEXP(rtxInner, 0);
              switch (GET_CODE(rtxPlusOp0))
            {
            case SUBREG:
                if (!register_operand(rtxPlusOp0, Pmode))
                {
                    break;
                }
                /*
                ** Fall thru
                */
            case REG:
                rtxPlusOp1 = XEXP(rtxInner, 1);
                switch (GET_CODE(rtxPlusOp1))
                {
                case SUBREG:
                    if (!register_operand(rtxPlusOp1,Pmode))
                    {
                        break;
                    }
                    /*
                    ** Fall thru
                    */
                case REG:
                    /*
                    ** Base with index
                    */
                    fMode3Operand = (GET_MODE(op)==mode) &&
                            ((mode == QImode) ||
                             (mode == HImode));
                    break;
                default:
                    break;
                }
                break;
            default:
                break;
            }
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
    return(fMode3Operand);
}
/************************************************************************/
/* pic30_wreg_operand(): predicate for the WREG addressing mode        */
/************************************************************************/
int
pic30_wreg_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fWReg = FALSE;

    if (register_operand(op, mode))
    {
        if (GET_CODE(op) == SUBREG)
        {
            op = SUBREG_REG(op);
        }
        fWReg = REG_P(op) && IS_AREG_OR_PSEUDO_REGNO(op);
    }
    return(fWReg);
}
/************************************************************************/
/* pic30_breg_operand(): predicate for the W1 addressing mode        */
/************************************************************************/
int
pic30_breg_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int bBReg = FALSE;

    if (register_operand(op, mode))
    {
        if (GET_CODE(op) == SUBREG)
        {
            op = SUBREG_REG(op);
        }
        bBReg = REG_P(op) && IS_BREG_OR_PSEUDO_REGNO(op);
    }
    return(bBReg);
}
/************************************************************************/
/* pic30_creg_operand(): predicate for the W2 addressing mode        */
/************************************************************************/
int
pic30_creg_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int bCReg = FALSE;

    if (register_operand(op, mode))
    {
        if (GET_CODE(op) == SUBREG)
        {
            op = SUBREG_REG(op);
        }
        bCReg = REG_P(op) && IS_CREG_OR_PSEUDO_REGNO(op);
    }
    return(bCReg);
}
/************************************************************************/
/* pic30_ereg_operand(): predicate for the W2..W14 addressing mode    */
/************************************************************************/
int
pic30_ereg_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int bEReg = FALSE;

    if (register_operand(op, mode))
    {
        if (GET_CODE(op) == SUBREG)
        {
            op = SUBREG_REG(op);
        }
        bEReg = REG_P(op) && IS_EREG_OR_PSEUDO_REGNO(op);
    }
    return(bEReg);
}
/************************************************************************/
/* pic30_near_operand(): predicate for the near addressing mode        */
/************************************************************************/
int
pic30_near_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fNear;

    fNear = ((mode == VOIDmode) || (mode == GET_MODE(op))) &&
                pic30_U_constraint(op);

    return(fNear);
}
/************************************************************************/
/* pic30_T_operand(): predicate for the data addressing mode        */
/************************************************************************/
int
pic30_T_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fT;

    fT = (mode == GET_MODE(op)) && pic30_T_constraint(op);

    return(fT);
}
/************************************************************************/
/* pic30_data_operand(): predicate for the data addressing mode        */
/************************************************************************/
int
pic30_data_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fData;

    switch (mode)
    {
    case QImode:
        fData = FALSE;
        break;
    default:
        fData = (mode == GET_MODE(op)) && pic30_T_constraint(op);
        break;
    }
    return(fData);
}
/************************************************************************/
/* pic30_code_operand(): predicate for the code addressing mode        */
/************************************************************************/
int
pic30_code_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fCode;

    fCode = (mode == GET_MODE(op)) &&
        (MEM == GET_CODE(op)) &&
        pic30_program_space_operand_p(XEXP(op,0));

    return(fCode);
}
/************************************************************************/
/* pic30_reg_or_code_operand(): predicate for reg or code addr mode    */
/************************************************************************/
int
pic30_reg_or_code_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    return(register_operand(op, mode) || pic30_code_operand(op, mode));
}
/************************************************************************/
/* pic30_reg_or_near_operand(): predicate for reg/near data addr mode    */
/************************************************************************/
int
pic30_reg_or_near_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    return(register_operand(op, mode) || pic30_near_operand(op, mode));
}
/************************************************************************/
/* pic30_reg_imm_or_near_operand(): predicate for reg/imm/near addr mode*/
/************************************************************************/
int
pic30_reg_imm_or_near_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    return(    register_operand(op, mode) ||
        immediate_operand(op, mode) ||
        pic30_near_operand(op, mode));
}
/************************************************************************/
/* pic30_rR_or_near_operand(): predicate for the r/R/near data addr mode*/
/************************************************************************/
int
pic30_rR_or_near_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    return(    pic30_reg_or_R_operand(op, mode) ||
        pic30_near_operand(op, mode));
}
/************************************************************************/
/* pic30_wreg_or_near_operand(): predicate for the WREG/near data addr mode*/
/************************************************************************/
int
pic30_wreg_or_near_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    return(pic30_wreg_operand(op, mode) || pic30_near_operand(op, mode));
}
/************************************************************************/
/* pic30_reg_or_imm_operand(): predicate for register/imm addressing.    */
/************************************************************************/
int
pic30_reg_or_imm_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fRegOrImm;

    fRegOrImm = register_operand(op, mode) || immediate_operand(op, mode);

    return(fRegOrImm);
}
/************************************************************************/
/* pic30_imm2to15_operand(): predicate for imm const [2,15] addressing    */
/************************************************************************/
int
pic30_imm2to15_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fImm2to15;

    fImm2to15 = immediate_operand(op, mode) && 
            (2 <= INTVAL(op)) && (INTVAL(op) <= 15);

    return(fImm2to15);
}
/************************************************************************/
/* pic30_imm8_operand(): predicate for imm const 8 addressing        */
/************************************************************************/
int
pic30_imm8_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fImm8;

    fImm8 = immediate_operand(op, mode) && (INTVAL(op) == 8);

    return(fImm8);
}
/************************************************************************/
/* pic30_imm16plus_operand(): predicate for imm const 16+ addressing    */
/************************************************************************/
int
pic30_imm16plus_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fImm16;

    fImm16 = immediate_operand(op, mode) && (INTVAL(op) >= 16);

    return(fImm16);
}
/************************************************************************/
/* pic30_math_operand(): predicate for the math instruction operand.    */
/*                                    */
/* This is needed to satisfy gen_add2_insn in optab.c, which needs    */
/* addhi to accept an immediate operand.  Constraints will prevent    */
/* a non-J or -K operand from actually being accepted.            */
/************************************************************************/
int
pic30_math_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fMathOperand;

    fMathOperand = pic30_mode1_operand(op, mode) ||
            immediate_operand(op, mode);

    return(fMathOperand);
}
/************************************************************************/
/* pic30_inc_imm_operand(): predicate for imm opnd suitable for inc/dec */
/************************************************************************/
int 
pic30_inc_imm_operand(op, mode) 
     rtx op;
     enum machine_mode mode;
{
    return immediate_operand(op,mode) && 
               (-2<=INTVAL(op)) && (INTVAL(op)!=0) && (INTVAL(op)<=2);
}

/************************************************************************/
/* pic30_near_math_operand(): predicate for the math instruction operand*/
/************************************************************************/
int
pic30_near_math_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    return(    pic30_math_operand(op, mode) ||
        pic30_wreg_or_near_operand(op, mode));
}
/************************************************************************/
/* pic30_near_mode2_operand(): predicate for math instruction operand.    */
/************************************************************************/
int
pic30_near_mode2_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    return(    pic30_mode2_operand(op, mode) ||
        pic30_wreg_or_near_operand(op, mode));
}
/************************************************************************/
/* pic30_near_mode1PN_operand(): predicate for math instruction operand.*/
/************************************************************************/
int
pic30_near_mode1PN_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    return(    pic30_mode1PN_operand(op, mode) ||
        pic30_wreg_or_near_operand(op, mode));
}
/************************************************************************/
/* pic30_move_operand(): predicate for the move instruction operand.    */
/************************************************************************/
int
pic30_move_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    int fMoveOperand;

    fMoveOperand = pic30_mode3_operand(op, mode) ||
            pic30_modek_operand(op, mode) ||
            pic30_near_operand(op, mode) ||
            pic30_data_operand(op, mode);

    return(fMoveOperand);
}

/* alternative move operand that does not have a near in it */
int pic30_move2_operand(rtx op, enum machine_mode mode) {
   return pic30_mode3_operand(op,mode) ||
          pic30_modek_operand(op,mode) ||
          pic30_data_operand(op,mode);
}

/* 
 *  copy of register_operand(); we dont' want it match on ACCUM_REGS
 *
 *  becuase that will break most peepholes and such
 */
int pic30_register_operand(rtx op, enum machine_mode mode) {
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    {
      rtx sub = SUBREG_REG (op);

      /* Before reload, we can allow (SUBREG (MEM...)) as a register operand
         because it is guaranteed to be reloaded into one.
         Just make sure the MEM is valid in itself.
         (Ideally, (SUBREG (MEM)...) should not exist after reload,
         but currently it does result from (SUBREG (REG)...) where the
         reg went on the stack.)  */
      if (! reload_completed && GET_CODE (sub) == MEM)
        return general_operand (op, mode);

#ifdef CANNOT_CHANGE_MODE_CLASS
      if (GET_CODE (sub) == REG
          && REGNO (sub) < FIRST_PSEUDO_REGISTER
          && REG_CANNOT_CHANGE_MODE_P (REGNO (sub), GET_MODE (sub), mode)
          && GET_MODE_CLASS (GET_MODE (sub)) != MODE_COMPLEX_INT
          && GET_MODE_CLASS (GET_MODE (sub)) != MODE_COMPLEX_FLOAT)
        return 0;
#endif

      /* FLOAT_MODE subregs can't be paradoxical.  Combine will occasionally
         create such rtl, and we must reject it.  */
      if (GET_MODE_CLASS (GET_MODE (op)) == MODE_FLOAT
          && GET_MODE_SIZE (GET_MODE (op)) > GET_MODE_SIZE (GET_MODE (sub)))
        return 0;

      op = sub;
    }

  /* If we have an ADDRESSOF, consider it valid since it will be
     converted into something that will not be a MEM.  */
  if (GET_CODE (op) == ADDRESSOF)
    return 1;

  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  return (GET_CODE (op) == REG && 
          (REGNO (op) >= FIRST_PSEUDO_REGISTER || 
          ((REGNO_REG_CLASS (REGNO (op)) != NO_REGS) && 
           (REGNO_REG_CLASS(REGNO(op)) != ACCUM_REGS))));
}

int pic30_accumulator_operand(rtx op, enum machine_mode mode ATTRIBUTE_UNUSED) {

  switch (GET_CODE(op)) {
    case REG:
      return (IS_ACCUM_REG(REGNO(op)) ||
#if 1
              0);
#else
             (REGNO(op) >= FIRST_PSEUDO_REGISTER));
#endif
    default:  break;
  } 
  return 0;
}

int pic30_xprefetch_operand(rtx op, enum machine_mode mode ATTRIBUTE_UNUSED) {

  switch (GET_CODE(op)) {
    case CONST_INT: return 1;
    case REG:
      return (IS_XPREFETCH_REG(REGNO(op)) ||
              (REGNO(op) >= FIRST_PSEUDO_REGISTER));
    default:  break;
  }
  return 0;
}

int pic30_yprefetch_operand(rtx op, enum machine_mode mode ATTRIBUTE_UNUSED) {

  switch (GET_CODE(op)) {
    case CONST_INT: return 1;
    case REG:
      return (IS_YPREFETCH_REG(REGNO(op)) ||
              (REGNO(op) >= FIRST_PSEUDO_REGISTER));
    default:  break;
  }
  return 0;
}

int pic30_mac_input_operand(rtx op, enum machine_mode mode ATTRIBUTE_UNUSED) {

  switch (GET_CODE(op)) {
    case CONST_INT: return 1;
    case REG:
      return (IS_PRODUCT_REG(REGNO(op)) ||
              (REGNO(op) >= FIRST_PSEUDO_REGISTER));
    default:  break;
  }
  return 0;
}

int pic30_awb_operand(rtx op, enum machine_mode mode ATTRIBUTE_UNUSED) {
  switch (GET_CODE(op)) {
    case CONST_INT: return 1;
    case REG: 
      return (IS_AWB_REG(REGNO(op)) || (REGNO(op) >= FIRST_PSEUDO_REGISTER));
    default: break;
  } 
  return 0;
}

int pic30_valid_operator(rtx op, enum machine_mode mode) {
  return ((GET_RTX_CLASS(GET_CODE(op)) == '2') ||
          (GET_RTX_CLASS(GET_CODE(op)) == 'c'));
}
 
/************************************************************************/
/* pic30_mode3_index(): Extract index field for mode3            */
/************************************************************************/
static void
pic30_mode3_index(rtx op, int *pnIndex)
{
    rtx rtxInner;
    rtx rtxPlusOp0;
    rtx rtxPlusOp1;

    pnIndex[0] = 0;
    pnIndex[1] = INT_MAX;
    switch (GET_CODE(op))
    {
    case REG:
    case SUBREG:
        /*
        ** Register to register
        */
        break;
    case MEM:
        rtxInner = XEXP(op, 0);
        switch (GET_CODE(rtxInner))
        {
        case REG:
        case SUBREG:
            break;
        case POST_DEC:
        case PRE_DEC:
            break;
        case POST_INC:
        case PRE_INC:
            break;
        case PLUS:
            /*
            ** Base with displacement.
            ** Base with index.
            */
            pnIndex[0] = INT_MAX;
              rtxPlusOp0 = XEXP(rtxInner, 0);
              if (GET_CODE(rtxPlusOp0) == REG)
            {
                rtxPlusOp1 = XEXP(rtxInner, 1);
                switch (GET_CODE(rtxPlusOp1))
                {
                case SUBREG:
                    rtxPlusOp1 = SUBREG_REG(rtxPlusOp1);
                    if (GET_CODE(rtxPlusOp1) != REG)
                    {
                        break;
                    }
                case REG:
                    /*
                    ** Base with index.
                    */
                    pnIndex[0] = REGNO(rtxPlusOp0);
                    pnIndex[1] = REGNO(rtxPlusOp1);
                    break;
                case CONST_INT:
                    /*
                    ** Base with displacement.
                    */
                    break;
                default:
                    break;
                }
            }
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
}
/************************************************************************/
/* pic30_IndexEqual(): Compare the index field for 2 mode3 ops        */
/************************************************************************/
int
pic30_IndexEqual(op0, op1)
     rtx op0;
     rtx op1;
{
    int fEqual;
    int nIndex0[2];
    int nIndex1[2];
    
    pic30_mode3_index(op0, nIndex0);
    pic30_mode3_index(op1, nIndex1);

    fEqual = (nIndex0[0] != INT_MAX) &&
        ((nIndex0[0] == nIndex1[0]) ||
         (nIndex0[0] == nIndex1[1]));
    if (!fEqual && (nIndex0[1] != INT_MAX))
    {
        fEqual = (nIndex0[1] == nIndex1[0]) ||
             (nIndex0[1] == nIndex1[1]);
    }
    if (!fEqual && (nIndex1[1] != INT_MAX))
    {
        fEqual = (nIndex0[0] == nIndex1[1]) ||
             (nIndex0[1] == nIndex1[1]);
    }
    return(fEqual);
}
/************************************************************************/
/* pic30_registerpairs_p(): See if a two sets of registers are pairs.    */
/************************************************************************/
int
pic30_registerpairs_p(op0, op1, op2, op3)
     rtx op0;
     rtx op1;
     rtx op2;
     rtx op3;
{
    int id0, id1;
    int id2, id3;
    int fPair = FALSE;

    id0 = REGNO(op0);
    id1 = REGNO(op1);
    id2 = REGNO(op2);
    id3 = REGNO(op3);
    if (id0 == (id1 - 1))
    {
        fPair = ((id0 & 1) == 0) &&
            ((id2 & 1) == 0) &&
            (id2 == (id3 - 1));
    }
    else if (id1 == (id0 - 1))
    {
        fPair = ((id1 & 1) == 0) &&
            ((id3 & 1) == 0) &&
            (id3 == (id2 - 1));
    }
    return(fPair);
}

/*
** Predicate for symbolic address operand.
*/
int
pic30_symbolic_address_operand(op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
    switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
        return(TRUE);
    case CONST:
        op = XEXP (op, 0);
        return ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
            || GET_CODE (XEXP (op, 0)) == LABEL_REF)
            && GET_CODE (XEXP (op, 1)) == CONST_INT);
    default:
        return(FALSE);
    }
}

/*
** More fully check address operand, ensuring the destination address is
**   in the correct section.
*/
static int
pic30_valid_symbolic_address_operand(register rtx op,
     enum machine_mode mode ATTRIBUTE_UNUSED)
{
   tree sym;
   const char *real_name;

   switch (GET_CODE (op))
   {
      case MEM: return pic30_valid_symbolic_address_operand(XEXP(op,0), mode);
      case SYMBOL_REF:
      case LABEL_REF:
         real_name = pic30_strip_name_encoding_helper(XSTR(op,0));
         sym = maybe_get_identifier(real_name);
         if (sym == 0) return (TRUE);
         sym = lookup_name(sym);
         if (sym == 0) return (TRUE);
         if (TREE_CODE(sym) != FUNCTION_DECL) {
            fatal_error("Calling non-function symbol '%s' is not possible\n", 
                        real_name);
            return (FALSE);
         }
         return (TRUE);
      case CONST:
         op = XEXP (op, 0);
         if ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
             || GET_CODE (XEXP (op, 0)) == LABEL_REF)
             && GET_CODE (XEXP (op, 1)) == CONST_INT) {
            op = XEXP (op, 0);
            real_name = pic30_strip_name_encoding_helper(XSTR(op,0));
            sym = maybe_get_identifier(real_name);
            if (sym == 0) return (TRUE);
            sym = lookup_name(sym);
            if (sym == 0) return (TRUE);
            if (TREE_CODE(sym) != FUNCTION_DECL) {
               fatal_error("Calling non-function symbol '%s' is "
                           "not possible\n", real_name);
               return (FALSE);
            }
            return (TRUE);
         }
      default: return(FALSE);
   }
}

int
pic30_valid_call_address_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
        return(REG_P(op) || pic30_valid_symbolic_address_operand(op, mode));
}

int pic30_invalid_address_operand(rtx op, enum machine_mode mode) {

  switch (GET_CODE(op)) {
    default: return FALSE;
    case MEM: return pic30_invalid_address_operand(XEXP(op,0), mode);
    case LABEL_REF:  return 0;
    case SYMBOL_REF:
      if (PIC30_HAS_NAME_P(XSTR(op,0), PIC30_PROG_FLAG)) {
        const char *real_name = pic30_strip_name_encoding_helper(XSTR(op,0));
        tree sym=0;

        sym = maybe_get_identifier(real_name);
        if (sym) sym = lookup_name(sym);
        if (sym) error_with_decl(sym, "Inappropriate program address '%s'", 
                                 real_name);
        else error("Inappropriate program address '%s'", real_name);
        return 1;
      }
      break;
    case CONST:
      op = XEXP (op, 0);
      if (((GET_CODE (XEXP (op, 0)) == SYMBOL_REF) || 
           (GET_CODE (XEXP (op, 0)) == LABEL_REF)) && 
          (GET_CODE (XEXP (op, 1)) == CONST_INT)) 
      return pic30_invalid_address_operand(XEXP(op,0), mode);
  }
  return 0;
}

/************************************************************************/
/* pic30_call_address_operand(): Predicate for the call insn operand.    */
/************************************************************************/
int
pic30_call_address_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
      return(REG_P(op) || pic30_symbolic_address_operand(op, mode));
}
/************************************************************************/
/* pic30_reg_or_zero_operand(): Predicate for register or zero operand.    */
/************************************************************************/
int
pic30_reg_or_zero_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    return(register_operand(op, mode) || (op == CONST0_RTX(mode)));
}
/************************************************************************/
/* pic30_rR_or_zero_operand(): Predicate for register or zero operand.    */
/************************************************************************/
int
pic30_rR_or_zero_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    return(pic30_reg_or_R_operand(op, mode) || (op == CONST0_RTX(mode)));
}
/************************************************************************/
/* pic30_emit_move_sequence(): Expand a move RTX.            */
/************************************************************************/
void
pic30_emit_move_sequence(operands, mode)
     rtx *operands;
     enum machine_mode mode;     
{
    int fForce;
    rtx op0;
    rtx op1;

    if (!reload_in_progress && ((mode == DImode) || (mode == DFmode)))
    {
        int id;

        /*
        ** Check for pre-increment and post-decrement addressing.
        ** (mov.q can't be synthesized from mov.d for these modes.)
        */
        for (id = 0; id < 2; ++id)
        {
            if (GET_CODE(operands[id]) == MEM)
            {
                enum rtx_code code;
                rtx reg;
                code = GET_CODE(XEXP(operands[id],0));
                switch (code)
                {
                case PRE_INC:
                    /*
                    ** Transform
                    **    [++wn]
                    ** to
                    **    wn += k ; [wn]
                    */
                    reg = XEXP(XEXP(operands[id], 0), 0);
                    emit_insn((rtx)gen_addhi3(reg, reg,
                        GEN_INT(GET_MODE_SIZE(mode))));
                    operands[id] = gen_rtx_MEM(mode, reg);
                    break;
                    case POST_DEC:
                    /*
                    ** Transform
                    **    [wn--]
                    ** to
                    **    [wn] ; wn -= k
                    */
                    reg = XEXP(XEXP(operands[id], 0), 0);
                    operands[id] = gen_rtx_MEM(mode, reg);
                    emit_insn((rtx)gen_addhi3(reg, reg,
                        GEN_INT(-GET_MODE_SIZE(mode))));
                    break;
                case SYMBOL_REF:
                    /*
                    ** Transform:
                    **    global
                    ** to
                    **    wn = &global ; [wn]
                    reg = gen_reg_rtx(Pmode);
                    emit_insn((rtx)
                        gen_movhi_address(reg,
                                operands[id]));
                    operands[id] = gen_rtx_MEM(mode, reg);
                    */
                    break;
                default:
                    break;
                }
            }
        }
    }
    op0 = operands[0];
    op1 = operands[1];
    /*
    ** Check for move immediate.
    */
    if (!reload_in_progress && (GET_CODE(op0) == MEM))
    {
        fForce = FALSE;
        switch (GET_CODE(op1))
        {
        case CONST:
        case CONST_INT:
        case CONST_DOUBLE:
        case SYMBOL_REF:
        case LABEL_REF:
            /*
            ** Source operand is an immediate constant.
            ** Insert a load to a register.
            */
            fForce = TRUE;
            break;
        default:
            break;
        }
        if (fForce)
        {
            op1 = force_reg(mode, op1);
        }
    }
    /*
    ** Adjust operands in case we have modified them.
    */
    operands[0] = op0;
    operands[1] = op1;
}
/************************************************************************/
/* pic30_O_operand(): dsPIC30 operand O: constant 0            */
/************************************************************************/
int
pic30_O_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    return(op == CONST0_RTX (mode));
}
/************************************************************************/
/* pic30_I_operand(): dsPIC30 operand I: constant 1            */
/************************************************************************/
int
pic30_I_operand(op, mode)
     rtx op;
     enum machine_mode mode;
{
    return(op == CONST1_RTX (mode));
}
/************************************************************************/
/* pic30_J_operand(): dsPIC30 operand J: 10-bit, unsigned        */
/************************************************************************/
int
pic30_J_operand(op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
    int fJ = FALSE;

    if (    (GET_CODE(op) == CONST_INT) &&
        (0 <= INTVAL(op) && INTVAL(op) <= 1023))
    {
        fJ = TRUE;
    }
    return(fJ);
}
/************************************************************************/
/* pic30_M_operand(): dsPIC30 operand M: 10-bit, unsigned        */
/************************************************************************/
int
pic30_M_operand(op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
    int fM = FALSE;

    if (    (GET_CODE(op) == CONST_INT) &&
        (-1023 <= INTVAL(op) && INTVAL(op) <= 0))
    {
        fM = TRUE;
    }
    return(fM);
}
/************************************************************************/
/* pic30_JM_operand(): dsPIC30 operand J&M: 10-bit, signed & unsigned    */
/************************************************************************/
int
pic30_JM_operand(op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
    int fJM = FALSE;

    if (    (GET_CODE(op) == CONST_INT) &&
        (-1023 <= INTVAL(op) && INTVAL(op) <= 1023))
    {
        fJM = TRUE;
    }
    return(fJM);
}
/************************************************************************/
/* pic30_JN_operand(): dsPIC30 operand J&N: 10-bit, signed & unsigned    */
/************************************************************************/
int
pic30_JN_operand(op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
    int fJN = FALSE;

    if (    (GET_CODE(op) == CONST_INT) &&
        (-31 <= INTVAL(op) && INTVAL(op) <= 1023))
    {
        fJN = TRUE;
    }
    return(fJN);
}
/************************************************************************/
/* pic30_PN_operand(): dsPIC30 operand P&N: 5-bit, signed & unsigned    */
/************************************************************************/
int
pic30_PN_operand(op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
    int fPN = FALSE;

    if (    (GET_CODE(op) == CONST_INT) &&
        (-31 <= INTVAL(op) && INTVAL(op) <= 31))
    {
        fPN = TRUE;
    }
    return(fPN);
}
/************************************************************************/
/* pic30_reg_or_P_operand(): dsPIC30 operand reg/P: 5-bit, unsigned    */
/************************************************************************/
int
pic30_reg_or_P_operand(op, mode)
    rtx op;
    enum machine_mode mode;
{
    return(register_operand(op, mode) || pic30_P_operand(op, mode));
}
/************************************************************************/
/* pic30_mode2_or_P_operand(): dsPIC30 operand mode2/P: 5-bit, unsigned    */
/************************************************************************/
int
pic30_mode2_or_P_operand(rtx op, enum machine_mode mode)
{
    return(pic30_mode2_operand(op, mode) || pic30_P_operand(op, mode));
}
/************************************************************************/
/* pic30_rR_or_JN_operand(): dsPIC30 operand J&M: 11-bit, signed    */
/************************************************************************/
int
pic30_rR_or_JN_operand(op, mode)
    rtx op;
    enum machine_mode mode;
{
    return(pic30_reg_or_R_operand(op, mode) || pic30_JN_operand(op, mode));
}
/************************************************************************/
/* pic30_reg_or_R_operand(): dsPIC30 operand r or R            */
/************************************************************************/
int
pic30_reg_or_R_operand(op, mode)
    rtx op;
    enum machine_mode mode;
{
    return(register_operand(op, mode) || pic30_R_operand(op, mode));
}
/************************************************************************/
/* pic30_P_operand(): dsPIC30 operand P: 0 <= k <= 31            */
/************************************************************************/
int
pic30_P_operand(op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
    int fP = FALSE;

    if (    (GET_CODE(op) == CONST_INT) &&
        ((0 <= INTVAL(op)) && (INTVAL(op) <= 31)))
    {
        fP = TRUE;
    }
    return(fP);
}
/************************************************************************/
/* pic30_Q_base(): extract the base for the Q addressing mode.        */
/************************************************************************/
int
pic30_Q_base(op)
    rtx op;
{
    int idBase = FIRST_PSEUDO_REGISTER;
    rtx rtxPlusop0;
    rtx rtxPlusop1;
    rtx rtxInner;

    switch (GET_CODE(op))
    {
    case MEM:
        rtxInner = XEXP(op, 0);
        switch (GET_CODE(rtxInner))
        {
        case PLUS:
            rtxPlusop0 = XEXP(rtxInner, 0);
            rtxPlusop1 = XEXP(rtxInner, 1);
            if (GET_CODE(rtxPlusop0) == REG)
            {
                idBase = REGNO(rtxPlusop0);
            }
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
    return(idBase);
}
/************************************************************************/
/* pic30_Q_displacement(): extract the displacement for the Q address    */
/************************************************************************/
int
pic30_Q_displacement(op)
    rtx op;
{
    int nDisplacement = INT_MAX;
    rtx rtxPlusOp0;
    rtx rtxPlusOp1;
    rtx rtxInner;

    switch (GET_CODE(op))
    {
    case MEM:
        rtxInner = XEXP(op, 0);
        switch (GET_CODE(rtxInner))
        {
        case PLUS:
            rtxPlusOp0 = XEXP(rtxInner, 0);
            rtxPlusOp1 = XEXP(rtxInner, 1);
            if (GET_CODE(rtxPlusOp0) == SUBREG)
            {
                rtxPlusOp0 = SUBREG_REG(rtxPlusOp0);
            }
            if (GET_CODE(rtxPlusOp0) == REG)
            {
                if (GET_CODE(rtxPlusOp1) == CONST_INT)
                {
                    nDisplacement = INTVAL(rtxPlusOp1);
                }
            }
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
    return(nDisplacement);
}
/************************************************************************/
/* pic30_Q_constraint(): dsPIC30 constraint Q: base+displacement    */
/************************************************************************/
int
pic30_Q_constraint(op)
     rtx op;
{
    int bQ;
    int nDisplacement;
    int nScale;

    nScale = (GET_MODE(op) == QImode) ? 1 : 2;
    nDisplacement = pic30_Q_displacement(op);
     bQ =    (nDisplacement >= (PIC30_DISP_MIN*nScale)) &&
        (nDisplacement <= (PIC30_DISP_MAX*nScale));
    return(bQ);
}
/************************************************************************/
/* pic30_Q_operand(): dsPIC30 predicate Q: base+displacement addressing    */
/************************************************************************/
int
pic30_Q_operand(op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
    return((mode == GET_MODE(op)) && pic30_Q_constraint(op));
}
/************************************************************************/
/* pic30_R_constraint(): dsPIC30 constraint R: indirect addressing    */
/************************************************************************/
int
pic30_R_constraint(op)
     rtx op;
{
    int fR = FALSE;
    rtx rtxInner;

    switch (GET_CODE(op))
    {
    case MEM:
        rtxInner = XEXP(op, 0);
        switch (GET_CODE(rtxInner))
        {
         case SUBREG:
            fR = register_operand(rtxInner, Pmode);
            break;
         case REG:
            fR = ((REGNO(op) <= WR15_REGNO) ||
                  (REGNO(op) >= FIRST_PSEUDO_REGISTER));
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
    return(fR);
}
/************************************************************************/
/* pic30_R_operand(): dsPIC30 predicate R: indirect addressing        */
/************************************************************************/
int
pic30_R_operand(op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
    return(pic30_R_constraint(op));
}
/************************************************************************/
/* pic30_S_constraint(): dsPIC30 constraint S: base+index        */
/************************************************************************/
int
pic30_S_constraint(op)
     rtx op;
{
    int fS = FALSE;
    enum machine_mode mode = VOIDmode;
    rtx rtxInner;
    rtx rtxPlusOp0;
    rtx rtxPlusOp1;

    switch (GET_CODE(op))
    {
    case MEM:
        rtxInner = XEXP(op, 0);
        if (GET_CODE(rtxInner) == PLUS)
        {
            rtxPlusOp0 = XEXP(rtxInner, 0);
            switch (GET_CODE(rtxPlusOp0))
            {
            case SUBREG:
                if (!register_operand(rtxPlusOp0, mode))
                {
                    break;
                }
                /*
                ** Fall thru
                */
            case REG:
                rtxPlusOp1 = XEXP(rtxInner, 1);
                switch (GET_CODE(rtxPlusOp1))
                {
                case SUBREG:
                    fS = register_operand(rtxPlusOp1, mode);
                    break;
                case REG:
                    fS = TRUE;
                    break;
                default:
                    break;
                }
                break;
            default:
                break;
            }
        }
        break;
    default:
        break;
    }
    return(fS);
}
/************************************************************************/
/* pic30_T_constraint(): dsPIC30 constraint T: direct address (far)    */
/************************************************************************/
int
pic30_T_constraint(op)
     rtx op;
{
    int fT = FALSE;

    if (GET_CODE(op) == MEM)
    {
        rtx inner = XEXP(op, 0);
        fT = pic30_data_space_operand_p(inner);
    }
    return(fT);
}
/************************************************************************/
/* pic30_U_constraint(): dsPIC30 constraint U: direct address (near)    */
/************************************************************************/
int
pic30_U_constraint(op)
     rtx op;
{
    int bU = FALSE;
    enum rtx_code code = GET_CODE(op);

    if (code == MEM)
    {
        rtx inner = XEXP(op, 0);
        bU = pic30_neardata_space_operand_p(inner);
    }
    else if (code == SUBREG)
    {
        rtx inner = SUBREG_REG(op);
        bU = pic30_U_constraint(inner);
    }
    return(bU);
}
/*
** A C compound statement to output to stdio stream STREAM the
** assembler syntax for an instruction operand X.  X is an RTL
** expression.

** CODE is a value that can be used to specify one of several ways
** of printing the operand.  It is used when identical operands
** must be printed differently depending on the context.  CODE
** comes from the `%' specification that was used to request
** printing of the operand.  If the specification was just `%DIGIT'
** then CODE is 0; if the specification was `%LTR DIGIT' then CODE
** is the ASCII code for LTR.

** If X is a register, this macro should print the register's name.
** The names can be found in an array `reg_names' whose type is
** `char *[]'.  `reg_names' is initialized from `REGISTER_NAMES'.

** When the machine description has a specification `%PUNCT' (a `%'
** followed by a punctuation character), this macro is called with
** a null pointer for X and the punctuation character for CODE.

** The dsPIC30 specific codes are:
** 'J' for the negative of a constant
** 'j' for 65536-the constant
** 'K' for the constant-16
** 'k' for 16-the constant
** 'L' for the constant-1
** 'I' for forcing post-increment on [R]
** 'D' for forcing post-decrement on [R]
** 'P' for forcing pre-increment on [R]
** 'p' for forcing pre-decrement on [R]
** 'm' for the memory-mapped name of a register
** 'd' for the second register in a pair
** 't' for the third register in a triple 
** 'q' for the fourth register in a quadruple 
** 'b' for the bit number (for bit <set,clr,tog,test,test+set>)
** 'B' for the bit number of the 1's complement (for bit clear)
** 'w' for int >> 48
** 'x' for int >> 32
** 'y' for int >> 16
** 'z' for int >> 0
** 'Q' for forcing extra displacement: [Wn+d] => [Wn+d+2]
** 'R' for forcing extra displacement: [Wn+d] => [Wn+d+4]
** 'S' for forcing extra displacement: [Wn+d] => [Wn+d+6]
** 's' strip any pre/post modify:      [++Wn] => [Wn]
** 'r' strip and memory deref:         [Wn] => Wn, [++Wn] => Wn
** 
*/
void
pic30_print_operand(file, x, letter)
     FILE *file;
     rtx x;
     int letter;
{
    rtx inner;
    int nDelta = 0;

    if (x == NULL_RTX)
    {
        fprintf(file, "%c", letter);
        return;
    }
    inner = XEXP(x, 0);
    switch (GET_CODE (x))
    {
    case REG:
        switch (letter)
        {
        case 'd':
            fprintf(file, "%s", reg_names[REGNO(x) + 1]);
            break;
        case 't':
            fprintf(file, "%s", reg_names[REGNO(x) + 2]);
            break;
        case 'q':
            fprintf(file, "%s", reg_names[REGNO(x) + 3]);
            break;
        case 'm':
            fprintf(file, "WREG%d", REGNO(x));
            break;
        default:
            fprintf(file, "%s", reg_names[REGNO(x)]);
            break;
        }
        break;

    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
        output_addr_const(file, x);
        break;

    case MEM:
        if (pic30_Q_constraint(x)) {
            int nScale, nDisplacement;

           nScale = (GET_MODE(x) == QImode) ? 1 : 2;
           nDisplacement = pic30_Q_displacement(x);
            if ((nScale == 2) && (nDisplacement & 1)) {
                error("Invalid displacement for operand type\n");
            }
        }
        switch (letter)
        {
        case 'I': /* post-increment */
        case 'D': /* post-decrement */
            switch (GET_CODE(inner))
            {
            case REG:
                fprintf(file,
                    (letter=='I') ? "[%s++]" : "[%s--]",
                        reg_names[REGNO(inner)]);
                break;
            default:
                output_address(inner);
                fprintf(file, "[bad code=%d]", GET_CODE(inner));
                break;
            }
            break;
        case 'P': /* pre-increment */
        case 'p': /* pre-decrement */
            switch (GET_CODE(inner))
            {
            case REG:
                fprintf(file,
                    (letter=='P') ? "[++%s]" : "[--%s]",
                        reg_names[REGNO(inner)]);
                break;
            default:
                output_address(inner);
                fprintf(file, "[bad code=%d]", GET_CODE(inner));
                break;
            }
            break;
        case 'S':
            nDelta += 2;
        case 'R':
            nDelta += 2;
        case 'Q':
            nDelta += 2;
        {
            rtx x = XEXP(inner, 0);
            rtx y = XEXP(inner, 1);
            if ((GET_CODE(inner) == REG))
            {
                fprintf(file, "[%s%+d]",
                        reg_names[REGNO(inner)],
                        nDelta);
            }
            else if (GET_CODE(inner) == CONST)
            {
                output_addr_const(file, x);
                fprintf(file, "%+d", nDelta);
            }
            else if (GET_CODE(inner) == CONST_INT)
            {
                fprintf(file, "%d%+d", INTVAL(inner), nDelta);
            }
            else if ((GET_CODE(inner) == SYMBOL_REF))
            {
                output_addr_const(file, inner);
                fprintf(file, "%+d", nDelta);
            }
            else if((GET_CODE(inner) == PLUS) &&
                (GET_CODE(x) == SYMBOL_REF) &&
                (GET_CODE(y) == CONST_INT))
            {
                output_addr_const(file, x);
                fprintf(file, "%+d%+d", INTVAL(y), nDelta);
            }
            else if((GET_CODE(inner) == PLUS) &&
                (GET_CODE(y) == SYMBOL_REF) &&
                (GET_CODE(x) == CONST_INT))
            {
                output_addr_const(file, y);
                fprintf(file, "%+d%+d", INTVAL(x), nDelta);
            }
            else if((GET_CODE(inner) == PLUS) &&
                (GET_CODE(x) == REG) &&
                (GET_CODE(y) == CONST_INT))
            {
                fprintf(file, "[%s%+d]",
                        reg_names[REGNO(x)],
                        INTVAL(y) + nDelta);
            }
            else
            {
                output_address(inner);
                fprintf(file, "{bad code=%d}", GET_CODE(inner));
            }
        }
            break;
        case 'r':
            switch (GET_CODE(inner)) {
                case POST_INC:
                case PRE_INC:
                case POST_DEC:
                case PRE_DEC:
                    /* for some reason we have decided NOT to deref */
                    inner = XEXP(inner,0);
                    break;
                default: break;
            }
            pic30_print_operand(file, inner, 'r');
            break;
        case 's':
            switch (GET_CODE(inner)) {
                case POST_INC:
                case PRE_INC:
                case POST_DEC:
                case PRE_DEC:
                    /* for some reason we have decided NOT to increment */
                    /* possibly because it is not supported on the current variant */
                    /* One can only hope that we execute the incrmement seperately */
                    inner = XEXP(inner,0);
                    break;
                default: break;
            }
            output_address(inner);
            break;

        default:
            output_address(inner);
            break;
        }
        break;

    case CONST_DOUBLE:
    {
        REAL_VALUE_TYPE r;
        long l[4] = { 0 };

        REAL_VALUE_FROM_CONST_DOUBLE(r, x);
                switch (GET_MODE(x))
                {
                case VOIDmode:
            /*
            ** Integer
            */
                        l[0] = CONST_DOUBLE_LOW(x);
                        l[1] = CONST_DOUBLE_HIGH(x);
                        break;
                case SFmode:
                        REAL_VALUE_TO_TARGET_SINGLE(r, l[1]);
                        break;
                default:
                        REAL_VALUE_TO_TARGET_LONG_DOUBLE(r, l);
                        break;
                }
        switch (letter)
        {
        case 'w':
            fprintf(file, "%ld", (l[1]>>16) & 0xFFFF);
            break;
        case 'x':
            fprintf(file, "%ld", (l[1]) & 0xFFFF);
            break;
        case 'y':
            fprintf(file, "%ld", (l[0]>>16) & 0xFFFF);
            break;
        case 'z':
            fprintf(file, "%ld", (l[0]) & 0xFFFF);
            break;
        default:
            fprintf(file, "%f", pic30_get_double(x));
            break;
        }
    }
        break;
    case CONST_INT:
        switch (letter)
        {
        case 'b':
            fprintf(file, "%d", pic30_which_bit(INTVAL(x)));
            break;
        case 'B':
            fprintf(file, "%d", pic30_which_bit(~INTVAL(x)));
            break;
        case 'j':
            fprintf(file, "%d", 65536-INTVAL(x));
            break;
        case 'J':
            fprintf(file, "%d", -INTVAL(x));
            break;
        case 'k':
            fprintf(file, "%d", 16-INTVAL(x));
            break;
        case 'K':
            fprintf(file, "%d", INTVAL(x)-16);
            break;
        case 'L':
            fprintf(file, "%d", INTVAL(x)-1);
            break;
        case 'w':
            fprintf(file, "%d", INTVAL(x) < 0 ? -1 : 0);
            break;
        case 'x':
            fprintf(file, "%d", INTVAL(x) < 0 ? -1 : 0);
            break;
        case 'y':
            fprintf(file, "%d", INTVAL(x) >> 16);
            break;
        case 'z':
            fprintf(file, "%d", INTVAL(x) & 0xFFFF);
            break;
        default:
            fprintf(file, "%d", INTVAL(x));
            break;
        }
        break;

    case CODE_LABEL:
        asm_fprintf (file, "%L%d", CODE_LABEL_NUMBER(x));
        break;

    case PLUS:
    {
        rtx op0 = XEXP (x, 0), op1 = XEXP (x, 1);
        int op0code = GET_CODE (op0), op1code = GET_CODE (op1);
        if (op1code == CONST_INT)
        {
            switch (op0code)
            {
            case SYMBOL_REF:
                fprintf(file, "%d+%s",
                        INTVAL (op1), XSTR (op0, 0));
                break;
            default:
                fprintf(file, "p_o_PLUS UFO, "
                        "code=%d, with CONST=%d",
                        (int) op0code, INTVAL (op1));
                break;
            }
        }
        else
        {
            fprintf(file, "p_o_+: op0code=%d, op1code=%d",
                            op0code, op1code);
        }
    }
        break;

    default:
        fprintf(file, "p_o_UFO code=%d (PLUS=%d)", GET_CODE(x), PLUS);
        break;
    }
}

/*
** A C compound statement to output to stdio stream stream the assembler syntax
** for an instruction operand that is a memory reference whose address is x.
** x is an RTL expression.
*/
void
pic30_print_operand_address(file, addr)
     FILE *file;
     rtx addr;
{
    switch (GET_CODE (addr))
    {
    case REG:
        fprintf(file, "[%s]", reg_names[REGNO(addr)]);
        break;
    case PLUS:
    {
        rtx x = XEXP(addr, 0), y = XEXP(addr, 1);
        switch (GET_CODE (x))
        {
        case REG:
            switch (GET_CODE (y))
            {
            case REG:
                fprintf(file, "[%s+%s]",reg_names[REGNO (x)],
                            reg_names[REGNO (y)]);
                break;
            case CONST:
                output_address(XEXP(y, 0));
                fprintf(file, ",r%d ;P_O_A reg + const expr",
                            REGNO (x));
                break;
            case CONST_INT:
                {
                fprintf(file, "[%s%+d]",
                        reg_names[REGNO (x)],
                        INTVAL(y));
                }
                break;
            case SYMBOL_REF:
                fprintf(file, "%s", XSTR (y, 0));
                fprintf(file, ",r%d  ; P_O_A reg + sym",
                            REGNO (x));
                break;
            case LABEL_REF:
                output_address(XEXP (y, 0));
                fprintf(file, ",r%d  ; P_O_A reg + label",
                            REGNO (x));
                break;
            default:
                fprintf(file, "[P_O_A reg%d+UFO code=%d]",
                        REGNO (x), GET_CODE (y));
            }
            break;
        case LABEL_REF:
            output_address (XEXP (x, 0));
            break;
        case SYMBOL_REF:
            switch (GET_CODE (y))
            {
            case CONST_INT:
                output_addr_const(file, x);
                fprintf(file, "%+d", INTVAL(y));
                break;
            case REG:
                fprintf (file, "%s,r%d ;P_O_A sym + reg",
                    XSTR (x, 0), REGNO (y));
                break;
            default:
                fprintf(file, "P_O_A sym/lab+UFO[sym=%s,"
                        "code(y)=%d]",
                        XSTR(x, 0), GET_CODE (y));
                break;
            }
            break;
        case CONST_INT:
            switch (GET_CODE(y))
            {
            case SYMBOL_REF:
                output_addr_const(file, y);
                fprintf(file, "%+d", INTVAL(x));
                break;
            default:
                fprintf(file, "P_O_A int+UFO[int=%d,"
                        "code(y)=%d]",
                        INTVAL(x), GET_CODE(y));
                break;
            }
            break;
        default:
            fprintf(file, "P_O_A plus op1_UFO[code1=%d,code2=%d]",
                        GET_CODE (x), GET_CODE (y));
            break;
        }
    }
        break;
    case LABEL_REF:
        output_addr_const(file, addr);
        break;
    case SYMBOL_REF:
        output_addr_const(file, addr);
        break;
    case CONST:
        output_address(XEXP(addr, 0));
        break;
    case CODE_LABEL:
        asm_fprintf(file, "%L%d", CODE_LABEL_NUMBER(addr));
        break;
    case PRE_INC:
        fprintf(file, "[++%s]", reg_names[REGNO(XEXP(addr, 0))]);
        break;
    case PRE_DEC:
        fprintf(file, "[--%s]", reg_names[REGNO(XEXP(addr, 0))]);
        break;
    case POST_DEC:
        fprintf(file, "[%s--]", reg_names[REGNO(XEXP(addr, 0))]);
        break;
    case POST_INC:
        fprintf(file, "[%s++]", reg_names[REGNO(XEXP(addr, 0))]);
        break;
    case CONST_INT:
        fprintf(file, "0x%x", (INTVAL(addr) & ((1 << POINTER_SIZE)-1)));
        break;
    default:
        fprintf(file, " p_o_a UFO, code=%d val=0x%x",
                (int) GET_CODE (addr), INTVAL (addr));
        break;
    }
}
/*
** Return non zero if the given value has just one bit set,
** otherwise return zero. Note this function may be used to detect one
** bit clear by inverting the param.
*/
int
pic30_one_bit_set_p (x)
     int x;
{
    /*
    ** (x-1) is (x) with the LSB changed.
    ** If their intersection is zero, then there is a single bit set.
    */
    return(x && (x & (x - 1)) == 0);
}
/*
** Return the number of the least significant bit set.
*/
int
pic30_which_bit(x)
     int x;
{
    int b = 0;

    while (x && ((x & 1) == 0))
    {
        b++;
        x >>= 1;
    }
    return(b);
}
int
pic30_get_function_return(void)
{
    return(lbFunctionHasReturn);
}
void
pic30_set_function_return(int bState)
{
    lbFunctionHasReturn = bState;
}
/*
** Determine if we must save a register.
*/
#define    CALLEE_SAVED(r)    (call_used_regs[(r)]==0)
static int
pic30_mustsave(int r, int fLeaf, int fInterrupt)
{
    int fMustSave;

    if (fInterrupt)
    {
        /*
        ** Interrupt function.
        */
        if (fLeaf)
        {
            /*
            ** Leaf interrupt function:
            ** save any register that is used.
            */
            fMustSave = regs_ever_live[r];
        }
        else
        {
            /*
            ** Non-Leaf interrupt function:
            ** save any register that is used,
            ** or any register that is not callee-saved
            ** (other than SP, which need not be saved).
            */
            fMustSave = regs_ever_live[r] ||
                    (!CALLEE_SAVED(r) && (r != SP_REGNO));
        }
    }
    else
    {
        /*
        ** Non-interrupt function:
        ** Save callee-saved registers that we use.
        */
        fMustSave = regs_ever_live[r] && CALLEE_SAVED(r);
    }
    return(fMustSave);
}
/*
** See if we need to set up the frame pointer.
** The frame pointer is required for any of the following reasons:
** 1) frame_pointer_needed is non-zero.
** 2) The function uses local space. Using lnk/unlk is more efficient
**    than manipulating the stack "by hand".
** 3) Arguments are on the stack. The FIRST_PARM_OFFSET macro assumes
**    that the old frame pointer is in the stack.
**    The FIRST_PARM_OFFSET macro is called with a function declaration,
**    so it could determine if any arguments are on the stack. However,
**    the function declaration alone is insufficient to determine if
**    we can eliminate the frame pointer. In particular, we don't know
**    if the function needs any local variable space.
*/
static int
pic30_frame_pointer_needed_p(int size)
{
    int fNeeded;

    fNeeded = (frame_pointer_needed || size);
    if (!fNeeded)
    {
        tree param;

          for (    param = DECL_ARGUMENTS(current_function_decl);
            param;
                  param = TREE_CHAIN(param))
        {
            rtx rtl = DECL_INCOMING_RTL(param);
            if (GET_CODE (rtl) != REG)
            {
                fNeeded = TRUE;
                break;
            }
        }
    }
    return(fNeeded);
}

int
pic30_asm_function_p(int bDiscover)
{
  rtx insn;
  rtx body;
  enum rtx_code code;
  static int bAsmFunction;

  if (bDiscover) {
    bAsmFunction = pic30_interrupt_function_p(current_function_decl) &&
                   pic30_interrupt_preprologue();
    for (insn = get_insns(); insn; insn = NEXT_INSN(insn)) {
      code = GET_CODE(insn);
      if (code == INSN) {
        body = PATTERN(insn);
        code = GET_CODE(body);
        if (code == ASM_INPUT) {
          bAsmFunction = TRUE;
          break;
        }
      }
    }
  }
  return bAsmFunction;
}

/*
** Calculate the offset between two frame registers.
*/
void
pic30_initial_elimination_offset(from, to, poffset)
    int from, to, *poffset;
{
    int fLeaf;
    int fInterrupt;
    int nLastReg;
    int regno;
    int nOffset;

    fLeaf = current_function_is_leaf;
    fInterrupt = pic30_interrupt_function_p(current_function_decl);
    nLastReg = (frame_pointer_needed) ? FP_REGNO : SP_REGNO;
    if ((from == FRAME_POINTER_REGNUM) && (to == STACK_POINTER_REGNUM))    
    {
        /*
        ** The offset between the stack pointer and the frame pointer is
        ** the frame size, plus (for non-interrupt functions) the size
        ** of the register save area. (Interrupt functions are excluded
        ** because they save the registers before establishing the
        ** frame. Why? Because interrupt functions never take
        ** parameters, and saving the registers above the frame gives
        ** greater addressibility to local variables.)
        */
        nOffset = 0;
        for (regno = 0; !fInterrupt && (regno < nLastReg); regno++)
        {
            if (pic30_mustsave(regno, fLeaf, fInterrupt))
            {
                nOffset += 2;
            }
        }
        *poffset = -(get_frame_size() + nOffset);
    }
    else if ((from)==ARG_POINTER_REGNUM && (to)==FRAME_POINTER_REGNUM)
    {
        *poffset = 0;
    }
    else
    {
        abort();
    }
}
/*
** Return the interrupt save variable list for the current function.
*/
static tree
pic30_get_save_variable_list()
{
    tree v;
    tree fcn;
    tree fcnargs = NULL_TREE;

    tree a = lookup_attribute(IDENTIFIER_POINTER(pic30_identInterrupt[0]),
                DECL_ATTRIBUTES(current_function_decl));
    if (a)
    {
        a = TREE_VALUE(a);
    }
    while (a)
    {
        v = TREE_VALUE(a);

        if ((v != NULL_TREE) && (TREE_CODE(v) == CALL_EXPR))
        {
            fcn = TREE_OPERAND(TREE_OPERAND(v,0),0);
            if (IDENT_SAVE(DECL_NAME(fcn)))
            {
                fcnargs = TREE_OPERAND(v,1);
                break;
            }
        }
        a = TREE_CHAIN(a);
    }
    return(fcnargs);
}
/*
** Return the interrupt save variable decl for the current list element.
*/
static tree
pic30_get_save_variable_decl(tree vl, int *pnwords, int *pnoffset)
{
    int nwords = 0;
    int noffset = 0;

    tree decl = TREE_VALUE(vl);
    while (    (TREE_CODE(decl) == NOP_EXPR) ||
        (TREE_CODE(decl) == ADDR_EXPR))
    {
        decl = TREE_OPERAND(decl, 0);
    }
    if (TREE_CODE(decl) == VAR_DECL)
    {
        nwords = TREE_INT_CST_LOW(DECL_SIZE(decl)) / BITS_PER_WORD;
        if (pnwords)
        {
            *pnwords = nwords;
        }
        if (pnoffset)
        {
            *pnoffset = 0;
        }
    }
    else if (TREE_CODE(decl) == ARRAY_REF)
    {
        nwords = TREE_INT_CST_LOW(TYPE_SIZE(TREE_TYPE(decl)))
                            / BITS_PER_WORD;
        noffset = TREE_INT_CST_LOW(TYPE_SIZE(TREE_TYPE(decl)))
                * TREE_INT_CST_LOW(TREE_OPERAND(decl, 1))
                            / BITS_PER_UNIT;
        if (pnwords)
        {
            *pnwords = nwords;
        }
        if (pnoffset)
        {
            *pnoffset = noffset;
        }
        decl = TREE_VALUE(decl);
    }
    if (TREE_CODE(decl) != VAR_DECL)
        abort();
    return(decl);
}
/*
** Verify that interrupt functions take no parameters and return void.
*/
static void
pic30_validate_isp(void)
{
    tree fntype = TREE_TYPE(current_function_decl);
      tree ret_type = TREE_TYPE(fntype);
    tree param;

    if (ret_type != void_type_node)
    {
        error("interrupt functions must return void");
    }
      for (    param = TYPE_ARG_TYPES(fntype);
        param;
              param = TREE_CHAIN(param))
    {
        if (TREE_VALUE(param) != void_type_node)
        {
            error("interrupt functions must not take parameters");
            break;
        }
    }
}
/*
** LIFO stack element.
*/
typedef struct tagtreelifo
{
    struct tagtreelifo*    next;
    tree            decl;
}
    treelifo, *ptreelifo;
static ptreelifo pic30_savelifo;
/*
** Push a save variable decl onto a LIFO stack.
*/
static void
pic30_push_save_variable(tree decl)
{
    ptreelifo tlnew;

    tlnew = (treelifo *)xmalloc(sizeof(tlnew));
    tlnew->decl = decl;
    tlnew->next = pic30_savelifo;
    pic30_savelifo = tlnew;
}
/*
** Pop a save variable decl from a LIFO stack.
*/
static tree
pic30_pop_save_variable()
{
    tree decl = NULL_TREE;

    if (pic30_savelifo)
    {
        ptreelifo tlold = pic30_savelifo;
        decl = tlold->decl;
        pic30_savelifo = tlold->next;
        free(tlold);
    }
    return(decl);
}
/*
** Find a register to use as a save variable scratch register.
*/
static int
pic30_get_save_variable_scratch(int fShadow, int fLeaf, int *pfScratch)
{
    int regno = 0;
    tree vl;
    int nLastReg = (frame_pointer_needed) ? FP_REGNO : SP_REGNO;
    
    *pfScratch = FALSE;
    for (vl = pic30_get_save_variable_list(); vl; vl = TREE_CHAIN(vl))
    {
        tree decl = pic30_get_save_variable_decl(vl, NULL, NULL);
        if (!pic30_near_decl_p(decl))
        {
            /*
            ** We need a scratch reg.
            */
            if (fShadow)
            {
                regno = 0;
            }
            else
            {
                for (regno = 0; regno < nLastReg; ++regno)
                {
                    if (pic30_mustsave(regno, fLeaf, TRUE))
                    {
                        break;
                    }
                }
                if (regno >= nLastReg)
                {
                    regno = 0;
                    *pfScratch = TRUE;
                }
            }
            break;
        }
    }
    return(regno);
}

static int pic30_scan_reg_sets(unsigned int regno) {
  rtx insn;
  rtx pattern;

  insn = ENTRY_BLOCK_PTR->next_bb->head;
  for (; insn; insn = NEXT_INSN(insn)) {
    if (INSN_P(insn)) {
      pattern = PATTERN(insn);
      if ((GET_CODE(pattern) == SET) && (GET_CODE(SET_DEST(pattern)) == REG)) {
        if (REGNO(SET_DEST(pattern)) == regno) return 1;
        if ((REGNO(SET_DEST(pattern)) < regno) &&
            (REGNO(SET_DEST(pattern)) + 
             HARD_REGNO_NREGS(0,GET_MODE(SET_DEST(pattern))) > regno)) 
           return 1;
      }
    }
  }
  return 0;
}

/*
** Set up the callee stack frame.
*/
static void
pic30_expand_prologue_frame(int size)
{
    /*
    ** Set up the frame pointer
    */
    if (pic30_frame_pointer_needed_p(size))
    {
        int n;
        rtx insn;

        n = (size > PIC30_LNK_MAX) ? PIC30_LNK_MAX : size;
        /* can't always use the lnk instruction because the FP might
           have been assigned to another register */
        if (!pic30_scan_reg_sets(FP_REGNO)) {
          insn = emit_insn(gen_lnk(GEN_INT(n)));
#if MAKE_FRAME_RELATED
          RTX_FRAME_RELATED_P(insn) = 1;
#endif
        } else {
          /* all our calculations are based upon a lnk instruction */
          n = 0;
          size += 2;
        }
        for (size -= n; size > 0; size -= n)
        {
            rtx sp = gen_rtx_REG(HImode, SP_REGNO);
            n = (size > (PIC30_ADD_MAX & ~1)) ? PIC30_ADD_MAX & ~1
                              : size;
            insn = emit_insn(gen_addhi3(sp, sp, GEN_INT(n)));
#if MAKE_FRAME_RELATED
            RTX_FRAME_RELATED_P(insn) = 1;
#endif
        }
    }
}
/*
** Destroy the callee stack frame.
*/
static void
pic30_expand_epilogue_frame(int size)
{
    /*
    ** Restore caller's frame
    */
    if (pic30_frame_pointer_needed_p(size))
    {
        rtx insn;
        int n;

        if (!pic30_scan_reg_sets(FP_REGNO)) {
          insn = emit_insn(gen_ulnk());
#if MAKE_FRAME_RELATED
          RTX_FRAME_RELATED_P(insn) = 1;
#endif
          n = size;
        } else {
          n = 0;
          size += 2;
        }
        for (size -= n; size > 0; size -= n) {
          rtx sp = gen_rtx_REG(HImode, SP_REGNO);
          n = (size > (PIC30_ADD_MAX & ~1)) ? PIC30_ADD_MAX & ~1 : size;
          insn = emit_insn(gen_subhi3(sp, sp, GEN_INT(n)));
#if MAKE_FRAME_RELATED
          RTX_FRAME_RELATED_P(insn) = 1;
#endif
        }
    }
}

/*
** Generate an RTL insn to push a register on the stack
*/
static rtx
pic30_expand_pushhi(int regno)
{
    rtx insn;
    rtx sp;
    rtx reg;

    sp = stack_pointer_rtx;
    sp = gen_rtx_POST_INC(HImode, sp);
    sp = gen_rtx_MEM(HImode, sp);
    reg = gen_rtx_REG(HImode, regno);
    insn = emit_insn(gen_pushhi(sp, reg));
#if MAKE_FRAME_RELATED
    RTX_FRAME_RELATED_P(insn) = 1;
#endif

    return(insn);
}
/*
** Generate an RTL insn to pop an HI or SI register from the stack
*/
static rtx
pic30_expand_pop(enum machine_mode mode, int regno)
{
    rtx insn;
    rtx sp;
    rtx reg;

    sp = stack_pointer_rtx;
    sp = gen_rtx_PRE_DEC(mode, sp);
    sp = gen_rtx_MEM(mode, sp);
    reg = gen_rtx_REG(mode, regno);
    if (mode == HImode)
    {
        insn = emit_insn(gen_pophi(reg, sp));
    }
    else
    {
        insn = emit_insn(gen_popsi(reg, sp));
    }
#if MAKE_FRAME_RELATED
    RTX_FRAME_RELATED_P(insn) = 1;
#endif
    emit_insn(gen_rtx_USE(VOIDmode, reg));

    return(insn);
}
/*
** The prologue pattern emits RTL for entry to a function.
** The function entry is responsible for setting up the stack frame,
** initializing the frame pointer register, saving callee saved registers, etc.
**
** Using a prologue pattern is generally preferred over defining
** TARGET_ASM_FUNCTION_PROLOGUE to emit assembly code for the prologue. 
**
** The prologue pattern is particularly useful for targets that perform
** instruction scheduling. 
*/
void
pic30_expand_prologue(void)
{
  rtx insn;
  int regno = 0;
  int size = get_frame_size();
  int fLeaf = current_function_is_leaf;
  int fInterrupt = pic30_interrupt_function_p(current_function_decl);
  int fShadow = pic30_shadow_function_p(current_function_decl);
  int nLastReg = (frame_pointer_needed) ? FP_REGNO : SP_REGNO;
  int noreturn = pic30_noreturn_function(current_function_decl);
  static char ACCAL[] = "ACCAL";
  static char ACCAH[] = "ACCAH";
  static char ACCAU[] = "ACCAU";
  static char ACCBL[] = "ACCBL";
  static char ACCBH[] = "ACCBH";
  static char ACCBU[] = "ACCBU";

  if (!pic30_asm_function_p(FALSE))
  {
    /*
    ** Enable PA
    */
    insn = emit_insn(gen_pa(GEN_INT(1)));
  }
  /*
  ** Special handling for interrupt functions.
  */
  if (fInterrupt)
  {
    int nVectorID;
        
    /*
    ** Verify that interrupt functions take
    ** no parameters and return void
    */
    pic30_validate_isp();
    /*
    ** Emit the interrupt vector, if needed.
    */
    nVectorID = pic30_interrupt_vector_id(current_function_decl);
    if (nVectorID)
    {
      insn = emit_insn(gen_iv(GEN_INT(nVectorID)));
    }
    /*
    ** Emit any user-specified preprologue code
    */
    if (pic30_interrupt_preprologue())
    {
      insn = emit_insn(gen_pp());
      insn = emit_insn(gen_blockage());
    }
    /*
    ** For interrupt functions, save shadowed registers
    */
    if (fShadow)
    {
      insn = emit_insn(gen_pushshadow());
      insn = emit_insn(gen_blockage());
    }
    /*
    ** For interrupt functions, save the repeat count (if used)
    */
    if (!fLeaf || regs_ever_live[RCOUNT_REGNO])
    {
      rtx sp;
      rtx sfr;
      const char *pszRCOUNT;

      sp = stack_pointer_rtx;
      sp = gen_rtx_POST_INC(HImode, sp);
      sp = gen_rtx_MEM(HImode, sp);
      pszRCOUNT = reg_names[RCOUNT_REGNO];
      if (pszRCOUNT[0] == '_')
      {
         ++pszRCOUNT;
      }
      sfr = gen_rtx_SYMBOL_REF(HImode, pszRCOUNT);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pushhi(sp, sfr));
#if MAKE_FRAME_RELATED
      RTX_FRAME_RELATED_P(insn) = 1;
#endif
    }
  }
  /*
  ** For non-interrupt functions, establish the callee frame
  ** before saving scratch registers.
  */
  if (!fInterrupt)
  {
    pic30_expand_prologue_frame(size);
  }
  /*
  ** Push scratch resources used in interrupt functions.
  ** Push callee-saved registers.
  **
  ** The array call_used_regs[] contains a 1 in element
  ** N if register N is clobbered by a function call
  ** (i.e., if register N is caller-saved).
  ** If element N is zero, then register N is callee-saved.
  **
  ** For functions using the shadow attribute, save registers
  ** other than [W0-W3] as these four registers are preserved
  ** using push.s.
  */
  if (!noreturn) {
    for (regno = (fShadow ? WR4_REGNO : 0); regno < nLastReg; regno++)
    {
      if (pic30_mustsave(regno, fLeaf, fInterrupt))
      {
        rtx sp;
        rtx r;
  
        if (IS_EVEN_REG(regno) && ((regno+1) < nLastReg) &&
            pic30_mustsave(regno+1, fLeaf, fInterrupt))
        {
          sp = stack_pointer_rtx;
          sp = gen_rtx_POST_INC(SImode, sp);
          sp = gen_rtx_MEM(SImode, sp);
          r = gen_rtx_REG(SImode, regno);
          insn = emit_insn(gen_pushsi(sp, r));
  #if MAKE_FRAME_RELATED
          RTX_FRAME_RELATED_P(insn) = 1;
  #endif
          lCalleeSaveRegs[regno] = SAVE_DWORD;
          lCalleeSaveRegs[regno+1] = SAVE_DWORD;
          regno++;
        }
        else
        {
          insn = pic30_expand_pushhi(regno);
          lCalleeSaveRegs[regno] = SAVE_SWORD;
        }
      }
    }
    /*
    ** save A and B if required
    */
    if (pic30_mustsave(A_REGNO, fLeaf, fInterrupt)) {
      rtx sp;
      rtx sfr;
  
      sp = stack_pointer_rtx;
      sp = gen_rtx_POST_INC(HImode, sp);
      sp = gen_rtx_MEM(HImode, sp);
      sfr = gen_rtx_SYMBOL_REF(HImode, ACCAL);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pushhi(sp, sfr));
      sfr = gen_rtx_SYMBOL_REF(HImode, ACCAH);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pushhi(sp, sfr));
      sfr = gen_rtx_SYMBOL_REF(HImode, ACCAU);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pushhi(sp, sfr));
    }
    if (pic30_mustsave(B_REGNO, fLeaf, fInterrupt)) {
      rtx sp;
      rtx sfr;
  
      sp = stack_pointer_rtx;
      sp = gen_rtx_POST_INC(HImode, sp);
      sp = gen_rtx_MEM(HImode, sp);
      sfr = gen_rtx_SYMBOL_REF(HImode, ACCBL);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pushhi(sp, sfr));
      sfr = gen_rtx_SYMBOL_REF(HImode, ACCBH);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pushhi(sp, sfr));
      sfr = gen_rtx_SYMBOL_REF(HImode, ACCBU);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pushhi(sp, sfr));
    }
  }
  /*
  ** For interrupt functions, push any save(...) variables.
  */
  if (fInterrupt)
  {
    rtx sp;
    tree vl;
    tree decl;
    int nwords, n, noffset;
    const char *pszv;
    int fScratch;

    sp = stack_pointer_rtx;
    sp = gen_rtx_POST_INC(HImode, sp);
    sp = gen_rtx_MEM(HImode, sp);

    /*
    ** See if we need a scratch register for saving the vars.
    */
    regno = pic30_get_save_variable_scratch(fShadow, fLeaf, &fScratch);
    if (fScratch)
    {
      insn = pic30_expand_pushhi(regno);
    }
    /*
    ** Ready to save the (external) world ...
    */
    for (vl = pic30_get_save_variable_list(); vl; vl = TREE_CHAIN(vl))
    {
      rtx var;
      rtx src;
      rtx disp;

      pic30_push_save_variable(vl);
      decl = pic30_get_save_variable_decl(vl, &nwords, &noffset);
      pszv = IDENTIFIER_POINTER(DECL_NAME(decl));
      var = gen_rtx_SYMBOL_REF(HImode, pszv);
      if (pic30_near_decl_p(decl))
      {
        for (n = 0; n < nwords; ++n)
        {
          disp = GEN_INT(noffset+n*2);
          src = gen_rtx_PLUS(HImode, var, disp);
          src = gen_rtx_CONST(HImode, src);
          src = gen_rtx_MEM(HImode, src);
          insn = emit_insn(gen_pushhi(sp, src));
#if MAKE_FRAME_RELATED
           RTX_FRAME_RELATED_P(insn) = 1;
#endif
        }
      }
      else
      {
        rtx reg;

        disp = GEN_INT(noffset);
        src = gen_rtx_PLUS(HImode, var, disp);
        src = gen_rtx_CONST(HImode, src);
        reg = gen_rtx_REG(HImode, regno);
        insn = emit_insn(gen_movhi_address(reg, src));
#if MAKE_FRAME_RELATED
        RTX_FRAME_RELATED_P(insn) = 1;
#endif

        src = gen_rtx_POST_INC(HImode, reg);
        src = gen_rtx_MEM(HImode, src);
        for (n = 0; n < nwords; ++n)
        {
          insn = emit_insn(gen_pushhi(sp,src));
#if MAKE_FRAME_RELATED
          RTX_FRAME_RELATED_P(insn) = 1;
#endif
        }
      }
    }
    pic30_expand_prologue_frame(size);
  }
}

/*
** This epilogue pattern emits RTL for exit from a function.
** The function exit is responsible for deallocating the stack frame,
** restoring callee saved registers and emitting the return instruction. 
**
** Using an epilogue pattern is generally preferred over defining
** TARGET_ASM_FUNCTION_EPILOGUE to emit assembly code for the epilogue. 
**
** The epilogue pattern is particularly useful for targets that perform
** instruction scheduling or which have delay slots for their return
** instruction.
*/
void
pic30_expand_epilogue(void)
{
  rtx insn;
  int regno = 0;
  int size = get_frame_size();
  int fLeaf = current_function_is_leaf;
  int fInterrupt = pic30_interrupt_function_p(current_function_decl);
  int noreturn = pic30_noreturn_function(current_function_decl);
  int fShadow = pic30_shadow_function_p(current_function_decl);
  int nLastReg = (frame_pointer_needed) ? FP_REGNO : SP_REGNO;
  static char ACCAL[] = "ACCAL";
  static char ACCAH[] = "ACCAH";
  static char ACCAU[] = "ACCAU";
  static char ACCBL[] = "ACCBL"; 
  static char ACCBH[] = "ACCBH";
  static char ACCBU[] = "ACCBU";

  /*
  ** For interrupt functions, destroy the callee frame
  ** before recovering scratch registers.
  */
  if (fInterrupt)
  {
    tree vl;
    tree decl;
    rtx var;
    rtx dst;
    rtx disp;
    rtx sp;
    int nwords, n, noffset;
    const char *pszv;
    int fScratch;

    sp = stack_pointer_rtx;
    sp = gen_rtx_PRE_DEC(HImode, sp);
    sp = gen_rtx_MEM(HImode, sp);

    pic30_expand_epilogue_frame(size);
    /*
    ** Pop any save(...) variables.
    */
    /*
    ** See if we need a scratch register for saving the vars.
    */
    regno = pic30_get_save_variable_scratch(fShadow, fLeaf, &fScratch);
    /*
    ** Ready to recover the (external) world ...
    */
    while ((vl = pic30_pop_save_variable()) != NULL_TREE)
    {
      decl = pic30_get_save_variable_decl(vl, &nwords, &noffset);
      pszv = IDENTIFIER_POINTER(DECL_NAME(decl));
      var = gen_rtx_SYMBOL_REF(HImode, pszv);
      if (pic30_near_decl_p(decl))
      {
        for (n = nwords-1; n >= 0; --n)
        {
          disp = GEN_INT(noffset+n*2);
          dst = gen_rtx_PLUS(HImode, var, disp);
          dst = gen_rtx_CONST(HImode, dst);
          dst = gen_rtx_MEM(HImode, dst);
          insn = emit_insn(gen_pophi_unspec(dst));
#if MAKE_FRAME_RELATED
          RTX_FRAME_RELATED_P(insn) = 1;
#endif
        }
      }
      else
      {
        rtx reg;

        disp = GEN_INT((nwords-1)*2+noffset);
        dst = gen_rtx_PLUS(HImode, var, disp);
        dst = gen_rtx_CONST(HImode, dst);
        reg = gen_rtx_REG(HImode, regno);
        insn = emit_insn(gen_movhi_address(reg, dst));
#if MAKE_FRAME_RELATED
        RTX_FRAME_RELATED_P(insn) = 1;
#endif
        dst = gen_rtx_POST_DEC(HImode, reg);
        dst = gen_rtx_MEM(HImode, dst);
        for (n = 0; n < nwords; ++n)
        {
          insn = emit_insn(gen_pophi_unspec(dst));
#if MAKE_FRAME_RELATED
          RTX_FRAME_RELATED_P(insn) = 1;
#endif
        }
      }
    }
    if (fScratch)
    {
      insn = pic30_expand_pop(HImode, regno);
    } 
    if (pic30_errata_mask & retfie_errata) {
      rtx w0 = gen_rtx_REG(HImode, WR0_REGNO);
      rtx addr;

      insn = pic30_expand_pushhi(WR0_REGNO);
      disp = GEN_INT(0x00e0);
      insn = emit_insn(gen_movhi(w0, disp));
      addr = gen_rtx_MEM(HImode, 
                         gen_rtx_SYMBOL_REF(HImode, PIC30_SFR_FLAG "SR"));
      SYMBOL_REF_FLAG(XEXP(addr,0)) = 1;  /* mark it as near */
      insn = emit_insn(gen_iorhi3_sfr0( addr, w0));
      insn = pic30_expand_pop(HImode, WR0_REGNO);
    }
  }
  if (!noreturn) {
    /*
    ** recover B and A if required
    */
    if (pic30_mustsave(B_REGNO, fLeaf, fInterrupt)) {
      rtx sp;
      rtx sfr;
  
      sp = stack_pointer_rtx;
      sp = gen_rtx_PRE_DEC(HImode, sp);
      sp = gen_rtx_MEM(HImode, sp);
      sfr = gen_rtx_SYMBOL_REF(HImode, ACCBU);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pophi(sfr, sp));
      sfr = gen_rtx_SYMBOL_REF(HImode, ACCBH);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pophi(sfr, sp));
      sfr = gen_rtx_SYMBOL_REF(HImode, ACCBL);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pophi(sfr, sp));
    }
    if (pic30_mustsave(A_REGNO, fLeaf, fInterrupt)) {
      rtx sp;
      rtx sfr;
  
      sp = stack_pointer_rtx;
      sp = gen_rtx_PRE_DEC(HImode, sp);
      sp = gen_rtx_MEM(HImode, sp);
      sfr = gen_rtx_SYMBOL_REF(HImode, ACCAU);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pophi(sfr, sp));
      sfr = gen_rtx_SYMBOL_REF(HImode, ACCAH);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pophi(sfr, sp));
      sfr = gen_rtx_SYMBOL_REF(HImode, ACCAL);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pophi(sfr, sp));
    }
  
    /*
    ** Recover scratch resources
    */
    for (regno = nLastReg-1; regno >= (fShadow ? WR4_REGNO: 0); regno--)
    {
      if (pic30_mustsave(regno, fLeaf, fInterrupt))
      {
        switch (lCalleeSaveRegs[regno])
        {
          case SAVE_QWORD:
            continue;
            break;
          case SAVE_DWORD:
            if (IS_EVEN_REG(regno))
            {
              insn = pic30_expand_pop(SImode, regno);
            }
            else
            {
              continue;
            }
            break;
          case SAVE_SWORD:
            insn = pic30_expand_pop(HImode, regno);
            break;
        }
      }
    }
  }
  /*
  ** For non-interrupt functions, destroy the callee frame
  ** after recovering scratch registers.
  */
  if (!fInterrupt)
  {
      pic30_expand_epilogue_frame(size);
  }
  /*
  ** For interrupt functions, restore the repeat count
  */
  if (fInterrupt)
  {
    if (!fLeaf || regs_ever_live[RCOUNT_REGNO])
    {
      rtx sp;
      rtx sfr;
      const char *pszRCOUNT;

      sp = stack_pointer_rtx;
      sp = gen_rtx_PRE_DEC(HImode, sp);
      sp = gen_rtx_MEM(HImode, sp);
      pszRCOUNT = reg_names[RCOUNT_REGNO];
      if (pszRCOUNT[0] == '_')
      {
        ++pszRCOUNT;
      }
      sfr = gen_rtx_SYMBOL_REF(HImode, pszRCOUNT);
      sfr = gen_rtx_MEM(HImode, sfr);
      insn = emit_insn(gen_pophi_unspec(sfr));
#if MAKE_FRAME_RELATED
      RTX_FRAME_RELATED_P(insn) = 1;
#endif
    }
    /*
    ** For interrupt functions, recover shadowed registers
    */
    if (fShadow)
    {
      insn = emit_insn(gen_popshadow());
      insn = emit_insn(gen_blockage());
    }
  }
  if ((fInterrupt) && (pic30_errata_mask & retfie_errata_disi)) {
    emit_insn(gen_disi(GEN_INT(1)));
  }
  /*
  ** Exit
  */
  insn = emit_jump_insn(gen_return_from_epilogue());
#if MAKE_FRAME_RELATED
  RTX_FRAME_RELATED_P(insn) = 1;
#endif
  /*
  ** Disable PA
  */
  insn = emit_insn(gen_pa(GEN_INT(0)));
}

/*
** See if the current function requires epilogue code.
*/
int
pic30_null_epilogue_p()
{
    int regno;

    if (reload_completed &&
        !pic30_interrupt_function_p(current_function_decl) &&
        !current_function_calls_alloca &&
        !current_function_args_size &&
        !(optimize < 2) &&
        !get_frame_size())
    {
        for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--)
        {
            if (regs_ever_live[regno] && !call_used_regs[regno])
            {
                return(FALSE);
            }
        }
        return(TRUE);
    }
    return(FALSE);
}
/*
** Table of library function names
*/
static const char *lpszLibcallNames[] =
{
    "__addsf3",
    "__divsf3",
    "__mulsf3",
    "__subsf3",
    "__negsf2",
    "__gesf2",
    "__gtsf2",
    "__lesf2",
    "__ltsf2",
    "__eqsf2",
    "__nesf2",

    "__adddf3",
    "__divdf3",
    "__muldf3",
    "__subdf3",
    "__negdf2",
    "__gedf2",
    "__gtdf2",
    "__ledf2",
    "__ltdf2",
    "__eqdf2",
    "__nedf2",

    "__floatsisf",
    "__floatsidf",
    "__floatdisf",
    "__floatdidf",

    "__fixsfsi",
    "__fixsfdi",
    "__fixdfsi",
    "__fixdfdi",
    "__fixunssfsi",
    "__fixunssfdi",
    "__fixunsdfsi",
    "__fixunsdfdi",

    "__extendsfdf2",

    "__truncdfsf2",

    "__ashlsi3",
    "__ashrsi3",
    "__lshrsi3",
    "__divsi3",
    "__udivsi3",
    "__modsi3",
    "__umodsi3",
    "__mulsi3",
    "__negsi2",
    "__cmpsi2",
    "__ucmpsi2",

    "__ashldi3",
    "__ashrdi3",
    "__lshrdi3",
    "__divdi3",
    "__udivdi3",
    "__muldi3",
    "__moddi3",
    "__umoddi3",
    "__negdi2",
    "__cmpdi2",
    "__ucmpdi2",

};
static int lfLibcallNamesSorted = FALSE;
/************************************************************************/
/* pic30_SortLibcallNames() -- Sort the array of libcall names.        */
/************************************************************************/
static void
pic30_SortLibcallNames()
{
    long gap, i, j;
    const char **ppj;
    const char **ppg;
    const char *pt;

    /*
    ** Sort the LIBCALLs array, using Shell's algorithm.
    */

    for (gap = NELEMENTS(lpszLibcallNames)/2; gap > 0; gap /= 2)
    {
        for (i = gap; i < (long)NELEMENTS(lpszLibcallNames); ++i)
        {
            for (j = i-gap; j >= 0; j -= gap)
            {
                ppj = &lpszLibcallNames[j];
                ppg = &lpszLibcallNames[j+gap];
                if (strcmp(*ppj, *ppg) <= 0)
                {
                    break;
                }

                /*
                ** Swap j, g
                */

                pt = *ppj;
                *ppj = *ppg;
                *ppg = pt;
            }
        }
    }
    lfLibcallNamesSorted = TRUE;
}
/*
** Return 1 if the operand is the name of a LIBCALL library routine.
*/
int
pic30_libcall(const char *pszSymbolName)
{
    int compare;
    int base,last,this;

    if (!lfLibcallNamesSorted)
    {
        pic30_SortLibcallNames();
    }
    base = 0;
    last = NELEMENTS(lpszLibcallNames) - 1;
    while (last >= base)
    {
        this = base + (last-base) / 2;
        compare = strcmp(pszSymbolName, lpszLibcallNames[this]);
        if (compare > 0)
        {
            base = this+1;
        }
        else if (compare == 0) 
        {
            return(TRUE);
        }
        else
        {
            last = this-1;
        }
    }
    return(FALSE);
}
/*
** Return 1 if the operand is a near data space reference.
*/
int
pic30_neardata_space_operand_p(op)
     rtx op;
{
    int fNear;

    fNear = FALSE;
    switch (GET_CODE (op))
    {
    case LABEL_REF:
        break;
    case SYMBOL_REF:
        fNear = SYMBOL_REF_FLAG(op);
        break;
    case PLUS:
        /* Assume canonical format of symbol + constant.
        Fall through.  */
    case CONST:
        fNear = pic30_neardata_space_operand_p(XEXP(op, 0));
        break;
    default:
        break;
    }
    return(fNear);
}
/*
** Return 1 if the operand is a program space reference.
*/
int
pic30_program_space_operand_p(op)
     rtx op;
{
    const char *pszSymbolName;

    switch (GET_CODE (op))
    {
    case LABEL_REF:
        return(TRUE);
    case SYMBOL_REF:
        pszSymbolName = XSTR(op, 0);
        if (PIC30_PGM_NAME_P(pszSymbolName) || PIC30_FCN_NAME_P(pszSymbolName))
        {
            return(TRUE);
        }
        /*
        ** Check for LIBCALL function names
        */
        if (pic30_libcall(pszSymbolName))
        {
            return(TRUE);
        }
        break;
    case PLUS:
        /* Assume canonical format of symbol + constant.
        Fall through.  */
    case CONST:
        return(pic30_program_space_operand_p(XEXP(op, 0)));
    default:
        break;
    }
    return(FALSE);
}

int pic30_has_space_operand_p(rtx op, char *has) {
   const char *pszSymbolName;

   switch (GET_CODE (op))
   {
   case LABEL_REF:
      return(TRUE);
   case SYMBOL_REF:
      pszSymbolName = XSTR(op, 0);
      if (PIC30_HAS_NAME_P(pszSymbolName, has))
      {
         return(TRUE);
      }
      break;
   case PLUS:
      /* Assume canonical format of symbol + constant.
      Fall through.  */
   case CONST:
      return pic30_has_space_operand_p(XEXP(op, 0), has);
   default:
      break;
   }
   return(FALSE);
}

/*
** Return 1 if the operand is a data space reference.
*/
int
pic30_data_space_operand_p(op)
     rtx op;
{
    switch (GET_CODE (op))
    {
    case LABEL_REF:
        return(FALSE);
    case SYMBOL_REF:
        return(!pic30_program_space_operand_p(op));
    case CONST_INT:
        /*
        ** Cast of constant integer to address.
        */
        return(TRUE);
    case PLUS:
        /* Assume canonical format of symbol + constant.
        Fall through.  */
    case CONST:
        return(pic30_data_space_operand_p(XEXP(op, 0)));
    default:
        break;
    }
    return(FALSE);
}

/*
** GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
** that is a valid memory address for an instruction.
** The MODE argument is the machine mode for the MEM expression
** that wants to use this address.
*/
int
pic30_check_legit_addr(mode, addr, fStrict)
     enum machine_mode mode;
     rtx addr;
     int fStrict;
{
    int fLegit;
    rtx base = NULL_RTX;        /* Base register */
    rtx indx = NULL_RTX;        /* Index register */
    rtx disp = NULL_RTX;        /* Displacement */

    fLegit = TRUE;
    if (CONSTANT_ADDRESS_P(addr))
    {
        switch (mode)
        {
        case HImode:
        case SImode:
        case SFmode:
            return(TRUE);
        case DFmode:
        case DImode:
            return(FALSE);
        default:
            return(    pic30_program_space_operand_p(addr) ||
                pic30_neardata_space_operand_p(addr));
        }
    }
    switch (GET_CODE(addr))
    {
    case PRE_DEC:
    case PRE_INC:
    case POST_DEC:
    case POST_INC:
        switch (mode)
        {
        case HImode:
        case QImode:
            base = XEXP(addr, 0);
            if (!REG_P(base))
            {
                fLegit = FALSE;
            }
            break;
        default:
            fLegit = FALSE;
            break;
        }
        break;
    case PRE_MODIFY:
    case POST_MODIFY:
        fLegit = FALSE;
        break;
    case REG:
        /*
        ** Register indirect
        */
        base = addr;
        break;
    case PLUS:
        /*
        ** Register indirect with displacement or index.
        */
        {
            rtx op0 = XEXP(addr, 0);
            rtx op1 = XEXP(addr, 1);
            enum rtx_code code0 = GET_CODE(op0);

            switch (code0)
            {
            case REG:
                base = op0;
                if (REG_P(op1))
                {
                    /*
                    ** base + index
                    */
                    indx = op1;
                }
                else
                {
                    /*
                    ** base + displacement
                    */
                    disp = op1;
                }
                break;
            default:
                fLegit = FALSE;
                break;
            }
        }
        break;
    case MEM:
        /*
        ** Indirect indirect addressing.
        */
        fLegit = FALSE;
        break;
    default:
        fLegit = FALSE;
        break;
    }
    /*
    ** Validate the base.
    */
    if (fLegit && (base != NULL_RTX))
    {
        if (GET_CODE(base) != REG)
        {
            fLegit = FALSE;
        }
        if (fLegit)
        {
            switch (mode)
            {
            case QImode:
            case HImode:
            case SImode:
            case DImode:
            case SFmode:
            case DFmode:
            case BLKmode:
                break;
            default:
                fLegit = FALSE;
                break;
            }
        }
        if (fLegit && fStrict)
        {
            if (!REGNO_OK_FOR_BASE_P((int)REGNO(base)))
            {
                fLegit = FALSE;
            }
        }
    }
    /*
    ** Validate the index.
    */
    if (fLegit && (indx != NULL_RTX))
    {
        if (GET_CODE(indx) != REG)
        {
            fLegit = FALSE;
        }
        if (fLegit)
        {
            switch (mode)
            {
            case HImode:
            case QImode:
                break;
            default:
                fLegit = FALSE;
                break;
            }
        }
        if (fLegit && fStrict)
        {
            if (!REGNO_OK_FOR_INDEX_P((int)REGNO(indx)))
            {
                fLegit = FALSE;
            }
        }
    }
    /*
    ** Validate displacement.
    */
    if (fLegit && (disp != NULL_RTX))
    {
        /*
        ** Can't add an index with a disp.
        */
        if (indx)
        {
            fLegit = FALSE;
        }
        if (fLegit)
        {
            if (GET_CODE (disp) != CONST_INT)
            {
                fLegit = FALSE;
            }
        }
        if (fLegit)
        {
            int nMin, nMax, nDisp;

            switch (mode)
            {
            case QImode:
                nMin = PIC30_DISP_MIN;
                nMax = PIC30_DISP_MAX;
                break;
            case SFmode:
            case DFmode:
            case HImode:
            case SImode:
            case DImode:
                nMin = PIC30_DISP_MIN*2;
                nMax = PIC30_DISP_MAX*2 + 
                    UNITS_PER_WORD - GET_MODE_SIZE(mode);
                break;
            default:
                nMin = 0;
                nMax = 0;
                break;
            }
            nDisp = INTVAL(disp);
            fLegit = ((nMin <= nDisp) && (nDisp <= nMax));
        }
    }
    return(fLegit);
}

enum reg_class
pic30_preferred_reload_class (x, class)
     rtx x;
     enum reg_class class;
{
    if ((GET_MODE(x) == QImode) && pic30_U_constraint(x))
    {
        class = A_REGS;
    }
    return(class);
}
/*
** Return the register class of a scratch register needed to load
** or store a register of class CLASS in MODE.
** For dsPIC, we can only load/store QImode using WREG.
*/
enum reg_class
pic30_secondary_reload_class (class, mode, x)
     enum reg_class class;
     enum machine_mode mode;
     rtx x;
{
    enum reg_class src = NO_REGS;

    if ((mode == QImode) && pic30_U_constraint(x))
    {
        src = (class == A_REGS) ? NO_REGS : A_REGS;
    }
    return(src);
}
/*
** Handle dsPIC30 specific pragmas for compatibility with existing
** compilers for the C17/C18.
**
** pragma                   attribute
** ----------------------------------------------------------
** interrupt function                      interrupt
** code section                    n/a
** idata section                n/a
** udata section                n/a
**
*/

/************************************************************************/
/*
** Parse the <#pragma <section> [section]> pragma.
*/
/************************************************************************/
static void 
pic30_handle_section_pragma(pfile, pvalue)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     tree *pvalue;
{
    int c;
    const char *pszScnName;
    tree x;

        c = c_lex(&x);
        if (c == CPP_EOF) *pvalue = NULL_TREE;
        else
    {
        /*
        ** User-specified <section-type> section name
        */
        for ( ; ; )
        {
            if (pic30_parse_pragma_option(c) == 0) {
                error("Invalid or missing section name");
            }
            /*
            ** Check for data qualifiers; keep reading until we
             ** get an identifier... we read one extra token
            ** but consume it later
            */
            pszScnName = IDENTIFIER_POINTER(x);
            c = c_lex(&x);
            if (    (strcmp("access", pszScnName) == 0) ||
                (strcmp("shared", pszScnName) == 0) ||
                (strcmp("overlay", pszScnName) == 0) )
            {
                warning("data qualifier '%s' ignored",
                                pszScnName);
                pszScnName = NULL;
            }
            else
            {
                break;
            }
        }
        *pvalue = make_node(STRING_CST);
        TREE_STRING_POINTER(*pvalue) = xstrdup(pszScnName);
        TREE_STRING_LENGTH(*pvalue) = strlen(pszScnName);
        /*
        ** Check for <location> specification
        */
        if (c == CPP_EQ)
        {
            warning("absolute address specification ignored");
            do {
                c = c_lex(&x);
            } while (c != CPP_EOF);
        }
        else if (c != CPP_EOF)
        {
            if (pic30_parse_pragma_option(c) == 0) {
                error("Invalid location qualifier: '%s'", 
                                      pszScnName);
                return;
            }
             pszScnName = IDENTIFIER_POINTER(x);
            /*
            ** Validate the location qualifier
            */
            if (    (strcmp("gpr", pszScnName) == 0) ||
                (strcmp("sfr", pszScnName) == 0) )
            {
                warning("location qualifier '%s' ignored",
                                pszScnName);
                pszScnName = NULL;
            }
            else
            {
                error("Invalid location qualifier: '%s'", 
                    pszScnName);
            }
        }
    }
}
/************************************************************************/
/*
** Parse the <#pragma code [section]> pragma.
*/
/************************************************************************/
void
pic30_handle_code_pragma(cpp_reader *pfile)
{
    pic30_handle_section_pragma(pfile, &lTreeTextScnName);
}
/************************************************************************/
/*
** Parse the <#pragma idata [section]> pragma.
*/
/************************************************************************/
void
pic30_handle_idata_pragma(cpp_reader *pfile)
{
    pic30_handle_section_pragma(pfile, &lTreeIDataScnName);
}
/************************************************************************/
/*
** Parse the <#pragma udata [section]> pragma.
*/
/************************************************************************/
void
pic30_handle_udata_pragma(cpp_reader *pfile)
{
    pic30_handle_section_pragma(pfile, &lTreeUDataScnName);
}

/************************************************************************/
/*
** Parse a pragma option.
*/
/************************************************************************/
static int 
pic30_parse_pragma_option(pc)
     int pc;
{
    return (pc == CPP_NAME);
}

/************************************************************************/
/*
** Lookup a name in the current scope.
*/
/************************************************************************/
static tree
pic30_lookup_vardecl(pszName)
    const char *pszName;
{
    int i, len;
    tree decl;
    tree namelist;
    const char *pszDeclName;

    namelist = getdecls();
    len = list_length(namelist);
    for (i = 0, decl = namelist; i < len; ++i, decl = TREE_CHAIN(decl))
    {
        if (TREE_CODE(decl) == VAR_DECL)
        {
            pszDeclName = IDENTIFIER_POINTER(DECL_NAME(decl));
            if (strcmp(pszName, pszDeclName) == 0)
            {
                break;
            }
        }
    }
    return(decl);
}
/************************************************************************/
/*
** Parse the interrupt pragma:
**
** #pragma interrupt <functionname> [shadow] [save=<symbol-list>]
*/
/************************************************************************/
void
pic30_handle_interrupt_pragma(cpp_reader *pfile ATTRIBUTE_UNUSED)
{
    int c;
    tree treeOptName;
    tree treeFcnName;
    tree treeFcn;
    tree treeFcnCall = NULL_TREE;
    tree treeFcnType;
    tree treeFcnArgs = NULL_TREE;
    tree treeFcnSave = NULL_TREE;
    const char *pszFcnName;
    const char *pszOptName;
    tree x;

    if (!global_bindings_p())
    {
        error("interrupt pragma must have file scope");
        return;
    }
    c = c_lex(&x);
    /*
    ** Parse the interrupt function name
    */
    if (pic30_parse_pragma_option(c) == 0)
    {
          error("Invalid or missing function name from interrupt pragma");
          return;
        }
    pszFcnName = IDENTIFIER_POINTER(x);
    /*
    ** Parse the optional shadow/save parameter
    */
        c = c_lex(&x);
    while (c != CPP_EOF)
    {
        if (pic30_parse_pragma_option(c) == 0) {
                  error("Invalid option to interrupt pragma");
                  return;
            }
        pszOptName = IDENTIFIER_POINTER(x);
        if (strcmp(pszOptName, "shadow") == 0)
        {
            /*
            ** Add this function to the shadow function tree
            */
            treeFcnName = get_identifier(pszFcnName);
            treeFcn = build_tree_list(treeFcnName, NULL_TREE);
            lTreeShadow = chainon(lTreeShadow, treeFcn);
                        c = c_lex(&x);
        }
        else if (strcmp(pszOptName, "save") == 0)
        {
            c = c_lex(&x);
            if (c != CPP_EQ)
            {
                error("Missing '=' for 'save' in interrupt"
                                      " pragma");
            }
            c = c_lex(&x);
            /*
            ** Parse the symbol list
            */
            for ( ; ; )
            {
                /*
                ** Locate the symbol's declaration.
                */
                if (pic30_parse_pragma_option(c) == 0) {
                    error("Invalid save variable in "
                          "interrupt pragma");
                    return;
                }
                pszOptName = IDENTIFIER_POINTER(x);
                treeOptName = pic30_lookup_vardecl(pszOptName);
                if (treeOptName == NULL_TREE)
                {
                    error("symbol '%s' not defined",
                        pszOptName);
                    return;
                }
                /*
                ** Add this variable to the save(...) list
                */
                treeOptName = build_tree_list(NULL_TREE,
                                treeOptName);
                treeFcnArgs = chainon(treeFcnArgs, treeOptName);
                /*
                ** Check for continuation
                */
                c = c_lex(&x);
                if (c != CPP_COMMA)
                {
                    /*
                    ** End of list.
                    */
                    break;
                }
                /*
                ** Advance to the 1st token after the ','
                */
                c = c_lex(&x);
            }
            /*
            ** Create a pseduo-function call for
            ** the interrupt save(...) list.
            */
            if (treeFcnSave == NULL_TREE)
            {
                treeFcnSave = pic30_identSave[1];
                treeFcnType = build_pointer_type(
                      build_pointer_type(
                       build_pointer_type(void_type_node)));
                treeFcnType = build_function_type(treeFcnType,
                                  NULL_TREE);
                treeFcnSave = build_decl(FUNCTION_DECL,
                            treeFcnSave,
                            treeFcnType);
            }
            treeFcnCall = build1(ADDR_EXPR,
                build_pointer_type(TREE_TYPE(treeFcnSave)),
                                treeFcnSave);
            treeFcnCall = build(CALL_EXPR,
                    TREE_TYPE(TREE_TYPE(treeFcnSave)),
                    treeFcnCall, treeFcnArgs, NULL_TREE);
    
            treeFcnCall = build_tree_list(NULL_TREE, treeFcnCall);
        }
        else
        {
            error("Invalid option '%s' to interrupt pragma",
                  pszOptName);
                        c = c_lex(&x);
        }
    }
    /*
    ** Add this function to the interrupt function tree
    */
    treeFcnName = get_identifier(pszFcnName);
    treeFcn = build_tree_list(treeFcnName, treeFcnCall);
    lTreeInterrupt = chainon(lTreeInterrupt, treeFcn);
}

/*
** dsPIC30-specific attribute support.
**
**   interrupt - for interrupt functions
**
**   near -- for data addressable in 13-bits
**   far -- for data addressable in 16-bits
**
**   space - select address space
**
**    prog: locate object in rom (program space)
**    data: locate object in ram (data space)
*/

/*
** Check to see if any of the attributes in <list> have associated
** with them the name of declaration <decl>.  If so, attach the
** atribute name <attrib> to the declaration.
**
** This is the method used to attached attributes defined with #pragma
** to declarations.
** For example
** #pragma interupt isr
** creates a node in the list lTreeInterrupt, with the associated name "isr".
** Then, when parsing
** void isr(void)
** this functin will detect that there is an lTreeInterrupt node with the
** assocatied name "isr", and so the function declaration will be tagged
** with an interrupt attribute.
*/
static int
pic30_check_decl_attribute(attrib, list, decl, attributes)
     tree attrib;
     tree list, decl, *attributes;
{
    int fCheck;
    const char *pszDeclName;

    fCheck = FALSE;
    pszDeclName = IDENTIFIER_POINTER(DECL_NAME(decl));
    while (list != NULL_TREE)
    {
        if ((TREE_CODE(list) != ERROR_MARK) &&
            (IDENTIFIER_POINTER(TREE_PURPOSE(list)) == pszDeclName))
        {
            break;
        }
        list = TREE_CHAIN(list);
    }
    if (list)
    {
        *attributes = chainon(*attributes,
                build_tree_list(attrib, TREE_VALUE(list)));
        DECL_ATTRIBUTES(decl) = *attributes;
        fCheck = TRUE;
    }
    return(fCheck);
}
static void
pic30_check_type_attribute(attrib, decl, attributes)
     tree attrib;
     tree decl, *attributes;
{
    tree attr;
    tree type;
    tree type_attr_list;
    const char *pszAttrName;

    pszAttrName = IDENTIFIER_POINTER(attrib);
    type = TREE_TYPE(decl);
    if (TREE_CODE(type) != ERROR_MARK)
    {
        type_attr_list = TYPE_ATTRIBUTES(type);
        attr = lookup_attribute(pszAttrName, type_attr_list);
        if (attr != NULL_TREE)
        {
            *attributes = chainon(*attributes,
                 build_tree_list(attrib, NULL_TREE));
            DECL_ATTRIBUTES(decl) = *attributes;
        }
    }
}

/*
** See if the <interrupt> attribute is set for a function.
*/
int pic30_interrupt_function_p(tree decl) {
  const char *attrname = IDENTIFIER_POINTER(pic30_identInterrupt[0]);
  tree attrlist = DECL_ATTRIBUTES(decl);
  int fInterrupt = lookup_attribute(attrname, attrlist) != NULL_TREE;

  return fInterrupt;
}

int pic30_noreturn_function(tree decl) {
  const char *attrname = "noreturn";
  tree attrlist = DECL_ATTRIBUTES(decl);
  int noreturn = lookup_attribute(attrname, attrlist) != NULL_TREE;

  return noreturn ;
}

/*
** See if the <shadow> attribute is set for a function.
*/
int
pic30_shadow_function_p(decl)
    tree decl;
{
    const char *attrname = IDENTIFIER_POINTER(pic30_identShadow[0]);
    tree attrlist = DECL_ATTRIBUTES(decl);
    int fShadow = lookup_attribute(attrname, attrlist) != NULL_TREE;

    return(fShadow);
}
/*
** See if the <shadow> attribute is set for an operand.
*/
int
pic30_shadow_operand_p(operand)
    rtx operand;
{
  int fShadow;
  rtx symbol;
  const char *pszSymbolName;

  fShadow = FALSE;
  if (GET_CODE(operand) == MEM)
  {
    symbol = XEXP(operand,0);
    if (GET_CODE(symbol) != REG)
    {
      pszSymbolName = XSTR(symbol, 0);
      fShadow = (PIC30_FCNS_NAME_P(pszSymbolName) != 0);
    }
  }
  return(fShadow);
}
/*
** See if the <interrupt(vector(id))> attribute is set for a function.
*/
static int
pic30_interrupt_vector_id(decl)
    tree decl;
{
    tree attr;
    tree v;
    tree fcn;
    tree fcnargs;
    int nVectorID = 0;

    attr = lookup_attribute(IDENTIFIER_POINTER(pic30_identInterrupt[0]),
                        DECL_ATTRIBUTES(decl));
    if (attr)
    {
        attr = TREE_VALUE(attr);
    }
    while (attr != NULL_TREE)
    {
        v = TREE_VALUE(attr);
        if (v && (TREE_CODE(v) == CALL_EXPR))
        {
            fcn = TREE_OPERAND(TREE_OPERAND(v,0),0);
            fcnargs = TREE_OPERAND(v,1);
            if (IDENT_IRQ(DECL_NAME(fcn)))
            {
                v = TREE_VALUE(fcnargs);
                nVectorID = TREE_INT_CST_LOW(v);
                break;
            }
            if (IDENT_ALTIRQ(DECL_NAME(fcn)))
            {
                v = TREE_VALUE(fcnargs);
                nVectorID = -(TREE_INT_CST_LOW(v)+1);
                break;
            }
        }
        attr = TREE_CHAIN(attr);
    }
    return(nVectorID);
}
/*
** See if the <interrupt(preprologe("asm"))> attribute is set for 
** the current function.
*/
char *
pic30_interrupt_preprologue(void)
{
        tree decl = current_function_decl;
    tree attr;
    tree v;
    tree fcn;
    tree fcnargs;
    const char *pszPreprologue = NULL;

    attr = lookup_attribute(IDENTIFIER_POINTER(pic30_identInterrupt[0]),
                DECL_ATTRIBUTES(decl));
    if (attr)
    {
                attr = TREE_VALUE(attr);
    }
    while (attr != NULL_TREE)
    {
        v = TREE_VALUE(attr);
        if (v && (TREE_CODE(v) == CALL_EXPR))
        {
            fcn = TREE_OPERAND(TREE_OPERAND(v,0),0);
            fcnargs = TREE_OPERAND(v,1);
            if (IDENT_PREPROLOGUE(DECL_NAME(fcn)))
            {
                v = TREE_VALUE(fcnargs);
                while ( (TREE_CODE(v) == NOP_EXPR) ||
                    (TREE_CODE(v) == ADDR_EXPR))
                {
                    v = TREE_OPERAND(v, 0);
                }
                pszPreprologue = TREE_STRING_POINTER(v);
                break;
            }
        }
        attr = TREE_CHAIN(attr);
    }
    return((char *)pszPreprologue);
}
/*
** See if the <near> attribute is set for a decl.
*/
int
pic30_near_decl_p(decl)
    tree decl;
{
    int fNear;

    fNear = (lookup_attribute(
            IDENTIFIER_POINTER(pic30_identNear[0]),
                    DECL_ATTRIBUTES(decl)) !=
                                NULL_TREE)
        ||
        (lookup_attribute(
            IDENTIFIER_POINTER(pic30_identSFR[0]),
                    DECL_ATTRIBUTES(decl)) !=
                                NULL_TREE)
        ;

    return(fNear);
}
/*
** See if the <far> attribute is set for a decl.
*/
int
pic30_far_decl_p(decl)
    tree decl;
{
    int fFar;

    fFar = lookup_attribute(IDENTIFIER_POINTER(pic30_identFar[0]),
                    DECL_ATTRIBUTES(decl)) !=
                                NULL_TREE;

    return(fFar);
}
/*
** See if a declaration is located in code space.
*/
#if 0
int
pic30_codespace_decl_p(decl)
    tree decl;
{
    int fCodeSpace = FALSE;
    tree space = NULL_TREE;

    /*
    ** Check if the <space(code)> attribute is set for the declaration.
    */
    space = lookup_attribute(IDENTIFIER_POINTER(pic30_identSpace[0]),
                        DECL_ATTRIBUTES(decl));
    if (space)
    {
        if (IDENT_PROG(TREE_VALUE(TREE_VALUE(space))))
        {
            fCodeSpace = TRUE;
        }
    }
    /*
    ** Check if the declaration is in a named section, and, if so,
    ** if that section is executable or read-only.
    */
    if ((fCodeSpace == FALSE) && 
        (space == NULL_TREE) && (DECL_SECTION_NAME(decl) != NULL_TREE))
    {
        const char *pszSectionName;

        pszSectionName = TREE_STRING_POINTER(DECL_SECTION_NAME(decl));
        if (pic30_section_type_flags(NULL, pszSectionName, 1) & SECTION_CODE)
        {
            fCodeSpace = TRUE;
            break;
        }
    }
    return(fCodeSpace);
}
#endif

#if 0
#define pic30_codespace_decl_p(decl) pic30_space_decl_p(decl, pic30_code)

int pic30_space_decl_p(tree decl, enum pic30_memory_space check_space) {
  tree space = NULL_TREE;
  int is_space = 0;
  int flag = 0;
  const char *pszSectionName;

  space = lookup_attribute(IDENTIFIER_POINTER(pic30_identSpace[0]),
                           DECL_ATTRIBUTES(decl));
  if ((space == 0) && (DECL_SECTION_NAME(decl) != NULL_TREE)) {
    pszSectionName = TREE_STRING_POINTER(DECL_SECTION_NAME(decl));
    flag = pic30_section_type_flags(NULL, pszSectionName, 1);
  }
  switch (check_space) {
    case pic30_code: 
               is_space = space &&
                          (IDENT_PROG(TREE_VALUE(TREE_VALUE(space))));
               is_space |= (flag & SECTION_CODE);
               break;
    case pic30_data:
               is_space = space &&
                          (IDENT_DATA(TREE_VALUE(TREE_VALUE(space))));
               is_space |= (flag & SECTION_WRITE);
               break;
    case pic30_xmemory:
               is_space = space &&
                          (IDENT_XMEMORY(TREE_VALUE(TREE_VALUE(space))));
               is_space |= (flag & SECTION_XMEMORY);
               break;
    case pic30_ymemory:
               is_space = space &&
                          (IDENT_YMEMORY(TREE_VALUE(TREE_VALUE(space))));
               is_space |= (flag & SECTION_YMEMORY);
               break;
    case pic30_const:
               is_space = space &&
                          (IDENT_CONST(TREE_VALUE(TREE_VALUE(space))));
               is_space |= (flag & SECTION_READ_ONLY);
               break;
    case pic30_psv:
               is_space = space &&
                          (IDENT_PSV(TREE_VALUE(TREE_VALUE(space))));
               is_space |= (flag & SECTION_PSV);
               break;
    case pic30_eedata:
               is_space = space &&
                          (IDENT_EEDATA(TREE_VALUE(TREE_VALUE(space))));
               is_space |= (flag & SECTION_EEDATA);
  }
  return is_space;
}
#endif

/*
** Nothing yet.
*/
void
pic30_insert_attributes(node, attributes, prefix_attributes)
    tree node ATTRIBUTE_UNUSED,
    *attributes ATTRIBUTE_UNUSED,
    *prefix_attributes ATTRIBUTE_UNUSED;
{
}
/*
** Assign default attributes to a newly defined declaration.
*/
static void
pic30_set_default_decl_attributes(decl, attributes)
     tree decl, *attributes;
{
    pic30_init_idents();

    switch (TREE_CODE(decl))
    {
    case FUNCTION_DECL:
        /*
        ** Check for <#pragma interrupt> attribute
        */
        if (!pic30_check_decl_attribute(pic30_identInterrupt[0],
                            lTreeInterrupt,
                            decl, attributes))
        {
            pic30_check_type_attribute(pic30_identInterrupt[0],
                            decl, attributes);
        }
        /*
        ** Check for <#pragma shadow> attribute
        */
        if (!pic30_check_decl_attribute(pic30_identShadow[0],
                            lTreeShadow,
                            decl, attributes))
        {
            pic30_check_type_attribute(pic30_identShadow[0],
                            decl, attributes);
        }
        /*
        ** Check for <#pragma code> section attribute
        */
        if (lTreeTextScnName != NULL_TREE)
        {
            DECL_SECTION_NAME(decl) = lTreeTextScnName;
        }
        break;

    case VAR_DECL:
        /*
        ** Check for space attributes associated with the data type.
        */
        pic30_check_type_attribute(pic30_identSpace[0],
                        decl,
                        attributes);
        /*
        ** Check for <#pragma idata> section attribute
        */
        if (    (lTreeIDataScnName != NULL_TREE) &&
            (DECL_INITIAL(decl) != NULL_TREE))
        {
            DECL_SECTION_NAME(decl) = lTreeIDataScnName;
        }
        /*
        ** Check for <#pragma udata> section attribute
        */
        if (    (lTreeUDataScnName != NULL_TREE) &&
            (DECL_INITIAL(decl) == NULL_TREE))
        {
            DECL_SECTION_NAME(decl) = lTreeUDataScnName;
        }
        break;

    default:
        break;
    }
}
/*
** Initialize our target-specific identifiers.
*/
static void
pic30_init_idents(void)
{
    if (pic30_identInterrupt[0] == 0)
    {
        pic30_identInterrupt[0] = get_identifier ("interrupt");
        pic30_identInterrupt[1] = get_identifier ("__interrupt__");
        pic30_identShadow[0] = get_identifier ("shadow");
        pic30_identShadow[1] = get_identifier ("__shadow__");
        pic30_identIRQ[0] = get_identifier ("irq");
        pic30_identIRQ[1] = get_identifier ("__irq__");
        pic30_identAltIRQ[0] = get_identifier ("altirq");
        pic30_identAltIRQ[1] = get_identifier ("__altirq__");
        pic30_identSave[0] = get_identifier ("save");
        pic30_identSave[1] = get_identifier ("__save__");
        pic30_identPreprologue[0] = get_identifier ("preprologue");
        pic30_identPreprologue[1] = get_identifier ("__preprologue__");

        pic30_identSFR[0] = get_identifier ("sfr");
        pic30_identSFR[1] = get_identifier ("__sfr__");
        pic30_identNear[0] = get_identifier ("near");
        pic30_identNear[1] = get_identifier ("__near__");
        pic30_identFar[0] = get_identifier ("far");
        pic30_identFar[1] = get_identifier ("__far__");

        pic30_identSpace[0] = get_identifier ("space");
        pic30_identSpace[1] = get_identifier ("__space__");
        pic30_identProg[0] = get_identifier ("prog");
        pic30_identProg[1] = get_identifier ("__prog__");
        pic30_identData[0] = get_identifier ("data");
        pic30_identData[1] = get_identifier ("__data__");
        pic30_identXmemory[0] = get_identifier("xmemory");
        pic30_identXmemory[1] = get_identifier("__xmemory__");
        pic30_identYmemory[0] = get_identifier("ymemory");
        pic30_identYmemory[1] = get_identifier("__ymemory__");
        pic30_identConst[0] = get_identifier("auto_psv");
        pic30_identConst[1] = get_identifier("__auto_psv__");
        pic30_identPersistent[0] = get_identifier("persistent");
        pic30_identPersistent[1] = get_identifier("__persistent__");
        pic30_identPsv[0] = get_identifier("psv");
        pic30_identPsv[1] = get_identifier("__psv__");
        pic30_identEedata[0] = get_identifier("eedata");
        pic30_identEedata[1] = get_identifier("__eedata__");
        pic30_identDma[0] = get_identifier("dma");
        pic30_identDma[1] = get_identifier("__dma__");

        pic30_identAddress[0] = get_identifier("address");
        pic30_identAddress[1] = get_identifier("__address__");
        pic30_identReverse[0] = get_identifier("reverse");
        pic30_identReverse[1] = get_identifier("__reverse__");
        pic30_identNoload[0] = get_identifier("noload");
        pic30_identNoload[1] = get_identifier("__noload__");
        pic30_identMerge[0] = get_identifier("merge");
        pic30_identMerge[1] = get_identifier("__merge__");
        pic30_identUnordered[0] = get_identifier("unordered");
        pic30_identUnordered[1] = get_identifier("__unordered__");
        pic30_identUnsafe[0] = get_identifier("unsafe");
        pic30_identUnsafe[1] = get_identifier("__unsafe__");
        pic30_identAligned[0] = get_identifier("aligned");
        pic30_identAligned[1] = get_identifier("__aligned__");
    }
}
/*
** Return nonzero if ARGS are valid decl interrupt(save) attribute args.
*/
static int
pic30_valid_machine_decl_save(fcnargs)
     tree fcnargs;
{
    while (fcnargs)
    {
        const char *pszv = NULL;
        tree decl;

        tree v = TREE_VALUE(fcnargs);
        while (    (TREE_CODE(v) == NOP_EXPR) ||
            (TREE_CODE(v) == ADDR_EXPR))
        {
            v = TREE_OPERAND(v, 0);
        }
        if (TREE_CODE(v) == ARRAY_REF)
        {
            decl = TREE_VALUE(v);
            if (TREE_CODE(decl) != VAR_DECL)
            {
                error("interrupt save modifier syntax error");
                return(FALSE);
            }
            pszv = IDENTIFIER_POINTER(DECL_NAME(decl));
            if ((TREE_CODE(TREE_OPERAND(v,1)) != INTEGER_CST)
                ||
                (TREE_CODE(TYPE_SIZE(TREE_TYPE(v))) != INTEGER_CST))
            {
                error("save variable '%s' index not constant",
                                    pszv);
                return(FALSE);
            }
        }
        else if (TREE_CODE(v) == VAR_DECL)
        {
            decl = v;
            pszv = IDENTIFIER_POINTER(DECL_NAME(decl));
        }
        else
        {
            error("interrupt save modifier syntax error");
            return(FALSE);
        }
        if (DECL_ALIGN(decl) < BITS_PER_WORD)
        {
            error("save variable '%s' is not word aligned", pszv);
            return(FALSE);
        }
        if (DECL_SIZE(decl) == NULL_TREE)
        {
            error("save variable '%s' size is not known", pszv);
            return(FALSE);
        }
        if (TREE_INT_CST_LOW(DECL_SIZE(decl)) % BITS_PER_WORD)
        {
            error("save variable '%s' size is not even", pszv);
            return(FALSE);
        }
        fcnargs = TREE_CHAIN(fcnargs);
    }
    return(TRUE);
}
/*
** Return nonzero if ARGS are valid decl interrupt(preprologue) attribute args.
*/
static int
pic30_valid_machine_decl_preprologue(fcnargs)
    tree fcnargs;
{
    tree v = TREE_VALUE(fcnargs);
    while ( (TREE_CODE(v) == NOP_EXPR) ||
        (TREE_CODE(v) == ADDR_EXPR))
    {
        v = TREE_OPERAND(v, 0);
    }
    if (TREE_CODE(v) != STRING_CST)
    {
        error("invalid preprologue argument");
        return(FALSE);
    }
    return(TRUE);
}
/*
** Return nonzero if ARGS are valid SFR attribute args.
*/
#define    MAX_SFR_ADDRESS    (8*1024-1)
static int
pic30_valid_machine_decl_sfr_attribute(tree decl, tree args)
{
    int address;
    PSFR pSFR;
    const char *pszIdent = IDENTIFIER_POINTER(DECL_NAME(decl));
    tree v;
    /*
    ** sfr(address) modifier
    */
    if (!DECL_EXTERNAL(decl))
    {
        error("sfr attribute requires extern storage class");
        return(FALSE);
    }
    if (args == 0) {
      return TRUE;
    }
    v = TREE_VALUE(args);
    if (TREE_CODE(v) != INTEGER_CST)
    {
        error("sfr address is not a constant");
        return(FALSE);
    }
    address = TREE_INT_CST_LOW(v);
    if ((0 > address) || (address > MAX_SFR_ADDRESS))
    {
        error("sfr address 0x%x is not valid", address);
        return(FALSE);
    }
    pSFR = (PSFR)xmalloc(sizeof(*pSFR));
    pSFR->pName = pszIdent;
    pSFR->address = address;
    pSFR->pNext = lpSFRs;
    lpSFRs = pSFR;

    return(TRUE);
}

#define       MIN_IRQ_ID      45
#define       MAX_IRQ_ID      53

/*
** Return nonzero if ARGS are valid decl interrupt attribute args.
*/
int
pic30_valid_machine_decl_interrupt_attribute(args, attached_to)
     tree args;
     char *attached_to;
{
  tree a;
  int validate_name = TARGET_ISR_WARN;
  int result = TRUE;

  for (a = args; a && (result == TRUE); a = TREE_CHAIN(a))
  {
    tree v = TREE_VALUE(a);
    tree fcn, fcnargs;

    switch (TREE_CODE(v))
    {
      case CALL_EXPR:
        if ((TREE_CODE(TREE_OPERAND(v,0)) == ADDR_EXPR) &&
            (TREE_CODE(TREE_OPERAND(TREE_OPERAND(v,0),0)) == FUNCTION_DECL))
        {
          fcn = TREE_OPERAND(TREE_OPERAND(v,0),0);
          fcnargs = TREE_OPERAND(v,1);
        }
        else
        {
          error("interrupt modifier syntax error");
          result = FALSE;
          break;
        }
        if (IDENT_IRQ(DECL_NAME(fcn)))
        {
          int id;

          /*
          ** irq(n) modifier
          */
          if (list_length(fcnargs) != 1)
          {
            result = FALSE;
            break;
          }
          v = TREE_VALUE(fcnargs);
          if (TREE_CODE(v) != INTEGER_CST)
          {
            error("interrupt vector is not a constant");
            result = FALSE;
            break;
          }
          id = TREE_INT_CST_LOW(v);
          if ((id < MIN_IRQ_ID) || (id > MAX_IRQ_ID))
          {
             error("interrupt vector number %d is not valid", id);
             result = FALSE;
             break;
          }
          validate_name = 0;
        }
        else if (IDENT_ALTIRQ(DECL_NAME(fcn)))
        {
          int id;

          /*
          ** altirq(n) modifier
          */
          if (list_length(fcnargs) != 1)
          {
             result = FALSE;
             break;
          }
          v = TREE_VALUE(fcnargs);
          if (TREE_CODE(v) != INTEGER_CST)
          {
            error("alternate interrupt vector is not a constant");
            result = FALSE;
            break;
          }
          id = TREE_INT_CST_LOW(v);
          if ((id < MIN_IRQ_ID) || (id > MAX_IRQ_ID))
          {
            error("alternate interrupt vector number %d is not valid", id);
            result = FALSE;
            break;
          }
          validate_name = 0;
        }
        else if (IDENT_SAVE(DECL_NAME(fcn)))
        {
          if (!pic30_valid_machine_decl_save(fcnargs))
          {
            result = FALSE;
            break;
          }
        }
        else if (IDENT_PREPROLOGUE(DECL_NAME(fcn)))
        {
          if (!pic30_valid_machine_decl_preprologue(fcnargs))
          {
             result = FALSE;
             break;
          }
        }
        else
        {
          error("interrupt modifier '%s' unknown", 
                IDENTIFIER_POINTER(DECL_NAME(fcn)));
          result = FALSE;
          break;
        }
        break;

      default:
        error("interrupt modifier syntax error");
        result = FALSE;
        break;
    }   
  }
  if (validate_name) {
    /* match on _AltInterruptnnn or _Interruptnnn */
    if (strncmp(attached_to, "_AltInterrupt", sizeof("_AltInterrupt")-1) == 0) {
      char *endptr;

      strtol(attached_to + sizeof("_AltInterrupt")-1, &endptr, 0);
      if (*endptr == 0) return result;
    } else if (strncmp(attached_to, "_Interrupt", sizeof("_Interrupt")-1) == 0){
      char *endptr;

      strtol(attached_to + sizeof("_Interrupt")-1, &endptr, 0);
      if (*endptr == 0) return result;
    }
    if (valid_isr_names_cnt) {
      if (bsearch(attached_to, valid_isr_names, valid_isr_names_cnt,
                 sizeof (struct isr_info), pic30_bsearch_isr_compare) == 0)
      warning("'%s' is not a valid interrupt vector name for %s", attached_to,
              pic30_target_cpu_id);
    } else {
      static int message_printed = 0;

      if (!message_printed) {
        if (pic30_target_cpu_id) {
          warning("No interrupt vector names defined for %s",
                  pic30_target_cpu_id);
        } else {
          warning("No target selected, cannot validate vector name");
        }
        message_printed=1;
      }
    }
  }
  return result;
}
/*
** Return nonzero if IDENTIFIER is a valid attribute.
*/

static tree 
pic30_valid_machine_attribute(decl, identifier, args, flags, no_add_attrs)
     tree *decl;
     tree identifier;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{ const char *pszIdent;
  const char *attached_to = 0;
  
  pic30_init_idents();
  pszIdent = IDENTIFIER_POINTER(identifier);
  if (DECL_P(*decl)) {
    attached_to = IDENTIFIER_POINTER(DECL_NAME(*decl));
  }

  if (DECL_P(*decl)) 
  {
    return pic30_valid_machine_decl_attribute(decl, identifier, args, 
                                              no_add_attrs, attached_to);
  }
  else if (TYPE_P(*decl)) 
  {
    return pic30_valid_machine_type_attribute(decl, identifier, args,
                                              no_add_attrs);
  }
  else
  {
      error("Inappropriately applied attribute '%s'", pszIdent);
  }
  return NULL_TREE;
}

static tree 
pic30_valid_machine_decl_attribute(node, identifier, args, no_add_attrs,  
                    attached_to)
    tree *node;
    tree identifier;
    tree args;
    bool *no_add_attrs;
    const char *attached_to;
{
    tree decl = *node;
    const char *pszIdent;

    pszIdent = IDENTIFIER_POINTER(identifier);

    /*
    ** Check for near/far attributes.
    */
    if (IDENT_NEAR(identifier))
    {
        if (pic30_far_decl_p(decl))
        {
            error("cannot specify both near and far attributes");
            *no_add_attrs = 1;
        }
        return NULL_TREE;
    }
    if (IDENT_FAR(identifier))
    {
        if (pic30_near_decl_p(decl))
        {
            error("cannot specify both near and far attributes");
            *no_add_attrs = 1;
            return NULL_TREE;
        }
        return NULL_TREE;
    }
    /*
    ** Check for address space attributes.
    */
    if (IDENT_SPACE(identifier))
    {  tree space;

      space = lookup_attribute(IDENTIFIER_POINTER(pic30_identSpace[0]),
                               DECL_ATTRIBUTES(decl));

      if (space) {
        warning("ignoring previous space attribute");
      }
      if (IDENT_PROG(TREE_VALUE(args)) || IDENT_DATA(TREE_VALUE(args)) ||
          IDENT_XMEMORY(TREE_VALUE(args)) || IDENT_YMEMORY(TREE_VALUE(args)) ||
          IDENT_CONST(TREE_VALUE(args)) || IDENT_PSV(TREE_VALUE(args)) ||
          IDENT_EEDATA(TREE_VALUE(args)) || IDENT_DMA(TREE_VALUE(args)))
        { char space;

          if ((TARGET_ARCH(PIC33) || TARGET_ARCH(PIC24F) || 
               TARGET_ARCH(PIC24H)) && (IDENT_EEDATA(TREE_VALUE(args)))) {
            error("space(eedata) not supported on this target");
          } else if ((TARGET_ARCH(PIC24F) || TARGET_ARCH(PIC24H)) && 
                     ((space='x', IDENT_XMEMORY(TREE_VALUE(args))) || 
                      (space='y', IDENT_YMEMORY(TREE_VALUE(args))))) {
            error("space(%cmemory) not supported on this target", space);
          } else if (IDENT_DMA(TREE_VALUE(args))) {
            if (TARGET_ARCH(PIC24F) || TARGET_ARCH(PIC30)) {
              warning("space(dma) not supported on this target, ignoring");
              *no_add_attrs=1;
            }
          }
          return NULL_TREE;
        }
        else
        {
          error("invalid space argument for '%s'", attached_to);
          *no_add_attrs = 1;
          return NULL_TREE;
        }
    }
   if (IDENT_ADDRESS(identifier))
   {  tree address;
      
      address = TREE_VALUE(args);
      if (TREE_CODE(address) != INTEGER_CST) {
        error("invalid address argument for '%s'", attached_to);
        *no_add_attrs = 1;
      } else 
      /* currently the assembler will not accept an odd address */
      if (TREE_INT_CST_LOW(address) & 0x1) {
        warning("invalid address argument for '%s'", attached_to);
        warning("odd addresses are not permitted, ignoring attribute");
        *no_add_attrs = 1;
      }
      return NULL_TREE;
   }
   if (IDENT_NOLOAD(identifier))
   {
      return NULL_TREE;
   }

    /*
    ** Check for function attributes.
    */
    if (TREE_CODE(decl) == FUNCTION_DECL)   
    {
        /*
        ** Check for interrupt attributes.
        */
        if (IDENT_INTERRUPT(identifier))
        {
            *no_add_attrs = 
              (!pic30_valid_machine_decl_interrupt_attribute(
                    args, (char *)attached_to));
            return NULL_TREE;
        }
        else if (IDENT_SHADOW(identifier))
        {
            return NULL_TREE;
        }
    }
    else
    {
      /*
      ** Data attributes.
      */
      /*
      ** Check for SFR attributes.
      */
      if (IDENT_SFR(identifier))
      {
        *no_add_attrs = (!pic30_valid_machine_decl_sfr_attribute(decl,args));
        return NULL_TREE;
      }
      if (IDENT_UNORDERED(identifier)) return NULL_TREE;
      if (IDENT_MERGE(identifier)) return NULL_TREE;
      if (IDENT_PERSISTENT(identifier)) return NULL_TREE;
      if (IDENT_REVERSE(identifier)) {
        tree address;

        address = TREE_VALUE(args);
        if (TREE_CODE(address) != INTEGER_CST) {
          error("invalid reverse argument for '%s'", attached_to);
          *no_add_attrs = 1;
        }
        return NULL_TREE;
      }
    }
   if (IDENT_UNSAFE(identifier)) return NULL_TREE;

    error("invalid attribute '%s' ignored", pszIdent);

    *no_add_attrs = 1;
   return NULL_TREE;
}
/*
** Return nonzero if IDENTIFIER is a valid type attribute.
*/
static tree
pic30_valid_machine_type_attribute(node, identifier, args, no_add_attrs)
     tree *node;
     tree identifier;
     tree args;
     bool *no_add_attrs;
{    tree type = *node;
    /*
    ** Check for near/far attributes.
    */
    if (IDENT_NEAR(identifier) || IDENT_FAR(identifier))
    {
        return(NULL_TREE);
    }
    /*
    ** Check for interrupt attributes.
    */
    if (TREE_CODE(type) == FUNCTION_TYPE)   
    {
        if (IDENT_INTERRUPT(identifier))
        {
            return(NULL_TREE);
        }
    }
    /*
    ** Check for address space attributes.
    */
    if (IDENT_SPACE(identifier) &&
        (IDENT_PROG(TREE_VALUE(args)) || IDENT_DATA(TREE_VALUE(args)) ||
        IDENT_XMEMORY(TREE_VALUE(args)) || IDENT_YMEMORY(TREE_VALUE(args)) ||
        IDENT_CONST(TREE_VALUE(args)) || IDENT_PSV(TREE_VALUE(args)) ||
        IDENT_EEDATA(TREE_VALUE(args)))) {
        if (lookup_attribute(IDENTIFIER_POINTER(identifier),
                     TYPE_ATTRIBUTES(type)) == NULL_TREE)
        { char space;

          if ((TARGET_ARCH(PIC33) || TARGET_ARCH(PIC24F) || 
               TARGET_ARCH(PIC24H)) && (IDENT_EEDATA(TREE_VALUE(args)))) {
            error("space(eedata) not supported on this target");
          } else if ((TARGET_ARCH(PIC24F) || TARGET_ARCH(PIC24H)) &&
                     ((space='x', IDENT_XMEMORY(TREE_VALUE(args))) ||
                      (space='y', IDENT_YMEMORY(TREE_VALUE(args))))) {
            error("space(%cmemory) not supported on this target", space);
          } else if (IDENT_DMA(TREE_VALUE(args))) {
            if (TARGET_ARCH(PIC24F) || TARGET_ARCH(PIC30)) {
              warning("space(dma) not supported on this target");
            }
          }
          return(NULL_TREE);
        }
    }
    *no_add_attrs = 1;

   return(NULL_TREE);
}
/*
** Determine if a function can be called using a short RCALL instruction.
*/
int
pic30_near_function_p(operand)
     rtx operand;
{
    int fNear;
    rtx symbol;
    const char *pszSymbolName;

    symbol = XEXP(operand,0);
    pszSymbolName = XSTR(symbol, 0);
    fNear = SYMBOL_REF_FLAG(symbol)
        || (pic30_libcall(pszSymbolName) && TARGET_SMALL_CODE);

    return(fNear);
}
/*
** Encode section information of DECL, which is either a VAR_DECL,
** FUNCTION_DECL, STRING_CST, CONSTRUCTOR, or ???.
*/
void
pic30_encode_section_info(decl, first_seen)
  tree decl;
  int first_seen;
{
  int fNear;
  char prefix[80] = { 0 };
  const char *fn_name;
  char *f = prefix;

  if (first_seen == 0) return;
  pic30_init_idents();
  switch (TREE_CODE(decl))
  {
    case FUNCTION_DECL:   
      if (pic30_far_decl_p(decl))
      {
        fNear = 0;
      }
      else if (pic30_near_decl_p(decl))
      {
        fNear = 1;
      }
      else
      {
        fNear = TARGET_SMALL_CODE;
      }
      fn_name = IDENTIFIER_POINTER(DECL_NAME(decl));
      if (TARGET_ISR_WARN && !pic30_interrupt_function_p(decl) &&
          bsearch(fn_name, valid_isr_names, valid_isr_names_cnt,
                  sizeof (struct isr_info), pic30_bsearch_isr_compare))
        warning("'%s' is an interrupt vector name", fn_name);
      SYMBOL_REF_FLAG(XEXP(DECL_RTL(decl), 0)) = fNear;
      if (pic30_shadow_function_p(decl))
      {
         f += sprintf(f,PIC30_FCNS_FLAG);
      }
      else
      {
        f += sprintf(f,PIC30_FCNN_FLAG);
      }
      pic30_build_prefix(decl, -1, f);
      break;

    case VAR_DECL:
      if (TARGET_CONST_IN_CODE && TREE_READONLY(decl))
      {
        /*
        ** If this is a constant declaration,
        ** and constants are located in code space,
        ** then it cannot be a near declaration.
        */
        fNear = 0;
      }
      else if (pic30_far_decl_p(decl))
      {
        fNear = 0;
      }
      else if (pic30_near_decl_p(decl))
      {
        fNear = 1;
      }
      else
      {
        if (DECL_MODE(decl) == BLKmode)
        {
          fNear = TARGET_SMALL_AGG;
        }
        else
        {
          fNear = TARGET_SMALL_SCALAR;
        }
      }
      SYMBOL_REF_FLAG(XEXP(DECL_RTL(decl), 0)) = 
      pic30_build_prefix(decl, fNear, prefix);
      break;

    default:
        break;
  }
  if ((prefix[0] != 0) == 0) fNear = 0;
  {
    rtx rtl = (TREE_CODE_CLASS(TREE_CODE(decl)) != 'd'
                ? TREE_CST_RTL(decl) : DECL_RTL(decl));
    const char *str = XSTR(XEXP (rtl, 0), 0);
    int len = strlen(str);
    char *newstr = xmalloc(len + strlen(prefix) + 1);
    sprintf(newstr, "%s%s", prefix, str);
    XSTR(XEXP(rtl, 0), 0) = newstr;
  }
}
/*
** A C statement or statements to switch to the appropriate
** section for output of DECL.  DECL is either a `VAR_DECL' node
** or a constant of some sort.  RELOC indicates whether forming
** the initial value of DECL requires link-time relocations.
*/
void
pic30_select_section (decl, reloc, align)
     tree decl;
     int reloc;
     unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED;
{ const char *s;
  rtx rtl;
  SECTION_FLAGS_INT flags;

  if ((TREE_CODE(decl) == VAR_DECL) || (TREE_CODE(decl) == FUNCTION_DECL)) {
    rtl = (TREE_CODE_CLASS(TREE_CODE(decl)) != 'd' ? TREE_CST_RTL(decl) : 
                                                     DECL_RTL(decl));
    flags = validate_identifier_flags(XSTR(XEXP(rtl, 0), 0));

    s = default_section_name(decl, flags);
    if (flags) {
      pic30_asm_named_section(s, flags);
      pic30_no_section();
      return;
    }
  }
  if (TREE_CODE (decl) == STRING_CST)
  {
    pic30_push_pop_constant_section(0,2 /* activate */);
  }
  else if (TREE_CODE (decl) == VAR_DECL)
  { int bNear = (PIC30_SFR_NAME_P(XSTR(XEXP(DECL_RTL(decl), 0), 0)) != 0);

    if ((flag_pic && reloc) || !TREE_READONLY (decl) ||
        TREE_SIDE_EFFECTS (decl) || !DECL_INITIAL (decl) ||
        (DECL_INITIAL (decl) != error_mark_node && 
        !TREE_CONSTANT (DECL_INITIAL (decl))))
    {
      if (bNear)
      {
         ndata_section();
      }
      else
      {
        data_section();
      }
    }
    else
    {
      /*
      ** Constants
      */
      if (TARGET_CONST_IN_CODE)
      {
        const_section();
      }
      else if (bNear)
      {
        ndconst_section();
      }
      else
      {
        dconst_section();
      }
    }
  }
  else
  {
    if (TARGET_CONST_IN_CODE)
    {
      const_section();
    }
    else
    {
      dconst_section();
    }
  }
}
/************************************************************************/
/* ASM_DECLARE_FUNCTION_NAME target macro.                */
/* =======================================                */
/* Output to the stdio stream <stream> any text necessary for declaring    */
/* the name <name> of a function which is being defined. This macro is    */
/* responsible for outputting the label definition.            */
/* The argument <name> is the name of the function.            */
/* The argument <decl> is the FUNCTION_DECL tree node representing the    */
/* function.                                 */
/************************************************************************/
void
pic30_asm_declare_function_name(FILE *file, char *name, 
                                tree decl ATTRIBUTE_UNUSED)
{
    if (pic30_obj_elf_p())
    {
        fprintf(file, "%s", "\t.type\t");
        assemble_name(file, name);
        putc(',', file);
        fprintf(file, "@%s", "function");
        putc('\n', file);
    }
          ASM_OUTPUT_LABEL(file, name);
}
/************************************************************************/
/* ASM_DECLARE_OBJECT_NAME target macro.                */
/* =====================================                */
/* Output to the stdio stream <stream> any text necessary for declaring    */
/* the name <name> of an initialized variable which is being defined.    */
/* This macro is responsible for outputting the label definition.    */
/* The argument <decl> is the VAR_DECL tree node representing the    */
/* variable.                                 */
/************************************************************************/
void
pic30_asm_declare_object_name(FILE *file, char *name, 
                              tree decl ATTRIBUTE_UNUSED)
{
    if (pic30_obj_elf_p())
    {
        fprintf(file, "%s", "\t.type\t");
        assemble_name(file, name);
        putc(',', file);
        fprintf(file, "@%s", "object");
        putc('\n', file);
    }
          ASM_OUTPUT_LABEL(file, name);
}
/************************************************************************/
/* TARGET_ASM_NAMED_SECTION target hook.                */
/* =====================================                */
/* Output assembly directives to switch to section pszSectionName.    */
/* The section name will have any user-specifed flags appended.        */
/* The section should have attributes as specified by flags, which is a    */
/* bit mask of the SECTION_* flags defined in output.h.         */
/************************************************************************/
static void
pic30_asm_named_section(const char *pszSectionName, SECTION_FLAGS_INT flags)
{
   if (set_section_stack(pszSectionName, flags) == 0) return;
   pic30_merged_asm_named_section(pszSectionName, flags);
}

static void
pic30_merged_asm_named_section(const char *pszSectionName, 
                               SECTION_FLAGS_INT flags)
{
  char pszSectionFlag[80] = "invalid section flag";
  char *f;

  if (pszSectionName == 0) return;
  f = pszSectionFlag;
  lfInExecutableSection = FALSE;
  if (flags & SECTION_BSS) {
    f += sprintf(f, "," SECTION_ATTR_BSS);
  }
  if (flags & SECTION_WRITE) {
    f += sprintf(f, "," SECTION_ATTR_DATA);
  }
  if (flags & SECTION_PSV) {
    f += sprintf(f, "," SECTION_ATTR_PSV);
  }
  if (flags & SECTION_CODE) {
    f += sprintf(f, "," SECTION_ATTR_CODE);
    lfInExecutableSection = TRUE;
  }
  if (flags & SECTION_READ_ONLY) {
    f += sprintf(f, "," SECTION_ATTR_CONST);
  }
  if (flags & SECTION_XMEMORY) {
    f += sprintf(f, "," SECTION_ATTR_XMEMORY);
  }
  if (flags & SECTION_YMEMORY) {
    f += sprintf(f, "," SECTION_ATTR_YMEMORY);
  }
  if (flags & SECTION_NEAR) {
    f += sprintf(f, "," SECTION_ATTR_NEAR);
  }
  if (flags & SECTION_PERSIST) {
    f += sprintf(f, "," SECTION_ATTR_PERSIST);
  }
  if (flags & SECTION_EEDATA) {
    f += sprintf(f, "," SECTION_ATTR_EEDATA);
  }
  if (flags & SECTION_NOLOAD) {
    f += sprintf(f, "," SECTION_ATTR_NOLOAD);
  }
  if (flags & SECTION_MERGE) {
    f += sprintf(f, "," SECTION_ATTR_MERGE);
  }
  if (flags & SECTION_DEBUG) {
    f += sprintf(f, "," SECTION_ATTR_INFO);
  }
  if (flags & SECTION_DMA) {
    f += sprintf(f, "," SECTION_ATTR_DMA);
  }
  fprintf(asm_out_file, "\t.section\t%s%s\n", pszSectionName, pszSectionFlag);
}

#if 0
static void
pic30_coff_asm_named_section(const char *pszSectionName, unsigned int flags)
{
    char *pFlag;
    const char *pszSectionFlag = "invalid section flag";

    if (pszSectionName)
    {
        pFlag = strchr(pszSectionName, ',');
        if (pFlag)
        {
            pFlag[0] = '\0';
        }
    }
    else
    {
        pFlag = 0;
    }
        if (flags == SECTION_BSS)
    {
        pszSectionFlag = SECTION_FLAG_BSS;
    }
        else if (flags == SECTION_WRITE)
    {
        pszSectionFlag = SECTION_FLAG_DATA;
    }
    else if ((flags == SECTION_CODE) && 
         (strcmp(pszSectionName,".const") == 0)) 
    {
        pszSectionFlag = SECTION_FLAG_READONLY;
    }
    else if (flags == SECTION_CODE)
    {
        pszSectionFlag = SECTION_FLAG_EXEC;
    }
    else if (flags == SECTION_READ_ONLY)
    {
        pszSectionFlag = SECTION_FLAG_READONLY;
    }
    if (pszSectionName == NULL)
    {
    }
    else if (strcmp(pszSectionName, ".text") == 0)
    {
        fprintf(asm_out_file, "\t.text\n");
    }
    else if (strcmp(pszSectionName, SECTION_NAME_DATA) == 0)
    {
        fprintf(asm_out_file, "\t.data\n");
    }
    else
    {
        fprintf(asm_out_file, "\t.section\t%s,\"%s\"\n",
                        pszSectionName, pszSectionFlag);
    }
    if (pszSectionFlag == NULL)
    {
        lfInExecutableSection = FALSE;
    }
    else
    {
        lfInExecutableSection =
            (strcmp(pszSectionFlag, SECTION_FLAG_EXEC) == 0);
    }
    if (pFlag)
    {
        pFlag[0] = ',';
    }
}

static void
pic30_elf_asm_named_section(const char *pszSectionName, unsigned int flags)
{
    char *pFlag;
    char flagchars[10], *f = flagchars;

    lfInExecutableSection = FALSE;
    if (!(flags & SECTION_DEBUG))
    {
        *f++ = 'a';
    }
    if (flags & (SECTION_WRITE|SECTION_BSS))
    {
        *f++ = 'w';
    }
    if (flags & SECTION_CODE)
    {
        *f++ = 'x';
        lfInExecutableSection = TRUE;
    }
    if (flags & SECTION_SMALL)
    {
        *f++ = 's';
    }
    if (flags & SECTION_MERGE)
    {
        *f++ = 'M';
    }
    if (flags & SECTION_STRINGS)
    {
        *f++ = 'S';
    }
    if (flags & SECTION_TLS)
    {
        *f++ = 'T';
    }
    *f = '\0';

    if (pszSectionName == NULL)
    {
        return;
    }
    if ((strcmp(pszSectionName, ".text") == 0) ||
        (strcmp(pszSectionName, ".data") == 0) ||
        (strcmp(pszSectionName, ".bss") == 0) )
    {
        fprintf(asm_out_file, "\t.section\t%s\n", pszSectionName);
        return;
    }
    /*
    ** Check for old-style section names
    */
    pFlag = strchr(pszSectionName, ',');
    if (pFlag)
    {
        /*
        ** Old-style section flag specified
        */
        pFlag[0] = '\0';
        f = flagchars;
        switch (pFlag[1])
        {
        case 'r':
            /*
            ** PSV section
            */
            *f++ = 'w';
            *f++ = 'a';
            *f++ = 'x';
            break;
        default:
            break;
        }
        *f = '\0';
    }
    fprintf(asm_out_file, "\t.section\t%s,\"%s\"", pszSectionName, flagchars);
        
    if (!(flags & SECTION_NOTYPE))
    {
      const char *type;
        
      if (flags & SECTION_BSS)
      {
        type = "nobits";
      }
      else
      {
        type = "progbits";
      }
      fprintf(asm_out_file, ",@%s", type);
      if (flags & SECTION_ENTSIZE)
      {
        fprintf(asm_out_file, ",%d", flags & SECTION_ENTSIZE);
      }
    }
    putc('\n', asm_out_file);
}
#endif

/************************************************************************/
/* Save the current section name.                    */
/************************************************************************/
static int set_section_stack(const char *pszSectionName, 
                             SECTION_FLAGS_INT pszSectionFlag) {

   if (!lSectionStack) {
     if (freeSectionStack) {
       lSectionStack = freeSectionStack;
       freeSectionStack = freeSectionStack->pop;
     } else lSectionStack =  xcalloc(sizeof(sectionStack),1);
     lSectionStack->pop = 0;
   } else if ((lSectionStack->pszFlag == pszSectionFlag) &&
              (pszSectionName[0] != '*') &&
              (strcmp(lSectionStack->pszName, pszSectionName) == 0)) {
     if (lSectionStack->flags & ss_should_pop) lSectionStack->flags = ss_set;
     return 0;
   } else if (lSectionStack->flags & ss_should_pop) {
     sectionStack *s; 

     s = lSectionStack;
     lSectionStack = s->pop;
     s->pop = freeSectionStack;
     freeSectionStack = s;
   }
   lSectionStack->pszName = xstrdup(pszSectionName);
   lSectionStack->pszFlag = pszSectionFlag;
   lSectionStack->flags |= ss_set;
   return 1;
}

static void
pic30_push_section_name(const char *pszSectionName, 
                        SECTION_FLAGS_INT pszSectionFlag)
{  sectionStack *s;

   if (lSectionStack->flags & ss_should_pop) {
     if ((lSectionStack->pszFlag == pszSectionFlag) &&
         (pszSectionName[0] != '*') &&
         (strcmp(lSectionStack->pszName, pszSectionName) == 0)) {
        /* pushing the section just popped */
       lSectionStack->flags &= ~ss_should_pop;
       return;
     } else s = lSectionStack;
   } else if (freeSectionStack) {
     s = freeSectionStack;
     freeSectionStack = s->pop;
   } else {
     s = (sectionStack *) xcalloc(sizeof(sectionStack),1);
   }
   if (s != lSectionStack) s->pop = lSectionStack;
   s->pszName = pszSectionName;
   s->pszFlag = pszSectionFlag;
   s->flags = 0;
   lSectionStack = s;
   pic30_merged_asm_named_section(pszSectionName, pszSectionFlag);
}

static void pic30_push_pop_constant_section(tree decl, int push) {
  struct decl_stack {
    tree decl;
    struct decl_stack *next;
    int activated;
  };
  static struct decl_stack *my_decl_stack = 0;
  int activated = 0;

  if (push == 1) {
    struct decl_stack *p;

    p = xcalloc(sizeof(struct decl_stack),1);
    p->next = my_decl_stack;
    p->decl = decl;
    my_decl_stack = p;
  } else if (push == 2 /*activate */) {
    /* this can happen iff we are creating code to initialize */
    decl = my_decl_stack ? my_decl_stack->decl : 0;
  } else if (push == 0 /*pop */) {
    if (my_decl_stack == 0) abort();
    if (decl != my_decl_stack->decl) abort();
    activated = my_decl_stack->activated;
    my_decl_stack = my_decl_stack->next;
  }
  if (push == 1) return;
  if (decl && DECL_SECTION_NAME(decl)) {
    SECTION_FLAGS_INT flags;
    const char *name = TREE_STRING_POINTER(DECL_SECTION_NAME(decl));

    flags = pic30_section_type_flags(decl, name, 1);
    if (flags & SECTION_PSV) {
      if (push == 2) {
        pic30_push_section_name(name, flags);
        my_decl_stack->activated = 1;
      } else if ((push == 0) && (activated))  pic30_pop_section_name();
      return;
    }
  }
  if (push == 2) {
    if (!flag_writable_strings) {
      if (TARGET_CONST_IN_CODE) const_section();
      else dconst_section();
    } else data_section();
  }
}

int pic30_pushed_constant_section() {
  return (lSectionStack->pszFlag & SECTION_PSV);
}

void pic30_push_constant_section(tree decl) {
  pic30_push_pop_constant_section(decl,1);
}

void pic30_pop_constant_section(tree decl) {
  pic30_push_pop_constant_section(decl,0);
}

/************************************************************************/
/* Restore a saved section name.                    */
/************************************************************************/
static void
pic30_pop_section_name(void)
{
   if (lSectionStack->flags & ss_set) {
     /* popping back to a sectionStack item that was set using
        set_section_stack() as a result of a named section ... convert it
        back to the default section */
     lSectionStack->pszName = default_section.pszName;
     lSectionStack->pszFlag = default_section.pszFlag;
     lSectionStack->flags = default_section.flags;
     pic30_merged_asm_named_section(lSectionStack->pszName,
                                    lSectionStack->pszFlag);
   } else lSectionStack->flags |= ss_should_pop;
}

/************************************************************************/
/* A C statement to be executed just prior to the output of        */
/* assembler code for INSN, to modify the extracted operands so        */
/* they will be output differently.                    */
/*                                    */
/* Here the argument OPVEC is the vector containing the operands    */
/* extracted from INSN, and NOPERANDS is the number of elements of    */
/* the vector which contain meaningful data for this insn.        */
/* The contents of this vector are what will be used to convert the    */
/* insn template into assembler code, so you can change the assembler    */
/* output by changing the contents of the vector.            */
/*                                    */
/* This function detect RAW hazards, and inserts NOPs to prevent the    */
/* hazard. This is done to work around an errror in the REV A silicon.    */
/*                                    */
/************************************************************************/
static unsigned int
pic30_reganydef(rtx op)
{
    unsigned regdef = 0;
    unsigned regmask = 0;
    rtx rtxInner;

    switch (GET_CODE(op))
    {
    case SUBREG:
        if (!register_operand(op, Pmode))
        {
            break;
        }
    case REG:
        switch (GET_MODE(op))
        {
        case QImode:
        case HImode:
            regmask = 0x1u;
            break;
        case SImode:
        case SFmode:
            regmask = 0x3u;
            break;
        case DImode:
        case DFmode:
            regmask = 0xFu;
            break;
        default:
            regmask = 0xFFFFu;
            break;
        }
        regdef |= regmask << REGNO(op);
        break;
    case MEM:
        rtxInner = XEXP(op, 0);
        switch (GET_MODE(op))
        {
        case DImode:
        case DFmode:
            switch (GET_CODE(rtxInner))
            {
            case REG:
                regmask |= 1u << REGNO(rtxInner);
                break;
            default:
                break;
            }
            break;
        default:
            break;
        }
        switch (GET_CODE(rtxInner))
        {
        case PRE_DEC:
        case PRE_INC:
        case POST_DEC:
        case POST_INC:
            rtxInner = XEXP(rtxInner, 0);
            regmask |= 1u << REGNO(rtxInner);
            break;
        default:
            break;
        }
        regdef |= regmask;
        break;
    default:
        break;
    }
    return(regdef);
}
static unsigned int
pic30_reginduse(rtx op)
{
    rtx rtxInner;
    rtx rtxPlusOp0;
    rtx rtxPlusOp1;

    unsigned regmask = 0;

    switch (GET_CODE(op))
    {
    case MEM:
        rtxInner = XEXP(op, 0);
        switch (GET_CODE(rtxInner))
        {
        case PLUS:
            /*
            ** Base with index.
            */
              rtxPlusOp0 = XEXP(rtxInner, 0);
              switch (GET_CODE(rtxPlusOp0))
            {
            case SUBREG:
                if (!register_operand(rtxPlusOp0, Pmode))
                {
                    break;
                }
                /*
                ** Fall thru
                */
            case REG:
                regmask |= 1u << REGNO(rtxPlusOp0);
                rtxPlusOp1 = XEXP(rtxInner, 1);
                switch (GET_CODE(rtxPlusOp1))
                {
                case SUBREG:
                    if (!register_operand(rtxPlusOp1,Pmode))
                    {
                        break;
                    }
                case REG:
                    regmask |= 1u << REGNO(rtxPlusOp1);
                    break;
                default:
                    break;
                }
                break;
            default:
                break;
            }
            break;
        case SUBREG:
            if (!register_operand(rtxInner, Pmode))
            {
                break;
            }
        case REG:
            regmask |= 1u << REGNO(rtxInner);
            break;
        case PRE_DEC:
        case PRE_INC:
        case POST_DEC:
        case POST_INC:
            rtxInner = XEXP(rtxInner, 0);
            regmask |= 1u << REGNO(rtxInner);
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
    return(regmask);
}

void
pic30_final_prescan_insn(rtx insn ATTRIBUTE_UNUSED, rtx *opvec, int noperands)
{
    int idop;
    bool bEmitNOP = false;
    unsigned int regdefmask = 0;
    unsigned int regusemask = 0;

#if (0)
    fprintf (asm_out_file, "/*****************\n");
    print_rtl_single (asm_out_file, insn);
    fprintf (asm_out_file, "*****************/\n");
#endif
    for (idop = 0; idop < noperands; ++idop)
    {
        rtx op = opvec[idop];

        regusemask |= pic30_reginduse(op);
        regdefmask |= pic30_reganydef(op);
#if (0)
              fprintf(asm_out_file, "\n");
              print_rtl_single (asm_out_file, op);
        fprintf(asm_out_file, "def=%04x, use=%04x\n",
                regdefmask, regusemask);
#endif
    }
    if (regusemask & l_RAWregdefmask)
    {
        bEmitNOP = true;
    }
    l_RAWregdefmask = regdefmask;
#if 0
    if (bEmitNOP && TARGET_ARCH(REV_A2))
    {
        fprintf(asm_out_file, "\tnop");
        if (flag_verbose_asm)
        {
            fprintf(asm_out_file, "\t\t; inhibit RAW hazard");
        }
        fprintf(asm_out_file, "\n");
    }
#endif
}

/************************************************************************/
/*
** This is how to output an assembler line that says to advance the
** location counter to a multiple of 2**LOG bytes.
*/
/************************************************************************/
void
pic30_asm_output_align(FILE *file, int log)
{
    if (log != 0)
    {
        int n = 1 << log;
    
        if (lfInExecutableSection)
        {
            if (n < 2)
            {
                n = 2;
            }
        }
        fprintf(file, "\t.align\t%d\n", n);
    }
}

/************************************************************************/
/*
** ASM_OUTPUT_COMMON target macro.
** ===============================
** A C statement (sans semicolon) to output to the stdio stream FILE the
** assembler definition of a common-label named NAME whose size is SIZE bytes.
** The variable ROUNDED is the size rounded up to whatever alignment the caller
** wants. Use the expression assemble_name (FILE, NAME) to output the name
** itself; before and after that, output the additional assembler syntax for
** defining the name, and a newline. This macro controls how the assembler
** definitions of uninitialized common global variables are output.
**
**
** NB: this function will *not* be used if there is a definition for 
**     ASM_OUTPUT_ALIGNED_DECL_COMMON - which we provide.  see varasm.c
**     where ASM_EMIT_COMMON is defined
*/
/************************************************************************/
void
pic30_asm_output_common(FILE *file, char *name,
            int size ATTRIBUTE_UNUSED, int rounded)
{
    const char *pszSectionName;

   if (target_flags & TARGET_MASK_OLD_OMF) {
      pszSectionName = PIC30_SFR_NAME_P(name)    ? SECTION_NAME_NBSS
                          : SECTION_NAME_BSS;
      pic30_push_section_name(pszSectionName, SECTION_BSS);
   } else {
     int flags = validate_identifier_flags(name);
    
     /* this is a BSS section */
     flags &= ~SECTION_WRITE;
     pszSectionName = default_section_name(0, SECTION_BSS | flags);
     pic30_push_section_name(pszSectionName, SECTION_BSS | flags);
   }
    if (pic30_obj_elf_p())
    {
        fprintf(file, "%s", "\t.type\t");
        assemble_name(file, name);
        putc(',', file);
        fprintf(file, "@%s", "object");
        putc('\n', file);
    }
    fputs("\t.global\t", file);
    assemble_name(file, name);
    fputs("\n", file);
    assemble_name(file, name);
    fputs(":\t.space\t", file);
    fprintf(file, "%u\n", rounded);
    pic30_pop_section_name();
}
/************************************************************************/
/*
** Like ASM_OUTPUT_COMMON except takes the required alignment as a separate,
** explicit argument. If you define this macro, it is used in place of
** ASM_OUTPUT_COMMON, and gives you more flexibility in handling the required
** alignment of the variable. The alignment is specified as the number of bits.
**
** For dsPIC30, this function is used to allocate 8-bit variables, since
** ASM_OUTPUT_COMMON always rounds up based on BIGGEST_ALIGNMENT.
*/
/************************************************************************/
void
pic30_asm_output_aligned_common(FILE *file, tree decl, char *name, int size, 
                                int alignment)
{
   const char *pszSectionName;
   int reverse_aligned = 0;

   if (target_flags & TARGET_MASK_OLD_OMF) {
     pszSectionName = PIC30_SFR_NAME_P(name) ? SECTION_NAME_NBSS
                                             : SECTION_NAME_BSS;
     pic30_push_section_name(pszSectionName, SECTION_BSS);
   } else {
     SECTION_FLAGS_INT flags = validate_identifier_flags(name);
     
     /* if this is a data sectino, this is now a BSS section */
     if (flags & SECTION_WRITE) {
       flags &= ~SECTION_WRITE;
       flags |= SECTION_BSS;
     }
     pszSectionName = default_section_name(decl, flags);
     pic30_push_section_name(pszSectionName, flags);
     reverse_aligned = flags & SECTION_REVERSE;
   }
   if (pic30_obj_elf_p())
   {
     fprintf(file, "%s", "\t.type\t");
     assemble_name(file, name);
     putc(',', file);
     fprintf(file, "@%s", "object");
     putc('\n', file);
   }
   fputs("\t.global\t", file);
   assemble_name(file, name);
   fputs("\n", file);
   if ((alignment > BITS_PER_UNIT) && (!reverse_aligned))
   {
     fprintf(file, "\t.align\t%d\n", alignment / BITS_PER_UNIT);
   }
   assemble_name(file, name);
   fputs(":\t.space\t", file);
   fprintf(file, "%u\n", size);
   if (decl) {
     /* on pic30, we generate a common as a definition, so it is possible
        to mark the symbol weak.  do it here, because later in finish_weak
        we will ignore unused symbols ... */
     if (DECL_WEAK(decl)) {
       fputs("\t.weak\t",file);
       assemble_name(file,name);
       fputs("\n", file);
     }
   }
   pic30_pop_section_name();
}
/************************************************************************/
/*
** A C statement (sans semicolon) to output to the stdio stream FILE the
** assembler definition of a local-common-label named NAME whose size is SIZE
** bytes. The variable ROUNDED is the size rounded up to whatever alignment the
** caller wants. Use the expression assemble_name (FILE, NAME) to output the
** name itself; before and after that, output the additional assembler syntax
** for defining the name, and a newline. This macro controls how the assembler
** definitions of uninitialized static variables are output.
*/
/************************************************************************/
void
pic30_asm_output_local(FILE *file, char *name,
            int size ATTRIBUTE_UNUSED, int rounded)
{
   const char *pszSectionName;

   if (target_flags & TARGET_MASK_OLD_OMF) {
     pszSectionName = PIC30_SFR_NAME_P(name) ? SECTION_NAME_NBSS
                                             : SECTION_NAME_BSS;
     pic30_push_section_name(pszSectionName, SECTION_BSS);
   } else {
     SECTION_FLAGS_INT flags = validate_identifier_flags(name);
     
     /* if this is a data sectino, this is now a BSS section */
     if (flags & SECTION_WRITE) {
       flags &= ~SECTION_WRITE;
       flags |= SECTION_BSS;
     }
     pszSectionName = default_section_name(0, flags);
     pic30_push_section_name(pszSectionName, flags);
   }
   assemble_name(file, name);
   fputs(":\t.space\t", file);
   fprintf(file, "%u\n", rounded);
   pic30_pop_section_name();
}
/************************************************************************/
/*
** Like ASM_OUTPUT_LOCAL except takes the required alignment as a separate,
** explicit argument. If you define this macro, it is used in place of
** ASM_OUTPUT_LOCAL, and gives you more flexibility in handling the required
** alignment of the variable. The alignment is specified as the number of bits.
**
** NB: this function is used in preference to pic30_asm_output_local
*/
/************************************************************************/
void
pic30_asm_output_aligned_decl_local(FILE *file, tree decl, char *name, 
                                    int size, int alignment)
{
   const char *pszSectionName;
   int reverse_aligned = 0;

   if (target_flags & TARGET_MASK_OLD_OMF) {
     pszSectionName = PIC30_SFR_NAME_P(name) ? SECTION_NAME_NBSS
                                             : SECTION_NAME_BSS;
     pic30_push_section_name(pszSectionName, SECTION_BSS);
   } else {
     SECTION_FLAGS_INT flags = validate_identifier_flags(name);
     
     /* if this is a data sectino, this is now a BSS section */
     if (flags & SECTION_WRITE) {
       flags &= ~SECTION_WRITE;
       flags |= SECTION_BSS;
     }
     pszSectionName = default_section_name(decl, flags);
     pic30_push_section_name(pszSectionName, flags);
     reverse_aligned = flags & SECTION_REVERSE;
   }
   if ((alignment > BITS_PER_UNIT) && !reverse_aligned)
   {
     fprintf(file, "\t.align\t%d\n", alignment / BITS_PER_UNIT);
   }
   assemble_name(file, name);
   fputs(":\t.space\t", file);
   fprintf(file, "%u\n", size);
   pic30_pop_section_name();
}
/************************************************************************/
/*
** pic30_text_section_asm_op()
** A C expression whose value is a string, including spacing, containing
** the assembler operation that should precede instructions and read-only
** data. Normally "\t.text" is right.
*/
/************************************************************************/
char *
pic30_text_section_asm_op()
{
   static char *pszSection;

   set_section_stack(pic30_text_scn ? pic30_text_scn : ".text", SECTION_CODE);
    lfInExecutableSection = TRUE;
    if (pic30_text_scn)
    {
        if (pszSection == NULL)
        {
            pszSection = (char *)
                    xmalloc(strlen(pic30_text_scn)+32);
        }
      if (target_flags & TARGET_MASK_OLD_OMF) {
          if (pic30_obj_elf_p())
          {
              sprintf(pszSection, "\t.section\t%s,\"ax\",@progbits",
                          lSectionStack->pszName);
          }
          else
          {
              sprintf(pszSection, "\t.section\t%s,\"%s\"",
                          lSectionStack->pszName,
                          SECTION_FLAG_EXEC);
          }
      } else sprintf(pszSection, "\t.section\t%s,%s", lSectionStack->pszName,
                                                      SECTION_ATTR_CODE);
        return(pszSection);
    }
    else
    {
        return((char *)"\t.text");
    }
}
/************************************************************************/
/*
** pic30_data_section_asm_op()
** A C expression whose value is a string, including spacing, containing
** the assembler operation to identify the following data as writable
** initialized data. Normally "\t.data" is right.
*/
/************************************************************************/
char *
pic30_data_section_asm_op()
{
   set_section_stack(SECTION_NAME_DATA,SECTION_WRITE);
    lfInExecutableSection = FALSE;

    return((char *)"\t.data");
}
/************************************************************************/
/*
** pic30_ndata_section_asm_op()
*/
/************************************************************************/
char *
pic30_ndata_section_asm_op()
{
static char szSection[32];

    lfInExecutableSection = FALSE;
   if (target_flags & TARGET_MASK_OLD_OMF) {
     set_section_stack(SECTION_NAME_NDATA, SECTION_WRITE);
      if (pic30_obj_elf_p())
      {
          sprintf(szSection, "\t.section\t%s,\"aw\",@progbits",
                     lSectionStack->pszName);
      }
      else
      {
          sprintf(szSection, "\t.section\t%s,\"%s\"",
                lSectionStack->pszName, SECTION_FLAG_DATA);
      }
   } else {
     sprintf(szSection, "\t.section\t%s,%s,%s",lSectionStack->pszName,
                  SECTION_ATTR_NEAR, SECTION_ATTR_DATA);
     set_section_stack(SECTION_NAME_NDATA, SECTION_WRITE | SECTION_NEAR);
   }
    return(szSection);
}
/************************************************************************/
/*
** pic30_const_section_asm_op()
**
** Constants go in the code/data window, hence the .const section
** is marked as executable or data (depending on the command-line),
** so that the assembler knows the word width.
*/
/************************************************************************/
char *
pic30_const_section_asm_op()
{
static char szSection[32];

    lfInExecutableSection = TRUE;

   if (target_flags & TARGET_MASK_OLD_OMF) {
     set_section_stack(SECTION_NAME_CONST, SECTION_CODE);
      if (pic30_obj_elf_p()) {
       sprintf(szSection, "\t.section\t%s,\"awx\",@progbits",
               lSectionStack->pszName);
     } else {
       sprintf(szSection, "\t.section\t%s,\"%s\"",
               lSectionStack->pszName, SECTION_FLAG_READONLY);
     }
    } else {
     set_section_stack(SECTION_NAME_CONST, SECTION_READ_ONLY);
     sprintf(szSection, "\t.section\t%s,%s", lSectionStack->pszName,
             SECTION_ATTR_CONST);
   }
    return(szSection);
}
/************************************************************************/
/*
** pic30_dconst_section_asm_op()
**
** Constants go in the code/data window, hence the .const section
** is marked as executable or data (depending on the command-line),
** so that the assembler knows the word width.
*/
/************************************************************************/
char *
pic30_dconst_section_asm_op()
{
static char szSection[32];

    lfInExecutableSection = FALSE;

   set_section_stack(SECTION_NAME_DCONST, SECTION_WRITE);
   if (target_flags & TARGET_MASK_OLD_OMF) {
     if (pic30_obj_elf_p()) {
       sprintf(szSection, "\t.section\t%s,\"aw\",@progbits",
               lSectionStack->pszName);
     } else {
       sprintf(szSection, "\t.section\t%s,\"%s\"",
               lSectionStack->pszName, SECTION_FLAG_DATA);
     }
    } else {
     sprintf(szSection, "\t.section\t%s,%s", SECTION_NAME_DCONST, 
                                             SECTION_ATTR_DATA);
   }
    return(szSection);
}
/************************************************************************/
/*
** pic30_ndconst_section_asm_op()
**
** Constants go in the code/data window, hence the .const section
** is marked as executable or data (depending on the command-line),
** so that the assembler knows the word width.
*/
/************************************************************************/
char *
pic30_ndconst_section_asm_op()
{
   static char szSection[32];

    lfInExecutableSection = FALSE;
   if (target_flags & TARGET_MASK_OLD_OMF) {
     set_section_stack(SECTION_NAME_NDCONST, SECTION_WRITE);
     if (pic30_obj_elf_p()) {
       sprintf(szSection, "\t.section\t%s,\"aw\",@progbits",
                    lSectionStack->pszName);
     } else {
       sprintf(szSection, "\t.section\t%s,\"%s\"",
               lSectionStack->pszName, SECTION_FLAG_DATA);
      }
   } else {
     set_section_stack(SECTION_NAME_NDCONST, SECTION_WRITE | SECTION_NEAR);
     sprintf(szSection, "\t.section %s, %s, %s", lSectionStack->pszName,
             SECTION_ATTR_DATA, SECTION_ATTR_NEAR);
   }
   return(szSection);
}

/*
** unsigned int
** TARGET_SECTION_TYPE_FLAGS(tree decl, const char *name, int reloc);
**
** A target hook to choose a set of section attributes for use by
** TARGET_ASM_NAMED_SECTION based on a variable or function decl,
** a section name, and whether or not the declaration's initializer
** may contain runtime relocations.
**
** <decl> is either a FUNCTION_DECL, a VAR_DECL or NULL_TREE.
** If <decl> is null, read-write data should be assumed.
** <reloc> indicates whether the initial value of exp requires
** link-time relocations.
*/
SECTION_FLAGS_INT
pic30_section_type_flags(decl, name, reloc)
    tree decl;
    const char *name;
    int reloc ATTRIBUTE_UNUSED;
{
  SECTION_FLAGS_INT flag = 0;
  rtx rtl;

  if (decl) {
    rtl = (TREE_CODE_CLASS(TREE_CODE(decl)) != 'd' ? 
           TREE_CST_RTL(decl) : DECL_RTL(decl));
    if (rtl && XSTR(XEXP(rtl, 0), 0))
      flag = validate_identifier_flags(XSTR(XEXP(rtl, 0), 0));
  }
  flag = validate_section_flags(name,flag);
  return(flag);
}

/*
** Define this if the label before a jump-table needs to be output specially.
** The first three arguments are the same as for ASM_OUTPUT_INTERNAL_LABEL;
** the fourth argument is the jump-table which follows (a jump_insn containing
** an addr_vec or addr_diff_vec).
*/
void
pic30_asm_output_case_label(file, prefix, num, table)
    FILE *file;
    char * prefix;
    int num;
    rtx table ATTRIBUTE_UNUSED;
{
    fprintf(file, "\t.set\t___PA___,%d\n", 0);
    ASM_OUTPUT_INTERNAL_LABEL(file, prefix, num);
}

/*
** The ASM_OUTPUT_CASE_END target description macro expands to a call
** of this function. It emits the code imediately following a jump table.
** The argument <table> is the jump-table instruction, and <num> is the
** label number of the preceding label (i.e. the jump table label).
*/
void
pic30_asm_output_case_end(file, num, table)
     FILE *file ATTRIBUTE_UNUSED;
    int num ATTRIBUTE_UNUSED;
    rtx table ATTRIBUTE_UNUSED;
{
    fprintf(file, "\t.set\t___PA___,%d\n", !pic30_asm_function_p(FALSE));
}

/*
** Output a 16-bit value.
*/
void
pic30_asm_output_short(file, value)
    FILE *file;
    rtx value;
{
    rtx rtxPlusOp0;
    rtx rtxPlusOp1;
    int fDone;
    enum rtx_code code;
    const char *pszSymbol;
    char szPrefix[32];

    fDone = FALSE;
    szPrefix[0] = 0;
    fprintf(file, "\t.word\t");
    code = GET_CODE(value);
    switch (code)
    {
    case CONST:
        if (GET_CODE(XEXP(value, 0)) == PLUS)
        {
              rtxPlusOp0 = XEXP(XEXP(value,0), 0);
              rtxPlusOp1 = XEXP(XEXP(value,0), 1);
            if (GET_CODE(rtxPlusOp1) == CONST_INT)
            {
                code = GET_CODE(rtxPlusOp0);
                switch (code)
                {
                case SYMBOL_REF:
                    pszSymbol = XSTR(rtxPlusOp0, 0);
                    if (    PIC30_PGM_NAME_P(pszSymbol) ||
                        PIC30_FCN_NAME_P(pszSymbol))
                    {
                        fprintf(file, "handle(");
                        output_addr_const(file, value);
                        fprintf(file, ")\n");
                        fDone = TRUE;
                    }
                    break;
                default:
                    break;
                }
            }
        }
        break;
    case LABEL_REF:
        fprintf(file, "handle(");
        output_addr_const(file, value);
        fprintf(file, ")\n");
        fDone = TRUE;
        break;
    case SYMBOL_REF:
        pszSymbol = XSTR(value, 0);
        if (PIC30_PGM_NAME_P(pszSymbol) || PIC30_FCN_NAME_P(pszSymbol))
        {
            fprintf(file, "handle(");
            output_addr_const(file, value);
            fprintf(file, ")\n");
            fDone = TRUE;
        }
        break;
    default:
        break;
    }
    if (!fDone)
    {
        output_addr_const(file, value);
        fprintf(file, "\n");
    }
}
/************************************************************************/
/* This is how to output an assembler line defining a string constant.  */
/************************************************************************/
void
pic30_asm_output_ascii(stream, ptr, len)
     FILE *stream;
     char *ptr;
     int len;
{
    unsigned int olen;
    unsigned char c;
    char oline[64+5];

    olen = 0;
    while (len--)
    {
        c = (unsigned char) *ptr++;
        if ((c == '"') || (c == '\\'))
        {
            /*
            ** Escape double quotes & escape.
            */
            if (olen >= (sizeof(oline)-2))
            {
                oline[olen] = 0;
                fprintf(stream, "\t.ascii\t\"%s\"\n", oline);
                olen = 0;
            }
            oline[olen++] = '\\';
            oline[olen++] = c;
        }
        else if (ISPRINT(c))
        {
            /*
            ** Just pass it through.
            */
            if (olen >= (sizeof(oline)-1))
            {
                oline[olen] = 0;
                fprintf(stream, "\t.ascii\t\"%s\"\n", oline);
                olen = 0;
            }
            oline[olen++] = c;
        }
        else if (c == 0)
        {
            /*
            ** Use .asciz
            */
            oline[olen] = 0;
            if (olen)
            {
                fprintf(stream, "\t.asciz\t\"%s\"\n", oline);
            }
            else
            {
                fprintf(stream, "\t.byte\t0\n");
            }
            olen = 0;
        }
        else
        {
            /*
            ** Octal escape
            */
            if (olen >= (sizeof(oline)-5))
            {
                oline[olen] = 0;
                fprintf(stream, "\t.ascii\t\"%s\"\n", oline);
                olen = 0;
            }
            sprintf(oline+olen, "\\%.3o", c);
            olen += 4;
        }
    }
    if (olen)
    {
        oline[olen] = 0;
        fprintf(stream, "\t.ascii\t\"%s\"\n", oline);
    }
}
/*
** output to file some assembler code to call the profiling subroutine mcount
*/
void
pic30_function_profiler(file, labelno)
     FILE *file;
     int labelno;
{
    fprintf(file, "\tpush w0\n");
    fprintf(file, "\tmov #" LOCAL_LABEL_PREFIX "P%d,w0\n", labelno);
    fprintf(file, "\tcall __mcount\n");
    fprintf(file, "\tpop w0\n");
}
rtx
pic30_return_addr_rtx(count, frameaddr)
      int count ATTRIBUTE_UNUSED;
      rtx frameaddr;
{
    rtx ret = NULL_RTX;

    if (flag_omit_frame_pointer)
    {
        error("this builtin requires a frame pointer, use -fno-omit-frame-pointer");
    }
    else
    {
           ret = gen_rtx_MEM(Pmode,
                  memory_address(Pmode,
                plus_constant(frameaddr, -3 * UNITS_PER_WORD)));
    }
    return(ret);
}
/************************************************************************/
/*  Output an assembler .file directive.                */
/************************************************************************/
static void
pic30_output_file_directive(FILE *file, const char *filename)
{
    /*
    ** Emit the .file assembler directive.
    **
    ** For compatibility with the MPLAB IDE,
    ** the full path name of the main input file is used.
    */

#if defined(__CYGWIN__)
extern void cygwin_conv_to_full_win32_path(const char *path, char *win32_path);
    char *name;
    char win32_path[MAXPATHLEN];

    cygwin_conv_to_full_win32_path(filename, win32_path);
    while ((name = strchr(win32_path, '\\')) != NULL)
    {
        name[0] = '/';
    }
    name = win32_path;
    fprintf(file, "\t.file \"%s\"\n", name);
#elif defined(__MINGW32__)
extern char *getpwd (void);
    int len;
    char *name;
    char *px;
    char *fullname = NULL;
    char *xlatname = NULL;

    name = (char *)filename;
#if 0
    /* this is supposed to check for a relative path at the start;
       on dos, full path should start with a drive letter; if it only starts
       with a '/' then it is missing the drive letter (but is otherwise a full
       path).  If it starts with anything else it is a relative path. */
    if (!((name[0] == '/') || (name[0] == '.')))
#endif
    if (name[1] != ':')
    {
        char *pwd;
        /*
        ** Relative path: prefix the pwd name.
        */
        pwd = getpwd();
        if (!pwd)
        {
            pfatal_with_name("getpwd");
        }
        if ((name[0] == '\\') || (name[0] == '/')) {
                /* partial full pathname; missing drive letter ; extract drive
                        letter from current workind directory */
                char *c;

                for (c = pwd; *c != ':'; c++);
                c++;
                *c = 0;
        }
        len = strlen(pwd) + strlen(name);
        fullname = (char *)xmalloc(len + 2);
        strcpy(fullname, pwd);
        strcat(fullname, "/");
        strcat(fullname, name);
        name = fullname;
    }
    /*
    ** translate '\' or '/' to '\\'
    */
    len = strlen(name) * 2 + 1;
    xlatname = (char *)xmalloc(len);
    for (px = xlatname; *name; ++name)
    {
        switch (*name)
        {
        case '/':
        case '\\':
         /* avoid duplicates */
            if (name[1] == *name)  name++;
            *px++ = '\\';
            *px++ = '\\';
            break;
        default:
            *px++ = *name;
            break;
        }
    }
    *px = 0;
    name = xlatname;

    /*
    ** emit the .file directive
    */
    fprintf(file, "\t.file \"%s\"\n", name);
    if (fullname)
    {
        free(fullname);
    }
    if (xlatname)
    {
        free(xlatname);
    }
#else
extern char *getpwd (void);
    char *name;
    char *fullname = NULL;

    name = (char *)filename;
    if (!((name[0] == '/') || (name[0] == '.')))
    {
        int len;
        char *pwd;
        /*
        ** Relative path: prefix the pwd name.
        */
        pwd = getpwd();
        if (!pwd)
        {
            pfatal_with_name("getpwd");
        }
        len = strlen(pwd) + strlen(name);
        fullname = (char *)xmalloc(len + 2);
        strcpy(fullname, pwd);
        strcat(fullname, "/");
        strcat(fullname, name);
        name = fullname;
    }
    fprintf(file, "\t.file \"%s\"\n", name);
    if (fullname)
    {
        free(fullname);
    }
#endif
}
/************************************************************************/
/* Output at beginning of assembler file.                             */
/************************************************************************/
void
pic30_asm_file_start(FILE *file)
{
    /*
    ** Emit the .file assembler directive for the main input file.
    **
    ** For compatibility with the MPLAB IDE,
    ** the full path name of the main input file is used.
    */
    pic30_output_file_directive(file, main_input_filename);
}
/************************************************************************/
/* Record the beginning of a new source file, named filename.          */
/************************************************************************/

/*
 *  Replacement for debug target hook for starting source file
 */
#if (!PIC30_DWARF2)
void
pic30_start_source_file(unsigned int i, const char *filename)
{    extern void sdbout_start_source_file(unsigned int, const char *);
    
    sdbout_start_source_file(i, filename);
    pic30_output_file_directive(asm_out_file, filename);
}
#endif

void
pic30_sdb_end_prologue(unsigned int i ATTRIBUTE_UNUSED) {
   extern void sdbout_end_prologue(unsigned int);

   fprintf(asm_out_file, 
           "\t.def\t___FP\n\t.val\t%d\n\t.scl\t4\n\t.type\t4\n\t.endef\n",
           frame_pointer_needed ? FP_REGNO : SP_REGNO);
}

/************************************************************************/
/* Output at end of assembler file.                      */
/************************************************************************/
void
pic30_asm_file_end(file)
     FILE *file;
{
    /*
    ** Emit SFR addresses
    */
    PSFR pSFR;

    for (pSFR = lpSFRs; pSFR; pSFR = pSFR->pNext)
    {
        fprintf(file, "\t.equ\t_%s,%d\n", pSFR->pName, pSFR->address);
    }
    fprintf(file,"\n\t.end\n");
}

static int
pic30_bsearch_compare(const void *va, const void *vb)
{
  pic30_interesting_fn *a = (pic30_interesting_fn *)va;
  pic30_interesting_fn *b = (pic30_interesting_fn *)vb;

  return strcmp(a->name, b->name);
}

static pic30_interesting_fn *pic30_match_conversion_fn(const char *name) {
  pic30_interesting_fn a,*res;
  a.name = name;

  res = bsearch(&a, pic30_fn_list, 
                sizeof(pic30_fn_list)/sizeof(pic30_interesting_fn)-1,
                sizeof(pic30_interesting_fn), pic30_bsearch_compare);
  while (res && (res != pic30_fn_list)  && (strcmp(name, res[-1].name) == 0))
    res--;
  return res;
}
  
static int mem_accesses_stack(rtx mem_access) {
  rtx x;

  x = XEXP(mem_access,0);
  switch (GET_CODE(x)) {
    case POST_INC:
       x = XEXP(x,0);
       if (REGNO(x) == SP_REGNO) return 1;
       return 0;
    default: break;
  } 
  return 0;
}

static void
conversion_info(pic30_conversion_status state,
                       pic30_interesting_fn *fn_id) {
  /* dependant upon the conversion status and the setting of the smart-io
     option, set up the pic30_fn_list table. */
 
  if (TARGET_SMART_IO == 0) {
    fn_id->function_convertable = 0;
  } else if ((TARGET_SMART_IO == 1) && (state != conv_possible)) {
    fn_id->function_convertable = 0;
  } else if ((TARGET_SMART_IO == 2) && (state == conv_not_possible)) {
    fn_id->function_convertable = 0;
  }
}

static pic30_conversion_status 
pic30_convertable_output_format_string(const char *string)
{
  const char *c = string;

  for ( ; *c; c++)
  {
    /* quickly deal with the un-interesting cases */
    if (*c != '%') continue;
    if (*(++c) == '%')
    {
      continue;
    }
    /* zero or more flags */
    while (1)
    {
      switch (*c)
      {
        case '-':
        case '+':
        case ' ':
        case '#':
        case '0': c++; continue;
        default: break;
      }
      break;
    }
    /* optional field width or * */ 
    if (*c == '*') c++; else
    while (ISDIGIT(*c)) c++; 
    /* optional precision or * */
    if (*c == '.') {
      c++;
      /* an illegal conversion sequence %.g, for example - give up and
         start looking from the g onwards */
      if (*c == '*') c++; 
      else {
        if (!ISDIGIT(*c)) {
          c--;
        }
        while(ISDIGIT(*c)) c++;
      }
    }
    /* optional conversion modifier */
    switch (*c) {
      case 'h':
      case 'l':
      case 'L': c++; break;
      default: break;
    }
    /* c should point to the conversion character */
    switch (*c) {
      case 'a':
      case 'A':
      case 'e':
      case 'E':
      case 'f':
      case 'F':
      case 'g':
      case 'G':  return conv_not_possible;
      default:   /* we aren't checking for legal format strings */
                 break;
    }
  }
  return conv_possible;
}

static pic30_conversion_status 
pic30_convertable_input_format_string(const char *string)
{
  const char *c = string;

  for ( ; *c; c++)
  {
    /* quickly deal with the un-interesting cases */
    if (*c != '%') continue;
    if (*(++c) == '%')
    {
      continue;
    }
    /* optional assignment suppression */
    if (*c == '*') c++;
    /* optional field width */ 
    while (ISDIGIT(*c)) c++; 
    /* optional conversion modifier */
    switch (*c) {
      case 'h':
      case 'l':
      case 'L': c++; break;
      default: break;
    }
    /* c should point to the conversion character */
    switch (*c) {
      case 'a':
      case 'A':
      case 'e':
      case 'E':
      case 'f':
      case 'F':
      case 'g':
      case 'G':  return conv_not_possible;
      /* string selection expr */
      case '[': {
        /* [^]...] or []...] or [...] ; get to the end of the conversion */
        c++;
        if (*c == '^') c++;
        if (*c == ']') c++;
        while (*c++ != ']');
      }
      default:   /* we aren't checking for legal format strings */
                 break;
    }
  }
  return conv_possible;
}

/*
 *   Check or set the conversion status for a particular rtl -
 *     to check the current state pass conv_state_unknown (always 0)
 *     This will create an entry if it doesn't exist or return the current
 *     state.
 */
static pic30_conversion_status 
cache_conversion_state(rtx val, int variant, pic30_conversion_status s) {
  pic30_conversion_cache *parent = 0;
  pic30_conversion_cache *save;

  save = pic30_saved_conversion_info;
  while (save && save->rtl != val) {
    parent = save;
    if ((int)val & sizeof(void *)) save = save->l; else save = save->r;
  }
  if (save) {
    /* we can only increase the current status */
    if (s > save->valid[variant]) {
      save->valid[variant] = s;
    }
    return save->valid[variant];
  }
  save = (pic30_conversion_cache *) xcalloc(sizeof(pic30_conversion_cache),1);
  save->rtl = val;
  save->valid[variant] = s;
  if (parent) {
    if ((int)val & sizeof(void *)) parent->l = save; else parent->r = save;
  } else pic30_saved_conversion_info = save;
  return s;
}

/* call-back to make sure all constant strings get seen */
void pic30_cache_conversion_state(rtx val, tree sym) {
  pic30_conversion_status s;

  s = cache_conversion_state(val, status_output, conv_state_unknown);
  if (s == conv_state_unknown) {
    if (sym && STRING_CST_CHECK(sym)) {
      const char *string = TREE_STRING_POINTER(sym);

      s = pic30_convertable_output_format_string(string);
      cache_conversion_state(val, status_output, s);
    }
  }
  s = cache_conversion_state(val, status_input, conv_state_unknown);
  if (s == conv_state_unknown) {
    if (sym && STRING_CST_CHECK(sym)) {
      const char *string = TREE_STRING_POINTER(sym);

      s = pic30_convertable_input_format_string(string);
      cache_conversion_state(val, status_input, s);
    }
  }
}

/* given an rtx representing a possible string, validate that the string is
   convertable */
static void pic30_handle_conversion(rtx val, 
                                    pic30_interesting_fn *matching_fn) {
  tree sym;
  int style;

  if (val == 0) {
    conversion_info(conv_indeterminate, matching_fn);
    return;
  }
  /* a constant string will be given a symbol name, and so will a
     symbol ... */
  sym = constant_string(val);
  if (!(sym && STRING_CST_CHECK(sym))) sym = 0;
  pic30_cache_conversion_state(val, sym);
  style = matching_fn->conversion_style == info_I ? status_input:status_output;
  conversion_info(cache_conversion_state(val, style, conv_state_unknown),
                  matching_fn);
}

static void
pic30_handle_io_conversion(rtx call_insn, 
                                  pic30_interesting_fn *matching_fn) {
  /* the info_I/O function calls are all varargs functions, with the format
     string pushed onto the stack as the anchor to the variable argument
     portion.  In short, the interesting_arg portion is not used.
     The format string is the last thing pushed onto the stack. */
  rtx format_arg;
  
  assert((matching_fn->conversion_style == info_I) ||
         (matching_fn->conversion_style == info_O));
  for (format_arg = PREV_INSN(call_insn); 
       !(NOTE_INSN_BASIC_BLOCK_P(format_arg) || 
         NOTE_INSN_FUNCTION_BEG_P(format_arg) ||
         (INSN_P(format_arg) && (GET_CODE(PATTERN(format_arg)) == CALL_INSN)));
       format_arg = PREV_INSN(format_arg)) {
    if (INSN_P(format_arg)) {
      if ((GET_CODE(PATTERN(format_arg)) == SET) && 
          (GET_CODE(XEXP(PATTERN(format_arg),0)) == MEM)) {
        /*  set (mem ) () */
        rtx mem = XEXP(PATTERN(format_arg),0);
        rtx val = XEXP(PATTERN(format_arg),1);
        rtx assignment = format_arg;

        if (mem_accesses_stack(mem)) {
          if ((GET_CODE(val) == REG) || (GET_CODE(val) == SUBREG)) {
            val = find_last_value(val, &assignment, 0, /* allow hw reg */ 1);
          } else if (GET_CODE(val) == MEM) {
            val = XEXP(val,0);
          }
          pic30_handle_conversion(val, matching_fn);
          return;
        }
      }
    }
  }
  conversion_info(conv_indeterminate, matching_fn);
}

static void
pic30_handle_io_conversion_v(rtx call_insn, 
                                    pic30_interesting_fn *matching_fn) {
  /* the info_O_v function calls are all normal functions, with the format
     string stored into a register identified by interesting_arg in the fn_list.  */
  rtx format_arg;

  assert(matching_fn->conversion_style == info_O_v);
  for (format_arg = PREV_INSN(call_insn);
       !(NOTE_INSN_BASIC_BLOCK_P(format_arg) ||
         NOTE_INSN_FUNCTION_BEG_P(format_arg)  ||
         (INSN_P(format_arg) && (GET_CODE(PATTERN(format_arg)) == CALL_INSN)));
       format_arg = PREV_INSN(format_arg)) {
    if (INSN_P(format_arg)) {
      if ((GET_CODE(PATTERN(format_arg)) == SET) &&
          (GET_CODE(XEXP(PATTERN(format_arg),0)) == REG) && 
          (REGNO(XEXP(PATTERN(format_arg),0)) == matching_fn->interesting_arg))
      {
        /*  set (reg interesting_arg) () */
        rtx val = XEXP(PATTERN(format_arg),1);
        rtx assignment = format_arg;

        if ((GET_CODE(val) == REG) || (GET_CODE(val) == SUBREG)) {
          val = find_last_value(val, &assignment, 0, /* allow hw reg */ 1);
        } else if (GET_CODE(val) == MEM) {
          val = XEXP(val,0);
        }
        pic30_handle_conversion(val, matching_fn);
        return;
      }
    }
  }
  conversion_info(conv_indeterminate, matching_fn);
}

static void
pic30_handle_dbl_conversion(pic30_interesting_fn *matching_fn) {
  /* the info_dbl function calls must be converted to the _d version if
     -fno-short-double is enabled
  */

  assert(matching_fn->conversion_style == info_dbl);
  if (flag_short_double == 1) conversion_info(conv_not_possible, matching_fn);
}

/*
 *  This function always returns true
 */
int pic30_check_for_conversion(rtx call_insn) {
  const char *name;
  const char *real_name;
  rtx fn_name;
  pic30_interesting_fn *match;

  if (GET_CODE(call_insn) != CALL_INSN) abort();
  /* (call_insn (set () (call (name) (size)))) for call returning value, and
     (call_insn (call (name) (size)))          for void call */ 
  if (GET_CODE(PATTERN(call_insn)) == SET) 
     fn_name = XEXP(XEXP(PATTERN(call_insn),1),0);
  else fn_name = XEXP(PATTERN(call_insn),0);
  if (pic30_clear_fn_list) {
    int i;
    for (i = 0; pic30_fn_list[i].name; i++) {
      pic30_fn_list[i].function_convertable=1;
    }
    pic30_clear_fn_list = 0; 
  }
  switch (GET_CODE(fn_name)) {
    default: return 1;

    case MEM: if (GET_CODE(XEXP(fn_name,0)) == SYMBOL_REF) {
                name = XSTR(XEXP(fn_name,0),0);
              } else {
                /* not calling a function directly, fn pointer or other such 
                   - give up */
                return 1;
              }
              real_name = pic30_strip_name_encoding_helper(name);
              match = pic30_match_conversion_fn(real_name);
              break;
  }
  /* function name not interesting or it is already proven to
     be not-convertable */
  while (match) {
    switch (match->conversion_style) {
      default: abort();  /* illegal conversion style */

      case info_I:    pic30_handle_io_conversion(call_insn, match); break;
      case info_O:    pic30_handle_io_conversion(call_insn, match); break;
      case info_O_v:  pic30_handle_io_conversion_v(call_insn, match); break;
      case info_dbl:  pic30_handle_dbl_conversion(match); break;
    }
    if (match[1].name &&
        (strcmp(match[1].name, real_name) == 0)) match++; else match = 0;
  }
  return 1;
}

char *pic30_default_include_path(void) {

#ifdef __WIN32__
  if (target_flags & TARGET_MASK_ARCH_PIC24F) {
    return "..\\include;..\\support\\h;..\\support\\h\\peripheral_24F";
  } else {
    return "..\\include;..\\support\\h;..\\support\\h\\peripheral_30F_24H_33F";
  }
#else
  if (target_flags & TARGET_MASK_ARCH_PIC24F) {
    return "../include:../support/h:../support/peripheral_24F";
  } else {
    return "../include:../support/h:../support/peripheral_30F_24H_33F";
  }
#endif
}

/*END********************************************************************/
