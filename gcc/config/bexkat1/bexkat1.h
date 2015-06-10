/* Target Definitions for bexkat1.
   Copyright (C) 2008-2015 Free Software Foundation, Inc.
   Contributed by Anthony Green.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_BEXKAT1_H
#define GCC_BEXKAT1_H

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "%{!mno-crt0:crt0%O%s} crti.o%s crtbegin.o%s"

/* Provide an ENDFILE_SPEC appropriate for svr4.  Here we tack on our own
   magical crtend.o file (see crtstuff.c) which provides part of the
   support for getting C++ file-scope static object constructed before
   entering `main', followed by the normal svr3/svr4 "finalizer" file,
   which is either `gcrtn.o' or `crtn.o'.  */

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

/* Provide a LIB_SPEC appropriate for svr4.  Here we tack on the default
   standard C library (unless we are building a shared library) and
   the simulator BSP code.  */

#undef LIB_SPEC
#define LIB_SPEC "%{!shared:%{!symbolic:-lc}}"

#undef  LINK_SPEC
#define LINK_SPEC "%{h*} %{v:-V} %{!mel:-EB} %{mel:-EL}\
		   %{static:-Bstatic} %{shared:-shared} %{symbolic:-Bsymbolic}"

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "meb" }
#endif

/* Layout of Source Language Data Types */

#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 0

#undef  SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef  WCHAR_TYPE
#define WCHAR_TYPE "unsigned int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* Registers...

   $0  - general purpose 32-bit register.
   $1  - general purpose 32-bit register.
   $2  - general purpose 32-bit register.
   $3  - general purpose 32-bit register.
   $4  - general purpose 32-bit register.
   $5  - general purpose 32-bit register.
   $6  - general purpose 32-bit register.
   $7  - general purpose 32-bit register.
   $8  - general purpose 32-bit register.
   $9  - general purpose 32-bit register.
   $10 - general purpose 32-bit register.
   $11 - general purpose 32-bit register.
   $12 - general purpose 32-bit register.
   $13 - general purpose 32-bit register.
   $14 - general purpose 32-bit register.
   $15 - general purpose 32-bit register.
   $16 - general purpose 32-bit register.
   $17 - general purpose 32-bit register.
   $18 - general purpose 32-bit register.
   $19 - general purpose 32-bit register.
   $20 - general purpose 32-bit register.
   $21 - general purpose 32-bit register.
   %22 - general purpose 32-bit register.
   %23 - general purpose 32-bit register.
   %24 - general purpose 32-bit register.
   %25 - general purpose 32-bit register.
   %26 - general purpose 32-bit register.
   %27 - general purpose 32-bit register.
   %28 - general purpose 32-bit register.
   %29 - general purpose 32-bit register.
   %fp - frame pointer
   %sp - stack pointer

   Special Registers...

   5pc - 32-bit program counter.
   
*/

#define REGISTER_NAMES {	\
  "%0", "%1", "%2", "%3", "%4", "%5", "%6", "%7", \
  "%8", "%9", "%10", "%11", "%12", "%13", "%14", "%15", \
  "%16", "%17", "%18", "%19", "%20", "%21", "%22", "%23", \
  "%24", "%25", "%26", "%27", "%28", "%29", "%fp", "%sp", \
  "?fp", "?ap", "%pc", "?cc" }

#define BEXKAT1_R0     0
#define BEXKAT1_R1     1 
#define BEXKAT1_R2     2
#define BEXKAT1_R3     3
#define BEXKAT1_R4     4
#define BEXKAT1_R5     5
#define BEXKAT1_R6     6
#define BEXKAT1_R7     7
#define BEXKAT1_R8     8
#define BEXKAT1_R9     9
#define BEXKAT1_R10    10
#define BEXKAT1_R11    11
#define BEXKAT1_R12    12
#define BEXKAT1_R13    13
#define BEXKAT1_R14    14
#define BEXKAT1_R15    15
#define BEXKAT1_R16    16
#define BEXKAT1_R17    17
#define BEXKAT1_R18    18
#define BEXKAT1_R19    19
#define BEXKAT1_R20    20
#define BEXKAT1_R21    21
#define BEXKAT1_R22    22
#define BEXKAT1_R23    23
#define BEXKAT1_R24    24
#define BEXKAT1_R25    25
#define BEXKAT1_R26    26
#define BEXKAT1_R27    27
#define BEXKAT1_R28    28
#define BEXKAT1_R29    29

#define BEXKAT1_QFP    32
#define BEXKAT1_QAP    33
#define BEXKAT1_PC     34
#define BEXKAT1_CC     35

#define FIRST_PSEUDO_REGISTER 36

enum reg_class
{
  NO_REGS,
  GENERAL_REGS,
  SPECIAL_REGS,
  CC_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};


#define REG_CLASS_CONTENTS \
{ { 0x000000000 }, /* Empty */			         \
  { 0x3FFFFFFFF }, /* %0 to %28, %fp, %sp, ?fp, ?ap */   \
  { 0x400000000 }, /* %pc */	                         \
  { 0x800000000 }, /* ?cc */                             \
  { 0xFFFFFFFFF }  /* All registers */                   \
}

#define N_REG_CLASSES LIM_REG_CLASSES

#define REG_CLASS_NAMES {\
    "NO_REGS", \
    "GENERAL_REGS", \
    "SPECIAL_REGS", \
    "CC_REGS", \
    "ALL_REGS" }

#define FIXED_REGISTERS     { 0, 0, 0, 0, 0, 0, 0, 0, \
                              0, 0, 0, 0, 0, 0, 0, 0, \
                              0, 0, 0, 0, 0, 0, 0, 0, \
                              0, 0, 0, 0, 0, 0, 1, 1, \
                              1, 1, 1, 1 }

#define CALL_USED_REGISTERS { 1, 1, 1, 1, 0, 0, 0, 0, \
                              0, 0, 0, 0, 0, 0, 0, 1, \
                              0, 0, 0, 0, 0, 0, 0, 0, \
                              0, 0, 0, 0, 0, 0, 1, 1, \
                              1, 1, 1, 1 }

/* We can't copy to or from our CC register. */
#define AVOID_CCMODE_COPIES 1

/* A C expression that is nonzero if it is permissible to store a
   value of mode MODE in hard register number REGNO (or in several
   registers starting with that one).  All gstore registers are 
   equivalent, so we can set this to 1.  */
#define HARD_REGNO_MODE_OK(R,M) 1

/* A C expression whose value is a register class containing hard
   register REGNO.  */
#define REGNO_REG_CLASS(R) ((R < BEXKAT1_PC) ? GENERAL_REGS :		\
                            (R == BEXKAT1_CC ? CC_REGS : SPECIAL_REGS))

/* A C expression for the number of consecutive hard registers,
   starting at register number REGNO, required to hold a value of mode
   MODE.  */
#define HARD_REGNO_NREGS(REGNO, MODE)			   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)		   \
   / UNITS_PER_WORD)

/* A C expression that is nonzero if a value of mode MODE1 is
   accessible in mode MODE2 without copying.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 1

/* The Overall Framework of an Assembler File */

#undef  ASM_SPEC
#define ASM_SPEC "%{!mel:-EB} %{mel:-EL}"
#define ASM_COMMENT_START "#"
#define ASM_APP_ON ""
#define ASM_APP_OFF ""

#define FILE_ASM_OP     "\t.file\n"

/* Switch to the text or data segment.  */
#define TEXT_SECTION_ASM_OP  "\t.text"
#define DATA_SECTION_ASM_OP  "\t.data"

/* Assembler Commands for Alignment */

#define ASM_OUTPUT_ALIGN(STREAM,POWER) \
	fprintf (STREAM, "\t.p2align\t%d\n", POWER);

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand X.  */
#define PRINT_OPERAND(STREAM, X, CODE) bexkat1_print_operand (STREAM, X, CODE)

#define PRINT_OPERAND_ADDRESS(STREAM ,X) bexkat1_print_operand_address (STREAM, X)

/* Output and Generation of Labels */

#define GLOBAL_ASM_OP "\t.global\t"

/* Passing Arguments in Registers */

/* A C type for declaring a variable that is used as the first
   argument of `FUNCTION_ARG' and other related values.  */
#define CUMULATIVE_ARGS unsigned int

/* If defined, the maximum amount of space required for outgoing arguments
   will be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* A C statement (sans semicolon) for initializing the variable CUM
   for the state at the beginning of the argument list.  
   For bexkat1, the first arg is passed in register 0 (aka %0).  */
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS) \
  (CUM = BEXKAT1_R0)

/* How Scalar Function Values Are Returned */

/* STACK AND CALLING */

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 0

/* Offset from the frame pointer to the first local variable slot to
   be allocated.  */
#define STARTING_FRAME_OFFSET 0

/* Define this if the above stack space is to be considered part of the
   space allocated by the caller.  */
#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1
#define STACK_PARMS_IN_REG_PARM_AREA

/* Define this if it is the responsibility of the caller to allocate
   the area reserved for arguments passed in registers.  */
#define REG_PARM_STACK_SPACE(FNDECL) (4 * UNITS_PER_WORD)

/* Offset from the argument pointer register to the first argument's
   address.  On some machines it may depend on the data type of the
   function.  */
#define FIRST_PARM_OFFSET(F) 8

/* Define this macro to nonzero value if the addresses of local variable slots
   are at negative offsets from the frame pointer.  */
#define FRAME_GROWS_DOWNWARD 1

/* Define this macro as a C expression that is nonzero for registers that are
   used by the epilogue or the return pattern.  The stack and frame
   pointer registers are already assumed to be used as needed.  */
#define EPILOGUE_USES(R) (R == BEXKAT1_R15)

/* A C expression whose value is RTL representing the location of the
   incoming return address at the beginning of any function, before
   the prologue.  */
#define INCOMING_RETURN_ADDR_RTX					\
  gen_frame_mem (Pmode,							\
		 plus_constant (Pmode, stack_pointer_rtx, UNITS_PER_WORD))

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N)	((N) < 4 ? (N+2) : INVALID_REGNUM)

/* Store the return handler into the call frame.  */
#define EH_RETURN_HANDLER_RTX						\
  gen_frame_mem (Pmode,							\
		 plus_constant (Pmode, frame_pointer_rtx, UNITS_PER_WORD))

/* Storage Layout */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN ( ! TARGET_LITTLE_ENDIAN )
#define WORDS_BIG_ENDIAN ( ! TARGET_LITTLE_ENDIAN )

/* Alignment required for a function entry point, in bits.  */
#define FUNCTION_BOUNDARY 32

/* Define this macro as a C expression which is nonzero if accessing
   less than a word of memory (i.e. a `char' or a `short') is no
   faster than accessing a word of memory.  */
#define SLOW_BYTE_ACCESS 1

/* Number of storage units in a word; normally the size of a
   general-purpose register, a power of two from 1 or 8.  */
#define UNITS_PER_WORD 4

/* Define this macro to the minimum alignment enforced by hardware
   for the stack pointer on this machine.  The definition is a C
   expression for the desired alignment (measured in bits).  */
#define STACK_BOUNDARY 32

/* Normal alignment required for function parameters on the stack, in
   bits.  All stack parameters receive at least this much alignment
   regardless of data type.  */
#define PARM_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY  32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Every structures size must be a multiple of 8 bits.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* Look at the fundamental type that is used for a bit-field and use 
   that to impose alignment on the enclosing structure.
   struct s {int a:8}; should have same alignment as "int", not "char".  */
#define	PCC_BITFIELD_TYPE_MATTERS	1

/* Largest integer machine mode for structures.  If undefined, the default
   is GET_MODE_SIZE(DImode).  */
#define MAX_FIXED_MODE_SIZE 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
   ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))
     
/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Generating Code for Profiling */
#define FUNCTION_PROFILER(FILE,LABELNO) (abort (), 0)

/* Trampolines for Nested Functions.  */
#define TRAMPOLINE_SIZE (4 + 8 + 4 + 8 + 8)

/* Alignment required for trampolines, in bits.  */
#define TRAMPOLINE_ALIGNMENT 32

/* An alias for the machine mode for pointers.  */
#define Pmode         SImode

/* An alias for the machine mode used for memory references to
   functions being called, in `call' RTL expressions.  */
#define FUNCTION_MODE SImode

/* The register number of the stack pointer register, which must also
   be a fixed register according to `FIXED_REGISTERS'.  */
#define STACK_POINTER_REGNUM BEXKAT1_SP

/* The register number of the frame pointer register, which is used to
   access automatic variables in the stack frame.  */
#define FRAME_POINTER_REGNUM BEXKAT1_QFP

/* The register number of the arg pointer register, which is used to
   access the function's argument list.  */
#define ARG_POINTER_REGNUM BEXKAT1_QAP

#define HARD_FRAME_POINTER_REGNUM BEXKAT1_FP

#define ELIMINABLE_REGS					\
 {{ ARG_POINTER_REGNUM,   HARD_FRAME_POINTER_REGNUM },	\
 {FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM }}

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if `ELIMINABLE_REGS' is
   defined.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  do {									\
    (OFFSET) = bexkat1_initial_elimination_offset ((FROM), (TO));		\
  } while (0)

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */
#define FUNCTION_ARG_REGNO_P(r) (r <= BEXKAT1_R3)

/* A macro whose definition is the name of the class to which a valid
   base register must belong.  A base register is one used in an
   address which is the register value plus a displacement.  */
#define BASE_REG_CLASS GENERAL_REGS

#define INDEX_REG_CLASS NO_REGS

#define HARD_REGNO_OK_FOR_BASE_P(NUM) \
  ((unsigned) (NUM) < FIRST_PSEUDO_REGISTER \
   && (REGNO_REG_CLASS(NUM) == GENERAL_REGS \
       || (NUM) == HARD_FRAME_POINTER_REGNUM))

/* A C expression which is nonzero if register number NUM is suitable
   for use as a base register in operand addresses.  */
#ifdef REG_OK_STRICT
#define REGNO_OK_FOR_BASE_P(NUM)		 \
  (HARD_REGNO_OK_FOR_BASE_P(NUM) 		 \
   || HARD_REGNO_OK_FOR_BASE_P(reg_renumber[(NUM)]))
#else
#define REGNO_OK_FOR_BASE_P(NUM)		 \
  ((NUM) >= FIRST_PSEUDO_REGISTER || HARD_REGNO_OK_FOR_BASE_P(NUM))
#endif

/* A C expression which is nonzero if register number NUM is suitable
   for use as an index register in operand addresses.  */
#define REGNO_OK_FOR_INDEX_P(NUM) BEXKAT1_FP

/* The maximum number of bytes that a single instruction can move
   quickly between memory and registers or between two memory
   locations.  */
#define MOVE_MAX 4
#define TRULY_NOOP_TRUNCATION(op,ip) 1

/* All load operations zero extend.  */
#define LOAD_EXTEND_OP(MEM) ZERO_EXTEND

/* A number, the maximum number of registers that can appear in a
   valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

#define TRULY_NOOP_TRUNCATION(op,ip) 1

/* An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* A C compound statement with a conditional `goto LABEL;' executed
   if X (an RTX) is a legitimate memory address on the target machine
   for a memory operand of mode MODE.  */
#define GO_IF_LEGITIMATE_ADDRESS(MODE,X,LABEL)		\
  do {                                                  \
    if (GET_CODE(X) == PLUS)				\
      {							\
	rtx op1,op2;					\
	op1 = XEXP(X,0);				\
	op2 = XEXP(X,1);				\
	if (GET_CODE(op1) == REG			\
	    && GET_CODE(op2) == CONST_INT 		\
	    && IN_RANGE (INTVAL (op2), -32768, 32767)	\
	    && REGNO_OK_FOR_BASE_P(REGNO(op1)))		\
	  goto LABEL;					\
      }							\
    if (REG_P (X) && REGNO_OK_FOR_BASE_P (REGNO (X)))	\
      goto LABEL;					\
    if (GET_CODE (X) == SYMBOL_REF			\
	|| GET_CODE (X) == LABEL_REF			\
	|| GET_CODE (X) == CONST)			\
      goto LABEL;					\
  } while (0)

/* Run-time Target Specification */

#define TARGET_CPU_CPP_BUILTINS() \
  { \
    builtin_define_std ("bexkat1");			\
    builtin_define_std ("BEXKAT1");			\
    if (TARGET_LITTLE_ENDIAN)				\
      builtin_define ("__BEXKAT1_LITTLE_ENDIAN__");	\
    else						\
      builtin_define ("__BEXKAT1_BIG_ENDIAN__");		\
  }

#define HAS_LONG_UNCOND_BRANCH true

#endif /* GCC_BEXKAT1_H */
