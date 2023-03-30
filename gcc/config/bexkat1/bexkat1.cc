/* Target Code for bexkat1
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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "insn-attr.h"
#include "output.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "expr.h"
#include "builtins.h"

/* included last */
#include "target-def.h"

#define LOSE_AND_RETURN(msgid, x)		\
  do						\
    {						\
      bexkat1_operand_lossage (msgid, x);		\
      return;					\
    } while (0)

static bool
bexkat1_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return (size == -1 || size > 2 * UNITS_PER_WORD);
}

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its
   FUNCTION_DECL; otherwise, FUNC is 0.  

   We always return values in register %12 for bexkat1.  */

static rtx
bexkat1_function_value (const_tree valtype, 
		      const_tree fntype_or_decl ATTRIBUTE_UNUSED,
		      bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (valtype), BEXKAT1_R12);
}

/* Define how to find the value returned by a library function.

   We always return values in register %12 for bexkat1.  */

static rtx
bexkat1_libcall_value (machine_mode mode,
                     const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, BEXKAT1_R12);
}

/* Handle TARGET_FUNCTION_VALUE_REGNO_P.

   We always return values in register %12 for bexkat1.  */

static bool
bexkat1_function_value_regno_p (const unsigned int regno)
{
  return (regno == BEXKAT1_R12);
}

enum bexkat1_function_kind
bexkat1_get_function_kind (tree func)
{
  tree a;

  gcc_assert (TREE_CODE (func) == FUNCTION_DECL);

  a = lookup_attribute ("interrupt", DECL_ATTRIBUTES (func));
  if (a != NULL_TREE)
    return bexkat1_fk_interrupt_handler;
  a = lookup_attribute ("interrupt_handler", DECL_ATTRIBUTES (func));
  if (a != NULL_TREE)
    return bexkat1_fk_interrupt_handler;
  a = lookup_attribute ("exception_handler", DECL_ATTRIBUTES (func));
  if (a != NULL_TREE)
    return bexkat1_fk_exception_handler;

  return bexkat1_fk_normal_function;
}

static tree
bexkat1_handle_fndecl_attribute (tree *node, tree name,
				 tree args ATTRIBUTE_UNUSED,
				 int flags ATTRIBUTE_UNUSED,
				 bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }
  return NULL_TREE;
}

static const struct attribute_spec bexkat1_attribute_table[] =
  {
   { "interrupt", 0, 0, true, false, false, false,
      bexkat1_handle_fndecl_attribute, NULL },
   { "interrupt_handler", 0, 0, true, false, false, false,
      bexkat1_handle_fndecl_attribute, NULL },
   { "exception_handler", 0, 0, true, false, false, false,
      bexkat1_handle_fndecl_attribute, NULL },
   { NULL, 0, 0, false, false, false, false, NULL, NULL }
  };

/* Emit an error message when we're in an asm, and a fatal error for
   "normal" insns.  Formatted output isn't easily implemented, since we
   use output_operand_lossage to output the actual message and handle the
   categorization of the error.  */

poly_int64
bexkat1_push_rounding (poly_int64 bytes)
{
  return (bytes + 3) & ~3;
}

static void
bexkat1_operand_lossage (const char *msgid, rtx op)
{
  debug_rtx (op);
  output_operand_lossage ("%s", msgid);
}

/* The PRINT_OPERAND_ADDRESS worker.  */

void
bexkat1_print_operand_address (FILE *file, rtx x)
{
  switch (GET_CODE (x))
    {
    case REG:
      fprintf (file, "(%s)", reg_names[REGNO (x)]);
      break;
      
    case PLUS:
      switch (GET_CODE (XEXP (x, 1)))
	{
	case CONST_INT:
	  fprintf (file, "%ld(%s)", 
		   INTVAL(XEXP (x, 1)), reg_names[REGNO (XEXP (x, 0))]);
	  break;
	case SYMBOL_REF:
	  output_addr_const (file, XEXP (x, 1));
	  fprintf (file, "(%s)", reg_names[REGNO (XEXP (x, 0))]);
	  break;
	case CONST:
	  {
	    rtx plus = XEXP (XEXP (x, 1), 0);
	    if (GET_CODE (XEXP (plus, 0)) == SYMBOL_REF 
		&& CONST_INT_P (XEXP (plus, 1)))
	      {
		output_addr_const(file, XEXP (plus, 0));
		fprintf (file,"+%ld(%s)", INTVAL (XEXP (plus, 1)),
			 reg_names[REGNO (XEXP (x, 0))]);
	      }
	    else
	      abort();
	  }
	  break;
	default:
	  abort();
	}
      break;

    default:
      output_addr_const (file, x);
      break;
    }
}

/* The PRINT_OPERAND worker.  */

void
bexkat1_print_operand (FILE *file, rtx x, int code)
{
  rtx operand = x;

  /* New code entries should just be added to the switch below.  If
     handling is finished, just return.  If handling was just a
     modification of the operand, the modified operand should be put in
     "operand", and then do a break to let default handling
     (zero-modifier) output the operand.  */

  switch (code)
    {
    case 0:
      /* No code, print as usual.  */
      break;
    case 'a':
      if (GET_CODE (operand) == REG) {
	if (REGNO (operand) > BEXKAT1_SP)
	  internal_error ("internal error: bad register: %d", REGNO (operand));
	fprintf (file, "(%s)", reg_names[REGNO (operand)]);
	return;
      }
      break;
    default:
      LOSE_AND_RETURN ("invalid operand modifier letter", x);
    }

  unsigned long value_long;

  /* Print an operand as without a modifier letter.  */
  switch (GET_CODE (operand))
    {
    case CONST_DOUBLE:
      REAL_VALUE_TO_TARGET_SINGLE
	(*CONST_DOUBLE_REAL_VALUE (operand), value_long);
      fprintf(file, HOST_WIDE_INT_PRINT_HEX, value_long);
      return;

    case REG:
      if (REGNO (operand) > BEXKAT1_SP)
	internal_error ("internal error: bad register: %d", REGNO (operand));
      fprintf (file, "%s", reg_names[REGNO (operand)]);
      return;

    case MEM:
      output_address (GET_MODE (operand), XEXP (operand, 0));
      return;

    default:
      /* No need to handle all strange variants, let output_addr_const
	 do it for us.  */
      if (CONSTANT_P (operand))
	{
	  output_addr_const (file, operand);
	  return;
	}

      LOSE_AND_RETURN ("unexpected operand", x);
    }
}

/* Per-function machine data.  */
struct GTY(()) machine_function
 {
   /* Number of bytes saved on the stack for callee saved registers.  */
   int callee_saved_reg_size;

   /* Number of bytes saved on the stack for local variables.  */
   int local_vars_size;

   /* The sum of 2 sizes: locals vars and padding byte for saving the
    * registers.  Used in expand_prologue () and expand_epilogue().  */
   int size_for_adjusting_sp;
 };

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
bexkat1_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}


/* The TARGET_OPTION_OVERRIDE worker.  */
static void
bexkat1_option_override (void)
{
  /* Set the per-function-data initializer.  */
  init_machine_status = bexkat1_init_machine_status;
}

/* Compute the size of the local area and the size to be adjusted by the
 * prologue and epilogue.  */

static void
bexkat1_compute_frame (bool interrupt_handler)
{
  /* For aligning the local variables.  */
  int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  int padding_locals;
  int regno;

  /* Padding needed for each element of the frame.  */
  cfun->machine->local_vars_size = get_frame_size ();

  /* Align to the stack alignment.  */
  padding_locals = cfun->machine->local_vars_size % stack_alignment;
  if (padding_locals)
    padding_locals = stack_alignment - padding_locals;

  cfun->machine->local_vars_size += padding_locals;

  cfun->machine->callee_saved_reg_size = 0;

  /* Save callee-saved registers.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (df_regs_ever_live_p (regno) && (! call_used_regs[regno]))
      cfun->machine->callee_saved_reg_size += 4;
  if (interrupt_handler)
    {
      cfun->machine->callee_saved_reg_size += 8; /* %12, %13 */
    }
  
  cfun->machine->size_for_adjusting_sp = 
    crtl->args.pretend_args_size
    + cfun->machine->local_vars_size 
    + (ACCUMULATE_OUTGOING_ARGS ?
       (HOST_WIDE_INT) crtl->outgoing_args_size : 0);
}

void
bexkat1_expand_prologue (void)
{
  int regno;
  rtx insn;
  enum bexkat1_function_kind func_kind =
    bexkat1_get_function_kind(current_function_decl);
  bool interrupt_handler = (func_kind == bexkat1_fk_interrupt_handler);
  
  bexkat1_compute_frame (interrupt_handler);

  /* Save callee-saved registers.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (!fixed_regs[regno] && df_regs_ever_live_p (regno) && !call_used_regs[regno])
	{
	  insn = emit_insn (gen_movsi_push (gen_rtx_REG (Pmode, regno)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  /* Save return registers if this is an interrupt or exception handler */
  if (interrupt_handler) {
    insn = emit_insn (gen_movsi_push (gen_rtx_REG (Pmode, 12)));
    insn = emit_insn (gen_movsi_push (gen_rtx_REG (Pmode, 13)));
    insn = emit_insn (gen_movcc_push ());
  }
  
  insn = emit_insn (gen_movsi_push (gen_rtx_REG (Pmode, FRAME_POINTER_REGNUM)));
  RTX_FRAME_RELATED_P (insn) = 1;
  emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);

  if (cfun->machine->size_for_adjusting_sp > 0) {
    int i = cfun->machine->size_for_adjusting_sp;
    while (i > 16382) {
      emit_insn(gen_subsi3(stack_pointer_rtx,
			   stack_pointer_rtx,
			   GEN_INT(16382)));
      i -= 16382;
    }
    emit_insn(gen_subsi3(stack_pointer_rtx,
			 stack_pointer_rtx,
			 GEN_INT(i)));
  }

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun->machine->size_for_adjusting_sp;
}

void
bexkat1_expand_epilogue (void)
{
  int regno;
  enum bexkat1_function_kind func_kind =
    bexkat1_get_function_kind(current_function_decl);
  bool interrupt_handler = (func_kind == bexkat1_fk_interrupt_handler);

  if (cfun->machine->size_for_adjusting_sp > 0)
    {
      int i = cfun->machine->size_for_adjusting_sp;
      while (i > 16382) {
	emit_insn(gen_addsi3(stack_pointer_rtx,
			     stack_pointer_rtx,
			     GEN_INT(16382)));
	i -= 16382;
      }
      emit_insn(gen_addsi3(stack_pointer_rtx,
  				stack_pointer_rtx,
				GEN_INT(i)));
    }
  emit_insn (gen_movsi_pop (gen_rtx_REG (Pmode, FRAME_POINTER_REGNUM)));

  
  if (interrupt_handler) {
    emit_insn (gen_movcc_pop ());
    emit_insn (gen_movsi_pop (gen_rtx_REG (Pmode, 13)));
    emit_insn (gen_movsi_pop (gen_rtx_REG (Pmode, 12)));
  }
  for (regno = FIRST_PSEUDO_REGISTER; regno-- > 0; )
	if (!fixed_regs[regno] && !call_used_regs[regno]
	    && df_regs_ever_live_p (regno))
	  {
	    rtx preg = gen_rtx_REG (Pmode, regno);
	    emit_insn (gen_movsi_pop ( preg));
	  }

  emit_jump_insn (gen_returner ());
}

/* Implements the macro INITIAL_ELIMINATION_OFFSET, return the OFFSET.  */

int
bexkat1_initial_elimination_offset (int from, int to)
{
  int ret;
  enum bexkat1_function_kind func_kind =
    bexkat1_get_function_kind(current_function_decl);
  bool interrupt_handler = (func_kind == bexkat1_fk_interrupt_handler);

  bexkat1_compute_frame (interrupt_handler);
  
  if ((from) == FRAME_POINTER_REGNUM && (to) == STACK_POINTER_REGNUM)
    {
      /* Compute this since we need to use cfun->machine->local_vars_size.  */
      ret = -cfun->machine->callee_saved_reg_size;
    }
  else if ((from) == ARG_POINTER_REGNUM && (to) == STACK_POINTER_REGNUM)
    {
      /* Compute this since we need to use cfun->machine->local_vars_size.  */
      ret = -cfun->machine->callee_saved_reg_size -
	cfun->machine->size_for_adjusting_sp;
    }
  else if ((from) == ARG_POINTER_REGNUM && (to) == FRAME_POINTER_REGNUM)
    ret = cfun->machine->callee_saved_reg_size;
  else {
    fprintf(stderr, "invalid elim %d %d\n", from, to);
    abort ();
  }

  return ret;
}

/* Worker function for TARGET_SETUP_INCOMING_VARARGS.  */

static void
bexkat1_setup_incoming_varargs (cumulative_args_t cum_v,
			      const function_arg_info &arg ATTRIBUTE_UNUSED,
			      int *pretend_size, int no_rtl)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int regno;
  int regs = 8 - *cum;
  
  *pretend_size = regs < 0 ? 0 : GET_MODE_SIZE (SImode) * regs;
  
  if (no_rtl)
    return;
  
  for (regno = *cum; regno < 8; regno++)
    {
      rtx reg = gen_rtx_REG (SImode, regno);
      rtx slot = gen_rtx_PLUS (Pmode,
			       gen_rtx_REG (SImode, ARG_POINTER_REGNUM),
			       GEN_INT (UNITS_PER_WORD * (3 + (regno-2))));
      
      emit_move_insn (gen_rtx_MEM (SImode, slot), reg);
    }
}


/* Return the fixed registers used for condition codes.  */

static bool
bexkat1_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = BEXKAT1_CC;
  *p2 = INVALID_REGNUM;
  return true;
}

/* We only pass args on the stack */

static rtx
bexkat1_function_arg (cumulative_args_t cum_v ATTRIBUTE_UNUSED,
		      const function_arg_info &args ATTRIBUTE_UNUSED)
{
  return NULL_RTX;
}

#define BEXKAT1_FUNCTION_ARG_SIZE(MODE, TYPE)	\
  ((MODE) != BLKmode ? GET_MODE_SIZE (MODE)	\
   : (unsigned) int_size_in_bytes (TYPE))

static void
bexkat1_function_arg_advance (cumulative_args_t cum_v ATTRIBUTE_UNUSED,
			      const function_arg_info &args ATTRIBUTE_UNUSED)
{
}

/* Return non-zero if the function argument described by TYPE is to be
   passed by reference.  */

static bool
bexkat1_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
		      const function_arg_info &arg)
{
  unsigned HOST_WIDE_INT size;

  if (arg.type)
    {
      if (AGGREGATE_TYPE_P (arg.type))
	return true;
      size = int_size_in_bytes (arg.type);
    }
  else
    size = GET_MODE_SIZE (arg.mode);

  return size > 4*6;
}

/* Some function arguments will only partially fit in the registers
   that hold arguments.  Given a new arg, return the number of bytes
   that fit in argument passing registers.  */

static int
bexkat1_arg_partial_bytes (cumulative_args_t cum_v,
			   const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes_left, size;

  if (*cum >= 8)
    return 0;

  if (bexkat1_pass_by_reference (cum_v, arg))
    size = 4;
  else if (arg.type)
    {
      if (AGGREGATE_TYPE_P (arg.type))
	return 0;
      size = int_size_in_bytes (arg.type);
    }
  else
    size = GET_MODE_SIZE (arg.mode);

  bytes_left = (4 * 6) - ((*cum - 2) * 4);

  if (size > bytes_left)
    return bytes_left;
  else
    return 0;
}

/* Worker function for TARGET_STATIC_CHAIN.  */

static rtx
bexkat1_static_chain (const_tree ARG_UNUSED (fndecl_or_type), bool incoming_p)
{
  rtx addr, mem;

  if (incoming_p)
    addr = plus_constant (Pmode, arg_pointer_rtx, 2 * UNITS_PER_WORD);
  else
    addr = plus_constant (Pmode, stack_pointer_rtx, -UNITS_PER_WORD);

  mem = gen_rtx_MEM (Pmode, addr);
  MEM_NOTRAP_P (mem) = 1;

  return mem;
}

/* Worker function for TARGET_ASM_TRAMPOLINE_TEMPLATE.  */

static void
bexkat1_asm_trampoline_template (FILE *f)
{
  fprintf (f, "\tpush %%0\n");
  fprintf (f, "\tldi %%0, 0x0\n");
  fprintf (f, "\tst.l %%0, 8(%%30)\n");
  fprintf (f, "\tpop %%0\n");
  fprintf (f, "\tjmp  0x0\n");
}

/* Worker function for TARGET_TRAMPOLINE_INIT.  */

static void
bexkat1_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx mem, fnaddr = XEXP (DECL_RTL (fndecl), 0);

  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  mem = adjust_address (m_tramp, SImode, 4);
  emit_move_insn (mem, chain_value);
  mem = adjust_address (m_tramp, SImode, 16);
  emit_move_insn (mem, fnaddr);
}

/* Return true for memory offset addresses between -32768 and 32767.  */
bool
bexkat1_offset_address_p (rtx x)
{
  x = XEXP (x, 0);

  if (GET_CODE (x) == PLUS)
    {
      x = XEXP (x, 1);
      if (GET_CODE (x) == CONST_INT)
	{
	  unsigned int v = INTVAL (x) & 0xFFFF0000;
	  return (v == 0xFFFF0000 || v == 0x00000000);
	}
    }
  return 0;
}

/* The Global `targetm' Variable.  */

/* Initialize the GCC target structure.  */

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES	hook_bool_const_tree_true

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY         bexkat1_return_in_memory
#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK	must_pass_in_stack_var_size

#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG		bexkat1_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE	bexkat1_function_arg_advance

#undef	TARGET_FIXED_CONDITION_CODE_REGS
#define	TARGET_FIXED_CONDITION_CODE_REGS bexkat1_fixed_condition_code_regs

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE          bexkat1_attribute_table

/* Define this to return an RTX representing the place where a
   function returns or receives a value of data type RET_TYPE, a tree
   node node representing a data type.  */
#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE bexkat1_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE bexkat1_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P bexkat1_function_value_regno_p

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED hook_bool_void_true

#undef TARGET_STATIC_CHAIN
#define TARGET_STATIC_CHAIN bexkat1_static_chain
#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE bexkat1_asm_trampoline_template
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT bexkat1_trampoline_init

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT constant_alignment_word_strings

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE bexkat1_option_override

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-bexkat1.h"
