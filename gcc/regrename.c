/* Register renaming for the GNU compiler.
   Copyright (C) 2000 Free Software Foundation, Inc.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#define REG_OK_STRICT

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "reload.h"
#include "output.h"
#include "function.h"
#include "recog.h"
#include "flags.h"
#include "obstack.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

#ifndef REGNO_MODE_OK_FOR_BASE_P
#define REGNO_MODE_OK_FOR_BASE_P(REGNO, MODE) REGNO_OK_FOR_BASE_P (REGNO)
#endif

#ifndef REG_MODE_OK_FOR_BASE_P
#define REG_MODE_OK_FOR_BASE_P(REGNO, MODE) REG_OK_FOR_BASE_P (REGNO)
#endif

static const char *const reg_class_names[] = REG_CLASS_NAMES;

struct du_chain
{
  struct du_chain *next_chain;
  struct du_chain *next_use;

  rtx insn;
  rtx *loc;
  enum reg_class class;
  unsigned int need_caller_save_reg:1;
};

enum scan_actions
{
  note_reference,
  terminate_all_read,
  terminate_overlapping_read,
  terminate_write,
  terminate_dead,
  mark_read,
  mark_write
};

static const char * const scan_actions_name[] =
{
  "note_reference",
  "terminate_all_read",
  "terminate_overlapping_read",
  "terminate_write",
  "terminate_dead",
  "mark_read",
  "mark_write"
};

static struct obstack rename_obstack;

static void do_replace PARAMS ((struct du_chain *, int));
static void scan_rtx_reg PARAMS ((rtx, rtx *, enum reg_class,
				  enum scan_actions, enum op_type));
static void scan_rtx_address PARAMS ((rtx, rtx *, enum reg_class,
				      enum scan_actions, enum machine_mode));
static void scan_rtx PARAMS ((rtx, rtx *, enum reg_class,
			      enum scan_actions, enum op_type));
static struct du_chain *build_def_use PARAMS ((basic_block, HARD_REG_SET *));
static void dump_def_use_chain PARAMS ((struct du_chain *));

void
regrename_optimize ()
{
  int b;
  char *first_obj;

  gcc_obstack_init (&rename_obstack);
  first_obj = (char *) obstack_alloc (&rename_obstack, 0);

  for (b = 0; b < n_basic_blocks; b++)
    {
      basic_block bb = BASIC_BLOCK (b);
      struct du_chain *all_chains = 0;
      HARD_REG_SET regs_used;
      HARD_REG_SET unavailable;
      HARD_REG_SET regs_seen;

      CLEAR_HARD_REG_SET (regs_used);
      CLEAR_HARD_REG_SET (unavailable);

      if (rtl_dump_file)
	fprintf (rtl_dump_file, "\nBasic block %d:\n", b);

      all_chains = build_def_use (bb, &regs_used);

      if (rtl_dump_file)
	dump_def_use_chain (all_chains);

      /* Available registers are not: used in the block, live at the start
	 live at the end, a register we've renamed to. */
      REG_SET_TO_HARD_REG_SET (unavailable, bb->global_live_at_start);
      REG_SET_TO_HARD_REG_SET (regs_seen, bb->global_live_at_end);
      IOR_HARD_REG_SET (unavailable, regs_seen);
      IOR_HARD_REG_SET (unavailable, regs_used);

      /* Don't clobber traceback for noreturn functions.  */
      if (frame_pointer_needed)
	{
	  SET_HARD_REG_BIT (unavailable, FRAME_POINTER_REGNUM);
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
	  SET_HARD_REG_BIT (unavailable, HARD_FRAME_POINTER_REGNUM);
#endif
	}

      CLEAR_HARD_REG_SET (regs_seen);
      while (all_chains)
	{
	  int n_uses;
	  struct du_chain *this = all_chains;
	  struct du_chain *tmp, *last;
	  HARD_REG_SET this_unavailable;
	  int reg = REGNO (*this->loc), treg;
	  int nregs = HARD_REGNO_NREGS (reg, GET_MODE (*this->loc));
	  int i;

	  all_chains = this->next_chain;

	  /* Only rename once we've seen the reg more than once.  */
	  if (! TEST_HARD_REG_BIT (regs_seen, reg))
	    {
	      SET_HARD_REG_BIT (regs_seen, reg);
	      continue;
	    }

	  if (fixed_regs[reg] || global_regs[reg])
	    continue;

	  COPY_HARD_REG_SET (this_unavailable, unavailable);

	  /* Find last entry on chain (which has the need_caller_save bit),
	     count number of uses, and narrow the set of registers we can
	     use for renaming.  */
	  n_uses = 0;
	  for (last = this; last->next_use; last = last->next_use)
	    {
	      n_uses++;
	      IOR_COMPL_HARD_REG_SET (this_unavailable,
				      reg_class_contents[last->class]);
	    }
	  if (n_uses < 1)
	    continue;

	  IOR_COMPL_HARD_REG_SET (this_unavailable,
				  reg_class_contents[last->class]);

	  if (last->need_caller_save_reg)
	    IOR_HARD_REG_SET (this_unavailable, call_used_reg_set);

	  /* Now potential_regs is a reasonable approximation, let's
	     have a closer look at each register still in there.  */
	  for (treg = 0; treg < FIRST_PSEUDO_REGISTER; treg++)
	    {
	      for (i = nregs - 1; i >= 0; --i)
	        if (TEST_HARD_REG_BIT (this_unavailable, treg+i)
		    || fixed_regs[treg+i]
		    || global_regs[treg+i]
		    /* Can't use regs which aren't saved by the prologue.  */
		    || (! regs_ever_live[treg+i] && ! call_used_regs[treg+i])
#ifdef HARD_REGNO_RENAME_OK
		    || ! HARD_REGNO_RENAME_OK (reg+i, treg+i)
#endif
		    )
		  break;
	      if (i >= 0)
		continue;

	      /* See whether it accepts all modes that occur in
		 definition and uses.  */
	      for (tmp = this; tmp; tmp = tmp->next_use)
		if (! HARD_REGNO_MODE_OK (treg, GET_MODE (*tmp->loc)))
		  break;
	      if (! tmp)
		break;
	    }

	  if (rtl_dump_file)
	    {
	      fprintf (rtl_dump_file, "Register %s in insn %d",
		       reg_names[reg], INSN_UID (last->insn));
	      if (last->need_caller_save_reg)
		fprintf (rtl_dump_file, " crosses a call");
	      }

	  if (treg == FIRST_PSEUDO_REGISTER)
	    {
	      if (rtl_dump_file)
		fprintf (rtl_dump_file, "; no available registers\n");
	      continue;
	    }

	  
	  for (i = nregs - 1; i >= 0; --i)
	    SET_HARD_REG_BIT (unavailable, treg+i);
	  do_replace (this, treg);

	  if (rtl_dump_file)
	    fprintf (rtl_dump_file, ", renamed as %s\n", reg_names[treg]);
	}

      obstack_free (&rename_obstack, first_obj);
    }

  obstack_free (&rename_obstack, NULL);

  if (rtl_dump_file)
    fputc ('\n', rtl_dump_file);

  count_or_remove_death_notes (NULL, 1);
  update_life_info (NULL, UPDATE_LIFE_LOCAL,
		    PROP_REG_INFO | PROP_DEATH_NOTES);
}

static void
do_replace (chain, reg)
     struct du_chain *chain;
     int reg;
{
  while (chain)
    {
      *chain->loc = gen_rtx_REG (GET_MODE (*chain->loc), reg);
      chain = chain->next_use;
    }
}


static HARD_REG_SET *referenced_regs;
static struct du_chain *open_chains;
static struct du_chain *closed_chains;

static void
scan_rtx_reg (insn, loc, class, action, type)
     rtx insn;
     rtx *loc;
     enum reg_class class;
     enum scan_actions action;
     enum op_type type;
{
  struct du_chain **p;
  rtx x = *loc;
  enum machine_mode mode = GET_MODE (x);
  int this_regno = REGNO (x);
  int this_nregs = HARD_REGNO_NREGS (this_regno, mode);

  if (action == note_reference)
    {
      while (this_nregs-- > 0)
	SET_HARD_REG_BIT (*referenced_regs, this_regno + this_nregs);
      return;
    }

  if (action == mark_write)
    {
      if (type == OP_OUT)
	{
	  struct du_chain *this = (struct du_chain *)
	    obstack_alloc (&rename_obstack, sizeof (struct du_chain));
	  this->next_use = 0;
	  this->next_chain = open_chains;
	  this->loc = loc;
	  this->insn = insn;
	  this->class = class;
	  this->need_caller_save_reg = 0;
	  open_chains = this;
	}
      return;
    }

  if ((type == OP_OUT && action != terminate_write)
      || (type != OP_OUT && action == terminate_write))
    return;

  for (p = &open_chains; *p;)
    {
      struct du_chain *this = *p;
      int regno = REGNO (*this->loc);
      int nregs = HARD_REGNO_NREGS (regno, GET_MODE (*this->loc));
      int exact_match = (regno == this_regno && nregs == this_nregs);

      if (regno + nregs <= this_regno
	  || this_regno + this_nregs <= regno)
	p = &this->next_chain;
      else if (action == mark_read)
	{
	  if (! exact_match)
	    abort ();
	  if (class == NO_REGS)
	    abort ();

	  this = (struct du_chain *)
	    obstack_alloc (&rename_obstack, sizeof (struct du_chain));
	  this->next_use = *p;
	  this->next_chain = (*p)->next_chain;
	  this->loc = loc;
	  this->insn = insn;
	  this->class = class;
	  this->need_caller_save_reg = 0;
	  *p = this;
	  return;
	}
      else if (action != terminate_overlapping_read || ! exact_match)
	{
	  struct du_chain *next = this->next_chain;

	  /* Whether the terminated chain can be used for renaming
	     depends on the action and this being an exact match.
	     In either case, we remove this element from open_chains.  */

	  if ((action == terminate_dead || action == terminate_write)
	      && exact_match)
	    {
	      this->next_chain = closed_chains;
	      closed_chains = this;
	      if (rtl_dump_file)
		fprintf (rtl_dump_file,
			 "Closing chain %s at insn %d (%s)\n",
			 reg_names[REGNO (*this->loc)], INSN_UID (insn),
			 scan_actions_name[(int) action]);
	    }
	  else
	    {
	      if (rtl_dump_file)
		fprintf (rtl_dump_file,
			 "Discarding chain %s at insn %d (%s)\n",
			 reg_names[REGNO (*this->loc)], INSN_UID (insn),
			 scan_actions_name[(int) action]);
	    }
	  *p = next;
	}
      else
	p = &this->next_chain;
    }
}

/* Adapted from find_reloads_address_1.  CLASS is INDEX_REG_CLASS or
   BASE_REG_CLASS depending on how the register is being considered.  */

static void
scan_rtx_address (insn, loc, class, action, mode)
     rtx insn;
     rtx *loc;
     enum reg_class class;
     enum scan_actions action;
     enum machine_mode mode;
{
  rtx x = *loc;
  RTX_CODE code = GET_CODE (x);
  const char *fmt;
  int i, j;

  if (action == mark_write)
    return;

  switch (code)
    {
    case PLUS:
      {
	rtx orig_op0 = XEXP (x, 0);
	rtx orig_op1 = XEXP (x, 1);
	RTX_CODE code0 = GET_CODE (orig_op0);
	RTX_CODE code1 = GET_CODE (orig_op1);
	rtx op0 = orig_op0;
	rtx op1 = orig_op1;
	rtx *locI = NULL;
	rtx *locB = NULL;

	if (GET_CODE (op0) == SUBREG)
	  {
	    op0 = SUBREG_REG (op0);
	    code0 = GET_CODE (op0);
	  }

	if (GET_CODE (op1) == SUBREG)
	  {
	    op1 = SUBREG_REG (op1);
	    code1 = GET_CODE (op1);
	  }

	if (code0 == MULT || code0 == SIGN_EXTEND || code0 == TRUNCATE
	    || code0 == ZERO_EXTEND || code1 == MEM)
	  {
	    locI = &XEXP (x, 0);
	    locB = &XEXP (x, 1);
	  }
	else if (code1 == MULT || code1 == SIGN_EXTEND || code1 == TRUNCATE
		 || code1 == ZERO_EXTEND || code0 == MEM)
	  {
	    locI = &XEXP (x, 1);
	    locB = &XEXP (x, 0);
	  }
	else if (code0 == CONST_INT || code0 == CONST
		 || code0 == SYMBOL_REF || code0 == LABEL_REF)
	  locB = &XEXP (x, 1);
	else if (code1 == CONST_INT || code1 == CONST
		 || code1 == SYMBOL_REF || code1 == LABEL_REF)
	  locB = &XEXP (x, 0);
	else if (code0 == REG && code1 == REG)
	  {
	    int index_op;

	    if (REG_OK_FOR_INDEX_P (op0)
		&& REG_MODE_OK_FOR_BASE_P (op1, mode))
	      index_op = 0;
	    else if (REG_OK_FOR_INDEX_P (op1)
		     && REG_MODE_OK_FOR_BASE_P (op0, mode))
	      index_op = 1;
	    else if (REG_MODE_OK_FOR_BASE_P (op1, mode))
	      index_op = 0;
	    else if (REG_MODE_OK_FOR_BASE_P (op0, mode))
	      index_op = 1;
	    else if (REG_OK_FOR_INDEX_P (op1))
	      index_op = 1;
	    else
	      index_op = 0;

	    locI = &XEXP (x, index_op);
	    locB = &XEXP (x, !index_op);
	  }
	else if (code0 == REG)
	  {
	    locI = &XEXP (x, 0);
	    locB = &XEXP (x, 1);
	  }
	else if (code1 == REG)
	  {
	    locI = &XEXP (x, 1);
	    locB = &XEXP (x, 0);
	  }

	if (locI)
	  scan_rtx_address (insn, locI, INDEX_REG_CLASS, action, mode);
	if (locB)
	  scan_rtx_address (insn, locB, BASE_REG_CLASS, action, mode);
	return;
      }

    case POST_INC:
    case POST_DEC:
    case POST_MODIFY:
    case PRE_INC:
    case PRE_DEC:
    case PRE_MODIFY:
#ifndef AUTO_INC_DEC
      /* If the target doesn't claim to handle autoinc, this must be
	 something special, like a stack push.  Kill this chain.  */
      action = terminate_all_read;
#endif
      break;

    case MEM:
      scan_rtx_address (insn, &XEXP (x, 0), BASE_REG_CLASS, action,
			GET_MODE (x));
      return;

    case REG:
      scan_rtx_reg (insn, loc, class, action, OP_IN);
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	scan_rtx_address (insn, &XEXP (x, i), class, action, mode);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  scan_rtx_address (insn, &XVECEXP (x, i, j), class, action, mode);
    }
}

static void
scan_rtx (insn, loc, class, action, type)
     rtx insn;
     rtx *loc;
     enum reg_class class;
     enum scan_actions action;
     enum op_type type;
{
  const char *fmt;
  rtx x = *loc;
  enum rtx_code code = GET_CODE (x);
  int i, j;

  code = GET_CODE (x);
  switch (code)
    {
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case CC0:
    case PC:
      return;

    case REG:
      scan_rtx_reg (insn, loc, class, action, type);
      return;

    case MEM:
      scan_rtx_address (insn, &XEXP (x, 0), BASE_REG_CLASS, action,
			GET_MODE (x));
      return;

    case SET:
      scan_rtx (insn, &SET_SRC (x), class, action, OP_IN);
      scan_rtx (insn, &SET_DEST (x), class, action, OP_OUT);
      return;

    case STRICT_LOW_PART:
      scan_rtx (insn, &XEXP (x, 0), class, action, OP_INOUT);
      return;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT: 
      scan_rtx (insn, &XEXP (x, 0), class, action,
		type == OP_IN ? OP_IN : OP_INOUT);
      scan_rtx (insn, &XEXP (x, 1), class, action, OP_IN);
      scan_rtx (insn, &XEXP (x, 2), class, action, OP_IN);
      return;

    case POST_INC:
    case PRE_INC:
    case POST_DEC:
    case PRE_DEC:
    case POST_MODIFY:
    case PRE_MODIFY:
      /* Should only happen inside MEM.  */
      abort ();

    case CLOBBER:
      scan_rtx (insn, &SET_DEST (x), class, action, OP_OUT);
      return;

    case EXPR_LIST:
      scan_rtx (insn, &XEXP (x, 0), class, action, type);
      if (XEXP (x, 1))
	scan_rtx (insn, &XEXP (x, 1), class, action, type);
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	scan_rtx (insn, &XEXP (x, i), class, action, type);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  scan_rtx (insn, &XVECEXP (x, i, j), class, action, type);
    }
}

/* Build def/use chain */

static struct du_chain *
build_def_use (bb, regs_used)
     basic_block bb;
     HARD_REG_SET *regs_used;
{
  rtx insn;

  open_chains = closed_chains = NULL;
  referenced_regs = regs_used;

  for (insn = bb->head; ; insn = NEXT_INSN (insn))
    {
      if (INSN_P (insn))
	{
	  int n_ops;
	  rtx note;
	  rtx old_operands[MAX_RECOG_OPERANDS];
	  rtx old_dups[MAX_DUP_OPERANDS];
	  int i;
	  int alt;
	  int predicated;

	  /* Record all mentioned registers in regs_used.  */
	  scan_rtx (insn, &PATTERN (insn), NO_REGS, note_reference, OP_IN);

	  /* Process the insn, determining its effect on the def-use
	     chains.  We perform the following steps with the register
	     references in the insn:
	     (1) Any read that overlaps an open chain, but doesn't exactly
	         match, causes that chain to be closed.  We can't deal
	         with overlaps yet.
	     (2) Any read outside an operand causes any chain it overlaps
	         with to be closed, since we can't replace it.
	     (3) Any read inside an operand is added if there's already
	         an open chain for it.
	     (4) For any REG_DEAD note we find, close open chains that
	         overlap it.
	     (5) For any write we find, close open chains that overlap it.
	     (6) For any write we find in an operand, make a new chain.
	     (7) For any REG_UNUSED, close any chains we just opened.  */

	  extract_insn (insn);
	  constrain_operands (1);
	  preprocess_constraints ();
	  alt = which_alternative;
	  n_ops = recog_data.n_operands;

	  /* Simplify the code below by rewriting things to reflect
	     matching constraints.  Also promote OP_OUT to OP_INOUT
	     in predicated instructions.  */

	  predicated = GET_CODE (PATTERN (insn)) == COND_EXEC;
	  for (i = 0; i < n_ops; ++i)
	    {
	      int matches = recog_op_alt[i][alt].matches;
	      if (matches >= 0)
		recog_op_alt[i][alt].class = recog_op_alt[matches][alt].class;
	      if (matches >= 0 || recog_op_alt[i][alt].matched >= 0
	          || (predicated && recog_data.operand_type[i] == OP_OUT))
		recog_data.operand_type[i] = OP_INOUT;
	    }

	  /* Step 1: Close chains for which we have overlapping reads.  */
	  for (i = 0; i < n_ops; i++)
	    scan_rtx (insn, recog_data.operand_loc[i],
		      NO_REGS, terminate_overlapping_read,
		      recog_data.operand_type[i]);

	  /* Step 2: Close chains for which we have reads outside operands.
	     We do this by munging all operands into CC0, and closing 
	     everything remaining.  */

	  for (i = 0; i < n_ops; i++)
	    {
	      old_operands[i] = recog_data.operand[i];
	      /* Don't squash match_operator or match_parallel here, since
		 we don't know that all of the contained registers are 
		 reachable by proper operands.  */
	      if (recog_data.constraints[i][0] == '\0')
		continue;
	      *recog_data.operand_loc[i] = cc0_rtx;
	    }
	  for (i = 0; i < recog_data.n_dups; i++)
	    {
	      old_dups[i] = *recog_data.dup_loc[i];
	      *recog_data.dup_loc[i] = cc0_rtx;
	    }

	  scan_rtx (insn, &PATTERN (insn), NO_REGS, terminate_all_read, OP_IN);

	  for (i = 0; i < recog_data.n_dups; i++)
	    *recog_data.dup_loc[i] = old_dups[i];
	  for (i = 0; i < n_ops; i++)
	    *recog_data.operand_loc[i] = old_operands[i];

	  /* Step 2B: Can't rename function call argument registers.  */
	  if (GET_CODE (insn) == CALL_INSN && CALL_INSN_FUNCTION_USAGE (insn))
	    scan_rtx (insn, &CALL_INSN_FUNCTION_USAGE (insn),
		      NO_REGS, terminate_all_read, OP_IN);

	  /* Step 3: Append to chains for reads inside operands.  */
	  for (i = 0; i < n_ops + recog_data.n_dups; i++)
	    {
	      int opn = i < n_ops ? i : recog_data.dup_num[i - n_ops];
	      rtx *loc = (i < n_ops
			  ? recog_data.operand_loc[opn]
			  : recog_data.dup_loc[i - n_ops]);
	      enum reg_class class = recog_op_alt[opn][alt].class;
	      enum op_type type = recog_data.operand_type[opn];

	      /* Don't scan match_operand here, since we've no reg class
		 information to pass down.  Any operands that we could
		 substitute in will be represented elsewhere.  */
	      if (recog_data.constraints[opn][0] == '\0')
		continue;

	      if (recog_op_alt[opn][alt].is_address)
		scan_rtx_address (insn, loc, class, mark_read, VOIDmode);
	      else
		scan_rtx (insn, loc, class, mark_read, type);
	    }

	  /* Step 4: Close chains for registers that die here.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_DEAD)
	      scan_rtx (insn, &XEXP (note, 0), NO_REGS, terminate_dead, OP_IN);

	  /* Step 4B: If this is a call, any chain live at this point
	     requires a caller-saved reg.  */
	  if (GET_CODE (insn) == CALL_INSN)
	    {
	      struct du_chain *p;
	      for (p = open_chains; p; p = p->next_chain)
		{
		  struct du_chain *p2;
		  for (p2 = p; p2->next_use; p2 = p2->next_use)
		    /* nothing */;
		  p2->need_caller_save_reg = 1;
		}
	    }

	  /* Step 5: Close open chains that overlap writes.  Similar to
	     step 2, we hide in-out operands, since we do not want to
	     close these chains.  */

	  for (i = 0; i < n_ops; i++)
	    {
	      old_operands[i] = recog_data.operand[i];
	      if (recog_data.operand_type[i] == OP_INOUT)
		*recog_data.operand_loc[i] = cc0_rtx;
	    }
	  for (i = 0; i < recog_data.n_dups; i++)
	    {
	      int opn = recog_data.dup_num[i];
	      old_dups[i] = *recog_data.dup_loc[i];
	      if (recog_data.operand_type[opn] == OP_INOUT)
		*recog_data.dup_loc[i] = cc0_rtx;
	    }

	  scan_rtx (insn, &PATTERN (insn), NO_REGS, terminate_write, OP_IN);

	  for (i = 0; i < recog_data.n_dups; i++)
	    *recog_data.dup_loc[i] = old_dups[i];
	  for (i = 0; i < n_ops; i++)
	    *recog_data.operand_loc[i] = old_operands[i];

	  /* Step 6: Begin new chains for writes inside operands.  */
	  /* ??? Many targets have output constraints on the SET_DEST
	     of a call insn, which is stupid, since these are certainly
	     ABI defined hard registers.  Don't change calls at all.  */
	  if (GET_CODE (insn) != CALL_INSN)
	    for (i = 0; i < n_ops + recog_data.n_dups; i++)
	      {
		int opn = i < n_ops ? i : recog_data.dup_num[i - n_ops];
		rtx *loc = (i < n_ops
			    ? recog_data.operand_loc[opn]
			    : recog_data.dup_loc[i - n_ops]);
		enum reg_class class = recog_op_alt[opn][alt].class;

		if (recog_data.operand_type[opn] == OP_OUT)
		  scan_rtx (insn, loc, class, mark_write, OP_OUT);
	      }

	  /* Step 7: Close chains for registers that were never
	     really used here.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_UNUSED)
	      scan_rtx (insn, &XEXP (note, 0), NO_REGS, terminate_dead, OP_IN);
	}
      if (insn == bb->end)
	break;
    }

  /* Since we close every chain when we find a REG_DEAD note, anything that
     is still open lives past the basic block, so it can't be renamed.  */
  return closed_chains;
}

/* Dump all def/use chains in CHAINS to RTL_DUMP_FILE.  They are
   printed in reverse order as that's how we build them.  */

static void
dump_def_use_chain (chains)
     struct du_chain *chains;
{
  while (chains)
    {
      struct du_chain *this = chains;
      int r = REGNO (*this->loc);
      int nregs = HARD_REGNO_NREGS (r, GET_MODE (*this->loc));
      fprintf (rtl_dump_file, "Register %s (%d):", reg_names[r], nregs);
      while (this)
	{
	  fprintf (rtl_dump_file, " %d [%s]", INSN_UID (this->insn),
		   reg_class_names[this->class]);
	  this = this->next_use;
	}
      fprintf (rtl_dump_file, "\n");
      chains = chains->next_chain;
    }
}
