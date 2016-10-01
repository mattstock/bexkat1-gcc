;; Machine description for Bexkat1
;; Copyright (C) 2009-2015 Free Software Foundation, Inc.
;; Contributed by Anthony Green <green@bexkat1logic.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------
;; Bexkat1 specific constraints, predicates and attributes
;; -------------------------------------------------------------------------

(include "constraints.md")
(include "predicates.md")

(define_constants
  [(BEXKAT1_FP 30)
   (BEXKAT1_SP 31)])

; Most instructions are four bytes long.
(define_attr "length" "" (const_int 4))

;; -------------------------------------------------------------------------
;; nop instruction
;; -------------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;; -------------------------------------------------------------------------
;; Arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	  (plus:SI
            (match_operand:SI 1 "register_operand" "0,r,r")
            (match_operand:SI 2 "bexkat1_add_operand" "K,i,r")))]
  ""
  "@
  inc\\t%0
  addi\\t%0, %1, %2
  add\\t%0, %1, %2")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	  (minus:SI
	   (match_operand:SI 1 "register_operand" "0,r,r")
	   (match_operand:SI 2 "bexkat1_sub_operand" "K,i,r")))]
  ""
  "@
  dec\\t%0
  subi\\t%0, %1, %2
  sub\\t%0, %1, %2")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (mult:SI
	   (match_operand:SI 1 "register_operand" "r,r")
	   (match_operand:SI 2 "register_operand" "i,r")))]
  ""
  "@
  muli\\t%0, %1, %2
  mul\\t%0, %1, %2")

(define_code_iterator EXTEND [sign_extend zero_extend])
(define_code_attr mul [(sign_extend "mul") (zero_extend "mulu")])

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (div:SI
	   (match_operand:SI 1 "register_operand" "r,r")
	   (match_operand:SI 2 "register_operand" "i,r")))]
  ""
  "@
  divi\\t %0, %1, %2
  div\\t%0, %1, %2")

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (udiv:SI
	   (match_operand:SI 1 "register_operand" "r,r")
	   (match_operand:SI 2 "register_operand" "i,r")))]
  ""
  "@
  diviu\\t%0, %1, %2
  divu\\t%0, %1, %2")

(define_insn "modsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (mod:SI
	   (match_operand:SI 1 "register_operand" "r,r")
	   (match_operand:SI 2 "register_operand" "i,r")))]
  ""
  "@
  modi\\t%0, %1, %2
  mod\\t%0, %1, %2")

(define_insn "umodsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (umod:SI
	   (match_operand:SI 1 "register_operand" "r,r")
	   (match_operand:SI 2 "register_operand" "i,r")))]
  ""
  "@
  modui\\t%0, %1, %2
  modu\\t%0, %1, %2")

;; -------------------------------------------------------------------------
;; Unary arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "neg\\t%0, %1")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "com\\t%0, %1")

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(and:SI (match_operand:SI 1 "register_operand" "r,r")
		(match_operand:SI 2 "register_operand" "i,r")))]
  ""
  "@
  andi\\t%0, %1, %2
  and\\t%0, %1, %2")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(xor:SI (match_operand:SI 1 "register_operand" "r,r")
		(match_operand:SI 2 "register_operand" "i,r")))]
  ""
  "@
  xori\\t%0, %1, %2
  xor\\t%0, %1, %2")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ior:SI (match_operand:SI 1 "register_operand" "r,r")
		(match_operand:SI 2 "register_operand" "i,r")))]
  ""
  "@
  ori\\t%0, %1, %2
  or\\t%0,%1,%2")

;; -------------------------------------------------------------------------
;; Shifters
;; -------------------------------------------------------------------------

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "register_operand" "r,r")
		   (match_operand:SI 2 "register_operand" "i,r")))]
  ""
  "@
  lsli\\t%0, %1, %2
  lsl\\t%0, %1, %2")

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "register_operand" "i,r")))]
  ""
  "@
  asri\\t%0, %1, %2
  asr\\t%0, %1, %2")

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "register_operand" "i,r")))]
  ""
  "@
  lsri\\t%0, %1, %2
  lsr\\t%0, %1, %2")

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

;; SImode

;; Push a register onto the stack
(define_insn "movsi_push"
  [(set (mem:SI (pre_dec:SI (reg:SI BEXKAT1_SP)))
  	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "push\\t%0")

;; Pop a register from the stack
(define_insn "movsi_pop"
  [(set (match_operand:SI 0 "register_operand" "=r")
  	(mem:SI (post_inc:SI (reg:SI BEXKAT1_SP))))]
  ""
  "pop\\t%0")

(define_expand "movsi"
   [(set (match_operand:SI 0 "general_operand" "")
 	(match_operand:SI 1 "general_operand" ""))]
   ""
  "
{
  if (! (reload_in_progress || reload_completed)) {
    if (MEM_P (operands[0])) {
      operands[1] = force_reg (SImode, operands[1]);
      if (GET_CODE (XEXP (operands[0], 0)) == MEM)
        operands[0] = gen_rtx_MEM(SImode, force_reg (SImode, XEXP (operands[0], 0)));
    } else
      if (GET_CODE (operands[1]) == MEM
          && GET_CODE (XEXP (operands[1], 0)) == MEM)
        operands[1] = gen_rtx_MEM(SImode, force_reg (SImode, XEXP (operands[1], 0)));
  }
}")

(define_insn "*movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,B,r,W,r,A")
  	(match_operand:SI 1 "bexkat1_general_movsrc_operand" "r,I,i,B,r,W,r,A,r"))]
  "register_operand(operands[0], SImode) ||
   register_operand(operands[1], SImode)"
  "@
   mov\\t%0, %1
   ldiu\\t%0, %1
   ldi\\t%0, %1
   ld.l\\t%0, %1
   st.l\\t%1, %0
   ld.l\\t%0, %1
   st.l\\t%1, %0
   ldd.l\\t%0, %1
   std.l\\t%1, %0"
  [(set_attr "length" "4,4,8,4,4,4,4,8,8")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
        (zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "0,r,B,A")))]
  ""
  "@
  nop
  mov\\t%0, %1
  ld.b\\t%0, %1
  ldd.b\\t%0, %1"
  [(set_attr "length"   "4,4,4,8")])

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,B,A")))]
  ""
  "@
  mov\\t%0, %1
  ld\\t%0, %1
  ldd\\t%0, %1"
  [(set_attr "length"   "4,4,8")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r")))]
  ""
  "ext.b\\t%0, %1"
  [(set_attr "length" "4")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r")))]
  ""
  "ext\\t%0, %1"
  [(set_attr "length" "4")])

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (QImode, operands[1]);
}")

(define_insn "*movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,B,r,W,r,A")
	(match_operand:QI 1 "bexkat1_general_movsrc_operand" "r,i,B,r,W,r,A,r"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)"
  "@
   mov\\t%0, %1
   ldiu\\t%0, %1
   ld.b\\t%0, %1
   st.b\\t%1, %0
   ld.b\\t%0, %1
   st.b\\t%1, %0
   ldd.b\\t%0, %1
   std.b\\t%1, %0"
  [(set_attr "length"	"4,4,4,4,4,4,8,8")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (HImode, operands[1]);
}")

(define_insn "*movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,B,r,W,r,A")
	(match_operand:HI 1 "general_operand" "r,i,B,r,W,r,A,r"))]
  "register_operand (operands[0], HImode)
   || register_operand (operands[1], HImode)"
  "@
   mov\\t%0, %1
   ldiu\\t%0, %1
   ld\\t%0, %1
   st\\t%1, %0
   ld\\t%0, %1
   st\\t%1, %0
   ldd\\t%0, %1
   std\\t%1, %0"
  [(set_attr "length"	"4,4,4,4,4,4,8,8")])

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------

(define_constants
  [(CC_REG 35)])

(define_expand "cbranchsi4"
  [(set (reg:CC CC_REG)
        (compare:CC
         (match_operand:SI 1 "general_operand" "")
         (match_operand:SI 2 "general_operand" "")))
   (set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
                       [(reg:CC CC_REG) (const_int 0)])
                      (label_ref (match_operand 3 "" ""))
                      (pc)))]
  ""
  "
  /* Force the compare operands into registers.  */
  if (GET_CODE (operands[1]) != REG)
	operands[1] = force_reg (SImode, operands[1]);
  if (GET_CODE (operands[2]) != REG)
	operands[2] = force_reg (SImode, operands[2]);
  ")

(define_insn "*cmpsi"
  [(set (reg:CC CC_REG)
	(compare
	 (match_operand:SI 0 "register_operand" "r")
	 (match_operand:SI 1 "register_operand"	"r")))]
  ""
  "cmp\\t%0, %1")

;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

(define_code_iterator cond [ne eq lt ltu gt gtu ge le geu leu])
(define_code_attr CC [(ne "ne") (eq "eq") (lt "lt") (ltu "ltu") 
		      (gt "gt") (gtu "gtu") (ge "ge") (le "le") 
		      (geu "geu") (leu "leu") ])
(define_code_attr rCC [(ne "eq") (eq "ne") (lt "ge") (ltu "geu") 
		       (gt "le") (gtu "leu") (ge "lt") (le "gt") 
		       (geu "ltu") (leu "gtu") ])

(define_insn "*b<cond:code>"
  [(set (pc)
	(if_then_else (cond (reg:CC CC_REG)
			    (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  if (get_attr_length (insn) == 4)
    return "b<CC>\\t%l0";
  else
    return "b<rCC>\\t.+8\n\tjmpd\t%l0";
}
  [(set (attr "length")
        (if_then_else (lt (abs (minus (pc) (match_dup 0))) (const_int 2046))
                      (const_int 4) (const_int 12)))])

;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------

(define_expand "call"
  [(call (match_operand:SI 0 "memory_operand" "")
		(match_operand 1 "general_operand" ""))]
  ""
{
  gcc_assert (MEM_P (operands[0]));
})

(define_insn "*call"
  [(call (mem:SI (match_operand:SI
		  0 "nonmemory_operand" "r,i"))
	 (match_operand 1 "" ""))]
  ""
  "@
   jsr\\t%a0
   jsrd\\t%0"
  [(set_attr "length"	"4,8")])

(define_expand "call_value"
  [(set (match_operand 0 "" "")
		(call (match_operand:SI 1 "memory_operand" "")
		 (match_operand 2 "" "")))]
  ""
{
  gcc_assert (MEM_P (operands[1]));
})

(define_insn "*call_value"
  [(set (match_operand 0 "register_operand" "=r")
	(call (mem:SI (match_operand:SI
		       1 "immediate_operand" "i"))
	      (match_operand 2 "" "")))]
  ""
  "jsrd\\t%1"
  [(set_attr "length"	"8")])

(define_insn "*call_value_indirect"
  [(set (match_operand 0 "register_operand" "=r")
	(call (mem:SI (match_operand:SI
		       1 "register_operand" "r"))
	      (match_operand 2 "" "")))]
  ""
  "jsr\\t%a1"
  [(set_attr "length"	"4")])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "nonimmediate_operand" "r"))]
  ""
  "jmp\\t%0")

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jmpd\\t%l0"
  [(set_attr "length"	"8")])


;; -------------------------------------------------------------------------
;; Prologue & Epilogue
;; -------------------------------------------------------------------------

(define_expand "prologue"
  [(clobber (const_int 0))]
  ""
  "
{
  bexkat1_expand_prologue ();
  DONE;
}
")

(define_expand "epilogue"
  [(return)]
  ""
  "
{
  bexkat1_expand_epilogue ();
  DONE;
}
")

(define_insn "returner"
  [(return)]
  "reload_completed"
  "rts")
