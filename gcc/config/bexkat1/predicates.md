;; -------------------------------------------------------------------------
;; Predicate definitions for Bexkat1
;; -------------------------------------------------------------------------

;; Nonzero if OP can be an operand to an add/inc/dec instruction.

(define_predicate "bexkat1_general_movsrc_operand"
  (match_code "mem,reg,subreg,symbol_ref,label_ref,const_double,const,const_int")
{
  /* Any (MEM LABEL_REF) is OK.  That is a pc-relative load.  */
  if (MEM_P (op) && GET_CODE (XEXP (op, 0)) == LABEL_REF)
    return 1;

  if (MEM_P (op)
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 0)) == REG
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT
      && IN_RANGE (INTVAL (XEXP (XEXP (op, 0), 1)), -32768, 32767))
    return 1;

  return general_operand (op, mode);
})

(define_predicate "bexkat1_arith_operand"
  (ior (match_code "reg")
       (match_code "const_int,const_double")))
