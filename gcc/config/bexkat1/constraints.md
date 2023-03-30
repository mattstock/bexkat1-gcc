;; -------------------------------------------------------------------------
;; Constraint definitions for Bexkat1
;; -------------------------------------------------------------------------

(define_constraint "A"
  "An absolute address."
  (and (match_code "mem")
       (ior (match_test "GET_CODE (XEXP (op, 0)) == SYMBOL_REF")
	    (match_test "GET_CODE (XEXP (op, 0)) == LABEL_REF")
	    (match_test "GET_CODE (XEXP (op, 0)) == CONST"))))

(define_constraint "B"
  "An offset address."
  (and (match_code "mem")
       (match_test "bexkat1_offset_address_p (op)")))

(define_constraint "W"
  "A register indirect memory operand."
  (and (match_code "mem")
       (match_test "REG_P (XEXP (op, 0))
		    && REGNO_OK_FOR_BASE_P (REGNO (XEXP (op, 0)))")))

(define_constraint "I"
  "A 15-bit positive constant"
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival < 32768")))

(define_constraint "J"
  "A 15-bit signed constant"
  (and (match_code "const_int")
       (match_test "ival >= -16384 && ival <= 16383")))

(define_constraint "O"
  "The constant zero"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "K"
  "The constant one"
  (and (match_code "const_int")
       (match_test "ival == 1")))
