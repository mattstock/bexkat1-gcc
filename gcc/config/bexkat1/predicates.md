;; Predicate definitions for Bexkat1
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
;; Predicates
;; -------------------------------------------------------------------------

;; Nonzero if OP can be an operand to an add/inc/dec instruction.

(define_predicate "bexkat1_add_operand"
  (ior (match_code "reg") (match_code "const_int")))

;; Nonzero if OP can be an operand to an sub/dec instruction.

(define_predicate "bexkat1_sub_operand"
  (ior (match_code "reg") (match_code "const_int")))