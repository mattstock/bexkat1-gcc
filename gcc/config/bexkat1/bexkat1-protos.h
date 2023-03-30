/* Prototypes for bexkat1.c functions used in the md file & elsewhere. */
extern void  bexkat1_expand_prologue (void);
extern void  bexkat1_expand_epilogue (void);
extern int   bexkat1_initial_elimination_offset (int, int);
extern void  bexkat1_print_operand (FILE *, rtx, int);
extern void  bexkat1_print_operand_address (FILE *, rtx);
extern bool  bexkat1_offset_address_p (rtx);
extern enum bexkat1_function_kind bexkat1_get_function_kind(tree);
extern poly_int64 bexkat1_push_rounding (poly_int64);
