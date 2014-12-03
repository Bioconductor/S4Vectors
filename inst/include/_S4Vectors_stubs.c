#include "S4Vectors_interface.h"

#define DEFINE_CCALLABLE_STUB(retT, stubname, Targs, args) \
typedef retT(*__ ## stubname ## _funtype__)Targs; \
retT stubname Targs \
{ \
	static __ ## stubname ## _funtype__ fun = NULL; \
	if (fun == NULL) \
		fun = (__ ## stubname ## _funtype__) R_GetCCallable("S4Vectors", "_" #stubname); \
	return fun args; \
}

/*
 * Using the above macro when retT (the returned type) is void will make Sun
 * Studio 12 C compiler unhappy. So we need to use the following macro to
 * handle that case.
 */
#define DEFINE_NOVALUE_CCALLABLE_STUB(stubname, Targs, args) \
typedef void(*__ ## stubname ## _funtype__)Targs; \
void stubname Targs \
{ \
	static __ ## stubname ## _funtype__ fun = NULL; \
	if (fun == NULL) \
		fun = (__ ## stubname ## _funtype__) R_GetCCallable("S4Vectors", "_" #stubname); \
	fun args; \
	return; \
}


/*
 * Stubs for callables defined in safe_arithm.c
 */

DEFINE_NOVALUE_CCALLABLE_STUB(reset_ovflow_flag,
	(),
	()
)

DEFINE_CCALLABLE_STUB(int, get_ovflow_flag,
	(),
	()
)

DEFINE_CCALLABLE_STUB(int, safe_int_add,
	(int x, int y),
	(    x,     y)
)

DEFINE_CCALLABLE_STUB(int, safe_int_mult,
	(int x, int y),
	(    x,     y)
)

/*
 * Stubs for callables defined in sort_utils.c
 */

DEFINE_NOVALUE_CCALLABLE_STUB(sort_int_array,
	(int *x, int nelt, int desc),
	(     x,     nelt,     desc)
)

DEFINE_NOVALUE_CCALLABLE_STUB(get_order_of_int_array,
	(const int *x, int nelt, int desc, int *out, int out_shift),
	(           x,     nelt,     desc,      out,     out_shift)
)

DEFINE_NOVALUE_CCALLABLE_STUB(get_order_of_int_pairs,
	(const int *a, const int *b, int nelt, int desc, int *out, int out_shift),
	(           a,            b,     nelt,     desc,      out,     out_shift)
)

DEFINE_NOVALUE_CCALLABLE_STUB(get_matches_of_ordered_int_pairs,
	(const int *a1, const int *b1, const int *o1, int nelt1, const int *a2, const int *b2, const int *o2, int nelt2, int nomatch, int *out, int out_shift),
	(           a1,            b1,            o1,     nelt1,            a2,            b2,            o2,     nelt2,     nomatch,      out,     out_shift)
)

DEFINE_NOVALUE_CCALLABLE_STUB(get_order_of_int_quads,
	(const int *a, const int *b, const int *c, const int *d, int nelt, int desc, int *out, int out_shift),
	(           a,            b,            c,            d,     nelt,     desc,      out,     out_shift)
)

DEFINE_NOVALUE_CCALLABLE_STUB(get_matches_of_ordered_int_quads,
	(const int *a1, const int *b1, const int *c1, const int *d1, const int *o1, int nelt1, const int *a2, const int *b2, const int *c2, const int *d2, const int *o2, int nelt2, int nomatch, int *out, int out_shift),
	(           a1,            b1,            c1,            d1,            o1,     nelt1,            a2,            b2,            c2,            d2,            o2,     nelt2,     nomatch,      out,     out_shift)
)

/*
 * Stubs for callables defined in hash_utils.c
 */

DEFINE_CCALLABLE_STUB(struct htab, new_htab,
	(int n),
	(    n)
)

DEFINE_CCALLABLE_STUB(int, get_hbucket_val,
	(const struct htab *htab, int bucket_idx),
	(                   htab,     bucket_idx)
)

DEFINE_NOVALUE_CCALLABLE_STUB(set_hbucket_val,
	(struct htab *htab, int bucket_idx, int val),
	(             htab,     bucket_idx,     val)
)

/*
 * Stubs for callables defined in AEbufs.c
 */

DEFINE_CCALLABLE_STUB(int, get_new_buflength,
	(int buflength),
	(    buflength)
)

DEFINE_CCALLABLE_STUB(int, IntAE_get_nelt,
	(const IntAE *int_ae),
	(             int_ae)
)

DEFINE_CCALLABLE_STUB(int, IntAE_set_nelt,
	(IntAE *int_ae, int nelt),
	(       int_ae,     nelt)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_set_val,
	(const IntAE *int_ae, int val),
	(             int_ae,     val)
)

DEFINE_CCALLABLE_STUB(IntAE, new_IntAE,
	(int buflength, int nelt, int val),
	(    buflength,     nelt,     val)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_insert_at,
	(IntAE *int_ae, int at, int val),
	(       int_ae,     at,     val)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_append,
	(IntAE *int_ae, const int *newvals, int nnewval),
	(       int_ae,            newvals,     nnewval)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_delete_at,
	(IntAE *int_ae, int at),
	(       int_ae,     at)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_shift,
	(const IntAE *int_ae, int shift),
	(             int_ae,     shift)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_sum_and_shift,
	(const IntAE *int_ae1, const IntAE *int_ae2, int shift),
	(             int_ae1,              int_ae2,     shift)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_append_shifted_vals,
	(IntAE *int_ae, const int *newvals, int nnewval, int shift),
	(       int_ae,            newvals,     nnewval,     shift)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_qsort,
	(const IntAE *int_ae, int desc),
	(             int_ae,     desc)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_delete_adjdups,
	(IntAE *int_ae),
	(       int_ae)
)

DEFINE_CCALLABLE_STUB(SEXP, new_INTEGER_from_IntAE,
	(const IntAE *int_ae),
	(             int_ae)
)

DEFINE_CCALLABLE_STUB(IntAE, new_IntAE_from_INTEGER,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(IntAE, new_IntAE_from_CHARACTER,
	(SEXP x, int keyshift),
	(     x,     keyshift)
)

DEFINE_CCALLABLE_STUB(int, IntAEAE_get_nelt,
	(const IntAEAE *int_aeae),
	(               int_aeae)
)

DEFINE_CCALLABLE_STUB(int, IntAEAE_set_nelt,
	(IntAEAE *int_aeae, int nelt),
	(         int_aeae,     nelt)
)

DEFINE_CCALLABLE_STUB(IntAEAE, new_IntAEAE,
	(int buflength, int nelt),
	(    buflength,     nelt)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAEAE_insert_at,
	(IntAEAE *int_aeae, int at, const IntAE *int_ae),
	(         int_aeae,     at,              int_ae)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAEAE_eltwise_append,
	(const IntAEAE *int_aeae1, const IntAEAE *int_aeae2),
	(               int_aeae1,                int_aeae2)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAEAE_shift,
	(const IntAEAE *int_aeae, int shift),
	(               int_aeae,     shift)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAEAE_sum_and_shift,
	(const IntAEAE *int_aeae1, const IntAEAE *int_aeae2, int shift),
	(               int_aeae1,                int_aeae2,     shift)
)

DEFINE_CCALLABLE_STUB(SEXP, new_LIST_from_IntAEAE,
	(const IntAEAE *int_aeae, int mode),
	(               int_aeae,     mode)
)

DEFINE_CCALLABLE_STUB(IntAEAE, new_IntAEAE_from_LIST,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, IntAEAE_toEnvir,
	(const IntAEAE *int_aeae, SEXP envir, int keyshift),
	(               int_aeae,      envir,     keyshift)
)

DEFINE_CCALLABLE_STUB(int, IntPairAE_get_nelt,
	(const IntPairAE *intpair_ae),
	(                 intpair_ae)
)

DEFINE_CCALLABLE_STUB(int, IntPairAE_set_nelt,
	(IntPairAE *intpair_ae, int nelt),
	(           intpair_ae,     nelt)
)

DEFINE_CCALLABLE_STUB(IntPairAE, new_IntPairAE,
	(int buflength, int nelt),
	(    buflength,     nelt)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntPairAE_insert_at,
	(IntPairAE *intpair_ae, int at, int a, int b),
	(           intpair_ae,     at,     a,     b)
)

DEFINE_CCALLABLE_STUB(int, IntPairAEAE_get_nelt,
	(const IntPairAEAE *intpair_aeae),
	(                   intpair_aeae)
)

DEFINE_CCALLABLE_STUB(int, IntPairAEAE_set_nelt,
	(IntPairAEAE *intpair_aeae, int nelt),
	(             intpair_aeae,     nelt)
)

DEFINE_CCALLABLE_STUB(IntPairAEAE, new_IntPairAEAE,
	(int buflength, int nelt),
	(    buflength,     nelt)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntPairAEAE_insert_at,
	(IntPairAEAE *intpair_aeae, int at, const IntPairAE *intpair_ae),
	(             intpair_aeae,     at,                  intpair_ae)
)

DEFINE_CCALLABLE_STUB(int, CharAE_get_nelt,
	(const CharAE *char_ae),
	(              char_ae)
)

DEFINE_CCALLABLE_STUB(int, CharAE_set_nelt,
	(CharAE *char_ae, int nelt),
	(        char_ae,     nelt)
)

DEFINE_CCALLABLE_STUB(CharAE, new_CharAE,
	(int buflength),
	(    buflength)
)

DEFINE_CCALLABLE_STUB(CharAE, new_CharAE_from_string,
	(const char *string),
	(            string)
)

DEFINE_NOVALUE_CCALLABLE_STUB(CharAE_insert_at,
	(CharAE *char_ae, int at, char c),
	(        char_ae,     at,      c)
)

DEFINE_NOVALUE_CCALLABLE_STUB(append_string_to_CharAE,
	(CharAE *char_ae, const char *string),
	(        char_ae,             string)
)

DEFINE_NOVALUE_CCALLABLE_STUB(CharAE_delete_at,
	(CharAE *char_ae, int at, int nelt),
	(        char_ae,     at,     nelt)
)

DEFINE_CCALLABLE_STUB(SEXP, new_RAW_from_CharAE,
	(const CharAE *char_ae),
	(              char_ae)
)

DEFINE_CCALLABLE_STUB(SEXP, new_LOGICAL_from_CharAE,
	(const CharAE *char_ae),
	(              char_ae)
)

DEFINE_CCALLABLE_STUB(int, CharAEAE_get_nelt,
	(const CharAEAE *char_aeae),
	(                char_aeae)
)

DEFINE_CCALLABLE_STUB(int, CharAEAE_set_nelt,
	(CharAEAE *char_aeae, int nelt),
	(          char_aeae,     nelt)
)

DEFINE_CCALLABLE_STUB(CharAEAE, new_CharAEAE,
	(int buflength, int nelt),
	(    buflength,     nelt)
)

DEFINE_NOVALUE_CCALLABLE_STUB(CharAEAE_insert_at,
	(CharAEAE *char_aeae, int at, const CharAE *char_ae),
	(          char_aeae,     at,               char_ae)
)

DEFINE_NOVALUE_CCALLABLE_STUB(append_string_to_CharAEAE,
	(CharAEAE *char_aeae, const char *string),
	(          char_aeae,             string)
)

DEFINE_CCALLABLE_STUB(SEXP, new_CHARACTER_from_CharAEAE,
	(const CharAEAE *char_aeae),
	(                char_aeae)
)

/*
 * Stubs for callables defined in SEXP_utils.c
 */

DEFINE_CCALLABLE_STUB(const char *, get_classname,
	(SEXP x),
	(     x)
)

/*
 * Stubs for callables defined in vector_utils.c
 */

DEFINE_CCALLABLE_STUB(int, vector_memcmp,
	(SEXP x1, int x1_offset, SEXP x2, int x2_offset, int nelt),
	(     x1,     x1_offset,      x2,     x2_offset,     nelt)
)

DEFINE_NOVALUE_CCALLABLE_STUB(vector_memcpy,
	(SEXP out, int out_offset, SEXP in, int in_offset, int nelt),
	(     out,     out_offset,      in,     in_offset,     nelt)
)

/*
 * Stubs for callables defined in int_utils.c
 */

DEFINE_CCALLABLE_STUB(int, sum_non_neg_ints,
	(const int *x, int x_len, const char *varname),
	(           x,     x_len,             varname)
)

DEFINE_CCALLABLE_STUB(int, check_integer_pairs,
	(SEXP a, SEXP b, const int **a_p, const int **b_p, const char *a_argname, const char *b_argname),
	(     a,      b,             a_p,             b_p,             a_argname,             b_argname)
)

DEFINE_CCALLABLE_STUB(SEXP, find_interv_and_start_from_width,
	(const int *x, int x_len, const int *width, int width_len),
	(           x,     x_len,            width,     width_len)
)

/*
 * Stubs for callables defined in Hits_class.c
 */

DEFINE_CCALLABLE_STUB(SEXP, new_Hits,
	(int *q_hits, const int *s_hits, int nhit, int q_len, int s_len, int already_sorted),
	(     q_hits,            s_hits,     nhit,     q_len,     s_len,     already_sorted)
)

DEFINE_CCALLABLE_STUB(int, get_select_mode,
	(SEXP select),
	(     select)
)

/*
 * Stubs for callables defined in Rle_class.c
 */

DEFINE_CCALLABLE_STUB(SEXP, logical_Rle_constructor,
	(const int *values, int nvalues, const int *lengths, int buflength),
	(           values,     nvalues,            lengths,     buflength)
)

DEFINE_CCALLABLE_STUB(SEXP, integer_Rle_constructor,
	(const int *values, int nvalues, const int *lengths, int buflength),
	(           values,     nvalues,            lengths,     buflength)
)

DEFINE_CCALLABLE_STUB(SEXP, numeric_Rle_constructor,
	(const double *values, int nvalues, const int *lengths, int buflength),
	(              values,     nvalues,            lengths,     buflength)
)

DEFINE_CCALLABLE_STUB(SEXP, complex_Rle_constructor,
	(const Rcomplex *values, int nvalues, const int *lengths, int buflength),
	(                values,     nvalues,            lengths,     buflength)
)

DEFINE_CCALLABLE_STUB(SEXP, character_Rle_constructor,
	(SEXP values, const int *lengths, int buflength),
	(     values,            lengths,     buflength)
)

DEFINE_CCALLABLE_STUB(SEXP, raw_Rle_constructor,
	(const Rbyte *values, int nvalues, const int *lengths, int buflength),
	(             values,     nvalues,            lengths,     buflength)
)

DEFINE_CCALLABLE_STUB(SEXP, seqselect_Rle,
	(SEXP x, const int *start, const int *width, int length),
	(     x,            start,            width,     length)
)

/*
 * Stubs for callables defined in List_class.c
 */

DEFINE_CCALLABLE_STUB(const char *, get_List_elementType,
	(SEXP x),
	(     x)
)

DEFINE_NOVALUE_CCALLABLE_STUB(set_List_elementType,
	(SEXP x, const char *type),
	(     x,             type)
)

/*
 * Stubs for callables defined in SimpleList_class.c
 */

DEFINE_CCALLABLE_STUB(SEXP, new_SimpleList,
	(const char *classname, SEXP listData),
	(            classname,      listData)
)

