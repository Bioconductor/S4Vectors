#include "../inst/include/S4Vectors_defines.h"
#include <string.h>

#define DEBUG_S4VECTORS 1

#define INIT_STATIC_SYMBOL(NAME) \
{ \
	if (NAME ## _symbol == NULL) \
		NAME ## _symbol = install(# NAME); \
}


/* safe_arithm.c */

void _reset_ovflow_flag();

int _get_ovflow_flag();

int _safe_int_add(
	int x,
	int y
);

int _safe_int_mult(
	int x,
	int y
);


/* sort_utils.c */

void _sort_int_array(
	int *x,
	int nelt,
	int desc
);

void _get_order_of_int_array(
	const int *x,
	int nelt,
	int desc,
	int *out,
	int out_shift
);

void _compare_int_pairs(
	const int *a1,
	const int *b1,
	int nelt1,
	const int *a2,
	const int *b2,
	int nelt2,
	int *out,
	int out_len,
	int with_warning
);

int _int_pairs_are_sorted(
	const int *a,
	const int *b,
	int nelt,
	int desc,
	int strict
);

void _get_order_of_int_pairs(
	const int *a,
	const int *b,
	int nelt,
	int desc,
	int *out,
	int out_shift
);

void _get_matches_of_ordered_int_pairs(
	const int *a1,
	const int *b1,
	const int *o1,
	int nelt1,
	const int *a2,
	const int *b2,
	const int *o2,
	int nelt2,
	int nomatch,
	int *out,
	int out_shift
);

void _get_order_of_int_quads(
	const int *a,
	const int *b,
	const int *c,
	const int *d,
	int nelt,
	int desc,
	int *out,
	int out_shift
);

void _get_matches_of_ordered_int_quads(
	const int *a1,
	const int *b1,
	const int *c1,
	const int *d1,
	const int *o1,
	int nelt1,
	const int *a2,
	const int *b2,
	const int *c2,
	const int *d2,
	const int *o2,
	int nelt2,
	int nomatch,
	int *out,
	int out_shift
);


/* hash_utils.c */

struct htab _new_htab(int n);

int _get_hbucket_val(
	const struct htab *htab,
	int bucket_idx
);

void _set_hbucket_val(
	struct htab *htab,
	int bucket_idx,
	int val
);


/* AEbufs.c */

SEXP debug_AEbufs();

SEXP AEbufs_use_malloc(SEXP x);

int _get_new_buflength(int buflength);

int _IntAE_get_nelt(const IntAE *int_ae);

int _IntAE_set_nelt(
	IntAE *int_ae,
	int nelt
);

void _IntAE_set_val(
	const IntAE *int_ae,
	int val
);

IntAE _new_IntAE(
	int buflength,
	int nelt,
	int val
);

void _IntAE_insert_at(
	IntAE *int_ae,
	int at,
	int val
);

void _IntAE_append(
	IntAE *int_ae,
	const int *newvals,
	int nnewval
);

void _IntAE_delete_at(
	IntAE *int_ae,
	int at
);

void _IntAE_shift(
	const IntAE *int_ae,
	int shift
);

void _IntAE_sum_and_shift(
	const IntAE *int_ae1,
	const IntAE *int_ae2,
	int shift
);

void _IntAE_append_shifted_vals(
	IntAE *int_ae,
	const int *newvals,
	int nnewval,
	int shift
);

void _IntAE_qsort(
	const IntAE *int_ae,
	int desc
);

void _IntAE_delete_adjdups(IntAE *int_ae);

SEXP _new_INTEGER_from_IntAE(const IntAE *int_ae);

IntAE _new_IntAE_from_INTEGER(SEXP x);

IntAE _new_IntAE_from_CHARACTER(
	SEXP x,
	int keyshift
);

int _IntAEAE_get_nelt(const IntAEAE *int_aeae);

int _IntAEAE_set_nelt(
	IntAEAE *int_aeae,
	int nelt
);

IntAEAE _new_IntAEAE(
	int buflength,
	int nelt
);

void _IntAEAE_insert_at(
	IntAEAE *int_aeae,
	int at,
	const IntAE *int_ae
);

void _IntAEAE_eltwise_append(
	const IntAEAE *int_aeae1,
	const IntAEAE *int_aeae2
);

void _IntAEAE_shift(
	const IntAEAE *int_aeae,
	int shift
);

void _IntAEAE_sum_and_shift(
	const IntAEAE *int_aeae1,
	const IntAEAE *int_aeae2,
	int shift
);

SEXP _new_LIST_from_IntAEAE(
	const IntAEAE *int_aeae,
	int mode
);

IntAEAE _new_IntAEAE_from_LIST(SEXP x);

SEXP _IntAEAE_toEnvir(
	const IntAEAE *int_aeae,
	SEXP envir,
	int keyshift
);

int _IntPairAE_get_nelt(const IntPairAE *intpair_ae);

int _IntPairAE_set_nelt(
	IntPairAE *intpair_ae,
	int nelt
);

IntPairAE _new_IntPairAE(
	int buflength,
	int nelt
);

void _IntPairAE_insert_at(
	IntPairAE *intpair_ae,
	int at,
	int a,
	int b
);

int _IntPairAEAE_get_nelt(const IntPairAEAE *intpair_aeae);

int _IntPairAEAE_set_nelt(
	IntPairAEAE *intpair_aeae,
	int nelt
);

IntPairAEAE _new_IntPairAEAE(
	int buflength,
	int nelt
);

void _IntPairAEAE_insert_at(
	IntPairAEAE *intpair_aeae,
	int at,
	const IntPairAE *intpair_ae
);

int _CharAE_get_nelt(const CharAE *char_ae);

int _CharAE_set_nelt(
	CharAE *char_ae,
	int nelt
);

CharAE _new_CharAE(int buflength);

CharAE _new_CharAE_from_string(const char *string);

void _CharAE_insert_at(
	CharAE *char_ae,
	int at,
	char c
);

void _append_string_to_CharAE(
	CharAE *char_ae,
	const char *string
);

void _CharAE_delete_at(
	CharAE *char_ae,
	int at,
	int nelt
);

SEXP _new_RAW_from_CharAE(const CharAE *char_ae);

SEXP _new_LOGICAL_from_CharAE(const CharAE *char_ae);

int _CharAEAE_get_nelt(const CharAEAE *char_aeae);

int _CharAEAE_set_nelt(
	CharAEAE *char_aeae,
	int nelt
);

CharAEAE _new_CharAEAE(
	int buflength,
	int nelt
);

void _CharAEAE_insert_at(
	CharAEAE *char_aeae,
	int at,
	const CharAE *char_ae
);

void _append_string_to_CharAEAE(
	CharAEAE *char_aeae,
	const char *string
);

SEXP _new_CHARACTER_from_CharAEAE(const CharAEAE *char_aeae);

SEXP AEbufs_free();


/* SEXP_utils.c */

const char *_get_classname(SEXP x);


/* anyMissing.c */

SEXP anyMissing(SEXP x);


/* vector_utils.c */

int _vector_memcmp(
	SEXP x1,
	int x1_offset,
	SEXP x2,
	int x2_offset,
	int nelt
);

void _vector_memcpy(
	SEXP out,
	int out_offset,
	SEXP in,
	int in_offset,
	int nelt
);

SEXP sapply_NROW(SEXP x);

SEXP vector_subsetByRanges(
	SEXP x,
	SEXP start,
	SEXP width
);

SEXP vector_seqselect(
	SEXP x,
	SEXP start,
	SEXP width
);


/* logical_utils.c */

SEXP logical_as_compact_bitvector(SEXP x);

SEXP compact_bitvector_as_logical(SEXP x, SEXP length_out);

SEXP subset_compact_bitvector(SEXP x, SEXP subscript);

SEXP compact_bitvector_bit_count(SEXP x);

SEXP compact_bitvector_last_bit(SEXP x);

SEXP compact_bitvector_set_op(SEXP query, SEXP ref, SEXP align);


/* int_utils.c */

SEXP Integer_any_missing_or_outside(SEXP x, SEXP lower, SEXP upper);

int _sum_non_neg_ints(
	const int *x,
	int x_len,
	const char *varname
);

SEXP Integer_sum_non_neg_vals(SEXP x);

SEXP Integer_diff_with_0(SEXP x);

SEXP Integer_diff_with_last(SEXP x, SEXP last);

SEXP Integer_order(
	SEXP x,
	SEXP decreasing
);

int _check_integer_pairs(
	SEXP a,
	SEXP b,
	const int **a_p,
	const int **b_p,
	const char *a_argname,
	const char *b_argname
);

SEXP Integer_compare2(
	SEXP a1,
	SEXP b1,
	SEXP a2,
	SEXP b2
);

SEXP Integer_sorted2(
	SEXP a,
	SEXP b,
	SEXP decreasing,
	SEXP strictly
);

SEXP Integer_order2(
	SEXP a,
	SEXP b,
	SEXP decreasing
);

SEXP Integer_match2_quick(
	SEXP a1,
	SEXP b1,
	SEXP a2,
	SEXP b2,
	SEXP nomatch
);

SEXP Integer_selfmatch2_quick(
	SEXP a,
	SEXP b
);

SEXP Integer_match2_hash(
	SEXP a1,
	SEXP b1,
	SEXP a2,
	SEXP b2,
	SEXP nomatch
);

SEXP Integer_selfmatch2_hash(
	SEXP a,
	SEXP b
);

int _check_integer_quads(
	SEXP a,
	SEXP b,
	SEXP c,
	SEXP d,
	const int **a_p,
	const int **b_p,
	const int **c_p,
	const int **d_p,
	const char *a_argname,
	const char *b_argname,
	const char *c_argname,
	const char *d_argname
);

SEXP Integer_order4(
	SEXP a,
	SEXP b,
	SEXP c,
	SEXP d,
	SEXP decreasing
);

SEXP Integer_match4_quick(
	SEXP a1,
	SEXP b1,
	SEXP c1,
	SEXP d1,
	SEXP a2,
	SEXP b2,
	SEXP c2,
	SEXP d2,
	SEXP nomatch
);

SEXP Integer_selfmatch4_quick(
	SEXP a,
	SEXP b,
	SEXP c,
	SEXP d
);

SEXP Integer_match4_hash(
	SEXP a1,
	SEXP b1,
	SEXP c1,
	SEXP d1,
	SEXP a2,
	SEXP b2,
	SEXP c2,
	SEXP d2,
	SEXP nomatch
);

SEXP Integer_selfmatch4_hash(
	SEXP a,
	SEXP b,
	SEXP c,
	SEXP d
);

SEXP Integer_tabulate2(
	SEXP x,
	SEXP nbins,
	SEXP weight,
	SEXP strict
);

SEXP Integer_explode_bits(
	SEXP x,
	SEXP bitpos
);

SEXP Integer_sorted_merge(
	SEXP x,
	SEXP y
);

SEXP Integer_mseq(
	SEXP from,
	SEXP to
);

SEXP Integer_fancy_mseq(
	SEXP lengths,
	SEXP offset,
	SEXP rev
);

SEXP _find_interv_and_start_from_width(
	const int *x,
	int x_len,
	const int *width,
	int width_len
);

SEXP findIntervalAndStartFromWidth(
	SEXP x,
	SEXP vec
);


/* str_utils.c */

SEXP unstrsplit_list(SEXP x, SEXP sep);

SEXP safe_strexplode(SEXP s);

SEXP strsplit_as_list_of_ints(SEXP x, SEXP sep);

SEXP svn_time();


/* eval_utils.c */

SEXP top_prenv(SEXP nm, SEXP env);

SEXP top_prenv_dots(SEXP env);


/* subsetting_internals.c */

SEXP vector_extract_window(SEXP x, SEXP start, SEXP end);


/* Hits_class.c */

SEXP _new_Hits(
	int *q_hits,
	const int *s_hits,
	int nhit,
	int q_len,
	int s_len,
	int already_sorted
);

SEXP Hits_new(
	SEXP q_hits,
	SEXP s_hits,
	SEXP q_len,
	SEXP s_len,
	SEXP revmap_envir
);

int _get_select_mode(SEXP select);

SEXP select_hits(
	SEXP q_hits,
	SEXP s_hits,
	SEXP q_len,
	SEXP select
);

SEXP make_all_group_inner_hits(
	SEXP group_sizes,
	SEXP hit_type
);


/* Rle_class.c */

SEXP _logical_Rle_constructor(
	const int *values,
	int nvalues,
	const int *lengths,
	int buflength
);

SEXP _integer_Rle_constructor(
	const int *values,
	int nvalues,
	const int *lengths,
	int buflength
);

SEXP _numeric_Rle_constructor(
	const double *values,
	int nvalues,
	const int *lengths,
	int buflength
);

SEXP _complex_Rle_constructor(
	const Rcomplex *values,
	int nvalues,
	const int *lengths,
	int buflength
);

SEXP _character_Rle_constructor(
	SEXP values,
	const int *lengths,
	int buflength
);

SEXP _raw_Rle_constructor(
	const Rbyte *values,
	int nvalues,
	const int *lengths,
	int buflength
);

SEXP Rle_constructor(
	SEXP values,
	SEXP lengths,
	SEXP check,
	SEXP buflength
);

SEXP Rle_start(SEXP x);

SEXP Rle_end(SEXP x);

SEXP Rle_getStartEndRunAndOffset(
	SEXP x,
	SEXP start,
	SEXP end
);

SEXP Rle_window_aslist(
	SEXP x,
	SEXP runStart,
	SEXP runEnd,
	SEXP offsetStart,
	SEXP offsetEnd
);

SEXP Rle_window(
	SEXP x,
	SEXP runStart,
	SEXP runEnd,
	SEXP offsetStart,
	SEXP offsetEnd,
	SEXP ans
);

SEXP _seqselect_Rle(
	SEXP x,
	const int *start,
	const int *width,
	int length
);

SEXP Rle_seqselect(
	SEXP x,
	SEXP start,
	SEXP width
);


/* Rle_utils.c */

SEXP Rle_runsum(
	SEXP x,
	SEXP k,
	SEXP na_rm
);

SEXP Rle_runwtsum(
	SEXP x,
	SEXP k,
	SEXP wt,
	SEXP na_rm
);

SEXP Rle_runq(
	SEXP x,
	SEXP k,
	SEXP which,
	SEXP na_rm
);


/* List_class.c */

const char *_get_List_elementType(SEXP x);

void _set_List_elementType(
	SEXP x,
	const char *type
);


/* SimpleList_class.c */

SEXP _new_SimpleList(
	const char *classname,
	SEXP listData
);


/* DataFrame_class.c */

SEXP _new_DataFrame(
	const char *classname,
	SEXP vars,
	SEXP rownames,
	SEXP nrows
);

