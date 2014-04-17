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

int _RangeAE_get_nelt(const RangeAE *range_ae);

int _RangeAE_set_nelt(
	RangeAE *range_ae,
	int nelt
);

RangeAE _new_RangeAE(
	int buflength,
	int nelt
);

void _RangeAE_insert_at(
	RangeAE *range_ae,
	int at,
	int start,
	int width
);

int _RangeAEAE_get_nelt(const RangeAEAE *range_aeae);

int _RangeAEAE_set_nelt(
	RangeAEAE *range_aeae,
	int nelt
);

RangeAEAE _new_RangeAEAE(
	int buflength,
	int nelt
);

void _RangeAEAE_insert_at(
	RangeAEAE *range_aeae,
	int at,
	const RangeAE *range_ae
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


/* anyMissing.c */

SEXP anyMissing(SEXP x);

