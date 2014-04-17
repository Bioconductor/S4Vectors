#include "S4Vectors.h"

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#define REGISTER_CCALLABLE(fun) \
	R_RegisterCCallable("S4Vectors", #fun, (DL_FUNC) &fun)


static const R_CallMethodDef callMethods[] = {

/* AEbufs.c */
	CALLMETHOD_DEF(debug_AEbufs, 0),
	CALLMETHOD_DEF(AEbufs_use_malloc, 1),
	CALLMETHOD_DEF(AEbufs_free, 0),

/* anyMissing.c */
	CALLMETHOD_DEF(anyMissing, 1),

	{NULL, NULL, 0}
};


void R_init_S4Vectors(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);

/* safe_arithm.c */
	REGISTER_CCALLABLE(_reset_ovflow_flag);
	REGISTER_CCALLABLE(_get_ovflow_flag);
	REGISTER_CCALLABLE(_safe_int_add);
	REGISTER_CCALLABLE(_safe_int_mult);

/* sort_utils.c */
	REGISTER_CCALLABLE(_sort_int_array);
	REGISTER_CCALLABLE(_get_order_of_int_array);
	REGISTER_CCALLABLE(_get_order_of_int_pairs);
	REGISTER_CCALLABLE(_get_matches_of_ordered_int_pairs);
	REGISTER_CCALLABLE(_get_order_of_int_quads);
	REGISTER_CCALLABLE(_get_matches_of_ordered_int_quads);

/* hash_utils.c */
	REGISTER_CCALLABLE(_new_htab);
	REGISTER_CCALLABLE(_get_hbucket_val);
	REGISTER_CCALLABLE(_set_hbucket_val);

/* AEbufs.c */
	REGISTER_CCALLABLE(_get_new_buflength);
	REGISTER_CCALLABLE(_IntAE_get_nelt);
	REGISTER_CCALLABLE(_IntAE_set_nelt);
	REGISTER_CCALLABLE(_IntAE_set_val);
	REGISTER_CCALLABLE(_new_IntAE);
	REGISTER_CCALLABLE(_IntAE_insert_at);
	REGISTER_CCALLABLE(_IntAE_append);
	REGISTER_CCALLABLE(_IntAE_delete_at);
	REGISTER_CCALLABLE(_IntAE_shift);
	REGISTER_CCALLABLE(_IntAE_sum_and_shift);
	REGISTER_CCALLABLE(_IntAE_append_shifted_vals);
	REGISTER_CCALLABLE(_IntAE_qsort);
	REGISTER_CCALLABLE(_IntAE_delete_adjdups);
	REGISTER_CCALLABLE(_new_INTEGER_from_IntAE);
	REGISTER_CCALLABLE(_new_IntAE_from_INTEGER);
	REGISTER_CCALLABLE(_new_IntAE_from_CHARACTER);
	REGISTER_CCALLABLE(_IntAEAE_get_nelt);
	REGISTER_CCALLABLE(_IntAEAE_set_nelt);
	REGISTER_CCALLABLE(_new_IntAEAE);
	REGISTER_CCALLABLE(_IntAEAE_insert_at);
	REGISTER_CCALLABLE(_IntAEAE_eltwise_append);
	REGISTER_CCALLABLE(_IntAEAE_shift);
	REGISTER_CCALLABLE(_IntAEAE_sum_and_shift);
	REGISTER_CCALLABLE(_new_LIST_from_IntAEAE);
	REGISTER_CCALLABLE(_new_IntAEAE_from_LIST);
	REGISTER_CCALLABLE(_IntAEAE_toEnvir);
	REGISTER_CCALLABLE(_RangeAE_get_nelt);
	REGISTER_CCALLABLE(_RangeAE_set_nelt);
	REGISTER_CCALLABLE(_new_RangeAE);
	REGISTER_CCALLABLE(_RangeAE_insert_at);
	REGISTER_CCALLABLE(_RangeAEAE_get_nelt);
	REGISTER_CCALLABLE(_RangeAEAE_set_nelt);
	REGISTER_CCALLABLE(_new_RangeAEAE);
	REGISTER_CCALLABLE(_RangeAEAE_insert_at);
	REGISTER_CCALLABLE(_CharAE_get_nelt);
	REGISTER_CCALLABLE(_CharAE_set_nelt);
	REGISTER_CCALLABLE(_new_CharAE);
	REGISTER_CCALLABLE(_new_CharAE_from_string);
	REGISTER_CCALLABLE(_CharAE_insert_at);
	REGISTER_CCALLABLE(_append_string_to_CharAE);
	REGISTER_CCALLABLE(_CharAE_delete_at);
	REGISTER_CCALLABLE(_new_RAW_from_CharAE);
	REGISTER_CCALLABLE(_new_LOGICAL_from_CharAE);
	REGISTER_CCALLABLE(_CharAEAE_get_nelt);
	REGISTER_CCALLABLE(_CharAEAE_set_nelt);
	REGISTER_CCALLABLE(_new_CharAEAE);
	REGISTER_CCALLABLE(_CharAEAE_insert_at);
	REGISTER_CCALLABLE(_append_string_to_CharAEAE);
	REGISTER_CCALLABLE(_new_CHARACTER_from_CharAEAE);
	return;
}

