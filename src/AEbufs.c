/****************************************************************************
 *                          Auto-Extending buffers                          *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "S4Vectors.h"
#include <stdlib.h>  /* for malloc, free, realloc */
#include <limits.h>  /* for INT_MAX */


#define MAX_BUFLENGTH_INC 33554432UL  // 2^25
/* IMPORTANT: Keep MAX_BUFLENGTH <= R_XLEN_T_MAX (i.e. 2^52, see Rinternals.h)
   otherwise casting a buffer length (size_t) to R_xlen_t will not do the
   right thing (undefined behavior).
   For now we set MAX_BUFLENGTH to 4294967296 (i.e. 2^32) only. This is big
   enough to support buffers of the length of the human genome. */
#define MAX_BUFLENGTH (128UL * MAX_BUFLENGTH_INC)

/* Guaranteed to return a new buflength > 'buflength', or to raise an error. */
size_t _increase_buflength(size_t buflength)
{
	if (buflength >= MAX_BUFLENGTH)
		error("_increase_buflength(): MAX_BUFLENGTH reached");
	if (buflength == 0)
		return 128;
	if (buflength <= MAX_BUFLENGTH_INC)
		return 2 * buflength;
	buflength += MAX_BUFLENGTH_INC;
	if (buflength <= MAX_BUFLENGTH)
		return buflength;
	return MAX_BUFLENGTH;
}


/****************************************************************************
 * Low-level memory management.
 */

static int use_malloc = 0;

SEXP AEbufs_use_malloc(SEXP x)
{
	use_malloc = LOGICAL(x)[0];
	return R_NilValue;
}

static void *alloc2(size_t nmemb, size_t size)
{
	void *ptr;

	if (nmemb > MAX_BUFLENGTH)
		error("S4Vectors internal error in alloc2(): "
		      "buffer is too big");
	if (use_malloc) {
		//printf("alloc2: nmemb=%d\n", nmemb);
		size *= nmemb;
		ptr = malloc(size);
		if (ptr == NULL)
			error("S4Vectors internal error in alloc2(): "
			      "cannot allocate memory");
	} else {
		ptr = (void *) R_alloc(nmemb, (int) size);
	}
	return ptr;
}

/* 'new_nmemb' must be > 'old_nmemb'. */
static void *realloc2(void *ptr, size_t old_nmemb, size_t new_nmemb,
		      size_t size)
{
	void *new_ptr;

	if (new_nmemb > MAX_BUFLENGTH)
		error("S4Vectors internal error in realloc2(): "
		      "buffer is too big");
	if (new_nmemb <= old_nmemb)
		error("S4Vectors internal error in realloc2(): "
		      "'new_nmemb' must be > 'old_nmemb'");
	if (old_nmemb == 0)
		return alloc2(new_nmemb, size);
	if (use_malloc) {
		//printf("realloc2: new_nmemb=%lu old_nmemb=%lu\n",
		//       new_nmemb, old_nmemb);
		size *= new_nmemb;
		new_ptr = realloc(ptr, size);
		if (new_ptr == NULL)
			error("S4Vectors internal error in realloc2(): "
			      "cannot reallocate memory");
	} else {
		new_ptr = (void *) R_alloc(new_nmemb, (int) size);
		memcpy(new_ptr, ptr, old_nmemb * size);
	}
	return new_ptr;
}


/****************************************************************************
 * IntAE buffers
 */

#define	INTAE_POOL_MAXLEN 256
static IntAE *IntAE_pool[INTAE_POOL_MAXLEN];
static int IntAE_pool_len = 0;

size_t _IntAE_get_nelt(const IntAE *ae)
{
	return ae->_nelt;
}

size_t _IntAE_set_nelt(IntAE *ae, size_t nelt)
{
	if (nelt > ae->_buflength)
		error("S4Vectors internal error in _IntAE_set_nelt(): "
		      "trying to set a nb of buffer elements that exceeds "
		      "the buffer length");
	return ae->_nelt = nelt;
}

static IntAE *new_empty_IntAE()
{
	IntAE *ae;

	if (use_malloc && IntAE_pool_len >= INTAE_POOL_MAXLEN)
		error("S4Vectors internal error in new_empty_IntAE(): "
		      "IntAE pool is full");
	ae = (IntAE *) alloc2(1, sizeof(IntAE));
	ae->_buflength = ae->_nelt = 0;
	if (use_malloc)
		IntAE_pool[IntAE_pool_len++] = ae;
	return ae;
}

void _IntAE_set_val(const IntAE *ae, int val)
{
	size_t ae_nelt, i;
	int *elt_p;

	ae_nelt = _IntAE_get_nelt(ae);
	elt_p = ae->elts;
	for (i = 0; i < ae_nelt; i++)
		*(elt_p++) = val;
	return;
}

static void IntAE_extend(IntAE *ae, size_t new_buflength)
{
	ae->elts = (int *) realloc2(ae->elts, ae->_buflength,
				    new_buflength, sizeof(int));
	ae->_buflength = new_buflength;
	return;
}

static int IntAE_extend_if_full(IntAE *ae)
{
	if (_IntAE_get_nelt(ae) < ae->_buflength)
		return 0;
	IntAE_extend(ae, _increase_buflength(ae->_buflength));
	return 1;
}

void _IntAE_insert_at(IntAE *ae, size_t at, int val)
{
	size_t ae_nelt, i;
	int *elt1_p;
	const int *elt2_p;

	ae_nelt = _IntAE_get_nelt(ae);
	if (at > ae_nelt)
		error("S4Vectors internal error in _IntAE_insert_at(): "
		      "trying to insert a buffer element at an invalid "
		      "buffer position");
	IntAE_extend_if_full(ae);
	elt1_p = ae->elts + ae_nelt;
	elt2_p = elt1_p - 1;
	for (i = ae_nelt; i > at; i--)
		*(elt1_p--) = *(elt2_p--);
	*elt1_p = val;
	_IntAE_set_nelt(ae, ae_nelt + 1);
	return;
}

IntAE *_new_IntAE(size_t buflength, size_t nelt, int val)
{
	IntAE *ae;

	ae = new_empty_IntAE();
	if (buflength != 0) {
		IntAE_extend(ae, buflength);
		_IntAE_set_nelt(ae, nelt);
		_IntAE_set_val(ae, val);
	}
	return ae;
}

void _IntAE_append(IntAE *ae, const int *newvals, size_t nnewval)
{
	size_t ae_nelt, new_nelt;
	int *dest;

	ae_nelt = _IntAE_get_nelt(ae);
	new_nelt = ae_nelt + nnewval;
	if (new_nelt > ae->_buflength)
		IntAE_extend(ae, new_nelt);
	dest = ae->elts + ae_nelt;
	memcpy(dest, newvals, nnewval * sizeof(int));
	_IntAE_set_nelt(ae, new_nelt);
	return;
}

/*
 * Delete 'nelt' elements, starting at position 'at'.
 * Calling _IntAE_delete_at(x, at, nelt) is equivalent to calling
 * _IntAE_delete_at(x, at, 1) nelt times.
 */
void _IntAE_delete_at(IntAE *ae, size_t at, size_t nelt)
{
	int *elt1_p;
	const int *elt2_p;
	size_t ae_nelt, i2;

	if (nelt == 0)
		return;
	elt1_p = ae->elts + at;
	elt2_p = elt1_p + nelt;
	ae_nelt = _IntAE_get_nelt(ae);
	for (i2 = at + nelt; i2 < ae_nelt; i2++)
		*(elt1_p++) = *(elt2_p++);
	_IntAE_set_nelt(ae, ae_nelt - nelt);
	return;
}

void _IntAE_shift(const IntAE *ae, size_t offset, int shift)
{
	size_t ae_nelt, i;
	int *elt_p;

	ae_nelt = _IntAE_get_nelt(ae);
	elt_p = ae->elts + offset;
	for (i = offset; i < ae_nelt; i++)
		*(elt_p++) += shift;
	return;
}

/*
 * Left and right IntAE buffers must have the same length.
 */
void _IntAE_sum_and_shift(const IntAE *ae1, const IntAE *ae2, int shift)
{
	size_t ae1_nelt, ae2_nelt, i;
	int *elt1_p;
	const int *elt2_p;

	ae1_nelt = _IntAE_get_nelt(ae1);
	ae2_nelt = _IntAE_get_nelt(ae2);
	if (ae1_nelt != ae2_nelt)
		error("S4Vectors internal error in _IntAE_sum(): "
		      "the 2 IntAE buffers to sum must have the same length");
	elt1_p = ae1->elts;
	elt2_p = ae2->elts;
	for (i = 0; i < ae1_nelt; i++)
		*(elt1_p++) += *(elt2_p++) + shift;
	return;
}

void _IntAE_qsort(const IntAE *ae, size_t offset, int desc)
{
	size_t ae_nelt;

	ae_nelt = _IntAE_get_nelt(ae);
	if (offset > ae_nelt)
		error("S4Vectors internal error in _IntAE_qsort(): "
		      "'offset' must be < nb of elements in buffer");
	_sort_int_array(ae->elts + offset, ae_nelt - offset, desc);
	return;
}

/*
 * Delete repeated elements i.e. same semantic as 'uniq' command in Unix.
 * To get the R unique() behavior (modulo re-ordering of the elements), call
 * _IntAE_qsort() first.
 */
void _IntAE_uniq(IntAE *ae, size_t offset)
{
	size_t ae_nelt, i2;
	int *elt1_p;
	const int *elt2_p;

	ae_nelt = _IntAE_get_nelt(ae);
	if (offset > ae_nelt)
		error("S4Vectors internal error in _IntAE_uniq(): "
		      "'offset' must be < nb of elements in buffer");
	if (ae_nelt - offset <= 1)
		return;
	elt1_p = ae->elts + offset;
	elt2_p = elt1_p + 1;
	for (i2 = offset + 1; i2 < ae_nelt; i2++) {
		if (*elt2_p != *elt1_p)
			*(++elt1_p) = *elt2_p;
		elt2_p++;
	}
	_IntAE_set_nelt(ae, elt1_p - ae->elts + 1);
	return;
}

SEXP _new_INTEGER_from_IntAE(const IntAE *ae)
{
	size_t ae_nelt;
	SEXP ans;

	ae_nelt = _IntAE_get_nelt(ae);
	/* ae_nelt <= R_XLEN_T_MAX so casting is safe. */
	PROTECT(ans = NEW_INTEGER((R_xlen_t) ae_nelt));
	memcpy(INTEGER(ans), ae->elts, ae_nelt * sizeof(int));
	UNPROTECT(1);
	return ans;
}

IntAE *_new_IntAE_from_INTEGER(SEXP x)
{
	size_t x_len;
	IntAE *ae;

	/* Casting R_xlen_t to size_t is safe. */
	x_len = (size_t) XLENGTH(x);
	ae = _new_IntAE(x_len, 0, 0);
	_IntAE_append(ae, INTEGER(x), x_len);
	return ae;
}

IntAE *_new_IntAE_from_CHARACTER(SEXP x, int keyshift)
{
	size_t x_len, i;
	IntAE *ae;
	int *elt_p;

	/* Casting R_xlen_t to size_t is safe. */
	x_len = (size_t) XLENGTH(x);
	ae = _new_IntAE(x_len, 0, 0);
	elt_p = ae->elts;
	for (i = 0; i < x_len; i++) {
		sscanf(CHAR(STRING_ELT(x, i)), "%d", elt_p);
		*(elt_p++) += keyshift;
	}
	_IntAE_set_nelt(ae, x_len);
	return ae;
}

/* Must be used on a malloc-based IntAE */
static void IntAE_free(IntAE *ae)
{
	if (ae->_buflength != 0)
		free(ae->elts);
	free(ae);
	return;
}

static void flush_IntAE_pool()
{
	IntAE *ae;

	while (IntAE_pool_len > 0) {
		IntAE_pool_len--;
		ae = IntAE_pool[IntAE_pool_len];
		IntAE_free(ae);
	}
	return;
}

static int remove_from_IntAE_pool(const IntAE *ae)
{
	int i;
	IntAE **ae1_p, **ae2_p;

	i = IntAE_pool_len;
	while (--i >= 0 && IntAE_pool[i] != ae) {;}
	if (i < 0)
		return -1;
	ae1_p = IntAE_pool + i;
	ae2_p = ae1_p + 1;
	for (i = i + 1; i < IntAE_pool_len; i++)
		*(ae1_p++) = *(ae2_p++);
	IntAE_pool_len--;
	return 0;
}


/****************************************************************************
 * IntAEAE buffers
 */

#define	INTAEAE_POOL_MAXLEN 256
static IntAEAE *IntAEAE_pool[INTAEAE_POOL_MAXLEN];
static int IntAEAE_pool_len = 0;

size_t _IntAEAE_get_nelt(const IntAEAE *aeae)
{
	return aeae->_nelt;
}

size_t _IntAEAE_set_nelt(IntAEAE *aeae, size_t nelt)
{
	if (nelt > aeae->_buflength)
		error("S4Vectors internal error in _IntAEAE_set_nelt(): "
		      "trying to set a nb of buffer elements that exceeds "
		      "the buffer length");
	return aeae->_nelt = nelt;
}

static IntAEAE *new_empty_IntAEAE()
{
	IntAEAE *aeae;

	if (use_malloc && IntAEAE_pool_len >= INTAEAE_POOL_MAXLEN)
		error("S4Vectors internal error in new_empty_IntAEAE(): "
		      "IntAEAE pool is full");
	aeae = (IntAEAE *) alloc2(1, sizeof(IntAEAE));
	aeae->_buflength = aeae->_nelt = 0;
	if (use_malloc)
		IntAEAE_pool[IntAEAE_pool_len++] = aeae;
	return aeae;
}

static void IntAEAE_extend(IntAEAE *aeae, size_t new_buflength)
{
	size_t old_buflength, i;

	old_buflength = aeae->_buflength;
	aeae->elts = (IntAE **) realloc2(aeae->elts, old_buflength,
					 new_buflength, sizeof(IntAE *));
	for (i = old_buflength; i < new_buflength; i++)
		aeae->elts[i] = NULL;
	aeae->_buflength = new_buflength;
	return;
}

static int IntAEAE_extend_if_full(IntAEAE *aeae)
{
	if (_IntAEAE_get_nelt(aeae) < aeae->_buflength)
		return 0;
	IntAEAE_extend(aeae, _increase_buflength(aeae->_buflength));
	return 1;
}

void _IntAEAE_insert_at(IntAEAE *aeae, size_t at, IntAE *ae)
{
	size_t aeae_nelt, i;
	IntAE **ae1_p, **ae2_p;

	aeae_nelt = _IntAEAE_get_nelt(aeae);
	if (at > aeae_nelt)
		error("S4Vectors internal error in _IntAEAE_insert_at(): "
		      "trying to insert a buffer element at an invalid "
		      "buffer position");
	IntAEAE_extend_if_full(aeae);
	if (use_malloc && remove_from_IntAE_pool(ae) == -1)
		error("S4Vectors internal error in _IntAEAE_insert_at(): "
		      "IntAE to insert cannot be found in pool for removal");
	ae1_p = aeae->elts + aeae_nelt;
	ae2_p = ae1_p - 1;
	for (i = aeae_nelt; i > at; i--)
		*(ae1_p--) = *(ae2_p--);
	*ae1_p = ae;
	_IntAEAE_set_nelt(aeae, aeae_nelt + 1);
	return;
}

IntAEAE *_new_IntAEAE(size_t buflength, size_t nelt)
{
	IntAEAE *aeae;
	size_t i;
	IntAE *ae;

	aeae = new_empty_IntAEAE();
	if (buflength != 0) {
		IntAEAE_extend(aeae, buflength);
		for (i = 0; i < nelt; i++) {
			ae = new_empty_IntAE();
			_IntAEAE_insert_at(aeae, i, ae);
		}
	}
	return aeae;
}

/*
 * Parallel append: left and right IntAEAE buffers must have the same length.
 */
void _IntAEAE_pappend(const IntAEAE *aeae1, const IntAEAE *aeae2)
{
	size_t aeae1_nelt, aeae2_nelt, i;
	IntAE *ae1;
	const IntAE *ae2;

	aeae1_nelt = _IntAEAE_get_nelt(aeae1);
	aeae2_nelt = _IntAEAE_get_nelt(aeae2);
	if (aeae1_nelt != aeae2_nelt)
		error("S4Vectors internal error in _IntAEAE_pappend(): "
		      "the 2 IntAEAE buffers to pappend must have "
		      "the same length");
	for (i = 0; i < aeae1_nelt; i++) {
		ae1 = aeae1->elts[i];
		ae2 = aeae2->elts[i];
		_IntAE_append(ae1, ae2->elts, _IntAE_get_nelt(ae2));
	}
	return;
}

void _IntAEAE_shift(const IntAEAE *aeae, int shift)
{
	size_t aeae_nelt, i;
	IntAE *ae;

	aeae_nelt = _IntAEAE_get_nelt(aeae);
	for (i = 0; i < aeae_nelt; i++) {
		ae = aeae->elts[i];
		_IntAE_shift(ae, 0, shift);
	}
	return;
}

/*
 * Left and right IntAEAE buffers must have the same length.
 */
void _IntAEAE_sum_and_shift(const IntAEAE *aeae1, const IntAEAE *aeae2,
		int shift)
{
	size_t aeae1_nelt, aeae2_nelt, i;
	IntAE *ae1;
	const IntAE *ae2;

	aeae1_nelt = _IntAEAE_get_nelt(aeae1);
	aeae2_nelt = _IntAEAE_get_nelt(aeae2);
	if (aeae1_nelt != aeae2_nelt)
		error("S4Vectors internal error in _IntAEAE_sum_and_shift(): "
		      "the 2 IntAEAE buffers to sum_and_shift must have "
		      "the same length");
	for (i = 0; i < aeae1_nelt; i++) {
		ae1 = aeae1->elts[i];
		ae2 = aeae2->elts[i];
		_IntAE_sum_and_shift(ae1, ae2, shift);
	}
	return;
}

/*
 * 'mode' controls how empty list elements should be represented:
 *   0 -> integer(0); 1 -> NULL; 2 -> NA
 */
SEXP _new_LIST_from_IntAEAE(const IntAEAE *aeae, int mode)
{
	size_t aeae_nelt, i;
	SEXP ans, ans_elt;
	const IntAE *ae;

	aeae_nelt = _IntAEAE_get_nelt(aeae);
	/* ae_nelt <= R_XLEN_T_MAX so casting is safe. */
	PROTECT(ans = NEW_LIST((R_xlen_t) aeae_nelt));
	for (i = 0; i < aeae_nelt; i++) {
		ae = aeae->elts[i];
		if (_IntAE_get_nelt(ae) != 0 || mode == 0) {
			PROTECT(ans_elt = _new_INTEGER_from_IntAE(ae));
		} else if (mode == 1) {
			continue;
		} else {
			// Not sure new LOGICALs are initialized with NAs,
			// need to check! If not, then LOGICAL(ans_elt)[0] must
			// be set to NA but I don't know how to do this :-/
			PROTECT(ans_elt = NEW_LOGICAL(1));
		}
		SET_VECTOR_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

IntAEAE *_new_IntAEAE_from_LIST(SEXP x)
{
	size_t x_len;
	IntAEAE *aeae;
	size_t i;
	SEXP x_elt;
	IntAE *ae;

	/* Casting R_xlen_t to size_t is safe. */
	x_len = (size_t) XLENGTH(x);
	aeae = _new_IntAEAE(x_len, 0);
	for (i = 0; i < aeae->_buflength; i++) {
		x_elt = VECTOR_ELT(x, i);
		if (TYPEOF(x_elt) != INTSXP)
			error("S4Vectors internal error in "
			      "_new_IntAEAE_from_LIST(): "
			      "not all elements in the list "
			      "are integer vectors");
		ae = _new_IntAE_from_INTEGER(x_elt);
		_IntAEAE_insert_at(aeae, i, ae);
	}
	return aeae;
}

SEXP _IntAEAE_toEnvir(const IntAEAE *aeae, SEXP envir, int keyshift)
{
	size_t aeae_nelt, i;
	const IntAE *ae;
	char key[11];
	SEXP value;

	aeae_nelt = _IntAEAE_get_nelt(aeae);
	for (i = 0; i < aeae_nelt; i++) {
		ae = aeae->elts[i];
		if (_IntAE_get_nelt(ae) == 0)
			continue;
		//snprintf(key, sizeof(key), "%d", i + keyshift);
		snprintf(key, sizeof(key), "%010lu", i + keyshift);
		PROTECT(value = _new_INTEGER_from_IntAE(ae));
		defineVar(install(key), value, envir);
		UNPROTECT(1);
	}
	return envir;
}

/* Must be used on a malloc-based IntAEAE */
static void IntAEAE_free(IntAEAE *aeae)
{
	size_t buflength, i;
	IntAE *ae;

	buflength = aeae->_buflength;
	for (i = 0; i < buflength; i++) {
		ae = aeae->elts[i];
		if (ae != NULL)
			IntAE_free(ae);
	}
	if (buflength != 0)
		free(aeae->elts);
	free(aeae);
	return;
}

static void flush_IntAEAE_pool()
{
	IntAEAE *aeae;

	while (IntAEAE_pool_len > 0) {
		IntAEAE_pool_len--;
		aeae = IntAEAE_pool[IntAEAE_pool_len];
		IntAEAE_free(aeae);
	}
	return;
}


/****************************************************************************
 * IntPairAE buffers
 */

#define	INTPAIRAE_POOL_MAXLEN 256
static IntPairAE *IntPairAE_pool[INTPAIRAE_POOL_MAXLEN];
static int IntPairAE_pool_len = 0;

size_t _IntPairAE_get_nelt(const IntPairAE *ae)
{
	return _IntAE_get_nelt(ae->a);
}

size_t _IntPairAE_set_nelt(IntPairAE *ae, size_t nelt)
{
	_IntAE_set_nelt(ae->a, nelt);
	_IntAE_set_nelt(ae->b, nelt);
	return nelt;
}

static IntPairAE *new_empty_IntPairAE()
{
	IntAE *a, *b;
	IntPairAE *ae;

	if (use_malloc && IntPairAE_pool_len >= INTPAIRAE_POOL_MAXLEN)
		error("S4Vectors internal error in new_empty_IntPairAE(): "
		      "IntPairAE pool is full");
	a = new_empty_IntAE();
	b = new_empty_IntAE();
	ae = (IntPairAE *) alloc2(1, sizeof(IntPairAE));
	ae->a = a;
	ae->b = b;
	if (use_malloc) {
		if (remove_from_IntAE_pool(a) == -1 ||
		    remove_from_IntAE_pool(b) == -1)
			error("S4Vectors internal error "
			      "in new_empty_IntPairAE(): "
			      "IntAEs to stick in IntPairAE cannot be found "
			      "in pool for removal");
		IntPairAE_pool[IntPairAE_pool_len++] = ae;
	}
	return ae;
}

static void IntPairAE_extend(IntPairAE *ae, size_t new_buflength)
{
	IntAE_extend(ae->a, new_buflength);
	IntAE_extend(ae->b, new_buflength);
	return;
}

void _IntPairAE_insert_at(IntPairAE *ae, size_t at, int a, int b)
{
	_IntAE_insert_at(ae->a, at, a);
	_IntAE_insert_at(ae->b, at, b);
	return;
}

IntPairAE *_new_IntPairAE(size_t buflength, size_t nelt)
{
	IntPairAE *ae;

	ae = new_empty_IntPairAE();
	if (buflength != 0) {
		IntPairAE_extend(ae, buflength);
		/* Elements are NOT initialized. */
		_IntPairAE_set_nelt(ae, nelt);
	}
	return ae;
}

/* Must be used on a malloc-based IntPairAE */
static void IntPairAE_free(IntPairAE *ae)
{
	IntAE_free(ae->a);
	IntAE_free(ae->b);
	free(ae);
	return;
}

static void flush_IntPairAE_pool()
{
	IntPairAE *ae;

	while (IntPairAE_pool_len > 0) {
		IntPairAE_pool_len--;
		ae = IntPairAE_pool[IntPairAE_pool_len];
		IntPairAE_free(ae);
	}
	return;
}

static int remove_from_IntPairAE_pool(const IntPairAE *ae)
{
	int i;
	IntPairAE **ae1_p, **ae2_p;

	i = IntPairAE_pool_len;
	while (--i >= 0 && IntPairAE_pool[i] != ae) {;}
	if (i < 0)
		return -1;
	ae1_p = IntPairAE_pool + i;
	ae2_p = ae1_p + 1;
	for (i = i + 1; i < IntPairAE_pool_len; i++)
		*(ae1_p++) = *(ae2_p++);
	IntPairAE_pool_len--;
	return 0;
}


/****************************************************************************
 * IntPairAEAE buffers
 */

#define	INTPAIRAEAE_POOL_MAXLEN 256
static IntPairAEAE *IntPairAEAE_pool[INTPAIRAEAE_POOL_MAXLEN];
static int IntPairAEAE_pool_len = 0;

size_t _IntPairAEAE_get_nelt(const IntPairAEAE *aeae)
{
	return aeae->_nelt;
}

size_t _IntPairAEAE_set_nelt(IntPairAEAE *aeae, size_t nelt)
{
	if (nelt > aeae->_buflength)
		error("S4Vectors internal error in _IntPairAEAE_set_nelt(): "
		      "trying to set a nb of buffer elements that exceeds "
		      "the buffer length");
	return aeae->_nelt = nelt;
}

static IntPairAEAE *new_empty_IntPairAEAE()
{
	IntPairAEAE *aeae;

	if (use_malloc && IntPairAEAE_pool_len >= INTPAIRAEAE_POOL_MAXLEN)
		error("S4Vectors internal error in new_empty_IntPairAEAE(): "
		      "IntPairAEAE pool is full");
	aeae = (IntPairAEAE *) alloc2(1, sizeof(IntPairAEAE));
	aeae->_buflength = aeae->_nelt = 0;
	if (use_malloc)
		IntPairAEAE_pool[IntPairAEAE_pool_len++] = aeae;
	return aeae;
}

static void IntPairAEAE_extend(IntPairAEAE *aeae, size_t new_buflength)
{
	size_t old_buflength, i;

	old_buflength = aeae->_buflength;
	aeae->elts = (IntPairAE **) realloc2(aeae->elts, old_buflength,
					     new_buflength,
					     sizeof(IntPairAE *));
	for (i = old_buflength; i < new_buflength; i++)
		aeae->elts[i] = NULL;
	aeae->_buflength = new_buflength;
	return;
}

static int IntPairAEAE_extend_if_full(IntPairAEAE *aeae)
{
	if (_IntPairAEAE_get_nelt(aeae) < aeae->_buflength)
		return 0;
	IntPairAEAE_extend(aeae, _increase_buflength(aeae->_buflength));
	return 1;
}

void _IntPairAEAE_insert_at(IntPairAEAE *aeae, size_t at, IntPairAE *ae)
{
	size_t aeae_nelt, i;
	IntPairAE **ae1_p, **ae2_p;

	aeae_nelt = _IntPairAEAE_get_nelt(aeae);
	if (at > aeae_nelt)
		error("S4Vectors internal error in _IntPairAEAE_insert_at(): "
		      "trying to insert a buffer element at an invalid "
		      "buffer position");
	IntPairAEAE_extend_if_full(aeae);
	if (use_malloc && remove_from_IntPairAE_pool(ae) == -1)
		error("S4Vectors internal error in _IntPairAEAE_insert_at(): "
		      "IntPairAE to insert cannot be found in pool for "
		      "removal");
	ae1_p = aeae->elts + aeae_nelt;
	ae2_p = ae1_p - 1;
	for (i = aeae_nelt; i > at; i--)
		*(ae1_p--) = *(ae2_p--);
	*ae1_p = ae;
	_IntPairAEAE_set_nelt(aeae, aeae_nelt + 1);
	return;
}

IntPairAEAE *_new_IntPairAEAE(size_t buflength, size_t nelt)
{
	IntPairAEAE *aeae;
	size_t i;
	IntPairAE *ae;

	aeae = new_empty_IntPairAEAE();
	if (buflength != 0) {
		IntPairAEAE_extend(aeae, buflength);
		for (i = 0; i < nelt; i++) {
			ae = new_empty_IntPairAE();
			_IntPairAEAE_insert_at(aeae, i, ae);
		}
	}
	return aeae;
}

/* Must be used on a malloc-based IntPairAEAE */
static void IntPairAEAE_free(IntPairAEAE *aeae)
{
	size_t buflength, i;
	IntPairAE *ae;

	buflength = aeae->_buflength;
	for (i = 0; i < buflength; i++) {
		ae = aeae->elts[i];
		if (ae != NULL)
			IntPairAE_free(ae);
	}
	if (buflength != 0)
		free(aeae->elts);
	free(aeae);
	return;
}

static void flush_IntPairAEAE_pool()
{
	IntPairAEAE *aeae;

	while (IntPairAEAE_pool_len > 0) {
		IntPairAEAE_pool_len--;
		aeae = IntPairAEAE_pool[IntPairAEAE_pool_len];
		IntPairAEAE_free(aeae);
	}
	return;
}


/****************************************************************************
 * LLongAE buffers
 */

#define	LLONGAE_POOL_MAXLEN 256
static LLongAE *LLongAE_pool[LLONGAE_POOL_MAXLEN];
static int LLongAE_pool_len = 0;

size_t _LLongAE_get_nelt(const LLongAE *ae)
{
	return ae->_nelt;
}

size_t _LLongAE_set_nelt(LLongAE *ae, size_t nelt)
{
	if (nelt > ae->_buflength)
		error("S4Vectors internal error in _LLongAE_set_nelt(): "
		      "trying to set a nb of buffer elements that exceeds "
		      "the buffer length");
	return ae->_nelt = nelt;
}

static LLongAE *new_empty_LLongAE()
{
	LLongAE *ae;

	if (use_malloc && LLongAE_pool_len >= LLONGAE_POOL_MAXLEN)
		error("S4Vectors internal error in new_empty_LLongAE(): "
		      "LLongAE pool is full");
	ae = (LLongAE *) alloc2(1, sizeof(LLongAE));
	ae->_buflength = ae->_nelt = 0;
	if (use_malloc)
		LLongAE_pool[LLongAE_pool_len++] = ae;
	return ae;
}

void _LLongAE_set_val(const LLongAE *ae, long long val)
{
	size_t ae_nelt, i;
	long long *elt_p;

	ae_nelt = _LLongAE_get_nelt(ae);
	elt_p = ae->elts;
	for (i = 0; i < ae_nelt; i++)
		*(elt_p++) = val;
	return;
}

static void LLongAE_extend(LLongAE *ae, size_t new_buflength)
{
	ae->elts = (long long *) realloc2(ae->elts, ae->_buflength,
					  new_buflength, sizeof(long long));
	ae->_buflength = new_buflength;
	return;
}

static int LLongAE_extend_if_full(LLongAE *ae)
{
	if (_LLongAE_get_nelt(ae) < ae->_buflength)
		return 0;
	LLongAE_extend(ae, _increase_buflength(ae->_buflength));
	return 1;
}

void _LLongAE_insert_at(LLongAE *ae, size_t at, long long val)
{
	size_t ae_nelt, i;
	long long *elt1_p;
	const long long *elt2_p;

	ae_nelt = _LLongAE_get_nelt(ae);
	if (at > ae_nelt)
		error("S4Vectors internal error in _LLongAE_insert_at(): "
		      "trying to insert a buffer element at an invalid "
		      "buffer position");
	LLongAE_extend_if_full(ae);
	elt1_p = ae->elts + ae_nelt;
	elt2_p = elt1_p - 1;
	for (i = ae_nelt; i > at; i--)
		*(elt1_p--) = *(elt2_p--);
	*elt1_p = val;
	_LLongAE_set_nelt(ae, ae_nelt + 1);
	return;
}

LLongAE *_new_LLongAE(size_t buflength, size_t nelt, long long val)
{
	LLongAE *ae;

	ae = new_empty_LLongAE();
	if (buflength != 0) {
		LLongAE_extend(ae, buflength);
		_LLongAE_set_nelt(ae, nelt);
		_LLongAE_set_val(ae, val);
	}
	return ae;
}

/* Must be used on a malloc-based LLongAE */
static void LLongAE_free(LLongAE *ae)
{
	if (ae->_buflength != 0)
		free(ae->elts);
	free(ae);
	return;
}

static void flush_LLongAE_pool()
{
	LLongAE *ae;

	while (LLongAE_pool_len > 0) {
		LLongAE_pool_len--;
		ae = LLongAE_pool[LLongAE_pool_len];
		LLongAE_free(ae);
	}
	return;
}


/****************************************************************************
 * CharAE buffers
 */

#define	CHARAE_POOL_MAXLEN 256
static CharAE *CharAE_pool[CHARAE_POOL_MAXLEN];
static int CharAE_pool_len = 0;

size_t _CharAE_get_nelt(const CharAE *ae)
{
	return ae->_nelt;
}

size_t _CharAE_set_nelt(CharAE *ae, size_t nelt)
{
	if (nelt > ae->_buflength)
		error("S4Vectors internal error in _CharAE_set_nelt(): "
		      "trying to set a nb of buffer elements that exceeds "
		      "the buffer length");
	return ae->_nelt = nelt;
}

static CharAE *new_empty_CharAE()
{
	CharAE *ae;

	if (use_malloc && CharAE_pool_len >= CHARAE_POOL_MAXLEN)
		error("S4Vectors internal error in new_empty_CharAE(): "
		      "CharAE pool is full");
	ae = (CharAE *) alloc2(1, sizeof(CharAE));
	ae->_buflength = ae->_nelt = 0;
	if (use_malloc)
		CharAE_pool[CharAE_pool_len++] = ae;
	return ae;
}

static void CharAE_extend(CharAE *ae, size_t new_buflength)
{
	ae->elts = (char *) realloc2(ae->elts, ae->_buflength,
				     new_buflength, sizeof(char));
	ae->_buflength = new_buflength;
	return;
}

static int CharAE_extend_if_full(CharAE *ae)
{
	if (_CharAE_get_nelt(ae) < ae->_buflength)
		return 0;
	CharAE_extend(ae, _increase_buflength(ae->_buflength));
	return 1;
}

void _CharAE_insert_at(CharAE *ae, size_t at, char c)
{
	size_t ae_nelt, i;
	char *elt1_p;
	const char *elt2_p;

	ae_nelt = _CharAE_get_nelt(ae);
	if (at > ae_nelt)
		error("S4Vectors internal error in _CharAE_insert_at(): "
		      "trying to insert a buffer element at an invalid "
		      "buffer position");
	CharAE_extend_if_full(ae);
	elt1_p = ae->elts + ae_nelt;
	elt2_p = elt1_p - 1;
	for (i = ae_nelt; i > at; i--)
		*(elt1_p--) = *(elt2_p--);
	*elt1_p = c;
	_CharAE_set_nelt(ae, ae_nelt + 1);
	return;
}

CharAE *_new_CharAE(size_t buflength)
{
	CharAE *ae;

	ae = new_empty_CharAE();
	if (buflength != 0)
		CharAE_extend(ae, buflength);
	return ae;
}

CharAE *_new_CharAE_from_string(const char *string)
{
	CharAE *ae;

	ae = _new_CharAE(strlen(string));
	_CharAE_set_nelt(ae, ae->_buflength);
	memcpy(ae->elts, string, ae->_buflength);
	return ae;
}

void _append_string_to_CharAE(CharAE *ae, const char *string)
{
	size_t nnewval, ae_nelt, new_nelt;
	char *dest;

	nnewval = strlen(string);
	ae_nelt = _CharAE_get_nelt(ae);
	new_nelt = ae_nelt + nnewval;
	if (new_nelt > ae->_buflength)
		CharAE_extend(ae, new_nelt);
	dest = ae->elts + ae_nelt;
	memcpy(dest, string, sizeof(char) * nnewval);
	_CharAE_set_nelt(ae, new_nelt);
	return;
}

/*
 * Delete 'nelt' elements, starting at position 'at'.
 * Calling _CharAE_delete_at(x, at, nelt) is equivalent to calling
 * _CharAE_delete_at(x, at, 1) nelt times.
 */
void _CharAE_delete_at(CharAE *ae, size_t at, size_t nelt)
{
	char *c1_p;
	const char *c2_p;
	size_t ae_nelt, i2;

	if (nelt == 0)
		return;
	c1_p = ae->elts + at;
	c2_p = c1_p + nelt;
	ae_nelt = _CharAE_get_nelt(ae);
	for (i2 = at + nelt; i2 < ae_nelt; i2++)
		*(c1_p++) = *(c2_p++);
	_CharAE_set_nelt(ae, ae_nelt - nelt);
	return;
}

SEXP _new_RAW_from_CharAE(const CharAE *ae)
{
	size_t ae_nelt;
	SEXP ans;

	if (sizeof(Rbyte) != sizeof(char)) // should never happen!
		error("_new_RAW_from_CharAE(): sizeof(Rbyte) != sizeof(char)");
	ae_nelt = _CharAE_get_nelt(ae);
	/* ae_nelt <= R_XLEN_T_MAX so casting is safe. */
	PROTECT(ans = NEW_RAW((R_xlen_t) ae_nelt));
	memcpy(RAW(ans), ae->elts, ae_nelt * sizeof(char));
	UNPROTECT(1);
	return ans;
}

/* only until we have a bitset or something smaller than char */
SEXP _new_LOGICAL_from_CharAE(const CharAE *ae)
{
	size_t ae_nelt, i;
	SEXP ans;
	const char *elt_p;

	ae_nelt = _CharAE_get_nelt(ae);
	/* ae_nelt <= R_XLEN_T_MAX so casting is safe. */
	PROTECT(ans = NEW_LOGICAL((R_xlen_t) ae_nelt));
	elt_p = ae->elts;
	for (i = 0; i < ae_nelt; i++)
		LOGICAL(ans)[i] = *(elt_p++);
	UNPROTECT(1);
	return ans;
}

/* Must be used on a malloc-based CharAE */
static void CharAE_free(CharAE *ae)
{
	if (ae->_buflength != 0)
		free(ae->elts);
	free(ae);
	return;
}

static void flush_CharAE_pool()
{
	CharAE *ae;

	while (CharAE_pool_len > 0) {
		CharAE_pool_len--;
		ae = CharAE_pool[CharAE_pool_len];
		CharAE_free(ae);
	}
	return;
}

static int remove_from_CharAE_pool(const CharAE *ae)
{
	int i;
	CharAE **ae1_p, **ae2_p;

	i = CharAE_pool_len;
	while (--i >= 0 && CharAE_pool[i] != ae) {;}
	if (i < 0)
		return -1;
	ae1_p = CharAE_pool + i;
	ae2_p = ae1_p + 1;
	for (i = i + 1; i < CharAE_pool_len; i++)
		*(ae1_p++) = *(ae2_p++);
	CharAE_pool_len--;
	return 0;
}


/****************************************************************************
 * CharAEAE buffers
 */

#define	CHARAEAE_POOL_MAXLEN 256
static CharAEAE *CharAEAE_pool[CHARAEAE_POOL_MAXLEN];
static int CharAEAE_pool_len = 0;

size_t _CharAEAE_get_nelt(const CharAEAE *aeae)
{
	return aeae->_nelt;
}

size_t _CharAEAE_set_nelt(CharAEAE *aeae, size_t nelt)
{
	if (nelt > aeae->_buflength)
		error("S4Vectors internal error in _CharAEAE_set_nelt(): "
		      "trying to set a nb of buffer elements that exceeds "
		      "the buffer length");
	return aeae->_nelt = nelt;
}

static CharAEAE *new_empty_CharAEAE()
{
	CharAEAE *aeae;

	if (use_malloc && CharAEAE_pool_len >= CHARAEAE_POOL_MAXLEN)
		error("S4Vectors internal error in new_empty_CharAEAE(): "
		      "CharAEAE pool is full");
	aeae = (CharAEAE *) alloc2(1, sizeof(CharAEAE));
	aeae->_buflength = aeae->_nelt = 0;
	if (use_malloc)
		CharAEAE_pool[CharAEAE_pool_len++] = aeae;
	return aeae;
}

static void CharAEAE_extend(CharAEAE *aeae, size_t new_buflength)
{
	size_t old_buflength, i;

	old_buflength = aeae->_buflength;
	aeae->elts = (CharAE **) realloc2(aeae->elts, old_buflength,
					  new_buflength, sizeof(CharAE *));
	for (i = old_buflength; i < new_buflength; i++)
		aeae->elts[i] = NULL;
	aeae->_buflength = new_buflength;
	return;
}

static int CharAEAE_extend_if_full(CharAEAE *aeae)
{
	if (_CharAEAE_get_nelt(aeae) < aeae->_buflength)
		return 0;
	CharAEAE_extend(aeae, _increase_buflength(aeae->_buflength));
	return 1;
}

void _CharAEAE_insert_at(CharAEAE *aeae, size_t at, CharAE *ae)
{
	size_t aeae_nelt, i;
	CharAE **ae1_p, **ae2_p;

	aeae_nelt = _CharAEAE_get_nelt(aeae);
	if (at > aeae_nelt)
		error("S4Vectors internal error in _CharAEAE_insert_at(): "
		      "trying to insert a buffer element at an invalid "
		      "buffer position");
	CharAEAE_extend_if_full(aeae);
	if (use_malloc && remove_from_CharAE_pool(ae) == -1)
		error("S4Vectors internal error in _CharAEAE_insert_at(): "
		      "CharAE to insert cannot be found in pool for removal");
	ae1_p = aeae->elts + aeae_nelt;
	ae2_p = ae1_p - 1;
	for (i = aeae_nelt; i > at; i--)
		*(ae1_p--) = *(ae2_p--);
	*ae1_p = ae;
	_CharAEAE_set_nelt(aeae, aeae_nelt + 1);
	return;
}

CharAEAE *_new_CharAEAE(size_t buflength, size_t nelt)
{
	CharAEAE *aeae;
	size_t i;
	CharAE *ae;

	aeae = new_empty_CharAEAE();
	if (buflength != 0) {
		CharAEAE_extend(aeae, buflength);
		for (i = 0; i < nelt; i++) {
			ae = new_empty_CharAE();
			_CharAEAE_insert_at(aeae, i, ae);
		}
	}
	return aeae;
}

void _append_string_to_CharAEAE(CharAEAE *aeae, const char *string)
{
	CharAE *ae;

	ae = _new_CharAE_from_string(string);
	_CharAEAE_insert_at(aeae, _CharAEAE_get_nelt(aeae), ae);
	return;
}

SEXP _new_CHARACTER_from_CharAEAE(const CharAEAE *aeae)
{
	size_t aeae_nelt, i, ae_nelt;
	SEXP ans, ans_elt;
	CharAE *ae;

	aeae_nelt = _CharAEAE_get_nelt(aeae);
	/* ae_nelt <= R_XLEN_T_MAX so casting is safe. */
	PROTECT(ans = NEW_CHARACTER((R_xlen_t) aeae_nelt));
	for (i = 0; i < aeae_nelt; i++) {
		ae = aeae->elts[i];
		ae_nelt = _CharAE_get_nelt(ae);
		if (ae_nelt > INT_MAX)
			error("S4Vectors internal error in "
			      "_new_CHARACTER_from_CharAEAE: character "
			      "buffer is too long for mkCharLen()");
		PROTECT(ans_elt = mkCharLen(ae->elts, (int) ae_nelt));
		SET_STRING_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/* Must be used on a malloc-based CharAEAE */
static void CharAEAE_free(CharAEAE *aeae)
{
	size_t buflength, i;
	CharAE *ae;

	buflength = aeae->_buflength;
	for (i = 0; i < buflength; i++) {
		ae = aeae->elts[i];
		if (ae != NULL)
			CharAE_free(ae);
	}
	if (buflength != 0)
		free(aeae->elts);
	free(aeae);
	return;
}

static void flush_CharAEAE_pool()
{
	CharAEAE *aeae;

	while (CharAEAE_pool_len > 0) {
		CharAEAE_pool_len--;
		aeae = CharAEAE_pool[CharAEAE_pool_len];
		CharAEAE_free(aeae);
	}
	return;
}


/****************************************************************************
 * Freeing the malloc-based AEbufs.
 */

SEXP AEbufs_free()
{
	flush_IntAE_pool();
	flush_IntAEAE_pool();
	flush_IntPairAE_pool();
	flush_IntPairAEAE_pool();
	flush_LLongAE_pool();
	flush_CharAE_pool();
	flush_CharAEAE_pool();
	return R_NilValue;
}

