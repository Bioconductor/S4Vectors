/****************************************************************************
 *           Low-level manipulation of the Auto-Extending buffers           *
 *           ----------------------------------------------------           *
 ****************************************************************************/
#include "S4Vectors.h"
#include <stdlib.h>  /* for malloc, free, realloc */

#define MAX_BUFLENGTH_INC (32 * 1024 * 1024)
#define MAX_BUFLENGTH (32 * MAX_BUFLENGTH_INC)


static int debug = 0;

SEXP debug_AEbufs()
{
#ifdef DEBUG_S4VECTORS
	debug = !debug;
	Rprintf("Debug mode turned %s in file %s\n",
		debug ? "on" : "off", __FILE__);
#else
	Rprintf("Debug mode not available in file %s\n", __FILE__);
#endif
	return R_NilValue;
}


/****************************************************************************
 * Core helper functions used for allocation/reallocation of the AEbufs.
 */

static int use_malloc = 0;

SEXP AEbufs_use_malloc(SEXP x)
{
	use_malloc = LOGICAL(x)[0];
	return R_NilValue;
}

/* Guaranteed to return a new buflength > 'buflength', or to raise an error. */
int _get_new_buflength(int buflength)
{
	if (buflength >= MAX_BUFLENGTH)
		error("_get_new_buflength(): MAX_BUFLENGTH reached");
	if (buflength == 0)
		return 128;
	if (buflength <= MAX_BUFLENGTH_INC)
		return 2 * buflength;
	buflength += MAX_BUFLENGTH_INC;
	if (buflength <= MAX_BUFLENGTH)
		return buflength;
	return MAX_BUFLENGTH;
}

static void *malloc_AEbuf(int buflength, size_t size)
{
	void *elts;

	if (buflength == 0)
		return NULL;
	elts = malloc((size_t) buflength * size);
	if (elts == NULL)
		error("S4Vectors internal error in malloc_AEbuf(): "
		      "cannot allocate memory");
	return elts;
}

static void *alloc_AEbuf(int buflength, size_t size)
{
	if (use_malloc)
		return malloc_AEbuf(buflength, size);
	if (buflength == 0)
		return NULL;
	return (void *) R_alloc(buflength, size);
}

static void *realloc_AEbuf(void *elts, int new_buflength,
		int buflength, size_t size)
{
	void *new_elts;

	if (use_malloc) {
		new_elts = realloc(elts, (size_t) new_buflength * size);
		if (new_elts == NULL)
			error("S4Vectors internal error in realloc_AEbuf(): "
			      "cannot reallocate memory");
		return new_elts;
	}
	new_elts = (void *) R_alloc(new_buflength, size);
	return memcpy(new_elts, elts, (size_t) buflength * size);
}


/****************************************************************************
 * IntAE buffers
 *
 * We use a "global IntAE malloc stack" to store a copy of each top-level
 * malloc-based IntAE that is created during the execution of a .Call entry
 * point. The copy must be modified at every reallocation or every time the
 * nb of elements in the buffer (_nelt member) is modified.
 * Every .Call() should start with an empty stack.
 * After the .Call() has returned, the stack must be emptied with
 *     .Call("AEbufs_free", PACKAGE="S4Vectors")
 */

#define	INTAE_MALLOC_STACK_NELT_MAX 2048
static IntAE IntAE_malloc_stack[INTAE_MALLOC_STACK_NELT_MAX];
static int IntAE_malloc_stack_nelt = 0;

static void IntAE_alloc(IntAE *ae, int buflength)
{
	ae->elts = (int *) alloc_AEbuf(buflength, sizeof(int));
	ae->buflength = buflength;
	ae->_AE_malloc_stack_idx = -1;
	return;
}

static void IntAE_realloc(IntAE *ae)
{
	int new_buflength, idx;

	new_buflength = _get_new_buflength(ae->buflength);
	ae->elts = (int *) realloc_AEbuf(ae->elts, new_buflength,
					ae->buflength, sizeof(int));
	ae->buflength = new_buflength;
	idx = ae->_AE_malloc_stack_idx;
	if (idx >= 0)
		IntAE_malloc_stack[idx] = *ae;
	return;
}

int _IntAE_get_nelt(const IntAE *ae)
{
	return ae->_nelt;
}

int _IntAE_set_nelt(IntAE *ae, int nelt)
{
	int idx;

	ae->_nelt = nelt;
	idx = ae->_AE_malloc_stack_idx;
	if (idx >= 0)
		IntAE_malloc_stack[idx] = *ae;
	return nelt;
}

#ifdef DEBUG_S4VECTORS
static void IntAE_print(const IntAE *ae)
{
	Rprintf("buflength=%d elts=%p _nelt=%d _AE_malloc_stack_idx=%d",
		ae->buflength,
		ae->elts,
		ae->_nelt,
		ae->_AE_malloc_stack_idx);
	return;
}
#endif

/* Must be used on a malloc-based IntAE */
static void IntAE_free(const IntAE *ae)
{
	if (ae->elts != NULL)
		free(ae->elts);
	return;
}

static void reset_IntAE_malloc_stack()
{
	int i;
	const IntAE *ae;

	for (i = 0, ae = IntAE_malloc_stack;
	     i < IntAE_malloc_stack_nelt;
	     i++, ae++)
	{
#ifdef DEBUG_S4VECTORS
		if (debug) {
			Rprintf("IntAE_malloc_stack[%d]: ", i);
			IntAE_print(ae);
			Rprintf("\n");
		}
#endif
		IntAE_free(ae);
	}
	IntAE_malloc_stack_nelt = 0;
	return;
}

void _IntAE_set_val(const IntAE *ae, int val)
{
	int nelt, i, *elt;

	nelt = _IntAE_get_nelt(ae);
	for (i = 0, elt = ae->elts; i < nelt; i++, elt++)
		*elt = val;
	return;
}

IntAE _new_IntAE(int buflength, int nelt, int val)
{
	IntAE ae;
	int idx;

	/* Allocation */
	IntAE_alloc(&ae, buflength);
	if (use_malloc) {
		if (IntAE_malloc_stack_nelt >= INTAE_MALLOC_STACK_NELT_MAX)
			error("S4Vectors internal error in _new_IntAE(): "
			      "the \"global IntAE malloc stack\" is full");
		idx = IntAE_malloc_stack_nelt++;
		ae._AE_malloc_stack_idx = idx;
		IntAE_malloc_stack[idx] = ae;
	}
	/* Initialization */
	_IntAE_set_nelt(&ae, nelt);
	_IntAE_set_val(&ae, val);
	return ae;
}

void _IntAE_insert_at(IntAE *ae, int at, int val)
{
	int nelt, i;
	int *elt1;
	const int *elt2;

	nelt = _IntAE_get_nelt(ae);
	if (nelt >= ae->buflength)
		IntAE_realloc(ae);
	elt1 = ae->elts + nelt;
	elt2 = elt1 - 1;
	for (i = nelt; i > at; i--)
		*(elt1--) = *(elt2--);
	*elt1 = val;
	_IntAE_set_nelt(ae, nelt + 1);
	return;
}

void _IntAE_append(IntAE *ae, const int *newvals, int nnewval)
{
	int new_nelt, *dest;

	new_nelt = _IntAE_get_nelt(ae) + nnewval;
	while (ae->buflength < new_nelt)
		IntAE_realloc(ae);
	dest = ae->elts + _IntAE_get_nelt(ae);
	memcpy(dest, newvals, nnewval * sizeof(int));
	_IntAE_set_nelt(ae, new_nelt);
	return;
}

void _IntAE_delete_at(IntAE *ae, int at)
{
	int *elt1;
	const int *elt2;
	int nelt0, i2;

	elt1 = ae->elts + at;
	elt2 = elt1 + 1;
	nelt0 = _IntAE_get_nelt(ae);
	for (i2 = at + 1; i2 < nelt0; i2++)
		*(elt1++) = *(elt2++);
	_IntAE_set_nelt(ae, nelt0 - 1);
	return;
}

void _IntAE_shift(const IntAE *ae, int shift)
{
	int nelt, i, *elt;

	nelt = _IntAE_get_nelt(ae);
	for (i = 0, elt = ae->elts; i < nelt; i++, elt++)
		*elt += shift;
	return;
}

/*
 * Left and right IntAE objects must have the same length. This is
 * NOT checked!
 */
void _IntAE_sum_and_shift(const IntAE *ae1, const IntAE *ae2, int shift)
{
	int nelt, i, *elt1, *elt2;

	nelt = _IntAE_get_nelt(ae1);
	for (i = 0, elt1 = ae1->elts, elt2 = ae2->elts;
	     i < nelt;
	     i++, elt1++, elt2++)
		*elt1 += *elt2 + shift;
	return;
}

void _IntAE_append_shifted_vals(IntAE *ae, const int *newvals,
		int nnewval, int shift)
{
	int nelt, new_nelt, i, *elt1;
	const int *elt2;

	nelt = _IntAE_get_nelt(ae);
	new_nelt = nelt + nnewval;
	while (ae->buflength < new_nelt)
		IntAE_realloc(ae);
	for (i = 0, elt1 = ae->elts + nelt, elt2 = newvals;
	     i < nnewval;
	     i++, elt1++, elt2++)
		*elt1 = *elt2 + shift;
	_IntAE_set_nelt(ae, new_nelt);
	return;
}

void _IntAE_qsort(const IntAE *ae, int desc)
{
	_sort_int_array(ae->elts, _IntAE_get_nelt(ae), desc);
	return;
}

void _IntAE_delete_adjdups(IntAE *ae)
{
	int nelt, *elt1;
	const int *elt2;
	int i2;

	nelt = _IntAE_get_nelt(ae);
	if (nelt <= 1)
		return;
	elt1 = ae->elts;
	elt2 = elt1 + 1;
	for (i2 = 1; i2 < nelt; i2++) {
		if (*elt2 != *elt1) {
			elt1++;
			*elt1 = *elt2;
		}
		elt2++;
	}
	_IntAE_set_nelt(ae, elt1 - ae->elts + 1);
	return;
}

SEXP _new_INTEGER_from_IntAE(const IntAE *ae)
{
	int nelt;
	SEXP ans;

	nelt = _IntAE_get_nelt(ae);
	PROTECT(ans = NEW_INTEGER(nelt));
	memcpy(INTEGER(ans), ae->elts, sizeof(int) * nelt);
	UNPROTECT(1);
	return ans;
}

static void copy_INTEGER_to_IntAE(SEXP x, IntAE *ae)
{
	_IntAE_set_nelt(ae, LENGTH(x));
	memcpy(ae->elts, INTEGER(x), sizeof(int) * LENGTH(x));
	return;
}

IntAE _new_IntAE_from_INTEGER(SEXP x)
{
	IntAE ae;

	ae = _new_IntAE(LENGTH(x), 0, 0);
	copy_INTEGER_to_IntAE(x, &ae);
	return ae;
}

IntAE _new_IntAE_from_CHARACTER(SEXP x, int keyshift)
{
	IntAE ae;
	int i, *elt;

#ifdef DEBUG_S4VECTORS
	if (debug) {
		Rprintf("[DEBUG] _new_IntAE_from_CHARACTER(): BEGIN ... "
			"LENGTH(x)=%d keyshift=%d\n",
			LENGTH(x), keyshift);
	}
#endif
	ae = _new_IntAE(LENGTH(x), 0, 0);
	_IntAE_set_nelt(&ae, ae.buflength);
	for (i = 0, elt = ae.elts; i < ae.buflength; i++, elt++) {
		sscanf(CHAR(STRING_ELT(x, i)), "%d", elt);
		*elt += keyshift;
#ifdef DEBUG_S4VECTORS
		if (debug) {
			if (i < 100
			 || i >= ae.buflength - 100)
				Rprintf("[DEBUG] _new_IntAE_from_CHARACTER(): "
					"i=%d key=%s *elt=%d\n",
					i,
					CHAR(STRING_ELT(x, i)), *elt);
		}
#endif
	}
#ifdef DEBUG_S4VECTORS
	if (debug) {
		Rprintf("[DEBUG] _new_IntAE_from_CHARACTER(): END\n");
	}
#endif
	return ae;
}


/****************************************************************************
 * IntAEAE buffers
 *
 * We use a "global IntAEAE malloc stack" to store a copy of each top-level
 * malloc-based IntAEAE that is created during the execution of a .Call entry
 * point. The copy must be modified at every reallocation or every time the
 * nb of elements in the buffer (nelt member) is modified.
 * Every .Call() should start with an empty stack.
 * After the .Call() has returned, the stack must be emptied with
 *     .Call("AEbufs_free", PACKAGE="S4Vectors")
 */

#define	INTAEAE_MALLOC_STACK_NELT_MAX 2048
static IntAEAE IntAEAE_malloc_stack[INTAEAE_MALLOC_STACK_NELT_MAX];
static int IntAEAE_malloc_stack_nelt = 0;

static void IntAEAE_alloc(IntAEAE *aeae, int buflength)
{
	aeae->elts = (IntAE *) alloc_AEbuf(buflength, sizeof(IntAE));
	aeae->buflength = buflength;
	aeae->_AE_malloc_stack_idx = -1;
	return;
}

static void IntAEAE_realloc(IntAEAE *aeae)
{
	int new_buflength, idx;

	new_buflength = _get_new_buflength(aeae->buflength);
	aeae->elts = (IntAE *) realloc_AEbuf(aeae->elts, new_buflength,
					aeae->buflength, sizeof(IntAE));
	aeae->buflength = new_buflength;
	idx = aeae->_AE_malloc_stack_idx;
	if (idx >= 0)
		IntAEAE_malloc_stack[idx] = *aeae;
	return;
}

int _IntAEAE_get_nelt(const IntAEAE *aeae)
{
	return aeae->_nelt;
}

int _IntAEAE_set_nelt(IntAEAE *aeae, int nelt)
{
	int idx;

	aeae->_nelt = nelt;
	idx = aeae->_AE_malloc_stack_idx;
	if (idx >= 0)
		IntAEAE_malloc_stack[idx] = *aeae;
	return nelt;
}

/* Must be used on a malloc-based IntAEAE */
static void IntAEAE_free(const IntAEAE *aeae)
{
	int nelt, i;
	IntAE *elt;

	nelt = _IntAEAE_get_nelt(aeae);
	for (i = 0, elt = aeae->elts; i < nelt; i++, elt++)
		IntAE_free(elt);
	if (aeae->elts != NULL)
		free(aeae->elts);
	return;
}

static void reset_IntAEAE_malloc_stack()
{
	int i;
	const IntAEAE *aeae;

	for (i = 0, aeae = IntAEAE_malloc_stack;
	     i < IntAEAE_malloc_stack_nelt;
	     i++, aeae++)
	{
		IntAEAE_free(aeae);
	}
	IntAEAE_malloc_stack_nelt = 0;
	return;
}

IntAEAE _new_IntAEAE(int buflength, int nelt)
{
	IntAEAE aeae;
	int idx, i;
	IntAE *elt;

	/* Allocation */
	IntAEAE_alloc(&aeae, buflength);
	if (use_malloc) {
		if (IntAEAE_malloc_stack_nelt >= INTAEAE_MALLOC_STACK_NELT_MAX)
			error("S4Vectors internal error in _new_IntAEAE(): "
			      "the \"global IntAEAE malloc stack\" is full");
		idx = IntAEAE_malloc_stack_nelt++;
		aeae._AE_malloc_stack_idx = idx;
		IntAEAE_malloc_stack[idx] = aeae;
	}
	/* Initialization */
	_IntAEAE_set_nelt(&aeae, nelt);
	for (i = 0, elt = aeae.elts; i < nelt; i++, elt++) {
		IntAE_alloc(elt, 0);
		_IntAE_set_nelt(elt, 0);
	}
	return aeae;
}

void _IntAEAE_insert_at(IntAEAE *aeae, int at, const IntAE *ae)
{
	int nelt, i;
	IntAE *elt1;
	const IntAE *elt2;

	if (ae->_AE_malloc_stack_idx >= 0)
		error("S4Vectors internal error in _IntAEAE_insert_at(): "
		      "cannot insert an IntAE that is in the "
		      "\"global IntAE malloc stack\"");
	nelt = _IntAEAE_get_nelt(aeae);
	if (nelt >= aeae->buflength)
		IntAEAE_realloc(aeae);
	elt1 = aeae->elts + nelt;
	elt2 = elt1 - 1;
	for (i = nelt; i > at; i--)
		*(elt1--) = *(elt2--);
	*elt1 = *ae;
	_IntAEAE_set_nelt(aeae, nelt + 1);
	return;
}

/*
 * Left and right IntAEAE objects must have the same length. This is
 * NOT checked!
 */
void _IntAEAE_eltwise_append(const IntAEAE *aeae1, const IntAEAE *aeae2)
{
	int nelt, i;
	IntAE *elt1;
	const IntAE *elt2;

	nelt = _IntAEAE_get_nelt(aeae1);
	for (i = 0, elt1 = aeae1->elts, elt2 = aeae2->elts;
	     i < nelt;
	     i++, elt1++, elt2++)
		_IntAE_append(elt1, elt2->elts, _IntAE_get_nelt(elt2));
	return;
}

void _IntAEAE_shift(const IntAEAE *aeae, int shift)
{
	int nelt, i;
	IntAE *elt;

	nelt = _IntAEAE_get_nelt(aeae);
	for (i = 0, elt = aeae->elts; i < nelt; i++, elt++)
		_IntAE_shift(elt, shift);
	return;
}

/*
 * Left and right IntAEAE objects must have the same length. This is
 * NOT checked!
 */
void _IntAEAE_sum_and_shift(const IntAEAE *aeae1, const IntAEAE *aeae2,
		int shift)
{
	int nelt, i;
	IntAE *elt1;
	const IntAE *elt2;

	nelt = _IntAEAE_get_nelt(aeae1);
	for (i = 0, elt1 = aeae1->elts, elt2 = aeae2->elts;
	     i < nelt;
	     i++, elt1++, elt2++)
		_IntAE_sum_and_shift(elt1, elt2, shift);
	return;
}

/*
 * 'mode' controls how empty list elements should be represented:
 *   0 -> integer(0); 1 -> NULL; 2 -> NA
 */
SEXP _new_LIST_from_IntAEAE(const IntAEAE *aeae, int mode)
{
	int nelt, i;
	SEXP ans, ans_elt;
	const IntAE *elt;

	nelt = _IntAEAE_get_nelt(aeae);
	PROTECT(ans = NEW_LIST(nelt));
	for (i = 0, elt = aeae->elts; i < nelt; i++, elt++) {
		if (_IntAE_get_nelt(elt) != 0 || mode == 0) {
			PROTECT(ans_elt = _new_INTEGER_from_IntAE(elt));
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

IntAEAE _new_IntAEAE_from_LIST(SEXP x)
{
	IntAEAE aeae;
	int i;
	IntAE *elt;
	SEXP x_elt;

	aeae = _new_IntAEAE(LENGTH(x), 0);
	_IntAEAE_set_nelt(&aeae, aeae.buflength);
	for (i = 0, elt = aeae.elts; i < aeae.buflength; i++, elt++) {
		x_elt = VECTOR_ELT(x, i);
		if (TYPEOF(x_elt) != INTSXP)
			error("S4Vectors internal error in "
			      "_new_IntAEAE_from_LIST(): "
			      "not all elements in the list "
			      "are integer vectors");
		IntAE_alloc(elt, LENGTH(x_elt));
		copy_INTEGER_to_IntAE(x_elt, elt);
	}
	return aeae;
}

SEXP _IntAEAE_toEnvir(const IntAEAE *aeae, SEXP envir, int keyshift)
{
	int nelt, i;
	const IntAE *elt;
	char key[11];
	SEXP value;

	nelt = _IntAEAE_get_nelt(aeae);
#ifdef DEBUG_S4VECTORS
	int nkey = 0, cum_length = 0;
	if (debug) {
		Rprintf("[DEBUG] _IntAEAE_toEnvir(): BEGIN ... "
			"aeae->_nelt=%d keyshift=%d\n",
			nelt, keyshift);
	}
#endif
	for (i = 0, elt = aeae->elts; i < nelt; i++, elt++) {
#ifdef DEBUG_S4VECTORS
		if (debug) {
			if (i < 100 || i >= nelt - 100)
				Rprintf("[DEBUG] _IntAEAE_toEnvir(): "
					"nkey=%d aeae->elts[%d]._nelt=%d\n",
					nkey, i, _IntAE_get_nelt(elt));
		}
#endif
		if (_IntAE_get_nelt(elt) == 0)
			continue;
		//snprintf(key, sizeof(key), "%d", i + keyshift);
		snprintf(key, sizeof(key), "%010d", i + keyshift);
#ifdef DEBUG_S4VECTORS
		if (debug) {
			if (i < 100 || i >= nelt - 100)
				Rprintf("[DEBUG] _IntAEAE_toEnvir(): "
					"installing key=%s ... ", key);
		}
#endif
		PROTECT(value = _new_INTEGER_from_IntAE(elt));
		defineVar(install(key), value, envir);
		UNPROTECT(1);
#ifdef DEBUG_S4VECTORS
		if (debug) {
			nkey++;
			cum_length += _IntAE_get_nelt(elt);
			if (i < 100 || i >= nelt - 100)
				Rprintf("OK (nkey=%d cum_length=%d)\n",
					nkey, cum_length);
		}
#endif
	}
#ifdef DEBUG_S4VECTORS
	if (debug) {
		Rprintf("[DEBUG] _IntAEAE_toEnvir(): END "
			"(nkey=%d cum_length=%d)\n", nkey, cum_length);
	}
#endif
	return envir;
}


/****************************************************************************
 * IntPairAE buffers
 *
 * We use a "global IntPairAE malloc stack" to store a copy of each top-level
 * malloc-based IntPairAE that is created during the execution of a .Call
 * entry point. The copy must be modified every time the a or b members are
 * modified.
 * Every .Call() should start with an empty stack.
 * After the .Call() has returned, the stack must be emptied with
 *     .Call("AEbufs_free", PACKAGE="S4Vectors")
 */

#define	INTPAIRAE_MALLOC_STACK_NELT_MAX 2048
static IntPairAE IntPairAE_malloc_stack[INTPAIRAE_MALLOC_STACK_NELT_MAX];
static int IntPairAE_malloc_stack_nelt = 0;

static void IntPairAE_alloc(IntPairAE *ae, int buflength)
{
	IntAE_alloc(&(ae->a), buflength);
	IntAE_alloc(&(ae->b), buflength);
	ae->_AE_malloc_stack_idx = -1;
	return;
}

int _IntPairAE_get_nelt(const IntPairAE *ae)
{
	return _IntAE_get_nelt(&(ae->a));
}

int _IntPairAE_set_nelt(IntPairAE *ae, int nelt)
{
	int idx;

	_IntAE_set_nelt(&(ae->a), nelt);
	_IntAE_set_nelt(&(ae->b), nelt);
	idx = ae->_AE_malloc_stack_idx;
	if (idx >= 0)
		IntPairAE_malloc_stack[idx] = *ae;
	return nelt;
}

#ifdef DEBUG_S4VECTORS
static void IntPairAE_print(const IntPairAE *ae)
{
	IntAE_print(&(ae->a));
	Rprintf(" ");
	IntAE_print(&(ae->b));
	Rprintf(" _AE_malloc_stack_idx=%d", ae->_AE_malloc_stack_idx);
	return;
}
#endif

/* Must be used on a malloc-based IntPairAE */
static void IntPairAE_free(const IntPairAE *ae)
{
	IntAE_free(&(ae->a));
	IntAE_free(&(ae->b));
	return;
}

static void reset_IntPairAE_malloc_stack()
{
	int i;
	const IntPairAE *ae;

	for (i = 0, ae = IntPairAE_malloc_stack;
	     i < IntPairAE_malloc_stack_nelt;
	     i++, ae++)
	{
#ifdef DEBUG_S4VECTORS
		if (debug) {
			Rprintf("IntPairAE_malloc_stack[%d]: ", i);
			IntPairAE_print(ae);
			Rprintf("\n");
		}
#endif
		IntPairAE_free(ae);
	}
	IntPairAE_malloc_stack_nelt = 0;
	return;
}

IntPairAE _new_IntPairAE(int buflength, int nelt)
{
	IntPairAE ae;
	int idx;

	/* Allocation */
	IntPairAE_alloc(&ae, buflength);
	if (use_malloc) {
		if (IntPairAE_malloc_stack_nelt >=
		    INTPAIRAE_MALLOC_STACK_NELT_MAX)
			error("S4Vectors internal error in _new_IntPairAE(): "
			      "the \"global IntPairAE malloc stack\" is full");
		idx = IntPairAE_malloc_stack_nelt++;
		ae._AE_malloc_stack_idx = idx;
		IntPairAE_malloc_stack[idx] = ae;
	}
	/* Elements are NOT initialized */
	_IntPairAE_set_nelt(&ae, nelt);
	return ae;
}

void _IntPairAE_insert_at(IntPairAE *ae, int at, int a, int b)
{
	int idx;

	_IntAE_insert_at(&(ae->a), at, a);
	_IntAE_insert_at(&(ae->b), at, b);
	idx = ae->_AE_malloc_stack_idx;
	if (idx >= 0)
		IntPairAE_malloc_stack[idx] = *ae;
	return;
}


/****************************************************************************
 * IntPairAEAE buffers
 *
 * We use a "global IntPairAEAE malloc stack" to store a copy of each
 * top-level malloc-based IntPairAEAE that is created during the execution of
 * a .Call entry point. The copy must be modified at every reallocation or
 * every time the nb of elements in the buffer (nelt member) is modified.
 * Every .Call() should start with an empty stack.
 * After the .Call() has returned, the stack must be emptied with
 *     .Call("AEbufs_free", PACKAGE="S4Vectors")
 */

#define	INTPAIRAEAE_MALLOC_STACK_NELT_MAX 2048
static IntPairAEAE IntPairAEAE_malloc_stack[INTPAIRAEAE_MALLOC_STACK_NELT_MAX];
static int IntPairAEAE_malloc_stack_nelt = 0;

static void IntPairAEAE_alloc(IntPairAEAE *aeae, int buflength)
{
	aeae->elts = (IntPairAE *) alloc_AEbuf(buflength,
							sizeof(IntPairAE));
	aeae->buflength = buflength;
	aeae->_AE_malloc_stack_idx = -1;
	return;
}

static void IntPairAEAE_realloc(IntPairAEAE *aeae)
{
	int new_buflength, idx;

	new_buflength = _get_new_buflength(aeae->buflength);
	aeae->elts = (IntPairAE *) realloc_AEbuf(aeae->elts,
					new_buflength, aeae->buflength,
					sizeof(IntPairAE));
	aeae->buflength = new_buflength;
	idx = aeae->_AE_malloc_stack_idx;
	if (idx >= 0)
		IntPairAEAE_malloc_stack[idx] = *aeae;
	return;
}

int _IntPairAEAE_get_nelt(const IntPairAEAE *aeae)
{
	return aeae->_nelt;
}

int _IntPairAEAE_set_nelt(IntPairAEAE *aeae, int nelt)
{
	int idx;

	aeae->_nelt = nelt;
	idx = aeae->_AE_malloc_stack_idx;
	if (idx >= 0)
		IntPairAEAE_malloc_stack[idx] = *aeae;
	return nelt;
}

/* Must be used on a malloc-based IntPairAEAE */
static void IntPairAEAE_free(const IntPairAEAE *aeae)
{
	int nelt, i;
	IntPairAE *elt;

	nelt = _IntPairAEAE_get_nelt(aeae);
	for (i = 0, elt = aeae->elts; i < nelt; i++, elt++)
		IntPairAE_free(elt);
	if (aeae->elts != NULL)
		free(aeae->elts);
	return;
}

static void reset_IntPairAEAE_malloc_stack()
{
	int i;
	const IntPairAEAE *aeae;

	for (i = 0, aeae = IntPairAEAE_malloc_stack;
	     i < IntPairAEAE_malloc_stack_nelt;
	     i++, aeae++)
	{
		IntPairAEAE_free(aeae);
	}
	IntPairAEAE_malloc_stack_nelt = 0;
	return;
}

IntPairAEAE _new_IntPairAEAE(int buflength, int nelt)
{
	IntPairAEAE aeae;
	int idx, i;
	IntPairAE *elt;

	/* Allocation */
	IntPairAEAE_alloc(&aeae, buflength);
	if (use_malloc) {
		if (IntPairAEAE_malloc_stack_nelt >=
		    INTPAIRAEAE_MALLOC_STACK_NELT_MAX)
			error("S4Vectors internal error in "
			      "_new_IntPairAEAE(): the \"global "
			      "IntPairAEAE malloc stack\" is full");
		idx = IntPairAEAE_malloc_stack_nelt++;
		aeae._AE_malloc_stack_idx = idx;
		IntPairAEAE_malloc_stack[idx] = aeae;
	}
	/* Initialization */
	_IntPairAEAE_set_nelt(&aeae, nelt);
	for (i = 0, elt = aeae.elts; i < nelt; i++, elt++) {
		IntPairAE_alloc(elt, 0);
		_IntPairAE_set_nelt(elt, 0);
	}
	return aeae;
}

void _IntPairAEAE_insert_at(IntPairAEAE *aeae, int at,
		const IntPairAE *ae)
{
	int nelt, i;
	IntPairAE *elt1;
	const IntPairAE *elt2;

	if (ae->_AE_malloc_stack_idx >= 0)
		error("S4Vectors internal error in _IntPairAEAE_insert_at(): "
		      "cannot insert a IntPairAE that is in the "
		      "\"global IntPairAE malloc stack\"");
	nelt = _IntPairAEAE_get_nelt(aeae);
	if (nelt >= aeae->buflength)
		IntPairAEAE_realloc(aeae);
	elt1 = aeae->elts + nelt;
	elt2 = elt1 - 1;
	for (i = nelt; i > at; i--)
		*(elt1--) = *(elt2--);
	*elt1 = *ae;
	_IntPairAEAE_set_nelt(aeae, nelt + 1);
	return;
}


/****************************************************************************
 * LongLongIntAE buffers
 *
 * We use a "global LongLongIntAE malloc stack" to store a copy of each
 * top-level malloc-based LongLongIntAE that is created during the execution
 * of a .Call entry point. The copy must be modified at every reallocation or
 * every time the nb of elements in the buffer (_nelt member) is modified.
 * Every .Call() should start with an empty stack.
 * After the .Call() has returned, the stack must be emptied with
 *     .Call("AEbufs_free", PACKAGE="S4Vectors")
 */

#define	LONGLONGINTAE_MALLOC_STACK_NELT_MAX 2048
static LongLongIntAE
	LongLongIntAE_malloc_stack[LONGLONGINTAE_MALLOC_STACK_NELT_MAX];
static int LongLongIntAE_malloc_stack_nelt = 0;

static void LongLongIntAE_alloc(LongLongIntAE *ae, int buflength)
{
	ae->elts = (long long int *) alloc_AEbuf(buflength,
						 sizeof(long long int));
	ae->buflength = buflength;
	ae->_AE_malloc_stack_idx = -1;
	return;
}

static void LongLongIntAE_realloc(LongLongIntAE *ae)
{
	int new_buflength, idx;

	new_buflength = _get_new_buflength(ae->buflength);
	ae->elts = (long long int *) realloc_AEbuf(ae->elts, new_buflength,
						   ae->buflength,
						   sizeof(long long int));
	ae->buflength = new_buflength;
	idx = ae->_AE_malloc_stack_idx;
	if (idx >= 0)
		LongLongIntAE_malloc_stack[idx] = *ae;
	return;
}

int _LongLongIntAE_get_nelt(const LongLongIntAE *ae)
{
	return ae->_nelt;
}

int _LongLongIntAE_set_nelt(LongLongIntAE *ae, int nelt)
{
	int idx;

	ae->_nelt = nelt;
	idx = ae->_AE_malloc_stack_idx;
	if (idx >= 0)
		LongLongIntAE_malloc_stack[idx] = *ae;
	return nelt;
}

#ifdef DEBUG_S4VECTORS
static void LongLongIntAE_print(const LongLongIntAE *ae)
{
	Rprintf("buflength=%d elts=%p _nelt=%d _AE_malloc_stack_idx=%d",
		ae->buflength,
		ae->elts,
		ae->_nelt,
		ae->_AE_malloc_stack_idx);
	return;
}
#endif

/* Must be used on a malloc-based LongLongIntAE */
static void LongLongIntAE_free(const LongLongIntAE *ae)
{
	if (ae->elts != NULL)
		free(ae->elts);
	return;
}

static void reset_LongLongIntAE_malloc_stack()
{
	int i;
	const LongLongIntAE *ae;

	for (i = 0, ae = LongLongIntAE_malloc_stack;
	     i < LongLongIntAE_malloc_stack_nelt;
	     i++, ae++)
	{
#ifdef DEBUG_S4VECTORS
		if (debug) {
			Rprintf("LongLongIntAE_malloc_stack[%d]: ", i);
			LongLongIntAE_print(ae);
			Rprintf("\n");
		}
#endif
		LongLongIntAE_free(ae);
	}
	LongLongIntAE_malloc_stack_nelt = 0;
	return;
}

void _LongLongIntAE_set_val(const LongLongIntAE *ae, long long int val)
{
	int nelt, i;
	long long int *elt;

	nelt = _LongLongIntAE_get_nelt(ae);
	for (i = 0, elt = ae->elts; i < nelt; i++, elt++)
		*elt = val;
	return;
}

LongLongIntAE _new_LongLongIntAE(int buflength, int nelt, long long int val)
{
	LongLongIntAE ae;
	int idx;

	/* Allocation */
	LongLongIntAE_alloc(&ae, buflength);
	if (use_malloc) {
		if (LongLongIntAE_malloc_stack_nelt >=
		    LONGLONGINTAE_MALLOC_STACK_NELT_MAX)
			error("S4Vectors internal error in "
			      "_new_LongLongIntAE(): the \"global "
			      "LongLongIntAE malloc stack\" is full");
		idx = LongLongIntAE_malloc_stack_nelt++;
		ae._AE_malloc_stack_idx = idx;
		LongLongIntAE_malloc_stack[idx] = ae;
	}
	/* Initialization */
	_LongLongIntAE_set_nelt(&ae, nelt);
	_LongLongIntAE_set_val(&ae, val);
	return ae;
}

void _LongLongIntAE_insert_at(LongLongIntAE *ae, int at, long long int val)
{
	int nelt, i;
	long long int *elt1;
	const long long int *elt2;

	nelt = _LongLongIntAE_get_nelt(ae);
	if (nelt >= ae->buflength)
		LongLongIntAE_realloc(ae);
	elt1 = ae->elts + nelt;
	elt2 = elt1 - 1;
	for (i = nelt; i > at; i--)
		*(elt1--) = *(elt2--);
	*elt1 = val;
	_LongLongIntAE_set_nelt(ae, nelt + 1);
	return;
}


/****************************************************************************
 * CharAE buffers
 *
 * We use a "global CharAE malloc stack" to store a copy of each top-level
 * malloc-based CharAE that is created during the execution of a .Call entry
 * point. The copy must be modified at every reallocation or every time the
 * nb of elements in the buffer (nelt member) is modified.
 * Every .Call() should start with an empty stack.
 * After the .Call() has returned, the stack must be emptied with
 *     .Call("AEbufs_free", PACKAGE="S4Vectors")
 */

#define	CHARAE_MALLOC_STACK_NELT_MAX 2048
static CharAE CharAE_malloc_stack[CHARAE_MALLOC_STACK_NELT_MAX];
static int CharAE_malloc_stack_nelt = 0;

static void CharAE_alloc(CharAE *ae, int buflength)
{
	ae->elts = (char *) alloc_AEbuf(buflength, sizeof(char));
	ae->buflength = buflength;
	ae->_AE_malloc_stack_idx = -1;
	return;
}

static void CharAE_realloc(CharAE *ae)
{
	int new_buflength, idx;

	new_buflength = _get_new_buflength(ae->buflength);
	ae->elts = (char *) realloc_AEbuf(ae->elts, new_buflength,
					ae->buflength, sizeof(char));
	ae->buflength = new_buflength;
	idx = ae->_AE_malloc_stack_idx;
	if (idx >= 0)
		CharAE_malloc_stack[idx] = *ae;
	return;
}

int _CharAE_get_nelt(const CharAE *ae)
{
	return ae->_nelt;
}

int _CharAE_set_nelt(CharAE *ae, int nelt)
{
	int idx;

	ae->_nelt = nelt;
	idx = ae->_AE_malloc_stack_idx;
	if (idx >= 0)
		CharAE_malloc_stack[idx] = *ae;
	return nelt;
}

/* Must be used on a malloc-based CharAE */
static void CharAE_free(const CharAE *ae)
{
	if (ae->elts != NULL)
		free(ae->elts);
	return;
}

static void reset_CharAE_malloc_stack()
{
	int i;
	const CharAE *ae;

	for (i = 0, ae = CharAE_malloc_stack;
	     i < CharAE_malloc_stack_nelt;
	     i++, ae++)
	{
		CharAE_free(ae);
	}
	CharAE_malloc_stack_nelt = 0;
	return;
}

CharAE _new_CharAE(int buflength)
{
	CharAE ae;
	int idx;

	/* Allocation */
	CharAE_alloc(&ae, buflength);
	if (use_malloc) {
		if (CharAE_malloc_stack_nelt >= CHARAE_MALLOC_STACK_NELT_MAX)
			error("S4Vectors internal error in _new_CharAE(): "
			      "the \"global CharAE malloc stack\" is full");
		idx = CharAE_malloc_stack_nelt++;
		ae._AE_malloc_stack_idx = idx;
		CharAE_malloc_stack[idx] = ae;
	}
	/* Initialization */
	_CharAE_set_nelt(&ae, 0);
	return ae;
}

CharAE _new_CharAE_from_string(const char *string)
{
	CharAE ae;

	ae = _new_CharAE(strlen(string));
	_CharAE_set_nelt(&ae, ae.buflength);
	memcpy(ae.elts, string, ae.buflength);
	return ae;
}

void _CharAE_insert_at(CharAE *ae, int at, char c)
{
	int nelt, i;
	char *elt1;
	const char *elt2;

	nelt = _CharAE_get_nelt(ae);
	if (nelt >= ae->buflength)
		CharAE_realloc(ae);
	elt1 = ae->elts + nelt;
	elt2 = elt1 - 1;
	for (i = nelt; i > at; i--)
		*(elt1--) = *(elt2--);
	*elt1 = c;
	_CharAE_set_nelt(ae, nelt + 1);
	return;
}

void _append_string_to_CharAE(CharAE *ae, const char *string)
{
	int nnewval, nelt, new_nelt;
	char *dest;

	nnewval = strlen(string);
	nelt = _CharAE_get_nelt(ae);
	new_nelt = nelt + nnewval;
	while (ae->buflength < new_nelt)
		CharAE_realloc(ae);
	dest = ae->elts + nelt;
	memcpy(dest, string, nnewval * sizeof(char));
	_CharAE_set_nelt(ae, new_nelt);
	return;
}

/*
 * Delete 'nelt' elements, starting at position 'at'.
 * Doing _CharAE_delete_at(x, at, nelt) is equivalent to doing
 * _CharAE_delete_at(x, at, 1) 'nelt' times.
 */
void _CharAE_delete_at(CharAE *ae, int at, int nelt)
{
	char *elt1;
	const char *elt2;
	int nelt0, i2;

	if (nelt == 0)
		return;
	elt1 = ae->elts + at;
	elt2 = elt1 + nelt;
	nelt0 = _CharAE_get_nelt(ae);
	for (i2 = at + nelt; i2 < nelt0; i2++)
		*(elt1++) = *(elt2++);
	_CharAE_set_nelt(ae, nelt0 - nelt);
	return;
}

SEXP _new_RAW_from_CharAE(const CharAE *ae)
{
	int nelt;
	SEXP ans;

	if (sizeof(Rbyte) != sizeof(char)) // should never happen!
		error("_new_RAW_from_CharAE(): sizeof(Rbyte) != sizeof(char)");
	nelt = _CharAE_get_nelt(ae);
	PROTECT(ans = NEW_RAW(nelt));
	memcpy(RAW(ans), ae->elts, sizeof(char) * nelt);
	UNPROTECT(1);
	return ans;
}

/* only until we have a bitset or something smaller than char */
SEXP _new_LOGICAL_from_CharAE(const CharAE *ae)
{
	int nelt, i, *ans_elt;
	SEXP ans;
	const char *elt;

	nelt = _CharAE_get_nelt(ae);
	PROTECT(ans = NEW_LOGICAL(nelt));
	for (i = 0, ans_elt = LOGICAL(ans), elt = ae->elts;
	     i < nelt;
	     i++, ans_elt++, elt++)
	{
		*ans_elt = *elt;
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * CharAEAE buffers
 *
 * We use a "global CharAEAE malloc stack" to store a copy of each top-level
 * malloc-based CharAEAE that is created during the execution of a .Call entry
 * point. The copy must be modified at every reallocation or every time the
 * nb of elements in the buffer (nelt member) is modified.
 * Every .Call() should start with an empty stack.
 * After the .Call() has returned, the stack must be emptied with
 *     .Call("AEbufs_free", PACKAGE="S4Vectors")
 */

#define	CHARAEAE_MALLOC_STACK_NELT_MAX 2048
static CharAEAE CharAEAE_malloc_stack[CHARAEAE_MALLOC_STACK_NELT_MAX];
static int CharAEAE_malloc_stack_nelt = 0;

static void CharAEAE_alloc(CharAEAE *aeae, int buflength)
{
	aeae->elts = (CharAE *) alloc_AEbuf(buflength, sizeof(CharAE));
	aeae->buflength = buflength;
	aeae->_AE_malloc_stack_idx = -1;
	return;
}

static void CharAEAE_realloc(CharAEAE *aeae)
{
	int new_buflength, idx;

	new_buflength = _get_new_buflength(aeae->buflength);
	aeae->elts = (CharAE *) realloc_AEbuf(aeae->elts,
					new_buflength,
					aeae->buflength, sizeof(CharAE));
	aeae->buflength = new_buflength;
	idx = aeae->_AE_malloc_stack_idx;
	if (idx >= 0)
		CharAEAE_malloc_stack[idx] = *aeae;
	return;
}

int _CharAEAE_get_nelt(const CharAEAE *aeae)
{
	return aeae->_nelt;
}

int _CharAEAE_set_nelt(CharAEAE *aeae, int nelt)
{
	int idx;

	aeae->_nelt = nelt;
	idx = aeae->_AE_malloc_stack_idx;
	if (idx >= 0)
		CharAEAE_malloc_stack[idx] = *aeae;
	return nelt;
}

/* Must be used on a malloc-based CharAEAE */
static void CharAEAE_free(const CharAEAE *aeae)
{
	int nelt, i;
	CharAE *elt;

	nelt = _CharAEAE_get_nelt(aeae);
	for (i = 0, elt = aeae->elts; i < nelt; i++, elt++)
		CharAE_free(elt);
	if (aeae->elts != NULL)
		free(aeae->elts);
	return;
}

static void reset_CharAEAE_malloc_stack()
{
	int i;
	const CharAEAE *aeae;

	for (i = 0, aeae = CharAEAE_malloc_stack;
	     i < CharAEAE_malloc_stack_nelt;
	     i++, aeae++)
	{
		CharAEAE_free(aeae);
	}
	CharAEAE_malloc_stack_nelt = 0;
	return;
}

CharAEAE _new_CharAEAE(int buflength, int nelt)
{
	CharAEAE aeae;
	int idx, i;
	CharAE *elt;

	/* Allocation */
	CharAEAE_alloc(&aeae, buflength);
	if (use_malloc) {
		if (CharAEAE_malloc_stack_nelt >=
		    CHARAEAE_MALLOC_STACK_NELT_MAX)
			error("S4Vectors internal error in _new_CharAEAE(): "
			      "the \"global CharAEAE malloc stack\" is full");
		idx = CharAEAE_malloc_stack_nelt++;
		aeae._AE_malloc_stack_idx = idx;
		CharAEAE_malloc_stack[idx] = aeae;
	}
	/* Initialization */
	_CharAEAE_set_nelt(&aeae, nelt);
	for (i = 0, elt = aeae.elts; i < nelt; i++, elt++) {
		CharAE_alloc(elt, 0);
		_CharAE_set_nelt(elt, 0);
	}
	return aeae;
}

void _CharAEAE_insert_at(CharAEAE *aeae, int at, const CharAE *ae)
{
	int nelt, i;
	CharAE *elt1;
	const CharAE *elt2;

	if (ae->_AE_malloc_stack_idx >= 0)
		error("S4Vectors internal error in _CharAEAE_insert_at(): "
		      "cannot insert a CharAE that is in the "
		      "\"global CharAE malloc stack\"");
	nelt = _CharAEAE_get_nelt(aeae);
	if (nelt >= aeae->buflength)
		CharAEAE_realloc(aeae);
	elt1 = aeae->elts + nelt;
	elt2 = elt1 - 1;
	for (i = nelt; i > at; i--)
		*(elt1--) = *(elt2--);
	*elt1 = *ae;
	_CharAEAE_set_nelt(aeae, nelt + 1);
	return;
}

void _append_string_to_CharAEAE(CharAEAE *aeae, const char *string)
{
	CharAE ae;

	CharAE_alloc(&ae, strlen(string));
	_CharAE_set_nelt(&ae, ae.buflength);
	memcpy(ae.elts, string, ae.buflength);
	_CharAEAE_insert_at(aeae, _CharAEAE_get_nelt(aeae), &ae);
	return;
}

SEXP _new_CHARACTER_from_CharAEAE(const CharAEAE *aeae)
{
	int nelt, i;
	SEXP ans, ans_elt;
	CharAE *elt;

	nelt = _CharAEAE_get_nelt(aeae);
	PROTECT(ans = NEW_CHARACTER(nelt));
	for (i = 0, elt = aeae->elts; i < nelt; i++, elt++) {
		PROTECT(ans_elt = mkCharLen(elt->elts, _CharAE_get_nelt(elt)));
		SET_STRING_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Freeing the malloc-based AEbufs.
 */

SEXP AEbufs_free()
{
	reset_IntAE_malloc_stack();
	reset_IntAEAE_malloc_stack();
	reset_IntPairAE_malloc_stack();
	reset_IntPairAEAE_malloc_stack();
	reset_LongLongIntAE_malloc_stack();
	reset_CharAE_malloc_stack();
	reset_CharAEAE_malloc_stack();
	return R_NilValue;
}

