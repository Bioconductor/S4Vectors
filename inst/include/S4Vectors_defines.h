/*****************************************************************************
 S4Vectors C interface: typedefs and defines
 -------------------------------------------

   The S4Vectors C interface is split in 2 files:
     1. S4Vectors_defines.h (this file): contains the typedefs and defines
        of the interface.
     2. S4Vectors_interface.h (in this directory): contains the prototypes
        of the S4Vectors C routines that are part of the interface.

   Please consult S4Vectors_interface.h for how to use this interface in your
   package.

 *****************************************************************************/
#ifndef S4VECTORS_DEFINES_H
#define S4VECTORS_DEFINES_H

#include <Rdefines.h>
#include <R_ext/Rdynload.h>


/* Hash table -- modified from R_HOME/src/main/unique.c */
struct htab {
	int K, M;
	unsigned int Mminus1;
	int *buckets;
};


/*
 * Auto-Extending buffers used for temporary storage of incoming data whose
 * size is not known in advance:
 *
 *   o IntAE:         Auto-Extending buffer of ints;
 *   o IntAEAE:       Auto-Extending buffer of Auto-Extending buffers of ints;
 *   o IntPairAE:     Auto-Extending buffer of pairs of ints;
 *   o IntPairAEAE:   Auto-Extending buffer of Auto-Extending buffers of pairs
 *                    of ints;
 *   o LongLongIntAE: Auto-Extending buffer of long long ints;
 *   o CharAE:        Auto-Extending buffer of chars;
 *   o CharAEAE:      Auto-Extending buffer of Auto-Extending buffers of chars.
 *
 * Some differences between AE buffers and SEXP: (a) AE buffers auto-extend
 * i.e. they automatically reallocate when more room is needed to add a new
 * element, (b) they are faster, and (c) they don't require any
 * PROTECT/UNPROTECT mechanism.
 */

typedef struct int_ae {
	int buflength;
	int *elts;
	int _nelt;
	int _AE_malloc_stack_idx;
} IntAE;

typedef struct int_aeae {
	int buflength;
	IntAE *elts;
	int _nelt;
	int _AE_malloc_stack_idx;
} IntAEAE; 

typedef struct intpair_ae {
	IntAE a;
	IntAE b;
	int _AE_malloc_stack_idx;
} IntPairAE;

typedef struct intpair_aeae {
	int buflength;
	IntPairAE *elts;
	int _nelt;
	int _AE_malloc_stack_idx;
} IntPairAEAE;

typedef struct longlongint_ae {
	int buflength;
	long long int *elts;
	int _nelt;
	int _AE_malloc_stack_idx;
} LongLongIntAE;

typedef struct char_ae {
	int buflength;
	char *elts;
	int _nelt;
	int _AE_malloc_stack_idx;
} CharAE; 

typedef struct char_aeae {
	int buflength;
	CharAE *elts;
	int _nelt;
	int _AE_malloc_stack_idx;
} CharAEAE; 


/*
 * Holder structs.
 */

typedef struct chars_holder {
	const char *ptr;
	int length;
} Chars_holder;

typedef struct ints_holder {
	const int *ptr;
	int length;
} Ints_holder;

typedef struct doubles_holder {
	const double *ptr;
	int length;
} Doubles_holder;


/*
 * Hit selection modes.
 */

#define ALL_HITS		1
#define FIRST_HIT		2
#define LAST_HIT		3
#define ARBITRARY_HIT		4
#define COUNT_HITS		5

#endif
