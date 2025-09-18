/****************************************************************************
 *                 Low-level manipulation of DFrame objects                 *
 ****************************************************************************/
#include "S4Vectors.h"


static SEXP nrows_symbol=NULL, rownames_symbol=NULL;

static void set_DFrame_nrows(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(nrows)
	SET_SLOT(x, nrows_symbol, value);
}

static void set_DFrame_rownames(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(rownames)
	SET_SLOT(x, rownames_symbol, value);
}

SEXP _new_DFrame(const char *classname, SEXP listData, SEXP nrows,
		 SEXP rownames)
{
	SEXP ans = PROTECT(_new_SimpleList(classname, listData));
	set_DFrame_nrows(ans, nrows);
	set_DFrame_rownames(ans, rownames);
	UNPROTECT(1);
	return ans;
}

