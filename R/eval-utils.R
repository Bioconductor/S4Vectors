### =========================================================================
### Helpers for environments and evaluation
### -------------------------------------------------------------------------

safeEval <- function(expr, envir, enclos=parent.env(envir), strict=FALSE) {
  expr <- eval(call("bquote", expr, enclos))
  if (strict) {
    enclos <- makeGlobalWarningEnv(expr, envir, enclos)
  }
  eval(expr, envir, enclos)
}

makeGlobalWarningEnv <- function(expr, envir, enclos) {
  envir <- as.env(envir, enclos)
  globals <- setdiff(all.names(expr, functions=FALSE), ls(envir))
  env <- new.env(parent=enclos)
  lapply(globals, function(g) {
    makeActiveBinding(g, function() {
      val <- get(g, enclos)
      warning("Symbol '", g, "' resolved from calling frame; ",
              "escape with .(", g, ") for safety.")
      val
    }, env)
  })
  env
}

.find_arg_enclos <- function(argname, where=parent.frame()) {
  dot_arg_index <- function(arg) {
    if (!is.symbol(arg)) {
      return(NA_integer_)
    }
    arg <- as.character(arg)
    if (!grepl("^\\.\\.[1-9][0-9]*$", arg)) {
      return(NA_integer_)
    }
    as.integer(substring(arg, 3L))
  }
  matched_call <- function(which) {
    match.call(sys.function(which), sys.call(which), expand.dots=FALSE,
               envir=sys.frame(parents[which]))
  }

  which <- Position(\(x) identical(x, where), sys.frames(), right=TRUE)
  parents <- sys.parents()
  mc <- matched_call(which)
  arg <- mc[[argname]]
  dot_idx <- dot_arg_index(arg)
  while (!is.na(dot_idx)) {
    which <- parents[which]
    mc <- matched_call(which)
    dots <- mc$...
    arg <- dots[[dot_idx]]
    dot_idx <- dot_arg_index(arg)
  }

  sys.frame(parents[which])
}

top_prenv <- function(x, where = parent.frame()) {
  .find_arg_enclos(substitute(x) |> as.character(), where)
}

.find_named_arg_enclos <- function(argname, which = sys.parent()) {
  parents <- sys.parents()
  while (argname %notin% names(sys.call(which))) {
    which <- parents[which]
  }
  sys.frame(parents[which])
}

top_prenv_dots <- function(...) {
  args <- substitute(list(...))[-1L]
  lapply(names(args), .find_named_arg_enclos, which = sys.parent()) |>
    setNames(names(args))
}

evalArg <- function(expr, envir, ..., where=parent.frame()) {
  enclos <- .find_arg_enclos(as.character(expr), where=where)
  expr <- eval(call("substitute", expr), where)
  safeEval(expr, envir, enclos, ...)
}

normSubsetIndex <- function(i) {
  i <- try(as.logical(i), silent=TRUE)
  if (inherits(i, "try-error"))
    stop("'subset' must be coercible to logical")
  i & !is.na(i)
}

missingArg <- function(arg, where=parent.frame()) {
  eval(call("missing", arg), where)
}

evalqForSubset <- function(expr, envir, ...) {
  if (missingArg(substitute(expr), parent.frame())) {
    rep(TRUE, NROW(envir))
  } else {
    i <- evalArg(substitute(expr), envir, ..., where=parent.frame())
    normSubsetIndex(i)
  }
}

evalqForSelect <- function(expr, df, ...) {
  if (missingArg(substitute(expr), parent.frame())) {
    rep(TRUE, ncol(df))
  } else {
    nl <- as.list(seq_len(ncol(df)))
    names(nl) <- colnames(df)
    evalArg(substitute(expr), nl, ..., where=parent.frame())
  }
}
