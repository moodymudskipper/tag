

#' a list of adverbs from the *base* and *purrr* packages
adverb_list <- list(
  base = rlang::exprs(
    Negate,
    Vectorize
    # `body<-`,
    # `formals<-`
  ),
  purrr = rlang::exprs(
    compose,
    lift,
    lift_dl,
    lift_dv,
    lift_vl,
    lift_vd,
    lift_ld,
    lift_lv,
    negate,
    partial,
    safely,
    quietly,
    possibly,
    auto_browse,
    insistently,
    slowly,
    rate_sleep,
    rate_reset,
    rate_delay,
    rate_backoff,
    is_rate))

#' # not robust : lapply(list("purrr::compose", "safely"), set_tag_adverb)
#' @export
set_tag_adverbs<- function(...) {
  if(!...length()) {
    funs <- adverb_list
    for(fun in funs$base){
      eval(substitute(class(FUN) <- union("tag_adverb",class(FUN)),
                      list(FUN = fun)),envir = .GlobalEnv)
    }
    for(fun in funs$purrr){
      eval(substitute(class(FUN) <- union("tag_adverb",class(FUN)),
                      list(FUN = fun)),envir = .GlobalEnv)
    }

  } else {
    funs <- rlang::enexprs(...)
    lapply(funs, function(x) if(!is.symbol(x))
      stop(sprintf("%s is not a symbol. Please provide valid function names",
                   deparse(x))))
    for(fun in funs){
      if(!is.symbol(fun)) stop(sprintf(
        "%s is not a symbol. Please provide valid function names", deparse(fun)))
      eval(substitute(class(FUN) <- union("tag_adverb",class(FUN)),
                      list(FUN = fun)),envir = .GlobalEnv)
    }
  }
  invisible(NULL)
}

#' @export
unset_tag_adverbs<- function(funs) {
  if(is.function(f)) {
    f_quoted <- substitute(f)
    if (!is.symbol(f_quoted)) stop("Please provide a valid function name")
  } else {
    f_quoted <- parse(text=f)[[1]]
  }
  eval(substitute(class(f) <- setdiff(class(f), "tag_adverb"),
                  list(f = f_quoted)),envir = .GlobalEnv)
  invisible(NULL)
}
