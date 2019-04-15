
add_class <- function(x, ...){
  class(x) <- union(c(...), class(x))
  x
}

rm_class <- function(x, cl){
  class(x) <- setdiff(class(x), cl)
  x
}

#' dollar methods for tag functions and tagged object
#'
#' tag functions can be used using the dollar notation so for `..foo$x`
#' is equivalent to `..foo(x)`. The resulting object may have their own
#' dollar method as well.
#' @param e1 lhs
#' @param e2 rhs
#' @name dollar_methods
NULL

#' @export
#' @rdname dollar_methods
`$.tag` <- function(e1, e2){
  # change original call so x$y, which is `$.tag`(tag=x, data=y), becomes x(y)
  mc <- match.call()
  mc[[1]] <- mc[[2]]
  mc[[2]] <- NULL
  names(mc) <- NULL
  # evaluate it in parent env
  eval.parent(mc)
}

#because formals doesn't work on primitives such as sqrt etc
# on fuctions without arguments like `[` it will return ...
formals2 <- function(f){
  args <- as.list(args(f))
  if(!length(args))
    as.pairlist(alist(...=))
  else
    as.pairlist(args[-length(args)])
}


# necessary to avoid bug
# https://github.com/tidyverse/dplyr/issues/548
.datatable.aware = TRUE

#`%>%` <- magrittr::`%>%`

is_tag    <- function(x) inherits(x, "tag")
is_adverb <- function(x) inherits(x, "adverb")

#
# # not working yet
# set_adverb <- function(f){
#   browser()
#   f_nm <- deparse(substitute(f))
#   formals_ <- formals(f)
#   class(f) <- union("adverb", class(f))
#   args <- lapply(formalArgs(f), as.name)
#   # copy original foo into foo.function
#   assign(
#     sprintf("%s.function",f_nm),
#     f,
#     parent.frame())
#   # foo.formula calls foo.function on rlang::as_function ()
#   assign(
#     sprintf("%s.formula",f_nm),
#     as.function(c(formals_, bquote({
#       .(args[[1]]) <- rlang::as_function(.(args[[1]]))
#         .(do.call(call,c("mean", args)))}))),
#       parent.frame())
#   # foo.default triggers explicit error
#   assign(
#     sprintf("%s.default", f_nm),
#     as.function(c(formals_, bquote(
#       stop(.(sprintf("%s can only be used on functions and formulas",f_nm)))))),
#     parent.frame())
#   # overwrite foo to make generic
#   assign(
#     f_nm,
#     as.function(c(formals_, bquote(UseMethod(.(f_nm))))),
#     parent.frame())
#   invisible(NULL)
# }
