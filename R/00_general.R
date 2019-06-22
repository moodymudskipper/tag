add_class <- function(x, ...){
  class(x) <- union(c(...), class(x))
  x
}

rm_class <- function(x, cl){
  class(x) <- setdiff(class(x), cl)
  x
}

#because formals doesn't work on primitives such as sqrt etc
# on functions without arguments like `[` it will return ...
formals2 <- function(f){
  args <- as.list(args(f))
  if(!length(args))
    as.pairlist(alist(...=))
  else
    as.pairlist(args[-length(args)])
}

#' imports
#'
#' @importFrom rlang names2 exec exprs expr set_names enexpr syms call_args
#' @name imports
NULL


#' Pattern helpers
#'
#' These functions should only be used inside of tag definitions.
#'
#' @param eval wether to evaluate the call or arguments
#' @param type type of call. `"expanded"` uses `match.call()`, `"unexpanded"`
#'   uses `match.call(expand.dots=FALSE)`, `"raw"` uses `sys.call()`
#'
#' @details
#' \describe{
#'   \item{`f`}{input function}
#'   \item{`CALL()`}{return evaluated or unevaluated call}
#'   \item{`F_ARGS()`}{return evaluated or unevaluated arguments of input function}
#'   \item{`T_ARGS()`}{return evaluated or unevaluated arguments of tag}
#'   \item{`F_FORMALS()`}{return the input function's formals}
#'   \item{`T_FORMALS()`}{return he tag's formals}
#' }
#'
#'
#' @export
CALL <- function(eval = TRUE, type = c("expanded","unexpanded","raw")){
  stop("CALL() should only be called in a tag definition")
}

#' @export
#' @rdname CALL
T_ARGS <- function(eval = TRUE){
  stop("T_ARGS() should only be called in a tag definition")
}

#' @export
#' @rdname CALL
F_ARGS <- function(eval = TRUE, type = c("expanded","unexpanded","raw")){
  stop("F_ARGS() should only be called in a tag definition")
}

#' @export
#' @rdname CALL
T_FORMALS <- function(){
  stop("T_FORMALS() should only be called in a tag definition")
}

#' @export
#' @rdname CALL
F_FORMALS <- function(){
  stop("F_FORMALS() should only be called in a tag definition")
}













