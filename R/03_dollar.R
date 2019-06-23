#' tag and tag_adverb methods for $
#'
#' *tag* provides `tag` and `tag_adverb` methods for `$` which are essentially
#' syntactic sugar, and the additional advantage to provide autocomplete in some
#' cases.
#'
#' `$.tag_adverb` makes it possible to write `some_tag(tag_arg)$fun(fun_arg)`
#' instead of `some_tag(tag_arg)(fun)(fun_arg)`.
#'
#' `$.tag` makes it possible to write `some_tag$fun(fun_arg, tag_arg)`
#' instead of `some_tag()(fun)(fun_arg, tag_arg)`.
#'
#' A additional advantage of `$.tag` is that autocomplete will be available to the
#' arguments of the manufactured function.
#' @name dollar
#' @param e1 lhs
#' @param e2 rhs
#'
#' @examples
#' \dontrun{
#' library(tags)
#' # These do the same, thanks to `$.tag_adverb`
#' trying(.else = "hi", .silent = TRUE)(paste)("hello", world)
#' trying(.else = "hi", .silent = TRUE)$paste("hello", world)
#'
#' # These do the same, thanks to `$.tag`
#' trying()(paste)("hello", world, .else = "hi", .silent = TRUE)
#' trying$paste("hello", world, .else = "hi", .silent = TRUE)
#' }
NULL


#' @rdname dollar
#' @export
`$.tag_adverb` <- function (e1, e2) {
  e2_val <- get(e2,envir = parent.frame())
  if(is_tag(e2_val)) {
    composed_adverb <- as.function(c(
      alist(f=),
      formals2(e2_val),
      substitute(
        E1(exec(E2, !!!call_args(match.call())[-1])(f)),
        list(E1 = substitute(e1), E2 = as.symbol(e2)))
    ))
    composed_adverb <- as_tag(add_class(composed_adverb, "tag_adverb"))
    def <- match.call()
    def[[1]] <- quote(`$`)
    attr(composed_adverb, "definition") <- def
    return(composed_adverb)
  }
  eval.parent(bquote(.(e1)(.(as.symbol(e2)))))
}

#' @rdname dollar
#' @export
`$.tag` <- function (e1, e2) {
  e2_val <- get(e2,envir = parent.frame())
  if(is_tag(e2_val)) {
    composed_adverb <- as.function(c(
      alist(f=),
      formals2(e2_val),
      substitute(
        E1()(exec(E2, !!!call_args(match.call())[-1])(f)),
        list(E1 = substitute(e1), E2 = as.symbol(e2)))
    ))
    composed_adverb <- as_tag(add_class(composed_adverb, "tag_adverb"))
    def <- match.call()
    def[[1]] <- quote(`$`)
    attr(composed_adverb, "definition") <- def
    return(composed_adverb)
  }
  eval.parent(substitute(E1()$E2,
              list(E1 = substitute(e1),
                   # because e2 is captured as a string after $
                   E2 = as.symbol(e2))))
}




