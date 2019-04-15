#' ..incr tag to increment or decrement numeric variables
#'
#' Use unary of binary `-` and `+` to increment or decrement of `1` or amount
#'   given on the left hand side.
#' @param x numeric vector
#'
#' @return numeric vector
#' @export
#' @examples
#' \dontrun{
#' x <- 3
#' +..incr$x
#' ..incr$x + 5
#' -..incr$x
#' ..incr$x - 2
#' }
..incr <- function(x) {
  add_class(x, "tagged_incr")
}
..ip$add_class(..incr, "tag")

#' @export
`+.tagged_incr` <- function(e1,e2){
  E1 <- utils::tail(substitute(e1),1)[[1]]
  if(missing(e2)) E2 <- 1 else E2 <- substitute(e2)
  eval.parent(bquote(.(E1) <- .Primitive("+")(.(E1), .(E2))))
}

#' @export
`-.tagged_incr` <- function(e1,e2){
  E1 <- utils::tail(substitute(e1),1)[[1]]
  if(missing(e2)) E2 <- 1 else E2 <- substitute(e2)
  eval.parent(bquote(.(E1) <- .Primitive("-")(.(E1), .(E2))))
}
