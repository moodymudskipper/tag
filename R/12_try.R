#' ..try tag to try a call or if failure call alternate call
#'
#' The expression stored in `else` will be executed if
#'
#' @param f function
#'
#' @export
#'
#' @examples
#' ..try$paste("hello","world", .else = "hi")
#' ..try$paste("hello", world, .else = "hi")
#' ..try$paste("hello", world, .else = "hi", .silent = TRUE)
..try <- function(f){
  res <- as.function(c(formals2(f),alist(.else=,.silent=FALSE), substitute({
    mc <- match.call()
    mc[[1]] <- quote(f)
    mc[c(".else",".silent")] <- NULL
    res <- eval.parent(bquote(try(.(mc), .(.silent))))
    if(inherits(res, "try-error")) res <- eval.parent(substitute(.else))
    res
  })))
  if(is_adverb(f)) res <- add_class(res, "tag","adverb")
  res
}
..ip$add_class(..try, "tag","adverb")
