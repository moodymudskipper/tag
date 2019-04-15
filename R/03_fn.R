#' ..fn tag to be able to use formula notation on FUN arguments
#'
#' inspired by `gsubfn::fn`, `..fn` is similar but use `rlang::as_function`
#' whre *gsubfn* uses its own API.
#'
#' @param f function to tag
#'
#' @return a modified function
#' @export
#'
#' @examples
#' \dontrun{
#' ..fn(sapply)(iris, ~class(.))
#' ..fn$sapply(iris, ~class(.))
#' sapply2 <- ..fn$sapply
#' sapply2(iris, ~class(.))
#' }
..fn <- function(f){
  if(!requireNamespace("rlang"))
    stop("The package rlang must be installed to use `..fn`.")
  res <- as.function(c(formals2(f), substitute({
    mc <- match.call()
    mc[[1]] <- quote(f)
    mc[["FUN"]] <- substitute(rlang::as_function(FUN), list(FUN = mc[["FUN"]]))
    eval.parent(mc)
  })))
  if(is_adverb(f)) res <- add_class(res, "tag","adverb")
  res
}
..ip$add_class(..fn, "tag", "adverb")
