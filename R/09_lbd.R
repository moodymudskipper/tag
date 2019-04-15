
#' ..lbd tag to apply function on argument when argument is a formula
#'
#' Useful in functions such as `transform`, `mutate`, `summarize`.
#' @param f function
#'
#' @export
#'
#' @examples
#' ..lbd$transform(head(iris,2), Petal.Width = ~1000*(.), Species = ~toupper(.))
..lbd <- function(f){
  res <- as.function(c(formals2(f), substitute({
    mc <- match.call()
    mc[[1]] <- quote(f)
    mc[] <- purrr::imap(mc,~if(is.call(.) && identical(.[[1]],quote(`~`))) {
      rlang::expr(purrr::as_mapper(!!.)(!!rlang::sym(.y)))
      } else .)
    eval.parent(mc)
  })))
  if(is_adverb(f)) res <- add_class(res, "tag","adverb")
  res
}
..ip$add_class(..lbd, "tag","adverb")

# ..grp(..lbd$mutate)(
#   iris, Petal.Width = ~median(.), Sepal.Length = ~mean(.), .by = "Species")
# ..lbd(..grp$summarize)(
#   iris, Petal.Width = ~median(.), Sepal.Length = ~mean(.), .by = "Species")



