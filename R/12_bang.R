
#' @export
..bang <- function(f){
  res <- as.function(c(alist(...=), substitute({
    call <- as.call(c(quote(f), rlang::list2(!!!purrr::map(rlang::quos(...), 2))))
    eval.parent(call)
  })))
  if(is_adverb(f)) res <- add_class(res, "tag","adverb")
  res
}
..ip$add_class(..bang, "tag","adverb")

# u <- "speed"
# v <- quote(dist)
# w <- quo(time)
# x <- list(a=c(1, 2), b=c(3, 4))
# ..bang$transform(head(cars,2), !!w := !!v / !!sym(u), !!!x)

