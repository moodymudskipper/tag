#' ..ip tag to modify objects in place
#'
#' @param f a function that returns a modified object, fed as the first
#'   parameter
#'
#' @export
#' @examples
#' iris2 <- iris
#' ..ip$head(iris2,2)
#' iris2
#' ..ip$transform(iris2, Species2 = toupper(Species))
#' iris2
..ip <- function(f){
  fmls <- formals2(f)
  data_nm <- as.name(names(fmls)[1])
  res <- as.function(c(fmls,substitute({
    mc <- match.call()
    mc[[1]] <- quote(INPUT)
    eval.parent(substitute(X <- MC, list(X=substitute(DATA_NM), MC = mc)))
  }, list(DATA_NM = data_nm, INPUT = substitute(f)))))
  if(is_adverb(f)) res <- add_class(res, "tag","adverb")
  res
}
..ip(add_class)(..ip,"tag","adverb")


