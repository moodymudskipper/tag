#' ..grp tag to group and ungroup around another operation
#'
#' The tagged function gains a parameter `.by`, groups that existed before
#' are preserved. One can group by a computed variable, it will be kept if
#' named, dropped otherwise.
#'
#' @param fun The function to tag
#'
#' @return a tagged function
#' @export
#'
#' @examples
#' library(dplyr)
#' ..grp(summarize)(iris,meanSL = mean(Sepal.Length), .by="Species")
#' ..grp$summarize(iris,meanSL = mean(Sepal.Length), .by="Species")
#' ..grp$summarize_all(iris,mean, .by="Species")
#' ..grp$slice(iris,1, .by="Species")
#' ..grp$summarize(iris,meanSW = mean(Sepal.Width),
#'   .by= vars(Species, Sepal.Width > 3.2))
#' ..grp$summarize(iris,meanSW = mean(Sepal.Width), .by= vars(
#'   Species, long_sepal = Sepal.Width > 3.2))
..grp <- function(fun){
  if(!requireNamespace("dplyr"))
    stop("The package dplyr must be installed to use `..grp`.")
  res <- as.function(c(alist(.data=, ...=, .by=NULL), substitute({
    # store names of by calls that were not created with an explicit name
    # they will be removed in the ends
    tmp_by_cols <- if (inherits(.by, "quosures")) {
      nms  <- rlang::names2(.by)
      vals <- purrr::map_chr(.by[nms == ""],~deparse(.[[2]]))
      setdiff(vals, names(.data))
    } else character(0)

    if (is.character(.by))
      .by <- tbl_at_syms(.data, .by)
    if (is.null(by)) {
      x <- eval(rlang::expr(fun(.data, !!!purrr::map(rlang::enquos(...),dplyr::last))))
    } else  {
      # save existing groups
      groups <- dplyr::group_vars(.data)
      # add new groups
      x <- dplyr::group_by(.data,!!!.by, add = TRUE)
      # call the former function, this complex form was necessary
      # to splice quosures well in all cases
      x <- eval(rlang::expr(fun(x, !!!lapply(rlang::enquos(...), dplyr::last))))
      # re establish former groups
      x <- dplyr::group_by(x, !!!rlang::syms(groups), add = FALSE)
      # remove temporary by cols
      if (length(tmp_by_cols)) x <- dplyr::select(x,-!!rlang::sym(tmp_by_cols))
    }
    x
  })))
  if(is_adverb(fun)) res <- add_class(res, "tag","adverb")
  res
}
..ip$add_class(..grp, "tag","adverb")

tbl_at_syms <- getFromNamespace("tbl_at_syms", "dplyr")

