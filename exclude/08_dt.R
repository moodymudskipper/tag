# necessary to avoid bug
# https://github.com/tidyverse/dplyr/issues/548
.datatable.aware = TRUE


#' ..dt tag to use data.table syntax without explicit conversion
#'
#' Will return an object of same class as input
#'
#' @param data object coercible to data.table
#'
#' @return an object of same class as input
#' @export
#'
#' @examples
#' iris2 <- head(iris,2)
#' ..dt$iris2[,Species2:=toupper(Species)]
..dt <- function(data){
  add_class(data,"tagged_dt")
}
..ip$add_class(..dt, "tag")

#' @export
`[.tagged_dt` <- function(data,...){
  cl <- class(data)
  data <- data.table::as.data.table(data)[...]
  class(data) <- setdiff(cl, "tagged_dt")
  data
}



