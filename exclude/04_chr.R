
#' ..chr tag to use arithmetic operators on characters
#'
#' Use `+` to append without separator (or to apply a function), `-` to prepend,
#' `*` to duplicate the string, and `/` to subset the elements that satisfy
#' the regex given as a rhs.
#'
#' @param x a character vector
#'
#' @return a tagged_chr object
#' @export
#'
#' @examples
#' x <- "hello"
#' ..chr$x + "world"
#' ..chr$x + "world" - "oh"
#' ..chr$x + "world" - "oh" + toupper + unclass
#' ..chr$x * 3
#' x <- c("hello","sup", "hey","mornin")
#' ..chr$x / "^h"
..chr <- function(x) {
  add_class(x,"tagged_chr")
}
..ip$add_class(..chr, "tag")

#' @export
`print.tagged_chr` <- function(x, ...) {
  print(rm_class(x,"tagged_chr"))
  invisible(x)
}

#' @export
`+.tagged_chr` <- function(e1,e2) {
  if(is.function(e2)) return(e2(e1))
  add_class(paste0(e1,e2), "tagged_chr")
}

#' @export
`-.tagged_chr` <- function(e1,e2) {
  add_class(paste0(e2,e1), "tagged_chr")
}

#' @export
`*.tagged_chr` <- function(e1,e2) {
  add_class(paste(rep(e1,e2),collapse=""), "tagged_chr")
}

#' @export
`/.tagged_chr` <- function(e1,e2) {
  add_class(e1[grepl(e2,e1)], "tagged_chr")
}

