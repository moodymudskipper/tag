#' tag and tag_adverb methods for print
#'
#' *tag* provides `tag` and `tag_adverb` methods for `print`. Additionally to
#' the raw code, they print the class of the object and its definition.
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
print.tag <- function(x, ...){
  cat("# a tag\n\n")
  cat("definition:\n")
  print(attr(x,"definition"))
  cat("\nCode:\n")
  print(`attributes<-`(x, NULL))
  invisible(x)
}

#' @export
#' @rdname print.tag
print.tag_adverb <- function(x, ...){
  cat("# a tag_adverb\n\n")
  cat("definition:\n")
  print(attr(x,"definition"))
  cat("\nCode:\n")
  print(`attributes<-`(x, NULL))
  invisible(x)
}

# #' @export
# print.tag_partial <- function(x, ...){
#   cat("# a tag_partial\n\n")
#   print(rm_class(x, "tag_partial"))
#   cat("\nenclosing environment:\n\n")
#   print(fn_env_content(x))
#   invisible(x)
# }
#
# #' @export
# print.tagged_function <- function(x, ...){
#   cat("# a tagged function\n\n")
#   print(rm_class(x, "tagged_function"))
#   invisible(x)
# }
#
#
