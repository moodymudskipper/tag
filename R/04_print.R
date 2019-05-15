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
