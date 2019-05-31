# #' @export
# is_tagged_function <- function(f){
#   inherits(f, "tagged_function")
# }

#' Check if a function is tag or a tag_adverb
#'
#' Checks if the class is `"tag"` or `"tag_adverb"`
#'
#' @param f a function
#' @export
is_tag <- function(f){
  inherits(f, "tag")
}

#' @export
#' @rdname is_tag
is_tag_adverb <- function(f){
  inherits(f, "tag_adverb")
}
