add_class <- function(x, ...){
  class(x) <- union(c(...), class(x))
  x
}

rm_class <- function(x, cl){
  class(x) <- setdiff(class(x), cl)
  x
}

#because formals doesn't work on primitives such as sqrt etc
# on functions without arguments like `[` it will return ...
formals2 <- function(f){
  args <- as.list(args(f))
  if(!length(args))
    as.pairlist(alist(...=))
  else
    as.pairlist(args[-length(args)])
}

eval_deep <- function(x){
  lapply(x, function(y) if (is.list(y)) eval(y) else eval_deep(y))
}

#' imports
#'
#' @importFrom rlang names2 exec exprs expr set_names enexpr syms
NULL



