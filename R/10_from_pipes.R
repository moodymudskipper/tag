#' @export
..debug <- function(f){
  res <-as.function(c(formals2(f), substitute({
    mc <- match.call()
    mc[[1]] <- quote(f)
    eval.parent(debugonce(f))
    eval.parent(mc)
  })))
  if(is_adverb(f)) res <- add_class(res, "tag","adverb")
  res
}
..ip$add_class(..debug, "tag","adverb")

#' @export
..View <- function(f){
  res <- as.function(c(formals2(f), substitute({
    mc <- match.call()
    mc[[1]] <- quote(f)
    res <- eval.parent(mc)
    View(res,deparse(mc))
    res
  })))
  if(is_adverb(f)) res <- add_class(res, "tag","adverb")
  res
}
..ip$add_class(..View, "tag","adverb")

#' @export
..nowarn <- function(f){
  res <- as.function(c(formals2(f),warn = -1, substitute({
    w <- options("warn")[[1]]
    on.exit(options(warn=w))
    options(warn= warn)
    mc <- match.call()
    mc$warn <- NULL
    mc[[1]] <- quote(f)
    eval.parent(mc)
  })))
  if(is_adverb(f)) res <- add_class(res, "tag","adverb")
  res
}
..ip$add_class(..nowarn, "tag","adverb")

#' @export
..strict <- function(f){
  res <- as.function(c(formals2(f), substitute({
    w <- options("warn")[[1]]
    on.exit(options(warn=w))
    options(warn=2)
    mc <- match.call()
    mc[[1]] <- quote(f)
    eval.parent(mc)
  })))
  if(is_adverb(f)) res <- add_class(res, "tag","adverb")
  res
}
..ip$add_class(..strict, "tag","adverb")


