# tag_adverb doesn't call any of tag, as_tag, as_tag_adverb
# tag calls tag_adverb and as_tag on top of it
# if as_tag receives a tag adverb with a tag_adverb(...) definition, it
# adapts the definition by substituting,

# When defining a tag or an adverb, they get a definition and a class
# when converting, we check these definitions
# If the definition starts with `tag` or `tag_adverb` we can do a substitution
# and execute it, it will ensure that
# as_tag(as_tag_adverb(x)) is the identity transformation for a tag adverb
# defined in this package. The conversion works in a similar fashion in the
# opposite direction.

#' Convert to a tag
#'
#' convert an regular function operator to a tag
#'
#' `as_tag` might not work on all cases but is made available as a
#' convenient way to change most adverbs into tags and benefit from
#' their syntax.
#'
#' @param f a function operator
#' @export
#' @example
#' using_quietly0 <- as_tag(purrr::quietly)
#' using_quietly0$sqrt(-1)
#' # https://colinfay.me/purrr-adverb-tidyverse/
#' sleepy <- function(fun, sleep){
#'   function(...){
#'    Sys.sleep(sleep)
#'    fun(...)
#'   }
#' }
#' sleep_print <- sleepy(Sys.time, 5)
#' sleep_print()
#'
#' using_sleepy0 <- as_tag(sleepy)
#' using_sleepy0(5)$Sys.time()
#' using_sleepy0$Sys.time(sleep = 5)
#' # by comparison, to define it directly as a tag :
#' using_sleepy1 <- tag(args=alist(.sleep=), {
#'   Sys.sleep(.sleep)
#'   CALL(eval=TRUE)})
#' using_sleepy1(5)$Sys.time()
#' using_sleepy1$Sys.time(.sleep = 5)
as_tag <- function(f){
  if(is_tag(f)) return(f)

  # clean up enclosing environment on exit
  on.exit(rm(tag_, tag_formals))

  # Convert first to tag_adverb
  f <- as_tag_adverb(f)

  # tags have same formals as tag_adverbs, minus the first one
  tag_formals <- formals2(f)[-1]

  tag_ <- as.function(c(tag_formals, quote({
    # get args from call
    partial_args <- rlang::call_args(match.call())
    # a tag creates a tag adverb, which is f partialized on given arguments
    tag_adv <- purrr::partial(f, !!!partial_args)
    # give the tag adverb has a definition attribute and a class
    attr(tag_adv, "definition") <- expr(purrr::partial(!!f, !!!partial_args))
    add_class(tag_adv, "tag_adverb")
  })))

  # give the tag a definition attribute and a class

  # tag definitions are adapted from tag_adverb definitions
  attr(tag_, "definition") <- attr(f, "definition")
  attr(tag_, "definition")[[1]] <-
  if(identical(attr(f, "definition")[[1]], quote(tag_adverb))) {
    quote(tag)
  } else if(identical(attr(f, "definition")[[1]], quote(as_tag_adverb))) {
    quote(as_tag)
  } else {
    expr(as_tag(!!attr(f, "definition")))
  }
  add_class("tag", x = tag_)
}

as_tag_adverb<- function(f) {
  if(is_tag_adverb(f)) return(f)

  if(is_tag(f)) {
    # if f is tag, rather than do a conversion, we reapply the definition
    # all tags have a definition, which starts by either `tag` or `as_tag`
    adv <- eval(do.call(substitute, list(attr(f, "definition"), list(
      tag = quote(tag_adverb), as_tag = quote(as_tag_adverb)))))
    attr(adv, "definition") <- do.call(substitute, list(attr(f, "definition"), list(
      tag = quote(tag_adverb), as_tag = quote(as_tag_adverb))))
    add_class(adv, "tag_adverb")
  } else  {
    # if it's an external adverb we rework it a bit and add a definition attribute
    adv <- as.function(c(formals(f),expr({
      f <- !!f
      f_fmls <- formals()
      # build manufactured function
      mc <- match.call()
      mc[[1]] <- f
      mf <- eval.parent(mc)
      # add to its formals the adverb formals that were not used
      formals(mf) <- c(formals(mf),
                       f_fmls[!names(f_fmls) %in% names(mc[-1])])
      mf
    })))
    attr(adv, "definition") <- as.call(c(quote(as_tag_adverb),f))
    add_class(adv, "tag_adverb")
  }
}
