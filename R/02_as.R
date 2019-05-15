# tag_adverb is a clean definition that doesn't call any of tag, as_tag, as_tag_adverb
# tag calls tag_adverb and as_tag on top of it
# if as_tag receives a tag adverb with a tag_adverb(...) definition, it
# adapts the definition by substituting,


# When defining a tag or an adverb, they get a definition and a class
# when converting, we check these definitions
# if the definition starts with `tag` or `tag_adverb` we can do a substitution
# and execute it, it will ensure that
# as_tag(as_tag_adverb(x)) is the identity transofmation for a tag adverb
# defined in this package (and same for tag adverbs)


#' Convert to a tag or a tag_adverb
#'
#' Convert without loss of information
#'
#' @param f a function operator (adverb)
#'
#' @export
as_tag <- function(f){
  if(is_tag(f)) return(f)
  on.exit(rm(tag_, tag_formals))

  # We only convert to tags from tag_adverbs
  f <- as_tag_adverb(f)
  tag_formals <- formals2(f)[-1]
  tag_ <- as.function(c(tag_formals, quote({
    # my_tag(t_arg = foo) needs to translate into partial(f, t_arg = foo)
    partial_args <- as.list(match.call())[-1]
    tag_adv <- purrr::partial(f, !!!partial_args)
    attr(tag_adv, "definition") <- rlang::expr(purrr::partial(!!f, !!!partial_args))
    add_class(tag_adv, "tag_adverb")
  })))
  tag_ <- add_class("tag", x = tag_)

  # if we have a proper tag_adverb definition, we adapt it to have a tag definition
  # if we have a tag_adverb definition, we adapt it likewise
  attr(tag_, "definition") <- attr(f, "definition")
  attr(tag_, "definition")[[1]] <-
  if(identical(attr(f, "definition")[[1]], quote(tag_adverb))) {
    quote(tag)
  } else if(identical(attr(f, "definition")[[1]], quote(as_tag_adverb))) {
    quote(as_tag)
  } else {
    rlang::expr(as_tag(!!attr(f, "definition")))
  }
  # attr(tag_, "definition") <- do.call(substitute, list(match.call(), list(f = eval(f))))
  tag_
}


#' @export
#' @rdname as_tag
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
    adv <- as.function(c(formals(f),rlang::expr({
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
