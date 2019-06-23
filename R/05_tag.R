#' Build a tag or a tag_adverb
#'
#' `tag`s are essentially adverb factories, they build adverbs (also called
#' function operators) from a set of parameters. `tag_adverb`s are adverbs,
#' unlike `tag`s they take a function as a first parameter.
#' Both benefit from a special syntax using the `$` operator.
#'
#' @param pattern an unquoted expression, describes what will be done to the naked call
#'
#' @param args a pairlist (use `base::alist()` or `rlang::exprs()`) which will
#'  be used as the formals of the built `tag`
#' @param eval_args if `TRUE` (default), the tag arguments are evaluated so
#'   they can be used directly in the tag definition, set to `FALSE` to use
#'   NSE on tag arguments
#'
#' @section Formalism:
#' `tag`s are designed to be used intuitively and are best understood by
#' examples such as the ones proposed below.
#'
#' The output of a `tag` call is a `tag_adverb`. The output of a `tag_adverb`
#' call is called a manufactured function. The first argument of a `tag_adverb`
#' is called the input function.
#'
#' @section Using patterns:
#'
#' The `pattern` argument is a representation of the body of the manufactured
#' function. It allows for short definitions of `tag`s thanks
#' to a set of objects / function calls such as `f` and `CALL()` that one can
#' use in the definitions. see `?CALL`
#'
#' @export
tag <- function(pattern, args = alist(),
               eval_args = TRUE){
 pattern <- substitute(pattern)
 res <- eval(bquote(as_tag(tag_adverb(.(pattern), args, eval_args))))
 on.exit(rm(res, pattern))
 attr(res,"definition") <- expr(tag(
   pattern = !!pattern, args = !!args))
 res
}
#
# tag_adverb <- function(pattern, args = alist(),
#                        eval_args = TRUE){
#   # clean the enclosing environment
#   on.exit(rm(list=ls()))
#
#   f <- NULL # to avoid CMD check note
#
#   if("..." %in% names(args)) {
#     stop("`...` is not supported by tags and tag_adverbs. Use a list argument ",
#          "instead")
#   }
#
#   pattern <- substitute(pattern)
#
#   tag_adv <- as.function(c(alist(f=), args, expr({
#     # clean the enclosing environment
#     on.exit(rm(list=setdiff(ls(),c(
#       "f", "CALL", "T_ARGS", "F_ARGS", "T_FORMALS", "F_FORMALS"))))
#
#     # formals of the input function
#     f_formals  <- formals2(f)
#     # tag formals / or new formals
#     t_formals  <- !!args
#
#     if(length(intersect(names(f_formals), names(t_formals))))
#       stop("The new formals and the arguments of the input function are conflicting")
#
#     t_args <- rlang::call_args(match.call())[-1] # args minus f
#
#
#     # mf has same formals as f, plus unused tag formals
#     mf_formals <- formals2(f)
#
#     # and add the tag formals that are not in the adverb call
#     mf_formals <- c(mf_formals,t_formals[
#       ! names(t_formals) %in% names(t_args)])
#
#     # the call to be altered by the adverb is the same as the adverb call
#     # - with a different function (f instead of adverb$f or adverb(f))
#     # - without the arguments from the adverb
#
#     CALL <- as.function(c(
#       alist(eval = TRUE, type = c("expanded","unexpanded","raw")),
#       bquote({
#         type = match.arg(type)
#         # CALL will capture the match.call() or sys.call(), remove the
#         # arguments that are not formals of f, and replace the function by f,
#         # then optionally evaluate
#         .ENV <- eval.parent(quote(.ENV))
#         mc <-
#           if (type == "expanded") {
#             eval(quote(match.call()), .ENV)
#           } else if (type == "unexpanded") {
#             eval(quote(match.call(expand.dots = FALSE)), .ENV)
#           } else {
#             sys.call(frame_pos(.ENV))
#           }
#         args <- as.list(mc[-1])
#         args[.(names(t_formals))] <- NULL
#         unevaluated_call <- as.call(c(eval.parent(f), args))
#         if (eval) eval.parent(unevaluated_call, 2) else unevaluated_call
#       })
#     ))
#
#     F_ARGS <-  as.function(c(
#       alist(eval = TRUE, type = c("expanded","unexpanded","raw")),
#       quote({
#         type = match.arg(type)
#         .ENV <- eval.parent(quote(.ENV))
#         mc <-
#           if (type == "expanded") {
#             eval(quote(match.call()), .ENV)
#           } else if (type == "unexpanded") {
#             eval(quote(match.call(expand.dots = FALSE)), .ENV)
#           } else {
#             sys.call(frame_pos(.ENV))
#           }
#         unevaluated_args <- as.list(mc[-1])
#         if (eval) lapply(unevaluated_args, eval.parent, 3) else unevaluated_args
#       })
#     ))
#
#     # F_FORMALS simply returns f's formals, no local computation needed
#     F_FORMALS <-  as.function(list(f_formals))
#
#     T_ARGS <- as.function(c(alist(eval = TRUE), expr({
#       unevaluated_args <- !!quote(!!t_args)
#       if (eval) lapply(unevaluated_args, eval.parent, 3) else unevaluated_args
#     })))
#
#     # T_FORMALS simply returns the tag's formals, no local computation needed
#     T_FORMALS <- as.function(list(expr({!!quote(!!t_formals)})))
#
#     # if eval_args is TRUE, the tag arguments are evaluated and put in the
#     # local environment, if not the function only contains the pattern
#     manufactured_function <-
#       if(!!eval_args) {
#         as.function(c(mf_formals, quote({
#           .ENV <- environment()
#           list2env(T_ARGS(eval = TRUE), environment())
#           !!pattern
#         })))
#       } else {
#         as.function(c(mf_formals, quote({
#           .ENV <- environment()
#           !!pattern
#           })))
#       }
#
#     manufactured_function
#   })))
#   attr(tag_adv,"definition") <- expr(tag_adverb(
#     pattern = !!pattern, args = !!args))
#   add_class(tag_adv, "tag_adverb")
# }



frame_pos <- function(frame) {
  pos <- which(sapply(sys.frames(), identical, frame))
  if(!length(pos)) pos <- 0
  pos
}



tag_adverb <- function(pattern, args = alist(),
                       eval_args = TRUE){
  # clean the enclosing environment
  on.exit(rm(list=ls()))

  f <- NULL # to avoid CMD check note

  if("..." %in% names(args)) {
    stop("`...` is not supported by tags and tag_adverbs. Use a list argument ",
         "instead")
  }

  pattern <- substitute(pattern)

  tag_adv <- as.function(c(alist(f=), args, expr({
    .CALLS <- NULL
    # clean the enclosing environment
    on.exit(rm(list=setdiff(ls(),c(
      "f", "CALL", "T_ARGS", "F_ARGS", "T_FORMALS", "F_FORMALS"))))

    # formals of the input function
    f_formals  <- formals2(f)
    # tag formals / or new formals
    t_formals  <- !!args

    if(length(intersect(names(f_formals), names(t_formals))))
      stop("The new formals and the arguments of the input function are conflicting")

    t_args <- rlang::call_args(match.call())[-1] # args minus f


    # mf has same formals as f, plus unused tag formals
    mf_formals <- formals2(f)

    # and add the tag formals that are not in the adverb call
    mf_formals <- c(mf_formals,t_formals[
      ! names(t_formals) %in% names(t_args)])

    # the call to be altered by the adverb is the same as the adverb call
    # - with a different function (f instead of adverb$f or adverb(f))
    # - without the arguments from the adverb

    CALL <- as.function(c(
      alist(eval = TRUE, type = c("expanded","unexpanded","raw")),
      bquote({
        type = match.arg(type)
        args <- call_args(.CALLS[[type]])
        args[.(names(t_formals))] <- NULL
        unevaluated_call <- as.call(c(eval.parent(f), args))
        if (eval) {
          if(type == "unexpanded")
            stop('Don\'t use `eval = TRUE` with `call = "unexpanded"`')
          eval.parent(unevaluated_call, 2)
        } else {
          unevaluated_call
        }
      })
    ))

    F_ARGS <-  as.function(c(
      alist(eval = TRUE, type = c("expanded","unexpanded","raw")),
      quote({
        type = match.arg(type)
        unevaluated_args <- call_args(.CALLS[[type]]) %setdiff% T_FORMALS()
        if (eval) lapply(unevaluated_args, eval.parent, 3) else unevaluated_args
      })
    ))

    # F_FORMALS simply returns f's formals, no local computation needed
    F_FORMALS <-  as.function(list(f_formals))

    T_ARGS <- as.function(c(alist(eval = TRUE), expr({
      unevaluated_args <- (!!quote(!!t_args)) %union% (
        call_args(.CALLS[["expanded"]]) %intersect% T_FORMALS())
      if (eval) lapply(unevaluated_args, eval.parent, 3) else unevaluated_args
    })))

    # T_FORMALS simply returns the tag's formals, no local computation needed
    T_FORMALS <- as.function(list(expr({!!quote(!!t_formals)})))

    # if eval_args is TRUE, the tag arguments are evaluated and put in the
    # local environment, if not the function only contains the pattern
    manufactured_function <-
      if(!!eval_args) {
        as.function(c(mf_formals, quote({
          .CALLS <<- get_calls()
          list2env(T_ARGS(eval = TRUE), environment())
          !!pattern
        })))
      } else {
        as.function(c(mf_formals, quote({
          .CALLS <<- get_calls()
          !!pattern
        })))
      }

    manufactured_function
  })))
  attr(tag_adv,"definition") <- expr(tag_adverb(
    pattern = !!pattern, args = !!args))
  add_class(tag_adv, "tag_adverb")
}

# the use of .Internal is forbidden so making a copy
.Internal_ <- .Internal

get_calls <- function() {
  # just an optimized list(match.call(), match.call(expand.dots = TRUE), sys.call())
  # about 3.5 times faster
  call       <- .Internal_(sys.call(0L))
  definition <- .Internal_(sys.function(0L)) # sys.function(sp)
  envir      <- .Internal_(parent.frame(1L))
  list(
  unexpanded = .Internal_(match.call(definition, call, FALSE, envir)),
  expanded =   .Internal_(match.call(definition, call, TRUE, envir)),
  raw = call)
}

`%union%` <- function(e1, e2) {
  c(e1, e2[!names(e2) %in% names(e1)])
}

`%setdiff%` <- function(e1, e2) {
  e1[names(e2)] <- NULL
  e1
}

`%intersect%` <- function(e1, e2) {
  e1[intersect(names(e1), names(e2))]
}
