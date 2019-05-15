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
#'  be used as the formals of the built `tag` or as additional parameters of the
#'  built `tag_adverb`
#' @param rm_args a character vector of formal names to remove from the the
#'   input function
#'
#' @section Formalism:
#' `tag`s are designed to be used intuitively and are best understood by
#' examples such as the ones proposed below.
#'
#' The output of a `tag` call is a `tag_adverb`. The output of a `tag_adverb`
#' call is called a manufactured function. The first argument of a `tag_adverb`
#' is called the input function.
#'
#' The `args` argument becomes either the formals of a `tag` object or the
#' additional arguments of a `tag_adverb` object (the first being `f`). We
#' may refer to `args` as the *new formals*.
#'
#' @section Syntax:
#' The `$` syntax offers an intuitive and flexible alternative to the traditional
#' adverb syntax. `some_tag(tag_arg)$fun(fun_arg)` can be used instead of
#' `some_tag(tag_arg)(fun)(fun_arg)`.
#'
#' When a parameter is not provided to the `tag` it is featured as a parameter
#' of its `tag_adverb` output so `some_tag(tag_arg)$fun(fun_arg)` can be written
#' `some_tag()(fun, tag_arg)(fun_arg)`.
#'
#' When a parameter is not provided to the `tag_adverb`, it is featured as a
#' parameter of the manufactured function, so the latter call can be spelt
#' `some_tag()(fun)(fun_arg, tag_arg)`, which we'll often simply write
#' `some_tag$fun(fun_arg, tag_arg)`.
#'
#' This forwarding of unused arguments mixed with the magic built into the
#' methods `$.tag` and `$.adverb` allows a lot of flexibility, and interesting
#' intermediate objects can be built to be used as shorthands or in functionals.
#'
#' @section Using patterns:
#' The `pattern` argument is a representation of the body of the manufactured
#' function. It allows for short definitions of `tag`s and `tag_adverb`s thanks
#'  to a set of objects that one can use in the definitions.
#'
#' Available objects are :
#'
#' \describe{
#'   \item{`f`}{input function}
#'   \item{`SCALL`}{unevaluated expression containing the naked input
#'     function call, i.e. the way the function would have been called without
#'     the tag or its arguments}
#'   \item{`CALL`}{Same as `SCALL` but the arguments that were not named in the
#'     call are named (unless they're part of the `...`)}
#'   \item{`CALL_UNEXP`}{Same as `CALL` but the `...` are *NOT* expanded}
#'   \item{`ARGS`}{List of the unevaluated arguments as given in the call,
#'   identical to `CALL`}
#'   \item{`FORMALS`}{List of the unevaluated arguments as given in the call}
#'   \item{`NEW_FORMALS`}{The formals of the `tag`, or additional formals of
#'     the `tag_adverb`}
#' }
#'
#' `f` and `CALL` suffice for most tag definitions.
#'
#' @export
#' @examples
#' ##################
#' # BUILDING A TAG #
#' ##################
#'
#' # this is the neutral tag, it doesn't do anything
#' identity_tag <- tag({eval(CALL)})
#' identity_tag$mean(c(1,2,NA,3), na.rm = TRUE)
#'
#' # this tags gives informations about the call using all our shortcuts
#'verbosing <- tag(args = alist(descr="Here's a breakdown of the call"), {
#'  message(descr)
#'  cat("----------------------------------------------\n")
#'  message("`f`: input function\n")
#'  print(f)
#'  cat("----------------------------------------------\n")
#'  message("`SCALL`: unevaluated expression containing the naked input function call\n")
#'  print(SCALL)
#'  cat("----------------------------------------------\n")
#'  message("`CALL`: Same as `SCALL` but the arguments that were not named in the call are named\n")
#'  print(CALL)
#'  cat("----------------------------------------------\n")
#'  message("`CALL_UNEXP`: Same as `CALL` but the `...` are *NOT* expanded\n")
#'  print(CALL_UNEXP)
#'  cat("----------------------------------------------\n")
#'  message("`ARGS`: List of the unevaluated arguments as given in the call, identical to `CALL`\n")
#'  print(ARGS)
#'  cat("----------------------------------------------\n")
#'  message("`FORMALS`: List of the unevaluated arguments as given in the call\n")
#'  print(FORMALS)
#'  cat("----------------------------------------------\n")
#'  message("`NEW_FORMALS`: The formals of the `tag`, or additional formals of the `tag_adverb`\n")
#'  print(NEW_FORMALS)
#'  cat("----------------------------------------------\n")
#'  message("`Now evaluating call:\n")
#'  eval(CALL)
#' })
#'
#' verbosing("See details below:")$mean(c(1,2,NA,3), na.rm = TRUE)
#'
#' ##########
#' # SYNTAX #
#' ##########
#'
#' # We build a copy of the `viewing` tag from the package `tags` and showcase
#' # the syntax.
#'
#' # viewing2 tag to view the output of a call
#' viewing2 <- tag(args = alist(.title = "View"),{
#'   res <- eval(CALL)
#'   View(res,title = .title)
#'   res
#' })
#' \dontrun{
#' viewing2("A")(head)(cars, 2) # "A" is fed to the tag
#' class(viewing2)
#' class(viewing2("A"))
#' class(viewing2("A")(head))
#' viewing2("B")$head(cars, 2)              # equivalent, arguably more readable
#' viewing2()(head, "C")(cars, 2)           # "C" is fed to the tag_adverb
#' viewing2()(head)(cars, 2, .title = "D")  # "D" is fed to the manufactured function
#' viewing2()$head(cars, 2, .title = "E")   # "E" equivalent, a bit more readable
#' viewing2$head(cars, 2, .title = "F")     # equivalent, arguably more readable
#' }
#'
#' #################
#' # MORE EXAMPLES #
#' #################
#' # these are copies of some simple tags that can be found in the package tags
#'
#' # strictly2 tag to adjust strictness of a call
#' strictly2 <- tag(args = alist(warn=2),{
#'   w <- options("warn")[[1]]
#'   on.exit(options(warn=w))
#'   options(warn= warn)
#'   eval(CALL)
#' })
#' strictly2(-1)$sqrt(-1)
#'
#' # dbg tag to debug a call
#' dbg2 <- tag({
#'   debugonce(f)
#'   exec("f", !!!purrr::map(ARGS, eval))
#' })
#' \dontrun{
#'   dbg2$sample(5)
#' }
tag <- function(pattern, args = alist(), rm_args = character(0)){
  on.exit(rm(res, pattern))
  pattern <- substitute(pattern)
  res <- eval(bquote(as_tag(tag_adverb(.(pattern), args, rm_args))))
  attr(res,"definition") <- rlang::expr(tag(
    pattern = !!pattern, args = !!args, rm_args = !!rm_args))
  res
}


#' @export
#' @rdname tag
tag_adverb <- function(pattern, args = alist(), rm_args = character(0)){
  f <- NULL # to avoid CMD check note

  pattern <- substitute(pattern)

  # clean the enclosing environment
  on.exit(rm(list=ls()))

  tag_adv <- as.function(c(alist(f=), args, rlang::expr({

    # tag additional formals
    all_args   <- as.list(match.call()[-1])
    f_formals  <- formals2(f)
    t_formals  <- !!args
    rm_args    <- !!rm_args
    t_args     <- all_args[names(all_args) %in% names(t_formals)]

    # clean the enclosing environment
    # we leave access to f and the new formals
    on.exit(rm(list=setdiff(ls(),c("f", names(t_formals)))))

    # the manufactured function has the same parameters as f, minus rm_args
    # and plus the tag formals that the adverb hasn't used.
    # starting from the original formals :
     mf_formals <- formals2(f)
    # we remove rm_args args from them
     mf_formals[rm_args] <- NULL
    # and add the tag formals that are not in the call
     mf_formals <- c(mf_formals,t_formals[
       ! names(t_formals) %in% names(match.call()[-1])])

    # the call to be altered by the adverb is the same as the adverb call
    # - with a different function (f instead of adverb$f or adverb(f))
    # - without the arguments from the adverb
    f_call <- substitute(as.call( c(f,`[<-`(
      as.list(match.call()[-1]),
      ARG_NAMES, value = NULL))),
      list(ARG_NAMES = names(t_formals)))

    f_call_unexp <- substitute(as.call( c(f,`[<-`(
      as.list(match.call(expand.dots = FALSE)[-1]),
      ARG_NAMES, value = NULL))),
      list(ARG_NAMES = names(t_formals)))

    s_call <- substitute(as.call( c(f,`[<-`(
      as.list(sys.call()[-1]),
      ARG_NAMES, value = NULL))),
      list(ARG_NAMES = names(t_formals)))

    as.function(c(mf_formals, substitute({
      SCALL <- SCALL_0
      CALL <- CALL_0
      CALL_UNEXP <- CALL_UNEXP_0
      ARGS     <- as.list(CALL[-1])
      ARGS_UNEXP <- as.list(CALL_UNEXP[-1])
      NEW_FORMALS <- NEW_FORMALS_0
      FORMALS <- formals2(f)
      # print(dplyr::lst(CALL, FUN, ARGS, A_FORMALS, F_FORMALS))
      PATTERN
    }, list(PATTERN = quote(!!pattern),
            SCALL_0 = s_call,
            CALL_0 = f_call,
            CALL_UNEXP_0 = f_call_unexp,
            NEW_FORMALS_0 = t_formals))))
  })))
  attr(tag_adv,"definition") <- rlang::expr(tag_adverb(
    pattern = !!pattern, args = !!args, rm_args = !!rm_args))
  add_class(tag_adv, "tag_adverb")
}


