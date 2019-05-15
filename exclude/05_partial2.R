# partial2 <- function(.f, ...){
#   # browser()
#   dot_args  <- list(...) # evaluated in calling environment
#   dot_names <- rlang::names2(dot_args)
#   f_formals <- formals2(.f)
#   f_names   <- rlang::names2(f_formals)
#   # unnamed arguments are given names if they fall before the dots
#   unnamed_index <- match("", dot_names)
#   unused_formals <- setdiff(f_names, dot_names)
#   ellipsis_pos   <- match("...", names(f_formals))
#   if(!is.na(ellipsis_pos))
#     unused_formals <- unused_formals[seq_len(ellipsis_pos)-1]
#   n_renamed <- min(length(unnamed_index), length(unused_formals))
#   names(dot_args)[unnamed_index[seq_len(n_renamed)]] <-
#     unused_formals[seq_len(n_renamed)]
#   dot_names <- names(dot_args)
#   # new args are formals that are NOT given in dots
#   new_args <- f_formals[setdiff(f_names, dot_names)]
#
#   # clean enclosing environment when exiting
#   on.exit(rm(new_args, res, f_formals,
#              unnamed_index, unused_formals, ellipsis_pos,
#              n_renamed))
#
#   # build function
#   res <- as.function(c(new_args, substitute({
#     #browser()
#     # replace the calling function by the original function
#     # and add the fixed arguments, then evaluate
#     mc <- match.call()
#     args_unsorted <- c(dot_args, `[<-`(as.list(mc)[-1], dot_names, value = NULL))
#     # to each arg is assessed a number
#     pos <- match(names2(args_unsorted),f_names)
#     # args not found are fed to
#     # the unnamed arguments are given to the missing positions before the
#     # optional ...
#     pos[is.na(pos)] <- match("...", f_names)
#     args_sorted <- args_unsorted[order(pos)]
#     mc <- as.call(c(f,args_sorted))
#     res <- eval.parent(mc)
#     # we remove the partial arg from the partialized adverb
#     res <- as.function(`[<-`(as.list(res), dot_names, value = NULL ))
#   },
#   list(f = .f) #as.function(`[<-`(as.list(.f), dot_names, value = NULL )))
#   )))
#
#
#
#   # add class to be able to use adequate printing method later
#   add_class(res, "tag_partial")
# }


# partial3 <- as_tag(partial2)
# narm <- partial3(na.rm = TRUE)
# debugonce(narm)
# narm(mean)(c(1,NA,3))
# partial2(mean,na.rm = TRUE)(c(1,NA,3))

# partial2(head, iris)(2)
# partial2(head, x = iris)(2)
# partial2(head, iris)(n =2)
# partial2(head, x = iris)(n =2)
# partial2(head, n = 2)(iris)
# partial2(head, n = 2)(x =iris)
#
# partial2(utils:::head.data.frame, iris)(2)
# partial2(utils:::head.data.frame, x = iris)(2)
# partial2(utils:::head.data.frame, iris)(n =2)
# partial2(utils:::head.data.frame, x = iris)(n =2)
# partial2(utils:::head.data.frame, n = 2)(iris)
# partial2(utils:::head.data.frame, n = 2)(x =iris)


## old version for archive
# partial2 <- function(f, ...){
#   dot_names <- names(substitute(...()))
#   dot_args  <- list(...) # evaluated in calling environment
#   f_formals <- formals2(f)
#   # new args are formals that are NOT given in dots
#   new_args <- f_formals[setdiff(names(f_formals), dot_names)]
#
#   # clean enclosing environment when exiting
#   on.exit(rm(f_formals, new_args, res, dot_names))
#
#   # build function
#   res <- as.function(c(new_args, quote({
#     # replace the calling function by the original function
#     # and add the fixed arguments, then evaluate
#     mc <- match.call()
#     mc[[1]] <- f
#     mc <- as.call(c(as.list(mc),dot_args))
#     eval.parent(mc)
#   })))
#
#   # add class to be able to use adequate printing method later
#   add_class(res, "tag_partial")
# }
