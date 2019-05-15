# mtcars %>%
#   group_by(cyl) %>% ..at$summarize(
#     at(vars(mpg,DISP = disp),
#            lst(median,MEAN = mean(.)),
#            .keys = vars(vs, AM=am)))
#
#

#' ..at tag to be able to use `at()` inside of `summarize` and `mutate`
#'
#' @param f function
#'
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' ..at$summarize(iris, at(vars(starts_with("Sepal")),lst(mean,median)))
#' mtcars %>%
#'   group_by(gear) %>%
#'   ..at$summarize(at(c("disp","hp"), funs(mean), .keys = vars(cyl)))
#' }
..at <- function(f){
  as.function(c(alist(...=), substitute({
    mc <- match.call()
    mc[[1]] <- quote(f)
    call <- eval(substitute(substitute(MC, list(at = quote(!!!.at))),list(MC = mc)))
    call[[2]] <- bquote(`*tmp*` <- .(call[[2]]))
    call <- parse(text= gsub(
      "(!!!.at)(", "!!!.at(`*tmp*`, ", deparse(call), fixed= TRUE))[[1]]
    eval.parent(call)
  })))
}
..ip$add_class(..at, "tag","adverb")

# .at <- function(.tbl, .vars, .funs, ...) {
#   dplyr:::manip_at(
#     .tbl, .vars, .funs, rlang::enquo(.funs), rlang:::caller_env(),
#     .include_group_vars = TRUE, ...)
# }

# @export
at <- function(){
  stop("`at` shouldn't be called directly")
}


manip_apply_syms2 <- function(vars, keys, funs, tbl, sep = "_"){
  # to avoid CMD check notes
  . = f_list = subset_quo = fun_quo = NULL
  # fix the change from funs to list
  default_env <- rlang::caller_env()
  funs <- purrr::map(funs, as_fun, default_env, list())

  val_tbl <- purrr::map(tbl[keys],unique) %>% tibble::enframe("key_old","value")
  expgr <- expand.grid(vars,keys,funs,stringsAsFactors = FALSE) %>%
    purrr::map_dfc(tibble::enframe) %>%
    stats::setNames(c("var_new", "var_old", "key_new", "key_old","f_name","f_list")) %>%
    dplyr::left_join(.,val_tbl, by = "key_old") %>%
    dplyr::mutate(
      subset_quo = purrr::pmap(.[c(2,4,7)],function(var_old, key_old, value)
        purrr::map(value, ~bquote(.(rlang::sym(var_old))[.(rlang::sym(key_old))==.(.x)])))) %>%
    dplyr::mutate(fun_quo = purrr::map2(f_list, subset_quo,
                          function(x,y) purrr::map(y, function(z) expr_substitute(x, quote(.), z))))

  fun_quos <- unlist(expgr$fun_quo)

  # remove cols to be able to unnest
  expgr <-
    expgr %>%
    dplyr::select(-fun_quo,-subset_quo, - f_list) %>%
    tidyr::unnest()
  # we don't unnest because we need to keep the type of value intact

  if(length(vars)==1 && is.integer(expgr$var_new)){
    varpart <- NULL
  } else {
    varpart <- expgr$var_new
    unnamed <- varpart==""
    varpart[unnamed] <- expgr$var_old[unnamed]
  }

  if(length(keys)==1 && is.integer(expgr$key_new)){
    keypart <- NULL
  } else {
    keypart <- expgr$key_new
    unnamed <- keypart==""
    keypart[unnamed] <- expgr$key_old[unnamed]
  }

  if(length(funs)==1 && is.integer(expgr$f_name)){
    fpart <- NULL
  } else {
    fpart <- expgr$f_name
  }

  nms <- paste(varpart,keypart,expgr$value,fpart, sep=sep)
  out <- stats::setNames(fun_quos,nms)
}

#' @export
.at <- function(.tbl, .vars, .funs, ..., .keys = NULL){
  # make character vars
  .vars <- tidyselect::vars_select(names(.tbl), !!!.vars)
  .keys <- tidyselect::vars_select(names(.tbl), !!!.keys)
  if(is.function(.funs)) .funs <- eval(substitute(lst(.funs)))
  if (!length(.keys)){
    return(manip_at(
      .tbl, .vars, .funs, rlang::enquo(.funs), rlang::caller_env(),
      .include_group_vars = TRUE, ...))
  }
  manip_apply_syms2 <- utils::getFromNamespace("manip_apply_syms2", "tags")
  manip_apply_syms2(.vars, .keys, .funs, .tbl)
}


as_fun <- getFromNamespace("as_fun", "dplyr")
expr_substitute <- getFromNamespace("expr_substitute", "dplyr")
manip_at <- getFromNamespace("manip_at", "dplyr")
