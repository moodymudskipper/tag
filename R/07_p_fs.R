#' ..fs tag to start a dollar piped functional sequence
#'
#' Based on `%>%` from the package magrittr
#'
#' @param x a function
#'
#' @return a tagged function
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#'\dontrun{
#' ..fs(head)$dim()(iris)
#' ..fs$head(2)$gsub("h","X",.)(c("hello","hi","foo"))
#' }
..fs <- function(x) {
  res <- eval.parent(substitute((. %>% identity)$x))
  add_class(res, "tagged_fs")
}
..ip$add_class(..fs, "tag")

#' ..p tag to start a dollar pipe sequence
#'
#' @param x an object
#'
#' @export
#' @examples
#' \dontrun{
#' ..p$iris$head$Filter(is.numeric,.)$dim()
#' }
..p <- function(x){
  add_class(x, "tagged_p")
}
..ip$add_class(..p, "tag")


#' @method $ fseq
#' @export
#' @importFrom magrittr "%>%"
#' @rdname dollar_methods
`$.fseq` <- function(e1,e2) {
  # define `$` as a pipe operator as used in functional sequences
  # the first eval substitute will substitute e2, but not e1
  res <- eval(substitute(
    as.function(alist(...=,{
      # the second eval subtitute is double and will first replace list_call
      # by `list(...)` and then replace `list` by `. %>% e1 %>% e2`
      # then the expression will be deparsed to remove the annoying parenthesis
      # as the expression became `(. %>% e1 %>% e2)(...)`
      # then parsed back and evaluated
      call0 <- eval(substitute(substitute(
        list_call,
        list(list = quote(. %>% e1 %>% e2))),
        list(list_call = substitute(list(...)))))
      call0_str <- deparse(call0)
      fun <- eval(parse(text=sub("\\(","",gsub("\\)\\(","(",call0_str))))
      fun
    })),
    list(e2=substitute(e2))))
  add_class(res,"tagged_fs")
}

#' @export
`$.tagged_fs` <- function(e1,e2){
  eval(substitute(e1()$e2))
}

#' @method $ tagged_p
#' @export
#' @importFrom magrittr "%>%"
#' @rdname dollar_methods
`$.tagged_p` <- function(e1,e2) {
  res <- if(deparse(substitute(e2)) %in% names(e1) && !is.atomic(e1)){
    eval.parent(substitute(.Primitive("$")(e1,e2)))
  } else {
    # define `$` as a pipe operator in its standard use
    eval(substitute(function(...) {
      res <-eval(substitute(e1 %>% e2(...)))
      class(res) <- union("tagged_p", class(res))
      res
    },list(e2=as.name(e2))
    ))
  }
  class(res) <- "tagged_fs"
  res
}

#' @export
print.tagged_p <- function(x,...){
  print(rm_class(x, "tagged_p"),...)
  invisible(x)
}


