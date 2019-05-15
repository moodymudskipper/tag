#' ..w tag for compact subsetting and evaluation within objects
#'
#' @param x object
#'
#' @return object of class `tagged_w`
#' @export
#'
#' @examples
#'\dontrun{
#' ..w$iris[nrow(.),]
#' vec1 <- c(a=1,b=2,c=3,d=4)
#' ..w$vec1[.>b]
#' vec2 <- 1:20
#' ..w$vec2[.>10]
#' ..w$iris[[nrow(.)]]
#' vec1 <- c(a=1,b=2,c=3,d=4)
#' ..w$vec1[[b+c]]
#' vec2 <- 1:20
#' ..w$vec2[[length(.)/2]]
#' }
..w <- function(x) {
  add_class(x,"tagged_w")
}
..ip$add_class(..w, "tag")

#' @export
`[.tagged_w` <- function(e1,...) {
  mc <- match.call()
  mc[[1]] <- quote(`[`)
  mc[[2]] <- utils::tail(mc[[2]],1)[[1]]
  names(mc) <- NULL
  E1 <- mc[[2]]
  res <- if(is.list(e1)){
    eval(eval(substitute(substitute(with(E1,MC), list(. = quote(E1))),
                        list(E1 = mc[[2]],
                             MC = mc))))
  } else if(!is.list(e1) && !is.null(names(e1))){
    eval(eval(substitute(substitute(with(as.list(E1),MC), list(. = quote(E1))),
                         list(E1 = mc[[2]],
                              MC = mc))))
  } else {
    eval(eval(substitute(substitute(MC, list(. = quote(E1))),
                         list(E1 = mc[[2]],
                              MC = mc))))
  }
  rm_class(res, "tagged_w")
}

#' @export
`[[.tagged_w` <- function(e1,expr) {
  e1_nm <- utils::tail(match.call()[[2]],1)[[1]]

  res <- if(is.list(e1)){
    eval.parent(eval(substitute(substitute(with(E1,EXPR), list(. = quote(E1))),
                                list(E1 = e1_nm,
                                     EXPR = substitute(expr)))))
  } else if(!is.list(e1) && !is.null(names(e1))){
    eval.parent(eval(substitute(substitute(with(as.list(E1),EXPR), list(. = quote(E1))),
                                list(E1 = e1_nm,
                                     EXPR = substitute(expr)))))
  } else {
    eval.parent(eval(substitute(substitute(EXPR, list(. = quote(E1))),
                                list(E1 = e1_nm,
                                     EXPR = substitute(expr)))))
  }
  class(res) <- setdiff(class(res), "dollar_w")
  res
}


