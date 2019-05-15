
#' @export
`$.tag_adverb` <- function (e1, e2) {
  eval.parent(bquote(.(e1)(.(as.symbol(e2)))))
}

#' @export
`$.tag` <- function (e1, e2) {
  eval.parent(substitute(E1()$E2,
              list(E1 = substitute(e1),
                   # because e2 is captured as a string after $
                   E2 = as.symbol(e2))))
}

