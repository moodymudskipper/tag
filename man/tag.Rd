% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_tag.R
\name{tag}
\alias{tag}
\title{Build a tag or a tag_adverb}
\usage{
tag(pattern, args = alist(), eval_args = TRUE)
}
\arguments{
\item{pattern}{an unquoted expression, describes what will be done to the naked call}

\item{args}{a pairlist (use \code{base::alist()} or \code{rlang::exprs()}) which will
be used as the formals of the built \code{tag}}

\item{eval_args}{if \code{TRUE} (default), the tag arguments are evaluated so
they can be used directly in the tag definition, set to \code{FALSE} to use
NSE on tag arguments}
}
\description{
\code{tag}s are essentially adverb factories, they build adverbs (also called
function operators) from a set of parameters. \code{tag_adverb}s are adverbs,
unlike \code{tag}s they take a function as a first parameter.
Both benefit from a special syntax using the \code{$} operator.
}
\section{Formalism}{

\code{tag}s are designed to be used intuitively and are best understood by
examples such as the ones proposed below.

The output of a \code{tag} call is a \code{tag_adverb}. The output of a \code{tag_adverb}
call is called a manufactured function. The first argument of a \code{tag_adverb}
is called the input function.
}

\section{Using patterns}{


The \code{pattern} argument is a representation of the body of the manufactured
function. It allows for short definitions of \code{tag}s thanks
to a set of objects / function calls such as \code{f} and \code{CALL()} that one can
use in the definitions. see \code{?CALL}
}

