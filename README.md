
<!-- README.md is generated from README.Rmd. Please edit that file -->
Introduction
------------

*tags* are essentially adverb factories, they build adverbs from a set of parameters.

Installation :

``` r
# devtools::install_github("moodymudskipper/tag")
library(tag)
```

See also the package `tags` which includes many tags defined using this package: <https://github.com/moodymudskipper/tags>.

Formalism
---------

*function factories* are functions that makes functions.

*function operators*, often called *adverbs*, are functions that modify a function (they're a subset of function factories).

*tags* are *function operator factories*, they build a special type of adverb of class `"tag_adverb"`

Both *tags* and *tag\_adverbs* benefit from a special syntax using the `$` operator.

They are best understood by examples.

We consider the tag `using_possibly` from the package `tags`, which provides an alternative to `base::tryCatch` (that it wraps) and contains code from `purrr::possibly`.

In the example below, we try to execute the call `log("a")`, this operation normally triggers an error as `"a"` is not numeric, we can change this behavior to return `NA` in case of failure.

``` r
library(tags)
using_possibly(.otherwise = NA, .quiet = TRUE)(log)("a")  
#> [1] NA
class(using_possibly)
#> [1] "tag"      "function"
class(using_possibly(.otherwise = NA, .quiet = TRUE))
#> [1] "tag_adverb"             "purrr_function_partial"
#> [3] "function"
class(using_possibly(.otherwise = NA, .quiet = TRUE)(log))# explicit form
#> [1] "function"
```

The output of a `tag` call is a `tag_adverb`. The output of a `tag_adverb` call is called a *manufactured function* (and doesn't have a special class).

The first argument of a `tag_adverb`, `log` in our example, is called the *input function*.

Syntax
------

`$.tag_adverb` provide a more readable option for tag calls, the following are equivalent :

``` r
identical(
  using_possibly(.otherwise = NA, .quiet = TRUE)$log("a"),
  using_possibly(.otherwise = NA, .quiet = TRUE)(log)("a")
)
#> [1] TRUE
```

One main advantage of tags compared to standard adverbs is that the input function is not burried in the call.

The other big advantage is flexibility.

When a parameter is not provided to the `tag` it is featured as a parameter of its `tag_adverb`, so we can write :

``` r
using_possibly(.otherwise = NA)(log, .quiet = TRUE)("a")
#> [1] NA
```

Much more interesting: when a parameter is not provided to the `tag_adverb`, it is featured as a parameter of the manufactured function, so we can write :

``` r
using_possibly(.otherwise = NA)$log("a", .quiet = TRUE)
#> [1] NA
```

Any combination is possible :

``` r
using_possibly()$log("a", .quiet = TRUE, .otherwise = NA)
#> [1] NA
```

`$.tag` provides a shortcut so instead of the above we can write :

``` r
using_possibly$log("a", .quiet = TRUE, .otherwise = NA)
#> [1] NA
```

In that last case the user can also enjoy RStudio's autocomplete feature.

This forwarding of unused arguments mixed with the magic built into the methods `$.tag` and `$.adverb` allows a lot of flexibility, and interesting intermediate objects can be built to be used as shorthands or in functionals.

Building a tag
--------------

The package `tag` contains two ways to build a tag, either by converting an existing adverb, or by building it from scratch.

We could have created a tag very similar to `tags::using_quietly`, *tag* counterpart or `purrr::quietly`, as :

``` r
using_quietly0 <- as_tag(purrr::quietly)
using_quietly0$sqrt(-1)
#> $result
#> [1] NaN
#> 
#> $output
#> [1] ""
#> 
#> $warnings
#> [1] "NaNs produced"
#> 
#> $messages
#> character(0)
```

Using `as_tag` can be convenient but is not as reliable as building one from scratch, moreover we recommend dotted arguments to avoid conflicts between tag arguments and arguments of the input function.

To build a tag from scratch we use the `tag` function.

The `pattern` argument defines what will be returned by the manufactured function. It allows for short definitions of *tags* thanks to a set of functions that we call *pattern helpers*

-   `f()`: input function
-   `CALL()`: return evaluated or unevaluated call
-   `F_ARGS()`: return evaluated or unevaluated arguments of input function
-   `T_ARGS()`: return evaluated or unevaluated arguments of tag
-   `F_FORMALS()`: return the input function's formals
-   `T_FORMALS()`: return he tag's formals

`CALL()`, `F_ARGS()` and `T_ARGS()` have a boolean `eval` argument to either trigger evaluation in the parent environment or return a language object.

`CALL()` and `F_ARGS()` have a `type` argument with possible values `"expanded"` (uses `match.call()`, names arguments explicitly), `"unexpanded"` (uses `match.call(expand.dots=FALSE)`), and `"raw"` (uses `sys.call()`, which doesn't names arguments that were'nt named in the call).

### examples

This is the identity tag, it doesn't do anything :

``` r
not_doing_anything <- tag(CALL(eval = TRUE))
not_doing_anything$mean(c(1,2,NA,3), na.rm = TRUE)
#> [1] 2
```

This second boring tag returns always 1 :

``` r
returning_1 <- tag(1)
returning_1$mean(c(1,2,NA,3), na.rm = TRUE)
#> [1] 1
```

This tag prints a message and the arguments of the input function:

``` r
printing_args <- tag(args=alist(msg="arguments:"),{
  message(msg)
  print(F_ARGS(eval=TRUE))
  CALL(eval = TRUE)
})
printing_args("ARGS:")$mean(c(1,2,NA,3), na.rm = TRUE)
#> ARGS:
#> $x
#> [1]  1  2 NA  3
#> 
#> $na.rm
#> [1] TRUE
#> [1] 2
```

This tags gives informations about the call using all our pattern helpers: :

``` r
showcasing <- tag(args = alist(tag_arg="foo"), {
  calls <- dplyr::lst(
    CALL(eval = FALSE, type = "raw"),
    CALL(eval = FALSE, type = "expanded"),
    CALL(eval = FALSE, type = "unexpanded"),
    CALL(eval = TRUE, type = "raw"),
    CALL(eval = TRUE, type = "expanded"),
    CALL(eval = TRUE, type = "unexpanded"),
    F_ARGS(eval = FALSE, type = "raw"),
    F_ARGS(eval = FALSE, type = "expanded"),
    F_ARGS(eval = FALSE, type = "unexpanded"),
    F_ARGS(eval = TRUE, type = "raw"),
    F_ARGS(eval = TRUE, type = "expanded"),
    F_ARGS(eval = TRUE, type = "unexpanded"),
    T_ARGS(eval = FALSE),
    T_ARGS(eval = TRUE),
    F_FORMALS(),
    T_FORMALS())
  purrr::iwalk(calls,~{
    message(.y)
    print(.x)
  })
  CALL(eval = TRUE)
 })

x <- 2
txt <- "bar"
showcasing(txt)$mean(c(1,x,NA,3), na.rm = TRUE)
#> CALL(eval = FALSE, type = "raw")
#> (function (x, ...) 
#> UseMethod("mean"))(c(1, x, NA, 3), na.rm = TRUE)
#> CALL(eval = FALSE, type = "expanded")
#> (function (x, ...) 
#> UseMethod("mean"))(x = c(1, x, NA, 3), na.rm = TRUE)
#> CALL(eval = FALSE, type = "unexpanded")
#> (function (x, ...) 
#> UseMethod("mean"))(x = c(1, x, NA, 3), ... = pairlist(na.rm = TRUE))
#> CALL(eval = TRUE, type = "raw")
#> [1] 2
#> CALL(eval = TRUE, type = "expanded")
#> [1] 2
#> CALL(eval = TRUE, type = "unexpanded")
#> [1] NA
#> F_ARGS(eval = FALSE, type = "raw")
#> [[1]]
#> c(1, x, NA, 3)
#> 
#> $na.rm
#> [1] TRUE
#> F_ARGS(eval = FALSE, type = "expanded")
#> $x
#> c(1, x, NA, 3)
#> 
#> $na.rm
#> [1] TRUE
#> F_ARGS(eval = FALSE, type = "unexpanded")
#> $x
#> c(1, x, NA, 3)
#> 
#> $...
#> $...$na.rm
#> [1] TRUE
#> F_ARGS(eval = TRUE, type = "raw")
#> [[1]]
#> [1]  1  2 NA  3
#> 
#> $na.rm
#> [1] TRUE
#> F_ARGS(eval = TRUE, type = "expanded")
#> $x
#> [1]  1  2 NA  3
#> 
#> $na.rm
#> [1] TRUE
#> F_ARGS(eval = TRUE, type = "unexpanded")
#> $x
#> [1]  1  2 NA  3
#> 
#> $...
#> $...$na.rm
#> [1] TRUE
#> T_ARGS(eval = FALSE)
#> $tag_arg
#> txt
#> T_ARGS(eval = TRUE)
#> $tag_arg
#> [1] "bar"
#> F_FORMALS()
#> $x
#> 
#> 
#> $...
#> T_FORMALS()
#> $tag_arg
#> [1] "foo"
#> [1] 2
```

printing tags
-------------

When printing a tag, its definition is displayed as well as the actual code, which is the same for all tags defined with `tag::tag`, the difference being in the objects contained in their enclosing environment.

``` r
printing_args
#> # a tag
#> 
#> definition:
#> tag(pattern = {
#>     message(msg)
#>     print(F_ARGS(eval = TRUE))
#>     CALL(eval = TRUE)
#> }, args = list(msg = "arguments:"))
#> 
#> Code:
#> function (msg = "arguments:") 
#> {
#>     partial_args <- as.list(match.call())[-1]
#>     tag_adv <- purrr::partial(f, !!!partial_args)
#>     attr(tag_adv, "definition") <- expr(purrr::partial(!!f, !!!partial_args))
#>     add_class(tag_adv, "tag_adverb")
#> }
#> <environment: 0x000000001e079920>
```

The package `tags` contains many tags that can give inspiration on how to build new ones

``` r
tags::debugging
#> # a tag
#> 
#> definition:
#> tag(pattern = {
#>     debugonce(f)
#>     CALL(eval = TRUE)
#> }, args = list(), rm_args = character(0))
#> 
#> Code:
#> function () 
#> {
#>     partial_args <- as.list(match.call())[-1]
#>     tag_adv <- purrr::partial(f, !!!partial_args)
#>     attr(tag_adv, "definition") <- expr(purrr::partial(!!f, !!!partial_args))
#>     add_class(tag_adv, "tag_adverb")
#> }
#> <bytecode: 0x000000001ad1eac0>
#> <environment: 0x000000001adf5aa0>
```
