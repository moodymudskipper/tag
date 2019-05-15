
<!-- README.md is generated from README.Rmd. Please edit that file -->
Introduction
------------

`tag`s are essentially adverb factories, they build adverbs (also called function operators) from a set of parameters. `tag_adverb`s are adverbs, unlike `tag`s they take a function as a first parameter. Both benefit from a special syntax using the `$` operator.

Installation :

``` r
# devtools::install_github("moodymudskipper/tag")
library(tag)
```

See also the package `tags` which includes many tags defined with the help of this package: <https://github.com/moodymudskipper/tags>.

Note: It is still largely in progress and potentially subject to major breaking changes.

Formalism
---------

`tag`s are designed to be used intuitively and are best understood by examples such as the ones proposed below.

The output of a `tag` call is a `tag_adverb`. The output of a `tag_adverb` call is called a manufactured function. The first argument of a `tag_adverb` is called the input function.

The `args` argument becomes either the formals of a `tag` object or the additional arguments of a `tag_adverb` object (the first being `f`). We may refer to `args` as the *new formals*.

Syntax
------

The `$` syntax offers an intuitive and flexible alternative to the traditional adverb syntax. `some_tag(tag_arg)$fun(fun_arg)` can be used instead of `some_tag(tag_arg)(fun)(fun_arg)`.

When a parameter is not provided to the `tag` it is featured as a parameter of its `tag_adverb` output so `some_tag(tag_arg)$fun(fun_arg)` can be written `some_tag()(fun, tag_arg)(fun_arg)`.

When a parameter is not provided to the `tag_adverb`, it is featured as a parameter of the manufactured function, so the latter call can be spelt `some_tag()(fun)(fun_arg, tag_arg)`, which we'll often simply write `some_tag$fun(fun_arg, tag_arg)`.

This forwarding of unused arguments mixed with the magic built into the methods `$.tag` and `$.adverb` allows a lot of flexibility, and interesting intermediate objects can be built to be used as shorthands or in functionals.

We build below a copy of the `viewing` tag from the package `tags` and showcase

``` r
 # viewing2 tag to view the output of a call
 viewing2 <- tag(args = alist(.title = "View"),{
   res <- eval(CALL)
   View(res,title = .title)
   res
 })
class(viewing2)
#> [1] "tag"      "function"
class(viewing2("A"))
#> [1] "tag_adverb"             "purrr_function_partial"
#> [3] "function"
class(viewing2("A")(head))
#> [1] "function"
```

``` r
 viewing2("A")(head)(cars, 2)             # "A" is fed to the tag
 viewing2("B")$head(cars, 2)              # equivalent, arguably more readable
 viewing2()(head, "C")(cars, 2)           # "C" is fed to the tag_adverb
 viewing2()(head)(cars, 2, .title = "D")  # "D" is fed to the manufactured function
 viewing2()$head(cars, 2, .title = "E")   # "E" equivalent, a bit more readable
 viewing2$head(cars, 2, .title = "F")     # equivalent, arguably more readable
```

Using patterns
--------------

The `pattern` argument is a representation of the body of the manufactured function. It allows for short definitions of `tag`s and `tag_adverb`s thanks to a set of objects that one can use in the definitions.

Available objects are :

-   `f`: input function
-   `SCALL`: unevaluated expression containing the naked input function call, i.e. the way the function would have been called without the tag or its arguments
-   `CALL`: Same as `SCALL` but the arguments that were not named in the call are named (unless they're part of the `...`)
-   `CALL_UNEXP`: Same as `CALL` but the `...` are *NOT* expanded
-   `ARGS`: List of the unevaluated arguments as given in the call, identical to `CALL`
-   `FORMALS`: List of the unevaluated arguments as given in the call
-   `NEW_FORMALS`: The formals of the `tag`, or additional formals of the `tag_adverb`

`f` and `CALL` suffice for most tag definitions.

This is the neutral tag, it doesn't do anything :

``` r
identity_tag <- tag({eval(CALL)})
identity_tag$mean(c(1,2,NA,3), na.rm = TRUE)
#> [1] 2
```

This tags gives informations about the call using all our shortcuts :

``` r
verbosing <- tag(args = alist(descr="Here's a breakdown of the call"), {
  message(descr)
  cat("----------------------------------------------\n")
  message("`f`: input function\n")
  print(f)
  cat("----------------------------------------------\n")
  message("`SCALL`: unevaluated expression containing the naked input function call\n")
  print(SCALL)
  cat("----------------------------------------------\n")
  message("`CALL`: Same as `SCALL` but the arguments that were not named in the call are named\n")
  print(CALL)
  cat("----------------------------------------------\n")
  message("`CALL_UNEXP`: Same as `CALL` but the `...` are *NOT* expanded\n")
  print(CALL_UNEXP)
  cat("----------------------------------------------\n")
  message("`ARGS`: List of the unevaluated arguments as given in the call, identical to `CALL`\n")
  print(ARGS)
  cat("----------------------------------------------\n")
  message("`FORMALS`: List of the unevaluated arguments as given in the call\n")
  print(FORMALS)
  cat("----------------------------------------------\n")
  message("`NEW_FORMALS`: The formals of the `tag`, or additional formals of the `tag_adverb`\n")
  print(NEW_FORMALS)
  cat("----------------------------------------------\n")
  message("`Now evaluating call:\n")
  eval(CALL)
 })

verbosing("See details below:")$mean(c(1,2,NA,3), na.rm = TRUE)
#> See details below:
#> ----------------------------------------------
#> `f`: input function
#> function (x, ...) 
#> UseMethod("mean")
#> <bytecode: 0x0000000019e1e290>
#> <environment: namespace:base>
#> ----------------------------------------------
#> `SCALL`: unevaluated expression containing the naked input function call
#> (function (x, ...) 
#> UseMethod("mean"))(c(1, 2, NA, 3), na.rm = TRUE)
#> ----------------------------------------------
#> `CALL`: Same as `SCALL` but the arguments that were not named in the call are named
#> (function (x, ...) 
#> UseMethod("mean"))(x = c(1, 2, NA, 3), na.rm = TRUE)
#> ----------------------------------------------
#> `CALL_UNEXP`: Same as `CALL` but the `...` are *NOT* expanded
#> (function (x, ...) 
#> UseMethod("mean"))(x = c(1, 2, NA, 3), ... = pairlist(na.rm = TRUE))
#> ----------------------------------------------
#> `ARGS`: List of the unevaluated arguments as given in the call, identical to `CALL`
#> $x
#> c(1, 2, NA, 3)
#> 
#> $na.rm
#> [1] TRUE
#> 
#> ----------------------------------------------
#> `FORMALS`: List of the unevaluated arguments as given in the call
#> $x
#> 
#> 
#> $...
#> 
#> 
#> ----------------------------------------------
#> `NEW_FORMALS`: The formals of the `tag`, or additional formals of the `tag_adverb`
#> $descr
#> [1] "Here's a breakdown of the call"
#> 
#> ----------------------------------------------
#> `Now evaluating call:
#> [1] 2
```

More examples with code
-----------------------

Thes ecome directly from the package tags.

#### `strictly` tag to adjust strictness of a call

``` r
strictly <- tag(args = alist(warn=2),{
  w <- options("warn")[[1]]
  on.exit(options(warn=w))
  options(warn= warn)
  eval(CALL)
})
strictly(-1)$sqrt(-1)
#> [1] NaN
```

#### `dbg` tag to debug a call

``` r
dbg <- tag({
  debugonce(f)
  exec("f", !!!purrr::map(ARGS, eval))
})
dbg$sample(5)
```

#### `logging` tag to log call and time it took

``` r
logging <- tag(args = alist(.time = TRUE, .print = TRUE),{
  message(deparse(match.call()))
  cat("  ~ ...")
  if(.time) cat("\b\b\b", system.time(res <- eval(CALL))[3], "sec\n") else
    res <- eval(CALL)
  if(.print) print(res)
  invisible(res)
})
logging$head(cars,2)
#> logging$head(x = cars, 2)
#>   ~ ... 0 sec
#>   speed dist
#> 1     4    2
#> 2     4   10
```

#### `trying` tag to try a call or if failure call alternate call

``` r
trying <- tag(
  args = alist(.else=, .silent = FALSE),{
    res <- try(eval(CALL), silent = .silent)
    if(inherits(res, "try-error")){
      res <- .else
      if(rlang::inherits_any(res, c("function","formula")))
        res <- rlang::as_function(res)(eval(ARGS[[1]]))
    }
    res
  })
trying$paste("hello","world", .else = "hi")
#> [1] "hello world"
trying$paste("hello", world, .else = "hi")
#> Error in (function (..., sep = " ", collapse = NULL)  : 
#>   object 'world' not found
#> [1] "hi"
trying$paste("hello", world, .else = "hi", .silent = TRUE)
#> [1] "hi"
```

#### `preserving_attr` tag to preserve attributes

``` r
preserving_attr <- tag(args = alist(
  .arg = 1, incl_row.names = FALSE, incl_class = FALSE,
  incl_names = FALSE, incl_dim = FALSE), {
    #browser()
    eval(expr(attr_saved <- attributes(!!(ARGS[[.arg]]))))
    attr_saved[names(attr_saved) %in% c(
      "row.names"[!incl_row.names],
      "class"[!incl_class],
      "dim"[!incl_dim])] <- NULL
    res <- eval(CALL)
    attributes(res)[names(attr_saved)] <- attr_saved
    res
  })
preserving_attr$map_dfr(head(iris,2),identity)
#> # A tibble: 2 x 5
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> *        <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#> 1          5.1         3.5          1.4         0.2 setosa 
#> 2          4.9         3            1.4         0.2 setosa
preserving_attr(incl_class = TRUE)$map_dfr(head(iris,2),identity)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
```
