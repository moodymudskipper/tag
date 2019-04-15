
<!-- README.md is generated from README.Rmd. Please edit that file -->
tags
====

tags are single argument functions which are always named with the prefix `..` and can be called using `$` so that `..tag$obj` is equivalent to `..tag(obj)`.

They're often used to alter a function (in this case they are adverbs), but can be used on other object types.

For now it's more a collection of experiment rather than a cohesive package.

See bottom for some reflexions

used as an adverb
-----------------

`..bang` will make any function compatible with quasiquotation

``` r
library(rlang)
#> Warning: package 'rlang' was built under R version 3.5.3
u <- "speed"
v <- quote(dist)
w <- quo(time)
x <- list(a=c(1, 2), b=c(3, 4))
..bang$transform(head(cars,2), !!w := !!v / !!sym(u), !!!x)
#>   speed dist time a b
#> 1     4    2  0.5 1 3
#> 2     4   10  2.5 2 4
```

------------------------------------------------------------------------

`..grp` uses `dplyr::group_by` for a single operation, it adds a `.by` argument to the function

``` r
library(dplyr, warn.conflicts = FALSE)
#> Warning: package 'dplyr' was built under R version 3.5.3
..grp$summarize_all(iris,mean, .by="Species")
#> # A tibble: 3 x 5
#>   Species    Sepal.Length Sepal.Width Petal.Length Petal.Width
#>   <fct>             <dbl>       <dbl>        <dbl>       <dbl>
#> 1 setosa             5.01        3.43         1.46       0.246
#> 2 versicolor         5.94        2.77         4.26       1.33 
#> 3 virginica          6.59        2.97         5.55       2.03
```

------------------------------------------------------------------------

`..lbd` detects formula arguments allows self referencing in functions such as `transform`, `dplyr::mutate` and `dplyr::summarize`

``` r
..lbd$mutate(head(iris,2), Petal.Width = ~1000*(.), Species = ~toupper(.))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         200  SETOSA
#> 2          4.9         3.0          1.4         200  SETOSA
```

------------------------------------------------------------------------

`..at` makes `dplyr::mutate` and `dplyr::summarize` compatible with the use of `at` to enjoy features of `mutate_at` and `summarize_at` in the same call. Through the `.key` argument `summarize` also supports multi spread in an intuitive way. `at(...)` is actually a shortcut for `!!!.at(FIRST_ARG, ...)` (where `FIRST_ARG` will just be `.` in a pipe chain), which can be called without the tag `..at`

``` r
..at$summarize(iris, at(vars(starts_with("Sepal")),lst(mean,median)))
#>   Sepal.Length_mean Sepal.Width_mean Sepal.Length_median
#> 1          5.843333         3.057333                 5.8
#>   Sepal.Width_median
#> 1                  3
iris %>% summarize(!!!.at(.,vars(starts_with("Sepal")),lst(mean,median)))
#>   Sepal.Length_mean Sepal.Width_mean Sepal.Length_median
#> 1          5.843333         3.057333                 5.8
#>   Sepal.Width_median
#> 1                  3
mtcars %>%
  group_by(gear) %>%
  ..at$summarize(at(c("disp","hp"), mean, .keys = vars(cyl)))
#> # A tibble: 3 x 7
#>    gear disp_cyl_6_mean disp_cyl_4_mean disp_cyl_8_mean hp_cyl_6_mean
#>   <dbl>           <dbl>           <dbl>           <dbl>         <dbl>
#> 1     3            242.            120.            358.          108.
#> 2     4            164.            103.            NaN           116.
#> 3     5            145             108.            326           175 
#> # ... with 2 more variables: hp_cyl_4_mean <dbl>, hp_cyl_8_mean <dbl>
```

------------------------------------------------------------------------

`..ip` modifies a function so it assigns in place to its first argument :

``` r
x <- "a"
..ip$toupper(x)
x
#> [1] "A"
```

------------------------------------------------------------------------

`..View` will open the viewer on the output, handy to debug, or in pipe chains, see also `%View>%` in my package *pipes*

``` r
..View$head(iris)
```

------------------------------------------------------------------------

`..nowarn` will suppress the warnings, a parameter `warn = -1` can be changed along the rules from `?options` . See also `%nowarn>%` in my package *pipes*.

``` r
..nowarn$sqrt(-1)
#> [1] NaN
```

------------------------------------------------------------------------

`..strict` will turn any warning into an error, a parameter `warn = -1` can be changed along the rules from `?options` . See also `%strict>%` in my package *pipes*.

``` r
..strict$sqrt(-1)
#> Error in sqrt(x = -1): (converted from warning) NaNs produced
```

------------------------------------------------------------------------

`..try` will add an argument `.else` which will be evaluated if normal call fails.

``` r
..try$paste("hello","world", .else = "hi")
#> [1] "hello world"
..try$paste("hello", world, .else = "hi")
#> Error in paste("hello", world) : object 'world' not found
#> [1] "hi"
..try$paste("hello", world, .else = "hi", .silent = TRUE)
#> [1] "hi"
```

------------------------------------------------------------------------

`..debug` runs `debugonce` on the tagged function

``` r
..debug$ave(1:4, rep(1:2,each =2))
#> debugging in: ave(x = 1:4, rep(1:2, each = 2))
#> debug: {
#>     if (missing(...)) 
#>         x[] <- FUN(x)
#>     else {
#>         g <- interaction(...)
#>         split(x, g) <- lapply(split(x, g), FUN)
#>     }
#>     x
#> }
#> debug: if (missing(...)) x[] <- FUN(x) else {
#>     g <- interaction(...)
#>     split(x, g) <- lapply(split(x, g), FUN)
#> }
#> debug: g <- interaction(...)
#> debug: split(x, g) <- lapply(split(x, g), FUN)
#> debug: x
#> exiting from: ave(x = 1:4, rep(1:2, each = 2))
#> [1] 1.5 1.5 3.5 3.5
```

special tags
------------

`..w` (like "with") is a tag that can be used for subsetting with `[`, allowing self referencing with `.`, named elements can be accessed by their names like in `with`, except that `with` doesn't support atomic vectors. `[[` doesn't subset but returns the value of the content of the brackets

``` r
..w$iris[nrow(.),]
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
#> 150          5.9           3          5.1         1.8 virginica
vec1 <- c(a=1,b=2,c=3,d=4)
..w$vec1[.>b]
#> c d 
#> 3 4
vec2 <- 1:20
..w$vec2[.>10]
#>  [1] 11 12 13 14 15 16 17 18 19 20
..w$iris[[nrow(.)]]
#> [1] 150
vec1 <- c(a=1,b=2,c=3,d=4)
..w$vec1[[b+c]]
#> [1] 5
vec2 <- 1:20
..w$vec2[[length(.)/2]]
#> [1] 10
```

`..fn` changes arguments named `FUN` from formula to function using `purrr::as_mapper`, (in base R all function arguments are named `FUN`). It is named as a tribute to Gabor Grothendieck's `gsubfn::fn`, which works in a similar way.

``` r
..fn$sapply(iris, ~class(.))
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
#>    "numeric"    "numeric"    "numeric"    "numeric"     "factor"
```

`..p` initiates a dollar pipe chain

``` r
..p$iris$head$Filter(is.numeric,.)$dim()
#> [1] 6 4
```

`..fs` initiates a dollar pipe unabled functional sequence (it's technically an adverb but too weird to be included above)

``` r
fun <- ..fs$head(2)$gsub("h","X",.)
fun(c("hello","hi","foo"))
#> [1] "Xello" "Xi"
```

`..dt` allows using *data.table* syntax and performance for a single operation without cumbersome conversions

``` r
iris2 <- head(iris,2)
..dt$iris2[,Species2:=toupper(Species)]
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species Species2
#> 1          5.1         3.5          1.4         0.2  setosa   SETOSA
#> 2          4.9         3.0          1.4         0.2  setosa   SETOSA
```

experiments
-----------

Those tags are not extremely useful, but were implemented when playing around with the concept of tag, and might serve as inspiration.

`..incr` is used to increment values

``` r
x <- 3
+..incr$x
#> [1] 4
..incr$x + 5
#> [1] 9
-..incr$x
#> [1] 8
..incr$x - 2
#> [1] 6
```

`..chr` allows arithmetic operations on strings, `+` and `-` paste in different order, `*` duplicates the strings and `/` subsets with a regex

``` r
x <- "hello"
..chr$x + "world"
#> [1] "helloworld"
..chr$x + "world" - "oh"
#> [1] "ohhelloworld"
..chr$x + "world" - "oh" + toupper + unclass
#> [1] "OHHELLOWORLD"
..chr$x * 3
#> [1] "hellohellohello"
x <- c("hello","sup", "hey","mornin")
..chr$x / "^h"
#> [1] "hello" "hey"
```

creating an adverb tag
======================

To create a custom adverb tag we just need to design an adverb and give it the appropriate class. The class `"tag"` is used for the `$.tag` method, the class `"adverb"` is used for composition.

``` r
..neg <- Negate
class(..neg) <- c("tag","adverb","function")
..neg$is.numeric("a")
#> [1] TRUE
..up <- function(f) function(...) toupper(f(...))
class(..up) <- c("tag","adverb","function")
..up$substr("hello world",1,5)
#> [1] "HELLO"
```

reflexions
==========

All exported functions from the package *tags* use the `$.tag` method.

Tag functions could have been just `NA` objects with a method for `$`, this is the approach taken by Gabor Grothendieck for `gsubfn::fn`. I think however that it's clearer to think of them as functions with a `$` shorthand thanks to their class `"tag"`.

------------------------------------------------------------------------

Usually adverbs don't add argument to a function, instead arguments are passed to the adverb (so they can be documented traditionally) and arguments are left untouched or some are removed (e.g. `purrr::partial`). i.e. We find a lot of `adv(fun, adv_arg)(fun_arg)` but never `adv(fun)(fun_arg, new_arg)` as is done in this package. The latter is less easy to document, but easier to read and makes tags possible.

------------------------------------------------------------------------

Adverb tags are the most likely to be expanded, maybe other tags should have a different prefix (or none ?), so we can have more consistency.

Traditional adverbs suffer from :

-   naming conventions (verb? adjective? adverb?): `purrr::compose`, `purrr::partial`, `purrr::safely`, `Negate`
-   naming inconsistencies: `rate_delay` is an adverb, `rate_backoff` isn't
-   brackets and parameters are awkward for inline use as parameters end up between the adverb and the input function
-   the adverb is highlighted by the editor, the function is not

``` r
library(purrr)
#> Warning: package 'purrr' was built under R version 3.5.3
#> 
#> Attaching package: 'purrr'
#> The following objects are masked from 'package:rlang':
#> 
#>     %@%, as_function, flatten, flatten_chr, flatten_dbl,
#>     flatten_int, flatten_lgl, flatten_raw, invoke, list_along,
#>     modify, prepend, splice
safely(log, otherwise = NA_real_)(10)
#> $result
#> [1] 2.302585
#> 
#> $error
#> NULL
safely(log, otherwise = NA_real_)("a")
#> $result
#> [1] NA
#> 
#> $error
#> <simpleError in .Primitive("log")(x, base): non-numeric argument to mathematical function>
safely(log, otherwise = NA_real_)("a")$result
#> [1] NA
# not the same thing but as a comparison
..try$log(10, .else = NA_real_)
#> [1] 2.302585
..try$log("a", .else = NA_real_)
#> Error in log(x = "a") : non-numeric argument to mathematical function
#> [1] NA
..try$log("a", .else = NA_real_, .silent = TRUE)
#> [1] NA
```

This package offers an opportunity for consistency, though it's spoiled by the presence of non adverb tags, which might be better off with a different prefix (or none) so all tags would be adverbs. Non adverb tags are `..dt`, `..w`, `..pp` and `..fs` (the latter being technically an adverb).

Adverbs might be underused because their use is not intuitive, maybe tags can help in this regard.

------------------------------------------------------------------------

Thanks to the fact they're functions, we can compose them when they are adverbs (another reason to dissociate non adverbs):

``` r
# same output:
..View(..grp$summarize_all)(iris, mean, .by = "Species")
#> # A tibble: 3 x 5
#>   Species    Sepal.Length Sepal.Width Petal.Length Petal.Width
#>   <fct>             <dbl>       <dbl>        <dbl>       <dbl>
#> 1 setosa             5.01        3.43         1.46       0.246
#> 2 versicolor         5.94        2.77         4.26       1.33 
#> 3 virginica          6.59        2.97         5.55       2.03
..grp(..View$summarize_all)(iris, mean, .by = "Species")
#> # A tibble: 3 x 5
#>   Species    Sepal.Length Sepal.Width Petal.Length Petal.Width
#>   <fct>             <dbl>       <dbl>        <dbl>       <dbl>
#> 1 setosa             5.01        3.43         1.46       0.246
#> 2 versicolor         5.94        2.77         4.26       1.33 
#> 3 virginica          6.59        2.97         5.55       2.03
```

Though chaining with `..View$..grp$(...)` is possible to implement.

------------------------------------------------------------------------

Maybe names starting with ".", are an issue, I haven't thought it through, maybe something like `T.tag$obj` but it doesn't looks as good, and less like a tag.

------------------------------------------------------------------------

Short names have been preferred to put focus on input function, not sure if it's smart if the family grows

------------------------------------------------------------------------

Through some miracle that I don't understand, RStudio's completion suggestions works on tagged functions (including added parameters).
