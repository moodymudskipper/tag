library(tags)

expect_no_error <- purrr::partial(expect_error, regexp = NA)

test_that("logging tag works",{
  capture.output({
    expect_equal(logging$identity("foo"),"foo")
    expect_message(logging$identity("foo"), 'logging$identity("foo")', fixed= TRUE)
    fun <- function(t, f, y) t$f(y)
    expect_error(fun(logging, identity,"foo"), NA)
  })
})

# test_that("trying tag works",{
#   expect_equal(trying$paste("hello","world", .otherwise = "hi"),
#                "hello world")
#   expect_equal(trying$paste("hello", world, .otherwise = "hi", .quiet = TRUE),
#                "hi")
#   # dealing with environments
#   fun1 <- function(t1, e, s, f, ...) t1(e,s)$f(...)
#   expect_equal(fun1(trying,  "hi", TRUE , paste, "hello",world), "hi")
#   fun2 <- function(t1, e, s, f, ...) t1(e)(f,s)(...)
#   expect_equal(fun2(trying,  "hi", TRUE , paste, "hello",world), "hi")
#   fun3 <- function(t1, e, s, f, ...) t1(.quiet = s)(f,e)(...)
#   expect_equal(fun3(trying,  "hi", TRUE , paste, "hello",world), "hi")
#   fun4 <- function(t1, e, s, f, ...) t1()(f,e,s)(...)
#   expect_equal(fun4(trying,  "hi", TRUE , paste, "hello",world), "hi")
#   fun5 <- function(t1, e, s, f, ...) t1$f(..., .otherwise = e, .quiet = s)
#   expect_equal(fun5(trying,  "hi", TRUE , paste, "hello",world), "hi")
#   fun6 <- function(t1, e, s, f, ...) t1(.quiet=s)$f(..., .otherwise = e)
#   expect_equal(fun6(trying,  "hi", TRUE , paste, "hello",world), "hi")
# })
