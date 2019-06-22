#library(tags)

expect_no_error <- purrr::partial(expect_error, regexp = NA)


test_that("logging tag works",{
  logging <- tag(args = alist(.time = TRUE, .print = TRUE),{
    message(deparse(sys.call()))
    if(.time) {
      time_sec <- system.time(res <- CALL(eval = TRUE))[3]
      cat("  ~", time_sec, "sec\n")
    } else {
      res <- CALL(eval = TRUE)
    }
    if(.print) print(res)
    invisible(res)
  })
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

test_that("wrapping tag works",{
wrapping <- tag(
  args= alist(.before=NULL, .after=NULL, .around = identity),
  eval_args = FALSE,{
    t_args <- T_ARGS(eval = FALSE)
    eval(t_args[[".before"]])
    . <- eval(expr(rlang::as_function(!!t_args[[".around"]])(!!CALL(eval = FALSE))))
    eval(t_args[[".after"]])
    .
  })
expect_no_error(wrapping(print("hi!"))$print('hello'))
})
