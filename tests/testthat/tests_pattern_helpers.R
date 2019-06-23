context("pattern_helpers")

test_that("pattern helpers return the same with all inputs", {
  test <- tag(args = alist(a=1,b=2),{
    dplyr::lst(
      F_FORMALS(),
      F_ARGS(),
      T_FORMALS(),
      T_ARGS())
  })

  expect_identical(
    test(a = 3, b = 4)$mean(1:3),
    test(a = 3)$mean(1:3, b = 4))
  expect_identical(
    test(a = 3, b = 4)$mean(1:3),
    test$mean(1:3, a = 3, b = 4))
  expect_identical(
    test(a = 3, b = 4)$mean(1:3),
    test()(mean, a = 3, b = 4)(1:3))
})

