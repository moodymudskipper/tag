context("definitions")
library(rlang)
library(purrr)


test_that("an adverb can be defined and work with all syntaxes", {
  adv1 <- tag_adverb(sprintf("%s: %s",descr, round(CALL(eval=TRUE),2)),alist(descr="Res"))
  expect_identical("Res: 2.3", adv1$log(10))
  expect_identical("Result: 2.3", adv1$log(10, descr = "Result"))
  expect_identical("Result: 2.3", adv1(log)(10, descr = "Result"))
  expect_identical("Result: 2.3", adv1(log, descr = "Result")(10))
})

test_that("Arguments cannot be on both sides", {
  adv1 <- tag_adverb(sprintf("%s: %s",descr, round(CALL(eval=TRUE),2)),alist(descr="Res"))
  expect_error(adv1(log, descr = "Result")(10, descr = "Result"))
})


test_that("a tag can be defined and work with all relevant syntaxes", {
  tag1 <- tag(sprintf("%s %s: %s",descr, letter, round(CALL(eval=TRUE),2)),
              alist(descr="Res",letter = "A"))
  expect_identical("Res A: 2.3", tag1$log(10))
  expect_identical("Result B: 2.3", tag1$log(10, descr = "Result", letter = "B"))
  expect_identical("Result B: 2.3", tag1()$log(10, descr = "Result", letter = "B"))
  expect_identical("Result B: 2.3", tag1()(log)(10, descr = "Result", letter = "B"))
  expect_identical("Result B: 2.3", tag1(descr = "Result")$log(10, letter = "B"))
  expect_identical("Result B: 2.3", tag1(descr = "Result")(log)(10, letter = "B"))
  expect_identical("Result B: 2.3", tag1(descr = "Result", letter = "B")$log(10))
  expect_identical("Result B: 2.3", tag1(descr = "Result", letter = "B")(log)(10))
})





test_that("Arguments cannot be on both sides", {
  tag1 <- tag(sprintf("%s %s: %s",descr, letter, round(eval(CALL),2)),
              alist(descr="Res",letter = "A"))
  expect_error(tag1(descr = "Result", letter = "B")(log)(10,descr = "Result"))
})



test_that("an adverb can be defined and work with all syntaxes", {
  adv1 <- tag_adverb(sprintf("%s: %s",descr, round(CALL(eval=TRUE),2)),alist(descr="Res"))
  expect_identical("Res: 2.3", adv1$log(10))
  expect_identical("Result: 2.3", adv1$log(10, descr = "Result"))
  expect_identical("Result: 2.3", adv1(log)(10, descr = "Result"))
  expect_identical("Result: 2.3", adv1(log, descr = "Result")(10))
})

test_that("Arguments cannot be on both sides", {
  adv1 <- tag_adverb(sprintf("%s: %s",descr, round(CALL(eval=TRUE),2)),alist(descr="Res"))
  expect_error(adv1(log, descr = "Result")(10, descr = "Result"))
})


test_that("a tag can be defined and work with all relevant syntaxes", {
  tag1 <- tag(sprintf("%s %s: %s",descr, letter, round(CALL(eval=TRUE),2)),
              alist(descr="Res",letter = "A"))
  expect_identical("Res A: 2.3", tag1$log(10))
  expect_identical("Result B: 2.3", tag1$log(10, descr = "Result", letter = "B"))
  expect_identical("Result B: 2.3", tag1()$log(10, descr = "Result", letter = "B"))
  expect_identical("Result B: 2.3", tag1()(log)(10, descr = "Result", letter = "B"))
  expect_identical("Result B: 2.3", tag1(descr = "Result")$log(10, letter = "B"))
  expect_identical("Result B: 2.3", tag1(descr = "Result")(log)(10, letter = "B"))
  expect_identical("Result B: 2.3", tag1(descr = "Result", letter = "B")$log(10))
  expect_identical("Result B: 2.3", tag1(descr = "Result", letter = "B")(log)(10))
})


test_that("Arguments cannot be on both sides", {
  tag1 <- tag(sprintf("%s %s: %s",descr, letter, round(CALL(eval=TRUE),2)),
              alist(descr="Res",letter = "A"))
  expect_error(tag1(descr = "Result", letter = "B")(log)(10,descr = "Result"))
})
