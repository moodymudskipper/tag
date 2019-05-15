context("definitions")
library(rlang)
library(purrr)


test_that("an adverb can be defined and work with all syntaxes", {
  adv1 <- tag_adverb(sprintf("%s: %s",desc, round(eval(CALL),2)),alist(desc="Res"))
  expect_identical("Res: 2.3", adv1$log(10))
  expect_identical("Result: 2.3", adv1$log(10, desc = "Result"))
  expect_identical("Result: 2.3", adv1(log)(10, desc = "Result"))
  expect_identical("Result: 2.3", adv1(log, desc = "Result")(10))
})

test_that("Arguments cannot be on both sides", {
  adv1 <- tag_adverb(sprintf("%s: %s",desc, round(eval(CALL),2)),alist(desc="Res"))
  expect_error(adv1(log, desc = "Result")(10, desc = "Result"))
})


test_that("a tag can be defined and work with all relevant syntaxes", {
  tag1 <- tag(sprintf("%s %s: %s",desc, letter, round(eval(CALL),2)),
              alist(desc="Res",letter = "A"))
  expect_identical("Res A: 2.3", tag1$log(10))
  expect_identical("Result B: 2.3", tag1$log(10, desc = "Result", letter = "B"))
  expect_identical("Result B: 2.3", tag1()$log(10, desc = "Result", letter = "B"))
  expect_identical("Result B: 2.3", tag1()(log)(10, desc = "Result", letter = "B"))
  expect_identical("Result B: 2.3", tag1(desc = "Result")$log(10, letter = "B"))
  expect_identical("Result B: 2.3", tag1(desc = "Result")(log)(10, letter = "B"))
  expect_identical("Result B: 2.3", tag1(desc = "Result", letter = "B")$log(10))
  expect_identical("Result B: 2.3", tag1(desc = "Result", letter = "B")(log)(10))
})


test_that("Arguments cannot be on both sides", {
  tag1 <- tag(sprintf("%s %s: %s",desc, letter, round(eval(CALL),2)),
              alist(desc="Res",letter = "A"))
  expect_error(tag1(desc = "Result", letter = "B")(log)(10,desc = "Result"))
})



