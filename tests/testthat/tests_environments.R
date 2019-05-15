context("Environments")

test_that("the enclosing environments of an adverb is empty", {
  adv1 <- tag_adverb(sprintf("%s: %s",desc, round(eval(CALL),2)),alist(desc="Res"))
  expect_length(ls(environment(adv1)), 0)
})

test_that("the enclosing environments of a manufactured function contains 'f' and the new arguments", {
  adv1 <- tag_adverb(sprintf("%s: %s",desc, round(eval(CALL),2)),alist(desc="Res"))
  expect_identical(ls(environment(adv1$log)), c("desc","f"))
})

test_that("the enclosing environments of a tag contains only f", {
  tag1 <- tag(sprintf("%s %s: %s",desc, letter, round(eval(CALL),2)),
              alist(desc="Res",letter = "A"))
  expect_identical(ls(environment(tag1)), c("f"))
})

test_that("the enclosing environments of an adverb made by a tag contains 'f' and the new arguments", {
  tag1 <- tag(sprintf("%s %s: %s",desc, letter, round(eval(CALL),2)),
              alist(desc="Res",letter = "A"))
  expect_identical(ls(environment(tag1$log)), c("desc","f","letter"))
})
