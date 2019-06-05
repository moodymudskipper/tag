

# test_that("by applying 2 conversions from a standard tag_adverb I end up with the same object", {
#   adv1 <- tag_adverb(sprintf("%s: %s",desc, round(eval(CALL),2)),alist(desc="Res"))
#   expect_equal(adv1, as_tag_adverb(as_tag(adv1)))
# })
#
# test_that("by applying 2 conversions from a standard tag I end up with the same object", {
#   tag1 <- tag(sprintf("%s: %s",desc, round(eval(CALL),2)),alist(desc="Res"))
#   expect_equal(tag1, as_tag(as_tag_adverb(tag1)))
# })
#
#
# test_that("conversions of purrr adverbs to tag adverbs work", {
#   safely2 <- as_tag_adverb(purrr::safely)
#   res <- purrr::safely(log, otherwise = "foo")("a")
#   expect_equal(res, safely2(log, otherwise = "foo")("a"))
#   expect_equal(res, safely2(log)("a", otherwise = "foo"))
#   expect_equal(res, safely2$log("a", otherwise = "foo"))
# })
#
# test_that("conversions of purrr adverbs to tags work", {
#   safely3 <- as_tag(purrr::safely)
#   res <- purrr::safely(log, otherwise = "foo")("a")
#   expect_equal(res, safely3(otherwise = "foo")(log)("a"))
#   expect_equal(res, safely3(otherwise = "foo")$log("a"))
#   expect_equal(res, safely3()(log, otherwise = "foo")("a"))
#   expect_equal(res, safely3()(log)("a", otherwise = "foo"))
#   expect_equal(res, safely3()$log("a",otherwise = "foo"))
#   expect_equal(res, safely3$log("a",otherwise = "foo"))
# })
#
# test_that("converting in one or two steps is the same", {
#   safely2 <- as_tag_adverb(purrr::safely)
#   safely3 <- as_tag(purrr::safely)
#   safely4 <- as_tag(safely2)
#   safely5 <- as_tag_adverb(safely3)
#   expect_equal(safely2, safely5)
#   expect_equal(safely3, safely4)
# })
#
# test_that("by applying 2 conversions from a converted tag_adverb I end up with the same object", {
#   safely2 <- as_tag_adverb(purrr::safely)
#   expect_equal(safely2, as_tag_adverb(as_tag(safely2)))
# })


test_that("by applying 2 conversions from a standard tag_adverb I end up with the same object", {
  adv1 <- tag_adverb(sprintf("%s: %s",desc, round(CALL(),2)),alist(desc="Res"))
  expect_equal(adv1, as_tag_adverb(as_tag(adv1)))
})

test_that("by applying 2 conversions from a standard tag I end up with the same object", {
  tag1 <- tag(sprintf("%s: %s",desc, round(CALL(),2)),alist(desc="Res"))
  expect_equal(tag1, as_tag(as_tag_adverb(tag1)))
})


test_that("conversions of purrr adverbs to tag adverbs work", {
  safely2 <- as_tag_adverb(purrr::safely)
  res <- purrr::safely(log, otherwise = "foo")("a")
  expect_equal(res, safely2(log, otherwise = "foo")("a"))
  expect_equal(res, safely2(log)("a", otherwise = "foo"))
  expect_equal(res, safely2$log("a", otherwise = "foo"))
})

test_that("conversions of purrr adverbs to tags work", {
  safely3 <- as_tag(purrr::safely)
  res <- purrr::safely(log, otherwise = "foo")("a")
  expect_equal(res, safely3(otherwise = "foo")(log)("a"))
  expect_equal(res, safely3(otherwise = "foo")$log("a"))
  expect_equal(res, safely3()(log, otherwise = "foo")("a"))
  expect_equal(res, safely3()(log)("a", otherwise = "foo"))
  expect_equal(res, safely3()$log("a",otherwise = "foo"))
  expect_equal(res, safely3$log("a",otherwise = "foo"))
})

test_that("converting in one or two steps is the same", {
  safely2 <- as_tag_adverb(purrr::safely)
  safely3 <- as_tag(purrr::safely)
  safely4 <- as_tag(safely2)
  safely5 <- as_tag_adverb(safely3)
  expect_equal(safely2, safely5)
  expect_equal(safely3, safely4)
})

test_that("by applying 2 conversions from a converted tag_adverb I end up with the same object", {
  safely2 <- as_tag_adverb(purrr::safely)
  expect_equal(safely2, as_tag_adverb(as_tag(safely2)))
})
