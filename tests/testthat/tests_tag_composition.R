# context("tag_composition")
#
# test_that("composed tag works and return right output", {
#
# logging <- tag(args = alist(.time = TRUE, .print = TRUE),{
#   message(deparse(sys.call()))
#   if(.time) {
#     time_sec <- system.time(res <- CALL(eval = TRUE))[3]
#     cat("  ~", time_sec, "sec\n")
#   } else {
#     res <- CALL(eval = TRUE)
#   }
#   if(.print) print(res)
#   invisible(res)
# })
#
# negating <- tag({
#   call <- CALL(eval = FALSE)
#   call[[1]] <- Negate(f)
#   eval.parent(call)
# })
#
# two_tags <- logging$negating
# two_tags2 <- logging(.time = FALSE,.print = FALSE)$negating
#
# expect_identical(logging$negating$isTRUE(FALSE), TRUE)
# expect_identical(two_tags$isTRUE(FALSE), TRUE)
# expect_identical(two_tags2$isTRUE(FALSE), TRUE)
# })
