usethis::use_directory("exclude", ignore = TRUE)
usethis::use_build_ignore("exclude")
usethis::use_git_ignore("exclude")
usethis::use_package("utils")
usethis::use_package("magrittr")
usethis::use_package("rlang")
usethis::use_package("data.table")
usethis::use_package("dplyr")
usethis::use_package("purrr")
usethis::use_package("tidyselect")
usethis::use_package("tibble")
usethis::use_package("tidyr")
usethis::use_gpl3_license("GPL-3")

usethis::use_testthat()
usethis::use_readme_rmd()

usethis::use_version()
usethis::use_news_md()


shell("git remote add origin https://github.com/moodymudskipper/tag.git", intern = TRUE)
shell("git push -u origin master",intern = TRUE)

devtools::install_github("moodymudskipper/tag")





# * A new system of pattern helpers has been designed, we use `CALL(eval = TRUE)`
# or `CALL(eval = FALSE)` instead of `eval.parent(CALL)` or `CALL`. Eliminating
# the weird substitution that we had makes things much clearer.
# * The environment play has been debugged as the code was very easy to break
# * `tag_adverb` and `as_tag_adverb` are not exported anymore, they added
# confusion and little value, we focus on tags
# * the parameter `rm_args` was removed from `tag` and `tag_adverb` as it was never
# used in package *tags* and seemed not so useful
# * the parameter `eval_args` was added to be able to use lazy evaluation on tag arguments
# * The README was completely reworked
# * The documentation was improved
