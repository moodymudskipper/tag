usethis::use_directory("exclude", ignore = TRUE)
usethis::use_build_ignore("exclude")
usethis::use_git_ignore("exclude")
usethis::use_git_ignore("package_builder","exclude")
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
shell("git push -u origin master -f",intern = TRUE)


shell("grep -rli tags.git .git/*")
shell("findstr tags.git .git/*")
shell("git commit",intern = TRUE)


devtools::install_github("moodymudskipper/tag")
