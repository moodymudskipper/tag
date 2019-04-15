usethis::use_directory("exclude", ignore = TRUE)
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
usethis::depen

usethis::use_readme_rmd()

devtools::check_win_devel()
