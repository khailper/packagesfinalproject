#check fars_summarize_years returns a tibble
library(testthat)
expect_that(fars_summarize_years(2013), is_a("list"))
