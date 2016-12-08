#check fars_summarize_years returns a list
library(testthat)
library(packagesfinalproject)
expect_that(fars_summarize_years(2013), is_a("list"))
