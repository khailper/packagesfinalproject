#check fars_summarize_years returns a list
library(testthat)
expect_that(fars_summarize_years(2013), is_a("list"))
