#check fars_summarize_years returns a list
library(packagesfinalproject)
testthat::expect_that(fars_summarize_years(2013), testthat::is_a("data.frame"))
