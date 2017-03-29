library(testthat)
library(farsassignment)

context("farstests")

test_that("File name created", expect_match(make_filename(2016), "accident_2016.csv.bz2"))



