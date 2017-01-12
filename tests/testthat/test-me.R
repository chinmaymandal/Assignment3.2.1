context("Test Assignment3.2.1 package")

library(dplyr)
library(maps)

setwd(system.file("extdata", package = "Assignment3.2.1"))

test_that("Testing fars_read()", {
  expect_is(fars_read("accident_2015.csv.bz2"), "tbl_df")
  expect_error(fars_read("accident_2016.csv.bz2"))
})

test_that("Testing fars_summarize_years()", {
  expect_is(fars_summarize_years(2013:2015), "tbl_df")
  expect_error(fars_summarize_years(2016))
})

test_that("Testing fars_map_state()", {
  expect_error(fars_map_state(1, 2016))
})
