## ---- echo = FALSE, include = FALSE--------------------------------------
library(Assignment3.2.1)
library(dplyr)
library(maps)

## ----fars_read_example---------------------------------------------------
filename <- system.file("extdata/accident_2013.csv.bz2", package = "Assignment3.2.1")
fars_read(filename)

## ----fars_summarize_years_example----------------------------------------
setwd(system.file("extdata", package = "Assignment3.2.1"))
fars_summarize_years(2013:2015)

## ----fars_map_state_example----------------------------------------------
setwd(system.file("extdata", package = "Assignment3.2.1"))
fars_map_state(45, 2015)

