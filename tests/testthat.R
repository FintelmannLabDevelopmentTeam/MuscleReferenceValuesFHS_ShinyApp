# Test runner. Run from the repo root, e.g.:
#   Rscript tests/testthat.R
# or, from an R session already `setwd()`'d to the repo root:
#   testthat::test_dir("tests/testthat")
#
# This is a plain Shiny app, not an R package, so there's no test_check()/
# DESCRIPTION to anchor paths. LMS_functions.R (and its own load() calls for
# resources/*.RData) uses paths relative to the repo root, so it must be
# sourced here - before test_dir() switches the working directory to
# tests/testthat for the duration of the tests. Objects and functions
# defined here are visible inside the test files.

library(testthat)

source("LMS_functions.R")

test_dir("tests/testthat", reporter = "summary")
