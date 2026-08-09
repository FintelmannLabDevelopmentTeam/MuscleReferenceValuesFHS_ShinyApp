# Unit tests for batch processing of the T12_abdomen / T12_thorax cohorts
# added to models_anon on top of the original T5/T8/T10/L3 cohort.
# LMS_functions.R is sourced once by tests/testthat.R before these run.

test_that("T12 abdomen and thorax models are merged into models_anon", {
  expect_equal(length(models_anon), 72) # 32 original + 20 abdomen + 20 thorax

  expect_true("CSMA_Female_T12_abdomen" %in% names(models_anon))
  expect_true("CSMA_Female_T12_thorax" %in% names(models_anon))
  expect_true("Fatty_muscle_fraction_Male_T12_thorax" %in% names(models_anon))
  expect_true("CSMA_Female_T5" %in% names(models_anon)) # original cohort untouched

  # plain "_T12" keys (ambiguous between abdomen/thorax) should not remain
  expect_false(any(grepl("_T12$", names(models_anon))))
})

test_that("get_z / get_percentile work for a T12_abdomen muscle metric", {
  expect_equal(get_z(150, 50, "CSMA", "Female", "T12_abdomen"), 2.94)
  expect_equal(get_percentile(150, 50, "CSMA", "Female", "T12_abdomen"), 1)
})

test_that("get_z / get_percentile work for a T12_thorax fat metric", {
  expect_equal(get_z(50, 60, "CSSAT", "Male", "T12_thorax"), -1.1)
  expect_equal(get_z(45, 60, "CSSAT", "Male", "T12_abdomen"), -1.97)
  expect_equal(get_percentile(45, 60, "CSSAT", "Male", "T12_abdomen"), 0.02)
})

test_that("calc_z_scores_percentiles handles a mixed batch across old and new cohorts", {
  df <- data.frame(
    measurement = c(150, 60,  45, 12,  180, 40,  90, 30,  200),
    metric      = c("CSMA", "SMRA", "CSSAT", "Fatty_muscle_fraction", "SMI", "IMAT_index", "CSMA", "SAT_index", "SMG"),
    age         = c(50,  65,  60,  45,  70,  55,  38,  80,  20),
    lvl         = c("T5", "T10", "T12_abdomen", "T12_thorax", "L3", "T12_thorax", "T12_abdomen", "T12_abdomen", "T8"),
    sex         = c("Female", "Male", "Male", "Female", "Female", "Male", "Male", "Female", "Female"),
    stringsAsFactors = FALSE
  )

  result <- calc_z_scores_percentiles(df)

  expect_equal(result$Z, c(0.34, 3.39, -1.97, Inf, 7.43, 5.32, -1.94, -1.45, NA))
  expect_equal(result$P, c(0.63, 1.00, 0.02, 1.00, 1.00, 1.00, 0.03, 0.07, NA))
})

test_that("ages outside the 38-80 fitted range return NA, not extrapolated values", {
  expect_null(get_z(200, 20, "SMG", "Female", "T8"))     # below MIN_AGE
  expect_null(get_percentile(200, 90, "SMG", "Female", "T8")) # above MAX_AGE
})
