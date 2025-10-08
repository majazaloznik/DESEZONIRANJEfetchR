# tests/testthat/test-extract_data.R

test_that("extract_single_sheet reads and renames columns correctly", {
  test_file <- testthat::test_path("testdata", "test_single.xlsx")

  config <- list(
    column_codes = c("period_id", "value_1", "value_2"),
    interval = "M"
  )

  result <- extract_single_sheet(test_file, config, "SA")

  expect_equal(names(result), c("period_id", "value_1", "value_2"))
  expect_equal(nrow(result), 3)
  expect_equal(result$period_id, c("2024M01", "2024M02", "2024M03"))
  expect_equal(result$value_1, c(100, 110, 120))
})

test_that("extract_single_sheet reads Orig sheet correctly", {
  test_file <- testthat::test_path("testdata", "test_single.xlsx")

  config <- list(
    column_codes = c("period_id", "value_1", "value_2"),
    interval = "M"
  )

  result <- extract_single_sheet(test_file, config, "Orig")

  expect_equal(names(result), c("period_id", "value_1", "value_2"))
  expect_equal(result$value_1, c(95, 105, 115))
})

test_that("extract_single_sheet ignores extra columns", {
  test_file <- testthat::test_path("testdata", "test_extra_cols.xlsx")

  config <- list(
    column_codes = c("period_id", "col_1", "col_2"),  # Only want first 3
    interval = "M"
  )

  result <- extract_single_sheet(test_file, config, "SA")

  expect_equal(names(result), c("period_id", "col_1", "col_2"))
  expect_equal(ncol(result), 3)  # Should not include Col3 or Col4
})

test_that("join_on_period combines dataframes with matching periods", {
  df1 <- data.frame(period_id = c("01/01/2024", "01/02/2024", "01/03/2024"),
                    val_a = c(10, 20, 30))
  df2 <- data.frame(period_id = c("01/01/2024", "01/02/2024", "01/03/2024"),
                    val_b = c(40, 50, 60))

  result <- join_on_period(list(df1, df2))

  expect_equal(names(result), c("period_id", "val_a", "val_b"))
  expect_equal(nrow(result), 3)
  expect_equal(result$val_a, c(10, 20, 30))
  expect_equal(result$val_b, c(40, 50, 60))
})

test_that("join_on_period handles non-overlapping periods", {
  df1 <- data.frame(period_id = c("2024M01", "2024M02", "2024M03"),
                    val_a = c(10, 20, 30))
  df2 <- data.frame(period_id = c("2024M01", "2024M02", "2024M04"),  # M04 instead of M03
                    val_b = c(40, 50, 60))

  result <- join_on_period(list(df1, df2))

  expect_equal(nrow(result), 4)  # All unique periods
  expect_true(is.na(result$val_b[result$period_id == "2024M03"]))
  expect_true(is.na(result$val_a[result$period_id == "2024M04"]))
})

test_that("join_on_period handles three-way joins", {
  df1 <- data.frame(period_id = c("2024M01", "2024M02"), val_a = c(10, 20))
  df2 <- data.frame(period_id = c("2024M01", "2024M02"), val_b = c(40, 50))
  df3 <- data.frame(period_id = c("2024M01", "2024M03"), val_c = c(70, 80))

  result <- join_on_period(list(df1, df2, df3))

  expect_equal(names(result), c("period_id", "val_a", "val_b", "val_c"))
  expect_equal(nrow(result), 3)
})

test_that("extract_all_desezoniranje_data integrates single file workflow", {
  test_file <- testthat::test_path("testdata", "test_single.xlsx")

  mock_config <- list(
    test_single = list(
      table_id = "single_table",
      column_codes = c("period_id", "value_1", "value_2"),
      interval = "M"
    )
  )

  # Create data frame matching new format
  file_paths_df <- data.frame(
    source_name = "test_single",
    file_path = test_file,
    file_mtime = Sys.time(),
    stringsAsFactors = FALSE
  )

  result <- extract_all_desezoniranje_data(file_paths_df, config = mock_config)

  expect_named(result, c("single_table_Orig", "single_table_SA"))
  expect_equal(nrow(result$single_table_SA), 3)
  expect_equal(names(result$single_table_SA), c("period_id", "value_1", "value_2"))
  expect_equal(result$single_table_SA$value_1, c(100, 110, 120))
  expect_equal(result$single_table_Orig$value_1, c(95, 105, 115))
})

test_that("extract_all_desezoniranje_data joins multi-source files correctly", {
  test_file_a <- testthat::test_path("testdata", "test_multi_a.xlsx")
  test_file_b <- testthat::test_path("testdata", "test_multi_b.xlsx")

  mock_config <- list(
    multi_source_a = list(
      table_id = "multi_table",
      column_codes = c("period_id", "value_a"),
      interval = "M"
    ),
    multi_source_b = list(
      table_id = "multi_table",
      column_codes = c("period_id", "value_b"),
      interval = "M"
    )
  )

  # Create data frame matching new format
  file_paths_df <- data.frame(
    source_name = c("multi_source_a", "multi_source_b"),
    file_path = c(test_file_a, test_file_b),
    file_mtime = rep(Sys.time(), 2),
    stringsAsFactors = FALSE
  )

  result <- extract_all_desezoniranje_data(file_paths_df, config = mock_config)

  expect_named(result, c("multi_table_Orig", "multi_table_SA"))

  # Check SA sheet
  expect_equal(names(result$multi_table_SA), c("period_id", "value_a", "value_b"))
  expect_equal(nrow(result$multi_table_SA), 4)  # M01, M02, M03, M04

  # M03 should have value_a but NA for value_b
  m03_row <- result$multi_table_SA[result$multi_table_SA$period_id == "2024M03", ]
  expect_equal(m03_row$value_a, 30)
  expect_true(is.na(m03_row$value_b))

  # M04 should have value_b but NA for value_a
  m04_row <- result$multi_table_SA[result$multi_table_SA$period_id == "2024M04", ]
  expect_true(is.na(m04_row$value_a))
  expect_equal(m04_row$value_b, 60)
})
