test_that("DESEZ_import_structure works correctly", {
  with_mock_db({
    con_test <- make_test_connection()
    result <- DESEZ_import_structure(con_test)
    expect_true("dimension_levels" %in% names(result))
    expect_true(is.list(result$dimension_levels))
    expect_true("count" %in% names(result$dimension_levels))
  })
})

test_that("DESEZ_import_data_points works correctly", {
  with_mock_db({
    con_test <- make_test_connection()
    result <- DESEZ_import_data_points(con_test)
    expect_true(all(c("vintages", "datapoints") %in% names(result)))
    expect_equal(nrow(result$vintages), 34)
    expect_equal(length(result$datapoints), 8)
    expect_true(is.list(result$datapoints))
    expect_equal(result$datapoints$RB_SA$datapoints_inserted, 690)
  })
})
