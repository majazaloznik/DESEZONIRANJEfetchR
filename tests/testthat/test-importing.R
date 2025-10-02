test_that("DESEZ_import_structure works correctly with mocked dimension_selector", {
  with_mock_db({
    con_test <- make_test_connection()
    result <- DESEZ_import_structure(con_test)
    expect_true("dimension_levels" %in% names(result))
    expect_true(is.list(result$dimension_levels))
    expect_true("count" %in% names(result$dimension_levels))
  })
})
