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

    mock_file_paths <- data.frame(
      source_name = names(desezoniranje_config),
      file_path = rep("/fake/path.xlsx", length(desezoniranje_config)),
      file_mtime = rep(as.POSIXct("2025-01-15 10:00:00"), length(desezoniranje_config)),
      stringsAsFactors = FALSE
    )

    mock_extracted_data <- list(
      BP_Orig = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(100, 110)),
      BP_SA = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(105, 115)),
      DA_Orig = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(200, 210)),
      DA_SA = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(205, 215)),
      ILO_Orig = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(300, 310)),
      ILO_SA = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(305, 315)),
      RB_Orig = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(400, 410)),
      RB_SA = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(405, 415)),
      BPR_Orig = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(500, 510)),
      BPR_SA = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(505, 515)),
      RMNP_Orig = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(600, 610)),
      RMNP_SA = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(605, 615))
    )

    mock_vintage_result <- list(count = 40)
    mock_datapoint_result <- list(
      periods_inserted = 0,
      datapoints_inserted = 2,
      flags_inserted = 0
    )

    # Stub at multiple levels
    mockery::stub(DESEZ_import_data_points, 'get_all_recent_files', mock_file_paths)
    mockery::stub(DESEZ_import_data_points, 'extract_all_desezoniranje_data', mock_extracted_data)

    # Stub inside prepare_vintage_table (this is where it's actually called)
    mockery::stub(prepare_vintage_table, 'UMARaccessR::sql_get_vintage_from_series_code', NA_integer_)

    # Stub insert functions
    mockery::stub(DESEZ_import_data_points, 'UMARimportR::insert_new_vintage', mock_vintage_result)
    mockery::stub(DESEZ_import_data_points, 'UMARimportR::insert_prepared_data_points', mock_datapoint_result)

    result <- DESEZ_import_data_points(con_test)

    expect_equal(names(result), c("vintages", "datapoints"))
    expect_equal(result$vintages, mock_vintage_result)
    expect_equal(length(result$datapoints), 12)
    expect_true(all(purrr::map_lgl(result$datapoints, is.list)))
  })
})
