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

    mock_vintage_table <- data.frame(
      series_id = 1:40,
      published = rep(as.POSIXct("2025-01-15 10:00:00"), 40)
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
      RMNP_SA = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(605, 615)),
      DBN_Orig = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(600, 610)),
      DBN_SA = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(605, 615)),
      DBR_Orig = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(600, 610)),
      DBR_SA = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(605, 615)),
      ST_Orig = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(600, 610)),
      ST_SA = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(605, 615)),
      PDT_Orig = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(600, 610)),
      PDT_SA = data.frame(period_id = c("2024M01", "2024M02"), col1 = c(605, 615))
    )

    # Mock the prepared datapoint tables (what prepare_datapoint_table returns)
    mock_datapoint_tables <- purrr::imap(mock_extracted_data, ~{
      list(
        data = data.frame(
          time = c("2024M01", "2024M02"),
          Meritev = c("col1", "col1"),
          Seasonally.adjusted = if(grepl("SA$", .y)) "Y" else "N",
          value = c(100, 110),
          flag = c("", "")
        ),
        table_id = 1,
        dimension_names = c("Meritev", "Seasonally.adjusted"),
        dimension_ids = c(1, 2),
        interval_id = "M"
      )
    })

    mock_vintage_result <- list(count = 40)
    mock_datapoint_result <- list(
      periods_inserted = 0,
      datapoints_inserted = 2,
      flags_inserted = 0
    )

    # Stub everything
    mockery::stub(DESEZ_import_data_points, 'prepare_vintage_table', mock_vintage_table)
    mockery::stub(DESEZ_import_data_points, 'get_all_recent_files', mock_file_paths)
    mockery::stub(DESEZ_import_data_points, 'extract_all_desezoniranje_data', mock_extracted_data)

    # Stub the imap that calls prepare_datapoint_table
    mockery::stub(DESEZ_import_data_points, 'purrr::imap',
                  function(data, fn) mock_datapoint_tables)

    mockery::stub(DESEZ_import_data_points, 'UMARimportR::insert_new_vintage', mock_vintage_result)
    mockery::stub(DESEZ_import_data_points, 'UMARimportR::insert_prepared_data_points', mock_datapoint_result)

    result <- DESEZ_import_data_points(con_test)

    expect_equal(names(result), c("vintages", "datapoints"))
    expect_equal(result$vintages, mock_vintage_result)
    expect_equal(length(result$datapoints), 20)
    expect_true(all(purrr::map_lgl(result$datapoints, is.list)))
  })
})
