test_that("mock tests table prep without db or file access", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Mock file_paths_df instead of calling get_all_recent_files()
    file_paths_df <- data.frame(
      source_name = c("bruto_placa", "delovno_aktivni", "delovno_aktivni_brez_kmetov",
                      "delovno_aktivni_disagr", "ilo_brezposelni", "ilo_stopnja",
                      "ilo_zaposleni", "reg_brezposelni", "reg_brezposelni_stopnja"),
      file_path = rep(testthat::test_path("testdata", "test_single.xlsx"), 9),
      file_mtime = rep(Sys.time(), 9),
      stringsAsFactors = FALSE
    )

    x <- prepare_vintage_table(file_paths_df, con_test, schema = "platform")
    expect_equal(dim(x), c(34, 2))
    expect_equal(names(x), c("series_id", "published"))

    # Mock extracted data instead of calling extract_all_desezoniranje_data()
    mock_data <- list(
      BP_Orig = data.frame(
        period_id = c("2024M01", "2024M02", "2024M03"),
        VSI = c(100, 110, 120),
        BK = c(200, 210, 220)
      ),
      BP_SA = data.frame(
        period_id = c("2024M01", "2024M02", "2024M03"),
        VSI = c(105, 115, 125),
        BK = c(205, 215, 225)
      )
    )

    datapoint_tables <- purrr::imap(mock_data, ~{
      prepare_datapoint_table(.x, .y, con_test)
    })

    expect_equal(length(datapoint_tables), 2)
    expect_equal(ncol(datapoint_tables[[1]]$data), 5)
    expect_equal(names(datapoint_tables[[1]]$data), c("time", "Meritev",
                                                      "Seasonally.adjusted",
                                                      "value", "flag"))
  })
})
