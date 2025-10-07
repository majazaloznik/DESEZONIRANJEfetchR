test_that("mock tests table prep without db access", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    x <- prepare_vintage_table( con_test, schema = "platform")
    expect_equal(dim(x), c(34,2))
    expect_equal(names(x), c("series_id", "published"))
    file_paths_df <- get_all_recent_files()
    data <- extract_all_desezoniranje_data(file_paths_df)
    datapoint_tables <- purrr::imap(data, ~{
      prepare_datapoint_table(.x, .y, con_test)
    })
    expect_equal(length(datapoint_tables), 8)
    expect_equal(ncol(datapoint_tables[[1]]$data), 5)
    expect_equal(names(datapoint_tables[[1]]$data), c("time", "Meritev",
                                                 "Seasonally.adjusted",
                                                 "value", "flag"))
  })
})
