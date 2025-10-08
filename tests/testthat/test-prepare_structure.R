test_that("prepare source table", {
  dittodb::with_mock_db({
    con <- make_connection()
    out <- prepare_source_table(con, schema = "platform")
    expect_equal(nrow(out), 1)
    expect_equal(out$id, 6)
  })
})

test_that("prepare table table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    table_table <- prepare_table_table(con_test)
    expect_equal(length(table_table), 4)
    expect_equal(ncol(table_table[[1]]), 6)
    expect_true(all(names(table_table[[1]]) %in%
                      c("name", "notes", "source_id", "url", "code", "keep_vintage")))
  })
})

test_that("prepare category table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_category_table( con = con_test)
    expect_equal(nrow(out), 5)
    expect_equal(ncol(out), 3)
    expect_true(all(names(out) %in% c("id", "name",  "source_id")))
    expect_equal(out$source_id[1], 6)
  })
})

test_that("prepare category relationship table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_category_relationship_table(con = con_test)
    expect_equal(nrow(out), 4)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("id", "parent_id", "source_id"))
    expect_equal(out$source_id[1], 6)
  })
})

test_that("prepare category table table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_category_table_table(con_test)
    expect_equal(nrow(out), 4)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("category_id", "table_id", "source_id"))
    expect_equal(out$source_id[1], 6)
    expect_equal(out$category_id[1], 1)
    expect_equal(out$table_id[1], 204)
  })
})

test_that("prepare table dinemsions table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_table_dimensions_table(con_test)
    expect_equal(nrow(out), 8)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("table_id", "dimension", "is_time"))
    expect_equal(out$dimension[1], "Meritev")
    expect_equal(out$dimension[2], "Seasonally adjusted")
    expect_equal(out$is_time[1], FALSE )
  })
})

test_that("prepare dimensions levels table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_dimension_levels_table(con_test)
    expect_equal(nrow(out), 25)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("tab_dim_id", "level_value", "level_text"))
    expect_true(any(is.na(out)) == FALSE)
  })
})

test_that("prepare series table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- series_table <- prepare_series_table(con_test)
    expect_equal(nrow(out), 34)
    expect_equal(ncol(out), 5)
    expect_equal(names(out), c("table_id", "name_long", "code", "unit_id", "interval_id"))
    expect_true(any(is.na(out)) == FALSE)
  })
})

test_that("prepare series levels table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_series_levels_table(con_test)
    expect_equal(nrow(out), 68)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("series_id", "tab_dim_id", "level_value"))
    expect_true(any(is.na(out)) == FALSE)
  })
})
