# This code was run once and is here for archival purposes.
# devtools::install_github("majazaloznik/UMARaccessR")
# devtools::install_github("majazaloznik/UMARimportR")
source("tests/testthat/helper-connection.R")

# start_db_capturing()
# con <- make_connection()
# out <- prepare_source_table(con)
# stop_db_capturing()
#
#
start_db_capturing()
con_test <- make_test_connection()
table_list <- prepare_table_table(con_test)
purrr::walk(table_list, ~{
  UMARimportR::insert_new_table_table( con = con_test, .x, "platform")
})
stop_db_capturing()

# start_db_capturing()
# con_test <- make_test_connection()
# category_table <- prepare_category_table(con = con_test)
# stop_db_capturing()
#
# UMARimportR::insert_new_category(con_test, category_table, "platform")

# start_db_capturing()
# con_test <- make_test_connection()
# category_relationship_table <- prepare_category_relationship_table(con = con_test)
# stop_db_capturing()
# UMARimportR::insert_new_category_relationship(con_test, category_relationship_table, "platform")
#
# start_db_capturing()
# con_test <- make_test_connection()
# category_table_table <- prepare_category_table_table(con_test)
# stop_db_capturing()
#
# UMARimportR::insert_new_category_table(con_test, category_table_table)

start_db_capturing()
con_test <- make_test_connection()
table_dimension_table <- prepare_table_dimensions_table(con_test)
stop_db_capturing()

UMARimportR::insert_new_table_dimensions(con_test, table_dimension_table)

start_db_capturing()
con_test <- make_test_connection()
dimension_levels_table <- prepare_dimension_levels_table(con_test)
stop_db_capturing()

UMARimportR::insert_new_dimension_levels(con_test, dimension_levels_table)

start_db_capturing()
con_test <- make_test_connection()
series_table <- prepare_series_table(con_test)
stop_db_capturing()

UMARimportR::insert_new_series(con_test, series_table)


start_db_capturing()
con_test <- make_test_connection()
series_levels_table <- prepare_series_levels_table(con_test)
stop_db_capturing()

UMARimportR::insert_new_series_levels(con_test, series_levels_table)

con_test <- make_test_connection()

start_db_capturing()
con_test <- make_test_connection()
result <- DESEZ_import_structure(con_test)
stop_db_capturing()

start_db_capturing()
con_test <- make_test_connection()
result <- prepare_vintage_table(con_test)
stop_db_capturing()
UMARimportR::insert_new_vintage(con_test, result)


start_db_capturing()
con_test <- make_test_connection()
file_paths_df <- get_all_recent_files()
data <- extract_all_desezoniranje_data(file_paths_df)
datapoint_tables <- purrr::imap(data, ~{
  prepare_datapoint_table(.x, .y, con_test)
})

purrr::walk(datapoint_tables, ~{
  UMARimportR::insert_prepared_data_points(.x, con = con_test, schema = "platform")
})
stop_db_capturing()
