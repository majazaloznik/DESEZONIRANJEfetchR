#' Import structural metadata for the (current) DESEZONIRANJE tables
#'
#' Umbrella function that prepares and import all the metadata tables into
#' the database. It uses the functions from the UMARimportR package to
#' insert the data into the database.
#'
#' @param config List of configurations. Defaults to `desezoniranje_config`.
#' @param con connection to database
#' @param schema schema name, defaults to "platform"
#' @param keep_vintage logical indicating whether to keep vintages, defaults to F
#'
#' @returns nothing
#' @export
#'
DESEZ_import_structure <- function(con,  config = desezoniranje_config,
                                schema = "platform",
                                keep_vintage = FALSE) {
  message("Importing structure data for DESEZONIRANJE into schema ", schema)
  # Create list to store all results
  insert_results <- list()
  # prepare and insert table
  table_list <- prepare_table_table(con, config, schema, keep_vintage)
  insert_results$table <- purrr::map_dfr(table_list, ~{
    UMARimportR::insert_new_table_table(con, .x, schema)
  })
  message("Table insert: ", sum(insert_results$table$count), " rows")
  # preapre and insert category table
  category_table <- prepare_category_table(con, config, schema)
  insert_results$category <- UMARimportR::insert_new_category(con, category_table, schema)
  message("Category insert: ", insert_results$category$count, " rows")
  # prepare and insert category relationship table
  category_relationship_table <- prepare_category_relationship_table(con, config, schema)
  insert_results$category_relantionship <- UMARimportR::insert_new_category_relationship(
    con, category_relationship_table, schema)
  message("Category relationship insert: ", insert_results$category_relantionship$count, " rows")
  # prepare and insert category table table
  category_table_table <- prepare_category_table_table(con, config, schema)
  insert_results$category_table <- UMARimportR::insert_new_category_table(
    con, category_table_table, schema)
  message("Category table insert: ", insert_results$category_table$count, " rows")
  # prepare and insert table dimension table
  table_dimension_table <- prepare_table_dimensions_table(con, config, schema)
  insert_results$table_dimensions <- UMARimportR::insert_new_table_dimensions(
    con, table_dimension_table, schema)
  message("Table dimensions insert: ", insert_results$table_dimensions$count, " rows")
  # prepare and select dimension levels before inserting them
  dimension_levels_table <- prepare_dimension_levels_table(con, config, schema)
  insert_results$dimension_levels <- UMARimportR::insert_new_dimension_levels(
    con, dimension_levels_table, schema)
  message("Dimension levels insert: ", insert_results$dimension_levels$count, " rows")
  # prepare and insert series table
  series_table <- prepare_series_table(con, config, schema)
  insert_results$series <- UMARimportR::insert_new_series(con, series_table, schema)
  message("Series insert: ", insert_results$series$count, " rows")
  # prepare and insert series levels table
  series_levels_table <- prepare_series_levels_table(con, config, schema)
  insert_results$series_levels <- UMARimportR::insert_new_series_levels(
    con, series_levels_table, schema)
  message("Series levels insert: ", insert_results$series_levels$count, " rows")
  invisible(insert_results)
}



#' Insert data points from DESEZONIRANJE
#'
#' Function to prepare and insert DESEZONIRANJE data points. The function first prepares
#' the required vintages and inserts them, then prepares the data points
#' table and inserts it. The function returns the results invisibly.
#'
#' This is a DESEZONIRANJE specific function, which should be followed by the generic
#' UMARimportR function to write the vintage hashes and clean up redundant
#' vintages.
#'
#' @param con Database connection
#' @param config List of configurations. Defaults to desezoniranje_config.
#' @param schema Schema name, defaults to "platform"
#'
#' @return Insertion results (invisibly)
#' @export
DESEZ_import_data_points <- function(con,
                                     config = desezoniranje_config,
                                     schema = "platform") {
  message("Importing data points from DESEZONIRANJE into schema ", schema)
  # Collect outputs into one result list
  result <- list()
  # Get file paths with timestamps
  message("Finding most recent files...")
  file_paths_df <- get_all_recent_files(config = config)
  if (nrow(file_paths_df) == 0) {
    warning("No files found, exiting.")
    return(invisible(NULL))}
  # Prepare vintage table
  message("Preparing vintages...")
  vintage_table <- prepare_vintage_table(file_paths_df, con, config, schema)
  if (nrow(vintage_table) == 0) {
    warning("No new vintages to insert, exiting.")
    return(invisible(NULL))}
  # Insert vintages
  message("Inserting ", nrow(vintage_table), " vintage records...")
  result$vintages <- UMARimportR::insert_new_vintage(con, vintage_table, schema)
  # Extract data from Excel files
  message("Extracting data from Excel files...")
  data <- extract_all_desezoniranje_data(file_paths_df, config)
  # Prepare datapoint tables for insertion
  message("Preparing datapoint tables...")
  datapoint_tables <- purrr::imap(data, ~{
    prepare_datapoint_table(.x, .y, con, config, schema)})
  # Insert data points
  message("Inserting datapoints for ", length(datapoint_tables), " tables...")
  result$datapoints <- purrr::map(datapoint_tables, ~{
    UMARimportR::insert_prepared_data_points(.x, con = con, schema = schema)})
  message("Import complete!")
  invisible(result)
}
