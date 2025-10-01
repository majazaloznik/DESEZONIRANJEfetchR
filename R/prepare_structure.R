#' Prepare table to insert into `source` table

#' Helper function that manually prepares the new line for the source table.
#'
#' @param con connection to the database.
#' @param schema the schema to use for the connection, default is "platform"
#' @return a dataframe with the `name`, `name_long` columns.
#' for this table.
#' @export

prepare_source_table <- function(con, schema = "platform") {
  DBI::dbExecute(con, paste0("set search_path to ", schema))
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "DESEZ", schema)
  if (is.null(source_id)){
    id <- dplyr::tbl(con, "source") |>
      dplyr::summarise(max = max(id, na.rm = TRUE)) |>
      dplyr::pull() + 1
    data.frame(id = id,
               name = "DESEZ",
               name_long = "Umarjeve desezonirane serije",
               url = NA)} else {
                 message("DESEZ already listed in the source table.")}
}


#' Prepare table to insert into `table` table
#'
#' Helper function that prepares the table metadata from desezoniranje_config.
#' Returns a list of tables ready to insert into the `table` table with the
#' db_writing family of functions from `UMARimportR`.
#'
#' @param con Connection to the database
#' @param config List of configurations. Defaults to `desezoniranje_config`.
#' @param schema Schema name, defaults to "platform"
#' @param keep_vintage Logical indicating whether to keep vintages, defaults to FALSE
#'
#' @return A list of dataframes, one for each unique table_id in the config.
#'   Each dataframe contains `code`, `name`, `source_id`, `url`, and `notes` columns.
#'
#' @export
prepare_table_table <- function(con,
                                config = desezoniranje_config,
                                schema = "platform",
                                keep_vintage = FALSE) {

  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "DESEZ", schema)
  # Extract unique table_id and table_name combinations
  table_info <- config |>
    purrr::map_dfr(~{
      data.frame(code = .x$table_id,
                 name = .x$table_name,
                 stringsAsFactors = FALSE)}) |>
    dplyr::distinct(code, name)
  # Create table metadata
  table_metadata <- table_info |>
    dplyr::mutate(source_id = source_id,
                  url = "",
                  notes = NA_character_,
                  keep_vintage = keep_vintage)
  # Split into list by table code for easy iteration
  table_metadata |>
    dplyr::group_by(code) |>
    dplyr::group_split() |>
    purrr::set_names(table_metadata$code)}


#' Prepare table to insert into `category` table
#'
#' Helper function that manually prepares the category table with field ids and
#' their names. Returns table ready to insert into the `category` table with the db_writing family
#' of functions from `UMARimportR`.
#'
#' @param con connection to the database
#' @param config List of configurations. Defaults to `desezoniranje_config`.
#' @param schema schema name
#'
#' @return a dataframe with the `id`, `name`, `source_id` for each all the categories
#' used to describe the DESEZONIRANJE ecosystem
#' @export
#'
prepare_category_table <- function(con, config = desezoniranje_config, schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "DESEZ", schema)
  table_info <- config |>
    purrr::map_dfr(~{
      data.frame(name = .x$table_name,
                 stringsAsFactors = FALSE)}) |>
    dplyr::distinct(name)
  # Create table metadata
  table_metadata <- table_info |>
    dplyr::mutate(source_id = source_id,
                  id = dplyr::row_number()) |>
    dplyr::bind_rows(data.frame(source_id = source_id, name = "DESEZONIRANJE", id = 0))

}



#' Prepare table to insert into `category_relationship` table
#'
#' Helper function that manually prepares the category_relationship table.
#' Returns table ready to insert into the `category_relationship` table with the db_writing family
#' of functions from `UMARimportR`.
#'
#' @param con connection to the database
#' @param config List of configurations. Defaults to `desezoniranje_config`.
#' @param schema schema name
#'
#' @return a dataframe with the `id`, `parent_id`, `source_id` for each relationship
#' betweeh categories
#' @export
#'
prepare_category_relationship_table <- function(con,
                                                config = desezoniranje_config,
                                                schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "DESEZ", schema)
  cat_names <- desezoniranje_config |>
    purrr::map_chr("table_name") |>
    unique()
  category_ids <- purrr::map_int(cat_names, ~{
    UMARaccessR::sql_get_category_id_from_name(.x, con, source_id, schema)})
  data.frame(id = category_ids,
             parent_id = 0,
             source_id = source_id)
}


#' Prepare table to insert into `category_table` table
#'
#' Helper function that manually prepares the category_table table.
#' Returns table ready to insert into the `category_table` table with the db_writing family
#' of functions from `UMARimportR`.
#'
#' @param con connection to the database
#' @param config List of configurations. Defaults to `desezoniranje_config`.
#' @param schema schema name
#'
#' @return a dataframe with the `category_id` `table_id` and `source_id` columns for
#' each table-category relationship.
#' @export
#' @importFrom stats na.omit
prepare_category_table_table <- function(con, config = desezoniranje_config, schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "DESEZ", schema)
  config |>
    purrr::map_dfr(~{
      data.frame(table_code = .x$table_id,
                 cat_names = .x$table_name,
                 stringsAsFactors = FALSE)}) |>
    dplyr::distinct(table_code, cat_names) |>
    dplyr::rowwise() |>
    dplyr::mutate(category_id = UMARaccessR::sql_get_category_id_from_name(cat_names, con, source_id, schema),
          table_id = UMARaccessR::sql_get_table_id_from_table_code(con, table_code, schema),
          source_id = source_id) |>
    dplyr::select(-table_code, -cat_names)
}


#' Prepare table to insert into `table_dimensions` table
#'
#' Helper function that manually prepares the table_dimensions table.
#' Returns table ready to insert into the `table_dimensions`table with the
#' db_writing family of functions.
#'
#' @param con connection to the database
#' @param config List of configurations. Defaults to `desezoniranje_config`.
#' @param schema schema name
#'
#' @return a dataframe with the `table_id`, `dimension_name`, `time` columns for
#' each dimension of this table.
#' @export

prepare_table_dimensions_table <- function(con, config = desezoniranje_config, schema = "platform"){
  config |>
    purrr::map_dfr(~{
      data.frame(table_code = .x$table_id,
                stringsAsFactors = FALSE)}) |>
    dplyr::distinct(table_code) |>
    dplyr::rowwise() |>
    dplyr::mutate(table_id = UMARaccessR::sql_get_table_id_from_table_code(con, table_code, schema)) |>
    dplyr::mutate(dimension = "Meritev",
                  is_time = rep(FALSE)) |>
    dplyr::select(-table_code) |>
    dplyr::arrange(table_id)

}



#' Prepare table to insert into `dimension_levels` table
#'
#' Helper function that manually prepares the dimension_levels for each
#' table and gets their codes and text from desezoniranje_config.
#' Returns a list of tables ready to insert into the `dimension_levels` table
#' with the db_writing family of functions.
#'
#' @param con Connection to the database
#' @param config List of configurations. Defaults to `desezoniranje_config`.
#' @param schema Schema name, defaults to "platform"
#'
#' @return A list of dataframes, one for each unique table_id. Each dataframe
#'   contains `tab_dim_id`, `level_value`, and `level_text` columns.
#' @export
#' @importFrom stats na.omit
prepare_dimension_levels_table <- function(con,
                                           config = desezoniranje_config,
                                           schema = "platform") {

  unique_tables <- config |>
    purrr::map_chr("table_id") |>
    unique()

  # For each unique table_id, prepare dimension levels
  purrr::map_dfr(unique_tables, ~{
    table_code <- .x
    table_id <- UMARaccessR::sql_get_table_id_from_table_code(
      con, table_code, schema)
    # Get dimension_id for "Meritev"
    dim_id <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(
      table_id, "Meritev", con, schema)
    # Get all config entries for this table_id
    table_configs <- config[purrr::map_chr(config, "table_id") == table_code]
    # Extract level_value and level_text from each config entry
    levels_data <- purrr::map_dfr(table_configs, ~{
      # Skip first element (period_id) in column_codes
      level_values <- .x$column_codes[-1]
      level_texts <- .x$expected_columns # Already correct length

      data.frame(
        level_value = level_values,
        level_text = level_texts,
        stringsAsFactors = FALSE
      )
    }) |>
      dplyr::distinct(level_value, level_text)  # Remove duplicates if any
    # Add dimension_id
    levels_data |>
      dplyr::mutate(tab_dim_id = dim_id) |>
      dplyr::select(tab_dim_id, level_value, level_text)
  })
}


#' Prepare table to insert into `series` table
#'
#' Prepares series metadata for all tables in desezoniranje_config.
#' Creates one series for each unique table_id and column_code combination
#' (excluding period_id).
#'
#' @param con Connection to the database
#' @param config List of configurations. Defaults to `desezoniranje_config`.
#' @param schema Schema name, defaults to "platform"
#'
#' @return A dataframe with columns: `table_id`, `name_long`, `code`,
#'   `unit_id`, and `interval_id` for each series.
#' @export
prepare_series_table <- function(con,
                                 config = desezoniranje_config,
                                 schema = "platform") {
  unique_tables <- config |>
    purrr::map_chr("table_id") |>
    unique()

  # For each unique table_id, prepare series
  purrr::map_dfr(unique_tables, ~{
    table_code <- .x
    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(
      con, table_code, schema)
    table_configs <- config[purrr::map_chr(config, "table_id") == table_code]
    interval <- table_configs[[1]]$interval
    purrr::map_dfr(table_configs, ~{
      # Skip first element (period_id) in column_codes
      column_codes <- .x$column_codes[-1]
      expected_cols <- .x$expected_columns
      table_name <- .x$table_name

      # Get unit_id for THIS specific config entry
      unit_name <- .x$unit
      unit_id <- UMARaccessR::sql_get_unit_id_from_unit_name(
        unit_name, con, schema
      )

      data.frame(
        table_id = tbl_id,
        name_long = paste(table_name, expected_cols, sep = " - "),
        code = paste("DESEZ", table_code, column_codes, interval, sep = "--"),
        unit_id = unit_id,
        interval_id = interval,
        stringsAsFactors = FALSE
      )
    }) |>
      dplyr::distinct(code, .keep_all = TRUE)  # Remove any duplicates
  })
}

#' Prepare table to insert into `series_levels` table
#'
#' Helper function that extracts the individual levels for each series and
#' gets the correct dimension id for each one and the correct series id to
#' keep with the constraints.
#' Returns table ready to insert into the `series_levels`table with the
#' db_writing family of functions.
#'
#'

#' @param con connection to the database
#' @param config List of configurations. Defaults to `desezoniranje_config`.
#' @param schema schema name
#'
#' @return a dataframe with the `series_id`, `tab_dim_id`, `value` columns
#' all the series-level combinatins for this table.
#' @export
#'
prepare_series_levels_table <- function(con,
                                        config = desezoniranje_config,
                                        schema = "platform") {  # Add default!
  unique_tables <- config |>
    purrr::map_chr("table_id") |>
    unique()

  purrr::map_dfr(unique_tables, ~{
    table_code <- .x
    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, table_code, schema)

    dimz <- UMARaccessR::sql_get_dimensions_from_table_id(tbl_id, con, schema) |>
      dplyr::filter(is_time != TRUE) |>
      dplyr::pull(id)

    UMARaccessR::sql_get_series_from_table_id(tbl_id, con, schema) |>
      dplyr::filter(table_id == tbl_id) |>
      dplyr::select(table_id, id, code) |>
      tidyr::separate(code, into = c("x1", "x2", paste0(dimz), "int"), sep = "--") |>
      dplyr::select(series_id = id, dplyr::all_of(paste0(dimz))) |>
      tidyr::pivot_longer(-series_id, names_to = "tab_dim_id") |>
      dplyr::rename(level_value = value) |>
      dplyr::mutate(tab_dim_id = as.integer(tab_dim_id)) |>
      as.data.frame()
  })
}
