#' Prepare table to insert into `vintage` table
#'
#' Helper function that populates the vintage table with the new vintages. It gets
#' the series ids from the database and adds the publication date from the file
#' modification time.
#'
#' Returns table ready to insert into the `vintage` table with the
#' UMARimportR::insert family of functions.
#'
#' @param file_paths_df Data frame with source_name, file_path, and file_mtime columns,
#'   typically from `get_all_recent_files()`
#' @param con Connection to the database
#' @param config List of configurations. Defaults to `desezoniranje_config`.
#' @param schema Schema to use for the connection, default is "platform"
#'
#' @return A dataframe with `series_id` and `published` columns
#'   for all series across all tables.
#' @export
prepare_vintage_table <- function(file_paths_df,
                                  con,
                                  config = desezoniranje_config,
                                  schema = "platform") {
  # Get unique table_ids
  unique_tables <- config |>
    purrr::map_chr("table_id") |>
    unique()

  # For each table, prepare vintages
  purrr::map_dfr(unique_tables, ~{
    table_code <- .x

    # Get table_id from database
    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, table_code, schema)

    # Get all config entries for this table_id to find associated source files
    table_config_entries <- config[purrr::map_chr(config, "table_id") == table_code]
    source_names <- names(table_config_entries)

    # Get the most recent file_mtime for this table (across all its sources)
    published <- file_paths_df |>
      dplyr::filter(source_name %in% source_names) |>
      dplyr::pull(file_mtime) |>
      max()

    # Check if this is a new vintage
    last_published <- UMARaccessR::sql_get_last_publication_date_from_table_id(tbl_id, con, schema)

    if (!is.null(last_published) && published <= last_published) {
      warning(paste0("Vintages for table ", table_code,
                     " are not newer than last publication, skipping."))
      return(data.frame(series_id = integer(0), published = as.POSIXct(character(0))))
    }

    # Get all series for this table
    series_ids <- UMARaccessR::sql_get_series_ids_from_table_id(tbl_id, con, schema)

    data.frame(
      series_id = series_ids$id,
      published = published,
      stringsAsFactors = FALSE
    )
  })
}



#' Prepare datapoint table for insertion
#'
#' Prepare the data points for insertion into the database. Converts wide format
#' (multiple value columns) to long format with series codes.
#'
#' @param df Data frame with period_id and value columns
#' @param table_name Character string, the name from extracted_data (e.g., "BP_SA")
#' @param con Connection to the database
#' @param config List of configurations. Defaults to desezoniranje_config.
#' @param schema Schema name
#'
#' @return A list with data, table_id, dimension_names, dimension_ids, and interval_id
#' @export
prepare_datapoint_table <- function(df, table_name, con, config = desezoniranje_config, schema = "platform") {

  # Parse table_name to get table_code and SA status
  parts <- stringr::str_split(table_name, "_")[[1]]
  table_code <- parts[1]
  sa_status <- parts[2]  # "SA" or "Orig"
  sa_code <- if (sa_status == "SA") "Y" else "N"

  # Get table_id
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, table_code, schema)

  # Get interval from config
  table_configs <- config[purrr::map_chr(config, "table_id") == table_code]
  interval <- table_configs[[1]]$interval

  # Get dimension IDs
  meritev_dim_id <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(
    tbl_id, "Meritev", con, schema
  )
  sa_dim_id <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(
    tbl_id, "Seasonally adjusted", con, schema
  )

  # Convert to long format
  df_long <- df |>
    tidyr::pivot_longer(
      cols = -period_id,
      names_to = "Meritev",
      values_to = "value"
    ) |>
    dplyr::mutate(
      `Seasonally adjusted` = sa_code,
      time = period_id,
      flag = ""
    ) |>
    dplyr::select(time, Meritev, `Seasonally adjusted`, value, flag)

  names(df_long) <- make.names(names(df_long))
  list(
    data = df_long,
    table_id = tbl_id,
    dimension_names = c("Meritev", "Seasonally adjusted"),
    dimension_ids = c(meritev_dim_id, sa_dim_id),
    interval_id = interval
  )
}

