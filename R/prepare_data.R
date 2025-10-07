#' Prepare table to insert into `vintage` table
#'
#' Helper function that populates the vintage table with the new vintages. It gets
#' the series ids from the database and adds the publication date from the file
#' modification time. Checks each source file individually to determine if new
#' vintages are needed, rather than checking at the table level, since multiple
#' source files can contribute to a single database table.
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
#' @return A dataframe with `series_id` and `published` columns for all series
#'   across tables that have new source data. Returns empty dataframe if no
#'   sources have new data.
#'
#' @details
#' The function checks each source file individually by examining a representative
#' series from that source. This is necessary because multiple source files can
#' contribute to a single database table (e.g., the DA table has three source files).
#' Only sources with file modification times newer than their last published vintage
#' are processed. Sources are then grouped by table_id to create vintage records
#' for all series in affected tables.
#'
#' @export
prepare_vintage_table <- function(file_paths_df,
                                  con,
                                  config = desezoniranje_config,
                                  schema = "platform") {

  # Get all source names that need checking
  source_names <- names(config)

  # Check each source file individually
  sources_to_process <- purrr::keep(source_names, ~{
    source_name <- .x
    source_config <- config[[source_name]]
    table_code <- source_config$table_id

    # Get file modification time for THIS specific source
    file_mtime <- file_paths_df |>
      dplyr::filter(source_name == !!source_name) |>
      dplyr::pull(file_mtime)

    if (length(file_mtime) == 0) {
      message("Source ", source_name, " not found in file paths, skipping.")
      return(FALSE)
    }

    # Build a representative series code from this source
    # Use first column code (not period_id)
    first_col <- source_config$column_codes[2]
    interval <- source_config$interval

    # Check both SA and Orig versions
    series_codes <- c(
      paste("DESEZ", table_code, first_col, "Y", interval, sep = "--"),
      paste("DESEZ", table_code, first_col, "N", interval, sep = "--")
    )

    # Get last published date from either series
    last_published <- NULL
    for (series_code in series_codes) {
      vintage_id <- UMARaccessR::sql_get_vintage_from_series_code(con, series_code, schema = schema)
      if (!is.na(vintage_id) && !is.null(vintage_id)) {
        last_published <- UMARaccessR::sql_get_date_published_from_vintage(vintage_id, con, schema)
        break
      }
    }

    # Compare dates (truncate to avoid timezone issues)
    file_date <- as.Date(file_mtime)
    last_published_date <- if (!is.null(last_published)) as.Date(last_published) else NULL

    is_new <- is.null(last_published_date) || file_date > last_published_date

    if (!is_new) {
      message("Source ", source_name, " (table ", table_code, ") has no new data (last published: ",
              last_published_date, ", file modified: ", file_date, "), skipping.")
    }

    is_new
  })

  if (length(sources_to_process) == 0) {
    message("No sources have new data.")
    return(data.frame(series_id = integer(0), published = as.POSIXct(character(0))))
  }

  # Group sources by table_id and prepare vintages
  sources_by_table <- split(sources_to_process,
                            purrr::map_chr(config[sources_to_process], "table_id"))

  purrr::map_dfr(names(sources_by_table), ~{
    table_code <- .x
    source_names_for_table <- sources_by_table[[.x]]

    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, table_code, schema)

    # Get the most recent file time across all sources for this table
    published <- file_paths_df |>
      dplyr::filter(source_name %in% source_names_for_table) |>
      dplyr::pull(file_mtime) |>
      max()

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

