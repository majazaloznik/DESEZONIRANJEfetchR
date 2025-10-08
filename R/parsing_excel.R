#' Extract all DESEZONIRANJE data from Excel files
#'
#' Reads data from all configured DESEZONIRANJE Excel files, extracting both
#' SA (seasonally adjusted) and Orig (original) sheets. Data from multiple
#' files sharing the same table_id are joined on period_id.
#'
#' @param file_paths_df Data frame with source_name, file_path, and file_mtime columns,
#'   typically from `get_all_recent_files()`. set as a separate parameter for testing
#'   purposes
#' @param config List of configurations. Defaults to `desezoniranje_config`.
#'   Primarily for testing purposes.
#'
#' @return Named list of data frames. Names are in format "table_id_sheet"
#'   (e.g., "DA_SA", "BP_Orig"). Each data frame contains period_id and
#'   value columns as specified in the configuration.
#'
#' @examples
#' \dontrun{
#' file_paths <- get_all_recent_files()
#' all_data <- extract_all_desezoniranje_data(file_paths)
#' names(all_data)  # Shows all table_id_sheet combinations
#' }
#'
#' @export
extract_all_desezoniranje_data <- function(file_paths_df, config = desezoniranje_config) {
  file_paths <- purrr::set_names(file_paths_df$file_path, file_paths_df$source_name)

  # Create extraction grid
  extraction_grid <- tidyr::crossing(
    file_name = names(file_paths),
    sheet = c("SA", "Orig")
  ) |>
    dplyr::mutate(
      file_path = file_paths[file_name],
      table_config = purrr::map(file_name, ~.env$config[[.x]])  # Force function parameter
    )

  # Extract all data
  all_extracted <- extraction_grid |>
    dplyr::mutate(
      data = purrr::pmap(list(file_path, table_config, sheet), extract_single_sheet),
      table_id = purrr::map_chr(table_config, "table_id")
    )

  # Group by table_id + sheet and join on period_id
  all_extracted |>
    dplyr::mutate(combined_id = paste(table_id, sheet, sep = "_")) |>
    dplyr::group_by(combined_id) |>
    dplyr::summarise(
      combined_data = list(join_on_period(data)),
      .groups = "drop"
    ) |>
    tibble::deframe()
}

#' Join multiple data frames on period_id
#'
#' Performs iterative full joins on a list of data frames using period_id
#' as the key. Used internally to combine data from multiple files that
#' share the same table_id.
#'
#' @param data_list List of data frames, each containing a period_id column
#'   and one or more value columns.
#'
#' @return Single data frame with period_id and all value columns from
#'   input data frames. Rows are matched on period_id using full join,
#'   so all periods from all inputs are preserved.
#'
#' @keywords internal
join_on_period <- function(data_list) {
  purrr::reduce(data_list, ~{
    dplyr::full_join(.x, .y, by = "period_id")
  })
}

#' Extract single sheet from DESEZONIRANJE Excel file
#'
#' Reads a specific sheet from an Excel file, skipping the first row and
#' renaming columns according to the configuration's column_codes.
#'
#' @param file_path Character string. Path to Excel file.
#' @param config List. Configuration entry from `desezoniranje_config`
#'   containing column_codes and other metadata.
#' @param sheet Character string. Name of sheet to read ("SA" or "Orig").
#'
#' @return Data frame with columns renamed according to config$column_codes.
#'   First column is always period_id, followed by value columns.
#'
#' @keywords internal
extract_single_sheet <- function(file_path, config, sheet) {
  suppressMessages({
    raw_data <- readxl::read_excel(file_path, sheet = sheet, skip = 1)
  })

  raw_data[, 1:length(config$column_codes)] |>
    purrr::set_names(config$column_codes) |>
    dplyr::mutate(period_id = format_period_id(period_id, config$interval))
}


