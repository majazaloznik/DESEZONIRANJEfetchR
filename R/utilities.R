#' Find most recent Excel file in nested date folders
#'
#' Navigates through year and month/quarter folder structures to locate
#' the most recent Excel file matching a given pattern. Used to find
#' DESEZONIRANJE files in the standardized O:/DESEZONIRANJE folder structure.
#'
#' @param base_path Character string. Base directory path containing year folders.
#' @param file_pattern Character string. Regex pattern to match Excel filenames.
#'   Pattern is anchored at the start and ".xls$" is appended automatically.
#' @param year_pattern Character string. Regex pattern to identify year folders
#'   (e.g., "Leto \\\\d{4}" or "\\\\d{4}").
#' @param month_pattern Character string. Regex pattern to identify month or
#'   quarter folders (e.g., "\\\\d{2} \\\\d{4}" or "Q\\\\d \\\\d{4}").
#'
#' @return Character string. Full path to the most recent matching Excel file.
#'
#' @details
#' The function assumes a folder structure: base_path/year_folder/month_folder/file.xls
#' It selects the lexicographically maximum year and month folders (which works
#' for standard date formats), then finds files matching the pattern.
#'
#' Stops with error if no matching files found. Issues warning if multiple
#' files match the pattern (returns first match).
#'
#' @examples
#' \dontrun{
#' find_most_recent_file(
#'   base_path = "O:/DESEZONIRANJE/Trg dela/ILO/Brezposelni",
#'   file_pattern = "^ILO brezposelni",
#'   year_pattern = "\\d{4}",
#'   month_pattern = "Q\\d \\d{4}"
#' )
#' }
#'
#' @keywords internal

find_most_recent_file <- function(base_path, file_pattern, year_pattern, month_pattern) {
  # Check base path exists
  if (!dir.exists(base_path)) {
    stop("Base path does not exist: ", base_path)
  }

  # Find most recent year folder
  year_dirs <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)
  year_dirs <- year_dirs[grepl(year_pattern, year_dirs)]

  if (length(year_dirs) == 0) {
    stop("No year folders matching pattern '", year_pattern, "' found in ", base_path)
  }

  latest_year <- max(year_dirs)

  # Find most recent month folder within that year
  year_path <- file.path(base_path, latest_year)
  month_dirs <- list.dirs(year_path, full.names = FALSE, recursive = FALSE)
  month_dirs <- month_dirs[grepl(month_pattern, month_dirs)]

  if (length(month_dirs) == 0) {
    stop("No month folders matching pattern '", month_pattern, "' found in ", year_path)
  }

  latest_month <- max(month_dirs)

  # Find matching file
  target_dir <- file.path(year_path, latest_month)
  files <- list.files(target_dir, pattern = paste0(file_pattern, ".*\\.xls$"), full.names = TRUE)

  if (length(files) == 0) {
    stop("No matching files found in ", target_dir)
  }

  if (length(files) > 1) {
    warning("Multiple files match pattern, using first: ", files[1])
  }

  files[1]
}


#' Get all recent DESEZONIRANJE file paths
#'
#' Finds the most recent Excel file for each data source configured in
#' `desezoniranje_config`. Uses `find_most_recent_file()` for each entry
#' with graceful error handling - failures are reported as warnings but
#' don't stop execution.
#'
#' @param config List of configurations. Defaults to `desezoniranje_config`.
#'   Primarily for testing purposes.
#'
#' @return Named character vector of file paths. Names correspond to entries
#'   in `desezoniranje_config`. Failed lookups are omitted from the result
#'   and reported via warning.
#'
#' @details
#' This function wraps `find_most_recent_file()` with `purrr::safely()` to
#' handle network drive issues or missing files. If any lookups fail, a
#' warning lists the failed table names, but successful lookups are still
#' returned.
#'
#' @examples
#' \dontrun{
#' file_paths <- get_all_recent_files()
#' names(file_paths)  # Shows which data sources were successfully located
#' }
#'
#' @seealso [find_most_recent_file()]
#'
#' @export
get_all_recent_files <- function(config = desezoniranje_config) {
  safely_find_file <- purrr::safely(find_most_recent_file)

  results <- purrr::map(config, ~{
    safely_find_file(
      base_path = .x$base_path,
      file_pattern = .x$file_pattern,
      year_pattern = .x$year_folder_pattern,
      month_pattern = .x$month_folder_pattern
    )
  })

  # Extract successful results and report failures
  successes <- purrr::map(results, "result")
  errors <- purrr::map(results, "error")

  # Filter out NULL results (failed lookups)
  has_error <- !purrr::map_lgl(errors, is.null)

  if (any(has_error)) {
    failed_tables <- names(config)[has_error]
    warning("Failed to find files for: ", paste(failed_tables, collapse = ", "))
  }

  # Return only successful results
  successes <- successes[!has_error]
  purrr::map_chr(successes, identity)
}
