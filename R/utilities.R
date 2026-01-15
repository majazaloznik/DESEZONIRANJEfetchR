#' Find most recent Excel file in nested date folders
#'
#' Navigates through year and optionally month/quarter folder structures to locate
#' the most recent Excel file matching a given pattern. Used to find
#' DESEZONIRANJE files in the standardized O:/DESEZONIRANJE folder structure.
#'
#' @param base_path Character string. Base directory path containing year folders.
#' @param file_pattern Character string. Regex pattern to match Excel filenames.
#'   Pattern is anchored at the start and ".xls$" is appended automatically.
#' @param year_pattern Character string. Regex pattern to identify year folders
#'   (e.g., "Leto \\\\d{4}" or "\\\\d{4}").
#' @param month_pattern Character string or NULL. Regex pattern to identify month or
#'   quarter folders (e.g., "\\\\d{2} \\\\d{4}" or "Q\\\\d \\\\d{4}").
#'   Set to NULL for folders with no month subfolders.
#'
#' @return List with two elements: `path` (character string full path to the most
#'   recent matching Excel file) and `mtime` (POSIXct file modification time).
#'
#' @details
#' The function handles two folder structures:
#' 1. With month folders: base_path/year_folder/month_folder/file.xls
#' 2. Without month folders (month_pattern = NULL): base_path/year_folder/file.xls
#'
#' It selects the lexicographically maximum year and (if applicable) month folders,
#' then finds the most recent file matching the pattern based on modification time.
#'
#' Stops with error if no matching files found. When multiple files match, selects
#' the one with the most recent modification time.
#'
#' @examples
#' \dontrun{
#' # With month folders
#' find_most_recent_file(
#'   base_path = "O:/DESEZONIRANJE/Trg dela/ILO/Brezposelni",
#'   file_pattern = "^ILO brezposelni",
#'   year_pattern = "\\d{4}",
#'   month_pattern = "Q\\d \\d{4}"
#' )
#'
#' # Without month folders
#' find_most_recent_file(
#'   base_path = "O:/DESEZONIRANJE/Some/Path",
#'   file_pattern = "^DataFile",
#'   year_pattern = "\\d{4}",
#'   month_pattern = NULL
#' )
#' }
#'
#' @keywords internal
find_most_recent_file <- function(base_path, file_pattern, year_pattern, month_pattern = NULL) {
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
  year_path <- file.path(base_path, latest_year)

  # Determine target directory based on whether month folders exist
  if (!is.null(month_pattern)) {
    # Case 1: With month folders
    month_dirs <- list.dirs(year_path, full.names = FALSE, recursive = FALSE)
    month_dirs <- month_dirs[grepl(month_pattern, month_dirs)]

    if (length(month_dirs) == 0) {
      stop("No month folders matching pattern '", month_pattern, "' found in ", year_path)
    }

    latest_month <- max(month_dirs)
    target_dir <- file.path(year_path, latest_month)
  } else {
    # Case 2: No month folders - files directly in year folder
    target_dir <- year_path
  }

  # Find matching files
  files <- list.files(target_dir, pattern = paste0(file_pattern, ".*\\.xls$"), full.names = TRUE)

  if (length(files) == 0) {
    stop("No matching files found in ", target_dir)
  }

  # Select most recent file by modification time
  if (length(files) > 1) {
    file_times <- file.info(files)$mtime
    file_path <- files[which.max(file_times)]
    message("Multiple files found, selected most recent: ", basename(file_path))
  } else {
    file_path <- files[1]
  }

  # Return both path and timestamp
  list(
    path = file_path,
    mtime = file.info(file_path)$mtime
  )
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

  # Return only successful results as data frame
  successes <- successes[!has_error]

  data.frame(
    source_name = names(successes),
    file_path = purrr::map_chr(successes, "path"),
    file_mtime = purrr::map_dbl(successes, ~as.numeric(.x$mtime)),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(file_mtime = as.POSIXct(file_mtime, origin = "1970-01-01"))
}


#' Helper function to format period IDs
#'
#' converts date to period id
#'
#' @param dates date
#' @param interval M or Q
#'
#' @keywords internal
format_period_id <- function(dates, interval) {
  dates <- as.Date(dates)
  year <- format(dates, "%Y")

  if (interval == "M") {
    # Monthly: 2025M08
    month <- format(dates, "%m")
    paste0(year, "M", month)
  } else if (interval == "Q") {
    # Quarterly: 2025Q2
    quarter <- as.integer(format(dates, "%m"))
    quarter <- ceiling(quarter / 3)
    paste0(year, "Q", quarter)
  } else {
    stop("Unsupported interval: ", interval)
  }
}
