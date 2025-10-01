#' DESEZONIRANJE data source configurations
#'
#' Configuration for 8 data sources from the DESEZONIRANJE network folder.
#' Each configuration defines paths, patterns, and metadata needed to locate
#' and extract the most recent data files.
#'
#' @format A named list with 8 elements, each containing:
#' \describe{
#'   \item{category}{Which category it belongs to}
#'   \item{base_path}{Base directory path on network drive}
#'   \item{file_pattern}{Regex pattern for matching Excel files}
#'   \item{year_folder_pattern}{Pattern for identifying year folders}
#'   \item{month_folder_pattern}{Pattern for identifying month/quarter folders}
#'   \item{table_id}{Unique identifier for database table}
#'   \item{table_name}{Description of table}
#'   \item{expected_columns}{Character vector of expected column names}
#'   \item{column_codes}{Code for column}
#'   \item{description}{Human-readable description of data source}
#'   \item{interval}{Interval, monthly (M) or quartery(Q)}
#'   \item{unit}{name of unit}
#' }
#'
#' @examples
#' # List all available data sources
#' names(desezoniranje_config)
#'
#' # Get configuration for specific table
#' desezoniranje_config$registrirani_brezposelni_stopnje
"desezoniranje_config"
