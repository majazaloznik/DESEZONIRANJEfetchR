# tests/testthat/test-path_utils.R

test_that("find_most_recent_file finds correct file in nested structure", {
  # Create temporary directory structure
  base_dir <- withr::local_tempdir()

  # Create year folders
  dir.create(file.path(base_dir, "Leto 2024"))
  dir.create(file.path(base_dir, "Leto 2025"))

  # Create month folders in 2025
  dir.create(file.path(base_dir, "Leto 2025", "05 2025"))
  dir.create(file.path(base_dir, "Leto 2025", "06 2025"))
  dir.create(file.path(base_dir, "Leto 2025", "07 2025"))

  # Create test files
  file.create(file.path(base_dir, "Leto 2025", "07 2025", "BP2024_2008 (julij 2025).xls"))
  file.create(file.path(base_dir, "Leto 2025", "06 2025", "BP2024_2008 (junij 2025).xls"))

  result <- find_most_recent_file(
    base_path = base_dir,
    file_pattern = "^BP\\d{4}_\\d{4}",
    year_pattern = "Leto \\d{4}",
    month_pattern = "\\d{2} \\d{4}"
  )

  expect_match(result$path, "07 2025.*BP2024_2008.*\\.xls$")
})

test_that("find_most_recent_file works with quarterly structure", {
  base_dir <- withr::local_tempdir()

  dir.create(file.path(base_dir, "2024"))
  dir.create(file.path(base_dir, "2025"))
  dir.create(file.path(base_dir, "2025", "Q1 2025"))
  dir.create(file.path(base_dir, "2025", "Q2 2025"))

  file.create(file.path(base_dir, "2025", "Q2 2025", "ILO brezposelni.xls"))

  result <- find_most_recent_file(
    base_path = base_dir,
    file_pattern = "^ILO brezposelni",
    year_pattern = "\\d{4}",
    month_pattern = "Q\\d \\d{4}"
  )

  expect_match(result$path, "Q2 2025.*ILO brezposelni\\.xls$")
})

test_that("find_most_recent_file errors when no files match", {
  base_dir <- withr::local_tempdir()
  dir.create(file.path(base_dir, "2025"))
  dir.create(file.path(base_dir, "2025", "Q1 2025"))

  expect_error(
    find_most_recent_file(
      base_path = base_dir,
      file_pattern = "^NonExistent",
      year_pattern = "\\d{4}",
      month_pattern = "Q\\d \\d{4}"
    ),
    "No matching files found"
  )
})

test_that("find_most_recent_file warns with multiple matching files", {
  base_dir <- withr::local_tempdir()
  dir.create(file.path(base_dir, "2025"))
  dir.create(file.path(base_dir, "2025", "Q1 2025"))

  file.create(file.path(base_dir, "2025", "Q1 2025", "BP2024_2008 (a).xls"))
  file.create(file.path(base_dir, "2025", "Q1 2025", "BP2024_2008 (b).xls"))

  expect_warning(
    find_most_recent_file(
      base_path = base_dir,
      file_pattern = "^BP\\d{4}_\\d{4}",
      year_pattern = "\\d{4}",
      month_pattern = "Q\\d \\d{4}"
    ),
    "Multiple files match pattern"
  )
})

test_that("get_all_recent_files returns named vector of file paths", {
  # Create temporary base directories for two mock data sources
  base_dir_1 <- withr::local_tempdir()
  base_dir_2 <- withr::local_tempdir()

  # Create structure for source 1 (monthly pattern)
  dir.create(file.path(base_dir_1, "Leto 2024"))
  dir.create(file.path(base_dir_1, "Leto 2025"))
  dir.create(file.path(base_dir_1, "Leto 2025", "06 2025"))
  dir.create(file.path(base_dir_1, "Leto 2025", "07 2025"))
  file.create(file.path(base_dir_1, "Leto 2025", "07 2025", "BP2024_2008.xls"))

  # Create structure for source 2 (quarterly pattern)
  dir.create(file.path(base_dir_2, "2024"))
  dir.create(file.path(base_dir_2, "2025"))
  dir.create(file.path(base_dir_2, "2025", "Q1 2025"))
  dir.create(file.path(base_dir_2, "2025", "Q2 2025"))
  file.create(file.path(base_dir_2, "2025", "Q2 2025", "ILO brezposelni.xls"))

  # Mock config
  mock_config <- list(
    source_1 = list(
      base_path = base_dir_1,
      file_pattern = "^BP\\d{4}_\\d{4}",
      year_folder_pattern = "Leto \\d{4}",
      month_folder_pattern = "\\d{2} \\d{4}"
    ),
    source_2 = list(
      base_path = base_dir_2,
      file_pattern = "^ILO brezposelni",
      year_folder_pattern = "\\d{4}",
      month_folder_pattern = "Q\\d \\d{4}"
    )
  )

  result <- get_all_recent_files(config = mock_config)

  expect_type(result$file_path, "character")
  expect_true(all(file.exists(result$file_path)))
  expect_match(result$file_path[1], "07 2025.*BP2024_2008\\.xls$")
  expect_match(result$file_path[2], "Q2 2025.*ILO brezposelni\\.xls$")
})

test_that("get_all_recent_files handles partial failures gracefully", {
  # Create one valid and one invalid base path
  base_dir_valid <- withr::local_tempdir()
  dir.create(file.path(base_dir_valid, "2025"))
  dir.create(file.path(base_dir_valid, "2025", "Q1"))
  file.create(file.path(base_dir_valid, "2025", "Q1", "test.xls"))

  mock_config <- list(
    valid_source = list(
      base_path = base_dir_valid,
      file_pattern = "^test",
      year_folder_pattern = "\\d{4}",
      month_folder_pattern = "Q\\d"
    ),
    invalid_source = list(
      base_path = file.path(withr::local_tempdir(), "nonexistent"),
      file_pattern = "^test",
      year_folder_pattern = "\\d{4}",
      month_folder_pattern = "Q\\d"
    )
  )

  expect_warning(
    result <- get_all_recent_files(config = mock_config),
    "Failed to find files for: invalid_source"
  )
  expect_length(result, 3)
  expect_true(file.exists(result$file_path[1]))
})

test_that("get_all_recent_files returns empty vector when all sources fail", {
  mock_config <- list(
    fail_1 = list(
      base_path = file.path(withr::local_tempdir(), "nonexistent1"),
      file_pattern = "^test",
      year_folder_pattern = "\\d{4}",
      month_folder_pattern = "Q\\d"
    ),
    fail_2 = list(
      base_path = file.path(withr::local_tempdir(), "nonexistent2"),
      file_pattern = "^test",
      year_folder_pattern = "\\d{4}",
      month_folder_pattern = "Q\\d"
    )
  )

  expect_warning(
    result <- get_all_recent_files(config = mock_config),
    "Failed to find files for: fail_1, fail_2"
  )
  expect_equal(nrow(result), 0)
})
