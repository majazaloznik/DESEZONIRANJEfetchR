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

  expect_match(result, "07 2025.*BP2024_2008.*\\.xls$")
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

  expect_match(result, "Q2 2025.*ILO brezposelni\\.xls$")
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

test_that("get_all_recent_files uses desezoniranje_config", {
  # This will actually try to access the network paths in the real config
  # Skip if not on the network
  skip_if_not(dir.exists("O:/DESEZONIRANJE"), "Network drive not available")

  result <- get_all_recent_files()

  expect_type(result, "character")
  expect_named(result)
  expect_true(all(file.exists(result)))
})
