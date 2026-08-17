test_that("list_assignments returns available assignment files", {
  assignments <- list_assignments()
  expect_type(assignments, "character")
  expect_true(length(assignments) > 0)
  expect_true("01_Intro.R" %in% assignments)
  expect_true("02_BaseR.R" %in% assignments)
})

test_that("list_solutions returns available solution files", {
  solutions <- list_solutions()
  expect_type(solutions, "character")
  expect_true(length(solutions) > 0)
  expect_true("03_Functions_answers.R" %in% solutions)
})

test_that("copy_assignments copies assignment files to target directory", {
  temp_dir <- file.path(tempdir(), "test_assignments_copy")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  copied <- copy_assignments(path = temp_dir, overwrite = TRUE)

  expect_type(copied, "character")
  expect_true(file.exists(file.path(temp_dir, "01_Intro.R")))
  expect_true(file.exists(file.path(temp_dir, "02_BaseR.R")))
})

test_that("copy_solutions copies solution files to target directory", {
  temp_dir <- file.path(tempdir(), "test_solutions_copy")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  copied <- copy_solutions(path = temp_dir, overwrite = TRUE)

  expect_type(copied, "character")
  expect_true(file.exists(file.path(temp_dir, "03_Functions_answers.R")))
})

test_that("find_package_dir finds assignments and solutions folders", {
  assign_dir <- find_package_dir("assignments")
  expect_true(dir.exists(assign_dir))

  sol_dir <- find_package_dir("solutions")
  expect_true(dir.exists(sol_dir))

  expect_error(find_package_dir("nonexistent_folder_xyz"))
})

test_that("copy_package_dir creates directory and copies files recursively", {
  temp_dir <- file.path(tempdir(), "test_pkg_dir_copy")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  copied <- copy_package_dir("solutions", path = temp_dir, overwrite = TRUE)

  expect_type(copied, "character")
  expect_true(dir.exists(temp_dir))
  expect_true(length(copied) > 0)
})
