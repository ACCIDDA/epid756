#' @import tidyverse
#' @import deSolve
#' @import tableone
#' @import data.table
#' @import knitr
#' @import rmarkdown
NULL

#' Copy Course Assignments to a Local Directory
#'
#' Copies all exercise and assignment scripts (and nested data files) from the
#' package to a target directory of the user's choice.
#'
#' @param path Character string; target directory path where assignment files
#'   should be copied. Defaults to `"assignments"`.
#' @param overwrite Logical; whether to overwrite existing files in the target
#'   directory. Defaults to `FALSE`.
#'
#' @return Invisibly returns a character vector of copied destination file
#'   paths.
#' @export
#'
#' @examples
#' \dontrun{
#' copy_assignments(path = "my_assignments")
#' }
copy_assignments <- function(path = "assignments", overwrite = FALSE) {
  copy_package_dir(
    sub_dir = "assignments",
    path = path,
    overwrite = overwrite
  )
}

#' Copy Course Solutions to a Local Directory
#'
#' Copies solution scripts from the package to a target directory of the
#' user's choice.
#'
#' @param path Character string; target directory path where solution files
#'   should be copied. Defaults to `"solutions"`.
#' @param overwrite Logical; whether to overwrite existing files in the target
#'   directory. Defaults to `FALSE`.
#'
#' @return Invisibly returns a character vector of copied destination file
#'   paths.
#' @export
#'
#' @examples
#' \dontrun{
#' copy_solutions(path = "my_solutions")
#' }
copy_solutions <- function(path = "solutions", overwrite = FALSE) {
  copy_package_dir(
    sub_dir = "solutions",
    path = path,
    overwrite = overwrite
  )
}

#' List Available Course Assignments
#'
#' Lists all assignment scripts and files available in the package.
#'
#' @return Character vector of assignment file names.
#' @export
#'
#' @examples
#' list_assignments()
list_assignments <- function() {
  src_dir <- find_package_dir("assignments")
  list.files(src_dir, recursive = TRUE)
}

#' List Available Course Solutions
#'
#' Lists all solution scripts available in the package.
#'
#' @return Character vector of solution file names.
#' @export
#'
#' @examples
#' list_solutions()
list_solutions <- function() {
  src_dir <- find_package_dir("solutions")
  list.files(src_dir, recursive = TRUE)
}

# Internal helper to find inst subdirectories in installed or dev mode
find_package_dir <- function(sub_dir) {
  src_dir <- system.file(sub_dir, package = "epid756")

  if (src_dir == "" || !dir.exists(src_dir)) {
    pkg_dir <- system.file(package = "epid756")
    if (pkg_dir != "" && dir.exists(file.path(pkg_dir, "inst", sub_dir))) {
      src_dir <- file.path(pkg_dir, "inst", sub_dir)
    } else if (dir.exists(file.path("inst", sub_dir))) {
      src_dir <- file.path("inst", sub_dir)
    } else {
      stop(
        sprintf(
          "Could not locate '%s' folder in package 'epid756'.",
          sub_dir
        ),
        call. = FALSE
      )
    }
  }

  src_dir
}

# Internal helper to perform recursive file copying
copy_package_dir <- function(sub_dir, path, overwrite = FALSE) {
  src_dir <- find_package_dir(sub_dir)

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  files <- list.files(src_dir, recursive = TRUE, full.names = FALSE)
  copied <- character(0)

  for (f in files) {
    src_file <- file.path(src_dir, f)
    dest_file <- file.path(path, f)

    dest_dir <- dirname(dest_file)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
    }

    if (file.copy(src_file, dest_file, overwrite = overwrite)) {
      copied <- c(copied, dest_file)
    }
  }

  message(
    sprintf(
      "Successfully copied %d %s file(s) to '%s'.",
      length(copied),
      sub_dir,
      path
    )
  )
  invisible(copied)
}
