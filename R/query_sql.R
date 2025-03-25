#' Load SQL from file, apply glue substitution, and run query
#'
#' @param con A DBI database connection
#' @param filepath Path to the SQL file
#' @param settings A named list of values for glue substitution
#'
#' @return A data frame with the query results
#' @export
query_sql <- function(con, filepath, settings = list()) {
  if (!file.exists(filepath)) {
    stop("File does not exist: ", filepath)
  }

  sql <- readLines(filepath, warn = FALSE) |>
    paste(collapse = "\n") |>
    (\(txt) glue::glue(txt, .envir = as.environment(settings)))()

  DBI::dbGetQuery(con, sql)
}
