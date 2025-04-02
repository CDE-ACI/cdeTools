utils::globalVariables(c("n"))  # for dplyr::count use
#' Check uniqueness of a dataset by specified variables
#'
#' This function checks whether the input dataset is unique by a given set of variables.
#' If duplicates are found, the function prints a sample and stops with an error.
#'
#' @param df A data frame.
#' @param vars A character vector of variable names to check uniqueness by.
#' @param n_show Number of duplicate rows to show (default is 5).
#'
#' @return Returns the original data frame if unique by `vars`; otherwise throws an error.
#' @export
#'
#' @importFrom dplyr count filter across all_of
#' @importFrom cli cli_alert_danger cli_alert_info cli_alert_success
#'
check_uniq <- function(df, vars, n_show = 5) {
  df_name <- deparse(substitute(df))  # captures the expression used for `df`

  dupes <- df |>
    count(across(all_of(vars)), name = "n") |>
    filter(n > 1)

  if (nrow(dupes) > 0) {
    cli::cli_alert_danger("Dataset {.strong {df_name}} is NOT unique by: {.val {paste(vars, collapse = ', ')}}")
    cli::cli_alert_info("{nrow(dupes)} duplicated combinations found.")
    print(utils::head(dupes, n_show))
    stop("Uniqueness check failed.")
  }

  cli::cli_alert_success("Dataset {.strong {df_name}} is unique by: {.val {paste(vars, collapse = ', ')}}")
  return(df)
}

