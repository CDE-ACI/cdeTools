utils::globalVariables(c("n"))  # for dplyr::count use

#' Check uniqueness of a dataset by specified variables
#'
#' This function checks whether the input dataset is unique by a given set of variables.
#' If duplicates are found, the function prints a sample and stops with an error (unless return_dupes is TRUE).
#'
#' @param df A data frame.
#' @param vars A character vector of variable names to check uniqueness by.
#' @param n_show Number of duplicate rows to show (default is 5).
#' @param df_label Optional label for the dataset, for more informative messages.
#' @param return_dupes Logical. If TRUE and duplicates are found, returns a list with
#'        the original df and a data frame of duplicate keys. Default is FALSE.
#'
#' @return If unique, returns the original data frame.
#'         If not unique and `return_dupes = TRUE`, returns a list with `df` and `dupes`.
#'         Otherwise, stops with an error.
#' @export
#'
#' @importFrom dplyr count filter across all_of select
#' @importFrom cli cli_alert_danger cli_alert_info cli_alert_success
check_uniq <- function(df, vars, n_show = 5, df_label = NULL, return_dupes = FALSE) {
  df_name <- if (!is.null(df_label)) df_label else deparse(substitute(df))

  dupes <- df |>
    count(across(all_of(vars)), name = "n") |>
    filter(n > 1)

  if (nrow(dupes) > 0) {
    cli::cli_alert_danger("Dataset {.strong {df_name}} is NOT unique by: {.val {paste(vars, collapse = ', ')}}")
    cli::cli_alert_info("{nrow(dupes)} duplicated combinations found.")
    print(utils::head(dupes, n_show))

    if (return_dupes) {
      return(list(df = df, dupes = dupes |> select(-n)))
    } else {
      stop("Uniqueness check failed.")
    }
  }

  cli::cli_alert_success("Dataset {.strong {df_name}} is unique by: {.val {paste(vars, collapse = ', ')}}")
  return(df)
}


