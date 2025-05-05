#' Create an HTML-formatted transition matrix of before→after ratings
#'
#' @param df        A data frame containing at least the columns indicated by `pre_col` and `post_col`, plus `test` and `sub`.
#' @param test      Optional string. If non-NULL, only rows with `test == test` are used.
#' @param subgroup  Optional string. If non-NULL, only rows with `sub == subgroup` are used.
#' @param pre_col   Name of the "before" rating column (ordered factor). Default `"rating_pre"`.
#' @param post_col  Name of the "after"  rating column (ordered factor). Default `"rating_post"`.
#' @param return_df Logical; if `TRUE`, returns the raw transition data frame instead of an HTML table. Default `FALSE`.
#' @return If `return_df = FALSE` (default), an HTML `kable` of the colored transition matrix.  
#'         If `return_df = TRUE`, a base data frame (matrix form) of counts, with rownames = pre-levels.
#' @importFrom dplyr filter
#' @importFrom knitr kable
#' @importFrom kableExtra cell_spec kable_styling
#' @export
trans_matrix <- function(df,
                         test      = NULL,
                         subgroup  = NULL,
                         pre_col   = "rating_pre",
                         post_col  = "rating_post",
                         return_df = FALSE) {
  data <- df

  if (!is.null(test)) {
    data <- data |> dplyr::filter(test == !!test)
  }
  if (!is.null(subgroup)) {
    data <- data |> dplyr::filter(sub == !!subgroup)
  }
  if (nrow(data) == 0) {
    stop("No data found",
         if (!is.null(test))     paste0(" for test=", test),
         if (!is.null(subgroup)) paste0(" and subgroup=", subgroup))
  }

  # build the raw transition counts matrix
  tbl <- with(data, table(.data[[pre_col]], .data[[post_col]]))
  mat <- as.data.frame.matrix(tbl)

  # if the user wants the raw counts, return here
  if (return_df) {
    return(mat)
  }

  # otherwise proceed to format as colored HTML table
  levels_before <- levels(data[[pre_col]])
  get_color <- function(before, after) {
    i0 <- match(before, levels_before)
    i1 <- match(after,  levels_before)
    if      (i1 < i0) "green"
    else if (i1 > i0) "red"
    else              "grey"
  }

  mat_fmt <- mat
  for (i in seq_len(nrow(mat_fmt))) {
    before <- rownames(mat_fmt)[i]
    for (j in seq_len(ncol(mat_fmt))) {
      after <- colnames(mat_fmt)[j]
      v     <- mat_fmt[i, j]
      if (v == 0 || before == "-") {
        mat_fmt[i, j] <- as.character(v)
      } else {
        mat_fmt[i, j] <- kableExtra::cell_spec(
          v,
          color      = "black",
          background = get_color(before, after),
          bold       = TRUE
        )
      }
    }
  }

  caption <- paste(
    "Transition Matrix",
    if (!is.null(test))     paste0("for test=", test),
    if (!is.null(subgroup)) paste0("subgroup=", subgroup)
  )

  knitr::kable(
    mat_fmt,
    format  = "html",
    escape  = FALSE,
    align   = "c",
    caption = caption
  ) |>
    kableExtra::kable_styling("striped", full_width = FALSE)
}


#' Summarize rating changes (before→after) for one test/subgroup
#'
#' @param df A data frame containing at least `test`, `sub`, `rating_pre`, and `rating_post`
#' @param test      Optional string to filter on `test`
#' @param subgroup  Optional string to filter on `sub`
#' @param debug     Logical; if `TRUE`, prints any rows where change is `NA`
#' @return An HTML table (a `kable`) with counts & percents by change category
#' @importFrom dplyr filter mutate count arrange
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @export
trans_sum <- function(df, test = NULL, subgroup = NULL, debug = FALSE) {
  df_sub <- df

  if (!is.null(test)) {
    df_sub <- df_sub |> dplyr::filter(test == !!test)
  }
  if (!is.null(subgroup)) {
    df_sub <- df_sub |> dplyr::filter(sub == !!subgroup)
  }

  if (nrow(df_sub) == 0) {
    stop("No data found",
         if (!is.null(test))     paste0(" for test = ", test),
         if (!is.null(subgroup)) paste0(" and subgroup = ", subgroup))
  }

  df2 <- df_sub |>
    dplyr::mutate(
      diff = ifelse(
        as.character(rating_pre) == "-", NA_real_,
        as.numeric(rating_post) - as.numeric(rating_pre)
      ),
      change = dplyr::case_when(
        rating_pre  == "-" & rating_post != "-" ~ "Previously Unrated",
        rating_pre  == "-" & rating_post == "-" ~ "No Rating",
        rating_pre  != "-" & rating_post == "-" ~ "Now Unrated",
        diff <  0  ~ paste("Up", abs(diff)),
        diff == 0  ~ "No Change",
        diff >  0  ~ paste("Down", diff),
        TRUE        ~ NA_character_
      )
    )

  if (debug) {
    nas <- df2 |> dplyr::filter(is.na(change))
    if (nrow(nas) > 0) {
      message("Rows with NA change:")
      print(nas)
    } else {
      message("No NA change rows.")
    }
  }

  summary_df <- df2 |>
    dplyr::count(change) |>
    dplyr::arrange(change) |>
    dplyr::mutate(
      Percent = paste0(round(n / sum(n) * 100, 1), "%")
    ) |>
    dplyr::rename(Change = change, Count = n)

  knitr::kable(
    summary_df,
    format  = "html",
    align   = "c",
    caption = paste(
      "Transition Summary",
      if (!is.null(test))     paste0("for test=", test),
      if (!is.null(subgroup)) paste0("subgroup=", subgroup)
    )
  ) |>
    kableExtra::kable_styling(full_width = FALSE, position = "center")
}
