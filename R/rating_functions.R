#' Read Cut Scores from Excel
#'
#' @param path Path to Excel file with cut scores.
#'
#' @return A list with traditional and AEC cut scores as tibbles.
#' @export
read_cut_scores <- function(path) {
  list(
    traditional = readxl::read_excel(path, sheet = "traditional"),
    aec = readxl::read_excel(path, sheet = "aec")
  )
}

#' Get Cut Scores (Default or External)
#'
#' @param path Optional path to Excel file. If NULL, uses internal package data.
#'
#' @return A list of cut scores
#' @export
get_cut_scores <- function(path = NULL) {
  if (is.null(path)) {
    return(cutscores_default)
  } else {
    return(read_cut_scores(path))
  }
}

#' Get rating based on score and cut scores table
#'
#' @param score Numeric score to rate
#' @param indicator Indicator (e.g. "GRO", "ACH")
#' @param measure Measure (e.g. "GROWTH", "WIDA OTG")
#' @param emh_code EMH level ("E", "M", "H")
#' @param subject Subject (e.g. "ELA", "MATH")
#' @param cuts_df Cut scores dataframe
#' @param n N-count or rule for missing data
#'
#' @return Rating ("Exceeds", "Meets", "Approaching", "Does Not Meet", "-")
#' @export
get_rating <- function(score, indicator, measure, emh_code, subject, cuts_df, n) {

  if (is.character(n) && n == "n < 20" || is.na(score) || is.nan(score)) {
    return("-")
  }

  # Special case: GRO cuts only have "ALL" for subject/emh, so force those
  if (indicator == "GRO") {
    emh_code <- "ALL"
    subject <- "ALL"
    measure <- "GROWTH"
  }

  cuts <- cuts_df |>
    dplyr::filter(indicator == indicator,
                  measure == measure,
                  (emh == "ALL" | emh == !!emh_code),
                  (subject == "ALL" | subject == !!subject))

  excd_cut <- cuts |>
    dplyr::filter(rating_short == "excd") |>
    dplyr::pull(cutscore) |>
    dplyr::first()

  meet_cut <- cuts |>
    dplyr::filter(rating_short == "meet") |>
    dplyr::pull(cutscore) |>
    dplyr::first()

  appr_cut <- cuts |>
    dplyr::filter(rating_short == "appr") |>
    dplyr::pull(cutscore) |>
    dplyr::first()

  if (!is.na(excd_cut) && score >= excd_cut) return("Exceeds")
  if (!is.na(meet_cut) && score >= meet_cut) return("Meets")
  if (!is.na(appr_cut) && score >= appr_cut) return("Approaching")
  return("Does Not Meet")
}

#' Get points from rating
#'
#' @param rating Rating value
#' @param category Category type
#'
#' @return Numeric points earned
#' @export
get_pts <- function(rating, category) {

  pts_elig <- dplyr::case_when(
    rating == "-" ~ 0,
    category == "English Language Proficiency" ~ 2,
    category == "ALL STUDENTS" ~ 8,
    category == "SUBGROUP" ~ 1,
    TRUE ~ NA_real_
  )

  pts_earned <- dplyr::case_when(
    rating == "-" ~ 0,
    rating == "Exceeds" ~ pts_elig,
    rating == "Meets" ~ pts_elig * 0.75,
    rating == "Approaching" ~ pts_elig * 0.5,
    rating == "Does Not Meet" ~ pts_elig * 0.25,
    TRUE ~ NA_real_
  )

  return(pts_earned)
}

#' Apply ratings and points to a dataframe
#'
#' @param df A dataframe with score, indicator, measure, EMH, subject, N-count, and category columns
#' @param score_col Name of the column with scores
#' @param indicator_col Name of the column with indicator
#' @param measure_col Name of the column with measure
#' @param emh_col Name of the column with EMH codes
#' @param subject_col Name of the column with subject
#' @param n_col Name of the column with N-count
#' @param category_col Name of the column with category
#' @param cuts_df Cut scores dataframe
#' @param rating_col Name of the column to create for ratings (default "rating")
#' @param pts_col Name of the column to create for points (default "pts")
#'
#' @return Dataframe with added rating and pts columns
#' @export
apply_rating <- function(df,
                         score_col,
                         indicator_col,
                         measure_col,
                         emh_col,
                         subject_col,
                         n_col,
                         category_col,
                         cuts_df,
                         rating_col = "rating",
                         pts_col    = "pts") {

  df <- df |>
    # 1) calculate raw .rating (chr) and .pts (dbl)
    dplyr::mutate(
      .rating = purrr::pmap_chr(
        list(score     = .data[[score_col]],
             indicator = .data[[indicator_col]],
             measure   = .data[[measure_col]],
             emh_code  = .data[[emh_col]],
             subject   = .data[[subject_col]],
             n         = .data[[n_col]]),
        ~ get_rating(..1, ..2, ..3, ..4, ..5, cuts_df, ..6)
      ),
      .pts = purrr::map2_dbl(.rating, .data[[category_col]], get_pts)
    ) |>
    # 2) rename to user‐specified columns
    dplyr::rename(
      !!rating_col := .rating,
      !!pts_col    := .pts
    ) |>
    # 3) convert the new rating column into an ordered factor
    dplyr::mutate(
      !!rating_col := factor(
        .data[[rating_col]],
        levels  = c("Exceeds", "Meets", "Approaching", "Does Not Meet","-"),
        ordered = TRUE
      )
    )

  return(df)
}
