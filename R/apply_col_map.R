#' Apply Column Mapping and Convert Types
#'
#' This function renames columns in a dataframe based on a predefined mapping and,
#' optionally, applies type conversions to standard variables.
#'
#' @param df A dataframe whose columns need renaming.
#' @param convert_type Logical. If TRUE, converts variable types according to predefined standards. Defaults to TRUE.
#' @return A dataframe with renamed columns and, optionally, converted types.
#' @examples
#' df <- data.frame(DASY_KEY = 2022, DDST_DIST_NUMBER = 1234)
#' apply_col_map(df, convert_type = TRUE)
#' @export
apply_col_map <- function(df, convert_type = TRUE) {

  # Define a named character vector mapping new names (names) to current names (values)
  rename_map <- c(
    year = "DASY_KEY",
    year = "REPORT_YEAR",
    year_detail = "DETAIL_DASY_KEY",
    dist = "DDST_DIST_NUMBER",
    distname = "DDST_DISTRICT_NAME",
    schname = "DSCH_SCHOOL_NAME",
    sch  = "DSCH_SCHOOL_NUMBER",
    grd_low = "DSCH_GRADE_SPAN_LOW",
    grd_high = "DSCH_GRADE_SPAN_HIGH",
    test = "TEST_NAME",
    sub = "SUBJECT",
    emh = "DEMH_EMH_CODE",
    emh = "EMH_CODE",
    include = "INCLUDED_IN_CALCS_YN",
    aec = "ALT_ED_CAMPUS_YN",
    aec = "AEC_YN",
    charter = "CHATER_YN",
    magnet = "MAGNET_YN",
    innovation = "INNOVATION_YN",
    online = "ONLINE_YN",
    mss = "ACH_MEAN_SS",
    mgp = "GRO_MEDIAN_SGP",
    pct_pts = "PCT_PTS_EARN",
    pct_pts_w = "PCT_PTS_EARN_WEIGHTED"

  )

  # Filter mapping to include only columns that exist in the dataframe
  existing_renames <- rename_map[rename_map %in% names(df)]

  if (length(existing_renames) == 0) {
    message("No matching columns found to rename.")
  } else {
    message("Renaming columns as follows:")
    print(existing_renames)
  }

  # Rename columns using dplyr
  df <- dplyr::rename(df, !!!existing_renames)

  # Lowercase all column names for consistency
  names(df) <- tolower(names(df))

  # Optionally convert variable types if convert_type is TRUE
  if (convert_type) {
    # Define standard variable names for conversion
    integer_vars <- c("scale_score", "sgp")
    numeric_vars <- c("mss", "mgp", "pct_pts", "pct_pts_w")
    factor_vars  <- c("year", "dist", "sch", "sub", "emh", "test", "aec", "online",
                      "charter", "magnet", "innovation", "grd_low", "grd_high",
                      "rating")

    # Determine which of these variables exist in the dataframe
    integer_vars_exist <- intersect(integer_vars, names(df))
    numeric_vars_exist <- intersect(numeric_vars, names(df))
    factor_vars_exist  <- intersect(factor_vars, names(df))

    # Apply conversions using dplyr's across function
    df <- df |>
      dplyr::mutate(dplyr::across(dplyr::all_of(numeric_vars_exist), as.numeric)) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(integer_vars_exist), as.integer)) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(factor_vars_exist), as.factor))
  }

  return(df)
}
