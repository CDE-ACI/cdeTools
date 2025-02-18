#' Apply Column Mapping and Convert Types
#'
#' This function renames columns in a dataframe based on a predefined mapping
#' and can also apply type conversions if needed.
#'
#' @param df A dataframe whose columns need renaming.
#' @return A dataframe with renamed columns.
#' @examples
#' df <- data.frame(DASY_KEY = 2022, DDST_DIST_NUMBER = 1234)
#' apply_col_map(df)
#' @export

apply_col_map  <- function(df) {

  # Define a named character vector where
  #   *names*     = the *new* column names
  #   *values*    = the *current* (old) column names in your df
  rename_map <- c(
    year = "DASY_KEY",
    year_detail = "DETAIL_DASY_KEY",
    dist = "DDST_DIST_NUMBER",
    distname = "DDST_DISTRICT_NAME",
    schname = "DSCH_SCHOOL_NAME",
    sch  = "DSCH_SCHOOL_NUMBER",
    test = "TEST_NAME",
    sub = "SUBJECT",
    emh = "DEMH_EMH_CODE",
    include = "INCLUDED_IN_CALCS_YN",
    aec = "ALT_ED_CAMPUS_YN",
    mss = "ACH_MEAN_SS",
    mgp = "GRO_MEDIAN_SGP"
  )

  # Filter mapping to names that exist in df
  existing_renames <- rename_map[rename_map %in% names(df)]

  # Optional debugging: which columns are being renamed?
  if (length(existing_renames) == 0) {
    message("No matching columns found to rename.")
  } else {
    message("Renaming columns as follows:")
    print(existing_renames)
  }

  # Perform the rename using the valid subset
  df <- dplyr::rename(df, !!!existing_renames)

  # Lowercase all names
  names(df) <- tolower(names(df))

  return(df)

}
