#' Round to nearest, always rounding .5 up
#'
#' Unlike R’s default “round()” (banker’s rounding), this version always pushes
#' .5 upward (e.g. 1.5→2, −1.5→−2).
#'
#' @param x Numeric vector to be rounded.
#' @param digits Integer: number of decimal places (default 0).
#' @return A numeric vector of the same length as x, rounded half‐up.
#' @examples
#' round_half_up(1.5)      # 2
#' round_half_up(2.345, 2) # 2.35
#' round_half_up(-1.5)     # -2
#' @export
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  return(z * posneg)
}
