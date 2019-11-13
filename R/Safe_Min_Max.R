#' NULL-safe Minimizer
#' 
#'  Minimizer that return 0 if object is NULL
#'  @param v numeric vector
#'  @return smallest value in v, or 0 if v is NULL
#' @keywords internal
my.min <- function (v) {
  ifelse (is.null(v),0,min(v))
}
#' NULL-safe Maximizer
#' 
#'  Maximizer that return 0 if object is NULL
#'  @param v numeric vector
#'  @return largest value in v, or 0 if v is NULL 
#' @keywords internal
my.max <- function (v) {
  ifelse (is.null(v),0,max(v))
}