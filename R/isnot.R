#' A function to make logical equivalencies with SAS
#'
#'
#' Makes the equivalent for ne in SAS code as R != returns NA if the value is NA but we want 2 != NA to be TRUE not NA
#'
#' @param x input number
#' @param y defaults to 1 but can be any number.
#'
#' @keywords mssic
#'
#' @export
#'
#' @examples
#' NA == 1
#' is.not(NA)


is.not = function(x, y = 1){ # Makes the equivalent for ne in SAS code as R != returns NA if the value is NA but we want 2 == NA to be FALSE not NA
  isit = (x !=y | is.na(x))
  return(isit)
}
