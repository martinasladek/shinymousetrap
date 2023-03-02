#' Format-round helper
#'
#' @param x a number to format for reporting
#' @param digits number of digits
#'
#' @return a rounded character object
#' @export
#'
#' @examples
#' fr(0.0123423, 2)
fr <- function(x, digits = 3){
  format(round(x, digits), nsmall = digits)
}
