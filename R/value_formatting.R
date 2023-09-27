#' Format Count Percent
#' 
#' @description Returns the numerator and in parenthesis the calculated percentage of denominator
#' 
#' @param numerator single value
#' @param denominator single value
#' @param decimals number of decimals places, defaults to zero
#' 
#' @return string
#' @export
#'
#' @examples
#' \dontrun{
#' format_count_percent()
#' }
format_count_percent <- function(numerator, denominator, decimals=0){
  return(paste0(numerator,' (', round(numerator / denominator * 100, decimals), '%)'))
}