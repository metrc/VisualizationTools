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
  return(paste0(numerator,' (', format(round(numerator / denominator * 100, decimals), nsmall = decimals), '%)'))
}


#' Format Mean Standand Deviation
#' 
#' @description Returns the mean and standard deviation for the requested vector(numbers)
#' 
#' @param values insert values that you request mean and standard deviation for
#' @param decimals number of decimals places, defaults to zero
#' 
#' @return string
#' @export
#'
#' @examples
#' \dontrun{
#' format_mean_sd()
#' }
format_mean_sd <- function(values, decimals = 2){
  return(paste0(format(round(mean(values, na.rm = TRUE),decimals), nsmall = decimals), " (", format(round(sd(values, na.rm = TRUE),decimals), nsmall = decimals), ")"))
}