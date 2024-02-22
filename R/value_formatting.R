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
  return(ifelse(numerator==0 & denominator==0, paste0(numerator,' (', trimws(format(round(numerator, decimals), nsmall = decimals)), '%)'), paste0(numerator,' (', trimws(format(round(numerator / denominator * 100, decimals), nsmall = decimals)), '%)')))
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
  return(paste0(trimws(format(round(mean(values, na.rm = TRUE),decimals), nsmall = decimals)), " (", trimws(format(round(sd(values, na.rm = TRUE),decimals), nsmall = decimals)), ")"))
}
