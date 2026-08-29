#' Format Count Percent
#' 
#' @description Returns the numerator and in parenthesis the calculated percentage of denominator
#' 
#' @param numerator single value
#' @param denominator single value
#' @param decimals number of decimals places, defaults to one so that small
#'   nonzero counts do not display as 0%
#'
#' @return string
#' @export
#'
#' @examples
#' \dontrun{
#' format_count_percent()
#' }
format_count_percent <- function(numerator, denominator, decimals=1){
  return(ifelse(as.numeric(numerator)==0 & as.numeric(denominator)==0, paste0(as.numeric(numerator),' (', trimws(format(round(as.numeric(numerator), decimals), nsmall = decimals)), '%)'), paste0(as.numeric(numerator),' (', trimws(format(round(as.numeric(numerator) / as.numeric(denominator) * 100, decimals), nsmall = decimals)), '%)')))
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
  return(paste0(trimws(format(round(mean(as.numeric(values), na.rm = TRUE),decimals), nsmall = decimals)), " (", trimws(format(round(sd(as.numeric(values), na.rm = TRUE),decimals), nsmall = decimals)), ")"))
}


#' Collapse free-text Other entries to a single Other level
#'
#' @description
#' Free-text "Other" entries are reported as a single Other category rather
#' than one row per verbatim. Without this a handful of fields contribute many
#' near-unique rows that make characteristics tables run for pages.
#'
#' @param x character vector of category values
#'
#' @return character vector with values beginning "other" (case-insensitive)
#'   replaced by "Other"; NA preserved.
#' @export
#'
#' @examples
#' collapse_other(c("Other: fell", "Fall", NA))
collapse_other <- function(x) {
  ifelse(is.na(x), NA_character_,
         ifelse(str_detect(x, regex("^\\s*other", ignore_case = TRUE)), "Other", x))
}


#' Collapse Other entries inside a delimited check-all-that-apply string
#'
#' @description
#' Check-all-that-apply fields arrive as a single "; "-separated string.
#' Collapsing the Other terms inside the list (rather than only when Other is
#' the whole value) lets a characteristics table split the list into one row
#' per selection.
#'
#' @param x character vector of delimited selections
#' @param sep output separator (defaults to "; ")
#'
#' @return character vector with each element's Other terms collapsed and
#'   duplicates removed.
#' @export
#'
#' @examples
#' collapse_other_multi("Other: rod; Plate; Other: pin")
collapse_other_multi <- function(x, sep = "; ") {
  vapply(x, function(value) {
    if (is.na(value)) return(NA_character_)
    parts <- str_trim(str_split(value, ";")[[1]])
    parts <- parts[nzchar(parts)]
    paste(unique(collapse_other(parts)), collapse = sep)
  }, character(1), USE.NAMES = FALSE)
}
