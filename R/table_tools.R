#' Count Split Cols Long and Wide
#' 
#' @description splits columns with a given seperator, then counts the values betwwen
#' the resultant groupt
#' 
#' @param df analytic dataset
#' @param seperator string to separate columns
#' @param remove_na whether nas ought to be removed
#' 
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' count_split_cols_long_and_wide()
#' }
count_split_cols_long_and_wide <- function(df, seperator, remove_na=TRUE) {
  out <- df %>% pivot_longer(colnames(df)) %>% 
    separate(name, into = c("prefix", 'suffix'), sep = seperator, convert = TRUE) %>%
    group_by(prefix, suffix) %>% 
    count(value) %>%
    filter(ifelse(rep(remove_na, n()), !is.na(value), rep(TRUE, n()))) %>%
    pivot_wider(names_from = value, values_from = n) %>% 
    pivot_longer(cols = 3:last_col(), names_to = "level") %>%
    pivot_wider(names_from = suffix, values_from = value)
  return(out)
}


#' Summate Levels
#' 
#' @description Gives the sum of all the columns in a table
#' 
#' @param df working dataframe
#' @param group_colnames groups to have in output
#' @param level_colname column of values to keep together
#' @param levels values to combine together
#' 
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' summate_levels()
#' }
summate_levels <- function(df, group_colnames, level_colname, levels) {
  out <- df %>%
    group_by(across(all_of(group_colnames))) %>%
    filter(!!sym(level_colname) %in% levels) %>%
    select(-all_of(level_colname)) %>%
    summarize_all(sum, na.rm=TRUE)
  return(out)
}


#' Reorder Rows
#' 
#' @description sreorders the rows of a dataframe based on given vectors
#' 
#' @param df working dataframe
#' @param new_row_order string vector of new order of things
#' @param unspecified_to_bottom whether values not specified will be on the top or bottom
#' 
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' reorder_rows()
#' }
reorder_rows <- function(df, new_row_order, unspecified_to_bottom=TRUE) {
  out <- df
  for (i in seq(1:length(new_row_order))) {
    col <- names(new_row_order)[i]
    top_levels <- new_row_order[[col]]
    if (unspecified_to_bottom) {
      out <- out %>%
        rename(rcol=!!col) %>%
        mutate(rcol = factor(rcol, levels = c(top_levels, unique(rcol[!rcol %in% top_levels])))) %>%
        rename(!!col:=rcol)
    }
    else {
      out <- out %>%
        rename(rcol=!!col) %>%
        mutate(rcol = factor(rcol, levels = c(unique(rcol[!rcol %in% top_levels]), top_levels))) %>%
        rename(!!col:=rcol)
    }
  }
  out <- out %>%
    arrange(across(all_of(names(new_row_order))))
  return(out)
}


#' Column unzipper
#' 
#' @description takes a dataframe with comma separated list and widens it
#' into multiple columns, with boolean values
#'
#' @param df data to unzip the columns, must be two columns
#' @param sep string to separate columns, default is ','
#'
#' @return unzipped tibble
#' @export
#'
#' @examples
#' \dontrun{
#' column_unzipper()
#' }
column_unzipper <- function(df, sep=', '){
  out <- df %>%
    mutate(col = strsplit(col,sep)) %>%
    unnest(col) %>%
    filter(!is.na(col)) %>%
    pivot_wider(
      names_from = col,
      values_from = col
    ) %>%
    mutate(across(!1, ~ !is.na(.)))
  return(out)
}


#' Boolean Column Counter
#' 
#' @description counts the boolean columns of a tibble
#'
#' @param df tibble with boolean columns to count
#' @param groups optional grouping variable
#'
#' @return summed tibble
#' @export
#'
#' @examples
#' \dontrun{
#' boolean_column_counter()
#' }
boolean_column_counter <- function(df, groups = NULL){
  if (is.null(groups)) {
    out <- df %>%
      summarize(across(is.logical, ~sum(. == TRUE, na.rm = TRUE), .names = "{.col}"))
  } else {
    out <- df %>%
      group_by(.data[[groups]]) %>% 
      summarize(across(is.logical, ~sum(. == TRUE, na.rm = TRUE), .names = "{.col}"))
  }
  return(out)
}


