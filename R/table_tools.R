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


Here’s an updated roxygen block (and the function header) that matches the hardened column_unzipper() we just wrote:

#' Column unzipper
#'
#' @description
#' Split a delimited “multi-value” column into multiple boolean indicator
#' columns (one per unique category), optionally retaining the original column.
#' Handles trimming, `NA`/empty values, and duplicate categories per row.
#'
#' @param df A data frame or tibble.
#' @param name Column to separate. Can be a string (e.g., `"tags"`) or a bare
#'   column name (e.g., `tags`).
#' @param sep A string delimiter used to separate values within the column.
#'   Defaults to `"; "` (common in REDCap-style exports).
#' @param drop_original Logical; if `TRUE`, the original delimited column is
#'   dropped from the result. Defaults to `FALSE`.
#' @param as_logical Logical; if `TRUE`, output indicators are `TRUE/FALSE`.
#'   If `FALSE`, outputs are `1/0` suitable for summation. Defaults to `TRUE`.
#'
#' @returns
#' A tibble with all original identifier columns preserved and one additional
#' column per unique category found in `name`. New columns are logicals
#' (`TRUE/FALSE`) or integers (`1/0`) depending on `as_logical`.
#'
#' @details
#' - Rows are expanded with `tidyr::separate_rows()`, values are `trimws()`’d,
#'   and empty/`NA`/`"NA"` entries are dropped.
#' - Duplicates per id+category are removed before widening, preventing
#'   `pivot_wider()` list-cols and warnings about non-unique values.
#' - Missing combinations are filled with `0` (or `FALSE` when `as_logical=TRUE`).
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' df <- tibble::tibble(
#'   study_id = c("1001","1002","1003"),
#'   facilitycode = c("HCM","UMD","UMD"),
#'   ineligibility_reasons = c(
#'     "Does Not Meet Criteria",
#'     "Not Enrolled Within Time; Does Not Meet Criteria",
#'     "Unable to Consent; Maintain Followup"
#'   )
#' )
#'
#' # Logical indicators (default)
#' out_logical <- column_unzipper(
#'   df, name = ineligibility_reasons, sep = "; ", as_logical = TRUE
#' )
#'
#' # Numeric indicators (1/0), dropping original column
#' out_numeric <- column_unzipper(
#'   df, name = "ineligibility_reasons", sep = "; ",
#'   drop_original = TRUE, as_logical = FALSE
#' )
#' }
#'
#' @export
column_unzipper <- function(df, name, sep = "; ", drop_original = FALSE, as_logical = TRUE) {
  nm <- rlang::ensym(name)
  orig_names <- names(df)
  id_cols <- setdiff(orig_names, rlang::as_string(nm))
  df_orig <- df

  out <- df %>%
    dplyr::rename(col = !!nm) %>%
    dplyr::mutate(col = as.character(col)) %>%
    tidyr::separate_rows(col, sep = sep) %>%
    dplyr::mutate(col = trimws(col)) %>%
    dplyr::filter(!is.na(col), col != "", col != "NA") %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(id_cols)), col, .keep_all = TRUE) %>%
    dplyr::mutate(.value = 1L) %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_cols),
      names_from  = col,
      values_from = .value,
      values_fill = 0
    )

  new_cols <- setdiff(names(out), id_cols)
  if (as_logical && length(new_cols)) {
    out <- out %>% dplyr::mutate(dplyr::across(dplyr::all_of(new_cols), ~ .x > 0))
  }

  if (!drop_original) {
    out <- df_orig %>%
      dplyr::select(dplyr::all_of(id_cols), !!nm) %>%
      dplyr::left_join(out, by = id_cols)
  }

  out
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
      group_by(across(all_of(groups))) %>% 
      summarize(across(is.logical, ~sum(. == TRUE, na.rm = TRUE), .names = "{.col}"))
  }
  return(out)
}



#' Confirm Stability of Related Visual
#' 
#' @description Makes sure a related visualization has not changed
#'
#' @param function_name name of the visualization function
#' @param key verification key
#'
#' @return summed tibble
#' @export
#'
#' @examples
#' \dontrun{
#' confirm_stability_of_related_visual()
#' }
confirm_stability_of_related_visual <- function(function_name, key){
  test <- get(function_name, envir = loadNamespace("VisualizationLibrary"))
  code <- deparse(test)
  code <- paste(code,collapse="\n")
  check_key <- rlang::hash(code)
  if(check_key != key){
    stop(paste0("Function: ",function_name," has changed since this function was updated (key: ",check_key,")"))
  }
}


#' Generate dummy dataset
#' 
#' @description 
#' Generates random data to test visualizations. 
#' 
#' Currently accepted data types are:
#' Category,
#' Boolean,
#' Date,
#' Number,
#' Number1dgt,
#' FacilityCode (15 Codes, AAA - AAO),
#' Character (random lorem string, 1-3 "sentences"),
#' Form,
#' Period,
#' FollowupStatus,
#' FollowupPeriod,
#' NamedCategory,
#' TreatmentArm
#' 
#' Adding a '-N' to the end of a type will replicate values, separated with a comma.
#' '-NS' will separate replicated values with a semicolon.
#' The '-U(n)' tag allows you to specify the size of the category or range of numbers 
#' you'd like to generate data for. Replace (n) with the integer size of the category 
#' or number range. EX: 'Number-U4'
#' 
#' Long Files are supported and are formed like
#' "('rowsep', 'colsep')Type1|Type2|Type3" etc.
#' NamedCategory type is formed like
#' "NamedCategory['Value 1' 'Value 2']" etc.
#'
#' @return dummy analytic dataset
#' @export
#'
#' @examples
#' if_needed_generate_example_data("Replace with Analytic Tibble", example_constructs = c("facilitycode", "severity"), example_types = c("FacilityCode", "NamedCategory['Mild' 'Moderate' 'Severe']))
#' 
if_needed_generate_example_data <- function(test_analytic, example_constructs = '', example_types = ''){
  today <- Sys.Date()
  lorem_words <- c("lorem", "ipsum", "dolor", "sit", "amet", "consectetur", 
                   "adipiscing", "elit", "sed", "do", "eiusmod", "tempor", 
                   "incididunt", "ut", "labore", "et", "dolore", "magna", "aliqua")
  generate_lorem <- function() {
    out <- sapply(1:sample(1:3, size = 1), function(x) {
      paste(sample(lorem_words, sample(5:10, size = 1), replace = TRUE), collapse = " ")
    })
    return(paste(out, collapse = '. '))
  }
  
  get_values <- function(n, type, sep=NULL, unique_vals = NULL) {
    if(type == "Category"){
      modes <- c("Category", "Group", "Type")
      counter <- c("A", "i", "1")
      
      number_to_roman <- function(n) {
        if (n <= 0 || n >= 4000) {
          stop("Number must be between 1 and 3999.")
        }
        
        roman_symbols <- c(
          "M"  = 1000, 
          "CM" = 900, 
          "D"  = 500, 
          "CD" = 400,
          "C"  = 100, 
          "XC" = 90, 
          "L"  = 50, 
          "XL" = 40,
          "X"  = 10, 
          "IX" = 9, 
          "V"  = 5, 
          "IV" = 4,
          "I"  = 1
        )
        
        roman <- ""
        
        for (symbol in names(roman_symbols)) {
          while (n >= roman_symbols[symbol]) {
            roman <- paste0(roman, symbol)
            n <- n - roman_symbols[symbol]
          }
        }
        return(roman)
      }
      number_to_letters <- function(n) {
        result <- ""
        while (n > 0) {
          remainder <- (n - 1) %% 26
          result <- paste0(LETTERS[remainder + 1], result)
          n <- (n - 1) %/% 26
        }
        return(result)
      }
      
      if (is.null(unique_vals)){
        number_of_categories <- sample(seq(12)+3, size=1)
      } else {
        number_of_categories <- unique_vals
      }
      counter <- sample(counter, size=1)
      mode <- sample(modes, size=1)
      start <- 0
      
      if(mode %in% names(used_category)){
        if(counter %in% names(used_category[[mode]])){
          start <- used_category[[mode]][[counter]]
        }
      } else{
        used_category[[mode]] <- list()
      }
      
      used_category[[mode]][[counter]] <- number_of_categories+start
      new_numbers <- seq(number_of_categories)+start
      
      if(counter=="i"){
        new_numbers <- Vectorize(number_to_roman)(new_numbers)
      }
      if(counter=="A"){
        new_numbers <-  Vectorize(number_to_letters)(new_numbers)
      }
      
      categories <- paste(sample(mode, size = 1),new_numbers)
      if (!is.null(sep)) {
        random_categories <- replicate(n, paste0(sample(categories, sample(1:5, 1), replace = TRUE), collapse = sep))
      } else {
        random_categories <- sample(categories, size = n, replace = TRUE)
      }
      return(random_categories)
    } else if(type == "Boolean") {
      if (is.null(sep)){
        random_booleans <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
      } else {
        random_booleans <- replicate(n, paste0(sample(c(TRUE, FALSE), size = sample(1:5, 1), replace = TRUE), 
                                               collapse = sep))
      }
      return(random_booleans)
    } else if(type == "Date") {
      if (is.null(sep)){
        random_date_indices <- sample(1:500, size=n, replace = TRUE)
        random_dates <- today - random_date_indices
      } else {
        random_dates <- replicate(n, paste0(today-sample(1:500, size = sample(1:5, 1), replace = TRUE), collapse = sep))
      }
      return(random_dates)
    } else if(type == "Number")  {
      if (is.null(sep) & is.null(unique_vals)){
        random_numbers <- sample(1:500, size=n, replace = TRUE)
      } else if (is.null(unique_vals)) {
        random_numbers <- replicate(n, paste0(sample(1:500, size = sample(1:5, 1), replace = TRUE), 
                                               collapse = sep))      
      } else {
        random_numbers <- sample(1:unique_vals, size=n, replace = TRUE)
      }
      return(random_numbers)
    } else if (type == "FacilityCode") {
      codes <- c(paste0("AA", LETTERS[seq(1,15)]))
      random_indices <- sample(1:15, size=n, replace = TRUE)
      random_codes <- codes[random_indices]
      return(random_codes)
    } else if (type == "Character") {
      if (is.null(sep)){
        random_strings <- replicate(n, generate_lorem())
      } else {
        random_strings <- replicate(
          n,
          paste0(sample(generate_lorem(), size = sample(1:5, 1), replace = TRUE),
            collapse = sep),
          simplify = TRUE
        )
      }
      return(random_strings)
    } else if (type == "Form") {
      forms <- paste("Form", seq(1, 5))
      random_indices <- sample(1:5, size=n, replace = TRUE)
      random_forms <- forms[random_indices]
      return(random_forms)
    } else if (type == "Period") {
      periods <- paste(seq(1, 6), "Unit Period")
      random_indices <- sample(1:6, size=n, replace = TRUE)
      random_periods <- periods[random_indices]
      return(random_periods)
    } else if (type == "FollowupStatus") {
      statuses <- c("Complete", "Complete: Early", "Complete: Late", "Incomplete", 
                    "Missed", "Not Started", "Not Expected")
      random_indices <- sample(1:7, size=n, replace = TRUE)
      random_statuses <- statuses[random_indices]
      return(random_statuses)
    } else if (type == "FollowupPeriod") {
      periods <- c("2 Week", "3 Month", "6 Month", "12 Month")
      random_indices <- sample(1:4, size=n, replace = TRUE)
      random_periods <- periods[random_indices]
      return(random_periods)
    } else if (str_detect(type, "NamedCategory")) {
      categories <- type %>%
        str_remove('NamedCategory\\[') %>%
        str_remove('\\]') %>%
        str_split_1("' '") %>%
        str_remove_all('\'') %>%
        str_replace_all('COMMASAFE', ',')
      if (is.null(sep)) {
        random_indices <- sample(1:length(categories), size=n, replace = TRUE)
        
        random_categories <- categories[random_indices]
      } else {
        random_categories <- replicate(n, paste0(sample(categories, size = sample(1:5, 1), 
                                                        replace = TRUE), collapse = sep))    
      } 
      return(random_categories)
    } else if (type == "TreatmentArm") {
      groups <- c('Group A', 'Group B', NA)
      random_groups <- sample(groups, size=n, replace = TRUE)
      return(random_groups)
    } else {
      stop('INVALID TYPE!')
    }
  }
  
  if(length(test_analytic)==1){
    if(is.character(test_analytic) & test_analytic=="Replace with Analytic Tibble") {
      if (any(str_detect(example_types, "\\[[^\\[|\\]]*\\]"))) {
        lookbehind_length <- max(
          str_length(unlist(str_extract_all(example_types, "\\[[^\\[|\\]]*\\]"))),
          na.rm = TRUE
        )
      } else {
        lookbehind_length <- NA
      }
      example_types <- str_remove_all(example_types, "\n[ \t]*")
      
      target_regex <- paste0('(?<=\\[[^\\]]{0,', lookbehind_length, '}),(?=[^\\[]*\\])')
      if(!is_empty(lookbehind_length) & !is.na(lookbehind_length) & lookbehind_length!=-Inf){
        example_types <- example_types %>%
           stringi::stri_replace_all_regex(target_regex, 'COMMASAFE')
      }
      test_analytic <- tibble(study_id=as.character(seq(999)+1000))
      used_category <- list()
      for(i in seq(length(example_constructs))){
        construct <- example_constructs[i]
        type <- example_types[i]
        
        if (str_detect(type, '\\|')) {
          random_booleans <- sample(c(TRUE, FALSE), size = nrow(test_analytic), replace = TRUE)
          test_analytic[construct] <- random_booleans
          
          inner_analytic <- test_analytic %>%
            mutate(rows = ifelse(!!sym(construct),
                                 sample(1:10, replace = TRUE),
                                 0))
          target_rows <- sum(inner_analytic$rows)
          
          seps <- str_remove(type, "\'\\).*") %>%  
            str_remove("\\(\'") %>%   
            str_split("', '") %>%       
            unlist()   
          row_sep <- seps[1]
          column_sep <- seps[2]
          
          type_list <- str_remove(type, '^[^\\)]+\\)') %>%
            str_split('\\|') %>%
            unlist()
          
          expanded_analytic <- tibble(
            study_id = rep(inner_analytic$study_id, inner_analytic$rows)
          )
          for (ii in seq(length(type_list))) {
            type <- type_list[ii]
            if (str_detect(type, ',|;')) {
              inner_sep <- ifelse(str_detect(type, ','), ',', ';')
              target_type <- type
              new_col <- NULL
              for (inner_target_type in unlist(str_split(target_type, inner_sep))){
                if (str_detect(inner_target_type, '-N')) {
                  inner_new_col <- get_values(target_rows, str_remove(inner_target_type, '-N'), sep = inner_sep)
                  if (is.null(new_col)) {
                    new_col <- inner_new_col
                  } else {
                    new_col <- paste0(new_col, inner_new_col, sep = inner_sep)
                  }
                }
              }
              
            } else if (str_detect(type, '-N')) {
              if (str_detect(type, '-NS')) {
                new_col <- get_values(target_rows, str_remove(type, '-NS'), sep = ';')
              } else {
                new_col <- get_values(target_rows, str_remove(type, '-N'), sep = ',')
              }
            } else if (str_detect(type, '-U')) {
              unique_vals_spec <- str_extract(type, "(?<=-U).*") %>% as.numeric()
              new_col <- get_values(nrow(expanded_analytic), str_remove(type, "-U(.*)"), 
                                                     unique_vals = unique_vals_spec)
            } else {
              new_col <- get_values(target_rows, type)
            }
            expanded_analytic[ii + 1] <- new_col
            
          }
          zipped_analytic <- expanded_analytic %>%
            unite('temporary', -study_id, sep = column_sep) %>%
            group_by(study_id) %>%
            reframe(!!sym(construct) := paste0(temporary, collapse = row_sep))
          test_analytic <- left_join(test_analytic %>% select(-!!sym(construct)), zipped_analytic)
        } else if (str_detect(type, ',')) {
          final_out <- c()
          for (typechild in unlist(str_split(type, ','))) {
            out_column <- get_values(nrow(test_analytic), typechild)
            if (length(final_out) == 0) {
              final_out <- out_column
            } else {
              final_out <- paste(final_out, out_column, sep = ',')
            }
          }
          test_analytic[construct] <- final_out
        } else if (str_detect(type, ';')) {
          final_out <- c()
          for (typechild in unlist(str_split(type, ';'))) {
            out_column <- get_values(nrow(test_analytic), typechild)
            if (length(final_out) == 0) {
              final_out <- out_column
            } else {
              final_out <- paste(final_out, out_column, sep = ';')
            }
          }
          test_analytic[construct] <- final_out
        } else if (str_detect(type, '-U')) {
          unique_vals_spec <- str_extract(type, "(?<=-U).*") %>% as.numeric()
          test_analytic[construct] <- get_values(nrow(test_analytic), str_remove(type, "-U(.*)"), 
                                                 unique_vals = unique_vals_spec)
        } else if (str_detect(type, '-N')) {
          if (str_detect(type, '-NS')) {
            test_analytic[construct] <- get_values(nrow(test_analytic), str_remove(type, '-NS'), sep = ';')
          } else {
            test_analytic[construct] <- get_values(nrow(test_analytic), str_remove(type, '-N'), sep = ',')
          }
        } else {
          test_analytic[construct] <- get_values(nrow(test_analytic), type)
        }
      }
      return(test_analytic)
    } else{
      return(test_analytic)
    }
  } else{
    return(test_analytic)
  }
}
