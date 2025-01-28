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
#' @param df data to unzip the columns
#' @param name of column to separate
#' @param sep string to separate columns, default is ','
#'
#' @return unzipped tibble
#' @export
#'
#' @examples
#' \dontrun{
#' column_unzipper()
#' }
column_unzipper <- function(df, name, sep=', '){
  orig <- colnames(df)
  out <- df %>%
    rename(col= !!sym(name)) %>% 
    mutate(col = strsplit(col,sep)) %>%
    unnest(col) %>%
    filter(!is.na(col)) %>%
    pivot_wider(
      names_from = col,
      values_from = col
    ) 
  new_cols <- colnames(out)[!colnames(out) %in% orig]
  out <- out %>%
    mutate(across(all_of(new_cols), ~ !is.na(.)))
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
#' if_needed_generate_example_data()
#' }
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
  
  get_values <- function(n, type, sep=NULL) {
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
      
      number_of_categories <- sample(seq(12)+3, size=1)
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
      if (is.null(sep)){
        random_numbers <- sample(1:500, size=n, replace = TRUE)
      } else {
        random_booleans <- replicate(n, paste0(sample(1:500, size = sample(1:5, 1), replace = TRUE), 
                                               collapse = sep))      
      } 
      return(random_numbers)
    }  else if (type == "FacilityCode") {
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
          paste0(
            sample(generate_lorem(), size = sample(1:5, 1), replace = TRUE),
            collapse = sep
          ),
          simplify = TRUE
        )
      }
      return(random_strings)
    }
  }
    
  if(is.character(test_analytic) & test_analytic=="Replace with Analytic Tibble") {
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
        for (i in seq(length(type_list))) {
          type <- type_list[i]
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
            new_col <- get_values(target_rows, str_remove(type, '-N'), sep = ',')
          } else {
            new_col <- get_values(target_rows, type)
          }
          expanded_analytic[i + 1] <- new_col
          
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
      } else {
        test_analytic[construct] <- get_values(nrow(test_analytic), type)
      }
    }
    return(test_analytic)
  } else{
    return(test_analytic)
  }
}
