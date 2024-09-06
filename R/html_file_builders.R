#' METRC Basic Report
#'
#' @description This function gets the payments for a study using the stored method
#'
#' @param html_table_list list of named html tables (set the name in the list to be the table name)
#' @param file_name place to store html file
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' metrc_basic_report()
#' }
metrc_basic_report <- function(html_table_list, file_name){
  injected_tags <- '\n<link rel="shortcut icon" href="#"><meta name="viewport" content="width=device-width, initial-scale=1">\n<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.css" />\n'
  style_text <- '// this is a simple bit of CSS that makes the tables and text in a report look refined\nbody {\n  font-family: "Times New Roman", Times, serif;\nmargin-left: 20px;\n}\ntable.table-basic {\n  font-family: "Times New Roman", Times, serif;\n  width: 100%;\n  text-align: center;\n}\ntable.table-basic td, table.table-basic th {\n  padding: 10px;\n}\ntable.table-basic tbody td {\n  font-size: 13px;\n}\ntable.table-basic tbody tr {\n    border-bottom: 1px solid black;\n    border-top: 1px solid black;\n    border-collapse: collapse;\n}\n'
  injected_style_text <- 'table{\n    border-collapse:collapse;\n}\ntr td{\n    page-break-inside: avoid;\n    white-space: nowrap;\n}'
  html_page <- paste('<!DOCTYPE html>\n<html>\n<head>',injected_tags,'<style>\n',style_text,injected_style_text,'</style></head>\n<br/>',sep='')
  
  first <- TRUE
  table_names <- names(html_table_list)
  for(i in seq(1:length(html_table_list))){
    table_html <- ""
    if(!first){
      table_html <- paste0(table_html, '\n<p style="page-break-after: always;">&nbsp;</p><br/>\n')
    }
    first <- FALSE
    input_table <- html_table_list[i]
    input_table <- str_replace_all(input_table, 'table table-striped"', 'table-basic" border=1 frame=hsides rules=rows')
    input_table <- str_replace_all(input_table, '#ddd', 'black')
    input_table <- gsub('([a-z])\\.([^ ])','\\1 \\2',input_table)
    table_html <- paste0(table_html, '<h3>Table ',i,'. ',table_names[i],'</h3>\n<br/>\n',input_table,'\n<br/>\n')
    html_page <- paste0(html_page,"\n",table_html)
  }
  
  html_page <- paste(html_page,'\n\n</body>\n</html>')
  write_file(html_page, file_name)
}


#' METRC Basic Report Numbers
#'
#' @description This function gets the payments for a study using the stored method
#'
#' @param html_table_list list of named html tables (set the name in the list to be the table name)
#' @param numbers vector of the numbers for the html tables (must match length of html_table_list)
#' @param file_name place to store html file
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' metrc_basic_report_numbers()
#' }
metrc_basic_report_numbers <- function(html_table_list, numbers, file_name){
  injected_tags <- '\n<link rel="shortcut icon" href="#"><meta name="viewport" content="width=device-width, initial-scale=1">\n<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.css" />\n'
  style_text <- '// this is a simple bit of CSS that makes the tables and text in a report look refined\nbody {\n  font-family: "Times New Roman", Times, serif;\n}\ntable.table-basic {\n  font-family: "Times New Roman", Times, serif;\n  width: 100%;\n  text-align: center;\n}\ntable.table-basic td, table.table-basic th {\n  padding: 10px;\n}\ntable.table-basic tbody td {\n  font-size: 13px;\n}\ntable.table-basic tbody tr {\n    border-bottom: 1px solid black;\n    border-top: 1px solid black;\n    border-collapse: collapse;\n}\n'
  injected_style_text <- 'table{\n    border-collapse:collapse;\n}\ntr td{\n    page-break-inside: avoid;\n    white-space: nowrap;\n}'
  html_page <- paste('<!DOCTYPE html>\n<html>\n<head>',injected_tags,'<style>\n',style_text,injected_style_text,'</style></head>\n<br/>',sep='')
  
  first <- TRUE
  table_names <- names(html_table_list)
  for(i in seq(1:length(html_table_list))){
    table_html <- ""
    if(!first){
      table_html <- paste0(table_html, '\n<p style="page-break-after: always;">&nbsp;</p><br/>\n')
    }
    first <- FALSE
    input_table <- html_table_list[i]
    input_table <- str_replace_all(input_table, 'table table-striped"', 'table-basic" border=1 frame=hsides rules=rows')
    input_table <- str_replace_all(input_table, '#ddd', 'black')
    input_table <- gsub('([a-z])\\.','\\1 ',input_table)
    table_html <- paste0(table_html, '<h3>Table ',
                         ifelse(!str_detect(numbers[i],"\\."),paste0(numbers[i],'. '), paste0(numbers[i],' ')),
                         table_names[i],'</h3>\n<br/>\n',input_table,'\n<br/>\n')
    html_page <- paste0(html_page,"\n",table_html)
  }
  
  html_page <- paste(html_page,'\n\n</body>\n</html>')
  write_file(html_page, file_name)
}


#' METRC Basic Report Numbers and Labels
#'
#' @description This function gets the payments for a study using the stored method
#'
#' @param html_table_list list of named html tables (set the name in the list to be the table name)
#' @param numbers vector of the numbers for the html tables (must match length of html_table_list)
#' @param labels vector of the labels for the html tables (ex. Table or Figure)
#' @param file_name place to store html file
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' metrc_basic_report_numbers_labels()
#' }
metrc_basic_report_numbers_labels <- function(html_table_list, numbers, labels, file_name){
  injected_tags <- '\n<link rel="shortcut icon" href="#"><meta name="viewport" content="width=device-width, initial-scale=1">\n<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.css" />\n'
  style_text <- '// this is a simple bit of CSS that makes the tables and text in a report look refined\nbody {\n  font-family: "Times New Roman", Times, serif;\n}\ntable.table-basic {\n  font-family: "Times New Roman", Times, serif;\n  width: 100%;\n  text-align: center;\n}\ntable.table-basic td, table.table-basic th {\n  padding: 10px;\n}\ntable.table-basic tbody td {\n  font-size: 13px;\n}\ntable.table-basic tbody tr {\n    border-bottom: 1px solid black;\n    border-top: 1px solid black;\n    border-collapse: collapse;\n}\ntable.table-basic tfoot tr td {\n    text-align: left;\n}\n'
  injected_style_text <- 'table{\n    border-collapse:collapse;\n}\ntr td{\n    page-break-inside: avoid;\n    white-space: nowrap;\n}'
  html_page <- paste('<!DOCTYPE html>\n<html>\n<head>',injected_tags,'<style>\n',style_text,injected_style_text,'</style></head>\n<br/>',sep='')
  
  first <- TRUE
  table_names <- names(html_table_list)
  for(i in seq(1:length(html_table_list))){
    table_html <- ""
    if(!first){
      table_html <- paste0(table_html, '\n<p style="page-break-after: always;">&nbsp;</p><br/>\n')
    }
    first <- FALSE
    input_table <- html_table_list[i]
    input_table <- str_replace_all(input_table, 'table table-striped"', 'table-basic" border=1 frame=hsides rules=rows')
    input_table <- str_replace_all(input_table, '#ddd', 'black')
    input_table <- gsub('([a-z])\\.','\\1 ',input_table)
    table_html <- paste0(table_html, '<h3>', labels[i],' ',
                         ifelse(!str_detect(numbers[i],"\\."),paste0(numbers[i],'. '), paste0(numbers[i],' ')),
                         table_names[i],'</h3>\n<br/>\n',input_table,'\n<br/>\n')
    html_page <- paste0(html_page,"\n",table_html)
  }
  
  html_page <- paste(html_page,'\n\n</body>\n</html>')
  write_file(html_page, file_name)
}




#' Add Image Footnote
#'
#' @description This function adds footnotes to images
#'
#' @param img_tag image tag that needs the footnote
#' @param footnotes the vector of the footnotes to add
#' @param notation number or alphabet mode for the superscript
#'
#' @return html string of characters
#' @export
#'
#' @examples
#' \dontrun{
#' add_image_footnote()
#' }
add_image_footnote <- function (img_tag, footnotes, notation = "number") 
{
  getMarker <- function(i, mode) {
    if (mode == "number") {
      return(as.character(i))
    }
    else {
      return(tolower(LETTERS[i]))
    }
  }
  wrapWithDiv <- function(content) {
    return(sprintf("<div style=\"text-align: left;\">%s</div>", 
                   content))
  }
  footnoteText <- ""
  for (i in 1:length(footnotes)) {
    marker <- getMarker(i, notation)
    footnoteText <- paste(footnoteText, sprintf("<sup>%s</sup>%s", 
                                                marker, footnotes[i]), sep = "<br>")
  }
  combinedContent <- paste(img_tag, wrapWithDiv(footnoteText))
  return(combinedContent)
}


#' Wrap HTML snippet in a figure tag with an optional caption and number
#'
#' This function takes an HTML snippet and wraps it in a `<figure>` tag. If a 
#' caption is provided, it will be added within a `<figcaption>` tag. If 
#' `caption_number` is provided, the caption will be prefixed with 
#' "Figure [caption_number]: ".
#'
#' @param html A character string containing the HTML snippet to be wrapped.
#' @param caption An optional character string for the figure caption. If `NULL`,
#' no caption will be added.
#' @param caption_number An optional integer or string to prefix the caption 
#' with a number.
#'
#' @return The function does not return a value but prints the HTML code with 
#' the figure and caption tags.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' figure()
#' }
figure <- function(html, caption = NULL, caption_number = NULL) {
  # Start building the figure HTML
  figure_html <- "<figure>"
  
  # Add the HTML content
  figure_html <- paste0(figure_html, html)
  
  # If a caption is provided, add it within a <figcaption> tag
  if (!is.null(caption)) {
    if (!is.null(caption_number)) {
      caption <- paste0("<b>Figure ", caption_number, ":</b> ", caption)
    }
    figure_html <- paste0(figure_html, "<figcaption>", caption, "</figcaption>")
  }
  
  # Close the figure tag
  figure_html <- paste0(figure_html, "</figure>")
  
  # Output the complete figure HTML
  cat(figure_html)
}

#' Add a caption to an HTML table snippet with an optional number
#'
#' This function takes an HTML snippet containing a table and adds a `<caption>` 
#' tag to it. If a caption is provided, it will be inserted after the opening 
#' `<table>` tag. If `caption_number` is provided, the caption will be prefixed 
#' with "Table [caption_number]: ". If no caption is provided, the function 
#' outputs the snippet as is.
#'
#' @param html A character string containing the HTML table snippet.
#' @param caption An optional character string for the table caption. If `NULL`,
#' no caption will be added.
#' @param caption_number An optional integer or string to prefix the caption 
#' with a number.
#' 
#' @export
#'
#' @return The function does not return a value but prints the modified HTML 
#' code with the added caption.
#'
#' @examples
#' \dontrun{
#' table()
#' }
table <- function(html, caption = NULL, caption_number = NULL) {
  # If a caption is provided, add it after the opening <table> tag
  if (!is.null(caption)) {
    # Find the position of the first <table> tag and remove it
    html <- sub("<table>", "", html, fixed = TRUE)
    
    # Prefix the caption with the caption number if provided
    if (!is.null(caption_number)) {
      caption <- paste0("<b>Table ", caption_number, ":</b> ", caption)
    }
    
    # Insert the <caption> tag after the opening <table> tag
    table_html <- paste0("<table><caption>", caption, "</caption>", html)
  } else {
    # If no caption is provided, output the HTML as is
    table_html <- html
  }
  
  # Output the complete table HTML
  cat(table_html)
}


#' Install Default Files for R Markdown Support
#'
#' @description Installs all files needed to support R Markdown files instead of R scripts
#'
#' @param target_path install path
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' install_default_files()
#' }
install_default_files <- function(target_path){
  cp_files <- c(system.file("css_files", "latex-css", "style.css", package = "VisualizationTools"),
                system.file("css_files", "override.css", package = "VisualizationTools"),
                system.file("css_files", "print.css", package = "VisualizationTools"),
                system.file("html_templates", "html-template.html", package = "VisualizationTools"))
  cp_files <- cp_files[cp_files!=""]
  for(cp_file in cp_files){
    cp_target <- file.path(target_path,basename(cp_file))
    file.copy(cp_file, cp_target)
  }
}
