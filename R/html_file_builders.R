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
  injected_tags <- '\n<meta name="viewport" content="width=device-width, initial-scale=1">\n<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.css" />\n'
  style_text <- '// this is a simple bit of CSS that makes the tables and text in a report look refined\nbody {\n  font-family: "Times New Roman", Times, serif;\nmargin-left: 20px;\n}\ntable.table-basic {\n  font-family: "Times New Roman", Times, serif;\n  width: 100%;\n  text-align: center;\n}\ntable.table-basic td, table.table-basic th {\n  padding: 10px;\n}\ntable.table-basic tbody td {\n  font-size: 13px;\n}\ntable.table-basic tbody tr {\n    border-bottom: 1px solid black;\n    border-top: 1px solid black;\n    border-collapse: collapse;\n}\n'
  injected_style_text <- 'table{\n    border-collapse:collapse;\n}\ntr td{\n    page-break-inside: avoid;\n    white-space: nowrap;\n}'
  html_page <- paste('<!DOCTYPE html>\n<html>\n<head>',get_favicon_tag(),injected_tags,'<style>\n',style_text,injected_style_text,'</style></head>\n<br/>',sep='')
  
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
  injected_tags <- '\n<meta name="viewport" content="width=device-width, initial-scale=1">\n<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.css" />\n'
  style_text <- '// this is a simple bit of CSS that makes the tables and text in a report look refined\nbody {\n  font-family: "Times New Roman", Times, serif;\n}\ntable.table-basic {\n  font-family: "Times New Roman", Times, serif;\n  width: 100%;\n  text-align: center;\n}\ntable.table-basic td, table.table-basic th {\n  padding: 10px;\n}\ntable.table-basic tbody td {\n  font-size: 13px;\n}\ntable.table-basic tbody tr {\n    border-bottom: 1px solid black;\n    border-top: 1px solid black;\n    border-collapse: collapse;\n}\n'
  injected_style_text <- 'table{\n    border-collapse:collapse;\n}\ntr td{\n    page-break-inside: avoid;\n    white-space: nowrap;\n}'
  html_page <- paste('<!DOCTYPE html>\n<html>\n<head>',get_favicon_tag(),injected_tags,'<style>\n',style_text,injected_style_text,'</style></head>\n<br/>',sep='')
  
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
  html_page <- paste('<!DOCTYPE html>\n<html>\n<head>',get_favicon_tag(),injected_tags,'<style>\n',style_text,injected_style_text,'</style></head>\n<br/>',sep='')
  
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
  # Prefix the caption with the caption number if provided
  if (!is.null(caption_number) && !is.null(caption)) {
    caption <- paste0("<b>Figure ", caption_number, ":</b> ", caption)
  }
  
  if (!is.null(caption_number) && is.null(caption)) {
    caption <- paste0("<b>Figure ", caption_number, ":</b> ")
  }
  
  if (is.null(caption_number) && !is.null(caption)) {
    caption <- paste0("<b>Figure:</b> ", caption)
  }
  
  figure_html <- paste0(figure_html, "<figcaption>", caption, "</figcaption>")
  
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
#' @param table_type defaults to 'Table' but can be overrode to say 'Appendix' for example
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
table <- function(html, caption = NULL, caption_number = NULL, table_type = "Table") {
  # If a caption is provided, add it after the opening <table> tag
  if (!is.null(caption)) {
    
    # Prefix the caption with the caption number if provided
    if (!is.null(caption_number) && !is.null(caption)) {
      replacement <- paste0("><caption><b>",table_type," ", caption_number, ":</b> ", caption,"</caption>")
    }
    
    if (!is.null(caption_number) && is.null(caption)) {
      replacement <- paste0("><caption><b>",table_type," ", caption_number, ":</b></caption>")
    }
    
    if (is.null(caption_number) && !is.null(caption)) {
      replacement <- paste0("><caption><b>",table_type, ":</b> ", caption,"</caption>")
    }
    
    table_html <- str_replace(html, ">", replacement)
    
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
  cp_files <- c(system.file("css_files", "style.css", package = "VisualizationTools"),
                system.file("css_files", "override.css", package = "VisualizationTools"),
                system.file("css_files", "print.css", package = "VisualizationTools"),
                system.file("html_templates", "html-template.html", package = "VisualizationTools"))
  cp_files <- cp_files[cp_files!=""]
  for(cp_file in cp_files){
    cp_target <- file.path(target_path,basename(cp_file))
    file.copy(cp_file, cp_target)
  }
}




#' Get Favicon Tag
#'
#' @description gets the favicon tag
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' get_favicon_tag()
#' }
get_favicon_tag <- function(){
  link <- '<link rel="icon" href="data:image/png;base64,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">'
}