setwd('~/Desktop/cram')

# set PAT key to access private githubs
Sys.setenv(GITHUB_PAT='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')
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)

devtools::install_github("metrc/AnalyticSystem@*release",   upgrade="never", dependencies="Depends")
devtools::install_github("metrc/AnalyticCodebase@*release",   upgrade="never", dependencies="Depends")
library(AnalyticCodebase)

# Set up for Weight Bearing

set_data("weight bearing","21E519A64998D63044E31730DE77E4E2")

analytic <- build_analytic_dataset()

df <- analytic %>% 
  select(mrr_status_6wk, mrr_status_3mo, mrr_status_6mo, mrr_status_12mo, cfu_status_6wk, cfu_status_3mo, cfu_status_6mo, 
         cfu_status_12mo, pfu_status_6wk, pfu_status_3mo, pfu_status_6mo, pfu_status_12mo, bpi_status_6wk, bpi_status_3mo, bpi_status_6mo, 
         bpi_status_12mo, aos_status_6wk, aos_status_3mo, aos_status_6mo, aos_status_12mo, koos_status_6wk, koos_status_3mo, koos_status_6mo, 
         koos_status_12mo)


output <- count_split_cols_long_and_wide(df, '_status_') %>%
  rename(Form=prefix, Status=level)

summed_completes <- summate_levels(output, 'Form', 'Status', c('Complete: Out of Window', 'Complete: In window')) %>%
  mutate(Status='Complete')
output <- output %>%
  filter(Status!='Complete: In window') %>%
  bind_rows(summed_completes)

df_table_raw <- reorder_rows(output, list('Form'=c('mrr', 'cfu', 'pfu', 'bpi', 'aos', 'koos'), 
                                          'Status'=c('Complete', 'Complete: Out of Window', 'Incomplete', 'Missing'))) %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  select(Form, Status, `6wk`, `3mo`, `6mo`, `12mo`) %>% 
  mutate(Form = toupper(Form)) %>% 
  rename("12 Months"=`12mo`, "3 Months"=`3mo`, "6 Months"=`6mo`,  "6 Weeks"=`6wk`)

index_vec <- c("MRR"=4,"CFU"=4, "PFU"=4, "BPI"=4, "AOS"=4, "KOOS"=4)

df_for_table <- df_table_raw %>%  ungroup() %>% select(-Form) %>% rename("Form Completion"=Status)

table_raw<- kable(df_for_table, align='l', padding='2l') %>%
  pack_rows(index = index_vec)


table_raw <- str_replace_all(table_raw, 'table table-striped"', 'table-basic" border=1 frame=hsides rules=rows')
table_raw <- str_replace_all(table_raw, '#ddd', 'black')