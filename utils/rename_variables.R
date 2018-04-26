

renames_clusters <- function(data){
  library(dplyr)
  # RENAMING VARIABLES FOR NICE PRINTING
  data <- 
    data %>% 
    rename(`clusters` = clusters
         ,`Av Quarterly Volume (#)` = Quarterly_Volume
         , `Av Quarterly Revenue($)`= Quarterly_Revenue
         , `Av Quarterly Profit ($)`= Quarterly_Profit
         , `Av Quarterly COGS ($)`= Quarterly_COGS
         , `Av Quarterly Count of Trsxs(#)`= Quarterly_count_of_trxs
         , `Av Quarterly Returns ($)`= Quarterly_returns
         , `Av YoY Quarterly Volume (%)` = Quarterly_Volume_yoy
         , `Av YoY Quarterly Revenue (%)` = Quarterly_Revenue_yoy
         , `Av YoY Quarterly Profit (%)` = Quarterly_Profit_yoy
         , `Av YoY Quarterly COGS (%)` = Quarterly_COGS_yoy
         , `Av YoY Quarterly Trsxs (%)` = Quarterly_count_of_trxs_yoy
         , `Av YoY Quarterly Returns (%)` = Quarterly_returns_yoy
         , `Transacts in Q1 (%)` = `Percent Q1`
         , `Transacts in Q2 (%)` = `Percent Q2`
         , `Transacts in Q3 (%)` = `Percent Q3`
         , `Transacts in Q4 (%)` = `Percent Q4`)
  
  data
}