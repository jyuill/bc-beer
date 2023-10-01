## GET lIQUOR SALES DATA FROM LDB - released quarterly in pdf format
## Requires scraping data tables from within PDF docs
## references:
## pdftools vignette: https://cran.r-project.org/web/packages/pdftools/pdftools.pdf
## pdftools website: https://docs.ropensci.org/pdftools/ 
##  manual: https://docs.ropensci.org/pdftools/reference/pdftools.html

library(pdftools)
library(tidyverse)
library(lubridate)
library(scales)
library(glue)
library(here)
library(readr) ## for easy conversion of $ characters to numeric

## PROCESS DESCR. ####
## LDB QMR has pages with single table per page
## - by category, separate table for net sales $ and litres
## - standard format/layout.
## Process below works by:
## 0. Identify link and download PDF.
## 1. Get report meta data to identify unique report
## 2. Go through each page, determine if table or other content (based on table patterns used).
## 3. If table pg: 
##    >> parse out meta data: category type, metric ($ or vol).
##    >> parse out col headings.
##    >> assemble data from table in std structure
##    >> If table runs across multiple pgs, combine all
## 4. Save individual table data, in both wide and long formats
## 5. Combine with previous tables
## 6. When all pages in report completed, join net sales and litres into 
##    single table for upload to database.
## 7. Upload to database -> delete any existing rows for period and overwrite with latest data.

## PROCESS START ####
## GET FUNCTIONS
source("functions/ldb_extract_functions_v2.R")

## 1. INPUT: LINK TO PDF ####
## SPECIFY LINK AND DESIRED FILE NAME: needed for each issue
## find link at: https://www.bcldb.com/publications/liquor-market-review 
## LINK URL - only manual input needed; rest is automated
furl <- "https://www.bcldb.com/files/Liquor_Market_Review_F22_23_Q4_March_2023_NEW.pdf"

## 2. IMPORT PDF ####
## Function to:
## - Import if previously downloaded
## - Download, save, import if not already, based on URL above
lmr <- fn_lmr(furl)
lmr_name <- unlist(lmr[2])
lmr_name_clean <- str_remove(lmr_name,"\\.pdf")
lmr <- unlist(lmr[1])
## - see 'pdftools-explore.R' for different ways to access page info
## > get report meta info ####
title_pg <- unlist(strsplit(lmr[1], "\n"))
title_pg_dt <- str_replace(trimws(title_pg[8])," ","_")

## 3. COLLECT EACH tbl data ####
## Look at each page to determine which ones have tables
## can skip the first 3 pgs - always cover, toc, intro
## > netsales df and litres df ####
tables_all_netsales <- data.frame()
tables_all_litres <- data.frame()
tbl_name_prev <- "" ## set start prev pg name, used for tables that continue over pgs 
tbl_cat_type <- ""
## > loop through each pg ####
## START LOOP
for(p in 4:length(lmr)){
  ## > identify/skip non-tbl pgs
  ## test for 'Item Subcategory' -> identifies chart pages; similar for Glossary
  if(str_detect(lmr[p],"Item Subcategory")){
    cat(p, "chart pg \n")  
    } else if(str_detect(lmr[p], regex("Glossary", ignore_case = TRUE))){
      cat(p, "glossary page \n")
    } else { ## do the main thing
      cat(p, "tbl pg: processing \n")
      ## > get pg content from PDF ####
      tbl_pg <- lmr[p]
      tbl_pg_rows <- unlist(strsplit(tbl_pg, "\n"))
      
      ## > get meta data ####
      ## table name, category type for processing / saving
      tbl_meta <- fn_pg_meta(tbl_pg_rows, p, tbl_name_prev, tbl_cat_type)
      tbl_name_clean <- tbl_meta[[1]]
      tbl_name_prev <- tbl_name_clean
      tbl_cat_type <- tbl_meta[[3]]
      
      ## > PROCESS tbl ####
      ## process content - pass in page content in rows, along with meta data
      page_data_tbls <- fn_tbl_content(tbl_pg_rows, tbl_meta)

      ## > SAVE: wide, long ####
      ## save results for page - identifying report by name and table -> wide and long versions
      ## confirm that table returned has data -> any more than 1 NA in first row, skip it
      if(rowSums(is.na(page_data_tbls[[1]][1,]))<2){
        ## as shown in report
        tbl_wide <- page_data_tbls[[1]]
        ## pivot long for database
        tbl_long <- page_data_tbls[[2]]
        write_csv(tbl_wide, paste0('data/',lmr_name_clean,"-",tbl_name_clean,".csv"))
        write_csv(tbl_long, paste0('data/',lmr_name_clean,"-",tbl_name_clean,"_long.csv"))
        
        ## > ADD to existing report data ####
        ## add to existing data from prev tables in report - depending on metric
        if(tbl_meta[[4]]=='netsales'){
          tables_all_netsales <- bind_rows(tables_all_netsales, tbl_long)
        } else {
          tables_all_litres <- bind_rows(tables_all_litres, tbl_long)
        }
      } ## end save & append section
    } ## end of the main page loop action
} ## end page loop
## 4. JOIN: netsales + litres ####
## - join tables with all pages/tables in each of netsales and litres as metrics
## - join on all fields except metrics
tables_all <- full_join(tables_all_litres, tables_all_netsales, by=c("cat_type", "category", "subcategory", "period"))

## simplify period format and rename to fy_qtr to match MySQL database
## from 'Fiscal 2021/22 Q4' to 'FY2022Q4'
tables_all_fyqtr <- tables_all %>% mutate(
  fy_qtr = str_replace(period, "Fiscal ", "FY"),
  ## remove two digits for prev yr + '/' ("21/" in example above)
  fy_qtr = str_replace_all(str_remove(fy_qtr, str_sub(fy_qtr, start=5, end=7))," ","")
) %>% select(-period)
## > SAVE joined tbl ####
## table for upload
tbl_save <- here('output',paste0(lmr_name_clean,"_db_upload.csv"))
write_csv(tables_all_fyqtr, here('output',paste0(lmr_name_clean,"_db_upload.csv")))

###== old process ####

## 5. REDO: UPDATE MySQL ####
## NEW TABLE AVAILABLE - NEED TO CHANGE BELOW TO ALIGN WITH NEW ARCHITECTURE
## - once created, ADD to tables info in ldb_extract_functions -> fn_tbls_info
library(RMariaDB) ## NOTE: pwd blocked out for security -> need to add
## > connect ####
## from file in .gitignore
source('credo.R')
con <- dbConnect(RMariaDB::MariaDB(), user='root', password=mypwd, dbname='bcbg')
#dbGetQuery(con, "SELECT * FROM tblLDB_beer_sales;")
## > insert ####
for(i in 1:nrow(tbl_df_t)){
  ## test for existence of row - insert if not already exist
  ## may want to change this to delete existing row from latest data set
  ##. on assumption that newest data is most accurate
  ## - change this from nrow test to DELETE where condition met
  if(nrow(dbGetQuery(con, glue("SELECT * FROM {tbl_info$mysql_tbl}
                       WHERE category='{tbl_df_t$category[i]}' AND
                       subcategory='{tbl_df_t$subcategory[i]}' AND
                       qtr='{tbl_df_t$qtr[i]}' AND
                       fyr='{tbl_df_t$fyr[i]}';")))==0) {
    ## insert query
    dbExecute(con, glue("INSERT INTO {tbl_info$mysql_tbl} (
            category,
            subcategory,
            period,
            netsales,
            qtr,
            fyr,
            cyr,
            end_dt,
            end_qtr_dt
          )
          VALUES('{tbl_df_t$category[i]}',
          '{tbl_df_t$subcategory[i]}',
          '{tbl_df_t$period[i]}',
          {tbl_df_t$netsales[i]},
          '{tbl_df_t$qtr[i]}',
          {tbl_df_t$fyr[i]},
          {tbl_df_t$cyr[i]},
          '{tbl_df_t$end_dt[i]}',
          '{tbl_df_t$end_qtr_dt[i]}'
          );"))
  } ## end test IF
} ## end insert loop
## END OLD 
## NEW QUERY ####
mysql_tbl <- "bcbg.tblLDB_lmr"
for(r in 1:nrow(tables_all_fyqtr)) {
  ## delete any existing match
  dbGetQuery(con, glue("DELETE FROM {mysql_tbl}
                       WHERE ...."))
  ## insert row
  dbGetQuery(con, glue("INSERT "))
}

## always disconnect when done
dbDisconnect(con)

