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
library(readr) ## for easy conversion of $ characters to numeric

## PROCESS DESCR. ####
## LDB QMR has pages with single table per page, different topic for each table, standard format/layout.
## Process below works by:
## 0. Identify link and download PDF.
## 1. Get meta data, determine pages with tables.
## 1. Go through each page, determine if table or other content (based on table patterns used).
## 2. If table pg: parse out category type, metric ($ or vol).
## 3. Parse out col headings.

## 3. process first section: parse out content for rows in section
## 4. process next section - add on to first
## 5. repeat for additional sections - add on to previous
## 6. convert to data frame: above processing creates matrices, incl. final combined matrix
## 7. tidy - format into long tidy structure

## PROCESS START ####
## GET FUNCTIONS
source("functions/ldb_extract_functions_v2.R")

## INPUT: LINK TO PDF ####
## SPECIFY LINK AND DESIRED FILE NAME: needed for each issue
## find link at: https://www.bcldb.com/publications/liquor-market-review 
## LINK URL - only manual input needed; rest is automated
furl <- "https://www.bcldb.com/files/Liquor_Market_Review_F22_23_Q4_March_2023_NEW.pdf"

## IMPORT PDF ####
## Function to:
## - Import if previously downloaded
## - Download, save, import if not already, based on URL above
lmr <- fn_lmr(furl)
lmr_name <- unlist(lmr[2])
lmr_name_clean <- str_remove(lmr_name,"\\.pdf")
lmr <- unlist(lmr[1])
## - see 'pdftools-explore.R' for different ways to access page info
## get report meta info
title_pg <- unlist(strsplit(lmr[1], "\n"))
title_pg_dt <- str_replace(trimws(title_pg[8])," ","_")

## 1. Look at each page to determine which ones have tables
## can skip the first 3 pgs - always cover, toc, intro
tables_all_netsales <- data.frame()
tables_all_litres <- data.frame()
for(p in 4:length(lmr)){
  ## test for 'Item Subcategory' -> identifies chart pages
  if(str_detect(lmr[p],"Item Subcategory")){
    cat(p, "chart pg \n")  
    } else if(str_detect(lmr[p], regex("Glossary", ignore_case = TRUE))){
      cat(p, "glossary page \n")
    } else { ## do the main thing
      cat(p, "tbl pg: processing \n")
      ## get page content from PDF 
      tbl_pg <- lmr[p]
      tbl_pg_rows <- unlist(strsplit(tbl_pg, "\n"))
      
      ## get meta data ####
      tbl_meta <- fn_pg_meta(tbl_pg_rows, p)
      tbl_name_clean <- tbl_meta[[1]]
      
      ## process content - pass in page content in rows, along with meta data
      page_data_tbls <- fn_tbl_content(tbl_pg_rows, tbl_meta)
      
      ## save results for page - identifying report and table -> wide and long
      tbl_wide <- page_data_tbls[[1]]
      tbl_long <- page_data_tbls[[2]]
      write_csv(tbl_wide, paste0('data/',lmr_name_clean,"-",tbl_name_clean,".csv"))
      write_csv(tbl_long, paste0('data/',lmr_name_clean,"-",tbl_name_clean,"_long.csv"))
      
      ## add to existing data - depending on metric
      if(tbl_meta[[3]]=='netsales'){
        tables_all_netsales <- bind_rows(tables_all_netsales, tbl_long)
      } else {
        tables_all_litres <- bind_rows(tables_all_litres, tbl_long)
      }
    } ## end of the main page loop action
} ## end page loop
## COMBINE netsales + litres ####
## - join tables with all pages/tables in each of netsales and litres as metrics
## - join on all fields except metrics

##== old process ####

## 8. SAVE ####
## save latest only - since in db
write_csv(tbl_df_t, paste0('input/',tbl_info$ref_file,'.csv'))
## save wide version with dates for the record
write_csv(tbl_df_cat, paste0('input/',tbl_info$ref_file,'_wide-',str_replace(fname, 'pdf','csv')))

## 9. UPDATE DATABASE ####
## NEED TO CREATE TABLES IN ADVANCE FOR NEW DATA TOPICS
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

## always disconnect when done
dbDisconnect(con)

