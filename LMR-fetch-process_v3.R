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

## GET FUNCTIONS
source("functions/ldb_extract_functions.R")

## this table used to merge in end dates at end in TIDY section
ldb_fy <- fn_ldbfy()

## PROCESS DESCR. ####
## LDB QMR has pages with single table per page, different topic for each table, standard format/layout.
## Process below works by:
## 0. Identify link and download PDF.
## 1. Identify page with desired table and grab the whole table
## 2. extract, parse out column headings
## 3. process first section: parse out content for rows in section
## 4. process next section - add on to first
## 5. repeat for additional sections - add on to previous
## 6. convert to data frame: above processing creates matrices, incl. final combined matrix
## 7. tidy - format into long tidy structure

## GET DATA ####
## START manual section ####
## > SET SPECS ####
## SPECIFY LINK AND DESIRED FILE NAME: needed for each issue
## find link at: https://www.bcldb.com/publications/liquor-market-review 
## LINK
#furl <- "https://www.bcldb.com/files/Liquor_Market_Review_F22_23_Q3_December_2022.pdf"
#furl <- "https://www.bcldb.com/files/Liquor_Market_Review_F22_23_Q2_September_2022.pdf"

## FILENAME for saving
fname <- "LMR_2022_06.pdf"
## **END manual section ####

## IMPORT PDF ####
## Import if previously downloadable
## Download, save, import if not already
lmr <- fn_lmr(fname)
## VIEW selected pages
## table of contents 
#cat(lmr[2])
## intro incl notes on FISCAL YR (Apr 1 - Mar 31)
#cat(lmr[3])

## INFO ON TABLES
## keep track of info related to different tables in LDB report
tbls_info <- fn_tbls_info()

## 1. EXTRACT TBL ####
## SPECIFY TOPIC of interest
data_topic <- "beer volume"
tbl_info <- tbls_info %>% filter(topic==data_topic)

## > GET TABLE
## isolate actual table content only (remove additional text)
tbl <- fn_tbl_content(tbl_info)

## 2. COL HEADINGS ####
## - col headings for table
## isolate data column headings: Fiscal 2020/21 Q4, etc
## remove misc title, take out excess whitespace, add marker for splitting, split into fixed cols
## headings df ready to be used below - just need to remove first row
tbl_heading <- fn_tbl_heading(tbl)
## REMOVE headings from tbl
tbl_nh <- tbl[-1] 

## 3. REMOVE SUMMARY ROWS ####
## remove Summary rows
tbl_ns <- fn_smry_rows(tbl_nh)

## 3. REMOVE FIRST COL ####
tbl_nfc <- fn_first_col(tbl_ns)

## 4. SPLIT TO COLS AND CLEAN ####
tbl_split_cols <- fn_split_cols(tbl_nfc)

## 5. CONV. TO DF, SET COL NAMES, CLEAN ####
tbl_df <- fn_tbl_dataframe(tbl_split_cols)

## 6. ADD BACK CATEGORIES ####
tbl_df_cat <- fn_first_col_return(tbl_df, tbl_info)

## 7. TIDY structure ####
tbl_df_t <- fn_tidy_structure(tbl_df_cat)

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

## Explore ####
## breakdown by main categories
## sales numbers
tbl_df_t %>% ggplot(aes(x=period, y=netsales, fill=category))+geom_col()+
  scale_y_continuous(labels=comma)
## % of total by category
tbl_df_t %>% ggplot(aes(x=period, y=net_sales, fill=category))+geom_col(position='fill')+
  scale_y_continuous(labels=percent_format())
## observation: domestic - bc beer is dominant and stable ~75%
## zoom in on domestic - bc beer
tbl_df_t %>% filter(category=='Domestic - BC Beer ') %>% 
  ggplot(aes(x=period, y=net_sales, fill=subcategory))+geom_col()+
  scale_y_continuous(labels=comma)
## % of total by category
tbl_df_t %>% filter(category=='Domestic - BC Beer ') %>% 
  ggplot(aes(x=period, y=net_sales, fill=subcategory))+geom_col(position='fill')+
  scale_y_continuous(labels=percent_format())
## observation: also stable, dominated by commercial at ~60%; micro brew ~15%