## functions for pulling tabular data out of LDB Quarterly Reports

library(pdftools)
library(tidyverse)
library(lubridate)
library(scales)
library(glue)
library(readr) ## for easy conversion of $ characters to numeric

## FY quarters
## cols use reference to fiscal quarters; BC LDB fy quarters align with end dates
fn_ldbfy <- function(){
  ldb_fy <- tribble(
    ~qtr,~end_dt,
    "Q1", "06-30",
    "Q2", "09-30",
    "Q3", "12-31",
    "Q4", "03-01"
  )
  ldb_fy
}
## get data - download if not already
fn_lmr <- function(fname){
  pdf_file <- paste0("input/",fname)
  ## if file doesn't exist download it, otherwise import
  if(!file.exists(pdf_file)){
    download.file(furl, pdf_file, mode='wd')
    lmr <- pdf_text(pdf_file)
  } else {
    lmr <- pdf_text(pdf_file)
  }
}

## table for keeping track of number of rows for each table
fn_tbls_info <- function(){
  tbls_info <- tribble(
    ~topic, ~ldb_table, ~pg, ~st_row,~end_row, ~ref_file, ~mysql_tbl,
    "beer sales","Beer Sales (Net $)", 5, 4, 19, "beer_sales", "tblLDB_beer_sales",
    "beer volume", "Beer Sales (Litres)", 7, 4, 19, "beer_volume","tblLDB_beer_volume"
  )
  tbls_info
}
## extract tbl content from pg
fn_tbl_content <- function(tbl_info){
  tbl_name <- tbl_info$ldb_table
  pg_num <- tbl_info$pg
  ## get page content from PDF 
  tbl_pg <- lmr[pg_num]
  
  tbl_pg_rows <- strsplit(tbl_pg, "\n")
  ## > GET TABLE
  ## isolate actual table content only (remove additional text)
  tbl <- tbl_pg_rows[[1]][tbl_info$st_row:tbl_info$end_row]
  tbl
}

## extract table headings - add back later
fn_tbl_heading <- function(tbl){
  tbl_heading <- tbl[1]
  tbl_heading <- str_squish(tbl_heading)
  tbl_heading <- str_replace_all(tbl_heading,"F","xF")
  tbl_heading <- str_split_fixed(tbl_heading, "x",6)
  ## basic clean result - can be used with any sub-sections of table
  tbl_heading ## individual items - some trailing sp
  ## add category and subcategory items to align with body of table
  tbl_heading[1] <- "subcategory"
  tbl_heading <- cbind("category",tbl_heading)
  ## clean up leading / trailing spaces
  tbl_heading <- str_squish(tbl_heading)
  tbl_heading
}

## remove Summary rows - not relevant
fn_smry_rows <- function(tbl){
  ## cycle through and identify row numbers of summary rows
  sr <- NULL
  for(s in 1:length(tbl)){
    if(str_detect(tbl[s], "Summary")){
      sr = c(sr,s)
    }
  }
  ## table with no summary rows
  tbl_nsmry <- tbl[c(sr*-1)] ## remove rows identified above
  tbl_nsmry
}

## REMOVE FIRST COL: manual values here but should create lookup db table for category map to subcategory
## - could also reference previous data saved as .csv, based on table info above
fn_first_col <- function(tbl){
  for(c in 1:length(tbl)){
    tbl[c] <- case_when(
      str_detect(tbl[c],"Domestic - BC Beer") ~ str_replace(tbl[c],"Domestic - BC Beer",""),
      str_detect(tbl[c],"Domestic - Other Province Beer") ~ str_replace(tbl[c],"Domestic - Other Province Beer",""),
      str_detect(tbl[c],"Import Beer") ~ str_replace(tbl[c],"Import Beer",""),
      TRUE ~ tbl[c]
    )
    ## take out empty white space
    tbl[c] <- str_squish(tbl[c])
  }
  tbl
}

## SPLIT into cols
fn_split_cols <- function(tbl){
  if(str_detect(tbl[1],"\\$")){
    ## split at $
    tbl_split_cols <- str_split_fixed(tbl, "\\$", n=6)
  } else {
    ## split based on number
    ## first two cols to separate text from numbers
    tbl_split <- str_split_fixed(tbl_nfc, "(?<=\\D)(?=\\d)", n=2)
    ## split number cols
    tbl_split_num <- str_split_fixed(tbl_split[,2], " ", n=5) 
    ## reunite first col with number cols 
    tbl_split_cols <- cbind(tbl_split[,1], tbl_split_num)
  }
  tbl_split_cols <- apply(tbl_split_cols, 2, str_squish)
  tbl_split_cols
}

fn_tbl_dataframe <- function(tbl){
  tbl_df <- data.frame(tbl)
  ## > col names from headings taken earlier####
  ## set col names based on row 1
  colnames(tbl_df) <- tbl_heading[-1] ## excluding first 'category' heading
  
  ## convert numbers in char form to numeric
  startcol <- 2 ## first col to convert
  for(i in startcol:ncol(tbl_df)){
    tbl_df[,i] <- parse_number(tbl_df[,i])
  }
  ## > col cleanup ####
  ## clean column names for simplicity and to avoid probs later
  startcol <- 2 ## first col to clean up
  for(i in startcol:ncol(tbl_df)){
    colnames(tbl_df)[i] <- str_replace(colnames(tbl_df)[i], 
                                       substr(colnames(tbl_df)[i],
                                              start=10, stop=12),"")
  }
  ## finish clean up col names for simplicity, avoid probs later
  colnames(tbl_df) <- str_replace_all(colnames(tbl_df),"Fiscal","FY")
  tbl_df
}

fn_first_col_return <- function(tbl, tbl_info){
  ## existing file to get categories from
  tbl_path <- tbl_info$ref_file
  tbl_cat <- read_csv(paste0('input/',tbl_path,'.csv'))
  ## get list of categories and subcategories
  categories <- tbl_cat %>% group_by(category, subcategory) %>% summarize(count=n()) %>%
    select(-count)
  ## add categories and reorder table
  tbl_df_cat <- full_join(tbl, categories, by=c('subcategory')) %>%
    select('category','subcategory',c(2:ncol(tbl))) ## ncol(tbl_df) for number of cols in original tbl
  tbl_df_cat
}

fn_tidy_structure <- function(tbl){
  ## > convert to long ####
  ## convert table with metrics in multiple rows to tidy format
  tbl_df_t <- tbl %>% pivot_longer(cols=c(3:ncol(tbl)), names_to='period', values_to='netsales')
  ## > add cols ####
  ## add cols for period info
  tbl_df_t <- tbl_df_t %>% mutate(
    qtr=substr(period, start=9, stop=10),
    fyr=as.numeric(substr(period, start=4, stop=7)),
    cyr=ifelse(qtr=='Q4',fyr, fyr-1)
  )
  ## add end date (month - day) for quarters
  tbl_df_t <- full_join(tbl_df_t, ldb_fy, by='qtr')
  ## add end date yr for time series
  tbl_df_t <- tbl_df_t %>% mutate(
    end_qtr_dt=date(paste(cyr,end_dt, sep="-"))
  )
  tbl_df_t
}