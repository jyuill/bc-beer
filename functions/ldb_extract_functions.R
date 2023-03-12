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
## - 
fn_tbls_rows <- function(){
  tbls_rows <- tribble(
    ~topic,~st_row,~end_row,
    "Beer Sales (Net $)", 4,19,
    "Beer Sales (Litres)", 4,19
  )
  tbls_rows
}

fn_tbl_content <- function(tbl_pg, tbls_rows, tbl_name){
  ## 
  tbl_pg_rows <- strsplit(tbl_pg, "\n")
  ## > GET TABLE ####
  ## isolate actual table content only (remove additional text)
  tbl_rows <- tbls_rows %>% filter(topic==tbl_name)
  tbl <- tbl_pg_rows[[1]][tbl_rows$st_row:tbl_rows$end_row]
  tbl
}

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
  tbl_heading
}

fn_smry_rows <- function(tbl){
  ## remove Summary rows
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