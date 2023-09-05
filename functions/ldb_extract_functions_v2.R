## functions for pulling tabular data out of LDB Quarterly Reports

library(pdftools)
library(tidyverse)
library(lubridate)
library(scales)
library(glue)
library(readr) ## for easy conversion of $ characters to numeric

## get data - download if not already
fn_lmr <- function(furl){
  ## convert URL to filename in standard format so don't have to specify
  fname_url <- str_split(furl,"/")
  fname_url2 <- str_split(fname_url[[1]][5],"_")
  fname_url_qtr <- paste0("FY",fname_url2[[1]][5],fname_url2[[1]][6])
  fname_url_yr <- str_split(fname_url2[[1]][8], "\\.")[[1]][1]
  fname_url_mth <- case_when(
    fname_url2[[1]][7] == 'March' ~ '03',
    fname_url2[[1]][7] == 'June' ~ '06',
    fname_url2[[1]][7] == 'September' ~ '09',
    fname_url2[[1]][7] == 'December' ~ '12',
  )
  fname <- paste0("LMR_",fname_url_yr,"_",fname_url_mth,"_",fname_url_qtr,".pdf")
  pdf_file <- paste0("input/",fname)
  ## if file doesn't exist download it, otherwise import
  if(!file.exists(pdf_file)){
    download.file(furl, pdf_file, mode='wd')
    lmr <- pdf_text(pdf_file)
  } else {
    lmr <- pdf_text(pdf_file)
  }
  return(list(lmr, fname))
}

## get meta data from pg
fn_pg_meta <- function(tbl_pg_rows, pg_num){
  ## > get META data ####
  tbl_name <- trimws(tbl_pg_rows[1])
  
  tbl_name_clean <- str_replace_all(tbl_name,"\\(","")
  tbl_name_clean <- str_replace_all(tbl_name_clean,"\\)","")
  tbl_name_clean <- str_replace_all(tbl_name_clean,"\\$","")
  tbl_name_clean <- trimws(tbl_name_clean, "right")
  tbl_name_clean <- str_replace_all(tbl_name_clean," ","_")
  cat("pg:",pg_num,'tbl:', tbl_name_clean,"\n")
  ## check for prev same table name
  if(exists('tbl_name_prev')){
    if(tbl_name!=tbl_name_prev){
      tbl_cat_type <- tbl_pg_rows[3]
      hrow <- 4
    } else { ## if table name same as prev, use prev cat_type
      tbl_cat_type <- tbl_cat_type
      hrow <- 2
    }
  } else {
    tbl_cat_type <- tbl_pg_rows[3]
    hrow <- 4
  }
  ## get table metric
  tbl_metric <- unlist(str_split(tbl_pg_rows[hrow], "\\s{2,}"))[2]
  tbl_metric <- case_when(
    tbl_metric=='Net Sales $' ~ 'netsales',
    tbl_metric=='Litres' ~ 'litres'
  )
  cat('metric:',tbl_metric,"\n")
  
  return(list(tbl_name_clean, tbl_cat_type, tbl_metric))
}

## extract tbl content from pg
fn_tbl_content <- function(tbl_pg_rows, tbl_meta){
  cat("start content function \n")
  ## unpack meta data
  tbl_name <- tbl_meta[[1]]
  tbl_cat_type <- tbl_meta[[2]]
  tbl_metric <- tbl_meta[[3]]
  ## > RM SUMMARY ROWS ####
  cat("remove summary rows \n")
  srows <- NULL
  for(r in 1:length(tbl_pg_rows)){
    if(str_detect(tbl_pg_rows[r],"Summary")==TRUE){
      srow <- r
      srows <- c(srows,srow)
    }
  }
  tbl_pg_rows_ns <- tbl_pg_rows[c(-srows)]
  
  ## > TABLE + HEADING ####
  cat("get headings, set up table \n")
  tbl_heading <- unlist(str_split(tbl_pg_rows_ns[hrow], "\\s{2,}"))[c(-1,-2)] # drop first two: empty, metric name
  first_cols <- c("cat_type","category","subcategory")
  tbl_heading <- c(first_cols, tbl_heading)
  tbl <- data.frame(matrix(ncol=length(tbl_heading)))
  colnames(tbl) <- tbl_heading
  
  ## > PROCESS DATA ####
  ## get data from each row and add to table
  cat("process data \n")
  start <- hrow+1
  tbl_all <- data.frame()
  for(r in start:length(tbl_pg_rows_ns)){
    cat("row: ", r, "\n")
    row_content <- unlist(str_split(trimws(tbl_pg_rows_ns[r]), "\\s{2,}"))
    col_nums <- length(row_content)
    cat("col nums:", col_nums, "\n")
    if(col_nums>5){ ## only rows with data
      tr <- r-hrow ## set table row number by subtract pdf start num
      tbl[tr,"cat_type"] <- tbl_cat_type
      if(col_nums==7){
        tbl$category[tr] <- row_content[1]
        tbl$subcategory[tr] <- row_content[2]
        tbl[tr,c(4:(col_nums+1))] <- row_content[c(3:col_nums)]
      } else {
        ## get category from prev row
        tbl$category[tr] <- tbl$category[tr-1]
        tbl$subcategory[tr] <- row_content[1]
        tbl[tr,c(4:(col_nums+2))] <- row_content[c(2:col_nums)]
      }
      tbl_all <- bind_rows(tbl_all, tbl)
    }
  } ## end single page loop
  
  ## TIDY FORMAT ####
  tbl_long <- fn_tidy_structure(tbl_all, tbl_metric)
  
  ## save table name for comparison with next table
  ## - in some cases, tables run over multiple pages
  tbl_name_prev <- tbl_name
  
  ## return tbl: wide and long
  return(list(tbl_all, tbl_long))
}

## extend to long format
fn_tidy_structure <- function(tbl, tbl_metric){
  cat("tidy process for tbl_long \n")
  ## > convert to long ####
  ## convert table with metrics in multiple rows to tidy format
  tbl_long <- tbl %>% pivot_longer(cols=c(4:ncol(tbl)), names_to='period', values_to=tbl_metric)
  tbl_long <- tbl_long %>% mutate(
    !!tbl_metric := parse_number(!!sym(tbl_metric))
  )
  tbl_long
}