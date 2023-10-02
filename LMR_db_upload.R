## Upload LMR data gathered from pdf report to MySQL database

library(tidyverse)
library(lubridate)
library(scales)
library(glue)
library(readr) ## for easy conversion of $ characters to numeric
library(RMariaDB) ## for MySQL

## load functions for MySQL upload
source('functions/lmr_db_functions.R')

## SET parameters for function
mysql_tbl <- "bcbg.tblLDB_lmr"
tbl_upload <- tables_all_fyqtr ## table produced frm LMR-fetch-process-all_vX.R
# alternately, import from saved
#tbl_upload <- read_csv('output/LMR_2020_09_FY21Q2_db_upload.csv')

## check/add data to LDB_quarters tbl -> automatically add if not present
fn_db_qtrs(tbl_upload)

## RUN function to UPLOAD DATA
fn_db_upload(mysql_tbl, tbl_upload)

## CHECK: spot-check data by category
fn_db_check(tbl_upload)
