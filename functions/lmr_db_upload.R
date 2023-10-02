## Upload LMR data gathered from pdf report to MySQL database

library(tidyverse)
library(lubridate)
library(scales)
library(glue)
library(readr) ## for easy conversion of $ characters to numeric
library(RMariaDB) ## for MySQL

## credentials ####
## from file in .gitignore
source('credo.R')

## SET parameters for function
mysql_tbl <- "bcbg.tblLDB_lmr"
tbl_upload <- tables_all_fyqtr ## table produced frm LMR-fetch-process-all_vX.R

## RUN below function
fn_db_upload(mysql_tbl, tbl_upload)

## FUNCTION
## set up as function for flexbility later - may want to call from other files/processes
fn_db_upload <- function(mysql_tbl, tbl_upload) {
    # connection needed for upload
    con <- dbConnect(RMariaDB::MariaDB(), user='root', password=mypwd, dbname='bcbg')
    # test
    #dbGetQuery(con, "SELECT * FROM tblLDB_lmr;")
    
    for(r in 1:nrow(tbl_upload)) {
      ## delete any existing match
      dbGetQuery(con, glue("DELETE FROM {mysql_tbl}
                           WHERE fy_qtr='{tbl_upload$fy_qtr[r]}' AND
                           cat_type='{tbl_upload$cat_type[r]}' AND
                           category='{tbl_upload$category[r]}' AND
                           subcategory='{tbl_upload$subcategory[r]}';"))
      ## insert row from new data
      dbGetQuery(con, glue("INSERT INTO {mysql_tbl} (
                fy_qtr,
                cat_type,
                category,
                subcategory,
                litres,
                netsales
              ) 
              VALUES('{tbl_upload$fy_qtr[r]}',
              '{tbl_upload$cat_type[r]}',
              '{tbl_upload$category[r]}',
              '{tbl_upload$subcategory[r]}',
              {tbl_upload$litres[r]},
              {tbl_upload$netsales[r]}
              );"))
    }
    ## always disconnect when done
    dbDisconnect(con)
}
