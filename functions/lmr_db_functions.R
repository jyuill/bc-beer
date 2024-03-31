## Upload LMR data gathered from pdf report to MySQL database

library(tidyverse)
library(lubridate)
library(scales)
library(glue)
library(readr) ## for easy conversion of $ characters to numeric
library(RMariaDB) ## for MySQL

## Run from LMR_db_upload.R for convenience/functional

## credentials ####
## from file in .gitignore
source('credo.R')

## TEST parameters for function
#mysql_tbl <- "bcbg.tblLDB_lmr"
#tbl_upload <- tables_all_fyqtr ## table produced frm LMR-fetch-process-all_vX.R

## TEST RUN below function
#fn_db_upload(mysql_tbl, tbl_upload)

## CHECK QTRS
## will add latest quarter if not already present
fn_db_qtrs <- function(tbl_upload) {
  #local mysql
  #con <- dbConnect(RMariaDB::MariaDB(), user=l.user, password=l.mypwd, dbname='bcbg')
  # amazon mysql
  con <- dbConnect(RMariaDB::MariaDB(),
                     dbname="bcbg",
                     host=a.endpt,
                     user=a.user,
                     password=a.pwd,
                     port=a.port)
  #dbListTables(con) # check connection
  # get list of quarters covered
  qtrs <- dbGetQuery(con, "SELECT * FROM tblLDB_quarter;")
  
  if(max(qtrs$fy_qtr)<max(tbl_upload$fy_qtr)) {
    ## set up values for new qtr info
    fy_qtr_new <- max(tbl_upload$fy_qtr)
    fyr_new <- as.numeric(str_sub(fy_qtr_new, start=3, end=6))
    qtr_new <- str_sub(fy_qtr_new, start=7, end=8)
    end_qtr_new <- case_when(
      qtr_new == 'Q1' ~ '06-30',
      qtr_new == 'Q2' ~ '09-30',
      qtr_new == 'Q3' ~ '12-31',
      qtr_new == 'Q4' ~ '03-31'
    )
    if(qtr_new=='Q4') {
      cyr_new <- fyr_new
    } else {
      cyr_new <- fyr_new-1
    }
    end_qtr_dt_new <- paste0(cyr_new,"-",end_qtr_new)
    if(qtr_new=='Q1' | qtr_new=='Q2') {
      season_new <- 'summer'
    } else {
      season_new <- 'winter'
    }
    ## insert info for new qtr
    dbExecute(con, glue("INSERT INTO tblLDB_quarter (
                fy_qtr,
                fyr,
                qtr,
                end_qtr,
                end_qtr_dt,
                cyr,
                season
              ) 
              VALUES('{fy_qtr_new}',
              {fyr_new},
              '{qtr_new}',
              '{end_qtr_new}',
              '{end_qtr_dt_new}',
              '{cyr_new}',
              '{season_new}'
              );"))
  }
  dbDisconnect(con)
}

## INSERT LMR DATA TO DB ####
## set up as function for flexbility later - may want to call from other files/processes
fn_db_upload <- function(mysql_tbl, tbl_upload) {
    # connection needed for upload
    #local mysql
    #con <- dbConnect(RMariaDB::MariaDB(), user='root', password=mypwd, dbname='bcbg')
    # amazon mysql
    con <- dbConnect(RMariaDB::MariaDB(),
                     host=a.endpt,
                     user=a.user,
                     password=a.pwd,
                     port=a.port)
    # test
    #dbGetQuery(con, "SELECT * FROM tblLDB_lmr;")
    
    for(r in 1:nrow(tbl_upload)) {
      ## delete any existing match
      dbExecute(con, glue("DELETE FROM {mysql_tbl}
                           WHERE fy_qtr='{tbl_upload$fy_qtr[r]}' AND
                           cat_type='{tbl_upload$cat_type[r]}' AND
                           category='{tbl_upload$category[r]}' AND
                           subcategory='{tbl_upload$subcategory[r]}';"))
      ## insert row from new data
      dbExecute(con, glue("INSERT INTO {mysql_tbl} (
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

## DATA CHECK ####
fn_db_check <- function(data_check) {
  #local mysql
  #con <- dbConnect(RMariaDB::MariaDB(), user='root', password=mypwd, dbname='bcbg')
  # amazon mysql
  con <- dbConnect(RMariaDB::MariaDB(),
                   host=a.endpt,
                   user=a.user,
                   password=a.pwd,
                   port=a.port)
  # query
  data_db <- dbGetQuery(con, "SELECT * FROM bcbg.tblLDB_lmr;")
  ## always disconnect when done
  dbDisconnect(con)
  
  data_smry_qtr_db <- data_db %>% group_by(cat_type, fy_qtr) %>% summarize(
    netsales=sum(netsales),
    litres=sum(litres)
  ) 
  fyqtrs <- unique(data_check$fy_qtr)
  data_smry_qtr_db <- data_smry_qtr_db %>% filter(fy_qtr==fyqtrs[1])
  # format fields for readability
  data_smry_qtr_db$litres <- format(as.numeric(data_smry_qtr_db$litres), big.mark=",", scientific=FALSE, trim=TRUE)
  data_smry_qtr_db$netsales <- currency(as.numeric(data_smry_qtr_db$netsales), symbol="$", digits=0, format='f')
  # print data to compare against online report
  print(data_smry_qtr_db)
}