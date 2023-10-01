## Upload LMR data gathered from pdf report to MySQL database

library(tidyverse)
library(lubridate)
library(scales)
library(glue)
library(readr) ## for easy conversion of $ characters to numeric
library(RMariaDB) ## for MySQL

## > connect ####
## from file in .gitignore
source('credo.R')
con <- dbConnect(RMariaDB::MariaDB(), user='root', password=mypwd, dbname='bcbg')
# test
#dbGetQuery(con, "SELECT * FROM tblLDB_lmr;")

mysql_tbl <- "bcbg.tblLDB_lmr"
tbl_upload <- tables_all_fyqtr
for(r in 1:nrow(tbl_upload)) {
  ## delete any existing match
  dbGetQuery(con, glue("DELETE FROM {mysql_tbl}
                       WHERE fy_qtr='{tbl_upload$fy_qtr[r]}' AND
                       cat_type='{tbl_upload$cat_type[r]}'
                       category='{tbl_upload$category[r]}' AND
                       subcategory='{tbl_upload$subcategory[r]}';"))
  ## insert row from new table
  ## NEEDS UPDATE TO NEW TABLE!!!
  dbGetQuery(con, glue("INSERT INTO {tbl_info$mysql_tbl} (
            fy_qtr,
            cat_type,
            category,
            subcategory,
            litres,
            netsales
          ) ## UPDATE FROM HERE DOWN
          VALUES('{tbl_upload$category[i]}',
          '{tbl_df_t$subcategory[i]}',
          '{tbl_df_t$period[i]}',
          {tbl_df_t$netsales[i]},
          '{tbl_df_t$qtr[i]}',
          {tbl_df_t$fyr[i]},
          {tbl_df_t$cyr[i]},
          '{tbl_df_t$end_dt[i]}',
          '{tbl_df_t$end_qtr_dt[i]}'
          );")))
}

## always disconnect when done
dbDisconnect(con)