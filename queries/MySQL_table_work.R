
## LDB LMR database table manipulation
## MySQL > bcbg > several tables
## - tables created within MySQL workbench
## - see LMR-table-setup.sql

library(tidyverse)
library(RMariaDB) ## for accessing MySQL
library(glue)

## > connect ####
## from file in .gitignore
source('credo.R')
con <- dbConnect(RMariaDB::MariaDB(), user='root', password=mypwd, dbname='bcbg')
## test
dbGetQuery(con, "SHOW TABLES;")
dbGetQuery(con, "SELECT * FROM tblLDB_quarter;")

## QUARTERS - build out table
## efficiently get yr_qtr combos
yrs <- seq(from='2017', to='2023', by=1)
qtrs <- c('Q1','Q2','Q3','Q4')
yr_qtr <- expand.grid(fyr=yrs, qtr=qtrs)
yr_qtr <- yr_qtr %>% arrange(fyr)
## build out cols
qtrs <- yr_qtr %>% mutate(
  fy_qtr = paste0('FY',fyr,qtr)
)
## - derive cols based on existing
qtrs_full <- qtrs %>% mutate(
  end_qtr = case_when(
    qtr=='Q4' ~ '03-31',
    qtr=='Q1' ~ '06-30',
    qtr=='Q2' ~ '09-30',
    qtr=='Q3' ~ '12-31'
  ),
  cyr = case_when(
    qtr=='Q4' ~ fyr,
    TRUE ~ fyr-1
  ),
  end_qtr_dt = paste0(cyr,"-",end_qtr),
  season = case_when(
    qtr=='Q1'|qtr=='Q2' ~ 'summer',
    TRUE ~ 'winter'
  )
)
qtrs_full <- qtrs_full %>% select(fy_qtr, fyr, qtr, end_qtr, end_qtr_dt, cyr, season)
## INSERT VALUES in table
for(q in 1:nrow(qtrs_full)){
  db_qtrs <- dbGetQuery(con, "SELECT * FROM tblLDB_quarter;")
  nr <- db_qtrs %>% filter(fy_qtr==qtrs_full$fy_qtr[q])
  cat(q,qtrs_full[q,1], "\n")
  if(nrow(nr)<1){
    dbGetQuery(con, glue("INSERT INTO tblLDB_quarter VALUES (
                '{qtrs_full$fy_qtr[q]}', {qtrs_full$fyr[q]}, '{qtrs_full$qtr[q]}', '{qtrs_full$end_qtr[q]}', 
                '{qtrs_full$end_qtr_dt[q]}',{qtrs_full$cyr[q]}, '{qtrs_full$season[q]}'
               );"))
    cat(qtrs_full[q, 1], "updated \n")
  }
}
## check table 
tbl_qtrs <- dbGetQuery(con, "SELECT * FROM tblLDB_quarter;")
