## EXPLORE lIQUOR SALES DATA FROM LDB
## collected via LMR-fetch-process.R
## uploaded to MySQL database BCBG

library(tidyverse)
library(lubridate)
library(scales)
library(glue)
library(readr) ## for easy conversion of $ characters to numeric
library(RMariaDB) ## best way to access MySQL from R

## IMPORT data ####
## get from database
## from file in .gitignore
source('credo.R')
con <- dbConnect(RMariaDB::MariaDB(), user='root', password=mypwd, dbname='bcbg')
#dbGetQuery(con, "SELECT * FROM tblLDB_beer_sales;")
## get list of tables
dbGetQuery(con, "SHOW TABLES;")
lmr_data <- dbGetQuery(con, "SELECT * FROM tblLDB_beer_sales;")

## Explore ####
## > summary ####
summary(lmr_data)
table(lmr_data$qtr, lmr_data$fyr)
table(lmr_data$category, lmr_data$subcategory)
lmr_data %>% group_by(category, subcategory) %>% summarize(sales=sum(netsales),
                                                           min_date=ymd(min(end_qtr_dt)),
                                                           max_date=ymd(max(end_qtr_dt)))
## breakdown by main categories
## sales numbers
lmr_data %>% ggplot(aes(x=period, y=netsales, fill=category))+geom_col()+
  scale_y_continuous(labels=comma)
## % of total by category
lmr_data %>% ggplot(aes(x=period, y=netsales, fill=category))+geom_col(position='fill')+
  scale_y_continuous(labels=percent_format())
## observation: domestic - bc beer is dominant and stable ~75%
## zoom in on domestic - bc beer
lmr_data %>% filter(category=='Domestic - BC Beer') %>% 
  ggplot(aes(x=period, y=netsales, fill=subcategory))+geom_col()+
  scale_y_continuous(labels=comma)
## % of total by category
lmr_data %>% filter(category=='Domestic - BC Beer') %>% 
  ggplot(aes(x=period, y=netsales, fill=subcategory))+geom_col(position='fill')+
  scale_y_continuous(labels=percent_format())
## observation: also stable, dominated by commercial at ~60%; micro brew ~15%