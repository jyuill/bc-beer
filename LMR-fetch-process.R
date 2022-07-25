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

## GET DATA ####
## **START manual section ####
## everything else is manual
## > SET SPECS ####
## SPECIFY LINK AND DESIRED FILE NAME: needed for each issue
## find link at: https://www.bcldb.com/publications/liquor-market-review 
## LINK
#furl <- "https://www.bcldb.com/files/Liquor_Market_Review_F21_22_Q4_March_2022.pdf"
#furl <- "https://www.bcldb.com/files/Liquor_Market_Review_F21_22_Q1_June_2021.pdf"
furl <- "https://www.bcldb.com/files/Liquor%20Market%20Review_F19_20_Q4_March_2020.pdf"
furl <- "https://www.bcldb.com/files/Liquor%20Market%20Review_F18_19_Q4_March_2019.pdf"
furl <- "https://www.bcldb.com/files/Liquor%20Market%20Review_Q4_March_2018.pdf"
furl <- "https://www.bcldb.com/files/Liquor%20Market%20Review_F16_17_Q4_March_2017.pdf"
## FILENAME for saving
#fname <- "LMR_2022_03.pdf"
#fname <- "LMR_2021_06.pdf"
fname <- "LMR_2020_03.pdf"
fname <- "LMR_2019_03.pdf"
fname <- "LMR_2018_03.pdf"
fname <- "LMR_2017_03.pdf"
## **END manual section ####

## > DL & SAVE ####
## DOWNLOAD and SAVE - using above set above
download.file(furl, paste0("input/",fname), mode='wd')
lmr <- pdf_text(paste0("input/",fname))
## VIEW selected pages
## table of contents 
#cat(lmr[2])
## intro incl notes on FISCAL YR (Apr 1 - Mar 31)
#cat(lmr[3])

## PROCESS DESCR. ####
## Each table has sections that are identifiable by different formatting / content.
## Different sections have different features that need to be parsed out.
## Requires manual approach for each section of each table.
## Process below works by:
## 1. grab the whole table
## 2. extract, parse out column headings
## 3. process first section: parse out content for rows in section
## 4. process next section - add on to first
## 5. repeat for additional sections - add on to previous
## 6. convert to data frame: above processing creates matrices, incl. final combined matrix
## 7. tidy - format into long tidy structure

## FISCAL TBL ####
## cols use reference to fiscal quarters
## for BC LDB, fy quarters align with these end dates
## this table used to merge in end dates at end in TIDY section
ldb_fy <- tribble(
  ~qtr,~end_dt,
  "Q1", "06-30",
  "Q2", "09-30",
  "Q3", "12-31",
  "Q4", "03-01"
)

## BEER $ TABLE ####
## 1. EXTRACT TBL ####
## extract beer $ sales table
## page with beer table: info is messy and needs to be structured in table
tbl_pg <- lmr[5]
## split the whole page into rows at each \n (not actually visible)
tbl_pg_rows <- strsplit(tbl_pg, "\n")
## > GET TABLE ####
## isolate actual table only (remove additional text)
tbl <- tbl_pg_rows[[1]][4:19]

## 2. COL HEADINGS ####
## - col headings for table
## isolate data column headings: Fiscal 2020/21 Q4, etc
## remove misc title, take out excess whitespace, add marker for splitting, split into fixed cols
tbl_heading <- str_remove(tbl[1],"Net Sales \\$")
tbl_heading <- str_squish(tbl_heading)
tbl_heading <- str_replace_all(tbl_heading,"F","xF")
tbl_heading <- str_split_fixed(tbl_heading, "x",6)
## basic clean result - can be used with any sub-sections of table
tbl_heading ## individual items - some trailing sp

## add category and subcategory items to align with body of table
tbl_heading[1] <- "subcategory"
tbl_heading <- cbind("category",tbl_heading)
## headings df ready to be used below - just need to remove first row

## 3. GET FIRST SECTION ####
## > DATA ####
## check/get target rows
tbl_sec <- tbl[2:4]
## > first data row ####
## remove leading whitespace
tbl_row <- str_squish(tbl[2])
## add markers for splitting
tbl_row <- str_replace_all(tbl_row, "D", "xD")
tbl_row <- str_replace_all(tbl_row, "\\$","x$")
## split into cols
tbl_rowcol <- str_split_fixed(tbl_row, "x",8)
tbl_rowcol_sp <- tbl_rowcol[1,-1] ## remove empty first item

## > next rows ####
## process several at a time
## next rows have one less leading header - no 'Domestic - BC Beer'
tbl[3:4]
mult_row <- str_squish(tbl[3:4])
mult_row <- str_replace_all(mult_row, "\\$","x$")
mult_rowcol <- str_split_fixed(mult_row,"x",6)
## add spacer row to align with first row
mult_rowcol <- cbind("",mult_rowcol)

## > JOIN all above ####
## join new rows to orig
tbl_top <- rbind(tbl_heading, tbl_rowcol_sp, mult_rowcol)
## fill down category for rows that don't have it
tbl_top[3:4,1] <- tbl_top[2,1]
tbl_sec_all <- tbl_top

## next row is summary row - skip

## 4. ADD NEXT SECTION ####
## - same format, same process
## > DATA ####
## check/get target rows
# tbl_sec <- tbl[6:8] ## pre Mar 2018
tbl_sec <- tbl[7:9] ## starting Mar 2018
## > first row ####
## remove leading whitespace
tbl_row <- str_squish(tbl_sec[1])
tbl_row <- str_squish(tbl_sec)
## add markers for splitting
tbl_row <- str_replace_all(tbl_row, "D", "xD")
tbl_row <- str_replace_all(tbl_row, "\\$","x$")
## split into cols
tbl_rowcol <- str_split_fixed(tbl_row, "x",8)
tbl_rowcol_sp <- tbl_rowcol[1,-1] ## remove empty first item

## > next rows ####
## next rows have one less leading header - no 'Domestic - BC Beer'
mult_row <- str_squish(tbl_sec[2:length(tbl_sec)])
## don't actually need this, since 
#mult_row <- str_replace_all(mult_row, "D","xD")
mult_row <- str_replace_all(mult_row, "\\$","x$")
mult_rowcol <- str_split_fixed(mult_row,"x",6)
mult_rowcol <- cbind("",mult_rowcol)

## > JOIN with prev ####
tbl_secs <- rbind(tbl_rowcol_sp, mult_rowcol)
tbl_secs[2:nrow(tbl_secs),1] <- tbl_secs[1,1]

## join with top section - before data frame
tbl_sec_all <- rbind(tbl_sec_all, tbl_secs)

## 5. ADD NEXT SECTION ####
## > DATA ####
## check/get target rows
#tbl_sec <- tbl[10:14] ## pre Mar 2018
tbl_sec <- tbl[12:16] ## starting Mar 2018
## > first row ####
## remove leading whitespace
tbl_row <- str_squish(tbl_sec[1])
tbl_row
## > manual markers for split ####
## add markers for splitting -> TRICKY because not consistent like before 
## manually customized for first 2 rows
tbl_row <- str_replace_all(tbl_row, "Import", "xImport")
tbl_row <- str_replace_all(tbl_row, "As","xAs")
tbl_row <- str_replace_all(tbl_row, "\\$","x$")
## split into cols
tbl_rowcol <- str_split_fixed(tbl_row, "x",8)
tbl_rowcol_sp <- tbl_rowcol[1,-1] ## remove empty first item
## > next rows ####
## next rows have one less leading header - no 'Domestic - BC Beer'
mult_row <- str_squish(tbl_sec[2:length(tbl_sec)])
## no need to split for subcategory after space removed
#mult_row <- str_replace_all(mult_row, "^E|^M|^O|^U","xD")
mult_row <- str_replace_all(mult_row, "\\$","xx$") ## double-x because some words have x (Mexico)
mult_rowcol <- str_split_fixed(mult_row,"xx",6)
mult_rowcol <- cbind("",mult_rowcol) ## need to add col at start
## > JOIN with prev ####
tbl_secs <- rbind(tbl_rowcol_sp, mult_rowcol)
tbl_secs[2:nrow(tbl_secs),1] <- tbl_secs[1,1]
## join with top section - before data frame
tbl_sec_all <- rbind(tbl_sec_all, tbl_secs)
## remove spaces at end of entries
tbl_sec_all <- apply(tbl_sec_all, 1:2, function(x) str_squish(x))

## 6. CONVERT TO DF ####
## at end with entire table
tbl_df <- data.frame(tbl_sec_all)
## > col names 1 ####
## set col names based on row 1
colnames(tbl_df) <- tbl_df[1,]
## drop first row, now that moved to col names
tbl_df <- tbl_df[-1,]
## > row names 1 ####
## set row names to numbers
rownames(tbl_df) <- seq(1:nrow(tbl_df))
## > data clean
## convert $ numbers in char form to numeric
firstcol <- 3 ## first col to convert
for(i in firstcol:ncol(tbl_df)){
  tbl_df[,i] <- parse_number(tbl_df[,i])
}

## > COL NAMES 2 ####
## clean column names for simplicity and to avoid probs later
firstcol <- 3 ## first col to clean up
for(i in firstcol:ncol(tbl_df)){
  colnames(tbl_df)[i] <- str_replace(colnames(tbl_df)[i], 
                                                  substr(colnames(tbl_df)[i],
                                                         start=10, stop=12),"")
}
## finish clean up col names for simplicity, avoid probs later
colnames(tbl_df) <- str_replace_all(colnames(tbl_df),"Fiscal","FY")

## 7. TIDY structure ####
## > convert to long ####
## convert table with metrics in multiple rows to tidy format
tbl_df_t <- tbl_df %>% pivot_longer(cols=c(3:7), names_to='period', values_to='netsales')
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

## 8. SAVE ####
write_csv(tbl_df_t, 'input/beer_sales.csv')
write_csv(tbl_df_t, paste0('input/beer_sales_',
                           min(tbl_df_t$end_qtr_dt),'-',max(tbl_df_t$end_qtr_dt),'.csv'))

## 9. UPDATE DATABASE ####
library(RMariaDB) ## NOTE: pwd blocked out for security -> need to add
## > connect ####
## from file in .gitignore
source('credo.R')
con <- dbConnect(RMariaDB::MariaDB(), user='root', password=mypwd, dbname='bcbg')
#dbGetQuery(con, "SELECT * FROM tblLDB_beer_sales;")
## > insert ####
for(i in 1:nrow(tbl_df_t)){
  ## test for existence of row - insert if not already exist
  if(nrow(dbGetQuery(con, glue("SELECT * FROM tblLDB_beer_sales 
                       WHERE category='{tbl_df_t$category[i]}' AND
                       subcategory='{tbl_df_t$subcategory[i]}' AND
                       qtr='{tbl_df_t$qtr[i]}' AND
                       fyr='{tbl_df_t$fyr[i]}';")))==0) {
    ## insert query
    dbExecute(con, glue("INSERT INTO tblLDB_beer_sales (
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
tbl_df_t %>% ggplot(aes(x=period, y=net_sales, fill=category))+geom_col()+
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