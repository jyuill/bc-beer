## PDFTOOLS Package Explore
## - pdftools is recommended for extracting text from PDF docs
## -  can be made to work with tables, but not designed for tabular data
## - tabulizer is another pkg designed to extract pdf tables, but...java dependencies can be frustrating, so avoiding it.
## ref: https://docs.ropensci.org/pdftools/ 

library(pdftools)
library(tidyverse)

## Examples of how to extract info with pdftools

## 1. Get all text from pdf
pdf_url <- "https://www.bcldb.com/files/Liquor_Market_Review_F22_23_Q4_March_2023_NEW.pdf"
# pdf_text: character vector of length = number of pages in pdf
# - each string in vector is plain text version of contents of pdf pg
pdf_doc <- pdf_text(pdf_url)
length(pdf_doc)
## text pg example
pg_num <- 3 ## select pg
pdf_doc_pg <- pdf_doc[pg_num] ## get text for pg
pdf_doc_pg
## split into separate lines at "\n"
pdf_doc_lines <- unlist(strsplit(pdf_doc_pg, "\n"))
pdf_doc_lines
cat(pdf_doc_lines)

## chart pg example
pdf_chart_pg <- pdf_doc[4]
pdf_chart_lines <- unlist(strsplit(pdf_chart_pg,"\n"))
pdf_chart_lines
## table pg example
pg_num <- 5 ## select pg
pdf_doc_pg <- pdf_doc[pg_num] ## get text for pg
pdf_doc_pg
## split into separate lines at "\n"
pdf_pg_lines <- unlist(strsplit(pdf_doc_pg, "\n"))
pdf_pg_lines
## select line
pdf_pg_lines[1]
## remove leading spaces in a line
trimws(pdf_pg_lines[1], "left")
## remove whitespace between words in a line
gsub("\\s+"," ", pdf_pg_lines[4])
## trim initial space along with other spaces
trimws(gsub("\\s+"," ", pdf_pg_lines[5]))
trimws(gsub("\\s+"," ", pdf_pg_lines[6]))
## count the number of whitespaces at the start of a line
attr(gregexpr("^\\s+", pdf_pg_lines[4])[[1]],"match.length")
attr(gregexpr("^\\s+", pdf_pg_lines[5])[[1]],"match.length")
attr(gregexpr("^\\s+", pdf_pg_lines[6])[[1]],"match.length")
## split a string when words have 2 or more spaces between them
unlist(str_split(pdf_pg_lines[4], "\\s{2,}"))
## create a table heading using available text
tbl_heading <- unlist(str_split(pdf_pg_lines[4], "\\s{2,}"))[-1] # drop the first item, empty in this case
first_cols <- c("cat_type","category","subcategory")
tbl_heading <- c(first_cols, tbl_heading)
tbl <- data.frame(matrix(ncol=length(tbl_heading)))
colnames(tbl) <- tbl_heading
pdf_pg_lines[3]

## chart pgs -> look for distingushing structure
## (using example above)
pdf_chart_lines[6]
pdf_line_items <- unlist(str_split(pdf_chart_lines[6], "\\s{2,}"))
str_detect(pdf_chart_lines[6],"Item Subcategory")
