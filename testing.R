# testing data extract etc

library(tidyverse)

t_fname <- "data/LMR_2016_06_FY17Q1-Beer_Sales_Litres_long.csv"
t_data <- read_csv(t_fname)

rowSums(t_data)
