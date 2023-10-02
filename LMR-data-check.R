## Test data - compare downloaded to online report

library(tidyverse)
library(lubridate)
library(scales)
library(glue)
library(here)
library(formattable)

## assumes data available in final table for each report
data_check <- tables_all_fyqtr

fyqtrs <- unique(data_check$fy_qtr)
data_smry_cat <- data_check %>% group_by(cat_type, fy_qtr) %>% summarize(
  netsales=sum(netsales),
  litres=sum(litres)
) 

data_smry_qtr <- data_smry_cat %>% filter(fy_qtr==fyqtrs[1])
# Format the Salary column as currency with commas
df$Salary <- format(df$Salary, big.mark = ",", scientific = FALSE, trim = TRUE)

data_smry_qtr$litres <- format(data_smry_qtr$litres, big.mark=",", scientific=FALSE, trim=TRUE)
data_smry_qtr$netsales <- currency(data_smry_qtr$netsales, symbol="$", digits=0, format='f')
print(data_smry_qtr)