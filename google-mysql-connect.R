## Connect to Google Cloud SQL
## use same approach as with local version of MySQL - just need new hostname
## note also that computer public IP has been added to Google Cloud SQL config
library(RMariaDB) ## successor to/expanded version of RMYSQL
pwd <- source('credo.R') ## get pwd from local file (in .gitignore)
con_g <- dbConnect(RMariaDB::MariaDB(), 
                   host=ghost,
                   user='root', 
                   password=gpwd, 
                   dbname='bcbg')
dbListTables(con_g)
dbGetQuery(con_g, "SELECT * FROM tblLDB_beer_sales;")

dbDisconnect(con_g)
