## Connect to AWS RDS
library(RMariaDB)

source('credo.R')
## needed to set security inbound to include my IP address
con_a <- dbConnect(RMariaDB::MariaDB(),
                   host=a.endpt,
                   user=a.user,
                   password=a.pwd,
                   port=a.port)

lmr_aws <- dbGetQuery(con_a, "SELECT * FROM bcbg.tblLDB_lmr;")

dbDisconnect(con)

