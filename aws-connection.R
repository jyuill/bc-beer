
## Connect to AWS RDS
source('credo.R')
## needed to set security inbound to include my IP address
con_a <- dbConnect(RMariaDB::MariaDB(),
                   host=endpt,
                   user='admin',
                   password=apwd,
                   port=aport)

dbGetQuery(con_a, "SELECT * FROM bcbg.tblLDB_lmr;")

dbDisconnect(con)

