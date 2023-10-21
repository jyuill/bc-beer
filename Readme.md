## BC LDB LMR: download, process, upload

Main purpose of this repo is to facilitate the collection of data from
the quarterly (BC LDB Liquor Market
Review)\[<https://www.bcldb.com/publications/liquor-market-review>\] for
reporting and analysis.

## Components

Key components are:

- LMR-fetch-process-all_v4.R: download report and process tables into
  usable format.
- LMR_db_upload.R: run functions to insert new data to mysql database
  (using Amazon RDS).
- lmr_db_functions.R: functions for working with mysql.
- credo.R: file with mysql access info, not shared in repo.

## Process

When new LMR is released on the BC LDB website:

1.  Get url for new report and enter in LMR-fetch-process-all_v4.R.
2.  Run LMR-fetch-process-all_v4.R
3.  Run LMR_db_upload.R to add to MySQL database.
4.  Refresh/update any reporting and/or analysis.
