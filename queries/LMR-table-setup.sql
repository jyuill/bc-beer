/* CREATE LDB TABLES
- Raw SQL queries -> this file available in R repo AND in MySQL Workbench
- develop/save in R -> run in MySQL Workbench
- Or make connection to MySQL in R, use dbGetQuery
- Purpose:
- going beyond beer to capture organized data for all beverages in Quarterly Market Report
- using some basic database normalization -> more complicated than flat table but better aligned with best practice
- better scalability and maintenance over time, especially if changes made by BC LDB but also for enhancements
*/

-- CATEGORY TABLE
-- table for beverage categories and subcategories from report, assigned to a category type
-- CANCELLED: decided not needed, since categories are stable and not likely to be changed/extended/collapsed/aggregated
-- CREATE TABLE bcbg.tblLDB_category (
cat_id INTEGER AUTO_INCREMENT PRIMARY KEY,
cat_type VARCHAR(30), 
category VARCHAR(30),
subcategory VARCHAR(30)
);

-- QUARTER TABLE
-- table for quarters and extended dimensions to manage in one place
-- based on unique quarter date
-- efficient because all date-related items in one place, based on qtr identified in report
-- in LDB report original format: 'Fiscal 2022/23 Q1'
-- converted/simplified in R code: 'FY 2023 Q1' -> better to remove sp: 'FY2023Q1'
-- fields: fy_qtr (PK - 'FY2023Q1'), fyr, qtr, end_dt (mm-dd), end_qtr_dt (yyyy-mm-dd), cyr (yyyy)
CREATE TABLE bcbg.tblLDB_quarter (
fy_qtr VARCHAR(10) PRIMARY KEY,
fyr INTEGER,
qtr VARCHAR(10),
end_qtr VARCHAR(10),
end_qtr_dt DATE,
cyr INTEGER,
season VARCHAR(10)
);

-- CAN ALSO UPDATE / INSERT VIA MYSQL WORKBENCH FORM EDITOR !
-- changed original col name for clarity
-- ALTER TABLE bcbg.tblLDB_quarter CHANGE end_dt end_qtr VARCHAR(10);
-- check
SELECT * FROM bcbg.tblLDB_quarter;
-- insert values - test; will do the main work in R
INSERT INTO bcbg.tblLDB_quarter VALUES
('FY2016Q3',2016,'Q3','12-31','2015-12-31',2015,'winter');

-- can also use Form editor in MySQL Workbench Form Editor!
-- fix incorrect
UPDATE bcbg.tblLDB_quarter
SET cyr='2016'
WHERE fy_qtr='FY2016Q4';

-- MAIN TABLE
-- rework/replace original
CREATE TABLE bcbg.tblLDB_lmr (
	lmr_id INTEGER AUTO_INCREMENT PRIMARY KEY,
    fy_qtr VARCHAR(10),
    cat_type VARCHAR(30),
    category VARCHAR(30),
    subcategory VARCHAR(30),
    netsales BIGINT,
    litres BIGINT,
    FOREIGN KEY(fy_qtr) REFERENCES tblLDB_quarter(fy_qtr)
);
-- rename original subcat to subcategory for clarity (changed above for future)
ALTER TABLE bcbg.tblLDB_lmr CHANGE COLUMN subcat subcategory VARCHAR(30);
-- too small -> need to change field to larger varchar
ALTER TABLE bcbg.tblLDB_lmr MODIFY COLUMN subcategory VARCHAR(100);

SELECT * FROM bcbg.tblLDB_lmr;