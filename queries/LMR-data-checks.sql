-- BC LDB QTRLY
-- check all
select * from bcbg.tblLDB_beer_sales;
-- selected period
select * from bcbg.tblLDB_beer_sales
where period='FY 2022 Q4';

-- check periods covered
select period, end_qtr_dt, count(netsales) as entries
from bcbg.tblLDB_beer_sales
group by period, end_qtr_dt
order by period;

-- check earliest and most recent period
select period, end_qtr_dt
from bcbg.tblLDB_beer_sales
where end_qtr_dt=(select min(end_qtr_dt) from bcbg.tblLDB_beer_sales) OR
end_qtr_dt=(select max(end_qtr_dt) from bcbg.tblLDB_beer_sales)
group by period, end_qtr_dt
order by end_qtr_dt;


