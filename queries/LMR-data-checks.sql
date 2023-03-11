-- BC LDB QTRLY
select * from bcbg.tblLDB_beer_sales;

select period, end_qtr_dt
from bcbg.tblLDB_beer_sales
group by period, end_qtr_dt
order by period;

select period, end_qtr_dt
from bcbg.tblLDB_beer_sales
where end_dt=min(end_qtr_dt);

select period, end_qtr_dt
from bcbg.tblLDB_beer_sales
where end_qtr_dt=(select min(end_qtr_dt) from bcbg.tblLDB_beer_sales) OR
end_qtr_dt=(select max(end_qtr_dt) from bcbg.tblLDB_beer_sales)
group by period, end_qtr_dt
order by end_qtr_dt;


