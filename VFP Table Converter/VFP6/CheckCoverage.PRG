create cursor cov ;
( nTime n(12,6), ;
cClass c(30), ;
cObj c(60), ;
nLine i, ;
cfile c(60), ;
cStack i )
append from c:\temp\coverage.log type delimited
* Lines that took the longest to run once.
select * ;
from cov ;
order by nTime descending ;
top 10
* Lines that ran the most number of times.
select count(*) as nCnt, * ;
from cov ;
group by cClass, cObj, nLine, cFile ;
order by nCnt descending ;
top 10
* Lines that accumulated the most time over successive executions
select sum( nTime ) as nTotTime, * ;
from cov ;
group by cClass, cObj, nLine, cFile ;
order by nTotTime descending ;
top 10