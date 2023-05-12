lparameters tcCoverageFile
lcLog = SET('COVERAGE')
SET COVERAGE TO

IF EMPTY(lcLog)
	Do Case
	Case Empty(tcCoverageFile)
		lcLog = GetFile()
	Case NOT Empty(tcCoverageFile) AND File(tcCoverageFile)
		lcLog = tcCoverageFile	
	Otherwise
		lcLog = GetFile()
	EndCase
ENDIF
IF EMPTY(lcLog)
	RETURN
ENDIF

wait window nowait "one moment"

create cursor cov ;
	( nTime n(12,6), ;
	cClass c(30), ;
	cObj c(60), ;
	nLine i, ;
	cfile c(60), ;
	cStack i )
set talk on
set talk window
*append from c:\temp\coverage.log type delimited
append from (lcLog) type delimited

* Lines that took the longest to run once.
select * ;
	from cov ;
	order by nTime descending ;
	top 10 ;
	into cursor cuCoverage
brow title "Lines that took the longest to run once"

* Lines that ran the most number of times.
select count(*) as nCnt, * ;
	from cov ;
	group by cClass, cObj, nLine, cfile ;
	order by nCnt descending ;
	top 10;
	into cursor cuCoverage
brow title "Lines that ran the most number of times"

* Lines that accumulated the most time over successive executions
select sum( nTime ) as nTotTime, * ;
	from cov ;
	group by cClass, cObj, nLine, cfile ;
	order by nTotTime descending ;
	top 10 ;
	into cursor cuCoverage
brow title "Lines that accumulated the most time over successive executions"

set talk nowin
set talk off
