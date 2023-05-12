* Program....: Compfields.PRG
* Version....: 1.0
* Author.....: David Folger
* Date.......: July 10, 2000
* Notice.....: Copyright (c) 2000 Doane Software, All Rights Reserved.
* Compiler...: Visual FoxPro 06.00.8862.00 for Windows
* Abstract...:
* Changes....:
********************************************************************************
LOCAL lnOk, lcArea, lcOldDele
lcArea = SELECT(0)
ON SHUTDOWN QUIT
ON KEY LABEL ctrl+Q QUIT
lcOldDele = SET("DELETED")

SET DELETED ON

IF USED("cuReportWritable")
 USE IN cuReportWritable
ENDIF
IF USED("FirstDbcToCheck")
 USE IN FirstDbcToCheck
ENDIF
IF USED("SecondDbcToCheck")
 USE IN SecondDbcToCheck
ENDIF
IF USED("cuViewsFirst")
 USE IN cuViewsFirst
ENDIF
IF USED("cuViewsSecond")
 USE IN cuViewsSecond
ENDIF
IF USED("cuReport")
 USE IN cuReport
ENDIF

lcFirstDbcToCheck = GETFILE('DBC', 'Select First Database Container to check', 'Open', 0, 'Find DBC File')
If Empty(lcFirstDbcToCheck)
 =messagebox("First Database Container not selected properly") 
 return
Endif
lcSecondDbcToCheck = GETFILE('DBC', 'Select Second Database Container to check', 'Open', 0, 'Find DBC File')
If Empty(lcSecondDbcToCheck)
 =messagebox("Seond Database Container not selected properly") 
 return
Endif

IF NOT USED("FirstDbcToCheck")
 USE (lcFirstDbcToCheck) AGAIN IN 0 ALIAS FirstDbcToCheck
ENDIF

Select  ;
 lowe(objectname) as objectname ;
 from FirstDbcToCheck ;
 having lowe(objecttype)="field" ;
 order by objectname ;
 INTO Cursor cuViewsFirst


IF NOT USED("SecondDbcToCheck")
 USE (lcSecondDbcToCheck) AGAIN IN 0 ALIAS SecondDbcToCheck
ENDIF

Select  ;
 lowe(objectname) as objectname ;
 from SecondDbcToCheck ;
 having lowe(objecttype)="field" ;
 order by objectname ;
 INTO Cursor cuViewsSecond


SELECT ;
 objectname ;
  FROM cuViewsFirst ;
   WHERE objectname ;
    NOT IN (SELECT;
     objectname;
      FROM cuViewsSecond) ;
  into cursor cuFirstNotSecond ;
  Order by objectname


WAIT WINDOW      
SELECT ;
 objectname ;
  FROM cuViewsSecond ;
  into cursor cuSecondNotFirst ;
  Order by 1 ;
   WHERE objectname;
    NOT IN (SELECT;
     objectname ;
      FROM cuViewsFirst)
      
select 0
select * from cuFirstNotSecond into cursor cuReport where .F. && to get structure
select 0
use DBF("cuReport") Again alias cuReportWritable
If Recc("cuFirstNotSecond") > 0
	append blank
	replace objectname with "The following are in "+lcFirstDbcToCheck+" but not in "+lcSecondDbcToCheck+" :"
	append from DBF("cuFirstNotSecond")
Else
	append blank
	replace objectname with "There are NO views in "+lcFirstDbcToCheck+" that are not in "+lcSecondDbcToCheck+" :"
Endif
If Recc("cuSecondNotFirst") > 0
	append blank
	replace objectname with "The following are in "+lcSecondDbcToCheck+" but not in "+lcFirstDbcToCheck+" :"
	append from DBF("cuSecondNotFirst")
Else
	append blank
	replace objectname with "There are NO views in "+lcSecondDbcToCheck+" that are not in "+lcFirstDbcToCheck+" :"
Endif

select cuReport
report form ViewDiff preview
go top
list off to viewdiff.txt nocon

IF USED("cuReportWritable")
 USE IN cuReportWritable
ENDIF
IF USED("FirstDbcToCheck")
 USE IN FirstDbcToCheck
ENDIF
IF USED("SecondDbcToCheck")
 USE IN SecondDbcToCheck
ENDIF
IF USED("cuViewsFirst")
 USE IN cuViewsFirst
ENDIF
IF USED("cuViewsSecond")
 USE IN cuViewsSecond
ENDIF
IF USED("cuReport")
 USE IN cuReport
ENDIF

SELECT (lcArea)
IF UPPER(lcOldDele) = "OFF"
 SET DELETED OFF
ENDIF
RETURN
* end of main program Compfields.PRG
