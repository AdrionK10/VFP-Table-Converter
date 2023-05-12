LOCAL lcLog, lnRec, lnStack, lnReal
SET TALK OFF
SET POINT TO
lcLog = SET('COVERAGE')
SET COVERAGE TO
IF EMPTY(lcLog)
   lcLog = GetFile('log')
ENDIF
IF EMPTY(lcLog)
   RETURN
ENDIF
CREATE CURSOR crsCover ( ;
   nTime N(11, 6), ;
   cClass C(50), ;
   cMethod C(50), ;
   nLine I, ;
   cFile C(115), ;
   nStack I, ;
   nReal N(11, 6))
APPEND FROM (lcLog) DELIMITED
SCAN
   lnRec = RECNO()
   lnStack = crsCover.nStack
   SKIP
   CALCULATE SUM(nTime) TO lnReal ;
      FOR crsCover.nStack = lnStack + 1 ;
      WHILE crsCover.nStack > lnStack
   
   GOTO lnRec
   REPLACE nReal WITH nTime - lnReal
ENDSCAN
SELECT cClass, ;
       cMethod, ;
       COUNT(*) AS nCnt, ;
       SUM(nReal) AS nTot ;
  FROM crsCover ;
 GROUP BY 1,2 ;
 ORDER BY nTot DESC ;
HAVING nTot <> 0 ;
  INTO CURSOR crsMethod
  
BROWSE LAST NOWAIT
SELECT cClass, ;
       cMethod, ;
       nLine, ;
       COUNT(*) AS nCnt, ;
       SUM(nReal) AS nTot ;
  FROM crsCover ;
 GROUP BY 1, 2, 3 ;
HAVING nTot <> 0 ;
 ORDER BY nTot DESC ;
  INTO CURSOR crsLine
  
BROWSE LAST NOWAIT FOR cClass = crsMethod.cClass AND cMethod = crsMethod.cMethod