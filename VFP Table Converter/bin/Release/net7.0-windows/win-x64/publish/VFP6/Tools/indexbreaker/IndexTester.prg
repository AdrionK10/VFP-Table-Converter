**********
*	Index Tester
*
*	This will perform an initial base count of the number of records in a table (no index set)
*	then step through each of the index tags set counting the records according to that index
*	and will report any discrepancies from the original base count.
*	This will not work if an index includes a filter condition.
*
*	Parameter: nLoopCount - the number of times to perform the below routine.
*
*	Assumptions: the table to be checked is open in the current work area
*
*	98-12-??	Todd Zmetana	Initial Code
*	99-03-03	Todd Zmetana	Add checking of the index expression values to ensure ascending
*	99-03-03	Todd Zmetana	Add handling for descending indexes
**********

PARAMETERS tnLoopCount

IF PARAMETERS() = 0
	tnLoopCount = 1
ENDIF

LOCAL lcDelete, lcEscape, lnTag, lnCurrLoop, lnBaseRec, lnCurrTag, lCurrVal

*** Save the environment
lcDelete = SET('delete')
lcEscape = SET('escape')

SET DELETE OFF
SET ESCAPE OFF

CLEAR

*** Initialize variables
lnTag = tagcount()
lnCurrLoop = 0

*** Obtain a base record count for the table with no index set
lnBaseRec = 0
SET ORDER TO
GO TOP
DO WHILE !EOF()
	lnBaseRec = lnBaseRec + 1
	SKIP
ENDDO
? "Record base count: " + ALLTRIM(STR(lnBaseRec))

*** Loop through this routine the number of times specified
DO WHILE IIF(tnLoopCount > 0, lnCurrLoop < tnLoopCount, (LASTKEY() <> 27))
	*** Go through all the tags to check
	FOR lnCurrTag = 1 TO lnTag
		lnCurrTagRec = 0
		SET ORDER TO lnCurrTag
		? "Checking tag: " + ALLTRIM(TAG())

		*** Skip through the records counting them
		GO TOP
		lCurrVal = EVAL(SYS(14, tagno()))
		DO WHILE !EOF()
			*** Ensure that the count is not exceeding the base count and we are about
			*	to go into an endless index loop
			IF (lnCurrTagRec > lnBaseRec)
				? "Record count for " + ALLTRIM(TAG(lnCurrTag)) + " exceeds base count"
				EXIT
			ENDIF

			*** Test the value to ensure the index is incrementing properly
			IF DESCENDING()
				IF EVAL(SYS(14, tagno())) > lCurrVal
					? "Index order is out for " + ALLTRIM(TAG(lnCurrTag)) + "!"
					EXIT
				ENDIF
			ELSE
				IF EVAL(SYS(14, tagno())) < lCurrVal
					? "Index order is out for " + ALLTRIM(TAG(lnCurrTag)) + "!"
					EXIT
				ENDIF
			ENDIF

			*** We have a new current index value
			lCurrVal = EVAL(SYS(14, tagno()))

			lnCurrTagRec = lnCurrTagRec + 1
			SKIP
		ENDDO

		*** Check that the count did not come up short compared to the base count
		IF lnCurrTagRec < lnBaseRec
			? "Record count for " + ALLTRIM(TAG(lnCurrTag)) + " is less than base count"
		ENDIF
	ENDFOR

	*** Increment the loop counter for doing this x iterations
	lnCurrLoop = lnCurrLoop + 1
ENDDO

WAIT WINDOW "Done" NOWAIT

*** Restore the environment
SET DELETE &lcDelete
SET ESCAPE &lcEscape

RETURN
