* UpCaption()
* Change the line into Proper Case
* Usage:		Rline = UpCaption(LLINE)
* FUNCTION UPCAPTION
LPARAMETERS PLINE

LLINE = ALLTRIM(LOWER(STRTRAN(PLINE,'	',' ')))
LLEN = LEN(LLINE)
RLINE = ""

IF LLEN>1
	RLINE = UPPER(LEFT(LLINE,1))		&& TAKE 1ST CHAR
	LLINE = SUBSTR(LLINE,2)			&& LLINE REMAIN

	DO WHILE !EMPTY(LLINE)	.AND. LEN(LLINE) > 1
		LASTCHAR = RIGHT(RLINE,1)		&& LASTCHAR OF RLINE
		THISCHAR = LEFT(LLINE,1)		&& 1STCHAR OF LLINE
		DO CASE
		CASE EMPTY(THISCHAR)	&& UPPERFLAG .T.
			RLINE = ALLTRIM(RLINE) + " "

		CASE LASTCHAR $ '+"/}])([{\-.,;!' .OR. EMPTY(LASTCHAR)	&& UPPER CASE
			RLINE = ALLTRIM(RLINE + UPPER(THISCHAR))
			
		OTHERWISE	&& MIDDLE
			RLINE = ALLTRIM(RLINE + LOWER(THISCHAR))
		ENDCASE

*		LLINE = RIGHT(LLINE,LLEN-1)
		LLINE = SUBSTR(LLINE,2)
	ENDDO

	LASTCHAR = RIGHT(RLINE,1)		&& LASTCHAR OF RLINE
	THISCHAR = LEFT(LLINE,1)		&& 1STCHAR OF LLINE
	DO CASE
	CASE EMPTY(THISCHAR)	&& UPPERFLAG .T.
		RLINE = ALLTRIM(RLINE) + " "

	CASE LASTCHAR $ '+"/}])([{\-.,;!' .OR. EMPTY(LASTCHAR)	&& UPPER CASE
		RLINE = ALLTRIM(RLINE + UPPER(THISCHAR))
		
	OTHERWISE	&& MIDDLE
		RLINE = ALLTRIM(RLINE + LOWER(THISCHAR))
	ENDCASE

	RLINE = ALLTRIM(RLINE)

	RETURN RLINE
ELSE
	RETURN ""
ENDIF
*!*	* UPCAPTION.PRG UPPER CASE EACH WORD
*!*	LPARAMETERS RLINE

*!*	LLINE = ALLTRIM(RLINE)
*!*	RLINE = ""

*!*	DO WHILE LEN(LLINE) > 0
*!*	IF AT(" ", LLINE) > 0
*!*		DO WHILE ALLTRIM(UPPER(LEFT(LLINE,1))) $ '(["\/'	&& START WITH  OPENING SYMBOL
*!*			ww(7,rline, lline)
*!*			RLINE = RLINE + ALLTRIM(UPPER(LEFT(LLINE,1)))
*!*			LLINE = ALLTRIM(SUBSTR(LLINE, 1))
*!*		ENDDO
*!*		* FINISH CLEAN ([
*!*		RLINE = RLINE + ALLTRIM(UPPER(LEFT(LLINE,1))) + ALLTRIM(LOWER(SUBSTR(LLINE, 2, AT(" ",LLINE) -1))) + " "
*!*		LLINE = ALLTRIM(SUBSTR(LLINE, AT(" ",LLINE) +1))
*!*	ELSE
*!*		DO CASE
*!*		CASE LEN(LLINE) > 2
*!*		DO WHILE ALLTRIM(UPPER(LEFT(LLINE,1))) $ '(["\/'	&& START WITH  OPENING SYMBOL
*!*			ww(7,rline, lline)
*!*			RLINE = RLINE + ALLTRIM(UPPER(LEFT(LLINE,1)))
*!*			LLINE = ALLTRIM(SUBSTR(LLINE, 1))
*!*		ENDDO
*!*			RLINE = RLINE + ALLTRIM(UPPER(LEFT(LLINE,1))) + ALLTRIM(LOWER(SUBSTR(LLINE, 2)))
*!*		
*!*		OTHERWISE
*!*			RLINE = RLINE + ALLTRIM(UPPER(LEFT(LLINE,1)))
*!*		ENDCASE
*!*		LLINE = ""
*!*	ENDIF
*!*	ENDDO

*!*	RETURN RLINE