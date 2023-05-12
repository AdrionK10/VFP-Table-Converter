LPARAMETERS tnMaxMemoToShow
* 1. Setup
	LOCAL lcAlias
	lcAlias = ALIAS()
	IF EMPTY(lcAlias)
		??CHR(7)
		WAIT WINDOW NOWAIT 'No table found'
		RETURN
	ENDIF

	* The max number of chars to show of a memo field.
	IF EMPTY(tnMaxMemoToShow)
		tnMaxMemoToShow = 200
	ENDIF
		

* 2. Create cursor which we'll show to user.
	* 2.1 Field list
		LOCAL laFields[1],lcCommand,j,ja[1],lcField
		= AFIELDS(laFields)

	* 2.2 Create string with SELECT command.  For memo fields investigate length of longest item.
		lcCommand = 'SELECT '
		FOR j = 1 TO ALEN(laFields,1)
			IF laFields[j,2] = 'M'
				* How long is the longest item ?
				SELECT MAX(LEN(&laFields[j,1])) FROM &lcAlias INTO ARRAY ja
				ja = MAX(1,ja)
				ja = MIN(ja,tnMaxMemoToShow)
				lcField = TRIM(laFields[j,1])
				lcCommand = lcCommand + 'PADR(' + lcField + ', ' ;
							+ ALLTRIM(STR(INT(ja))) + ') AS ' + lcField + ', '
			ELSE
				lcCommand = lcCommand + laFields[j,1] + ', '
			ENDIF
		ENDFOR
		lcCommand = LEFT(lcCommand, LEN(lcCommand) - 2 )
		lcCommand = lcCommand + ' FROM ' + lcAlias + ' INTO CURSOR xbrow'

	* 2.3 Run select
		&lcCommand

	* 2.4 Open cursor for writing.
		LOCAL lcOldFullPath,lcFullPath
		lcOldFullpath = SET('FULLPATH')
		SET FULLPATH ON
		lcFullpath = FULLPATH(DBF())
		SET FULLPATH &lcOldFullpath

		USE (lcFullpath) AGAIN IN SELECT(1) ALIAS &lcAlias._X
		USE
		SELECT &lcAlias._X
		BROW
RETURN
