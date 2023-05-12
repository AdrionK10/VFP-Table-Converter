ON ERROR DO errhand WITH ;
	ERROR( ), MESSAGE( ), MESSAGE(1), PROGRAM( ), LINENO( )
_screen.windowstate = 2
_screen.visible = .T.
_dblclick = 1
SET ANSI OFF		&& i.e. 'Tommy' = 'Tom'
SET AUTOSAVE ON	&& Save At Once if possible
SET CENTURY ON	&& YYYY
SET CENTURY TO 19 ROLLOVER 80	&& 19YY
SET DATE TO MDY	&& 01/16/94
*SET DECIMALS TO 2	&& 9,999.99
SET DEFAULT TO		&& LEFT(SYS(2005), RAT("\", SYS(2005))-1)
SET DELETE OFF		&& ALLOW TO ON WHEN NEEDED!
SET DEVELOPMENT ON	&& Recompile code when change
SET EXACT OFF		&& Compare Flag
SET EXCLUSIVE OFF	&& Allow Share in Using
*SET FIXED ON		&& CUT DECIMALS TO 2
SET MESSAGE TO		&& Ensure Message is show out!!
SET MULTILOCK ON	&& Allow lock more than 1 lock
SET NEAR ON		&& Search Near if Possible for non-match
SET REPROCESS TO AUTOMATIC	&& Auto Try if Fail in lock
**SET RESOURCE ON	&& Do not Create FoxUser.* cause Error on log!!
*SET SYSMENU TO	&& Easy for Debug
SET HELP ON			&& Use VFP Help File to Fix Bug!
IF FILE("C:\PROGRAM FILES\VFP\FOXHELP.HLP")
	SET HELP TO	"C:\PROGRAM FILES\VFP\FOXHELP.HLP"		&& Use VFP Standard Help
ENDIF
ON ESCAPE RESUME
ON SHUTDOWN QUIT
SET PROCEDURE TO bwmain.PRG

LOCAL fbwTable

SET CLASSLIB TO no_ime ADDITIVE
DO FORM fbwTable NAME fbwTable LINKED
READ EVENTS

*!*	LOCAL fbwTable

*!*	SET CLASSLIB TO no_ime ADDITIVE
*!*	fbwTable = CREATEOBJECT("bwtable")
*!*	fbwTable.show
*!*	READ EVENTS

* ERROR ON LOAD DATA OVER 32K, SO IGNORE IT!! ~_~"
ON ERROR WAIT "" WINDOW NOWAIT	
RELEASE CLASSLIB no_ime

_screen.caption = "Microsoft Visual Foxpro: Development Control"
SET SYSMENU TO DEFAULT	&& Easy for Debug
CLEAR ALL
ON ERROR
SET PROCEDURE TO
*QUIT
*** Sure Not Further Execution, escape Error ***
PROCEDURE errhand
PARAMETER merror, mess, mess1, mprog, mlineno
	DEBUGING=.F.
	IF Merror = 1884	&& Duplicate Key
		TABLEREVERT(.T.)
	ENDIF
	CLEAR
	SET MESSAGE TO
	=MessageBox(	'Error number:	' + LTRIM(STR(merror)) + chr(13) +;
				'Error message:	' + mess + chr(13) +;
				'Program:		' + mprog + chr(13) +;
				'Line number:	' + ALLTRIM(STR(mlineno)) + chr(13) +;
				'Error code:' + repl(chr(13),2) + [> ] + mess1;
				, 48, "Error Message Found!!")
*	ON ERROR
	IF DEBUGING
		SUSPEND
		DEBUG
		DEBUGING = .F.
	ENDIF
*	RETURN .F.
ENDPROC

PROCEDURE objErr
PARAMETER nError, cMethod, nLine, ErrObject, Mess, Mess1
SET MESSAGE TO
messagebox(	'Error number:	' + LTRIM(STR(nError)) + chr(13) +;
			'Error message:	' + mess + chr(13)+;
			'Object:		'+fullname(ErrObject)+chr(13)+;
			'Method:		'+cMethod+chr(13)+;
			'Error Line:	'+ltrim(str(nLine))+CHR(13)+;
			'Error Code:	'+repl(chr(13),2) + [> ] +Mess1;
			,48, 'Object Error' )
ENDPROC