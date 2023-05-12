*********************************************************************
*** (C) 1996 Optimized Data Solutions
***  Author: Harold Chattaway
*** This program has been placed in the public domain.
*** The author assumes no responsibility for any program defects or possibly incorrect operation.
*** This program is distributed "as is"
*********************************************************************
***    Assume: Called from Form or Class Designer.
***            This program is in SET PATH or
***            the current directory, the
***            HOME()+"WIZARDS" directory when
***            called from BUILDER or command bo
***  PARMS:    PCACTION    = "REGISTER" to install
***            p2         - required by builder
***            p3         - Required by builder
***   Example: DO enforcer WITH "REGISTER";
*************************************************
PARAMETER pcaction, p2, p3
SET CLASSLIB TO enforcer ADDITIVE

=doform("enforce",pcaction)

RELEASE CLASSLIB enforcer
RETURN

*************************
FUNCTION DoForm(tcClassName, tuParm1)
  LOCAL loObject

  IF PCOUNT() < 2
    loObject = CREATEOBJECT(tcClassName)
  ELSE
    loObject = CREATEOBJECT(tcClassName, tuParm1)
  ENDIF

  IF TYPE("loObject") == "O" AND !ISNULL(loObject)
    loObject.Show()
    IF TYPE("loObject.uRetVal") <> "U"
      RETURN loObject.uRetVal
    ENDIF
  ENDIF
ENDFUNC
