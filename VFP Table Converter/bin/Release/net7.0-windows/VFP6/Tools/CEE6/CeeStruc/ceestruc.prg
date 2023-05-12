***
*** ceestruc.fxp
***
*
*** 
*** ReFox 8.25  #UK814906  DavidÿFolger  DoaneÿSoftware   
***
 PRIVATE m.ctAlk, m.lbYref, m.ceRror, m.csEtsysmenu
 PRIVATE lcSortfld, lnOptsort
 lnOptsort = 1
 lcSortfld = "None"
 DIMENSION afUnclist[300, 2]
 IF SET("TALK")="ON"
      SET TALK OFF
      m.ctAlk = "ON"
 ELSE
      m.ctAlk = "OFF"
 ENDIF
 IF _DOS
      m.csEtsysmenu = SET("SYSMENU")
      SET SYSMENU OFF
 ENDIF
 m.lbYref = (SET("UDFParms")=="REFERENCE")
 SET UDFPARMS OFF
 m.ceRror = ON("ERROR")
 DO CEEFIELD.SPR
 DO reSetenv
 IF _DOS
      SET SYSMENU &cSetSysMenu
 ENDIF
 RETURN
ENDPROC
*
PROCEDURE ResetEnv
 IF m.lbYref
      SET UDFPARMS ON
 ENDIF
 ON ERROR &cError
 SET TALK &cTalk
 RETURN
ENDPROC
*
*** 
*** ReFox - all is not lost 
***
