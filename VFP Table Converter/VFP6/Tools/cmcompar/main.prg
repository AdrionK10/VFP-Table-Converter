***
*** main.fxp
***
*
*** 
*** ReFox 8.25  #UK814906  DavidÿFolger  DoaneÿSoftware   
***
 LPARAMETERS tcDir1, tcDir2
 DECLARE INTEGER GetPrivateProfileString IN Win32API AS GetPrivStr STRING,  ;
         STRING, STRING, STRING @, INTEGER, STRING
 DECLARE INTEGER WritePrivateProfileString IN Win32API AS WritePrivStr  ;
         STRING, STRING, STRING, STRING
 DECLARE INTEGER GetProfileString IN Win32API AS GetProStr STRING, STRING,  ;
         STRING, STRING @, INTEGER
 DECLARE INTEGER WriteProfileString IN Win32API AS WriteProStr STRING,  ;
         STRING, STRING
 LOCAL pcHomedir
 pcHomedir = ADDBS(JUSTPATH(SYS(16, 0)))
 LOCAL lcOldproc
 lcOldproc = SET('PROC')
 SET PROCEDURE TO ModulS ADDITIVE
 LOCAL pcFoxtools
 pcFoxtools = 'FOXTOOLS.FLL'
 IF FILE(HOME()+m.pcFoxtools)
      SET LIBRARY TO (HOME()+m.pcFoxtools) ADDITIVE
 ELSE
      SET LIBRARY TO (m.pcHomedir+m.pcFoxtools) ADDITIVE
 ENDIF
 IF NOT m.pcFoxtools$seTlibrary()
      = meSsagefatal('Can not locate '+m.pcFoxtools)
      RETURN .F.
 ENDIF
 DO FORM Comp0.SCX WITH m.tcDir1, m.tcDir2
 RETURN
ENDFUNC
*
*** 
*** ReFox - all is not lost 
***
