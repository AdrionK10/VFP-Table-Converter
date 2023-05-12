 LPARAMETERS roObject, pnError, pcMethod, pnLine
 LOCAL lcObject, lnResult, lcMessage1
 lcMessage1 = "Error:"+PADL(pnError, 6)+" "+MESSAGE()+CHR(13)+CHR(13)
 lcObject = "Object: "+obJect_path(roObject)+"  Method: "+pcMethod+ ;
            "  Line:"+PADL(pnLine, 6)+CHR(13)+CHR(13)
 lnResult = MESSAGEBOX(lcMessage1+lcObject+"Suspend?", 019)
 DO CASE
      CASE lnResult=6
           ACTIVATE WINDOW trAce
           SUSPEND
      CASE lnResult=2
           CANCEL
 ENDCASE
 RETURN
ENDPROC
*
