***
*** list_tables_start.fxp
***
*
*** 
*** ReFox 8.25  #UK814906  DavidÿFolger  DoaneÿSoftware   
***
*
PROCEDURE LIST_TABLES_START
 LOCAL prEv_dir
 PRIVATE prEv_error
 prEv_error = ON("ERROR")
 prEv_dir = cuRr_dir()
 DO FORM Print_Table_Structs NAME foRm_print_table_structures
 READ EVENTS
 ON ERROR  &PREV_ERROR
 CD (prEv_dir)
 CLEAR EVENTS
 RETURN
ENDPROC
*
PROCEDURE errhandler
 LPARAMETERS xeRr, xmEss, xmEss1, xlIne
 IF TYPE('_error')='N'
      IF INLIST(xeRr, 1, 1552, 1520, 1562, 11, 12, 24, 202)
           _eRror = 1
           RETURN
      ENDIF
 ENDIF
 ON ERROR  &PREV_ERROR
 xmSg = 'Error in Print_Table_Structures.'+CHR(13)+'Error = '+ ;
        to_char(xeRr)+'   '+xmEss+CHR(13)+'Line = '+to_char(xlIne)+'   '+xmEss1
 = MESSAGEBOX(xmSg, 16, 'Print Table Structures ....')
 foRm_print_table_structures.reLease()
 CLOSE DATABASES
 CLOSE ALL
 CLEAR EVENTS
 CANCEL
 RETURN
ENDPROC
*
*** 
*** ReFox - all is not lost 
***
