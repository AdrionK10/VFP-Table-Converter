***
*** temp_files_dir.fxp
***
*
*
FUNCTION TEMP_FILES_DIR
 LPARAMETERS l_Backslash
 LOCAL c_Temp_dir
 l_Backslash = IIF((TYPE("L_BACKSLASH")=="L"), l_Backslash, .F.)
 c_Temp_dir = SYS(2023)
 c_Temp_dir = IIF(EMPTY(c_Temp_dir), "", (c_Temp_dir+IIF((l_Backslash= ;
              .T.), "\", "")))
 RETURN (c_Temp_dir)
ENDFUNC
*
