*
FUNCTION GOTO_DIR
 LPARAMETERS c_Dir_path, l_Stay_there
 LOCAL c_Full_path
 LOCAL n_Params
 LOCAL l_Good_path
 LOCAL prEv_error, prEv_dir
 n_Params = PARAMETERS()
 IF (TYPE("C_DIR_PATH")<>"C" .OR. EMPTY(c_Dir_path))
      c_Full_path = ""
 ELSE
      c_Dir_path = UPPER(ALLTRIM(c_Dir_path))
      l_Stay_there = IIF((n_Params=2), IIF((TYPE("L_STAY_THERE")="L"),  ;
                     l_Stay_there, .T.), .T.)
      prEv_error = ON("ERROR")
      prEv_dir = cuRr_dir(.T.)
      l_Good_path = .T.
      ON ERROR L_GOOD_PATH = .F.
      SET DEFAULT TO (c_Dir_path)
      ON ERROR		&PREV_ERROR
      c_Full_path = IIF((l_Good_path=.T.), cuRr_dir(.T.), "")
      IF (l_Good_path=.F. .OR. l_Stay_there=.F.)
           SET DEFAULT TO (prEv_dir)
      ENDIF
 ENDIF
 RETURN (c_Full_path)
ENDFUNC
*
