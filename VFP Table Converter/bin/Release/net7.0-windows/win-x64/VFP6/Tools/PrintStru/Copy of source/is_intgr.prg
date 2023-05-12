*
FUNCTION IS_INTGR
 PARAMETER n_Number, n_Start, n_End
 PRIVATE n_End, n_Number, n_Start, n_Params
 PRIVATE l_Answer
 n_Params = PARAMETERS()
 l_Answer = .T.
 l_Answer = IIF((TYPE("N_NUMBER")<>"N") .OR. (INT(n_Number)<>n_Number),  ;
            .F., l_Answer)
 IF (l_Answer=.T.)
      DO CASE
           CASE (n_Params=2)
                l_Answer = IIF((TYPE("N_START")<>"N") .OR. (n_Number< ;
                           n_Start), .F., l_Answer)
           CASE (n_Params=3)
                IF (TYPE("N_START")="L") .AND. (n_Start=.F.)
                     l_Answer = IIF((TYPE("N_END")<>"N") .OR. (n_Number> ;
                                n_End), .F., l_Answer)
                ELSE
                     l_Answer = IIF((TYPE("N_START")<>"N") .OR.  ;
                                (TYPE("N_END")<>"N") .OR. (NOT  ;
                                (BETWEEN(n_Number, n_Start, n_End))), .F.,  ;
                                l_Answer)
                ENDIF
      ENDCASE
 ENDIF
 RETURN (l_Answer)
ENDFUNC
*
