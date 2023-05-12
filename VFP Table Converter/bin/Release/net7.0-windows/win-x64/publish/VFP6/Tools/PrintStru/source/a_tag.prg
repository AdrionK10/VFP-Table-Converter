*
PROCEDURE A_TAG
 PARAMETER c_Error, a_Array_1, a_Array_2, a_Array_3
 PRIVATE n_Params, n_Rows_1, n_Rows_2, n_Columns1, n_Columns2
 PRIVATE n_Start
 PRIVATE i
 n_Params = PARAMETERS()
 c_Error = ""
 IF n_Params>=4
      IF (isArray("A_ARRAY_1") .AND. isArray("A_ARRAY_2"))
           n_Rows_1 = ALEN(a_Array_1, 1)
           n_Columns1 = IIF((ALEN(a_Array_1, 2)<2), 0, ALEN(a_Array_1, 2))
           n_Columns1 = IIF((n_Columns1=0), 1, n_Columns1)
           n_Rows_2 = ALEN(a_Array_2, 1)
           n_Columns2 = IIF((ALEN(a_Array_2, 2)<2), 0, ALEN(a_Array_2, 2))
           n_Columns2 = IIF((n_Columns2=0), 1, n_Columns2)
           IF n_Columns1=n_Columns2
                DIMENSION a_Array_3((n_Rows_1+n_Rows_2), n_Columns1)
                = ACOPY(a_Array_1, a_Array_3)
                n_Start = ALEN(a_Array_1)+1
                = ACOPY(a_Array_2, a_Array_3, 1, -1, n_Start)
           ELSE
                c_Error = STR(-3, 7)+ ;
                          "  The two source arrays do not have the same "+ ;
                          "number of columns."
           ENDIF
      ELSE
           c_Error = STR(-2, 7)+ ;
                     "  One of the two array parameters is not of type  ARRAY."
      ENDIF
 ELSE
      c_Error = STR(-1, 7)+ ;
                "  Less than the minimum of 4 parameters were passed to "+ ;
                "the routine."
 ENDIF
 RETURN
ENDPROC
*
