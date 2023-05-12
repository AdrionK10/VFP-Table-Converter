*
FUNCTION A_CLEAN
 PARAMETER a_Array, n_Column
 PRIVATE n_Column, n_Params, n_Deleted, n_Rows, n_Cols, n_Row
 PRIVATE n_Origcols, n_Startrow
 PRIVATE a_Array
 PRIVATE x_Value, x_Type, x_Thiscell
 PRIVATE prEv_exact
 n_Params = PARAMETERS()
 n_Deleted = 0
 prEv_exact = SET("EXACT")
 SET EXACT ON
 IF n_Params>=2
      IF (isArray("A_ARRAY") .AND. is_intgr(n_Column))
           n_Rows = ALEN(a_Array, 1)
           n_Cols = ALEN(a_Array, 2)
           n_Origcols = n_Cols
           n_Cols = IIF((n_Cols=0), 1, n_Cols)
           n_Startrow = 1
           IF BETWEEN(n_Column, 1, n_Cols)
                IF n_Origcols=0
                     DO WHILE n_Startrow<n_Rows
                          x_Value = a_Array(n_Startrow)
                          x_Type = TYPE("X_VALUE")
                          n_Row = n_Startrow+1
                          DO WHILE n_Row<=ALEN(a_Array, 1)
                               x_Thiscell = a_Array(n_Row)
                               IF (TYPE("X_THISCELL")==x_Type .AND.  ;
                                  x_Thiscell==x_Value)
                                    = ADEL(a_Array, n_Row)
                                    n_Deleted = n_Deleted+1
                                    n_Rows = n_Rows-1
                                    DIMENSION a_Array(n_Rows)
                               ELSE
                                    n_Row = n_Row+1
                               ENDIF
                          ENDDO
                          n_Startrow = n_Startrow+1
                     ENDDO
                ELSE
                     DO WHILE n_Startrow<n_Rows
                          x_Value = a_Array(n_Startrow,n_Column)
                          x_Type = TYPE("X_VALUE")
                          n_Row = n_Startrow+1
                          DO WHILE n_Row<=ALEN(a_Array, 1)
                               x_Thiscell = a_Array(n_Row,n_Column)
                               IF (TYPE("X_THISCELL")==x_Type .AND.  ;
                                  x_Thiscell==x_Value)
                                    = ADEL(a_Array, n_Row)
                                    n_Deleted = n_Deleted+1
                                    n_Rows = n_Rows-1
                                    DIMENSION a_Array(n_Rows, n_Cols)
                               ELSE
                                    n_Row = n_Row+1
                               ENDIF
                          ENDDO
                          n_Startrow = n_Startrow+1
                     ENDDO
                ENDIF
           ELSE
                n_Deleted = -3
           ENDIF
      ELSE
           n_Deleted = -2
      ENDIF
 ELSE
      n_Deleted = -1
 ENDIF
 SET EXACT	&PREV_EXACT
 RETURN (n_Deleted)
ENDFUNC
*
