*
FUNCTION FCONTENT
 PARAMETER c_Filename, a_Array
 LOCAL c_Message, c_Used
 LOCAL n_Handle, n_Length, n_Params, n_Open, n_Pointer, n_Size
 LOCAL l_Array
 LOCAL i
 PRIVATE c_Filename, a_Array
 n_Size = 100
 n_Params = PARAMETERS()
 l_Array = isArray("A_ARRAY")
 IF ((n_Params=1) .AND. (TYPE("C_FILENAME")="C")) .OR. ((n_Params=2)  ;
    .AND. (TYPE("C_FILENAME")="C") .AND. (l_Array=.T.))
      c_Used = f_N_used(c_Filename)
      n_Open = VAL(LEFT(c_Used, 3))
      IF (n_Open<=0)
           n_Handle = FOPEN(c_Filename)
      ELSE
           n_Handle = n_Open
           n_Pointer = VAL(SUBSTR(c_Used, 6, 10))
      ENDIF
      IF (n_Handle=-1)
           IF exIsts(c_Filename,"F")
                IF LEFT(exIsts(c_Filename,"F",.T.), 1)="D"
                     c_Message = "      4  -  File specified is a directory, not a file."
                ELSE
                     c_Message = "      2  -  File cannot be opened. It may be in use."
                ENDIF
           ELSE
                c_Message = "      1  -  File does not exist."
           ENDIF
      ELSE
           = FSEEK(n_Handle, 0, 0)
           n_Length = FSEEK(n_Handle, 0, 2)
           = FSEEK(n_Handle, 0, 0)
           IF (n_Params=1)
                c_Message = "      0  -  "+FREAD(n_Handle, n_Length)
           ELSE
                c_Message = "      0  -  Success"
                DIMENSION a_Array(100)
                i = 1
                DO WHILE NOT (FEOF(n_Handle))
                     IF (i>n_Size)
                          n_Size = n_Size+100
                          DIMENSION a_Array(n_Size)
                     ENDIF
                     a_Array(i) = FGETS(n_Handle)
                     i = i+1
                ENDDO
                IF i>1
                     DIMENSION a_Array(i-1)
                ENDIF
           ENDIF
           IF n_Open=0
                = FCLOSE(n_Handle)
           ELSE
                = FSEEK(n_Handle, n_Pointer, 0)
           ENDIF
      ENDIF
 ELSE
      IF (n_Params=2) .AND. (NOT (l_Array))
           c_Message = "      5  -  Second parameter is not an array."
      ELSE
           c_Message = "      3  -  Filename parameter is not a character string."
      ENDIF
 ENDIF
 RETURN (c_Message)
ENDFUNC
*
