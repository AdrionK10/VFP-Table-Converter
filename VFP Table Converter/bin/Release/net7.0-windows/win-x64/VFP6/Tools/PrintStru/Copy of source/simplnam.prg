*
FUNCTION SIMPLNAM
 PARAMETER c_Filename
 PRIVATE c_Filename, c_Xtension
 c_Filename = fiLename(c_Filename,"F")
 c_Xtension = xtEnsion(c_Filename,.T.)
 c_Filename = LEFT(c_Filename, (LEN(c_Filename)-LEN(c_Xtension)))
 IF RIGHT(c_Filename, 1)=="."
      c_Filename = LEFT(c_Filename, (LEN(c_Filename)-1))
 ENDIF
 RETURN (c_Filename)
ENDFUNC
*
