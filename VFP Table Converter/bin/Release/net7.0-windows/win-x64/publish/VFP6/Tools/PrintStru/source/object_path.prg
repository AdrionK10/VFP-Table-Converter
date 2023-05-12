***
*** object_path.fxp
***
*
*
FUNCTION OBJECT_PATH
 LPARAMETERS o_Object_reference
 LOCAL c_Object_path
 c_Object_path = ""
 IF TYPE("O_OBJECT_REFERENCE")=="O"
      IF NOT ISNULL(o_Object_reference)
           IF TYPE("O_OBJECT_REFERENCE.Parent")=="O"
                c_Object_path = obJect_path(o_Object_reference.paRent)+ ;
                                "."+o_Object_reference.naMe
           ELSE
                c_Object_path = o_Object_reference.naMe
           ENDIF
      ENDIF
 ENDIF
 RETURN (c_Object_path)
ENDFUNC
*
