*
FUNCTION CURR_DIR
 PARAMETER l_Bckslash
 PRIVATE c_This_dir
 PRIVATE l_Bckslash
 l_Bckslash = IIF((TYPE("L_BCKSLASH")="L"), l_Bckslash, .F.)
 c_This_dir = SET("DEFAULT")+CURDIR()
 c_This_dir = IIF((l_Bckslash=.T.), c_This_dir, LEFT(c_This_dir,  ;
              (LEN(c_This_dir)-1)))
 RETURN (c_This_dir)
ENDFUNC
*
