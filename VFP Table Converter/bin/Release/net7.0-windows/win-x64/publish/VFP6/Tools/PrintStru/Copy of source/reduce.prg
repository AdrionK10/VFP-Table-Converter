*
FUNCTION REDUCE
 PARAMETER c_String, l_Spaces, l_Cr_nl, l_Tabs
 PRIVATE c_String, c_Cr_nl
 PRIVATE l_Spaces, l_Cr_nl, l_Tabs
 IF TYPE("C_STRING")<>"C"
      c_String = ""
 ELSE
      DO CASE
           CASE PARAMETERS()=1
                STORE .T. TO l_Spaces, l_Cr_nl, l_Tabs
           CASE PARAMETERS()=2
                l_Spaces = IIF((TYPE("L_SPACES")<>"L"), .T., l_Spaces)
                l_Cr_nl = .T.
                l_Tabs = .T.
           CASE PARAMETERS()=3
                l_Spaces = IIF((TYPE("L_SPACES")<>"L"), .T., l_Spaces)
                l_Cr_nl = IIF((TYPE("L_CR_NL")<>"L"), .T., l_Cr_nl)
                l_Tabs = .T.
           CASE PARAMETERS()=4
                l_Spaces = IIF((TYPE("L_SPACES")<>"L"), .T., l_Spaces)
                l_Cr_nl = IIF((TYPE("L_CR_NL")<>"L"), .T., l_Cr_nl)
                l_Tabs = IIF((TYPE("L_TABS")<>"L"), .T., l_Tabs)
      ENDCASE
      DO CASE
           CASE (l_Cr_nl=.T. .AND. l_Tabs=.T.)
                c_Cr_nl = SPACE(31)
                c_String = STRTRAN(c_String, CHR(0), " ")
                c_String = SYS(15, c_Cr_nl, c_String)
           CASE l_Cr_nl=.T.
                c_Cr_nl = SPACE(8)+CHR(9)+SPACE(22)
                c_String = STRTRAN(c_String, CHR(0), " ")
                c_String = SYS(15, c_Cr_nl, c_String)
           CASE l_Tabs=.T.
                c_Cr_nl = ""+CHR(8)+" "
                c_String = SYS(15, c_Cr_nl, c_String)
      ENDCASE
      IF l_Spaces=.T.
           c_String = STRTRAN(c_String, "        ", " ")
           c_String = STRTRAN(c_String, "       ", " ")
           c_String = STRTRAN(c_String, "      ", " ")
           c_String = STRTRAN(c_String, "     ", " ")
           c_String = STRTRAN(c_String, "    ", " ")
           c_String = STRTRAN(c_String, "   ", " ")
           c_String = STRTRAN(c_String, "  ", " ")
           c_String = STRTRAN(c_String, "  ", " ")
           c_String = STRTRAN(c_String, "  ", " ")
      ENDIF
 ENDIF
 RETURN (c_String)
ENDFUNC
*
