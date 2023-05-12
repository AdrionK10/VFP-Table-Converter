*
FUNCTION LSM_DATE
 PARAMETER d_Date, l_Century, n_Monthlen, l_Expand, a_Months
 EXTERNAL ARRAY a_Months
 PRIVATE i, n_Monthlen
 PRIVATE c_Century, c_Format, c_The_date
 PRIVATE d_Date
 PRIVATE a_Months
 PRIVATE l_Monthsok, l_Century, l_Expand
 DO CASE
      CASE PARAMETERS()=0
           d_Date = DATE()
           l_Century = .F.
           n_Monthlen = 3
           l_Expand = .F.
           l_Monthsok = .F.
      CASE PARAMETERS()=1
           d_Date = IIF(TYPE("D_DATE")="D", d_Date, DATE())
           d_Date = IIF(d_Date={}, DATE(), d_Date)
           l_Century = .F.
           n_Monthlen = 3
           l_Expand = .F.
           l_Monthsok = .F.
      CASE PARAMETERS()=2
           d_Date = IIF(TYPE("D_DATE")="D", d_Date, DATE())
           d_Date = IIF(d_Date={}, DATE(), d_Date)
           l_Century = IIF(TYPE("L_CENTURY")="L", l_Century, .F.)
           n_Monthlen = 3
           l_Expand = .F.
           l_Monthsok = .F.
      CASE PARAMETERS()=3
           d_Date = IIF(TYPE("D_DATE")="D", d_Date, DATE())
           d_Date = IIF(d_Date={}, DATE(), d_Date)
           l_Century = IIF(TYPE("L_CENTURY")="L", l_Century, .F.)
           n_Monthlen = IIF(TYPE("N_MONTHLEN")="N", n_Monthlen, 3)
           n_Monthlen = IIF(BETWEEN(n_Monthlen, 3, 15), n_Monthlen, 3)
           l_Expand = .F.
           l_Monthsok = .F.
      CASE PARAMETERS()=4
           d_Date = IIF(TYPE("D_DATE")="D", d_Date, DATE())
           d_Date = IIF(d_Date={}, DATE(), d_Date)
           l_Century = IIF(TYPE("L_CENTURY")="L", l_Century, .F.)
           n_Monthlen = IIF(TYPE("N_MONTHLEN")="N", n_Monthlen, 3)
           n_Monthlen = IIF(BETWEEN(n_Monthlen, 3, 15), n_Monthlen, 3)
           l_Expand = IIF(TYPE("L_EXPAND")="L", l_Expand, .F.)
           l_Monthsok = .F.
      CASE PARAMETERS()>=5
           d_Date = IIF(TYPE("D_DATE")="D", d_Date, DATE())
           d_Date = IIF(d_Date={}, DATE(), d_Date)
           l_Century = IIF(TYPE("L_CENTURY")="L", l_Century, .F.)
           n_Monthlen = IIF(TYPE("N_MONTHLEN")="N", n_Monthlen, 3)
           n_Monthlen = IIF(BETWEEN(n_Monthlen, 3, 15), n_Monthlen, 3)
           l_Expand = IIF(TYPE("L_EXPAND")="L", l_Expand, .F.)
           l_Monthsok = isArray("A_MONTHS")
           l_Monthsok = IIF((l_Monthsok=.T. .AND. ALEN(a_Months)=12 .AND.  ;
                        ALEN(a_Months, 2)=0), .T., .F.)
           IF l_Monthsok=.T.
                FOR i = 1 TO 12
                     IF TYPE("A_MONTHS(I)")<>"C"
                          l_Monthsok = .F.
                          EXIT
                     ENDIF
                ENDFOR
           ENDIF
 ENDCASE
 c_Format = SET("DATE")
 c_Century = SET("CENTURY")
 SET DATE TO BRITISH
 SET CENTURY ON
 IF l_Expand=.F.
      c_The_date = STR(DAY(d_Date), 2)+"-"
      IF LEFT(c_The_date, 1)=="0"
           c_The_date = " "+RIGHT(c_The_date, 2)
      ENDIF
      IF l_Monthsok=.T.
           c_The_date = c_The_date+ ;
                        PROPER(LEFT(ALLTRIM(a_Months(MONTH(d_Date))),  ;
                        n_Monthlen))+"-"
      ELSE
           c_The_date = c_The_date+LEFT(CMONTH(d_Date), n_Monthlen)+"-"
      ENDIF
      IF l_Century=.T.
           c_The_date = c_The_date+STR(YEAR(d_Date), 4)
      ELSE
           c_The_date = c_The_date+RIGHT(STR(YEAR(d_Date), 4), 2)
      ENDIF
 ELSE
      IF l_Monthsok=.T.
           c_The_date = STR(DAY(d_Date), 2)+" "
           IF LEFT(c_The_date, 1)=="0"
                c_The_date = " "+RIGHT(c_The_date, 2)
           ENDIF
           c_The_date = c_The_date+ ;
                        PROPER(ALLTRIM(a_Months(MONTH(d_Date))))+" "+ ;
                        STR(YEAR(d_Date), 4)
      ELSE
           c_The_date = DMY(d_Date)
           IF LEFT(c_The_date, 1)=="0"
                c_The_date = STUFF(c_The_date, 1, 1, " ")
           ENDIF
      ENDIF
 ENDIF
 SET DATE TO  &C_FORMAT					
 SET CENTURY  &C_CENTURY					
 RETURN (c_The_date)
ENDFUNC
*
