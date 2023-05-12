*Fieldlist.prg

LOCAL nloopCnt, lcAlternate, lcWholeFieldName, lcFieldName, lcString, lcFieldType, lcFieldContents

lcFieldName = ""
lcAlternate = ""
lcWholeFieldName = ""
lcString = ""
lcFieldType = ""
lcFieldContents = ""

set alternate to
set alternate off

if not empty(alias())
 lcAlternate = alias()+".str"
 if file(lcAlternate)
  erase (lcAlternate)
 endif
FOR nloopCnt = 1 to fcount()
 PRIVATE (FIELD(nloopCnt))

 lcWholeFieldName = field(nloopCnt)
 lcFieldName = proper(lcWholeFieldName)
 lcFieldLen = Transform(fsize(field(nloopCnt)))
 lcFieldType = TYPE(FIELD(nloopCnt))
 IF UPPER(lcFieldType) # "G"
	 lcFieldContents = Transform(EVAL(FIELD(nloopCnt)))
 Else
	 lcFieldContents = "*General*"	 
 Endif	 
	 lcString = PROPER(ALIAS())+"."+padr(lcFieldName, 25)+" "+lcFieldType+"("+lcFieldLen+") "+lcFieldContents+chr(13)
	 strtofile( (lcString) , (lcAlternate) , .T.)
ENDFOR 
modi file (lcAlternate)
erase (lcAlternate)
else
 =messagebox("Nothing open in this work area")
endif