*FieldlistWithStructureSortable
* no field contents, sorted by field name
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
 
=afields(aFieldList)
strtofile("Dbc Name "+Alltrim(DBC())+" View Name "+Alltrim(aFieldList[1,12])+chr(13) , (lcAlternate) , .T.)
asort(aFieldList)

FOR nloopCnt = 1 to alen(aFieldList,1)  && # of rows in array
 PRIVATE (FIELD(nloopCnt))
 lcWholeFieldName = aFieldList[nloopCnt,1]
 lcFieldName = proper(lcWholeFieldName)
 lcFieldType = Alltrim(aFieldList[nloopCnt,2])
 lcFieldLen = Transform(aFieldList[nloopCnt,3])
 lcFieldDec = Transform(aFieldList[nloopCnt,4])
 IF UPPER(lcFieldType) # "G"
	 lcFieldContents = Transform(EVAL(lcFieldName))
 Else
	 lcFieldContents = "*General*"	 
 Endif	 
	 lcString = PROPER(ALIAS())+"."+padr(lcFieldName, 25)+" "+lcFieldType+"("+lcFieldLen+") "+lcFieldDec+chr(13)       && +lcFieldContents+chr(13)
	 strtofile( (lcString) , (lcAlternate) , .T.)
ENDFOR 
modi file (lcAlternate)
erase (lcAlternate)
else
 =messagebox("Nothing open in this work area")
endif