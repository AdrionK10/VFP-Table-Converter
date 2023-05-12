*
*  X3ED.PRG
*  Encrypt/Decrypt text
*
*  Copyright (c) 1997  GE Capital Consulting, Inc.  (Public Domain)  All Rights Reserved
*                      14170 Newbrook Drive, Suite 100
*                      Chantilly, VA  20151
*                      703-631-6500
*  Author:  Drew Speedie
*
*  TYPICAL USAGE:
*    replace Table.Field with x3edt("E",m.somevalue)
*    lcString = x3edt("D",Table.Field)
*    browse fields ... calc1=x3edt("D",LastName):H="Last Name" ...
*
*
*  NOTE:  X3ED is around 30% faster if memvar gcAscii
*         is available.  If gcAscii does not already
*         exist, it will be here and thus be available
*         for subsequent calls to X3ED
*
*
*  PARAMETERS:
*    tcAction = "E"ncrypt/"D"ecrypt
*    tcString = character string to be Encrypted/Decrypted
*
*
parameters tcAction, tcString
local lnLen, ;
      lcString, ;
      lnXX, ;
      lnCharPos, ;
      lnAscChar, ;
      lcString2, ;
      lcRetVal
IF type("gcAscii") = "U" ;
     OR leftc(gcAscii,1) # chr(255) ;
     OR rightc(gcAscii,1) # chr(0)
  public gcAscii
  gcAscii = space(0)
  FOR lnXX = 255 to 0 step -1
    gcAscii = gcAscii + chr(lnXX)
  ENDFOR
ENDIF
lnLen = lenc(Alltrim(tcString))
IF tcAction = "E"   && encrypt
  lcString = space(0)
  FOR lnXX = 1 to lnLen
    lnCharPos = lnLen-lnXX+1
    lnAscChar = asc(substrc(tcString,lnCharPos,1))+IIF(mod(lnXX,2)=0,1,2)
    * if you'd like it a little more scrambled, use this variation
    * and pay a .01/per penalty:
    *lnAscChar = asc(substrc(tcString,lnCharPos,1)) + ;
         IIF(mod(lnXX,7)=0,2,IIF(mod(lnXX,4)=0,1,-1))
    lcString = lcString + chr(lnAscChar)
  ENDFOR
  return sys(15,gcAscii,lcString)
 ELSE tcAction = "D" && decrypt
  lcString = sys(15,gcAscii,tcString)
  store space(0) to lcString2, lcRetVal
  FOR lnXX = 1 to lnLen
    lcString2 = lcString2 + chr(asc(substrc(lcString,lnXX,1)) - IIF(mod(lnXX,2)=0,1,2))
    * if you'd like it a little more scrambled, use this variation
    * and pay a .01/per penalty:
    *lcString2 = lcString2 + chr(asc(substrc(lcString,lnXX,1)) - ;
         IIF(mod(lnXX,7)=0,2,IIF(mod(lnXX,4)=0,1,-1)))
  ENDFOR
  FOR lnXX = 1 to lnLen
    lcRetVal = lcRetVal + substrc(lcString2,lnLen-lnXX+1,1)
  ENDFOR
  return lcRetVal
ENDIF  



PROCEDURE keywords
* ENCRYPTION|MEMO FIELDS
return