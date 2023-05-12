***
*** moduls.fxp
***
*
*
FUNCTION CPchange
 LPARAMETERS tcFullpathtable, tnCodepage
 LOCAL llRet
 PRIVATE atAbleinfo, afIeldinfo
 DIMENSION atAbleinfo[10], afIeldinfo[1, 7]
 atAbleinfo[9] = m.tnCodepage
 llRet = 0=taBleheader(foRce_dbf(m.tcFullpathtable,.T.),2,@atAbleinfo, ;
         @afIeldinfo)
 RETURN m.llRet
ENDFUNC
*
FUNCTION AddToDBC
 LPARAMETERS tcFullpathtable, tcPathtowrite
 LOCAL llRet
 PRIVATE atAbleinfo, afIeldinfo
 DIMENSION atAbleinfo[10], afIeldinfo[1, 7]
 tcFullpathtable = foRce_dbf(m.tcFullpathtable,.T.)
 DO CASE
      CASE UPPER(JUSTPATH(m.tcFullpathtable))==UPPER(JUSTPATH(m.tcPathtowrite))
           tcPathtowrite = JUSTFNAME(m.tcPathtowrite)
 ENDCASE
 atAbleinfo[10] = LOWER(m.tcPathtowrite)
 llRet = 0=taBleheader(m.tcFullpathtable,2,@atAbleinfo,@afIeldinfo)
 RETURN m.llRet
ENDFUNC
*
FUNCTION TableHeader
 LPARAMETERS tcTable, tnOpenmode, atAbleinfo, afIeldinfo
 LOCAL lnHand, lcBuffer, lnVal, lnRet, lnTypetable, lnNofields,  ;
       lnHeadersize, lnX, lcTemp, i
 lnRet = 0
 DIMENSION atAbleinfo[10]
 lnHand = FOPEN(m.tcTable, m.tnOpenmode)
 IF m.lnHand=-1
      RETURN -1
 ENDIF
 lnVal = FSEEK(m.lnHand, 0, 2)
 IF m.tnOpenmode=2 .AND. VARTYPE(atAbleinfo(1))='N'
      IF FCHSIZE(m.lnHand, atAbleinfo(1))<0
           lnRet = 3
      ENDIF
 ELSE
      atAbleinfo[1] = m.lnVal
 ENDIF
 IF atAbleinfo(1)=0
      = FCLOSE(m.lnHand)
      RETURN 1
 ENDIF
 = FSEEK(m.lnHand, 0, 0)
 IF m.tnOpenmode=2 .AND. VARTYPE(atAbleinfo(2))='N'
      lnTypetable = atAbleinfo(2)
      lcBuffer = CHR(m.lnTypetable)
      IF FWRITE(m.lnHand, m.lcBuffer)=0
           lnRet = 3
      ENDIF
 ELSE
      lcBuffer = FREAD(m.lnHand, 1)
      lnTypetable = ASC(m.lcBuffer)
      atAbleinfo[2] = m.lnTypetable
 ENDIF
 IF NOT INLIST(m.lnTypetable, 2, 3, 48, 67, 99, 131, 139, 203, 245, 251)
      = FCLOSE(m.lnHand)
      RETURN 2
 ENDIF
 lcBuffer = FREAD(m.lnHand, 3)
 lnVal = ASC(SUBSTR(m.lcBuffer, 1, 1))
 atAbleinfo[3] = DATE(IIF(m.lnVal>30, 1900, 2000)+m.lnVal,  ;
           ASC(SUBSTR(m.lcBuffer, 2, 1)), ASC(SUBSTR(m.lcBuffer, 3, 1)))
 IF m.tnOpenmode=2 .AND. VARTYPE(atAbleinfo(4))='N'
      lcBuffer = deCtobytes(atAbleinfo(4),4)
      IF FWRITE(m.lnHand, m.lcBuffer)=0
           lnRet = 3
      ENDIF
 ELSE
      lcBuffer = FREAD(m.lnHand, 4)
      atAbleinfo[4] = byTestodec(m.lcBuffer)
 ENDIF
 IF m.tnOpenmode=2 .AND. VARTYPE(atAbleinfo(5))='N'
      lnHeadersize = atAbleinfo(5)
      lcBuffer = deCtobytes(m.lnHeadersize,2)
      IF FWRITE(m.lnHand, m.lcBuffer)=0
           lnRet = 3
      ENDIF
 ELSE
      lcBuffer = FREAD(m.lnHand, 2)
      lnHeadersize = byTestodec(m.lcBuffer)
      atAbleinfo[5] = m.lnHeadersize
 ENDIF
 IF INLIST(m.lnTypetable, 48)
      lnNofields = INT(((m.lnHeadersize-296)/32))
 ELSE
      lnNofields = INT((m.lnHeadersize+1)/32)-1
 ENDIF
 atAbleinfo[6] = m.lnNofields
 IF m.tnOpenmode=2 .AND. VARTYPE(atAbleinfo(7))='N'
      lcBuffer = deCtobytes(atAbleinfo(7),2)
      IF FWRITE(m.lnHand, m.lcBuffer)=0
           lnRet = 3
      ENDIF
 ELSE
      lcBuffer = FREAD(m.lnHand, 2)
      atAbleinfo[7] = byTestodec(m.lcBuffer)
 ENDIF
 lcBuffer = FREAD(m.lnHand, 16)
 IF m.tnOpenmode=2 .AND. VARTYPE(atAbleinfo(8))='N'
      lcBuffer = CHR(atAbleinfo(8))
      IF FWRITE(m.lnHand, m.lcBuffer)=0
           lnRet = 3
      ENDIF
 ELSE
      lcBuffer = FREAD(m.lnHand, 1)
      atAbleinfo[8] = ASC(m.lcBuffer)
 ENDIF
 PRIVATE cpNums
 LOCAL lnCptotal
 lnCptotal = 22
 DIMENSION cpNums[m.lnCptotal, 2]
 cpNums[1, 1] = 437
 cpNums[1, 2] = 1
 cpNums[2, 1] = 850
 cpNums[2, 2] = 2
 cpNums[3, 1] = 1252
 cpNums[3, 2] = 3
 cpNums[4, 1] = 10000
 cpNums[4, 2] = 4
 cpNums[5, 1] = 852
 cpNums[5, 2] = 100
 cpNums[6, 1] = 866
 cpNums[6, 2] = 101
 cpNums[7, 1] = 865
 cpNums[7, 2] = 102
 cpNums[8, 1] = 861
 cpNums[8, 2] = 103
 cpNums[9, 1] = 895
 cpNums[9, 2] = 104
 cpNums[10, 1] = 620
 cpNums[10, 2] = 105
 cpNums[11, 1] = 737
 cpNums[11, 2] = 106
 cpNums[12, 1] = 857
 cpNums[12, 2] = 107
 cpNums[13, 1] = 863
 cpNums[13, 2] = 108
 cpNums[14, 1] = 10007
 cpNums[14, 2] = 150
 cpNums[15, 1] = 10029
 cpNums[15, 2] = 151
 cpNums[16, 1] = 10006
 cpNums[16, 2] = 152
 cpNums[17, 1] = 1250
 cpNums[17, 2] = 200
 cpNums[18, 1] = 1251
 cpNums[18, 2] = 201
 cpNums[19, 1] = 1253
 cpNums[19, 2] = 203
 cpNums[20, 1] = 1254
 cpNums[20, 2] = 202
 cpNums[21, 1] = 1257
 cpNums[21, 2] = 204
 cpNums[22, 1] = 0
 cpNums[22, 2] = 0
 IF m.tnOpenmode=2 .AND. VARTYPE(atAbleinfo(9))='N'
      FOR i = 1 TO m.lnCptotal
           IF atAbleinfo(9)=cpNums(m.i,1)
                atAbleinfo[9] = cpNums(m.i,2)
                EXIT
           ENDIF
      ENDFOR
      lcBuffer = CHR(atAbleinfo(9))
      IF FWRITE(m.lnHand, m.lcBuffer)=0
           lnRet = 3
      ENDIF
 ELSE
      lcBuffer = FREAD(m.lnHand, 1)
      atAbleinfo[9] = ASC(m.lcBuffer)
      FOR i = 1 TO m.lnCptotal
           IF atAbleinfo(9)=cpNums(m.i,2)
                atAbleinfo[9] = cpNums(m.i,1)
                EXIT
           ENDIF
      ENDFOR
 ENDIF
 lcBuffer = FREAD(m.lnHand, 2)
 DIMENSION afIeldinfo[m.lnNofields, 7]
 FOR lnX = 1 TO m.lnNofields
      IF m.tnOpenmode=2 .AND. VARTYPE(afIeldinfo(m.lnX,1))='C'
           lcBuffer = PADR(ALLTRIM(UPPER(afIeldinfo(m.lnX,1))), 11, CHR(0))
           IF FWRITE(m.lnHand, m.lcBuffer)=0
                lnRet = 3
           ENDIF
      ELSE
           afIeldinfo[m.lnX, 1] = trIm1(FREAD(m.lnHand, 11),CHR(0))
      ENDIF
      IF m.tnOpenmode=2 .AND. VARTYPE(afIeldinfo(m.lnX,2))='C'
           lcBuffer = PADR(ALLTRIM(UPPER(afIeldinfo(m.lnX,2))), 1)
           IF FWRITE(m.lnHand, m.lcBuffer)=0
                lnRet = 3
           ENDIF
      ELSE
           afIeldinfo[m.lnX, 2] = FREAD(m.lnHand, 1)
      ENDIF
      IF m.tnOpenmode=2 .AND. VARTYPE(afIeldinfo(m.lnX,3))='N'
           lcBuffer = deCtobytes(afIeldinfo(m.lnX,3),4)
           IF FWRITE(m.lnHand, m.lcBuffer)=0
                lnRet = 3
           ENDIF
      ELSE
           lcBuffer = FREAD(m.lnHand, 4)
           afIeldinfo[m.lnX, 3] = byTestodec(m.lcBuffer)
      ENDIF
      IF m.tnOpenmode=2 .AND. VARTYPE(afIeldinfo(m.lnX,4))='N'
           lcBuffer = CHR(afIeldinfo(m.lnX,4))
           IF FWRITE(m.lnHand, m.lcBuffer)=0
                lnRet = 3
           ENDIF
      ELSE
           afIeldinfo[m.lnX, 4] = ASC(FREAD(m.lnHand, 1))
      ENDIF
      IF m.tnOpenmode=2 .AND. VARTYPE(afIeldinfo(m.lnX,5))='N'
           lcBuffer = CHR(afIeldinfo(m.lnX,5))
           IF FWRITE(m.lnHand, m.lcBuffer)=0
                lnRet = 3
           ENDIF
      ELSE
           afIeldinfo[m.lnX, 5] = ASC(FREAD(m.lnHand, 1))
      ENDIF
      IF m.tnOpenmode=2 .AND. VARTYPE(afIeldinfo(m.lnX,6))='N'
           lcBuffer = CHR(afIeldinfo(m.lnX,6))
           IF FWRITE(m.lnHand, m.lcBuffer)=0
                lnRet = 3
           ENDIF
      ELSE
           afIeldinfo[m.lnX, 6] = ASC(FREAD(m.lnHand, 1))
      ENDIF
      afIeldinfo[m.lnX, 7] = FREAD(m.lnHand, 13)
 ENDFOR
 lcBuffer = FREAD(m.lnHand, 1)
 lnVal = FSEEK(m.lnHand, 0, 1)
 lnVal = MIN(263, m.lnHeadersize-m.lnVal)
 IF m.tnOpenmode=2 .AND. VARTYPE(atAbleinfo(10))='C' .AND. m.lnVal>0
      lcBuffer = PADR(atAbleinfo(10), m.lnVal, CHR(0))
      IF FWRITE(m.lnHand, m.lcBuffer)=0
           lnRet = 3
      ENDIF
 ELSE
      atAbleinfo[10] = ''
      IF m.lnVal>0
           lcBuffer = FREAD(m.lnHand, m.lnVal)
           IF SUBSTR(m.lcBuffer, 1, 1)=CHR(0)
                atAbleinfo[10] = ''
           ELSE
                atAbleinfo[10] = trIm1(m.lcBuffer,CHR(0))
           ENDIF
      ENDIF
 ENDIF
 = FCLOSE(m.lnHand)
 RETURN m.lnRet
ENDFUNC
*
FUNCTION TableData
 LPARAMETERS tcTable, lnHand, tnOpenmode, tnHeadersize, tnRecordsize,  ;
             afIeldinfo, tnRecord, arEcdata, tcDelmark, tlConvert
 LOCAL lcBuffer, lnVal, lnRet, lnNofields, lnStart, i, llClose
 lnRet = 0
 IF m.lnHand=-1
      lnHand = FOPEN(m.tcTable, m.tnOpenmode)
      llClose = .T.
      IF m.lnHand=-1
           RETURN -1
      ENDIF
      = FSEEK(m.lnHand, 8, 0)
      lcBuffer = FREAD(m.lnHand, 2)
      tnHeadersize = byTestodec(m.lcBuffer)
      lcBuffer = FREAD(m.lnHand, 2)
      tnRecordsize = byTestodec(m.lcBuffer)
 ENDIF
 EXTERNAL ARRAY afIeldinfo
 lnNofields = ALEN(afIeldinfo, 1)
 DIMENSION arEcdata[m.lnNofields]
 lnStart = m.tnHeadersize+m.tnRecordsize*(m.tnRecord-1)
 IF m.lnStart<>FSEEK(m.lnHand, m.lnStart, 0)
      IF m.llClose
           = FCLOSE(m.lnHand)
      ENDIF
      RETURN 1
 ENDIF
 tcDelmark = FREAD(m.lnHand, 1)
 IF m.tlConvert
      tcDelmark = m.tcDelmark='*'
 ENDIF
 FOR i = 1 TO m.lnNofields
      arEcdata[m.i] = FREAD(m.lnHand, afIeldinfo(m.i,4))
      IF m.tlConvert
           DO CASE
                CASE afIeldinfo(m.i,2)$'L'
                     arEcdata[m.i] = arEcdata(m.i)='T'
                CASE afIeldinfo(m.i,2)$'D'
                     arEcdata[m.i] = stOd(arEcdata(m.i))
                CASE afIeldinfo(m.i,2)$'NF'
                     arEcdata[m.i] = VAL(arEcdata(m.i))
                     IF afIeldinfo(m.i,5)=0
                          arEcdata[m.i] = INT(arEcdata(m.i))
                     ENDIF
                CASE afIeldinfo(m.i,2)$'I'
                     arEcdata[m.i] = byTestodec(arEcdata(m.i))
                CASE afIeldinfo(m.i,2)$'MGP'
                     IF afIeldinfo(m.i,4)=10
                          arEcdata[m.i] = INT(VAL(arEcdata(m.i)))
                     ELSE
                          arEcdata[m.i] = byTestodec(arEcdata(m.i))
                     ENDIF
                CASE afIeldinfo(m.i,2)$'Y'
                CASE afIeldinfo(m.i,2)$'T'
           ENDCASE
      ENDIF
 ENDFOR
 IF m.llClose
      = FCLOSE(m.lnHand)
 ENDIF
 RETURN m.lnRet
ENDFUNC
*
FUNCTION MemoHeader
 LPARAMETERS tcFile, tnOpenmode, amEmoinfo
 LOCAL lnHand, lcBuffer, lnVal, lnRet, lnHeadersize
 lnRet = 0
 lnHeadersize = 512
 DIMENSION amEmoinfo[3]
 lnHand = FOPEN(m.tcFile, m.tnOpenmode)
 IF m.lnHand=-1
      RETURN -1
 ENDIF
 lnVal = FSEEK(m.lnHand, 0, 2)
 IF m.tnOpenmode=2 .AND. VARTYPE(amEmoinfo(1))='N'
      IF FCHSIZE(m.lnHand, amEmoinfo(1))<0
           lnRet = 3
      ENDIF
 ELSE
      amEmoinfo[1] = m.lnVal
 ENDIF
 IF amEmoinfo(1)<m.lnHeadersize
      = FCLOSE(m.lnHand)
      RETURN 1
 ENDIF
 = FSEEK(m.lnHand, 0, 0)
 IF m.tnOpenmode=2 .AND. VARTYPE(amEmoinfo(2))='N'
      lcBuffer = deCtobytes(amEmoinfo(2),4,.T.)
      IF FWRITE(m.lnHand, m.lcBuffer)=0
           lnRet = 3
      ENDIF
 ELSE
      lcBuffer = FREAD(m.lnHand, 4)
      amEmoinfo[2] = byTestodec(m.lcBuffer,.T.)
 ENDIF
 lcBuffer = FREAD(m.lnHand, 2)
 IF m.tnOpenmode=2 .AND. VARTYPE(amEmoinfo(3))='N'
      lcBuffer = deCtobytes(amEmoinfo(3),2,.T.)
      IF FWRITE(m.lnHand, m.lcBuffer)=0
           lnRet = 3
      ENDIF
 ELSE
      lcBuffer = FREAD(m.lnHand, 2)
      amEmoinfo[3] = byTestodec(m.lcBuffer,.T.)
 ENDIF
 = FCLOSE(m.lnHand)
 RETURN m.lnRet
ENDFUNC
*
FUNCTION MemoData
 LPARAMETERS tcFile, lnHand, tnOpenmode, tnBlocknumber, amEmoinfo
 LOCAL lnHand, lcBuffer, lnDatalength, lnRet, lnHeadersize, lnMemoheader,  ;
       lnBlocksize, lnFilesize, lnStart, llClose
 lnRet = 0
 lnHeadersize = 512
 lnMemoheader = 8
 DIMENSION amEmoinfo[4]
 IF m.lnHand=-1
      lnHand = FOPEN(m.tcFile, m.tnOpenmode)
      llClose = .T.
      IF m.lnHand=-1
           RETURN -1
      ENDIF
 ENDIF
 lnFilesize = FSEEK(m.lnHand, 0, 2)
 IF m.lnFilesize<m.lnHeadersize
      IF m.llClose
           = FCLOSE(m.lnHand)
      ENDIF
      RETURN 1
 ENDIF
 = FSEEK(m.lnHand, 6, 0)
 lcBuffer = FREAD(m.lnHand, 2)
 lnBlocksize = byTestodec(m.lcBuffer,.T.)
 amEmoinfo[1] = m.lnBlocksize
 lnStart = m.lnBlocksize*m.tnBlocknumber
 IF m.lnStart+m.lnMemoheader>m.lnFilesize
      IF m.llClose
           = FCLOSE(m.lnHand)
      ENDIF
      RETURN 2
 ENDIF
 = FSEEK(m.lnHand, m.lnStart, 0)
 IF m.tnOpenmode=2 .AND. VARTYPE(amEmoinfo(2))='N'
      lcBuffer = deCtobytes(amEmoinfo(2),4,.T.)
      IF FWRITE(m.lnHand, m.lcBuffer)=0
           lnRet = 5
      ENDIF
 ELSE
      lcBuffer = FREAD(m.lnHand, 4)
      amEmoinfo[2] = byTestodec(m.lcBuffer,.T.)
 ENDIF
 IF NOT INLIST(amEmoinfo(2), 0, 1)
      m.lnRet = 3
 ENDIF
 IF m.tnOpenmode=2 .AND. VARTYPE(amEmoinfo(3))='N'
      lcBuffer = deCtobytes(amEmoinfo(3),4,.T.)
      IF FWRITE(m.lnHand, m.lcBuffer)=0
           lnRet = 5
      ENDIF
 ELSE
      lcBuffer = FREAD(m.lnHand, 4)
      amEmoinfo[3] = byTestodec(m.lcBuffer,.T.)
 ENDIF
 m.lnDatalength = amEmoinfo(3)
 IF m.tnOpenmode=2 .AND. VARTYPE(amEmoinfo(4))='C'
      IF FWRITE(m.lnHand, amEmoinfo(4))=0
           lnRet = 5
      ENDIF
 ELSE
      IF m.lnStart+m.lnMemoheader+m.lnDatalength>m.lnFilesize
           m.lnRet = 4
      ELSE
           amEmoinfo[4] = FREAD(m.lnHand, m.lnDatalength)
      ENDIF
 ENDIF
 IF m.llClose
      = FCLOSE(m.lnHand)
 ENDIF
 RETURN m.lnRet
ENDFUNC
*
FUNCTION BytesToDec
 LPARAMETERS tcBuffer, tlSignifisfirst
 LOCAL lcTemp, i, lcHex
 lcTemp = ""
 FOR i = 1 TO LEN(m.tcBuffer)
      lcHex = deCtohex(ASC(SUBSTR(m.tcBuffer, m.i, 1)))
      IF m.tlSignifisfirst
           lcTemp = m.lcTemp+m.lcHex
      ELSE
           lcTemp = m.lcHex+m.lcTemp
      ENDIF
 ENDFOR
 RETURN heXtodec(m.lcTemp)
ENDFUNC
*
FUNCTION DecToBytes
 LPARAMETERS tnValue, tnBytesnumb, tlSignifisfirst
 LOCAL lcBytes, lcHex, i, lcByte1
 lcHex = PADL(ALLTRIM(deCtohex(m.tnValue)), 2*m.tnBytesnumb, "0")
 lcBytes = ''
 FOR i = 1 TO m.tnBytesnumb
      lcByte1 = CHR(heXtodec(SUBSTR(m.lcHex, 2*m.i-1, 2)))
      IF m.tlSignifisfirst
           lcBytes = m.lcBytes+m.lcByte1
      ELSE
           lcBytes = m.lcByte1+m.lcBytes
      ENDIF
 ENDFOR
 RETURN m.lcBytes
ENDFUNC
*
FUNCTION DevelopVersion
 RETURN NOT "Support"$VERSION(1)
ENDFUNC
*
FUNCTION JustDrv
 PARAMETER m.paThin
 PRIVATE m.drIvelc
 m.drIvelc = ''
 m.paThin = ALLTRIM(m.paThin)
 IF LEN(m.paThin)>1
      DO CASE
           CASE SUBSTR(m.paThin, 2, 1)=':'
                m.drIvelc = LEFT(m.paThin, 2)
           CASE LEFT(m.paThin, 2)='\\'
                PRIVATE m.enDdrvpoz
                m.enDdrvpoz = AT('\', m.paThin, 4)-1
                IF m.enDdrvpoz>0
                     m.drIvelc = LEFT(m.paThin, m.enDdrvpoz)
                ELSE
                     m.enDdrvpoz = AT('\', m.paThin, 3)
                     IF m.enDdrvpoz>0 .AND. LEN(m.paThin)=m.enDdrvpoz+1
                          m.drIvelc = m.paThin
                     ENDIF
                ENDIF
      ENDCASE
 ENDIF
 RETURN UPPER(m.drIvelc)
ENDFUNC
*
FUNCTION no_slash
 PARAMETER m.diRd
 m.diRd = ALLTRIM(m.diRd)
 IF EMPTY(m.diRd)
      RETURN m.diRd
 ENDIF
 PRIVATE m.i1, m.i2
 m.i1 = IIF(LEFT(m.diRd, 1)="\", 2, 1)
 m.i2 = IIF(RIGHT(m.diRd, 1)="\", LEN(m.diRd)-1, LEN(m.diRd))
 RETURN SUBSTR(m.diRd, m.i1, m.i2-m.i1+1)
ENDFUNC
*
FUNCTION NoEndSlash
 PARAMETER m.diRd
 PRIVATE m.i2
 m.i2 = IIF(RIGHT(m.diRd, 1)="\", LEN(m.diRd)-1, LEN(m.diRd))
 RETURN SUBSTR(m.diRd, 1, m.i2)
ENDFUNC
*
FUNCTION stripDrive
 PARAMETER m.fiLename
 m.fiLename = ALLTRIM(m.fiLename)
 PRIVATE m.enDdrvpoz, m.reTname
 m.reTname = ''
 m.enDdrvpoz = LEN(juStdrv(m.fiLename))
 IF LEN(m.fiLename)>m.enDdrvpoz
      m.reTname = SUBSTR(m.fiLename, m.enDdrvpoz+1)
 ENDIF
 RETURN m.reTname
ENDFUNC
*
FUNCTION putDrive
 LPARAMETERS m.paThinp, tcDrivesource
 IF PCOUNT()<2
      tcDrivesource = m.hoMe
 ENDIF
 m.paThinp = ALLTRIM(m.paThinp)
 IF PADR(m.paThinp, 1)='\'
      RETURN juStdrv(m.tcDrivesource)+m.paThinp
 ENDIF
 RETURN m.paThinp
ENDFUNC
*
FUNCTION JustDrv
 PARAMETER m.paThin
 PRIVATE m.drIvelc
 m.drIvelc = ''
 m.paThin = ALLTRIM(m.paThin)
 IF LEN(m.paThin)>1
      DO CASE
           CASE SUBSTR(m.paThin, 2, 1)=':'
                m.drIvelc = LEFT(m.paThin, 2)
           CASE LEFT(m.paThin, 2)='\\'
                PRIVATE m.enDdrvpoz
                m.enDdrvpoz = AT('\', m.paThin, 4)-1
                IF m.enDdrvpoz>0
                     m.drIvelc = LEFT(m.paThin, m.enDdrvpoz)
                ELSE
                     m.enDdrvpoz = AT('\', m.paThin, 3)
                     IF m.enDdrvpoz>0 .AND. LEN(m.paThin)=m.enDdrvpoz+1
                          m.drIvelc = m.paThin
                     ENDIF
                ENDIF
      ENDCASE
 ENDIF
 RETURN UPPER(m.drIvelc)
ENDFUNC
*
FUNCTION dirExist
 LPARAMETERS inDir, atTribout
 PRIVATE teMparr
 IF vaLidpath1(m.inDir) .AND. ADIR(teMparr, JUSTPATH(m.inDir), 'DHS')=1
      m.atTribout = teMparr(1,5)
      RETURN 'D'$m.atTribout
 ENDIF
 RETURN .F.
ENDFUNC
*
FUNCTION ValidPath1
 LPARAMETERS tcNametotest
 LOCAL llError, lcOldonerror
 PRIVATE tmParr
 llError = 0
 lcOldonerror = onError()
 ON ERROR LLERROR=ERROR()
 = ADIR(tmParr, m.tcNametotest)
 ON ERROR &lcOldOnError
 RETURN m.llError=0
ENDFUNC
*
FUNCTION mkdirMes
 PARAMETER m.diR__in, m.meSsin
 IF PCOUNT()<2
      m.meSsin = 1
 ENDIF
 PRIVATE m.reZult
 m.reZult = mkDir(noEndslash(m.diR__in))
 IF m.meSsin=1
      DO CASE
           CASE INLIST(m.reZult, 1, 2)
                = inFo_mesg('Неуспешно създаване на каталог '+m.diR__in)
           CASE m.reZult=6
                = inFo_mesg('Каталог '+m.diR__in+' вече съществува')
      ENDCASE
 ENDIF
 RETURN m.reZult
ENDFUNC
*
FUNCTION stripExt
 PARAMETER m.fiLename
 m.fiLename = TRIM(m.fiLename)
 PRIVATE m.doTpos, m.teRminator
 m.doTpos = RAT(".", m.fiLename)
 m.teRminator = MAX(RAT("\", m.fiLename), RAT(":", m.fiLename))
 IF m.doTpos>m.teRminator
      m.fiLename = LEFT(m.fiLename, m.doTpos-1)
 ENDIF
 RETURN m.fiLename
ENDFUNC
*
FUNCTION Force_DBF
 LPARAMETERS m.fiLname, m.olNlyifnot
 IF PCOUNT()<2
      m.olNlyifnot = .F.
 ENDIF
 RETURN foRce_ext(m.fiLname,"DBF",m.olNlyifnot)
ENDFUNC
*
FUNCTION force_FPT
 LPARAMETERS m.fiLname, m.olNlyifnot
 IF PCOUNT()<2
      m.olNlyifnot = .F.
 ENDIF
 RETURN foRce_ext(m.fiLname,"FPT",m.olNlyifnot)
ENDFUNC
*
FUNCTION force_CDX
 PARAMETER m.fiLname, m.olNlyifnot
 IF PCOUNT()<2
      m.olNlyifnot = .F.
 ENDIF
 RETURN foRce_ext(m.fiLname,"CDX",m.olNlyifnot)
ENDFUNC
*
FUNCTION force_APP
 PARAMETER m.fiLname, m.olNlyifnot
 IF PCOUNT()<2
      m.olNlyifnot = .F.
 ENDIF
 RETURN foRce_ext(m.fiLname,"APP",m.olNlyifnot)
ENDFUNC
*
FUNCTION force_EXE
 PARAMETER m.fiLname, m.olNlyifnot
 IF PCOUNT()<2
      m.olNlyifnot = .F.
 ENDIF
 RETURN foRce_ext(m.fiLname,"EXE",m.olNlyifnot)
ENDFUNC
*
FUNCTION force_FRX
 PARAMETER m.fiLname, m.olNlyifnot
 IF PCOUNT()<2
      m.olNlyifnot = .F.
 ENDIF
 RETURN foRce_ext(m.fiLname,"FRX",m.olNlyifnot)
ENDFUNC
*
FUNCTION force_FRT
 PARAMETER m.fiLname, m.olNlyifnot
 IF PCOUNT()<2
      m.olNlyifnot = .F.
 ENDIF
 RETURN foRce_ext(m.fiLname,"FRT",m.olNlyifnot)
ENDFUNC
*
FUNCTION force_TXT
 PARAMETER m.fiLname, m.olNlyifnot
 IF PCOUNT()<2
      m.olNlyifnot = .F.
 ENDIF
 RETURN foRce_ext(m.fiLname,"TXT",m.olNlyifnot)
ENDFUNC
*
FUNCTION force_IDX
 PARAMETER m.fiLname, m.olNlyifnot
 IF PCOUNT()<2
      m.olNlyifnot = .F.
 ENDIF
 RETURN foRce_ext(m.fiLname,"IDX",m.olNlyifnot)
ENDFUNC
*
FUNCTION MemoExt
 LPARAMETERS lcTablename
 LOCAL lcExt, lcDbext
 lcDbext = UPPER(JUSTEXT(m.lcTablename))
 lcExt = "FPT"
 DO CASE
      CASE m.lcDbext="FRX"
           lcExt = "FRT"
      CASE m.lcDbext="LBX"
           lcExt = "LBT"
      CASE m.lcDbext="DBC"
           lcExt = "DCT"
      CASE m.lcDbext='SCX'
           lcExt = 'SCT'
      CASE m.lcDbext='VCX'
           lcExt = 'VCT'
      CASE m.lcDbext='MNX'
           lcExt = 'MNT'
      CASE m.lcDbext='PJX'
           lcExt = 'PJT'
 ENDCASE
 RETURN m.lcExt
ENDFUNC
*
FUNCTION CindexExt
 LPARAMETERS lcTablename
 LOCAL lcExt, lcDbext
 lcDbext = UPPER(JUSTEXT(m.lcTablename))
 lcExt = "CDX"
 DO CASE
      CASE INLIST(m.lcDbext, "FRX", "LBX", 'SCX', 'VCX', 'MNX', 'PJX')
           lcExt = ''
      CASE m.lcDbext="DBC"
           lcExt = 'DCX'
 ENDCASE
 RETURN m.lcExt
ENDFUNC
*
FUNCTION force_EXT
 PARAMETER m.fiLname, m.neWext, m.olNlyifnot
 IF PCOUNT()<3
      m.olNlyifnot = .F.
 ENDIF
 PRIVATE m.exTsprtr
 m.exTsprtr = '.'
 IF m.olNlyifnot .AND. m.exTsprtr$JUSTFNAME(m.fiLname)
      RETURN m.fiLname
 ENDIF
 m.fiLname = stRipext(m.fiLname)
 m.neWext = ALLTRIM(m.neWext)
 DO CASE
      CASE EMPTY(m.neWext)
           RETURN m.fiLname
      CASE LEFT(m.neWext, 1)==m.exTsprtr
           RETURN m.fiLname+m.neWext
 ENDCASE
 RETURN m.fiLname+m.exTsprtr+m.neWext
ENDFUNC
*
FUNCTION FILEDBF
 PARAMETER m.fiLein
 RETURN FILE(foRce_ext(m.fiLein,"DBF",.T.))
ENDFUNC
*
FUNCTION OnlyPath1
 PARAMETER m.paThin
 RETURN ADDBS(JUSTPATH(noEndslash(m.paThin)))
ENDFUNC
*
FUNCTION onlyDirLast
 PARAMETER m.paThorfile
 RETURN JUSTFNAME(JUSTPATH(m.paThorfile))
ENDFUNC
*
FUNCTION makeName
 PARAMETER m.ll1, m.ll2, m.ll3, m.ll4, m.ll5, m.ll6, m.ll7
 PRIVATE m.ijKl, m.stRlist, m.vaRparam, m.noMbparam
 m.noMbparam = PCOUNT()
 m.stRlist = ALLTRIM(m.ll1)
 FOR m.ijKl = 2 TO m.noMbparam
      m.vaRparam = "m.ll"+ALLTRIM(STR(m.ijKl))
      IF !EMPTY(&varparam)
           m.strlist = m.strlist + IIF(EMPTY(m.strlist),""," ") + TRIM(&varparam)
      ENDIF
 ENDFOR
 RETURN m.stRlist
ENDFUNC
*
PROCEDURE ONcyrlat
 PARAMETER m.phO_bdsin
 IF PCOUNT()<1
      m.phO_bdsin = 1
 ENDIF
 ON KEY LABEL ALT+HYPHEN DO OLDBUFFER
 _CLIPTEXT = cyRtolat(_CLIPTEXT,m.phO_bdsin)
 KEYBOARD '{Ctrl+V}{ALT+HYPHEN}' CLEAR
 RETURN
ENDPROC
*
PROCEDURE ONlatcyr
 PARAMETER m.phO_bdsin
 IF PCOUNT()<1
      m.phO_bdsin = 1
 ENDIF
 ON KEY LABEL ALT+HYPHEN DO OLDBUFFER
 _CLIPTEXT = laTtocyr(_CLIPTEXT,m.phO_bdsin)
 KEYBOARD '{Ctrl+V}{ALT+HYPHEN}' CLEAR
 RETURN
ENDPROC
*
PROCEDURE ONwindos
 ON KEY LABEL ALT+HYPHEN DO OLDBUFFER
 _CLIPTEXT = CHRTRAN(_CLIPTEXT, ocYr.dw_wintab, ocYr.dw_dostab)
 KEYBOARD '{Ctrl+V}{ALT+HYPHEN}' CLEAR
 RETURN
ENDPROC
*
PROCEDURE ONdoswin
 ON KEY LABEL ALT+HYPHEN DO OLDBUFFER
 _CLIPTEXT = CHRTRAN(_CLIPTEXT, ocYr.dw_dostab, ocYr.dw_wintab)
 KEYBOARD '{Ctrl+V}{ALT+HYPHEN}' CLEAR
 RETURN
ENDPROC
*
PROCEDURE ONrusBul
 ON KEY LABEL ALT+RBRACKET DO OLDBUFFER
 _CLIPTEXT = CHRTRAN(_CLIPTEXT, ocYr.dw_rustab, ocYr.dw_dostab)
 KEYBOARD '{Ctrl+V}{ALT+HYPHEN}' CLEAR
 RETURN
ENDPROC
*
PROCEDURE ONlower
 ON KEY LABEL ALT+HYPHEN DO OLDBUFFER
 _CLIPTEXT = loWer_c(_CLIPTEXT)
 KEYBOARD '{Ctrl+V}{ALT+HYPHEN}' CLEAR
 RETURN
ENDPROC
*
PROCEDURE ONinvers
 ON KEY LABEL ALT+HYPHEN DO OLDBUFFER
 _CLIPTEXT = inVers(_CLIPTEXT)
 KEYBOARD '{Ctrl+V}{ALT+HYPHEN}' CLEAR
 RETURN
ENDPROC
*
PROCEDURE ONupper
 ON KEY LABEL ALT+HYPHEN DO OLDBUFFER
 _CLIPTEXT = upPer_c(_CLIPTEXT)
 KEYBOARD '{Ctrl+V}{ALT+HYPHEN}' CLEAR
 RETURN
ENDPROC
*
PROCEDURE ONproper
 ON KEY LABEL ALT+HYPHEN DO OLDBUFFER
 _CLIPTEXT = x__(_CLIPTEXT)
 KEYBOARD '{Ctrl+V}{ALT+HYPHEN}' CLEAR
 RETURN
ENDPROC
*
PROCEDURE OLDbuffer
 _CLIPTEXT = _cLiptext9
 RELEASE _cLiptext9
 ON KEY LABEL ALT+HYPHEN
 RETURN
ENDPROC
*
FUNCTION CYRtoLAT
 PARAMETER tcString, m.phO_bds
 IF PCOUNT()<2
      m.phO_bds = 1
 ENDIF
 DO CASE
      CASE m.phO_bds=1
           RETURN CHRTRAN(m.tcString, ocYr.cyRlowupp, ocYr.ccLl)
      CASE m.phO_bds=2
           RETURN CHRTRAN(m.tcString, ocYr.cyRlowupp, ocYr.bdStolat_low+ ;
                  ocYr.bdStolat_upp)
      OTHERWISE
           RETURN m.tcString
 ENDCASE
ENDFUNC
*
FUNCTION LATtoCYR
 PARAMETER tcString, m.phO_bds
 IF PCOUNT()<2
      m.phO_bds = 1
 ENDIF
 DO CASE
      CASE m.phO_bds=1
           RETURN CHRTRAN(m.tcString, ocYr.ccLl, ocYr.cyRlowupp)
      CASE m.phO_bds=2
           RETURN CHRTRAN(m.tcString, ocYr.bdStolat_low+ocYr.bdStolat_upp,  ;
                  ocYr.cyRlowupp)
      OTHERWISE
           RETURN m.tcString
 ENDCASE
ENDFUNC
*
FUNCTION LOWER_C
 LPARAMETERS tcString
 IF VARTYPE(ocYr)='O'
      RETURN ocYr.loWer_c(m.tcString)
 ENDIF
 RETURN LOWER(m.tcString)
ENDFUNC
*
FUNCTION UPPER_C
 LPARAMETERS tcString
 IF VARTYPE(ocYr)='O'
      RETURN ocYr.upPer_c(m.tcString)
 ENDIF
 RETURN UPPER(m.tcString)
ENDFUNC
*
FUNCTION Invers
 PARAMETER m.tcString
 RETURN CHRTRAN(m.tcString, ocYr.laT_low+ocYr.laT_upp+ocYr.cyR_low+ ;
        ocYr.cyR_upp, ocYr.laT_upp+ocYr.laT_low+ocYr.cyR_upp+ocYr.cyR_low)
ENDFUNC
*
FUNCTION x__
 PARAMETER m.tcString
 PRIVATE m.xx, m.leN
 m.xx = m.tcString
 m.leN = LEN(m.xx)
 DO CASE
      CASE m.leN<1
           RETURN m.xx
      CASE m.leN=1
           RETURN upPer_c(m.xx)
      OTHERWISE
           RETURN upPer_c(SUBSTR(m.xx, 1, 1))+loWer_c(SUBSTR(m.xx, 2))
 ENDCASE
ENDFUNC
*
FUNCTION to_left
 PARAMETER m.stRing
 IF NOT INLIST(TYPE('m.string'), 'C', 'M')
      RETURN m.stRing
 ENDIF
 PRIVATE m.leNg
 m.leNg = LEN(m.stRing)
 RETURN PADR(LTRIM(m.stRing), m.leNg)
ENDFUNC
*
FUNCTION AS
 PARAMETER m.nuMber
 RETURN ALLTRIM(STR(m.nuMber))
ENDFUNC
*
FUNCTION intSpec
 PARAMETER m.nuMberin
 RETURN IIF(isWhole(m.nuMberin), INT(m.nuMberin), m.nuMberin)
ENDFUNC
*
FUNCTION IsWhole
 PARAMETER m.nuMbrinn
 RETURN m.nuMbrinn=INT(m.nuMbrinn)
ENDFUNC
*
FUNCTION ASspec
 PARAMETER m.nuMberin
 IF isWhole(m.nuMberin)
      RETURN as(m.nuMberin)
 ELSE
      RETURN LTRIM(trIm1(STR(m.nuMberin, 25, 15),'0'))
 ENDIF
ENDFUNC
*
FUNCTION inlist1
 PARAMETER m.inStr, m.liStstr, m.seParinn
 IF PCOUNT()<3
      m.seParinn = ','
 ENDIF
 PRIVATE m.il, m.il0, m.il1, m.nmBr
 m.nmBr = OCCURS(m.seParinn, m.liStstr)
 m.inStr = ALLTRIM(m.inStr)
 IF m.nmBr=0
      IF m.inStr==ALLTRIM(m.liStstr)
           RETURN 1
      ELSE
           RETURN 0
      ENDIF
 ENDIF
 m.il = AT(m.seParinn, m.liStstr)
 IF m.il>1 .AND. m.inStr==ALLTRIM(LEFT(m.liStstr, m.il-1))
      RETURN 1
 ENDIF
 FOR m.il = 1 TO m.nmBr-1
      m.il0 = AT(m.seParinn, m.liStstr, m.il)
      m.il1 = AT(m.seParinn, m.liStstr, m.il+1)-m.il0-1
      IF m.il1>0 .AND. m.inStr==ALLTRIM(SUBSTR(m.liStstr, m.il0+1, m.il1))
           RETURN m.il+1
      ENDIF
 ENDFOR
 m.il = RAT(m.seParinn, m.liStstr)
 IF m.il<LEN(m.liStstr) .AND. m.inStr==ALLTRIM(SUBSTR(m.liStstr, m.il+1))
      RETURN m.nmBr+1
 ENDIF
 RETURN 0
ENDFUNC
*
FUNCTION compres
 PARAMETER m.stRing, m.siMbolin
 IF PCOUNT()<2
      m.siMbolin = " "
 ENDIF
 PRIVATE m.leNsmbin, m.i, m.siMbloc
 m.leNsmbin = LEN(m.siMbolin)
 IF m.leNsmbin=0
      RETURN m.stRing
 ENDIF
 PRIVATE m.ouTput, m.siMbolin2
 m.ouTput = m.stRing
 FOR m.i = 1 TO m.leNsmbin
      m.siMbloc = SUBSTR(m.siMbolin, m.i, 1)
      m.siMbolin2 = m.siMbloc+m.siMbloc
      DO WHILE AT(m.siMbolin2, m.ouTput)>0
           m.ouTput = STRTRAN(m.ouTput, m.siMbolin2, m.siMbloc)
      ENDDO
 ENDFOR
 RETURN m.ouTput
ENDFUNC
*
FUNCTION trimList
 PARAMETER m.stRingin, m.siMbinl
 IF PCOUNT()<2
      m.siMbinl = ','
 ENDIF
 RETURN trIm1(coMpres(m.stRingin,m.siMbinl),m.siMbinl)
ENDFUNC
*
FUNCTION trim1
 PARAMETER m.stRin, m.siMbin
 IF PCOUNT()<2
      m.siMbin = ','
 ENDIF
 PRIVATE m.stRloc, m.leNloc, m.ilOc
 m.stRloc = TRIM(m.stRin)
 m.leNloc = LEN(m.stRloc)
 DO WHILE .T.
      IF m.leNloc=0
           RETURN ''
      ENDIF
      IF NOT SUBSTR(m.stRloc, m.leNloc, 1)$SPACE(1)+m.siMbin
           EXIT
      ENDIF
      m.leNloc = m.leNloc-1
 ENDDO
 RETURN LEFT(m.stRloc, m.leNloc)
ENDFUNC
*
FUNCTION Ltrim1
 PARAMETER m.stRin, m.siMbin
 IF PCOUNT()<2
      m.siMbin = ','
 ENDIF
 PRIVATE m.leNlocal, m.i
 m.leNlocal = LEN(m.stRin)
 FOR m.i = 1 TO m.leNlocal
      IF NOT SUBSTR(m.stRin, m.i, 1)$SPACE(1)+m.siMbin
           RETURN SUBSTR(m.stRin, m.i)
      ENDIF
 ENDFOR
 RETURN ''
ENDFUNC
*
FUNCTION ALLtrim1
 PARAMETER m.stRinn, m.siMbinn
 IF PCOUNT()<2
      m.siMbinn = ','
 ENDIF
 RETURN ltRim1(trIm1(m.stRinn,m.siMbinn),m.siMbinn)
ENDFUNC
*
FUNCTION trimCR
 PARAMETER m.inString
 PRIVATE m.ouTstr, m.crLoc, m.lfLoc, m.leNloc
 m.crLoc = CHR(13)
 m.lfLoc = CHR(10)
 m.ouTstr = TRIM(m.inString)
 DO WHILE .T.
      m.ouTstr = TRIM(m.ouTstr)
      m.leNloc = LEN(m.ouTstr)
      IF m.leNloc>0 .AND. INLIST(RIGHT(m.ouTstr, 1), m.crLoc, m.lfLoc)
           IF LEN(m.ouTstr)=1
                RETURN ''
           ELSE
                m.ouTstr = SUBSTR(m.ouTstr, 1, m.leNloc-1)
           ENDIF
      ELSE
           EXIT
      ENDIF
 ENDDO
 RETURN m.ouTstr
ENDFUNC
*
FUNCTION LtrimCR
 PARAMETER m.inString
 PRIVATE m.ouTstr, m.crLoc, m.lfLoc, m.leNloc
 m.crLoc = CHR(13)
 m.lfLoc = CHR(10)
 m.ouTstr = LTRIM(m.inString)
 DO WHILE .T.
      m.ouTstr = LTRIM(m.ouTstr)
      m.leNloc = LEN(m.ouTstr)
      IF m.leNloc>0 .AND. INLIST(LEFT(m.ouTstr, 1), m.crLoc, m.lfLoc)
           IF LEN(m.ouTstr)=1
                RETURN ''
           ELSE
                m.ouTstr = SUBSTR(m.ouTstr, 2)
           ENDIF
      ELSE
           EXIT
      ENDIF
 ENDDO
 RETURN m.ouTstr
ENDFUNC
*
FUNCTION ALLtrimCR
 PARAMETER m.inStr
 RETURN trImcr(ltRimcr(m.inStr))
ENDFUNC
*
FUNCTION makeList
 PARAMETER m.ll1, m.ll2, m.ll3, m.ll4, m.ll5, m.ll6, m.ll7
 LOCAL m.ijKl, m.stRlist, m.vaRparam
 m.stRlist = ALLTRIM(m.ll1)
 FOR m.ijKl = 2 TO PCOUNT()
      m.vaRparam = "m.ll"+ALLTRIM(STR(m.ijKl))
      IF !EMPTY(&varparam)
           m.strlist = m.strlist + IIF(EMPTY(m.strlist),"",", ") + TRIM(&varparam)
      ENDIF
 ENDFOR
 RETURN m.stRlist
ENDFUNC
*
FUNCTION date00
 RETURN CTOD('')
ENDFUNC
*
PROCEDURE inp_date
 RELEASE WINDOW caLendar
 KEYBOARD DTOC(_DIARYDATE) CLEAR
 RETURN
ENDPROC
*
PROCEDURE max_text
 LPARAMETERS m.tcActivecontrol
 IF PCOUNT()<1
      m.tcActivecontrol = '_screen.ActiveForm.ActiveControl.Value'
 ENDIF
 IF INLIST(TYPE(m.tcActivecontrol), 'C', 'M')
      &tcActiveControl = EditInWindow( &tcActiveControl )
 ENDIF
 RETURN
ENDPROC
*
FUNCTION EditInWindow
 LPARAMETERS lcText, lcWintitle, llReadonly, tlMaximize, tnHeight,  ;
             tnWidth, tnSpecfont, tcStyle
 IF PCOUNT()<2 .OR. EMPTY(m.lcWintitle)
      lcWintitle = "Edit in window"
 ENDIF
 IF PCOUNT()<3
      llReadonly = .F.
 ENDIF
 IF PCOUNT()<5 .OR. EMPTY(m.tnHeight)
      tnHeight = 18
 ENDIF
 IF PCOUNT()<6 .OR. EMPTY(m.tnWidth)
      tnWidth = 60
 ENDIF
 IF PCOUNT()<7
      tnSpecfont = 0
 ENDIF
 IF PCOUNT()<8
      tcStyle = "N"
 ENDIF
 LOCAL tmPcursor, orIginalfl, lcModyclaus
 m.orIginalfl = SELECT()
 m.tmPcursor = SYS(2015)
 IF crEtabl1('','tmpmemo M',0,m.tmPcursor)=0
      APPEND BLANK
      REPLACE tmPmemo WITH m.lcText
 ELSE
      RETURN m.lcText
 ENDIF
 lcModyclaus = IIF(m.llReadonly, 'NOEDIT', '')
 LOCAL wiNtxtfnt, wiNfixfnt
 wiNtxtfnt = geTwintxtfnt()
 wiNfixfnt = geTwinfixfnt()
 LOCAL lcFonf
 IF m.tnSpecfont=0
      lcFonf = m.wiNtxtfnt
 ELSE
      lcFonf = m.wiNfixfnt
 ENDIF
 DEFINE WINDOW memoedit  AT 0.000, 0.000 SIZE m.tnHeight,m.tnWidth  FONT &lcFonf STYLE m.tcStyle  TITLE (m.lcWinTitle)  SYSTEM FLOAT GROW MINIMIZE CLOSE ZOOM
 MOVE WINDOW meMoedit CENTER
 IF m.tlMaximize
      KEYBOARD '{Ctrl+F10}'
 ENDIF
 MODIFY MEMO tmpmemo WINDOW memoedit &lcModyClaus
 m.lcText = &tmpcursor..tmpmemo
 RELEASE WINDOW meMoedit
 USE IN (m.tmPcursor)
 SELECT (m.orIginalfl)
 RETURN m.lcText
ENDFUNC
*
PROCEDURE dialtel0
 PARAMETER m.teL_input, m.teL_prefix
 IF PCOUNT()<2
      m.teL_prefix = ''
 ENDIF
 IF PCOUNT()>0
      m.teLtodeal = m.teL_input
 ELSE
      IF NOT EMPTY(ALIAS()) .AND. fiEldnum("tel")>0 .AND. NOT EMPTY(teL)
           DO CASE
                CASE INLIST(TYPE("tel"), "C", "M")
                     m.teLtodeal = alLtrimcr(teL)
                CASE INLIST(TYPE("tel"), "N")
                     m.teLtodeal = ALLTRIM(STR(teL))
           ENDCASE
      ENDIF
 ENDIF
 m.teL_prefix = ALLTRIM(m.teL_prefix)
 IF EMPTY(m.paThcomm)
      DO diAltxt
 ELSE
      IF TYPE('m.comIsInit')<>'L' .OR. NOT m.coMisinit
           DO (foRce_ext('comm'+IIF(_WINDOWS, 'w', ''),csPr)) WITH .T., .F.
      ENDIF
      IF EMPTY(m.teLtodeal)
           WAIT WINDOW NOWAIT "Няма въведен телефонен номер"
      ELSE
           IF NOT ('\'$m.teLtodeal .OR. '/'$m.teLtodeal) .AND. NOT  ;
              EMPTY(m.teL_prefix)
                IF m.teL_prefix<>m.usR_telkod
                     m.teLtodeal = m.teL_prefix+'\'+m.teLtodeal
                ENDIF
           ENDIF
           IF ';'$m.teLtodeal .OR. ':'$m.teLtodeal
                PRIVATE m.poZition
                m.poZition = AT(';', m.teLtodeal)
                IF m.poZition=0
                     m.poZition = AT(':', m.teLtodeal)
                ENDIF
                IF m.poZition<>0
                     m.teLtodeal = LEFT(m.teLtodeal, m.poZition-1)
                ENDIF
           ENDIF
           DO ("dialtel") IN (foRce_ext('commw',ceSpr))
      ENDIF
 ENDIF
 RETURN
ENDPROC
*
PROCEDURE dialtxt
 WAIT WINDOW NOWAIT "Модул за комуникация с модем не е инсталиран"
 RETURN
ENDPROC
*
PROCEDURE hanguptel0
 PARAMETER m.teL_input
 IF EMPTY(m.paThcomm)
      DO diAltxt
 ELSE
      WAIT WINDOW NOWAIT "Прекъсване"
      DO ("hanguptel") IN (foRce_ext('commw',csPr))
 ENDIF
 RETURN
ENDPROC
*
FUNCTION close1
 PARAMETER m.alIasin, m.usEd_loc
 IF PCOUNT()<1
      IF USED()
           USE
      ENDIF
      RETURN NOT USED()
 ENDIF
 IF PCOUNT()<2
      m.usEd_loc = .F.
 ENDIF
 IF m.usEd_loc
      RETURN .F.
 ENDIF
 IF USED(m.alIasin)
      USE IN (m.alIasin)
      RETURN .T.
 ENDIF
 RETURN .F.
ENDFUNC
*
FUNCTION fieldNum
 PARAMETER m.fiEldname, m.wkArea_x
 IF PCOUNT()<2
      m.wkArea_x = ""
 ENDIF
 PRIVATE m.nuMb, m.fiEldtmp
 m.nuMb = 0
 IF NOT EMPTY(m.fiEldname)
      DO WHILE .T.
           m.nuMb = m.nuMb+1
           IF EMPTY(m.wkArea_x)
                m.fiEldtmp = FIELD(m.nuMb)
           ELSE
                m.fiEldtmp = FIELD(m.nuMb, m.wkArea_x)
           ENDIF
           IF EMPTY(m.fiEldtmp)
                m.nuMb = 0
                EXIT
           ENDIF
           IF m.fiEldtmp==UPPER(ALLTRIM(m.fiEldname))
                EXIT
           ENDIF
      ENDDO
 ENDIF
 RETURN m.nuMb
ENDFUNC
*
FUNCTION copyDBF
 PARAMETER m.soUrcedbf, m.duBledbf, m.usEttsloc, m.foRcefpt, m.foRcecdx
 IF PCOUNT()<3 .OR. TYPE('m.useTTSloc')<>'L'
      m.usEttsloc = TYPE('m.useTTS')='L' .AND. m.usEtts
 ENDIF
 IF PCOUNT()<4
      m.foRcefpt = .F.
 ENDIF
 IF PCOUNT()<5
      m.foRcecdx = .F.
 ENDIF
 PRIVATE m.reTlog0, m.reTfpt, m.reTcdx
 m.reTlog0 = coPyfile(foRce_dbf(m.soUrcedbf,.T.),foRce_dbf(m.duBledbf, ;
             .T.),m.usEttsloc)
 m.reTfpt = coPyfile(foRce_fpt(m.soUrcedbf),foRce_fpt(m.duBledbf),m.usEttsloc)
 m.reTcdx = coPyfile(foRce_cdx(m.soUrcedbf),foRce_cdx(m.duBledbf),m.usEttsloc)
 IF m.foRcefpt
      m.reTlog0 = m.reTlog0 .AND. m.reTfpt
 ENDIF
 IF m.foRcecdx
      m.reTlog0 = m.reTlog0 .AND. m.reTcdx
 ENDIF
 RETURN m.reTlog0
ENDFUNC
*
FUNCTION copyFile
 PARAMETER m.fiLe1in, m.fiLe2in, m.usEttsin, m.meSs_l
 IF PCOUNT()<3
      m.usEttsin = .F.
 ENDIF
 IF PCOUNT()<4
      m.meSs_l = .F.
 ENDIF
 IF FILE(m.fiLe1in)
      IF NOT deLfile(m.fiLe2in,m.usEttsin)
           RETURN .F.
      ENDIF
      IF m.meSs_l
           WAIT CLEAR
           WAIT WINDOW NOWAIT 'Копира файл '+m.fiLe1in+' във '+m.fiLe2in
      ENDIF
      COPY FILE (m.fiLe1in) TO (m.fiLe2in)
      IF m.meSs_l
           WAIT CLEAR
      ENDIF
      RETURN FILE(m.fiLe2in)
 ELSE
      RETURN .F.
 ENDIF
ENDFUNC
*
FUNCTION delFile
 PARAMETER m.tmPfilein, m.usEttsin, m.erRor_l
 IF PCOUNT()<2 .OR. TYPE('m.useTTSIN')<>'L'
      m.usEttsin = .F.
 ENDIF
 m.erRor_l = 0
 IF FILE(m.tmPfilein)
      PRIVATE m.olDonerr
      m.olDonerr = onError()
      ON ERROR ERROR_L=ERROR()
      DELETE FILE (m.tmPfilein)
      ON ERROR &oldOnERR
      IF FILE(m.tmPfilein)
           IF m.erRor_l=0
                m.erRor_l = -108
           ENDIF
           RETURN .F.
      ENDIF
 ENDIF
 RETURN .T.
ENDFUNC
*
FUNCTION deleteDbf
 PARAMETER m.dbFpath, m.usEttsloc
 IF PCOUNT()<2
      m.usEttsloc = TYPE('m.useTTS')='L' .AND. m.usEtts
 ENDIF
 PRIVATE m.dbFpathx, m.deL_l1, m.deL_l2, m.deL_l3
 m.dbFpathx = stRipext(m.dbFpath)
 m.deL_l1 = deLfile(foRce_dbf(m.dbFpathx),m.usEttsloc)
 m.deL_l2 = deLfile(foRce_fpt(m.dbFpathx),m.usEttsloc)
 m.deL_l3 = deLfile(foRce_cdx(m.dbFpathx),m.usEttsloc)
 RETURN m.deL_l1 .AND. m.deL_l2 .AND. m.deL_l3
ENDFUNC
*
FUNCTION renameDbf
 PARAMETER m.dbFpath1, m.dbFpath2, m.usEttsloc
 IF PCOUNT()<3
      m.usEttsloc = TYPE('m.useTTS')='L' .AND. m.usEtts
 ENDIF
 PRIVATE m.dbFpathx1, m.dbFpathx2, m.reN_l1, m.reN_l2, m.reN_l3
 m.dbFpathx1 = stRipext(m.dbFpath1)
 m.dbFpathx2 = stRipext(m.dbFpath2)
 STORE .T. TO m.reN_l2, m.reN_l3
 m.reN_l1 = reNfile(foRce_dbf(m.dbFpathx1),foRce_dbf(m.dbFpathx2),m.usEttsloc)
 IF FILE(foRce_fpt(m.dbFpathx1))
      m.reN_l2 = reNfile(foRce_fpt(m.dbFpathx1),foRce_fpt(m.dbFpathx2), ;
                 m.usEttsloc)
 ENDIF
 IF FILE(foRce_cdx(m.dbFpathx1))
      m.reN_l3 = reNfile(foRce_cdx(m.dbFpathx1),foRce_cdx(m.dbFpathx2), ;
                 m.usEttsloc)
 ENDIF
 RETURN m.reN_l1 .AND. m.reN_l2 .AND. m.reN_l3
ENDFUNC
*
FUNCTION RenFile
 PARAMETER m.tmPfile1, m.tmPfile2, m.usEtts_n
 IF PCOUNT()<3
      m.usEtts_n = .F.
 ENDIF
 IF FILE(m.tmPfile1)
      IF FILE(m.tmPfile2) .AND. NOT deLfile(m.tmPfile2,m.usEtts_n)
           RETURN .F.
      ENDIF
      RENAME &tmpFile1 TO &tmpFile2
      RETURN FILE(m.tmPfile2) .AND. NOT FILE(m.tmPfile1)
 ELSE
      RETURN .F.
 ENDIF
ENDFUNC
*
FUNCTION locateDB
 PARAMETER m.dbFin, m.omOdin, m.alIasin, m.clAusin, m.usEttsloc,  ;
           m.wkAreain, m.seCondsin
 IF PCOUNT()<2
      m.omOdin = 0
 ENDIF
 IF PCOUNT()<3
      m.alIasin = ''
 ENDIF
 IF PCOUNT()<4
      m.clAusin = ''
 ENDIF
 IF PCOUNT()<5
      m.usEttsloc = IIF(TYPE('m.useTTS')='L', m.usEtts, .F.)
 ENDIF
 IF PCOUNT()<6
      m.wkAreain = 0
 ENDIF
 IF PCOUNT()<7
      m.seCondsin = 5
 ENDIF
 m.clAusin = UPPER(m.clAusin)
 m.erRordbf = UPPER(IIF("."$m.dbFin, m.dbFin, stRipext(m.dbFin)+".DBF"))
 IF EMPTY(m.alIasin)
      m.alIasin = JUSTSTEM(m.erRordbf)
 ENDIF
 PRIVATE m.loPenmod, m.tmPstr, m.tiTlfunc
 m.tiTlfunc = PROGRAM()+' - ПРОБЛЕМ ПРИ ОТВАРЯНЕ НА ТАБЛИЦА'
 m.tmPstr = 'EXCLUSIVE'
 IF TYPE('m.oModIN')='L'
      m.loPenmod = m.omOdin
 ELSE
      m.loPenmod = (SET(m.tmPstr)='ON')
 ENDIF
 IF LEFT(m.tmPstr, 4)$m.clAusin
      m.loPenmod = .T.
      m.clAusin = STRTRAN(m.clAusin, m.tmPstr, '')
      m.clAusin = STRTRAN(m.clAusin, LEFT(m.tmPstr, 4), '')
 ENDIF
 m.tmPstr = 'SHARED'
 IF LEFT(m.tmPstr, 4)$m.clAusin
      m.loPenmod = .F.
      m.clAusin = STRTRAN(m.clAusin, m.tmPstr, '')
      m.clAusin = STRTRAN(m.clAusin, LEFT(m.tmPstr, 4), '')
 ENDIF
 IF USED(m.alIasin)
      SELECT (m.alIasin)
      IF UPPER(JUSTSTEM(DBF()))==UPPER(JUSTSTEM(m.dbFin))
           IF (m.loPenmod .AND. ISEXCLUSIVE()) .OR. (NOT m.loPenmod .AND.  ;
              NOT ISEXCLUSIVE())
                RETURN .T.
           ELSE
                IF m.wkAreain=0
                     m.wkAreain = SELECT()
                ELSE
                     IF m.wkAreain=SELECT()
                     ELSE
                          = woRn_mesg('Зададена е работна област '+ ;
                            as(m.wkAreain)+CHR(13)+'но '+m.alIasin+ ;
                            ' е отворена'+CHR(13)+ ;
                            'по друг начин в област '+SELECT()+'.',m.tiTlfunc)
                          SELECT (m.wkAreain)
                          RETURN .F.
                     ENDIF
                ENDIF
           ENDIF
      ELSE
           USE
      ENDIF
 ENDIF
 SELECT (m.wkAreain)
 DO WHILE .T.
      IF neTuse(m.erRordbf,m.loPenmod,m.seCondsin,m.alIasin,m.clAusin,.T., ;
         m.usEttsloc) .AND. USED()
           EXIT
      ELSE
           IF NOT yeS("Неуспешен опит за отваряне на база",m.erRordbf, ;
              "Да се направи ли нов опит?",1,m.tiTlfunc)
                RETURN .F.
           ENDIF
      ENDIF
 ENDDO
 RETURN .T.
ENDFUNC
*
FUNCTION ExclOpenMG
 PARAMETER m.tyPemesg, m.al_nm0
 IF PCOUNT()<2
      m.al_nm0 = ALIAS()
 ENDIF
 PRIVATE m.reTvall
 m.reTvall = ISEXCLUSIVE(m.al_nm0)
 IF NOT m.reTvall
      = woRn_mesg('Базата е отворена за ПОДЕЛЕНО ползване.'+CHR(13)+ ;
        IIF(m.tyPemesg=1, 'Пакет', 'Индекс')+'иране НЕ е извършено.')
 ENDIF
 RETURN m.reTvall
ENDFUNC
*
FUNCTION OthrLocked
 PRIVATE m.otHrlocked
 IF EOF() .OR. ISRLOCKED()
      m.otHrlocked = .F.
 ELSE
      PRIVATE m.olDrepro, m.erRor_l, m.olDonerr
      m.olDrepro = seTreproc()
      m.erRor_l = 0
      m.olDonerr = onError()
      ON ERROR ERROR_L=ERROR()
      SET REPROCESS TO 1
      IF DELETED()
           DELETE
      ELSE
           RECALL
      ENDIF
      m.otHrlocked = m.erRor_l<>0
      SET REPROCESS TO (m.olDrepro)
      ON ERROR &oldOnERR
 ENDIF
 RETURN m.otHrlocked
ENDFUNC
*
FUNCTION unLockIn
 PARAMETER m.alIasin
 IF USED(m.alIasin)
      UNLOCK IN (m.alIasin)
 ENDIF
 RETURN .T.
ENDFUNC
*
FUNCTION RecLock
 PARAMETER m.nsEconds, m.c_Mes
 IF PCOUNT()<1
      m.nsEconds = 5
 ENDIF
 IF PCOUNT()<2
      m.c_Mes = 'Записът е зает, моля изчакайте...'
 ENDIF
 PRIVATE m.olDrepro
 m.olDrepro = seTreproc()
 SET REPROCESS TO 5
 IF RLOCK()
      SET REPROCESS TO (m.olDrepro)
      RETURN .T.
 ENDIF
 PRIVATE m.lfOrever, m.olDcurs, m.reTvalue
 m.reTvalue = .F.
 m.olDcurs = seTcursor()
 m.lfOrever = (m.nsEconds=0)
 IF NOT EMPTY(m.c_Mes)
      WAIT CLEAR
      WAIT WINDOW NOWAIT m.c_Mes
 ENDIF
 DO WHILE (m.lfOrever .OR. m.nsEconds>0)
      IF RLOCK()
           m.reTvalue = .T.
           EXIT
      ENDIF
      SET CURSOR OFF
      = INKEY(0.5)
      m.nsEconds = m.nsEconds-0.5
 ENDDO
 IF NOT EMPTY(m.c_Mes)
      WAIT CLEAR
 ENDIF
 SET CURSOR &oldCurs
 SET REPROCESS TO (m.olDrepro)
 RETURN m.reTvalue
ENDFUNC
*
FUNCTION RecLockMes
 PARAMETER m.nsEconds0
 IF PCOUNT()<1
      m.nsEconds0 = 5
 ENDIF
 PRIVATE m.r_Val
 m.r_Val = reClock(m.nsEconds0)
 IF NOT m.r_Val
      DO peRmitmes WITH m.tiTlloc
 ENDIF
 RETURN m.r_Val
ENDFUNC
*
FUNCTION FilLock
 PARAMETER m.nsEconds, m.c_Mes
 IF PCOUNT()<1
      m.nsEconds = 5
 ENDIF
 IF PCOUNT()<2
      m.c_Mes = 'Файлът е зает, моля изчакайте...'
 ENDIF
 PRIVATE m.olDrepro
 m.olDrepro = seTreproc()
 SET REPROCESS TO 5
 IF FLOCK()
      SET REPROCESS TO (m.olDrepro)
      RETURN .T.
 ENDIF
 PRIVATE m.lfOrever, m.olDcurs, m.reTvalue
 m.reTvalue = .F.
 m.olDcurs = seTcursor()
 m.lfOrever = (m.nsEconds=0)
 IF NOT EMPTY(m.c_Mes)
      WAIT CLEAR
      WAIT WINDOW NOWAIT m.c_Mes
 ENDIF
 DO WHILE (m.lfOrever .OR. m.nsEconds>0)
      SET CURSOR OFF
      = INKEY(0.5)
      m.nsEconds = m.nsEconds-0.5
      IF FLOCK()
           m.reTvalue = .T.
           EXIT
      ENDIF
 ENDDO
 IF NOT EMPTY(m.c_Mes)
      WAIT CLEAR
 ENDIF
 SET CURSOR &oldCurs
 SET REPROCESS TO (m.olDrepro)
 RETURN m.reTvalue
ENDFUNC
*
FUNCTION AddRec
 PARAMETER m.nsEconds, m.c_Mes
 IF PCOUNT()<1
      m.nsEconds = 5
 ENDIF
 IF PCOUNT()<2
      m.c_Mes = 'Файлът е зает, моля изчакайте...'
 ENDIF
 PRIVATE m.lfOrever, m.olDonerror, m.erRor_l, m.olDcurs, m.reTvalue
 m.olDcurs = seTcursor()
 m.olDonerror = onError()
 m.erRor_l = 0
 ON ERROR ERROR_L=ERROR()
 APPEND BLANK
 IF m.erRor_l=0
      ON ERROR &oldONError
      RETURN .T.
 ENDIF
 m.reTvalue = .F.
 m.lfOrever = (m.nsEconds=0)
 IF NOT EMPTY(m.c_Mes)
      WAIT CLEAR
      WAIT WINDOW NOWAIT m.c_Mes
 ENDIF
 DO WHILE (m.lfOrever .OR. m.nsEconds>0)
      m.erRor_l = 0
      APPEND BLANK
      IF m.erRor_l=0
           m.reTvalue = .T.
           EXIT
      ENDIF
      SET CURSOR OFF
      = INKEY(0.5)
      m.nsEconds = m.nsEconds-0.5
 ENDDO
 IF NOT EMPTY(m.c_Mes)
      WAIT CLEAR
 ENDIF
 ON ERROR &oldONError
 SET CURSOR &oldCurs
 RETURN m.reTvalue
ENDFUNC
*
FUNCTION NetUse
 PARAMETER m.cdAtabase, m.loPenmode, m.nsEconds, m.alIasin, m.clAusin,  ;
           m.meSage_l, m.usEttsin, m.erRor_l, m.moDe1707
 IF PCOUNT()<2
      m.loPenmode = .T.
 ENDIF
 IF PCOUNT()<3 .OR. m.nsEconds<0
      m.nsEconds = 3
 ENDIF
 IF PCOUNT()<4
      m.alIasin = ''
 ENDIF
 IF PCOUNT()<5
      m.clAusin = ''
 ENDIF
 IF PCOUNT()<6
      m.meSage_l = .T.
 ENDIF
 IF PCOUNT()<7
      m.usEttsin = .F.
 ENDIF
 IF PCOUNT()<9
      m.moDe1707 = 0
 ENDIF
 PRIVATE m.dbFloc, m.tiTlloc, m.clAusloc
 m.clAusloc = ''
 m.tiTlloc = PROGRAM()+' - '+'Open table problem'
 m.dbFloc = foRce_dbf(m.cdAtabase,.T.)
 IF FILE(m.dbFloc)
      EXTERNAL ARRAY arR_finf
      IF TYPE('arr_Finf')='U'
           PRIVATE arR_finf
      ENDIF
      IF ADIR(arR_finf, m.dbFloc)=1
           PRIVATE m.noUpdword
           m.noUpdword = 'NOUP'
           IF PADR(arR_finf(1,5), 1)='R' .AND. NOT m.noUpdword$UPPER(m.clAusin)
                m.clAusloc = m.noUpdword
           ENDIF
      ENDIF
 ELSE
      IF m.meSage_l
           = woRn_mesg("File does not exist: "+CHR(13)+m.dbFloc,m.tiTlloc)
      ENDIF
      RETURN .F.
 ENDIF
 PRIVATE m.lfOrever, m.olDcurs, m.olDonerror
 m.olDcurs = seTcursor()
 m.lfOrever = (m.nsEconds=0)
 m.olDonerror = onError()
 m.clAusloc = m.clAusloc+' '+IIF(m.loPenmode, 'EXCLUSIVE', 'SHARED')
 IF NOT EMPTY(m.alIasin)
      m.alIasin = UPPER(m.alIasin)
      IF USED(m.alIasin) .AND. ALIAS()<>m.alIasin
           IF m.meSage_l
                = woRn_mesg('Опит да се отвори '+m.dbFloc+CHR(13)+ ;
                  'под име '+m.alIasin+', което вече се използва'+CHR(13)+ ;
                  'в друга работна област.',m.tiTlloc)
           ENDIF
           RETURN .F.
      ENDIF
      m.clAusloc = LTRIM(m.clAusloc)+' ALIAS '+m.alIasin
 ENDIF
 m.clAusloc = TRIM(m.clAusloc+' '+m.clAusin)
 PRIVATE m.inDmesag
 m.inDmesag = 'Препоръка: преиндексирайте.'
 ON ERROR ERROR_L=ERROR()
 m.erRor_l = 0
 DO WHILE (m.lfOrever .OR. m.nsEconds>0)
      IF NOT INLIST(m.erRor_l, 41)
           m.erRor_l = 0
      ENDIF
      USE (m.dbfLoc) AGAIN &clausLoc  
      DO CASE
           CASE INLIST(m.erRor_l, 0, 94) .AND. USED()
                ON ERROR &oldONerror
                RETURN .T.
           CASE INLIST(m.erRor_l, 1707)
                PRIVATE m.tmP_lims
                m.tmP_lims = 'Липсва индекс-файла на база '
                DO CASE
                     CASE m.moDe1707=0
                          WAIT CLEAR
                          = woRn_mesg(m.tmP_lims+CHR(13)+m.dbFloc+CHR(13)+ ;
                            m.inDmesag,m.tiTlloc)
                          ON ERROR &oldONerror
                          RETURN .F.
                     CASE m.moDe1707=1
                          m.nsEconds = m.nsEconds-1
                          LOOP
                     CASE m.moDe1707=2
                          WAIT CLEAR
                          = woRn_mesg(m.tmP_lims+CHR(13)+m.dbFloc,m.tiTlloc)
                          m.nsEconds = m.nsEconds-1
                          LOOP
                ENDCASE
           CASE INLIST(m.erRor_l, 41)
                IF FILE(stRipext(m.dbFloc)+'.FPT') .OR.  ;
                   FILE(stRipext(m.dbFloc)+'.DBT')
                     ON ERROR
                ELSE
                     WAIT CLEAR
                     = woRn_mesg('Липсва MEMO файла на '+CHR(13)+m.dbFloc, ;
                       m.tiTlloc)
                     ON ERROR &oldONerror
                     RETURN .F.
                ENDIF
           CASE INLIST(m.erRor_l, 19, 114)
                WAIT CLEAR
                = woRn_mesg('Повреда в индекса на база '+CHR(13)+m.dbFloc+ ;
                  CHR(13)+m.inDmesag,m.tiTlloc)
                ON ERROR &oldONerror
                RETURN .F.
           CASE INLIST(m.erRor_l, 1113)
                WAIT CLEAR
                = woRn_mesg('Не е отворен необходим помощен файл към '+ ;
                  m.dbFloc+CHR(13)+ ;
                  '(Има индекс с обръщение към подчинена база)',m.tiTlloc)
                ON ERROR &oldONerror
                RETURN .F.
           CASE INLIST(m.erRor_l, 15)
                WAIT CLEAR
                IF yeS("Повреда в структурата на база",m.dbFloc, ;
                   "Да започне ли поправка?",0,m.tiTlloc)
                     DO (m.hoMe+m.chEckexe) WITH (m.dbFloc)
                ENDIF
                ON ERROR &oldONerror
                RETURN .F.
      ENDCASE
      SET CURSOR OFF
      IF m.meSage_l
           WAIT WINDOW NOWAIT 'Опит да се отвори '+m.dbFloc
      ENDIF
      = INKEY(1)
      SET CURSOR &oldCurs
      m.nsEconds = m.nsEconds-1
 ENDDO
 DO CASE
      CASE INLIST(m.erRor_l, 41)
           WAIT CLEAR
           IF yeS("Повреда в MEMO файла на база",m.dbFloc, ;
              "Да започне ли поправка?",0,m.tiTlloc)
                DO (m.hoMe+m.chEckexe) WITH (m.dbFloc)
           ENDIF
      OTHERWISE
           IF m.meSage_l
                DO peRmitmes WITH m.tiTlloc, m.erRor_l, m.dbFloc
           ENDIF
 ENDCASE
 ON ERROR &oldONerror
 RETURN .F.
ENDFUNC
*
PROCEDURE permitMes
 PARAMETER m.tiTlein, m.erRnumb, m.dfFi
 IF PCOUNT()<2
      m.erRnumb = 0
 ENDIF
 IF PCOUNT()<3
      m.dfFi = ''
 ENDIF
 WAIT CLEAR
 PRIVATE m.stRinloc
 m.stRinloc = ''
 IF NOT EMPTY(m.erRnumb)
      m.stRinloc = m.stRinloc+as(m.erRnumb)+' '
 ENDIF
 IF NOT EMPTY(m.dfFi)
      m.stRinloc = m.stRinloc+dfFi
 ENDIF
 = woRn_mesg('В момента друг потребител'+CHR(13)+ ;
   'е забранил работата със системата'+IIF(EMPTY(m.stRinloc), '', CHR(13)+ ;
   '('+m.stRinloc+')'),m.tiTlein)
 RETURN
ENDPROC
*
FUNCTION ANDstr
 PARAMETER m.in_filter
 RETURN IIF(EMPTY(m.in_filter), "", " AND ")
ENDFUNC
*
FUNCTION FORstr
 PARAMETER m.in_filter
 RETURN IIF(EMPTY(m.in_filter), "", "FOR ")
ENDFUNC
*
FUNCTION ORstr
 PARAMETER m.in_filter
 RETURN IIF(EMPTY(m.in_filter), "", " OR ")
ENDFUNC
*
FUNCTION ANDstrFlt
 PARAMETER m.in_fltr
 RETURN anDstr(m.in_fltr)+m.in_fltr
ENDFUNC
*
FUNCTION setDatases
 RETURN SET('DATASESSION')
ENDFUNC
*
FUNCTION setFilter
 RETURN SET('FILTER')
ENDFUNC
*
FUNCTION setOrder
 RETURN SET('ORDER')
ENDFUNC
*
FUNCTION setCursor
 RETURN SET('CURSOR')
ENDFUNC
*
FUNCTION setMulti
 RETURN SET('MULTILOCKS')
ENDFUNC
*
FUNCTION setEscape
 RETURN SET('ESCAPE')
ENDFUNC
*
FUNCTION OnEscape
 RETURN ON('ESCAPE')
ENDFUNC
*
FUNCTION setNear
 RETURN SET('NEAR')
ENDFUNC
*
FUNCTION setSafety
 RETURN SET('SAFETY')
ENDFUNC
*
FUNCTION setExact
 RETURN SET('EXACT')
ENDFUNC
*
FUNCTION setDeleted
 RETURN SET('DELETED')
ENDFUNC
*
FUNCTION setConsole
 RETURN SET('CONSOLE')
ENDFUNC
*
FUNCTION setExclus
 RETURN SET('EXCLUSIVE')
ENDFUNC
*
FUNCTION setMessage
 RETURN SET('MESSAGE')
ENDFUNC
*
FUNCTION setConf
 RETURN SET('CONFIRM')
ENDFUNC
*
FUNCTION setShadows
 RETURN SET('SHADOWS')
ENDFUNC
*
FUNCTION setReproc
 RETURN SET('REPROCESS')
ENDFUNC
*
FUNCTION setBlock
 RETURN SET('BLOCKSIZE')
ENDFUNC
*
FUNCTION setLibrary
 RETURN SET('LIBRARY')
ENDFUNC
*
FUNCTION onError
 RETURN ON('ERROR')
ENDFUNC
*
FUNCTION onKey
 PARAMETER m.laBel
 RETURN ON('KEY', m.laBel)
ENDFUNC
*
FUNCTION Yes
 PARAMETER m.roW1in, m.roW2in, m.roW3in, m.yn, m.tiTlin, m.hlPtopic
 IF PCOUNT()<1
      m.roW1in = ''
 ENDIF
 IF PCOUNT()<2
      m.roW2in = ''
 ENDIF
 IF PCOUNT()<3
      m.roW3in = ''
 ENDIF
 IF PCOUNT()<4
      m.yn = 1
 ENDIF
 IF PCOUNT()<5
      m.tiTlin = ''
 ENDIF
 IF PCOUNT()<6
      RETURN INLIST(yeSno(trImcr(anYtoc(m.roW1in)+CHR(13)+ ;
             anYtoc(m.roW2in)+CHR(13)+anYtoc(m.roW3in)),m.tiTlin,IIF(m.yn= ;
             1, 0, 256)), 1, 6)
 ENDIF
 DO CASE
      CASE EMPTY(m.roW1in+m.roW2in+m.roW3in)
           m.roW2in = 'Сигурни ли сте?'
      CASE EMPTY(m.roW2in+m.roW3in)
           m.roW2in = m.roW1in
           m.roW1in = ''
 ENDCASE
 WAIT CLEAR
 DO FORM ('yes_no.SCX') TO m.yn WITH m.roW1in, m.roW2in, m.roW3in, (m.yn),  ;
    m.tiTlin, m.hlPtopic
 RETURN (m.yn=1)
ENDFUNC
*
FUNCTION yesNo
 PARAMETER tcMessage, tcTitlebox, ndEfaultbutton, ndIalogboxtype, niCon
 IF PCOUNT()<2 .OR. EMPTY(m.tcTitlebox)
      tcTitlebox = _SCREEN.caPtion
 ENDIF
 IF PCOUNT()<3
      ndEfaultbutton = 0
 ENDIF
 IF PCOUNT()<4
      ndIalogboxtype = 4
 ENDIF
 IF PCOUNT()<5
      niCon = 32
 ENDIF
 RETURN MESSAGEBOX(tcMessage, ndIalogboxtype+ndEfaultbutton+niCon, tcTitlebox)
ENDFUNC
*
FUNCTION MessageFatal
 PARAMETER tcMessagein, tnErrornum
 LOCAL lcMesstodisplay
 lcMesstodisplay = tcMessagein
 IF EMPTY(tcMessagein) .AND. PCOUNT()>1
      lcMesstodisplay = as(tnErrornum)+': '+MESSAGE()+CHR(13)+MESSAGE(1)
 ENDIF
 RETURN MESSAGEBOX(m.lcMesstodisplay+CHR(13)+ ;
        "Please contact system administrator"+" to update the program.",  ;
        016, "An error has occurred")
ENDFUNC
*
FUNCTION info_mesg
 PARAMETER m.meSsagein, m.tiTlein, ndIalogboxtype
 IF PCOUNT()<2 .OR. EMPTY(m.tiTlein)
      m.tiTlein = UPPER("information")
 ENDIF
 IF PCOUNT()<3
      ndIalogboxtype = 064
 ENDIF
 RETURN MESSAGEBOX(m.meSsagein, ndIalogboxtype, m.tiTlein)
ENDFUNC
*
FUNCTION noRights_mesg
 LPARAMETERS lnTypmess, tcMessage, tcTitle, tnDialogboxtype
 IF PCOUNT()<3 .OR. EMPTY(m.tcTitle)
      tcTitle = UPPER("information")
 ENDIF
 IF PCOUNT()<4
      tnDialogboxtype = 064
 ENDIF
 LOCAL lcMessage
 lcMessage = " You have no rights to "
 DO CASE
      CASE m.lnTypmess=1
           lcMessage = m.lcMessage+"change this value."
      CASE m.lnTypmess=2
           lcMessage = m.lcMessage+"use this function of the program."
      CASE m.lnTypmess=3
           lcMessage = m.lcMessage+"enter empty value."
 ENDCASE
 IF NOT EMPTY(m.tcMessage)
      lcMessage = m.lcMessage+CHR(13)+m.tcMessage
 ENDIF
 lcMessage = m.lcMessage+CHR(13)+"Please contact system administrator"+ ;
             " for more information."
 RETURN MESSAGEBOX(m.lcMessage, tnDialogboxtype, m.tcTitle)
ENDFUNC
*
FUNCTION worn_mesg
 PARAMETER m.meSsagein, m.tiTlein, ndIalogboxtype
 IF PCOUNT()<2 .OR. EMPTY(m.tiTlein)
      m.tiTlein = 'ПРЕДУПРЕЖДЕНИЕ'
 ENDIF
 IF PCOUNT()<3
      ndIalogboxtype = 048
 ENDIF
 RETURN MESSAGEBOX(m.meSsagein, ndIalogboxtype, m.tiTlein)
ENDFUNC
*
FUNCTION EscapeProc
 PARAMETER m.inMessag, m.chRlist
 IF PCOUNT()<2
      m.chRlist = CHR(27)
 ENDIF
 IF CHRSAW()
      m.keYnumb = INKEY()
      CLEAR TYPEAHEAD
      IF CHR(m.keYnumb)$m.chRlist .AND. INLIST(MESSAGEBOX("Procedure "+ ;
         m.inMessag+CHR(13)+"will be canceled"+CHR(13)+"Do you confirm?",  ;
         0292), 1, 6)
           RETURN .T.
      ENDIF
 ENDIF
 RETURN .F.
ENDFUNC
*
PROCEDURE pushKeyClear
 PUSH KEY CLEAR
 DO dfLtdebug
 RETURN
ENDPROC
*
PROCEDURE dfltDebug
 ON KEY LABEL Alt+F9 ACTIVATE WINDOW TRACE
 ON KEY LABEL Alt+F11 ACTIVATE WINDOW DEBUG
 ON KEY LABEL Alt+F12 ACTIVATE WINDOW VIEW
 RETURN
ENDPROC
*
PROCEDURE closDataE
 PARAMETER m.a1, m.a2, m.a3, m.a4, m.a5, m.a6, m.a7
 PRIVATE m.i, m.p, m.tmPstr
 m.p = PCOUNT()
 m.tmPstr = ''
 PRIVATE m.tmPwn, m.wkMax
 FOR m.i = 1 TO m.p
      m.tmPwn = 'm.a'+ALLTRIM(STR(m.i))
      IF !EMPTY( &tmpWn )
           m.tmpStr = makeList(m.tmpStr, ALLTRIM(STR( &tmpWn )))
      ENDIF
 ENDFOR
 m.wkMax = SELECT(1)
 IF EMPTY(m.tmPstr)
      FOR m.i = 1 TO m.wkMax
           IF USED(m.i)
                USE IN m.i
           ENDIF
      ENDFOR
 ELSE
      FOR m.i = 1 TO m.wkMax
           IF USED(m.i)
                IF !INLIST(m.i, &tmpStr )
                     USE IN m.i
                ENDIF
           ENDIF
      ENDFOR
 ENDIF
 RETURN
ENDPROC
*
FUNCTION recNo1
 LPARAMETERS m.alIasin
 IF PCOUNT()<1
      m.alIasin = ALIAS()
 ENDIF
 IF EMPTY(m.alIasin)
      RETURN -3
 ENDIF
 IF EOF(m.alIasin)
      RETURN -2
 ENDIF
 IF BOF(m.alIasin)
      RETURN -1
 ENDIF
 RETURN RECNO(m.alIasin)
ENDFUNC
*
FUNCTION GO1
 PARAMETER m.reCtogo
 DO CASE
      CASE m.reCtogo>RECCOUNT()
           GOTO BOTTOM
           RETURN .F.
      CASE m.reCtogo<1
           GOTO TOP
           RETURN .F.
      OTHERWISE
           GOTO m.reCtogo
           RETURN (RECNO()=m.reCtogo)
 ENDCASE
ENDFUNC
*
FUNCTION NullInit
 PARAMETER m.vaRin
 PRIVATE m.tyPeloc
 m.tyPeloc = TYPE('m.varIn')
 DO CASE
      CASE INLIST(m.tyPeloc, 'N', 'F', 'Y')
           RETURN 0
      CASE INLIST(m.tyPeloc, 'D', 'T')
           RETURN daTe00()
      CASE m.tyPeloc=='L'
           RETURN .F.
 ENDCASE
 RETURN ''
ENDFUNC
*
FUNCTION CtoAny
 LPARAMETERS tcString, tnTyperet
 tnTyperet = UPPER(m.tnTyperet)
 DO CASE
      CASE INLIST(m.tnTyperet, 'C', 'M')
           RETURN m.tcString
      CASE INLIST(m.tnTyperet, 'D')
           RETURN CTOD(m.tcString)
      CASE INLIST(m.tnTyperet, 'T')
           RETURN CTOT(m.tcString)
      CASE INLIST(m.tnTyperet, 'L')
           tcString = LOWER(m.tcString)
           IF 'д'$m.tcString .OR. 't'$m.tcString .OR. 'y'$m.tcString
                RETURN .T.
           ELSE
                RETURN .F.
           ENDIF
      CASE INLIST(m.tnTyperet, 'N', 'Y', 'I', 'F')
           IF EMPTY(m.tcString)
                RETURN 0
           ELSE
                RETURN &tcString
           ENDIF
      OTHERWISE
           RETURN &tcString
 ENDCASE
ENDFUNC
*
FUNCTION DataTypeToStr
 LPARAMETERS tnTyperet
 tnTyperet = UPPER(m.tnTyperet)
 DO CASE
      CASE INLIST(m.tnTyperet, 'C', 'M')
           RETURN "text"
      CASE INLIST(m.tnTyperet, 'D')
           RETURN "date"
      CASE INLIST(m.tnTyperet, 'T')
           RETURN "date"+'/'+"time"
      CASE INLIST(m.tnTyperet, 'L')
           RETURN "logical"
      CASE INLIST(m.tnTyperet, 'N', 'Y', 'I', 'F')
           RETURN "numeric"
      CASE INLIST(m.tnTyperet, 'O')
           RETURN "Object"
      CASE INLIST(m.tnTyperet, 'G')
           RETURN "General"
      OTHERWISE
           RETURN 'Unknown'
 ENDCASE
ENDFUNC
*
FUNCTION AnyToC
 LPARAMETERS inVar, lnTyperet
 IF PCOUNT()<2
      lnTyperet = 0
 ENDIF
 LOCAL tyPvar
 tyPvar = TYPE("m.inVar")
 DO CASE
      CASE INLIST(m.tyPvar, 'C', 'M')
           RETURN m.inVar
      CASE INLIST(m.tyPvar, 'N', 'I', 'Y')
           RETURN asSpec(m.inVar)
      CASE m.tyPvar='D'
           RETURN DTOC(m.inVar)
      CASE m.tyPvar='T'
           RETURN TTOC(m.inVar)
      CASE m.tyPvar='L'
           IF m.lnTyperet=1
                RETURN IIF(m.inVar, "Yes", "No")
           ELSE
                RETURN IIF(m.inVar, ".t.", ".f.")
           ENDIF
      CASE m.tyPvar='O'
           RETURN "Object"
      CASE m.tyPvar='G'
           RETURN "General"
      OTHERWISE
           RETURN m.inVar
 ENDCASE
ENDFUNC
*
FUNCTION callHelp
 PARAMETER m.heLptopic
 IF SET("HELP")="OFF" .OR. EMPTY(m.heLptopic)
      RETURN .F.
 ENDIF
 IF TYPE('m.helptopic')='N'
      DO (m.hoMe+'err.APP') WITH m.heLptopic
      m.heLptopic = 'Диалогов прозорец ГРЕШКА'
 ENDIF
 HELP &helptopic
 RETURN .T.
ENDFUNC
*
PROCEDURE win_mesg
 LPARAMETERS tcMessin, tcWintitle, tlMaximize, tnHeight, tnWidth,  ;
             tnSpecfont, tcStyle
 IF PCOUNT()<2
      tcWintitle = UPPER("information")
 ENDIF
 IF PCOUNT()<4
      tnHeight = 0
 ENDIF
 IF PCOUNT()<5
      tnWidth = 0
 ENDIF
 IF PCOUNT()<6
      tnSpecfont = 0
 ENDIF
 IF PCOUNT()<7
      tcStyle = "N"
 ENDIF
 = edItinwindow(m.tcMessin,m.tcWintitle,.T.,m.tlMaximize,m.tnHeight, ;
   m.tnWidth,m.tnSpecfont,m.tcStyle)
ENDPROC
*
FUNCTION Encrypt
 PARAMETER stRingin, paSswin
 LOCAL j, pwLen, stRlen, paSsnum, i, nbUff, stRingout, chRstring, chRpass
 pwLen = LEN(m.paSswin)
 IF m.pwLen<3
      RETURN m.stRingin
 ENDIF
 stRlen = LEN(m.stRingin)
 paSsnum = INT((((_tR_pnum(m.paSswin)/997)-1)/254)+1)
 stRingout = ''
 j = 1
 FOR i = 1 TO m.stRlen
      paSsnum = INT(((m.paSsnum+(m.i-1-m.stRlen))-1)/254)+1
      chRstring = SUBSTR(m.stRingin, m.i, 1)
      chRpass = SUBSTR(m.paSswin, m.j, 1)
      nbUff = BITXOR(ASC(m.chRstring), BITXOR(m.paSsnum, ASC(m.chRpass)))
      IF BETWEEN(m.nbUff, 0, 255)
           stRingout = m.stRingout+CHR(m.nbUff)
      ELSE
           stRingout = m.stRingout+m.chRstring
      ENDIF
      IF m.j>=m.pwLen
           j = 1
      ELSE
           j = m.j+1
      ENDIF
 ENDFOR
 RETURN m.stRingout
ENDFUNC
*
FUNCTION _tr_pnum
 LPARAMETERS m.cpAsswrd
 LOCAL m.nrEturn, m.i, m.leNs
 m.nrEturn = 1
 m.leNs = LEN(m.cpAsswrd)
 FOR m.i = 1 TO m.leNs
      m.nrEturn = m.nrEturn+ASC(SUBSTR(m.cpAsswrd, m.i, 1))+m.i-1
 ENDFOR
 DO WHILE m.nrEturn<10000000
      m.nrEturn = BITLSHIFT(m.nrEturn, 1)
 ENDDO
 RETURN m.nrEturn
ENDFUNC
*
FUNCTION to_left
 PARAMETER m.stRing
 IF NOT INLIST(TYPE('m.string'), 'C', 'M')
      RETURN m.stRing
 ENDIF
 PRIVATE m.leNg
 m.leNg = LEN(m.stRing)
 RETURN PADR(LTRIM(m.stRing), m.leNg)
ENDFUNC
*
FUNCTION TextWidth
 LPARAMETERS tcTextin, tnTypewidth, tcFontname, tnFontsize, tlFontbold,  ;
             tlFontitalic
 LOCAL lcFontstyle
 lcFontstyle = foNtstyle(m.tlFontbold,m.tlFontitalic)
 RETURN CEILING(TXTWIDTH(m.tcTextin, m.tcFontname, m.tnFontsize,  ;
        m.lcFontstyle)*FONTMETRIC(m.tnTypewidth, m.tcFontname,  ;
        m.tnFontsize, m.lcFontstyle))
ENDFUNC
*
FUNCTION FontStyle
 LPARAMETERS tlFontbold, tlFontitalic
 LOCAL lcStyle
 lcStyle = ''
 IF tlFontbold
      lcStyle = lcStyle+'B'
 ENDIF
 IF tlFontitalic
      lcStyle = lcStyle+'I'
 ENDIF
 IF EMPTY(lcStyle)
      lcStyle = 'N'
 ENDIF
 RETURN m.lcStyle
ENDFUNC
*
FUNCTION FontStyleN
 LPARAMETERS tnFontstyle
 DO CASE
      CASE m.tnFontstyle=1
           RETURN 'B'
      OTHERWISE
           RETURN 'N'
 ENDCASE
ENDFUNC
*
FUNCTION WriteIniData
 LPARAMETERS tcInicontent, tcSectionname, tcKeyname, tcNewdata
 LOCAL lcHrbeg, lcHrend, i, lcSectionname, lcSectmpname, lnSeclen,  ;
       lcTmprow, llFoundsection, llFoundkey, lnTmplen, lcKeyname,  ;
       lnKeylen, lcEqusign, lnBegpoz, lnEndpoz, lnEndpozvalid, lnLeninicontent
 m.reTvalue = .F.
 lcHrbeg = '['
 lcHrend = ']'
 lcEqusign = '='
 lcSectionname = LOWER(ALLTRIM(m.tcSectionname))
 lcKeyname = LOWER(ALLTRIM(tcKeyname))
 lnSeclen = LEN(m.lcSectionname)
 lnKeylen = LEN(m.lcKeyname)
 llFoundsection = .F.
 llFoundkey = .F.
 lnLeninicontent = LEN(m.tcInicontent)
 i = 0
 lnBegpoz = 1
 lnEndpozvalid = 0
 DO WHILE lnBegpoz<lnLeninicontent
      i = m.i+1
      lnEndpoz = AT(CHR(13), m.tcInicontent, m.i)
      IF m.lnEndpoz=0
           lnEndpoz = lnLeninicontent
           lcTmprow = SUBSTR(m.tcInicontent, lnBegpoz, lnEndpoz-lnBegpoz+1)
      ELSE
           lcTmprow = SUBSTR(m.tcInicontent, lnBegpoz, lnEndpoz-lnBegpoz)
           IF lnLeninicontent>lnEndpoz .AND. SUBSTR(m.tcInicontent,  ;
              lnEndpoz+1, 1)=CHR(10)
                lnEndpoz = lnEndpoz+1
           ENDIF
      ENDIF
      lnTmplen = LEN(m.lcTmprow)
      IF PADR(m.lcTmprow, 1)=CHR(10)
           IF lnTmplen>1
                lnBegpoz = lnBegpoz+1
                lcTmprow = SUBSTR(m.lcTmprow, 2)
           ELSE
                lcTmprow = ''
           ENDIF
      ENDIF
      lcTmprow = LTRIM(m.lcTmprow)
      lnTmplen = LEN(m.lcTmprow)
      IF llFoundsection
           IF PADR(m.lcTmprow, 1)=m.lcHrbeg
                EXIT
           ENDIF
           lnPozequ = AT(lcEqusign, m.lcTmprow)
           IF m.lnPozequ>m.lnKeylen
                IF lcKeyname==LOWER(ALLTRIM(SUBSTR(m.lcTmprow, 1, lnPozequ-1)))
                     llFoundkey = .T.
                     EXIT
                ENDIF
           ENDIF
           IF m.lnPozequ>0
                lnEndpozvalid = lnEndpoz
           ENDIF
      ELSE
           IF m.lnTmplen>m.lnSeclen+1
                lcSectmpname = LOWER(PADR(m.lcTmprow, m.lnSeclen+2))
                IF LEFT(m.lcSectmpname, 1)=m.lcHrbeg .AND.  ;
                   m.lcSectionname==SUBSTR(m.lcSectmpname, 2, m.lnSeclen)  ;
                   .AND. RIGHT(m.lcSectmpname, 1)=m.lcHrend
                     llFoundsection = .T.
                     lnEndpozvalid = lnEndpoz
                ENDIF
           ENDIF
      ENDIF
      lnBegpoz = lnEndpoz+1
 ENDDO
 DO CASE
      CASE llFoundsection .AND. llFoundkey
           tcInicontent = SUBSTR(m.tcInicontent, 1, m.lnBegpoz-1)+ ;
                          m.tcKeyname+m.lcEqusign+m.tcNewdata+CHR(13)+ ;
                          CHR(10)+IIF(lnLeninicontent<=m.lnEndpoz, '',  ;
                          SUBSTR(m.tcInicontent, m.lnEndpoz+1))
           m.reTvalue = .T.
      CASE llFoundsection .AND. NOT llFoundkey
           IF lnEndpozvalid=0
                lnEndpozvalid = lnEndpoz
           ENDIF
           tcInicontent = SUBSTR(m.tcInicontent, 1, m.lnEndpozvalid)+ ;
                          m.tcKeyname+m.lcEqusign+m.tcNewdata+CHR(13)+ ;
                          CHR(10)+IIF(lnLeninicontent<=m.lnEndpozvalid,  ;
                          '', SUBSTR(m.tcInicontent, m.lnEndpozvalid+1))
           m.reTvalue = .T.
      CASE NOT llFoundsection
           tcInicontent = m.tcInicontent+IIF(EMPTY(m.tcInicontent), '',  ;
                          CHR(13)+CHR(10))+m.lcHrbeg+m.tcSectionname+ ;
                          m.lcHrend+CHR(13)+CHR(10)
           tcInicontent = m.tcInicontent+m.tcKeyname+m.lcEqusign+ ;
                          m.tcNewdata+CHR(13)+CHR(10)
           m.reTvalue = .T.
 ENDCASE
 RETURN m.reTvalue
ENDFUNC
*
FUNCTION GetIniData
 LPARAMETERS tcInicontent, tcSectionname, tcKeyname, tcGotdata
 LOCAL lcHrbeg, lcHrend, lnRownumber, i, lcSectionname, lcSectmpname,  ;
       lnSeclen, lcTmprow, llFoundsection, lnTmplen, lcKeyname, lnKeylen,  ;
       reTvalue
 reTvalue = .F.
 tcGotdata = ''
 lcHrbeg = '['
 lcHrend = ']'
 lcSectionname = LOWER(ALLTRIM(m.tcSectionname))
 lcKeyname = LOWER(ALLTRIM(tcKeyname))
 lnSeclen = LEN(m.lcSectionname)
 lnKeylen = LEN(m.lcKeyname)
 lnRownumber = woRds(m.tcInicontent,CHR(13))
 llFoundsection = .F.
 FOR i = 1 TO m.lnRownumber
      lcTmprow = woRdnum(m.tcInicontent,m.i,CHR(13))
      lnTmplen = LEN(m.lcTmprow)
      IF PADR(m.lcTmprow, 1)=CHR(10)
           IF lnTmplen>1
                lcTmprow = SUBSTR(m.lcTmprow, 2)
           ELSE
                lcTmprow = ''
           ENDIF
      ENDIF
      lcTmprow = LTRIM(m.lcTmprow)
      lnTmplen = LEN(m.lcTmprow)
      IF llFoundsection
           IF PADR(m.lcTmprow, 1)=m.lcHrbeg
                EXIT
           ENDIF
           IF EMPTY(m.tcKeyname)
                tcGotdata = m.tcGotdata+IIF(EMPTY(m.tcGotdata), '',  ;
                            CHR(13))+lcTmprow
           ELSE
                lnPozequ = AT('=', m.lcTmprow)
                IF m.lnPozequ>m.lnKeylen
                     IF lcKeyname==LOWER(ALLTRIM(SUBSTR(m.lcTmprow, 1,  ;
                        lnPozequ-1)))
                          reTvalue = .T.
                          IF m.lnPozequ<m.lnTmplen
                               tcGotdata = SUBSTR(m.lcTmprow, lnPozequ+1)
                          ENDIF
                          EXIT
                     ENDIF
                ENDIF
           ENDIF
      ELSE
           IF m.lnTmplen>m.lnSeclen+1
                lcSectmpname = LOWER(PADR(m.lcTmprow, m.lnSeclen+2))
                IF LEFT(m.lcSectmpname, 1)=m.lcHrbeg .AND.  ;
                   m.lcSectionname==SUBSTR(m.lcSectmpname, 2, m.lnSeclen)  ;
                   .AND. RIGHT(m.lcSectmpname, 1)=m.lcHrend
                     llFoundsection = .T.
                ENDIF
           ENDIF
      ENDIF
 ENDFOR
 RETURN m.reTvalue
ENDFUNC
*
FUNCTION CreTabl1
 LPARAMETERS paThin, asTrct, blOcksiz, cuRsname, tcClause
 IF PCOUNT()<3 .OR. EMPTY(m.blOcksiz)
      m.blOcksiz = seTblock()
 ENDIF
 IF PCOUNT()<4
      cuRsname = ''
 ENDIF
 IF PCOUNT()<5
      tcClause = IIF(EMPTY(m.cuRsname), 'FREE', '')
 ENDIF
 PRIVATE stRuctinfo
 IF TYPE("aStrct[1]")="U"
      stRuctinfo = "("+m.asTrct+")"
 ELSE
      stRuctinfo = "FROM ARRAY aStrct"
 ENDIF
 PRIVATE m.olDblock, m.erRor_l, m.olDonerr
 m.olDblock = seTblock()
 m.erRor_l = 0
 m.olDonerr = onError()
 ON ERROR ERROR_L=ERROR()
 SET BLOCKSIZE TO m.blOcksiz
 IF EMPTY(m.cuRsname)
      m.paThin = foRce_dbf(m.paThin,.T.)
      IF deLfile(m.paThin,0,@m.erRor_l)
           CREATE TABLE (m.pathIN) &tcClause  &structInfo 
      ENDIF
 ELSE
      CREATE CURSOR (m.cursName) &tcClause  &structInfo
 ENDIF
 SET BLOCKSIZE TO m.olDblock
 ON ERROR &oldOnERR
 RETURN m.erRor_l
ENDFUNC
*
PROCEDURE addAstruc
 LPARAMETERS a_Struc, tnLen, fnAme, ftYpe, flEng, fdEci, tlNull, tlNocptrans
 DO CASE
      CASE INLIST(m.ftYpe, 'M', 'G', 'I', 'P')
           flEng = 4
           fdEci = 0
      CASE INLIST(m.ftYpe, 'D', 'T')
           flEng = 8
           fdEci = 0
      CASE INLIST(m.ftYpe, 'L')
           flEng = 1
           fdEci = 0
 ENDCASE
 IF EMPTY(m.fdEci)
      m.fdEci = 0
 ENDIF
 IF m.tnLen=-1
      tnLen = ALEN(a_Struc, 1)
 ENDIF
 tnLen = m.tnLen+1
 DIMENSION a_Struc[m.tnLen, 16]
 a_Struc[m.tnLen, 1] = m.fnAme
 a_Struc[m.tnLen, 2] = m.ftYpe
 a_Struc[m.tnLen, 3] = m.flEng
 a_Struc[m.tnLen, 4] = m.fdEci
 a_Struc[m.tnLen, 5] = m.tlNull
 a_Struc[m.tnLen, 6] = m.tlNocptrans
 STORE "" TO a_Struc[m.tnLen, 7], a_Struc[m.tnLen, 8], a_Struc[m.tnLen,  ;
       9], a_Struc[m.tnLen, 10], a_Struc[m.tnLen, 11], a_Struc[m.tnLen,  ;
       12], a_Struc[m.tnLen, 13], a_Struc[m.tnLen, 14], a_Struc[m.tnLen,  ;
       15], a_Struc[m.tnLen, 16]
 RETURN
ENDPROC
*
FUNCTION CreaCurs
 PARAMETER m.alIasi, m.cuRsname
 IF NOT USED(m.alIasi) .OR. EMPTY(m.cuRsname)
      RETURN .F.
 ENDIF
 SELECT (m.alIasi)
 PRIVATE tmPflds
 IF AFIELDS(tmPflds)=0
      RETURN .F.
 ENDIF
 RETURN (crEtabl1('',@tmPflds,0,m.cuRsname)=0)
ENDFUNC
*
FUNCTION appFrom1
 PARAMETER m.frOmdbf, m.spEcname, m.foRexpr_in, m.fuN_onrec, m.apPdeletd
 IF PCOUNT()<2
      m.spEcname = m.frOmdbf
 ENDIF
 IF PCOUNT()<3
      m.foRexpr_in = ''
 ENDIF
 IF PCOUNT()<4
      m.fuN_onrec = ''
 ENDIF
 IF PCOUNT()<5
      m.apPdeletd = .F.
 ENDIF
 frOmdbf = foRce_dbf(m.frOmdbf,.T.)
 m.reTvalue = FILE(m.frOmdbf)
 IF m.reTvalue
      DO waItmess WITH 'Добавя данни от '+m.spEcname
      PRIVATE m.olDtalkw, m.olDtalk
      m.olDtalkw = seTtalkwin()
      m.olDtalk = seTtalkon()
      m.reTvalue = (apPndnodel(m.frOmdbf,m.foRexpr_in,m.fuN_onrec, ;
                   m.apPdeletd)=0)
      IF m.reTvalue
           FLUSH
      ENDIF
      SET TALK &oldTalk
      SET TALK &OldTalkW
      RELEASE WINDOW taLk
      WAIT CLEAR
 ENDIF
 RETURN m.reTvalue
ENDFUNC
*
FUNCTION appndNOdel
 PARAMETER m.frOmdbf, m.foRexprin, m.fuNonrec, m.apPdeleted, m.frOmal
 IF PCOUNT()<2
      m.foRexprin = ''
 ENDIF
 IF PCOUNT()<3
      m.fuNonrec = ''
 ENDIF
 IF PCOUNT()<4
      m.apPdeleted = .F.
 ENDIF
 IF PCOUNT()<5
      m.frOmal = ''
 ENDIF
 PRIVATE m.olDdeltd, m.erRor_l, m.olDonerror
 m.erRor_l = 0
 m.olDdeltd = seTdeleted()
 IF m.apPdeleted
      SET DELETED OFF
 ELSE
      m.foRexprin = m.foRexprin+anDstr(m.foRexprin)+'!DELETED()'
      SET DELETED ON
 ENDIF
 m.foRexprin = foRstr(m.foRexprin)+m.foRexprin
 IF EMPTY(m.fuNonrec) .AND. NOT m.apPdeleted
      m.olDonerror = onError()
      ON ERROR ERROR_L=ERROR()
      APPEND FROM (m.fromDBF) &forExprIN
      ON ERROR &oldONError
 ELSE
      PRIVATE m.toDbf, m.okLocate, m.clOsefrom
      m.okLocate = .T.
      m.clOsefrom = .F.
      m.toDbf = SELECT()
      IF EMPTY(m.frOmal) .OR. NOT USED(m.frOmal)
           m.frOmal = unIqalias()
           m.okLocate = loCatedb(m.frOmdbf,.F.,m.frOmal)
           m.clOsefrom = .T.
      ENDIF
      IF m.okLocate
           SELECT (m.frOmal)
           PRIVATE tmParrflds, m.ifOrfor, m.dlTdlocl
           DIMENSION tmParrflds[1]
           IF NOT EMPTY(liStfields(1,@tmParrflds,245))
                FOR m.ifOrfor = 1 TO ALEN(tmParrflds)
                     PRIVATE &tmpArrFlds[m.iForFor]
                ENDFOR
           ENDIF
           SCAN &forExprIN
                SCATTER MEMO MEMVAR
                m.dlTdlocl = DELETED()
                SELECT (m.toDbf)
                IF adDrec() .AND. reClock()
                     GATHER MEMO MEMVAR
                ELSE
                     m.erRor_l = -2
                     EXIT
                ENDIF
                IF NOT EMPTY(m.fuNonrec)
                     IF !&funOnRec()
                          m.erRor_l = -3
                          EXIT
                     ENDIF
                ENDIF
                IF m.dlTdlocl
                     DELETE
                ENDIF
                UNLOCK
                SELECT (m.frOmal)
           ENDSCAN
           IF m.clOsefrom
                = clOse1(m.frOmal)
           ENDIF
           SELECT (m.toDbf)
      ELSE
           m.erRor_l = -1
      ENDIF
 ENDIF
 SET DELETED &oldDeltd
 RETURN m.erRor_l
ENDFUNC
*
FUNCTION indexWind
 PARAMETER m.meSsagein, m.taG_namein, m.taG_exprin, m.to_file,  ;
           m.adDclauses, m.tyPdefw
 IF PCOUNT()<4
      m.to_file = .F.
 ENDIF
 IF PCOUNT()<5
      m.adDclauses = ''
 ENDIF
 IF PCOUNT()<6
      m.tyPdefw = 0
 ENDIF
 IF NOT USED()
      RETURN .F.
 ENDIF
 PRIVATE m.taLkwind
 m.taLkwind = 'TALK'
 IF m.meSsagein=1
      PRIVATE m.olDtalkw, m.olDtalk
      m.olDtalkw = SET('TALK', 1)
      IF INLIST(m.tyPdefw, -1, 0, 1) .OR. NOT WVISIBLE(m.taLkwind)
           = seTtalkwin()
      ENDIF
      SET TALK WINDOW &talkWind
      m.olDtalk = seTtalkon()
      ACTIVATE WINDOW &talkWind
      ?
      ? ' Индексира '+as(RECCOUNT())+' записа, файл '+DBF()
      ? ' Име '+m.taG_namein+', израз '+m.taG_exprin+' '+m.adDclauses
 ENDIF
 IF m.to_file
      INDEX ON &tag_exprIN TO &tag_nameIN ADDITIVE &addClauses
 ELSE
      INDEX ON &tag_exprIN TAG &tag_nameIN ADDITIVE &addClauses
 ENDIF
 IF m.meSsagein=1
      SET TALK &oldTalk
      SET TALK &OldTalkW
      IF INLIST(m.tyPdefw, 0, 2)
           RELEASE WINDOW &talkWind
      ENDIF
 ENDIF
 IF m.tyPdefw=-1
      m.tyPdefw = 3
 ENDIF
 RETURN TAGNO(m.taG_namein)>0
ENDFUNC
*
PROCEDURE waitMess
 PARAMETER m.in_string
 WAIT CLEAR
 WAIT WINDOW NOWAIT m.in_string+'.'+CHR(13)+"Please wait..."
 RETURN
ENDPROC
*
FUNCTION setTalkWin
 PRIVATE m.olDtalkwin
 m.olDtalkwin = SET('TALK', 1)
 LOCAL wiNtxtfnt
 wiNtxtfnt = geTwintxtfnt()
 DEFINE WINDOW talk FROM 1, 0 TO 7,79  DOUBLE COLOR RGB(,,,192,192,192)  FONT &wintxtfnt STYLE "B"
 ACTIVATE WINDOW taLk
 SET TALK WINDOW taLk
 RETURN m.olDtalkwin
ENDFUNC
*
FUNCTION setTalkOn
 PRIVATE m.loColdtalk
 m.loColdtalk = SET('TALK')
 SET TALK ON
 RETURN m.loColdtalk
ENDFUNC
*
FUNCTION setTalkOff
 PRIVATE m.loColdtalk
 m.loColdtalk = SET('TALK')
 SET TALK OFF
 RETURN m.loColdtalk
ENDFUNC
*
FUNCTION uniqAlias
 PARAMETER m.fiRstlett
 IF PCOUNT()<1
      m.fiRstlett = 'T'
 ENDIF
 DO WHILE .T.
      m.tmPali = m.fiRstlett+LEFT(SYS(3), 7)
      IF NOT USED(m.tmPali)
           RETURN m.tmPali
      ENDIF
 ENDDO
ENDFUNC
*
FUNCTION listFields
 PARAMETER m.tyPret, arRlist, m.maXlen
 IF PCOUNT()<1
      m.tyPret = 0
 ENDIF
 IF PCOUNT()<3
      m.maXlen = 256
 ENDIF
 PRIVATE m.flDcount, m.i, m.reTval, m.leNarr, m.tmPstrng, m.tmParrstr
 STORE '' TO m.tmParrstr, m.reTval
 m.leNarr = 1
 DIMENSION arRlist[m.leNarr]
 IF USED()
      m.flDcount = FCOUNT()
      m.reTval = IIF(m.tyPret=0, '', 'm.')+LOWER(FIELD(1))
      m.tmParrstr = m.reTval
      arRlist[m.leNarr] = m.tmParrstr
      FOR m.i = 2 TO m.flDcount
           m.tmPstrng = IIF(m.tyPret=0, ', ', ',m.')+LOWER(FIELD(m.i))
           m.reTval = m.reTval+m.tmPstrng
           m.tmParrstr = m.tmParrstr+m.tmPstrng
           IF LEN(m.tmParrstr)>m.maXlen
                m.leNarr = m.leNarr+1
                DIMENSION arRlist[m.leNarr]
                m.tmParrstr = SUBSTR(m.tmPstrng, 2)
           ENDIF
           arRlist[m.leNarr] = m.tmParrstr
      ENDFOR
 ENDIF
 RETURN m.reTval
ENDFUNC
*
FUNCTION LtoStr
 PARAMETER m.loG_in
 IF PCOUNT()<1
      m.loG_in = .F.
 ENDIF
 IF m.loG_in
      RETURN "Yes"
 ELSE
      RETURN "No"
 ENDIF
ENDFUNC
*
FUNCTION telType
 PARAMETER ttYp, shOrtl
 IF PCOUNT()<1
      ttYp = ttYpe
 ENDIF
 IF PCOUNT()<2
      shOrtl = 0
 ENDIF
 PRIVATE m.reTvalue
 m.reTvalue = ""
 DO CASE
      CASE m.ttYp=1
           DO CASE
                CASE m.shOrtl=1
                     m.reTvalue = PADR("Phone", 3)+"."
                CASE m.shOrtl=2
                     m.reTvalue = ''
                OTHERWISE
                     m.reTvalue = "Phone"
           ENDCASE
      CASE m.ttYp=2
           m.reTvalue = "Fax"
      CASE m.ttYp=3
           m.reTvalue = "Phone/Fax"
      CASE m.ttYp=4
           m.reTvalue = "Telex"
      CASE m.ttYp=5
           m.reTvalue = "E-mail"
      CASE m.ttYp=6
           m.reTvalue = "Other"
 ENDCASE
 RETURN m.reTvalue
ENDFUNC
*
FUNCTION TelTypeConv
 LPARAMETERS tcOldttype
 DO CASE
      CASE m.tcOldttype="F"
           RETURN 2
      CASE m.tcOldttype="2"
           RETURN 3
      CASE m.tcOldttype="X"
           RETURN 4
      OTHERWISE
           RETURN 1
 ENDCASE
ENDFUNC
*
FUNCTION FLDGetProp
 PARAMETER tcAlias, tcField, tnColumn
 tcField = UPPER(ALLTRIM(m.tcField))
 LOCAL lnLen, lcRet, i
 PRIVATE laStruct
 lcRet = ''
 IF USED(m.tcAlias)
      lnLen = AFIELDS(laStruct, m.tcAlias)
      FOR i = 1 TO m.lnLen
           IF UPPER(ALLTRIM(laStruct(m.i,1)))==m.tcField
                IF EMPTY(m.tnColumn)
                     lcRet = laStruct(m.i,2)+","+ALLTRIM(STR(laStruct(m.i, ;
                             3)))+","+ALLTRIM(STR(laStruct(m.i,4)))
                ELSE
                     lcRet = laStruct(m.i,m.tnColumn)
                ENDIF
                EXIT
           ENDIF
      ENDFOR
 ENDIF
 RETURN lcRet
ENDFUNC
*
FUNCTION FLDGetType
 PARAMETER tcAlias, tcField
 RETURN flDgetprop(m.tcAlias,m.tcField,2)
ENDFUNC
*
FUNCTION FLDGetLen
 PARAMETER tcAlias, tcField
 RETURN flDgetprop(m.tcAlias,m.tcField,3)
ENDFUNC
*
FUNCTION FLDGetDeci
 PARAMETER tcAlias, tcField
 RETURN flDgetprop(m.tcAlias,m.tcField,4)
ENDFUNC
*
FUNCTION GetExprData
 LPARAMETERS tcExpr, tcType, tnSize, tnDeci
 STORE 0 TO tnSize, tnDeci
 tcExpr = ltRim1(trIm1(UPPER(tcExpr),')'),'(')
 LOCAL i, llFound, lnOldselect
 lnOldselect = SELECT()
 llFound = geTexprsize(tcExpr,@tnSize,@tnDeci)
 IF NOT m.llFound .AND. "."$m.tcExpr
      FOR i = 1 TO 254
           llFound = geTexprsize(tcExpr,@tnSize,@tnDeci)
           IF m.llFound
                EXIT
           ENDIF
      ENDFOR
 ENDIF
 IF NOT m.llFound
      DO CASE
           CASE INLIST(m.tcType, 'L')
                tnSize = 1
           CASE INLIST(m.tcType, 'C')
                tnSize = 60
           CASE INLIST(m.tcType, 'D', 'T')
                tnSize = 8
           CASE INLIST(m.tcType, 'G', 'B', 'M')
                tnSize = 4
           CASE INLIST(m.tcType, 'N', 'F')
                tnSize = 12
                tnDeci = 2
           CASE INLIST(m.tcType, 'I', 'Y')
                tnSize = 8
      ENDCASE
 ENDIF
 SELECT (m.lnOldselect)
 RETURN .T.
ENDFUNC
*
FUNCTION GetExprSize
 LPARAMETERS tcExpr, tnSize, tnDeci
 STORE 0 TO tnSize, tnDeci
 LOCAL llRet, i, lcAlias, lcField
 llRet = .F.
 IF USED()
      lcAlias = UPPER(ALIAS())
      FOR i = 1 TO FCOUNT()
           lcField = UPPER(FIELD(m.i))
           IF m.tcExpr==m.lcField
                llRet = .T.
                EXIT
           ENDIF
           IF m.tcExpr==m.lcAlias+"."+m.lcField
                llRet = .T.
                EXIT
           ENDIF
      ENDFOR
      IF m.llRet
           tnSize = flDgetlen(m.lcAlias,m.lcField)
           tnDeci = flDgetdeci(m.lcAlias,m.lcField)
           IF flDgettype(m.lcAlias,m.lcField)='M'
                tnSize = 60
           ENDIF
      ENDIF
 ENDIF
 RETURN m.llRet
ENDFUNC
*
FUNCTION ATspec
 LPARAMETERS csEarchexpr, ceXprsearched, lmAtchcase, lwHolewords
 LOCAL lnPoz
 IF m.lmAtchcase
      lnPoz = AT_C(m.csEarchexpr, m.ceXprsearched)
 ELSE
      lnPoz = ATCC(m.csEarchexpr, m.ceXprsearched)
 ENDIF
 IF m.lnPoz>0 .AND. m.lwHolewords
      LOCAL lwOrdstartok, lwOrdendok, lnLensearchexpr
      lnLensearchexpr = LEN(m.csEarchexpr)
      lwOrdstartok = m.lnPoz=1 .OR. SUBSTR(m.ceXprsearched, m.lnPoz-1, 1)$" .,()[]{}!?@#$%^&*-+=:;\|/<>'`~"+'"'+CHR(13)+CHR(10)
      lwOrdendok = m.lnPoz+m.lnLensearchexpr-1>=LEN(m.ceXprsearched) .OR. SUBSTR(m.ceXprsearched, m.lnPoz+m.lnLensearchexpr, 1)$" .,()[]{}!?@#$%^&*-+=:;\|/<>'`~"+'"'+CHR(13)+CHR(10)
      IF m.lwOrdstartok .AND. m.lwOrdendok
      ELSE
           m.lnPoz = 0
      ENDIF
 ENDIF
 RETURN m.lnPoz
ENDFUNC
*
FUNCTION escRp
 IF NOT m.esCyet .AND. CHRSAW()
      CLEAR TYPEAHEAD
      IF yeS('Регистриран е опит за','прекъсване на справката.', ;
         'Потвърждавате ли?',0,'ВХОД ОТ КЛАВИАТУРАТА')
           m.esCyet = .T.
           SET FILTER TO .F.
      ENDIF
 ENDIF
 RETURN m.esCyet
ENDFUNC
*
FUNCTION typeRep
 PARAMETER m.paThrfin
 PRIVATE m.olDselct, m.reTvalue
 IF NOT FILE(m.paThrfin)
      RETURN -1
 ENDIF
 m.olDselct = SELECT()
 SELECT 0
 USE &pathrfIN ALIAS frxTMP AGAIN
 m.reTvalue = 0
 IF USED()
      LOCATE FOR plAtform='DOS'
      IF FOUND()
           m.reTvalue = 1
      ENDIF
      LOCATE FOR plAtform='WINDOWS'
      DO CASE
           CASE FOUND() .AND. m.reTvalue=1
                m.reTvalue = 3
           CASE FOUND() .AND. m.reTvalue=0
                m.reTvalue = 2
      ENDCASE
      USE
 ENDIF
 SELECT (m.olDselct)
 RETURN m.reTvalue
ENDFUNC
*
FUNCTION getCompUser
 PARAMETER m.coMputer, m.usEr
 STORE '' TO m.coMputer, m.usEr
 PRIVATE m.sySstr, m.poZ
 m.sySstr = SYS(0)
 m.poZ = AT('#', m.sySstr)
 IF m.poZ=0
      RETURN .F.
 ENDIF
 m.coMputer = ALLTRIM(SUBSTR(m.sySstr, 1, m.poZ-1))
 m.usEr = ALLTRIM(SUBSTR(m.sySstr, m.poZ+1))
 RETURN .T.
ENDFUNC
*
FUNCTION WordnumX
 PARAMETER m.tcString, m.nuMelem, m.seParin
 IF PCOUNT()<3
      m.seParin = ','
 ENDIF
 PRIVATE m.stArt, m.enD, m.maXell
 m.maXell = OCCURS(m.seParin, m.tcString+m.seParin)
 IF m.maXell=0 .AND. m.nuMelem=1
      RETURN m.tcString
 ENDIF
 IF m.nuMelem>m.maXell
      RETURN ""
 ENDIF
 IF m.nuMelem=1
      m.stArt = 1
 ELSE
      m.stArt = AT(m.seParin, m.tcString, m.nuMelem-1)+1
 ENDIF
 IF m.stArt>LEN(m.tcString)
      RETURN ""
 ENDIF
 m.enD = AT(m.seParin, m.tcString+m.seParin, m.nuMelem)
 RETURN SUBSTR(m.tcString, m.stArt, m.enD-m.stArt)
ENDFUNC
*
FUNCTION WordsX
 PARAMETER tcStrinp, tcSepinp
 IF PCOUNT()<2
      tcSepinp = ","
 ENDIF
 RETURN OCCURS(m.tcSepinp, m.tcStrinp)+1
ENDFUNC
*
FUNCTION getMesRow
 PARAMETER m.enTertext, m.esCtext, tcSpecdlm
 IF PCOUNT()<1 .OR. m.enTertext=='*'
      m.enTertext = 'избор '
 ENDIF
 IF PCOUNT()<2 .OR. m.esCtext=='*'
      m.esCtext = 'отказ'
 ENDIF
 IF PCOUNT()<3
      m.tcSpecdlm = '*'
 ENDIF
 RETURN m.tcSpecdlm+'F1-помощ '+m.tcSpecdlm+' F10-подредба '+m.tcSpecdlm+ ;
        ' клавиш,F6,9,Ctrl+F,G-търси '+IIF(EMPTY(m.enTertext), '',  ;
        m.tcSpecdlm+' ENTER-'+m.enTertext)+IIF(EMPTY(m.esCtext), '',  ;
        m.tcSpecdlm+' ESC-'+m.esCtext+m.tcSpecdlm)
ENDFUNC
*
FUNCTION seekPart
 LPARAMETERS m.seEkexpr
 IF NOT INLIST(VARTYPE(m.seEkexpr), 'C', 'M')
      RETURN SEEK(m.seEkexpr)
 ENDIF
 LOCAL olDexact
 m.olDexact = seTexact()
 SET EXACT OFF
 SEEK m.seEkexpr 
 SET EXACT &OldExact
 RETURN FOUND()
ENDFUNC
*
FUNCTION CreateIniVars
 LPARAMETERS tcMainname
 IF VARTYPE(pcHomedir)<>'C'
      PUBLIC pcHomedir
      pcHomedir = ADDBS(JUSTPATH(SYS(16, 0)))
 ENDIF
 SET DEFAULT TO &pcHomeDir
 PUBLIC inIdirdatarelated
 inIdirdatarelated = ''
 PUBLIC inIdirnodatarelated
 inIdirnodatarelated = m.pcHomedir
 PUBLIC inIdircomprelated
 inIdircomprelated = m.pcHomedir
 PUBLIC m.inIfilegeneral
 m.inIfilegeneral = FORCEEXT(m.tcMainname, "INI")
 RETURN .T.
ENDFUNC
*
FUNCTION escRp
 IF NOT m.esCyet .AND. CHRSAW()
      CLEAR TYPEAHEAD
      IF yeS('Регистриран е опит за','прекъсване на справката.', ;
         'Потвърждавате ли?',0,'ВХОД ОТ КЛАВИАТУРАТА')
           m.esCyet = .T.
           SET FILTER TO .F.
      ENDIF
 ENDIF
 RETURN m.esCyet
ENDFUNC
*
FUNCTION doMemo
 PARAMETER m.inString
 PRIVATE m.liNetodo, m.coUnter, m.olDmemo
 m.inString = STRTRAN(m.inString, CHR(9), ' ')
 m.olDmemo = SET('MEMOWIDTH')
 SET MEMOWIDTH TO 256
 m.coUnter = 0
 DO WHILE .T.
      m.coUnter = m.coUnter+1
      m.liNetodo = alLtrimcr(MLINE(m.inString, m.coUnter))
      IF EMPTY(m.liNetodo)
           EXIT
      ELSE
           &lineToDo 
      ENDIF
 ENDDO
 SET MEMOWIDTH TO m.olDmemo
 RETURN m.coUnter-1
ENDFUNC
*
FUNCTION waitTimeout
 PARAMETER m.meSsin, m.tiMeout
 IF PCOUNT()<2
      m.tiMeout = 0.5
 ENDIF
 WAIT CLEAR
 WAIT WINDOW TIMEOUT m.tiMeout m.meSsin
 WAIT WINDOW NOWAIT m.meSsin
 RETURN .T.
ENDFUNC
*
FUNCTION BrowseInWin
 PARAMETER tcPreffer, tcWinname, tcTitle, tcFields, tlNoedit, tlNoappend,  ;
           tlNodelete, tlNowait, tlSavesize, tcOthcla, tnFromcol, tnTocol,  ;
           tnFromrow, tnHeight, tlRefresh
 PRIVATE lcNoedit, lcNoappend, lcNodelete, lcNowait, lcWindowcl, lcNorefresh
 lcNoedit = IIF(m.tlNoedit, 'NOEDIT', '')
 lcNoappend = IIF(m.tlNoappend, 'NOAPPEND', '')
 lcNodelete = IIF(m.tlNodelete, 'NODELETE', '')
 lcNowait = IIF(m.tlNowait, 'NOWAIT', '')
 IF EMPTY(m.tnFromcol)
      tnFromcol = 0
 ENDIF
 IF EMPTY(m.tnTocol)
      tnTocol = laStcolt()
 ENDIF
 IF EMPTY(m.tnFromrow)
      m.tnFromrow = 0
 ENDIF
 IF EMPTY(m.tcWinname)
      tnFromrow = m.tnFromrow
 ELSE
      PRIVATE lnWinrows
      lnWinrows = WROWS(m.tcWinname)
      tnFromrow = m.lnWinrows+wiNdisplace(m.lnWinrows)+m.tnFromrow
 ENDIF
 PRIVATE lnTorow
 lnTorow = laStrowt()
 IF NOT EMPTY(m.tnHeight)
      lnTorow = m.tnFromrow+m.tnHeight-1
 ENDIF
 LOCAL wiNtxtfnt
 wiNtxtfnt = geTwintxtfnt()
 DEFINE WINDOW BrTmpWin  FROM m.tnFromRow, m.tnFromCol  TO  m.lnToRow, m.tnToCol  FONT &wintxtfnt
 lcNorefresh = IIF(m.tlRefresh, '', 'NOREFRESH')
 lcWindowcl = 'WINDOW BrTmpWin'
 tcFields = IIF(EMPTY(m.tcFields), '', 'FIELDS '+m.tcFields)
 LOCAL reMember
 m.reMember = .T.
 IF m.reMember .AND. fiNd_reso(m.tcPreffer)
      IF m.tlSavesize
           lcWindowcl = ''
      ENDIF
      tcFields = ''
 ELSE
      = deL_reso(m.tcPreffer)
 ENDIF
 BROWSE NORMAL &lcWindowCl &lcNoWait &lcNoedit &lcNoAppend &lcNoDelete &lcNoRefresh  PREFERENCE (m.tcPreffer) TITLE (m.tcTitle)  &tcOthCla  &tcFields
 RELEASE WINDOW brTmpwin
 RETURN .T.
ENDFUNC
*
FUNCTION WinDisplace
 PARAMETER lnHeightrows
 PRIVATE lnDisplace
 lnDisplace = -(m.lnHeightrows/10+(m.lnHeightrows-15)/16)
 RETURN m.lnDisplace
ENDFUNC
*
FUNCTION find_reso
 PARAMETER m.prEfin
 IF EMPTY(m.prEfin) .OR. EMPTY(SYS(2005))
      RETURN .F.
 ENDIF
 PRIVATE m.olDbase, m.tyPeidloc, m.prEfloc, m.reTvall
 m.olDbase = SELECT()
 SELECT 0
 USE SYS(2005) AGAIN
 m.reTvall = USED()
 IF m.reTvall
      m.tyPeidloc = PADR("WINDBROW", LEN(id))
      m.prEfloc = PADR(UPPER(ALLTRIM(m.prEfin)), LEN(naMe))
      LOCATE FOR id==m.tyPeidloc .AND. naMe==m.prEfloc
      m.reTvall = FOUND()
      IF m.reTvall .AND. EMPTY(daTa)
           m.reTvall = .F.
      ENDIF
      USE
 ENDIF
 SELECT (m.olDbase)
 RETURN m.reTvall
ENDFUNC
*
FUNCTION del_reso
 PARAMETER m.prEfin
 IF EMPTY(m.prEfin) .OR. EMPTY(SYS(2005))
      RETURN .T.
 ENDIF
 PRIVATE m.olDbase, m.tyPeidloc, m.prEfloc, m.reTvall
 m.olDbase = SELECT()
 SELECT 0
 USE SYS(2005) AGAIN
 m.reTvall = USED()
 IF m.reTvall
      m.tyPeidloc = PADR("WINDBROW", LEN(id))
      m.prEfloc = PADR(UPPER(ALLTRIM(m.prEfin)), LEN(naMe))
      REPLACE daTa WITH '', naMe WITH '' ALL FOR id==m.tyPeidloc .AND.  ;
              naMe==m.prEfloc
      USE
 ENDIF
 SELECT (m.olDbase)
 RETURN m.reTvall
ENDFUNC
*
FUNCTION lastRowT
 RETURN IIF(_DOS, 24, 24.7)
ENDFUNC
*
FUNCTION lastColT
 RETURN IIF(_DOS, 79, 80)
ENDFUNC
*
FUNCTION GetWinTxtFnt
 LOCAL wiNtxtfnt
 IF TYPE('oApp')<>'O'
      wiNtxtfnt = '"MS Sans Serif", 8'
 ELSE
      wiNtxtfnt = oaPp.wiNtxtfnt
 ENDIF
 RETURN m.wiNtxtfnt
ENDFUNC
*
FUNCTION GetWinFixFnt
 LOCAL wiNfixfnt
 IF TYPE('oApp')<>'O'
      wiNfixfnt = '"Courier New", 9'
 ELSE
      wiNfixfnt = oaPp.wiNfixfnt
 ENDIF
 RETURN m.wiNfixfnt
ENDFUNC
*
FUNCTION packIndex
 PARAMETER m.meSsagein, m.reIndx_l, m.tyPdefw, m.deLtagall, m.onLymemo,  ;
           m.tyPeact, m.toFile, m.spEcname
 IF PCOUNT()<1
      m.meSsagein = 0
 ENDIF
 IF PCOUNT()<2
      m.reIndx_l = .T.
 ENDIF
 IF PCOUNT()<3
      m.tyPdefw = 0
 ENDIF
 IF PCOUNT()<4
      m.deLtagall = .F.
 ENDIF
 IF PCOUNT()<5
      m.onLymemo = .F.
 ENDIF
 IF PCOUNT()<6
      m.tyPeact = 0
 ENDIF
 IF PCOUNT()<7
      m.toFile = ''
 ENDIF
 IF PCOUNT()<8
      m.spEcname = geTdbname(ALIAS())
 ENDIF
 IF NOT USED()
      = inFo_mesg('Няма отворена база.'+CHR(13)+IIF(m.tyPeact=0,  ;
        'Пакетиране', 'Копиране')+' не е извършено.')
      RETURN .F.
 ENDIF
 PRIVATE m.reCsloc, m.reColdloc, m.taLkwind
 m.taLkwind = 'TALK'
 m.reCsloc = RECCOUNT()
 m.reColdloc = RECNO()
 _TALLY = 0
 IF m.meSsagein=1
      DO waItmess WITH 'Обработва '+m.spEcname
      PRIVATE m.olDtalkw, m.olDtalk
      m.olDtalkw = SET('TALK', 1)
      IF INLIST(m.tyPdefw, -1, 0, 1) .OR. NOT WVISIBLE(m.taLkwind)
           = seTtalkwin()
      ENDIF
      SET TALK WINDOW &talkWind
      m.olDtalk = seTtalkon()
      ACTIVATE WINDOW &talkWind
      ?
      ? IIF(m.tyPeact=0, 'Пакетира ', 'Копира ')+as(RECCOUNT())+ ;
        ' записа, файл '+DBF()
 ENDIF
 PRIVATE m.reTval
 m.reTval = .F.
 DO CASE
      CASE m.tyPeact=0
           PRIVATE m.seTtts_l
           m.seTtts_l = .F.
           IF m.deLtagall
                DELETE TAG alL
           ENDIF
           IF exClopenmg(1)
                IF m.onLymemo
                     PACK MEMO
                ELSE
                     PACK
                ENDIF
                IF m.reIndx_l .AND. m.reCsloc=_TALLY
                     REINDEX
                ENDIF
                FLUSH
           ENDIF
           m.reTval = IIF(_TALLY=0 .AND. m.reCsloc<>0, .F., .T.)
      CASE m.tyPeact=3
           PRIVATE m.cdXkeyword, m.toFile
           IF m.deLtagall
                m.cdXkeyword = ''
           ELSE
                m.cdXkeyword = 'CDX'
           ENDIF
           PRIVATE m.lcOldorder, m.lcOldfilter
           m.lcOldorder = seTorder()
           m.lcOldfilter = seTfilter()
           SET FILTER TO
           SET ORDER TO
           m.reTval = deLfile(m.toFile)
           IF m.reTval
                LOCAL lnCodepage
                lnCodepage = CPDBF()
                COPY TO &toFile FOR !DELETED() &cdxKeyWord
                = cpChange(m.toFile,m.lnCodepage)
                FLUSH
           ENDIF
           SET FILTER TO &lcOldFilter
           SET ORDER TO &lcOldOrder
 ENDCASE
 = go1(m.reColdloc)
 IF m.meSsagein=1
      SET TALK &oldTalk
      SET TALK &OldTalkW
      IF INLIST(m.tyPdefw, 0, 2)
           RELEASE WINDOW &talkWind
      ENDIF
      WAIT CLEAR
 ENDIF
 IF m.tyPdefw=-1
      m.tyPdefw = 3
 ENDIF
 RETURN m.reTval
ENDFUNC
*
FUNCTION packDelTag
 RETURN paCkindex(1,.F.,0,.T.)
ENDFUNC
*
FUNCTION aCreaPack
 PARAMETER m.paThdbf, asTrct, m.tiTledbf, m.blOcksiz, m.foRcecrea,  ;
           m.fuN_on_rec, m.tyPdefw, m.tyPeact, m.toDir, m.tyPeprobl, m.lcOpycdx
 IF PCOUNT()<5
      m.foRcecrea = .F.
 ENDIF
 IF PCOUNT()<6
      m.fuN_on_rec = ''
 ENDIF
 IF PCOUNT()<7
      m.tyPdefw = 0
 ENDIF
 IF PCOUNT()<8
      m.tyPeact = 0
 ENDIF
 IF PCOUNT()<9
      m.toDir = ''
 ENDIF
 m.tyPeprobl = ''
 IF PCOUNT()<11
      m.lcOpycdx = .T.
 ENDIF
 IF PCOUNT()<4 .OR. EMPTY(m.blOcksiz)
      m.blOcksiz = seTblock()
 ENDIF
 IF m.tyPeact=-1
      RETURN .T.
 ENDIF
 m.paThdbf = foRce_dbf(m.paThdbf,.T.)
 LOCAL llFreetable
 llFreetable = NOT inDbctable(JUSTSTEM(m.paThdbf))
 PRIVATE m.orIgalias
 m.orIgalias = UPPER(JUSTSTEM(m.paThdbf))
 IF FILE(m.paThdbf)
      DO CASE
           CASE INLIST(m.tyPeact, 0, 2)
                = clOse1(m.orIgalias)
                IF NOT inDexuse(m.paThdbf)
                     RETURN .F.
                ENDIF
           CASE INLIST(m.tyPeact, 1, 3) .AND. NOT loCatedb(m.paThdbf,.F.)
                m.tyPeprobl = 'не може да се отвори'
                RETURN .F.
      ENDCASE
 ELSE
      DO CASE
           CASE INLIST(m.tyPeact, 0, 2)
                = clOse1(m.orIgalias)
                IF crEtabl1(m.paThdbf,@asTrct,m.blOcksiz,'', ;
                   IIF(m.llFreetable, 'FREE', ''))<>0
                     RETURN .F.
                ENDIF
           CASE INLIST(m.tyPeact, 1, 3)
                m.tyPeprobl = 'не съществува'
                RETURN .F.
      ENDCASE
 ENDIF
 IF NOT USED(m.orIgalias)
      RETURN .F.
 ENDIF
 SELECT (m.orIgalias)
 IF EMPTY(m.tiTledbf)
      m.tiTledbf = geTdbname(ALIAS())
 ENDIF
 PRIVATE m.tmPali, m.tmPname, m.orIgdbf, aoRig, m.leNorig, m.leNin,  ;
         m.eqUstruct, m.i
 EXTERNAL ARRAY asTrct
 m.leNorig = AFIELDS(aoRig)
 m.eqUstruct = (m.leNorig=ALEN(asTrct, 1))
 IF m.eqUstruct
      FOR m.i = 1 TO m.leNorig
           IF UPPER(ALLTRIM(asTrct(m.i,1)))<>aoRig(m.i,1) .OR. asTrct(m.i, ;
              2)<>aoRig(m.i,2) .OR. asTrct(m.i,3)<>aoRig(m.i,3) .OR.  ;
              asTrct(m.i,4)<>aoRig(m.i,4)
                m.eqUstruct = .F.
                m.tyPeprobl = 'структура - разлика в поле '+aoRig(m.i,1)
                EXIT
           ENDIF
      ENDFOR
 ELSE
      m.tyPeprobl = 'структура - различен брой полета'
 ENDIF
 PRIVATE m.reTv
 DO CASE
      CASE INLIST(m.tyPeact, 0)
           IF m.eqUstruct .AND. NOT m.foRcecrea
                m.reTv = paCkindex(1,.F.,@m.tyPdefw,.T.,.F.,m.tyPeact,'', ;
                         m.tiTledbf)
           ELSE
                m.orIgdbf = DBF()
                m.tmPali = unIqalias()
                m.tmPname = ADDBS(JUSTPATH(m.orIgdbf))+m.tmPali+'.'+ ;
                            JUSTEXT(m.orIgdbf)
                m.reTv = crEtabl1(m.tmPname,@asTrct,m.blOcksiz,'','FREE')= ;
                         0 .AND. USED(m.tmPali)
                IF m.reTv
                     SELECT (m.tmPali)
                     m.reTv = apPfrom1(m.orIgdbf,m.tiTledbf,'', ;
                              m.fuN_on_rec,.F.)
                     IF m.reTv
                          = clOse1(m.orIgalias)
                          m.reTv = deLetedbf(m.orIgdbf)
                          IF m.reTv
                               = clOse1(m.tmPali)
                               m.reTv = reNamedbf(m.tmPname,m.orIgdbf)
                               IF m.reTv
                                    m.reTv = inDexuse(m.orIgdbf)
                               ENDIF
                          ELSE
                          ENDIF
                     ELSE
                          = clOse1(m.tmPali)
                          = deLetedbf(m.tmPname)
                     ENDIF
                ENDIF
           ENDIF
      CASE INLIST(m.tyPeact, 1)
           WAIT WINDOW NOWAIT 'Проверява '+m.tiTledbf
           m.reTv = m.eqUstruct
      CASE INLIST(m.tyPeact, 2)
           WAIT WINDOW NOWAIT 'Нулира '+m.tiTledbf
           ZAP
           m.reTv = (RECCOUNT()=0)
      CASE INLIST(m.tyPeact, 3)
           m.reTv = paCkindex(1,.F.,@m.tyPdefw,NOT m.lcOpycdx,.F., ;
                    m.tyPeact,FORCEPATH(DBF(), m.toDir))
 ENDCASE
 RETURN m.reTv
ENDFUNC
*
FUNCTION aPackInd
 PARAMETER aiNd, m.cyRnamdb, m.inDdeleted, m.meSifnot, m.paCkdbf, m.meSs,  ;
           m.tyPdefw, m.tyPeact, m.tyPeprobl
 IF PCOUNT()<3
      m.inDdeleted = .T.
 ENDIF
 IF PCOUNT()<4
      m.meSifnot = .T.
 ENDIF
 IF PCOUNT()<5
      m.paCkdbf = .T.
 ENDIF
 IF PCOUNT()<6
      m.meSs = 1
 ENDIF
 IF PCOUNT()<7
      m.tyPdefw = 0
 ENDIF
 IF PCOUNT()<8
      m.tyPeact = 0
 ENDIF
 IF PCOUNT()<2
      m.cyRnamdb = geTdbname(ALIAS())
 ENDIF
 m.tyPeprobl = ''
 IF m.tyPeact=-1
      RETURN .T.
 ENDIF
 PRIVATE m.tyPoper
 m.tyPoper = 'индекси - '
 IF NOT USED()
      m.tyPeprobl = m.tyPoper+'не е отворена'
      RETURN .F.
 ENDIF
 EXTERNAL ARRAY aiNd
 PRIVATE aiNdloc, m.leNnew
 = ACOPY(aiNd, aiNdloc)
 m.leNnew = ALEN(aiNdloc, 1)
 IF m.inDdeleted
      = adDindnm(@m.leNnew,@aiNdloc,"DELETED","DELETED()")
 ENDIF
 PRIVATE m.reTv, m.reTvp, m.reTvi, m.i, m.ii, m.taGname, m.taGexpr,  ;
         m.taGclause, m.tmPstr
 m.tmPstr = ''
 STORE .T. TO m.reTvp, m.reTvi
 DO CASE
      CASE INLIST(m.tyPeact, 0, 2, 3)
           IF m.paCkdbf
                m.reTvp = paCkdtmes(m.cyRnamdb) .AND. m.reTvp
                IF NOT m.reTvp
                     m.tmPstr = 'ПАКЕТИРАНЕ'
                ENDIF
           ENDIF
           m.ii = 0
           FOR m.i = 1 TO m.leNnew
                m.taGname = aiNdloc(m.i,1)
                IF NOT EMPTY(m.taGname)
                     m.taGexpr = aiNdloc(m.i,2)
                     IF EMPTY(m.taGexpr)
                          m.taGexpr = m.taGname
                     ENDIF
                     m.taGclause = aiNdloc(m.i,3)
                     m.reTvi = inDexwinm(m.meSs,m.taGname,m.taGexpr,@m.ii, ;
                               m.leNnew,m.cyRnamdb,m.taGclause, ;
                               @m.tyPdefw) .AND. m.reTvi
                ENDIF
           ENDFOR
           FLUSH
           IF NOT m.reTvi
                m.tmPstr = m.tmPstr+IIF(EMPTY(m.tmPstr), '', '/')+'ИНДЕКСИРАНЕ'
           ENDIF
      CASE m.tyPeact=1
           PRIVATE m.leNcur
           m.leNcur = TAGCOUNT()
           IF m.leNcur<m.leNnew
                m.tyPeprobl = m.tyPoper+'по-малък брой'
                m.reTvi = .F.
           ELSE
                PRIVATE m.jk, m.taGnm
                FOR m.i = 1 TO m.leNnew
                     m.taGnm = UPPER(ALLTRIM(aiNdloc(m.i,1)))
                     m.jk = TAGNO(m.taGnm)
                     IF m.jk=0
                          m.tyPeprobl = m.tyPoper+'липсва '+m.taGnm
                          m.reTvi = .F.
                          EXIT
                     ENDIF
                     IF KEY(m.jk)<>UPPER(ALLTRIM(aiNdloc(m.i,2)))
                          m.tyPeprobl = m.tyPoper+'различен израз за '+m.taGnm
                          m.reTvi = .F.
                          EXIT
                     ENDIF
                ENDFOR
           ENDIF
 ENDCASE
 m.reTv = m.reTvp .AND. m.reTvi
 IF m.meSifnot .AND. NOT m.reTv .AND. m.tyPeact=0
      = inFo_mesg('Операция '+IIF(m.tyPeact=1, 'ПРОВЕРКА ИНДЕКСИ',  ;
        m.tmPstr)+' за файл '+DBF()+' е неуспешна.')
 ENDIF
 RETURN m.reTv
ENDFUNC
*
FUNCTION indexWinM
 PARAMETER m.meSsin, m.taGnam, m.taGexp, m.cuRrnomb, m.toTnumb, m.naMedb,  ;
           m.adDclaus, m.tyPdefw
 IF PCOUNT()<5
      m.toTnumb = m.cuRrnomb
 ENDIF
 IF PCOUNT()<6
      m.naMedb = ''
 ENDIF
 IF PCOUNT()<7 .OR. EMPTY(m.adDclaus)
      m.adDclaus = ''
 ENDIF
 IF PCOUNT()<8
      m.tyPdefw = 0
 ENDIF
 IF EMPTY(m.taGexp)
      m.taGexp = m.taGnam
 ENDIF
 IF NOT USED()
      RETURN .F.
 ENDIF
 m.cuRrnomb = m.cuRrnomb+1
 IF m.meSsin=1
      DO waItmess WITH m.naMedb+IIF(EMPTY(m.naMedb), 'И', ' - и')+ ;
         'ндекс '+as(m.cuRrnomb)+IIF(EMPTY(toTnumb), '', '/'+as(m.toTnumb))
 ENDIF
 PRIVATE loCreturn
 loCreturn = inDexwind(m.meSsin,m.taGnam,m.taGexp,.F.,m.adDclaus,@m.tyPdefw)
 IF m.meSsin=1 .AND. m.cuRrnomb=m.toTnumb
      WAIT CLEAR
 ENDIF
 RETURN m.loCreturn
ENDFUNC
*
FUNCTION indexWind
 PARAMETER m.meSsagein, m.taG_namein, m.taG_exprin, m.to_file,  ;
           m.adDclauses, m.tyPdefw
 IF PCOUNT()<4
      m.to_file = .F.
 ENDIF
 IF PCOUNT()<5
      m.adDclauses = ''
 ENDIF
 IF PCOUNT()<6
      m.tyPdefw = 0
 ENDIF
 IF NOT USED()
      RETURN .F.
 ENDIF
 PRIVATE m.taLkwind
 m.taLkwind = 'TALK'
 IF m.meSsagein=1
      PRIVATE m.olDtalkw, m.olDtalk
      m.olDtalkw = SET('TALK', 1)
      IF INLIST(m.tyPdefw, -1, 0, 1) .OR. NOT WVISIBLE(m.taLkwind)
           = seTtalkwin()
      ENDIF
      SET TALK WINDOW &talkWind
      m.olDtalk = seTtalkon()
      ACTIVATE WINDOW &talkWind
      ?
      ? ' Индексира '+as(RECCOUNT())+' записа, файл '+DBF()
      ? ' Име '+m.taG_namein+', израз '+m.taG_exprin+' '+m.adDclauses
 ENDIF
 IF m.to_file
      INDEX ON &tag_exprIN TO &tag_nameIN ADDITIVE &addClauses
 ELSE
      INDEX ON &tag_exprIN TAG &tag_nameIN ADDITIVE &addClauses
 ENDIF
 IF m.meSsagein=1
      SET TALK &oldTalk
      SET TALK &OldTalkW
      IF INLIST(m.tyPdefw, 0, 2)
           RELEASE WINDOW &talkWind
      ENDIF
 ENDIF
 IF m.tyPdefw=-1
      m.tyPdefw = 3
 ENDIF
 RETURN TAGNO(m.taG_namein)>0
ENDFUNC
*
FUNCTION packDTmes
 PARAMETER m.naMedb
 IF PCOUNT()<1
      m.naMedb = ''
 ENDIF
 IF NOT USED()
      RETURN .F.
 ENDIF
 DO waItmess WITH m.naMedb+IIF(EMPTY(m.naMedb), 'У', ' - у')+'плътняване'
 RETURN paCkdeltag()
ENDFUNC
*
PROCEDURE addIndNm
 PARAMETER m.inDnumb, aiNdex, m.taGname, m.taGexpr, m.clAuses
 IF PCOUNT()<4 .OR. EMPTY(m.taGexpr)
      m.taGexpr = m.taGname
 ENDIF
 IF PCOUNT()<5
      m.clAuses = ''
 ENDIF
 m.inDnumb = m.inDnumb+1
 DIMENSION aiNdex(m.inDnumb, 3)
 aiNdex(m.inDnumb, 1) = m.taGname
 aiNdex(m.inDnumb, 2) = m.taGexpr
 aiNdex(m.inDnumb, 3) = m.clAuses
 RETURN
ENDPROC
*
FUNCTION indexUSE
 PARAMETER m.dbFin
 PRIVATE m.usEttsloc
 m.usEttsloc = TYPE('m.useTTS')='L' .AND. m.usEtts
 = deLfile(foRce_cdx(stRipext(m.dbFin)),m.usEttsloc)
 = doCommand('FREE TABLE '+m.dbFin)
 SELECT 0
 RETURN neTuse(m.dbFin,.T.,5,'','',.T.,m.usEttsloc,0,1)
ENDFUNC
*
FUNCTION DoCommand
 LPARAMETERS tcCommandtodo
 LOCAL llError, lcOldonerror
 PRIVATE tmParr
 llError = 0
 lcOldonerror = onError()
 ON ERROR LLERROR=ERROR()
 &tcCommandToDo
 ON ERROR &lcOldOnError
 RETURN m.llError=0
ENDFUNC
*
FUNCTION DecToHex
 LPARAMETERS tnInnum
 IF m.tnInnum<=15
      RETURN PADL(fiNdhex(m.tnInnum), 2, "0")
 ENDIF
 LOCAL lcOutstr
 lcOutstr = SPACE(0)
 DO WHILE m.tnInnum>0
      lcOutstr = fiNdhex(MOD(m.tnInnum, 16))+m.lcOutstr
      tnInnum = INT(m.tnInnum/16)
 ENDDO
 RETURN m.lcOutstr
ENDFUNC
*
FUNCTION FindHex
 LPARAMETERS inVal
 LOCAL joUtstr
 DO CASE
      CASE m.inVal=10
           joUtstr = 'A'
      CASE m.inVal=11
           joUtstr = 'B'
      CASE m.inVal=12
           joUtstr = 'C'
      CASE m.inVal=13
           joUtstr = 'D'
      CASE m.inVal=14
           joUtstr = 'E'
      CASE m.inVal=15
           joUtstr = 'F'
      OTHERWISE
           joUtstr = STR(m.inVal, 1, 0)
 ENDCASE
 RETURN m.joUtstr
ENDFUNC
*
FUNCTION HexToDec
 LPARAMETERS tcInstr
 LOCAL jlEn, nsUm, rpTr, ncTr
 tcInstr = ALLTRIM(m.tcInstr)
 jlEn = LEN(m.tcInstr)
 STORE 0 TO nsUm, rpTr
 FOR ncTr = 1 TO m.jlEn
      cpTr = UPPER(SUBSTR(m.tcInstr, m.jlEn-m.rpTr, 1))
      DO CASE
           CASE cpTr='A'
                cpTr = '10'
           CASE cpTr='B'
                cpTr = '11'
           CASE cpTr='C'
                cpTr = '12'
           CASE cpTr='D'
                cpTr = '13'
           CASE cpTr='E'
                cpTr = '14'
           CASE cpTr='F'
                cpTr = '15'
      ENDCASE
      nsUm = m.nsUm+(VAL(m.cpTr)*16^(m.ncTr-1))
      rpTr = m.rpTr+1
 ENDFOR
 RETURN INT(m.nsUm)
ENDFUNC
*
FUNCTION DTOC2
 LPARAMETERS tdAte
 LOCAL lcRet, lcYear
 lcRet = DTOC(m.tdAte)
 IF SET('CENT')='ON'
      IF EMPTY(m.tdAte)
           lcYear = SPACE(4)
      ELSE
           lcYear = ALLTRIM(STR(YEAR(m.tdAte)))
      ENDIF
      lcRet = STRTRAN(m.lcRet, m.lcYear, RIGHT(m.lcYear, 2))
 ENDIF
 RETURN m.lcRet
ENDFUNC
*
FUNCTION STOD
 LPARAMETERS tcDatestring
 LOCAL ldReturn, llError, lcOldonerror
 ldReturn = CTOD('')
 IF NOT EMPTY(m.tcDatestring)
      tcDatestring = PADR(ALLTRIM(m.tcDatestring), 8)
      llError = 0
      lcOldonerror = onError()
      ON ERROR LLERROR=ERROR()
      ldReturn = DATE(VAL(LEFT(m.tcDatestring, 4)),  ;
                 VAL(SUBSTR(m.tcDatestring, 5, 2)),  ;
                 VAL(SUBSTR(m.tcDatestring, 7, 2)))
      ON ERROR &lcOldOnError
 ENDIF
 RETURN m.ldReturn
ENDFUNC
*
FUNCTION FileSize
 LPARAMETERS tcFilename
 LOCAL lcSetcompatible, lnFilesize
 lcSetcompatible = SET('COMPATIBLE')
 SET COMPATIBLE DB4
 lnFilesize = FSIZE(tcFilename)
 SET COMPATIBLE &lcSetCompatible
 RETURN lnFilesize
ENDFUNC
*
FUNCTION INDBCtable
 LPARAMETERS tcTablename
 LOCAL llRet
 llRet = .F.
 IF NOT EMPTY(DBC())
      llRet = INDBC(m.tcTablename, 'TABLE')
 ENDIF
 RETURN m.llRet
ENDFUNC
*
FUNCTION GetDbName
 PARAMETER m.tc_dbf
 RETURN dbGettblcomment(m.tc_dbf)
ENDFUNC
*
FUNCTION DBGETfldCaption
 PARAMETER m.naMein
 RETURN DBGETPROP(m.naMein, "FIELD", "Caption")
ENDFUNC
*
FUNCTION DBGETtblComment
 PARAMETER m.naMein
 RETURN DBGETPROP(m.naMein, "TABLE", "Comment")
ENDFUNC
*
FUNCTION GetBufferMode
 LPARAMETERS lcAlias
 IF PCOUNT()<1
      lcAlias = ALIAS()
 ENDIF
 RETURN CURSORGETPROP("Buffering", m.lcAlias)
ENDFUNC
*
FUNCTION SetBufferMode
 LPARAMETERS nmOde, lcAlias
 IF PCOUNT()<2
      lcAlias = ALIAS()
 ENDIF
 RETURN CURSORSETPROP("Buffering", m.nmOde, m.lcAlias)
ENDFUNC
*
FUNCTION IsMemo
 LPARAMETERS tcType
 RETURN m.tcType$'MPG'
ENDFUNC
*
FUNCTION ASCANdouble
 LPARAMETERS arRtoscan, teValue, tnColtoscan
 EXTERNAL ARRAY arRtoscan
 LOCAL lcList, lnLenarr, lnColumncnt, i, leValue
 lnLenarr = ALEN(arRtoscan, 1)
 lnColumncnt = ALEN(arRtoscan, 2)
 IF m.lnColumncnt>0 .AND. EMPTY(m.tnColtoscan)
      tnColtoscan = 1
 ENDIF
 lcList = ''
 FOR i = 1 TO lnLenarr
      leValue = IIF(m.lnColumncnt=0, arRtoscan(m.i), arRtoscan(m.i, ;
                m.tnColtoscan))
      IF m.leValue==m.teValue
           lcList = m.lcList+IIF(EMPTY(m.lcList), '', ",")+as(m.i)
      ENDIF
 ENDFOR
 RETURN m.lcList
ENDFUNC
*
FUNCTION aColScan
 LPARAMETERS aaRray, eeLement, ncOlumn
 LOCAL lfOundit, nsTartpos, nfOundelem
 IF TYPE("aArray[1]")="U"
      RETURN -1
 ENDIF
 IF TYPE("nColumn")<>"N" .OR. NOT BETWEEN(ncOlumn, 1, ALEN(aaRray, 2))
      RETURN -1
 ENDIF
 lfOundit = .F.
 nsTartpos = 1
 DO WHILE NOT lfOundit .AND. nsTartpos<=ALEN(aaRray)
      nfOundelem = ASCAN(aaRray, eeLement, nsTartpos)
      IF nfOundelem>0
           IF ASUBSCRIPT(aaRray, nfOundelem, 2)=ncOlumn
                lfOundit = .T.
           ELSE
                nsTartpos = nfOundelem+1
           ENDIF
      ELSE
           nsTartpos = ALEN(aaRray)+1
      ENDIF
 ENDDO
 IF nsTartpos>ALEN(aaRray)
      RETURN 0
 ELSE
      RETURN ASUBSCRIPT(aaRray, nfOundelem, 1)
 ENDIF
 RETURN
ENDFUNC
*
FUNCTION CleanName
 PARAMETER m.inStr
 m.inStr = STRTRAN(m.inStr, '\!', '')
 m.inStr = STRTRAN(m.inStr, '\?', '')
 m.inStr = STRTRAN(m.inStr, '\<', '')
 m.inStr = STRTRAN(m.inStr, '...', '')
 RETURN m.inStr
ENDFUNC
*
FUNCTION SeekNear
 LPARAMETERS m.seEkexpr
 LOCAL llFound, olDexact, olDnear
 m.olDnear = seTnear()
 m.olDexact = seTexact()
 SET NEAR ON
 SET EXACT OFF
 llFound = SEEK(m.seEkexpr)
 SET NEAR &oldnear
 SET EXACT &oldexact
 RETURN m.llFound
ENDFUNC
*
