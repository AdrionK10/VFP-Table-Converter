***
*** 3dbox.fxp
***
*
 PARAMETER m.boXrow, m.boXcol, m.boXhgt, m.boXwdth, m.boXpenw, m.suNr,  ;
           m.suNg, m.suNb, m.shAder, m.shAdeg, m.shAdeb, m.faCer, m.faCeg,  ;
           m.faCeb, m.ouTliner, m.ouTlineg, m.ouTlineb, m.paTtern, m.peNtype
 PRIVATE m.boXrow, m.boXcol, m.boXhgt, m.boXwdth, m.cuRpenw, m.boXpenw,  ;
         m.suNr, m.suNg, m.suNb, m.shAder, m.shAdeg, m.shAdeb, m.teMpr,  ;
         m.teMpg, m.teMpb, m.faCer, m.faCeg, m.faCeb, m.ouT_on,  ;
         m.ouTliner, m.ouTlineg, m.ouTlineb, m.paTtern, m.peNtype,  ;
         m.seTdec, m.cuRfont, m.cuRsize, m.cuRstyle, m.hcLength,  ;
         m.vcLength, m.cuRrow, m.cuRcol, m.loOppen, m.prEvpen, m.coUnter,  ;
         mcOunter, m.roW, m.coL
 IF NOT _WINDOWS
      WAIT WINDOW NOWAIT 'This program requires FoxPro for Windows.'
      RETURN .F.
 ENDIF
 IF m.boXrow<0
      WAIT WINDOW NOWAIT 'Negative row provided for this box.'
      RETURN .F.
 ENDIF
 IF m.boXcol<0
      WAIT WINDOW NOWAIT 'Negative column provided for this box.'
      RETURN .F.
 ENDIF
 IF m.boXhgt<=0
      WAIT WINDOW NOWAIT 'Invalid height provided for this box.'
      RETURN .F.
 ENDIF
 IF m.boXwdth<=0
      WAIT WINDOW NOWAIT 'Invalid width provided for this box.'
      RETURN .F.
 ENDIF
 IF TYPE('m.pattern')<>'N' .OR. EMPTY(m.paTtern)
      m.paTtern = 1
 ENDIF
 IF TYPE('m.pentype')<>'N' .OR. EMPTY(m.peNtype)
      m.peNtype = -1
 ENDIF
 IF m.boXcol+m.boXwdth>=WCOLS()
      WAIT WINDOW NOWAIT 'Box width too long for window.'
      RETURN .F.
 ENDIF
 IF m.boXrow+m.boXhgt>=WROWS()
      WAIT WINDOW NOWAIT 'Box height too big for window.'
      RETURN .F.
 ENDIF
 m.cuRpenw = m.boXpenw
 IF TYPE('m.curpenw')<>'N'
      m.cuRpenw = 0
 ENDIF
 IF BETWEEN(m.cuRpenw, -2, 2)
      m.cuRpenw = ROUND(m.cuRpenw, 0)
 ENDIF
 IF TYPE('m.sunr')<>'N'
      m.suNr = 255
 ENDIF
 IF TYPE('m.sung')<>'N'
      m.suNg = 255
 ENDIF
 IF TYPE('m.sunb')<>'N'
      m.suNb = 255
 ENDIF
 IF TYPE('m.shader')<>'N'
      m.shAder = 128
 ENDIF
 IF TYPE('m.shadeg')<>'N'
      m.shAdeg = 128
 ENDIF
 IF TYPE('m.shadeb')<>'N'
      m.shAdeb = 128
 ENDIF
 IF m.cuRpenw<0
      m.teMpr = m.suNr
      m.teMpg = m.suNg
      m.teMpb = m.suNb
      m.suNr = m.shAder
      m.suNg = m.shAdeg
      m.suNb = m.shAdeb
      m.shAder = m.teMpr
      m.shAdeg = m.teMpg
      m.shAdeb = m.teMpb
      m.cuRpenw = -m.cuRpenw
 ENDIF
 IF TYPE('m.facer')<>'N'
      m.faCer = 192
 ENDIF
 IF TYPE('m.faceg')<>'N'
      m.faCeg = 192
 ENDIF
 IF TYPE('m.faceb')<>'N'
      m.faCeb = 192
 ENDIF
 IF TYPE('m.outliner')<>'N' .AND. TYPE('m.outlineg')<>'N' .AND.  ;
    TYPE('m.outlineb')<>'N'
      m.ouT_on = .F.
      STORE 192 TO m.ouTliner, m.ouTlineg, m.ouTlineb
 ELSE
      m.ouT_on = .T.
      IF TYPE('m.outliner')<>'N'
           m.ouTliner = 192
      ENDIF
      IF TYPE('m.outlineg')<>'N'
           m.ouTlineg = 192
      ENDIF
      IF TYPE('m.outlineb')<>'N'
           m.ouTlineb = 192
      ENDIF
 ENDIF
 m.seTdec = SET('DECIMALS')
 SET DECIMALS TO 15
 m.woUtput = WOUTPUT()
 m.cuRfont = WFONT(1, m.woUtput)
 m.cuRsize = WFONT(2, m.woUtput)
 m.cuRstyle = WFONT(3, m.woUtput)
 m.hcLength = FONTMETRIC(6, m.cuRfont, m.cuRsize, m.cuRstyle)
 m.vcLength = FONTMETRIC(1, m.cuRfont, m.cuRsize, m.cuRstyle)+ ;
              FONTMETRIC(5, m.cuRfont, m.cuRsize, m.cuRstyle)
 IF m.hcLength*m.boXwdth<=m.cuRpenw*2
      WAIT WINDOW NOWAIT 'Pen width along horizontal axis too thick.'
      SET DECIMALS TO &m.setdec
      RETURN .F.
 ENDIF
 IF m.vcLength*m.boXhgt<=m.cuRpenw*2
      WAIT WINDOW NOWAIT 'Pen width along vertical axis too thick.'
      SET DECIMALS TO &m.setdec
      RETURN .F.
 ENDIF
 m.cuRrow = m.boXrow
 m.cuRcol = m.boXcol
 m.loOppen = m.cuRpenw
 FOR m.coUnter = 1 TO CEILING(m.cuRpenw/6)
      m.loOppen = IIF(m.loOppen<7, m.loOppen, 6)
      IF m.coUnter=1
           m.prEvpen = m.loOppen
      ENDIF
      DO boXdraw
      IF m.coUnter<CEILING(m.cuRpenw/6)
           m.boXrow = m.boXrow+(m.loOppen/m.vcLength)
           m.boXcol = m.boXcol+(m.loOppen/m.hcLength)
           m.prEvpen = m.loOppen
           m.loOppen = m.cuRpenw-(6*m.coUnter)
      ENDIF
 ENDFOR
 @ m.cuRrow+(m.cuRpenw/m.vcLength), m.cuRcol+(m.cuRpenw/m.hcLength) TO  ;
   (m.cuRrow+m.boXhgt)-(m.cuRpenw/m.vcLength), (m.cuRcol+m.boXwdth)- ;
   (m.cuRpenw/m.hcLength) PATTERN m.paTtern COLOR RGB(m.faCer,m.faCeg, ;
   m.faCeb,m.faCer,m.faCeg,m.faCeb)
 IF m.ouT_on
      @ m.cuRrow, m.cuRcol TO m.cuRrow+m.boXhgt, m.cuRcol+m.boXwdth STYLE  ;
        '1' PATTERN 0 PEN 0, m.peNtype COLOR RGB(m.ouTliner,m.ouTlineg, ;
        m.ouTlineb,m.ouTliner,m.ouTlineg,m.ouTlineb)
 ENDIF
 SET DECIMALS TO &m.setdec
 RETURN
ENDFUNC
*
PROCEDURE BOXDRAW
 @ m.boXrow, m.boXcol TO m.boXrow, m.boXcol+(m.boXwdth-(((((2*m.coUnter)- ;
   1)*m.prEvpen)-(m.prEvpen-m.loOppen))/m.hcLength)) STYLE '0' PATTERN 0  ;
   PEN m.loOppen COLOR RGB(m.suNr,m.suNg,m.suNb,m.suNr,m.suNg,m.suNb)
 DO trIangles WITH m.boXrow, m.boXcol+(m.boXwdth-(((((2*m.coUnter)-1)* ;
    m.prEvpen)-(m.prEvpen-m.loOppen))/m.hcLength))
 @ m.boXrow+(m.loOppen/m.vcLength), m.boXcol TO m.boXrow+(m.boXhgt-(((((2* ;
   m.coUnter)-1)*m.prEvpen)-(m.prEvpen-m.loOppen))/m.vcLength)), m.boXcol  ;
   STYLE '0' PATTERN 0 PEN m.loOppen COLOR RGB(m.suNr,m.suNg,m.suNb, ;
   m.suNr,m.suNg,m.suNb)
 @ m.boXrow+(m.loOppen/m.vcLength), m.boXcol+(m.boXwdth-(((((2*m.coUnter)- ;
   1)*m.prEvpen)-(m.prEvpen-m.loOppen))/m.hcLength)) TO m.boXrow+m.boXhgt- ;
   ((((m.coUnter-1)*2)*m.prEvpen)/m.vcLength), m.boXcol+(m.boXwdth-(((((2* ;
   m.coUnter)-1)*m.prEvpen)-(m.prEvpen-m.loOppen))/m.hcLength)) STYLE '0'  ;
   PATTERN 0 PEN m.loOppen COLOR RGB(m.shAder,m.shAdeg,m.shAdeb,m.shAder, ;
   m.shAdeg,m.shAdeb)
 DO trIangles WITH m.boXrow+(m.boXhgt-(((((2*m.coUnter)-1)*m.prEvpen)- ;
    (m.prEvpen-m.loOppen))/m.vcLength)), m.boXcol
 @ m.boXrow+(m.boXhgt-(((((2*m.coUnter)-1)*m.prEvpen)-(m.prEvpen- ;
   m.loOppen))/m.vcLength)), m.boXcol+(m.loOppen/m.hcLength) TO m.boXrow+ ;
   (m.boXhgt-(((((2*m.coUnter)-1)*m.prEvpen)-(m.prEvpen-m.loOppen))/ ;
   m.vcLength)), m.boXcol+(m.boXwdth-(((((2*m.coUnter)-1)*m.prEvpen)- ;
   (m.prEvpen-m.loOppen))/m.hcLength)) STYLE '0' PATTERN 0 PEN m.loOppen  ;
   COLOR RGB(m.shAder,m.shAdeg,m.shAdeb,m.shAder,m.shAdeg,m.shAdeb)
 RETURN
ENDPROC
*
PROCEDURE TRIANGLES
 PARAMETER m.roW, m.coL
 FOR mcOunter = 1 TO m.loOppen
      @ m.roW+((mcOunter-1)/m.vcLength), m.coL TO m.roW+((mcOunter-1)/ ;
        m.vcLength), m.coL+((m.loOppen-mcOunter)/m.hcLength) STYLE '0'  ;
        PATTERN 0 PEN 1 COLOR RGB(m.suNr,m.suNg,m.suNb)
      @ m.roW+((mcOunter-1)/m.vcLength), m.coL+((m.loOppen-mcOunter)/ ;
        m.hcLength) TO m.roW+((mcOunter-1)/m.vcLength), m.coL+(m.loOppen/ ;
        m.hcLength) STYLE '0' PATTERN 0 PEN 1 COLOR RGB(m.shAder,m.shAdeg, ;
        m.shAdeb)
 ENDFOR
 RETURN
ENDPROC
*
