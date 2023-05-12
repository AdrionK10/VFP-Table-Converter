*     Program: AB.PRG (Array Browser)
*            :
*   Architect: Whil Hentzen
*     Contact: Hentzenwerke
*            : PO Box 17343
*            : Milwaukee WI 53217-0343
*            : U.S.A.
*            : Voice: 414.332.9876
*            : Fax: 414.963.4999
*            : CompuServe: 70651,2270
*            :
*   Copyright: (c) 1994 Hentzenwerke
*            : 
*            : This program is provided "as is" without warranty 
*            : of any kind, expressed or implied.  IN NO EVENT 
*            : SHALL ITS AUTHORS, CONTRIBUTORS, OR DISTRIBUTORS 
*            : BE LIABLE FOR ANY COMMERCIAL, SPECIAL, INCIDENTAL, 
*            : OR CONSEQUENTIAL DAMAGES.
*            :
*   Hierarchy: Standalone function
*            :
*     Purpose: Display the contents of an array in a Browse format
*            :
*     Version: 1.0
*            :
*   Platforms: FoxPro 2.5a/D/W
*            :
*      Syntax: =ab( <c ArrayName> )
*            :
*       Parms: <c ArrayName> text string of array name
*            :
*     Samples: =ab("aDealers")
*            :  
*      Return: nothing
*            :
*       Files: None
*    Required: 
*            : 
*       Files: _ZYXWVUTS.DBF
*     Created: If array name is not found, _ZYXWVUTS.RID, _ZYXWVUTS.TXT
*            : both are deleted if the function terminates normally
*            :
*       Notes: Function returns error message if no parm is passed
*            : 
*            : Function looks for arrays in memory (via DISP MEMO)
*            : if parm passed is not an array and presents a list 
*            : of available arrays choose from
*            : 
*      Hidden: None
*   Knowledge: 
*            : 
*      Thanks: Frank Bosso, Geary Rachel, Vince Rice, 
*  (Testing &: Ceil Rosenfeld, Rick Strahl, Norm Strawser, 
*Suggestions): Randy Wallin
*            : 
*         ERs: Write edits to browse window back to array (gfr, rw) 
*            : Filter parm for browse window (vr)
*            : Keep browser open & updated during program execution (cr)
*            : Enable sysmenu (vr)
*            : Browse two arrays at same time (gfr)
*            : 

para m.jcX
priv m.c, m.r, m.i
priv all like j*

m.jcCreaStr = "crea curs ZXYWVUTS ("

if set("talk") == "ON"
 set talk off
 m.jlTalkIsOn = .T.
else
 m.jlTalkIsOn = .F.
endi

*m.jcCurWA = alia()
m.jcCurWA = select(0) && d.f. 5/10/00

if para() < 1
 wait wind "Helllllppppp mmmmmeeeeeee - I need an array name..."
else
 *
 * test to see that the parm is really an array
 *
 m.jcOldErrHandler = on("ERROR")
 m.jlArrIsGood = .T. 
 *
 * set up error handler that will look for array names in case
 * the passed parm isn't one
 *
 *on erro do TRAPBADA 
 on error do TRAPBADA WITH ERROR() && 5/10/00 D.F. CHANGED TO PASS ARG TO ERRORHAND: "TRAPBADA"
 *
 * these functions will determine if the parm is an array
 * if the parm isn't the error handler is sprung
 *
 m.jnRows = alen(&jcX,1)
 m.jnCols = alen(&jcX,2)
 *
 * now that we're here, 
 * - we have a good array, either because the user
 *   - gave us a good one, or 
 *   - picked a good one from the list provided in the error handler
 * OR
 * - we set the flag to exit 
 *
 * before we do anything else, let's set things back the way they were
 *
 if !empt(m.jcOldErrHandler)
  on erro &jcOldErrHandler
 else
  on erro
 endi
 *
 * if we have a good array
 *
 if m.jlArrIsGood
  *
  * let's provide a little visual feedback to the user
  *
  m.jnTimeBegin = seco()
  wait wind nowa "Array browser..."
  *
  * we need to do this again in case the user had picked an 
  * array from the error handler
  *
  m.jnRows = alen(&jcX,1)
  m.jnCols = alen(&jcX,2)
  *
  * alen,2 returns 0 if the array is one dimensional
  *
  * we're going to create a cursor from the array and then
  * browse the cursor
  *
  * we have to know how to create the cursor columns - type,
  * width and # of decimals - so we're going to examine each
  * element in the array and determine what the cursor type,
  * width and decimals are
  *
  if m.jnCols = 0
   *
   * array is one dimensional
   *
   * we want m.jnCols to reflect how many columns - even if just 1
   *
   m.jnCols = 1
   *
   * initialize arrays that store the cursor column type, width & decimals
   *
   decl jaColType[1]
   jaColType[1] = type("&jcX[1]")
   decl jaColWidth[1]
   jaColWidth = 1
   decl jaColDec[1]
   jaColDec = 0
   jnDecCurEl = 0
   *
   * go through each row and determine the type, width & decimals
   *
   for r = 1 to m.jnRows
    *
    * we're checking for the situation where the elements in
    * a single column are not all of the same type - if this
    * happens, we're going to make the entire column Char
    * and convert the other data types 
    *
    * if the type of the current row differs from the type
    * of the first row, make it Char
    *
    if ! type("&jcX[r]") = jaColType[1]
     jaColType[1] = "C"
    endi
    *
    * now check to see how wide the column must be
    *
    do case
    case type("&jcX[r]") = "C"
     m.jnWidthCurEl = len(&jcX[r])
    case type("&jcX[r]") = "N"
     m.jnWidthCurEl = len(allt(str(int(&jcX[r]))))
     *
     * note the call to the NUMDEC function
     *
     m.jnDecCurEl = NumDec(&jcX[r])
    case type("&jcX[r]") = "D"
     if set("CENT") = "ON"
      m.jnWidthCurEl = 10
     else
      m.jnWidthCurEl = 8
     endi
    case type("&jcX[r]") = "L"
     m.jnWidthCurEl = 1
    endc
    *
    * compare this row's numbers with the "master" numbers
    * and take the largest
    *
    jaColWidth[1] = max( jaColWidth[1], m.jnWidthCurEl )
    jaColDec[1]   = max( jaColDec[1], m.jnDecCurEl )
   endf
   *
   * up to here, the Column Width was the number of places to the
   * left of the decimal and we have ignored the point
   *
   * I.E. if the number = 123.45678, aColWidth = 3 & aColDec = 5
   * but we want to pass 9,5 to the CREA CURS command
   *
   jaColWidth[1] = jaColWidth[1] + jaColDec[1] + 1
   *
  else
   *
   * array is two dimensional
   *
   * examine each column
   * - what is the max width?
   * - are the contents the same all the way down?
   *
   decl jaColType[m.jnCols]
   jaColType = "C"
   decl jaColWidth[m.jnCols]
   jaColWidth = 1
   decl jaColDec[m.jnCols]
   jaColDec = 0
   *
   for c = 1 to m.jnCols
    *
    wait wind nowa "Array browser (" + allt(str(m.jnCols - c)) + ")"
    *
    * for each column, set a default Col Type and Width 
    * then look at each row in the column and see if they change
    *
    jaColType[c] = type("&jcX[1,c]")
    jaColWidth[c] = 1
    jaColDec[c] = 0
    jnDecCurEl = 0
    for r = 1 to m.jnRows
     *
     * we're checking for the situation where the elements in
     * a single column are not all of the same type - if this
     * happens, we're going to make the entire column Char
     * and convert the other data types 
     *
     * if the type of the current row differs from the type
     * of the first row, make it Char
     *
     * note that we can't just bail if we find out the column 
     * contents varies because we have to check the width
     *
     * to make the code simpler, even after we know that the 
     * column has to be Char, we'll continue to process this 
     * IF for the rest of the rows in the array
     *
     if ! type("&jcX[r,c]") = jaColType[c]
      *
      jaColType[c] = "C"
      *
      * if we are flipping the type, then make sure this column width
      * is at least 10 so that numerics don't get truncated when they
      * get appended
      *
      jaColWidth[c] = max( jaColWidth[c], 10 )
     endi
     *
     * get the larger of the default width and the width of this element
     * 
     do case
     case type("&jcX[r,c]") = "C"
      m.jnWidthCurEl = len(&jcX[r,c])
     case type("&jcX[r,c]") = "N"
      m.jnWidthCurEl = len(allt(str(int(&jcX[r,c]))))
      *
      * note the call to the NUMDEC function
      *
      m.jnDecCurEl = NumDec(&jcX[r,c])
     case type("&jcX[r,c]") = "D"
      if set("CENT") = "ON"
       m.jnWidthCurEl = 10
      else
       m.jnWidthCurEl = 8
      endi
     case type("&jcX[r,c]") = "L"
      m.jnWidthCurEl = 1
     endc
     *
     * compare this row's numbers with the "master" numbers
     * and take the largest
     *
     jaColWidth[c] = max( jaColWidth[c], m.jnWidthCurEl )
     jaColWidth[c] = min( jaColWidth[c], 253 )
     jaColDec[c]   = max( jaColDec[c], m.jnDecCurEl )
    endf
    *
    * let's handle the "real" column width and decimal as above
    *
    jaColWidth[c] = jaColWidth[c] + jaColDec[c] + 1
   endf
  endi && one or two dim
  *
  *
  * now that we're done with ColType, ColWidth and ColDec,
  * create the SQL CREATE command string
  *
  * note that the width = ColWidth + ',' + ColDec - even if char!
  *
  m.jcCreaStr = "crea curs ZXYWVUTS ("
  m.jcStr = ""
  *
  * this FOR gets processed for each field
  *
  for i = 1 to m.jnCols
   *
   * the name, type, and beginning parens of length
   *
   m.jcStr = m.jcStr ;
             + " Col_" ;
             + padl(allt(str(m.i)),2,"0") ;
             + " " ;
             + jaColType[i] ;
             + "("
   *
   * the length
   *
   if jaColType[i] = "N"
    * the entire column is numeric
    m.jcStr = m.jcStr + allt(str( jaColWidth[i] )) + "," + allt(str( jaColDec[i] ))
   else
    * for C, D, L
    m.jcStr = m.jcStr + allt(str( jaColWidth[i] + jaColDec[i] ))
   endi
   *
   * the end parens of the length
   *
   m.jcStr = m.jcStr + ")"
   *
   * if not at last column, add a comma
   *
   if i != m.jnCols
    m.jcStr = m.jcStr + ", "
   endi
  endf
  rele i
  *
  * close parens to finish the CREATE
  *
  m.jcCreaStr = m.jcCreaStr + m.jcStr + ")"
  *
  * create the cursor
  *
  &jcCreaStr
  *
  * the cursor is created - now pull the data from the array
  * into the cursor
  *
  if m.jnCols = 1
   *
   * APPE FROM wants to add a 1 dim array as a single record with
   * many fields, but we want many records of 1 field each
   *
   for i = 1 to m.jnRows
    appe blan
    *
    * since we are not doing the automatic "appe from" that handles
    * the typing, we have to go through this CASE mess...
    *
    do case
    case type("COL_01") = "C"
     do case
     case type("&jcX[i]") = "C"
      repl COL_01 with &jcX[i]
     case type("&jcX[i]") = "N"
      repl COL_01 with str(&jcX[i], jaColWidth[1], jaColDec[1])
     case type("&jcX[i]") = "D"
      repl COL_01 with dtoc(&jcX[i])
     case type("&jcX[i]") = "L"
      repl COL_01 with iif(&jcX[i], ".T.", ".F.")
     endc
    othe
      repl COL_01 with &jcX[i]
    endc
   endf
  else
   *
   * two dimensional array can use APPE FROM
   *
   appe from arra &jcX
   go top
  endi
  *
  * figure how wide the Browse window should be
  *
  * start out with 6 for the first column and 1 for each margin
  *
  m.jnBrWinSize = 8
  *
  * we're in the cursor so we're just going to FSIZE each field
  *
  for i = 1 to m.jnCols
   m.jnBrWinSize = m.jnBrWinSize + max( fsize(field(i)) + 1, 7)
  endf
  rele i
  *
  * don't build a browse window that's bigger than the screen
  *
  defi wind wArrBrow from 0,0 to ;
   min(m.jnRows+8,srows()), min(m.jnBrWinSize,scols()-1) ;
   syst clos floa grow zoom mini
  *
  * create the field list, including the row number in the
  * first column
  *
  m.jcFldList = "RowNo=recn():5:H='Row #',"
  for i = 1 to m.jnCols
   m.jcFldList = m.jcFldList ;
                 + " Col_" ;
                 + padl(allt(str(m.i)),2,"0") ;
                 + " " 
   *
   * put the comma after every field, except the last one
   *
   if ! (i = m.jnCols)
    m.jcFldList = m.jcFldList + ","
   endi
  endf  
  *
  * get ready - here's the moment we've all been waiting for...
  *
  wait wind nowa "Array Browser... " ;
   + allt(str(seco() - m.jnTimeBegin,5,3)) + " seconds"
  brow fiel &jcFldList titl "Array: "+jcX colo sche 10 wind wArrBrow
  rele wind wArrBrow
 endi && m.jlArrIsGood
endi && para() < 1
*
* be well-behaved - put everything back the way we found it
*
*-* Commented Out 05/10/2000 01:13:54 PM
*-* if !empt(m.jcCurWA)
*-*  sele &jcCurWA
*-* endi

select (jcCurWA) && d.f. 5/10/00

if m.jlTalkIsOn
 set talk on
endi
retu .T.



*    Function: NUMDEC (Number of Decimals)
*      Syntax: <n Number of Decimals> = numdec( <n AnyNumber> )
*            :
*       Parms: <n AnyNumber> a number (numeric type)
*            :
*     Samples: m.jnDecCurEl = NumDec(m.jnOrigNum)
*            :  
*      Return: number of decimals places in a number
*
func NumDec
para m.jnX
priv m.jcWholeNum, m.jnDecLoc, m.jcDecPart, m.jnLenDec
*
* convert to a string
*
m.jcWholeNum = str(m.jnX, 18, 18)
*
* locate the decimal
*
m.jnDecLoc = at(".", m.jcWholeNum)
*
* strip just the decimal part (including the point)
*
m.jcDecPart = subs(m.jcWholeNum, m.jnDecLoc, 18)
*
* how long is the decimal part (not incl the point)
*
m.jnLenDec = len(m.jcDecPart) - 1
*
* find the trailing zeros
*
do whil righ(m.jcDecPart, 1) = "0"
 m.jnLenDec = m.jnLenDec - 1
 m.jcDecPart = left(m.jcDecPart, m.jnLenDec + 1)
endd
retu m.jnLenDec

*    Function: TRAPBADA (Trap Bad Array)
*      Syntax: do TRAPBADA
*            :
*       Parms: none
*            :
*     Samples: on erro do TRAPBADA
*            :  
*      Return: places an array name in the existing m.jcX memvar
*
func TrapBadA
PARAMETER tError
on error && default

*if error() = 232 or error() = 12 && 5/10/00 d.f. changed to use passed arg
if tError = 232 or tError = 12

 *
 * error 12: "variable" not found
 * error 232: "name" is not an array
 *
 wait wind nowa "The array " + uppe(allt(m.jcX)) + " does not exist. Finding current arrays..."
 *
 * since the parm passed isn't a valid array name, we're going to 
 * present the user with a list
 *
 * list memory to a text file and bring it into a temp table
 *
 list memo to _ZYXWVUTS.TXT noco
 
*-* Commented Out 05/10/2000 11:27:10 AM
*-*  crea tabl    _ZYXWVUTS.RID ;
*-*   (;
*-*   cText C(70), ;
*-*   cArrName C(10) ;
 
 crea tabl    _ZYXWVUTS.RID free ;
  (;
  cText C(250), ;
  cArrName C(10) ;
  )
  
 appe from _ZYXWVUTS.TXT type sdf
 go top
 *
 * we're just going to look at every line in the LIST MEMO until we 
 * get to the "how many variables" message
 *
 * if we find a line with an array name (position 20 = 'A')
 * place that name into a second column in the temp table
 *
*-*  scan whil subs(_ZYXWVUTS.cText,7,9) <> "variables"
*-*   repl cArrName with left(_ZYXWVUTS.cText,10) ;
*-*    for subs(_ZYXWVUTS.cText,20,1) = "A"
*-*  ends
 
 scan whil not "variables" $ lower(_ZYXWVUTS.cText)
  if subs(_ZYXWVUTS.cText,84,1) = "A"
   repl cArrName with left(_ZYXWVUTS.cText,10)
  endif 
 ends
 
 *
 * if there weren't any arrays, set the "get outa town" flag and
 * alert the user
 *
 coun for !empt(cArrName) to m.jnNumFound
 if m.jnNumFound = 0
  m.jlArrIsGood = .f.
  wait wind "No arrays were found - Quitting Array Browser"
 else
  *
  * browse the array field name column of the temp table and
  * let the user select which array they want
  *
  * since this function may be used inside another program,
  * we're not going to reassign the Enter key
  *
  brow fiel cArrName:H="Possible Arrays":15 ;
   for !empt(cArrName) ;
   titl "Ctrl-W to Select"
  *
  * they have pressed Ctrl-W with the record pointer
  * on a particular array name
  *
  m.jcX = _ZYXWVUTS.cArrName
  *
  * we've put the desired array name back into m.jcX
  * now clean up and do what we came here for
  *
  wait wind nowa "You have chosen " + allt(m.jcX)
 endi
 use
 dele file _ZYXWVUTS.RID
 dele file _ZYXWVUTS.TXT
else
 *
 * we can't correctly handle any other type of error so we're just
 * going to set a flag that exists AB
 *
 on error
 wait wind "Error... quitting Array Browser..."
 m.jlArrIsGood = .F.
endi
on error do TRAPBADA WITH ERROR() && 5/10/00 D.F. CHANGED TO PASS ARG TO ERRORHAND: "TRAPBADA"
retu .t.
