*************************************************************************
*			IndexBreaker.PRG      -     Table Stress Tester
*           Created by Bob Cassady, based on an original program by
*           Cam MacKenzie    May 1999
*************************************************************************

*---------------------------------------------------------------------------
* Run 8 copies of the executable at the same time (or maybe fewer) with the
* data in the same directory as IndexBreaker.exe.  This should take about 20
* minutes at 166 MHz.
*---------------------------------------------------------------------------
* Afterwards (under VFP) type in USE TXMESS
*                                DO INDEXTESTER
* for index corruption results.  (You will normally get some)
*---------------------------------------------------------------------------

*---------------------------- Buffering Fix --------------------------------
* UnComment the "# Add for Buffering" lines below and recompile to run with
* Buffered tables.  Should stop any index corruption.
*---------------------------------------------------------------------------

*---------------------------- Last Record Fix ------------------------------
* UnComment the "# Block last record recycling" and "# Block deleting last
* record" lines below and use the "Allow" equivalents, then recompile.  
* Should stop any index corruption.
*---------------------------------------------------------------------------

#define MAX_RECORDS_INSERTED	6000	&& quit after this many insertions
#define INSERTION_PROBABILITY	0.48	&& probability of insertion vs deletion
                                        && to simulate a real system
set reprocess  to automatic
set deleted    off
* set multilocks on               && # Add for Buffering
on escape do CloseAllAndQuit

open database IndexBreaker shared
use TxMess 	shared in 0
* cursorsetprop("Buffering",3)    && # Add for Buffering

HitTable()  && hammer the table until hit escape

*///////////////////////////////////////////////////////////////////////////////
function HitTable()
* randomly insert and delete records from the requested table
* probability of insertion and deletion is equal

local lnMessageCounter
  
lnMessageCounter = 0
  
do while (.T.)
  if rand() <= INSERTION_PROBABILITY
    CreateTxmessRecord(rand() * 10, lnMessageCounter, 0, "MDT", "001",     ;
  	   					  "C ", "ABC", left("0123456789", 1 + (rand() * 9)))
    lnMessageCounter = lnMessageCounter + 1
  else
    DeleteRecord()
  endif
  	 
  if lnMessageCounter >= MAX_RECORDS_INSERTED
    CloseAllAndQuit()
  endif
enddo

*/////////////////////////////  Insertion Routines  ////////////////////////////
function CreateTxmessRecord(tnMessageID, tnMessageGroupId, tnSequenceNumber, ;
							tcDestProcessType, tcDestProcessId, ;
							tcFleet, tcVehicle, tcMessageInfoStr)
	  
if Recycler() < 0  && Don't recycle
  append blank
endif

replace TxMess.imessid    with tnMessageID      ,;
        TxMess.imessgrpid with tnMessageGroupID ,;
        TxMess.iseqnum    with tnSequenceNumber ,;
        TxMess.tmessgendt with datetime()       ,;
        TxMess.cfleet     with tcFleet          ,;
        TxMess.cvehicle   with tcVehicle        ,;
        TxMess.cdesttype  with tcDestProcessType,;
        TxMess.cdestid    with tcDestProcessID  ,;
        TxMess.cpart1     with tcMessageInfoStr ,;
        TxMess.ipart2ind  with 0                ,;
        TxMess.lDeleted   with .F.                 in TxMess
				
* tableupdate()          && # Add for Buffering
unlock

*///////////////////////////////////////////////////////////////////////////////
function DeleteRecord()

* delete a randomly selected record from the requested table
* depending upon the table either delete a record or flag it as deleted
local lnRecCount

  lnRecCount = reccount()      && # Allow deleting last record
* lnRecCount = reccount() - 1  && # Block deleting last record

if lnRecCount > 0
  go 1 + (rand() * lnRecCount)
  
  if lDeleted 
    locate for lDeleted = .F.
  endif
  if rlock()
    replace lDeleted with .T. in TxMess
*   tableupdate()     && # Add for Buffering
    unlock
  endif
endif

*//////////////////////////////////  Recycling  ////////////////////////////////
function Recycler()
  
local lnReturnedRecNo, lnOldReprocess, lnRecCount

lnReturnedRecNo = -1              && Default: Recyclable record not found

  lnRecCount = reccount()    	  && # Allow last record recycling
* lnRecCount = reccount() - 1	  && # Block last record recycling

if lnRecCount > 0                 && Only recycle if table is not empty
  lnOldReprocess = set("reprocess")
  set reprocess to 0 seconds      && each rlock() gets 1 and only 1 try

  for i = 1 to 5                  && # of tries before giving up on recycling
    go 1 + (rand() * lnRecCount)  && randomly select a record (for most records,
    if lDeleted and rlock()       && lDeleted = .t.)
      if lDeleted                 && It didn't changed while we were getting
        lnReturnedRecNo = recno() && the lock so we will send back it's
        exit                      && record # for recycling.
      else
        unlock record recno()     && unlock JUST this one (It was lDeleted, but
      endif                       && by the time we got the lock, it wasn't!)
    endif
  endfor
  set reprocess  to lnOldReprocess
endif

return lnReturnedRecNo

*///////////////////////////////////////////////////////////////////////////////
function CloseAllAndQuit()
close tables all
quit
