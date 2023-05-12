==================================
Reindexer for VFP 6.0 - Version 1.3
==================================

Contents
--------
* What is Reindexer?
* What's in Reindexer.ZIP
* How to run Reindexer
* Legal Stuff, etc.

-----------------
What is Reindexer?
-----------------
Reindexer is a VFP 6.0 class that allows your to build a 
Meta Data table that can later be used for rebuilding the 
indexs of a database container.

I created this because I need a way to clean up index files and 
make sure they could be rebuilt even if they were destroyed.
I use a system table that stores a logical field called Rebuild_Index
and if this field is .T. I run the Build_Index_Table() function 
from within my application. 
Now anytime I make changes to tables I just throw this flag and the 
next time a user logs in I get a new meta data table with the latest indexs.

See the About() method in the Classlib for more information.

Version 1.0 and above of the classlib was compiled in VFP6 SP3.

----------------------
What's in Reindexer.ZIP
----------------------
1. Reindexer.VCX/VCT - class library containing the class 
   definition for Reindexer
   
2. README.TXT - this file.

-------------------
How to run Reindexer
-------------------
Extract the contents of the ZIP file into the directory that
has the Database container and tables in it. From the 
VFP command window, CD to that directory and type the following:

     SET CLASSLIB TO Reindexer ADDITIVE
     oReindexer = CREATEOBJ([Indexer] [,DBC Name])
     
     oReindexer.Build_Index_Table() && Builds the meta data table
     oReindexer.Rebuild_Indexs() 	&& Deletes and reindexes the tables

---------------------
License and Copyright
---------------------
* Class......: Reindexer
*
* Version....: 1.3
*
* Author.....: Ronald L. Thorp
*              WWFF of South Florida, Inc.
*			   PO Box 98
*              Dania, FL 33004
*			   954-961-7747
*              Ron@WWFFofSouthFlorida.com
*              http://www.WWFFofSouthFlorida.com
*
* Date.......: December 29, 1999
*
* Abstract...: Rebuilds indexs for Tables in a DBC
*
* Copyright..: 1999, WWFF of Soouth Florida, Inc.
*
* License....: Freeware.
*
* Distribution: You may redistribute this class as long as
*              (a) you distribute all the files in their original
*              form, and (b) you do not charge anything for it.
*
* Warranty...: None. Distributed "AS IS". Always BACK UP DATA FIRST!
*
* Support....: None, but your comments and suggestions for 
*              improvements are welcome and I will answer questions 
*			   if time permits. Please e-mail me at
*              Ron@WWFFofSouthFlorida.com or reach me via the
*              Universal Thread at http://www.universalthread.com.
*              
* History....: 
*              January 4, 2000   - Version 1.3
*				 - When you delete the tag the relationships go also.
*				   This will now store and and rebuild the relations.
*				   I didn't catch this because I define the relations 
*				   internally.
*				   Thanks to a note from Ed Leafe pointing this out.
*				 - Added a new fields to the table for the relations
*				 - Now overwrites the IndexMetaData table
*              December 30, 1999 - Version 1.2
*				 - Now Trys to open the database exclusively and returns 
*				   False if it can't be opened.
*              December 30, 1999 - Version 1.1
*				 - Fixed spelling error
*				 - Fixed problem where closed the database 
*				   when you were building the metadata table
*				 - Added a Date Time field to the table (Requires the
*				   metadata table to be deleted.)
*              December 29, 1999 - Version 1.0
*		                 - initial release 
*                		 - compiled in VFP6 SP3 

