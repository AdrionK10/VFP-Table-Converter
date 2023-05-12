Program : eview.app
Purpose : View editing utility
Author: Erik Moore     esmoore@flash.net
Date: 12/22/98
Revised : 10/25/99
********************************************
Revisions to eView

11/17/99	- Moved the showplan editbox to  4th page to save screen real-estate.

10/25/99	- Added a window that displays VFP's SQL Showplan information for the view
		- Made several readonly fields that were tied to some view properties editable.
		  This opens the door for the developer to edit potentially sensitive properties 
		  that could break the view. I have implemented some limited checking that will
		  prevent really stupid mistakes, but beyond that, you're on your own. With power 
		  comes responsibility. :-) 
			(Some of this juggling was due to the bug mentioned a 
			  few revisions ago that caused VFP 5 to throw an error when opening a view whose 
			  RuleExpression or Ruletext properties had been set to "". IMPORTANT- If you use eView
			  to modify a view that needs to be opened by a VFP 5 application, this VFP bug WILL
			  cause you grief.)
		- eView is now compiled in VFP 6. This means it will no longer run under VFP 5.


07/12/99	- Added ability to call eview with a view name as a parameter.
			(Requested by Steve Dingle)

05/10/99	- Removed ability to change the view's RuleExpression and RuleText properties.
			This decision was made after learning that VFP 5 and 6 have a bug
			that makes it impossible to remove a view's RuleExpression and RuleText 
			properties without corrupting the view. Until the view can be regenerated,
			it gives an error 1584 when opened. 
	
05/01/99	-Fixed a bug that reversed the UPDATE property from the desired setting		
		-Added the ability to edit a view's RuleExpression and RuleText properties	
		-Added VCR buttons to the field properties page to allow scrolling through the fields
		-Added a "Set Key" button that finds the primary key for all base tables, 
			and sets the corresponding key fields in the view

********************************************		


One of the most obvious problems with the VFP development environment is the buggy view designer. The designer works well when creating and editing views that use two tables or fewer, but add a third table to the mix, and you are on your own. 

Luckily VFP also gives us CREATE SQL VIEW. This command lets us create a view in code, and never have to use the view designer to edit the view that we've created. This is fine, except that the SQL that creates the view is only a small part of the view definition. To make a view updatable, several properties must be set. The view designer is designed to take care of these properties for you in an intuitive way, but on views that are too complex to edit in the VFP designer, you have to fend for yourself. This is where eView comes in.

eView is designed to be a functional replacement for the native VFP view designer, as well as adding a few more features that the native designer doesn't provide.

eView allows you to:
- set the view as updatable or not
- edit the keyfield property for view fields
- edit the updatable property for view fields
- View each field's updatename property
- specify how the WHERE clause is constructed for updatable views
- specify whether rows are updated using UPDATE, or DELETE and then INSERT
- edit extended properties for view fields including:
	- Validation Rule Expression
	- Rule Text (Message)
	- Default Value
	- Caption
	- Format
	- Input mask
	- Comment
	- Display class
	- Display class library

- View and edit properties pertaining to Remote views including;
	- Fetch memo
	- Prepared
	- Offline
	- Share Connection
	- Compare memo
	- Fetch as needed
	- Memo size
	- Fetch size
	- Max records
	- Batch update count

- View and edit the SQL used to construct the view 
	If changes are made to the defining SQL, eView will redefine your view using the new SQL,
	and ask if you want to attempt to restore all previous view settings. 


In addition to these functions, eview has utilities to:
- Generate a prg that will completely recreate the view from scratch (this is the same as my GenView utility)
- Copy all field properties from underlying tables (ie caption, input mask, etc)

eView does NOT initially define your view. The native VFP view designer allows you to CREATE as well as edit views. eView does not. I personally create and maintain views with SQL, and I wrote eView to fill my needs. I elected not to include the added complexity of an SQL /View construction interface. Maybe someday in the future when I am really bored :-).


To use eView, just type DO eView.app in the command window. If a view is open and selected, eview will edit that view. If no view is open eview prompts you to choose a view from the current database. If no database is open, eview prompts you to open one, and then allows you to select a view from that database.

To automatically load eView into your VFP tools menu, place the following lines in a program that runs when VFP starts (you can specify a VFP startup program in your config.fpw file with 'COMMAND = DO VFPSTART.PRG'):

DEFINE BAR 8 OF _MTOOLS PROMPT "\-"
DEFINE BAR 9 OF _MTOOLS PROMPT "EView..."

ON SELECTION BAR 9 OF _MTOOLS do c:\vfp\tools\eview.app

This program is hereby placed in the public domain by its author, Erik Moore. You are authorized to use it free of charge. 
You are not authorized to distribute, sell, or decompile the program for the purpose of viewing or changing its source code. 

Any and all comments, criticisms and suggestions can be sent to Erik Moore at esmoore@texas.net


