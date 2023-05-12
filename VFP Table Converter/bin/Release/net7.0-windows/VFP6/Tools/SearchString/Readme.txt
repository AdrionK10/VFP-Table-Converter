The enclosed file "searchstring.app" is utility to search through the files contained in a project for a key phrase that you enter.
The form has a pageframe with two pages- one for filling in the phrase and the name of the project, and that contains the list of 
files contining the key phrase. The second page contains options for searching. 
To search a string, fill in the search string combo box, and specify a project in the project combo, and click Search!.

After the prgroam is finished searching, you can modify any of the files that it finds by double clicking its record in the grid.

The form uses the windows registry to remember projects and phrases that you have searched before. 


**************************
Revisions (1/20/99)- Several known bugs were fixed
	- Support for searching databases
	- form is now stretchable to show more files in the found list
	- found list is sortable by clicking on the grid headers

**************************


To make the app available in you VFP tools menu, add the following lines to your vfpstart program. (If you don't have a program that runs when you star Visual Foxpro, you can specify one in your Config.fpw
file with the following syntax: COMMAND = DO 'C:\Program files\VFP\VFPSTART.PRG')


DEFINE BAR 8 OF _MTOOLS PROMPT "\-"
DEFINE BAR 9 OF _MTOOLS PROMPT "Searc\<h a string..."
ON SELECTION BAR 9 OF _MTOOLS do k:\src\vfp50\searchstring\searchstring.app

_________________________________________________________________
This utility is hereby placed in the public domain by the author, Erik Moore.

