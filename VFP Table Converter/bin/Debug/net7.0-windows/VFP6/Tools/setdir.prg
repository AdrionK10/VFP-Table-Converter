lparameter m.skipform
set dele on
if m.skipform
	cd c:\dev\pp7.5
	_screen.backcolor = rgb(200,160,160)
else
	*do form home() + 'setdir' with 4 to m.result && "C:\PROGRAM FILES\MICROSOFT VISUAL STUDIO 7\VFP7\TOOLS\
	*do form  setdir   to m.result
	do form "C:\PROGRAM FILES\MICROSOFT VISUAL STUDIO 7\VFP7\TOOLS\SetDir" to m.result
	if NOT m.result
		return
	endif
endif

close all
clear all
clear program
clear
_screen.caption = "VFP6 " + sys(5) + sys(2003)
if file('setpath.prg')
	do setpath
else
	wait window nowait "No SetPath found"
endif

public gcusername
gcusername = "PDS"
if file('calendar.dll')
	declare INTEGER ShowCalendar in Calendar STRING BegDt, STRING EndDt,;
	STRING BegDtTitle, STRING EndDtTitle, STRING WndTitle
endif
set status bar on
set excl off