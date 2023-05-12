local lcFromFolder, lcToFolder, lcOldSafe, lcDBCFullPathName, lcDbcJustFileName, ;
   lnAnswer, lnDialogType, lcRunCommand, lcDbcDestinationFullName

lcRunCommand = ""
lcFromFolder = ""
lcToFolder = ""
lnDialogType = 4 + 32 + 256
*  4 = Yes and No buttons
*  32 = Question mark icon
*  256 = Second button is default
lnAnswer = 6 && "yes"
lcDbcJustFileName = ""
lcDbcDestinationFullName = ""
lcDBC = ""
lcOldSafe = Set("safety")
clos data all
lcToFolder = getdir("", "Destination for Core metas")
lcFromFolder = "Y:\DEVELOPMENT\DBC\"
If  Empty(lcToFolder)
 *lcFromFolder = getdir("", "Source of Core metas")
Endif
If Not Empty(lcFromFolder )
 wait 'Which DBC is source?' window nowait noclear
 lcDBCFullPathName = GETFILE('DBC', 'Which DBC is source?', 'Copy', 0 ,'Copy')
 wait clear
 lcDbcJustFileName = justStem(lcDBCFullPathName)+".*"
 lcDBCFullPathName = StrTran(lcDBCFullPathName , ".dbc" , ".*" )
Endif

set safe off

If lcToFolder = "Y:\DEVELOPMENT\DBC\"
      = MessageBox('Destination cannot be "Y:\DEVELOPMENT\DBC\" .')
      return
Endif

If Not Empty(lcToFolder) AND Not Empty(lcFromFolder) AND Not Empty(lcDBCFullPathName)
   If lcToFolder == lcFromFolder
      = MessageBox("Source and Destination cannot be the same.")
   Else && folders not the same
      If file(lcToFolder+"coremeta.dbf")
         lnAnswer = MESSAGEBOX("Overwrite the old core files in "+lcToFolder, lnDialogType, "Overwrite?")
      Else
         lnAnswer = MESSAGEBOX("Copy from "+lcFromFolder+" to "+lcToFolder+"?", lnDialogType, "Copy")
      Endif
      If File(lcFromFolder+"coremeta.dbf")
         If lnAnswer = 6
            copy file lcFromFolder+"coremeta.*" to lcToFolder+"coremeta.*"
            copy file lcFromFolder+"DbcXReg.*" to lcToFolder+"DbcXReg.*"
            copy file lcFromFolder+"SDTmeta.*" to lcToFolder+"SDTmeta.*"
            
			lcDbcDestinationFullName = lcToFolder + lcDbcJustFileName
            copy file (lcDBCFullPathName) to (lcDbcDestinationFullName)
            
	            lcDBCFullPathName  = strtran(lcDBCFullPathName  , ".dbc" , ".dct")
	            lcDbcJustFileName = strtran(lcDbcJustFileName  , ".dbc" , ".dct")
				lcDbcDestinationFullName = lcToFolder + lcDbcJustFileName
	            copy file (lcDBCFullPathName) to (lcDbcDestinationFullName)
	            
	            lcDBCFullPathName  = strtran(lcDBCFullPathName  , ".dct" , ".dcx")
	            lcDbcJustFileName = strtran(lcDbcJustFileName  , ".dct" , ".dcx")
				lcDbcDestinationFullName = lcToFolder + lcDbcJustFileName
	            copy file (lcDBCFullPathName) to (lcDbcDestinationFullName)
            
            lcRunCommand = "run  attrib -r "+lcToFolder+lcDbcJustFileName+".*"
            &lcRunCommand
            
            lcRunCommand = "run  attrib -r "+lcToFolder+"coremeta.*"
            &lcRunCommand

            lcRunCommand = "run  attrib -r "+lcToFolder+"DbcXReg.*"
            &lcRunCommand

            lcRunCommand = "run  attrib -r "+lcToFolder+"SDTmeta.*"
            &lcRunCommand
            
         Endif
      Else
         =messagebox('Specified folder "'+lcFromFolder+'" does not contain core-meta files.')
      Endif
   Endif
Else
   =messagebox('One or more folders/files not Specified correctly.')
Endif && Not Empty(lcToFolder) AND Not Empty(lcFromFolder) AND Not Empty(lcDBCFullPathName)

set safety &lcOldSafe
