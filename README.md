# Convert To VFP
 The Visual FoxPro (VFP) Table Converter is a C# desktop application that translates C# class structures into Visual FoxPro scripts. 
 Its graphical interface allows users to select a C# file, from which it generates a corresponding VFP script based on the class structure within the file. 
 This script can be saved as a .prg file.  The application also has a feature for locating and selecting the VFP6 executable, which is used to run the generated scripts. 
 The scripts are executed with administrator privileges directly within the application. 
 The main function of these scripts is to create a new table in a specified database that reflects the structure of the input C# class file.  
 Additionally, the application provides a feature to regenerate scripts based on the current content of the text box displaying the C# code meaning you can just copy / paste classes.
