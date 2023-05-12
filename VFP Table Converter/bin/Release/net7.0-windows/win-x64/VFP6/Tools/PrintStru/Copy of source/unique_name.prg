***
*** unique_name.fxp
***
*
*
FUNCTION UNIQUE_NAME
 LPARAMETERS l_Exclude_run
 l_Exclude_run = IIF((TYPE("L_EXCLUDE_RUN")=="L"), l_Exclude_run, .F.)
 RETURN (IIF((l_Exclude_run=.T.), SUBSTR(SYS(2015), 5, 6), SYS(2015)))
ENDFUNC
*
