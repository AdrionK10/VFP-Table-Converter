* FullName.prg  13-Jun-95

* Generate an object's full containership name

* 03-Dec-95 added additional validity testing
* 26-Apr-96 added testing to handle _VFP object which is its own .Parent

function FullName( roObject )

if ( ( type( "roObject" ) == 'O' ) and ! isnull( m.roObject ) )
   if ( ( type( "roObject.Parent" ) == 'O' ) and ;
        ( roObject.Name != "Microsoft Visual FoxPro" ) )
      return ( FullName( roObject.Parent ) + "." + roObject.Name )
   else
      return roObject.Name
   endif
else
   return ""
endif
