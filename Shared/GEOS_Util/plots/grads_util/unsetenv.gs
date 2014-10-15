function  unsetenv (args)
    name  = subwrd (args,1)

* Create VAR between quotes as Text File
* --------------------------------------
       last = substr(name,1,1)
       if( last = '"' )
            nmax = 200
               n = 2
       while ( n<max )
       last = substr(name,n,1)
       if( last = '"' )
           len = n-2
             n = max
       else
             n = n+1
       endif
       endwhile
       var = substr( name,2,len )
       else
       var = name
       endif

          filename = var % ".txt"
'!remove 'filename

return
