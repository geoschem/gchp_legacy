  function getenv (args)
  name = subwrd(args,1)

* Extract VAR between quotes
* --------------------------
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


* Check for csh environment variable
* ----------------------------------
'!echo $'var' > envar.txt'
envar = sublin ( read(envar.txt),2 )

* Check for grads environment variable
* ------------------------------------
if( envar = "" | envar = "NULL" )
    filename = var % ".txt"
   '!remove   grads.txt' 
   '!listfile 'var'.txt > grads.txt'
    checkls = sublin ( read(grads.txt),2 )
    if( checkls = filename )
        envar = sublin ( read(filename),2 )
      '!remove  grads.txt' 
    else
        envar = NULL
    endif
endif

'!remove envar.txt' 
  return envar
