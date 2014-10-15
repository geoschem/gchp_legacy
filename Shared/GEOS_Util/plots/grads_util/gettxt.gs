  function gettxt (args)
  name = subwrd(args,1)
  tag  = subwrd(args,2)

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

* Check for VAR textfile
* ----------------------
    filename = var % ".txt"
   '!remove   grads.txt'
   '!listfile 'var'.txt > grads.txt'
    checkls = sublin ( read(grads.txt),2 )
                      close(grads.txt)
    if( checkls = filename )
      '!remove  grads.txt'
      '!grepfile 'filename' 'tag
        envar = sublin ( read(grads.txt),2 )
                        close(grads.txt)
      '!remove  grads.txt'
    else
        envar = NULL
    endif

* Remove TAG parameter from result
* --------------------------------
   if( tag != '' )
    'numargs 'envar
     num    = result
     result = ''
          n = 2
     while( n<=num )
       result = result' 'subwrd(envar,n)
            n = n+1
     endwhile
   else
     result = envar
   endif

return result
