  function getresource (args)

  rcfile = subwrd(args,1)
  tag    = subwrd(args,2)
  envar  = NULL

* Extract VAR between quotes
* --------------------------
       last = substr(rcfile,1,1)
       if( last = '"' )
            nmax = 200
               n = 2
       while ( n<max )
       last = substr(rcfile,n,1)
       if( last = '"' )
           len = n-2
             n = max
       else
             n = n+1
       endif
       endwhile
       var = substr( rcfile,2,len )
       else
       var = rcfile
       endif

* Check for Existance of RCFILE
* -----------------------------
    filename = var
   '!remove   grads.txt'
   '!listfile 'var' > grads.txt'
    checkls = sublin ( read(grads.txt),2 )
                 rc = close(grads.txt)
    if( checkls = filename )
      '!remove  grads.txt'
      '!grepfile 'filename' 'tag

      '!getsize grads.txt'
        size = sublin ( read(size.txt),2 )
                  rc = close(size.txt)

        if( size != 0 ) 
        envar = sublin ( read(grads.txt),2 )
                   rc = close(grads.txt)
      '!remove  grads.txt'
      '!remove   size.txt'
        else
        envar = NULL
        endif
    else
        envar = NULL
    endif

* Check for Exact TAG parameter
* -----------------------------
   if( tag != '' & envar != 'NULL' )
           result  = subwrd(envar,1)
       if( result != tag":" )
           return NULL
       endif
   endif

* Remove TAG parameter from result
* --------------------------------
   if( tag != '' & envar != 'NULL' )
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

if( result != 'NULL' ) ; say 'Get_Resource: 'tag' = 'result ; endif
return result
