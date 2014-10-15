function getlevs (args)

            name = subwrd(args,1)
'lowercase 'name
            name = result

'getinfo nvars'
         nvars = result
'query file'

    nlev = NULL
  offset = 6
       n = 1
while( n <= nvars )
       rec  = n + offset
       line = sublin(result,rec)
       vars = subwrd(line,1)
       levs = subwrd(line,2)
       if( vars = name )
           if( levs = 0 ) ; levs = 1 ; endif
           nlev = levs
       endif
       n = n + 1
endwhile
return nlev
