function formula (args)

'numargs  'args
 numargs = result

qname.1 = subwrd(args,1)

       n  = 2
while( n <= numargs )
       qname.n = subwrd(args,n)
       if( n = 2 )
           string  = qname.n
       else
           string  = string'+'qname.n
       endif
       n  = n + 1
endwhile

* Formula:  DQDTTOT = DQDT_MOIST + DQDT_TURB + DQDT_DYN [+ DQDT_ANA]
* ------------------------------------------------------------------
say 'define 'qname.1' = 'string

'seasonalf -FUNCTION 'string' -NAME 'qname.1
 newfile = result

return newfile
