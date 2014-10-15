function formula (args)

'numargs  'args
 numargs = result

       n  = 1
while( n <= numargs )
       qname.n = subwrd(args,n)
       n  = n + 1
endwhile

* Formula:  CONVKE = convke
* ----------------------------------------------------------------
say 'define 'qname.1' = 'qname.2
*   'define 'qname.1' = 'qname.2

'seasonalf -FUNCTION 'qname.2' -NAME 'qname.1
 newfile = result

*       n  = 10
*       n  = 0
*       m  = 1
*while( m <= n )
*      'define 'qname.1' = smth9('qname.1')'
*       m  = m + 1
*endwhile

return newfile
