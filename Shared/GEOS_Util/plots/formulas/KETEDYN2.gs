function formula (args)

'numargs  'args
 numargs = result

       n  = 1
while( n <= numargs )
       qname.n = subwrd(args,n)
       n  = n + 1
endwhile

* Formula:  KETEDYN2 = ( kegen + convphi + convke )
* ----------------------------------------------------------------
say 'define 'qname.1' = smth9('qname.2' + 'qname.3' + 'qname.4')'
*   'define dum = 'qname.2' + 'qname.3' + 'qname.4

'seasonalf -FUNCTION 'qname.2'+'qname.3'+'qname.4' -NAME 'qname.1
 newfile = result

*       n  = 7
*       n  = 0
*       m  = 1
*while( m <= n )
*      'define dum = smth9(dum)'
*       m  = m + 1
*endwhile
*    'define 'qname.1' = dum'

return newfile
