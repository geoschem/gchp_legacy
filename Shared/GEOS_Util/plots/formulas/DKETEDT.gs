function formula (args)

'numargs  'args
 numargs = result

       n  = 1
while( n <= numargs )
       qname.n = subwrd(args,n)
       n  = n + 1
endwhile

* Formula:  DKETEDT = kedyn + kephy + keana + tedyn + tephy + teana
* ----------------------------------------------------------------
say 'define 'qname.1' = 'qname.2' + 'qname.3' + 'qname.4' + 'qname.5' + 'qname.6' + 'qname.7
*   'define 'qname.1' = 'qname.2' + 'qname.3' + 'qname.4' + 'qname.5' + 'qname.6' + 'qname.7

'seasonalf -FUNCTION 'qname.2'+'qname.3'+'qname.4'+'qname.5'+'qname.6'+'qname.7' -NAME 'qname.1
 newfile = result

return newfile
