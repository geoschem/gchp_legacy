function formula (args)

'numargs  'args
 numargs = result

       n  = 1
while( n <= numargs )
       qname.n = subwrd(args,n)
       n  = n + 1
endwhile

* Formula:  KETEPHY = kephy + tephy
* ----------------------------------------------------------------
say 'define 'qname.1' = 'qname.2' * 1.00E+06'
*   'define 'qname.1' = 'qname.2' * 1.00E+06'

'seasonalf -FUNCTION 'qname.2'*1.00E+06' -NAME 'qname.1
 newfile = result

return newfile
