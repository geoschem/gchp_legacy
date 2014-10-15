function formula (args)

'numargs  'args
 numargs = result

       n  = 1
while( n <= numargs )
       qname.n = subwrd(args,n)
       n  = n + 1
endwhile

* Formula:  SWGNETCAF = rscs - rscsna
* ----------------------------------------------------------------
say 'define 'qname.1' = ('qname.2' - 'qname.3')'
*   'define 'qname.1' = ('qname.2' - 'qname.3')'

'seasonalf -FUNCTION 'qname.2'-'qname.3' -NAME 'qname.1
 newfile = result

return newfile
