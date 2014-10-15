function formula (args)

'numargs  'args
 numargs = result

       n  = 1
while( n <= numargs )
       qname.n = subwrd(args,n)
       n  = n + 1
endwhile

* Formula:  KEGEN_DIFF1 = kedyn + tedyn - convke - convphi + kegen
* ----------------------------------------------------------------
say 'define 'qname.1' = 'qname.2' + 'qname.3' - 'qname.4' - 'qname.5' - 'qname.6
*   'define 'qname.1' = 'qname.2' + 'qname.3' - 'qname.4' - 'qname.5' - 'qname.6

'seasonalf -FUNCTION 'qname.2'+'qname.3'-'qname.4'-'qname.5'-'qname.6' -NAME 'qname.1
 newfile = result

return newfile
