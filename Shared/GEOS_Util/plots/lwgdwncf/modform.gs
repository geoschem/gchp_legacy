function formula (args)

'numargs  'args
 numargs = result

       n  = 1
while( n <= numargs )
       qname.n = subwrd(args,n)
       n  = n + 1
endwhile

* Formula:  lwgdwncf = lws/emis - lcs/emis
* ------------------------------------------
say 'define qmod = ('qname.1' - 'qname.2')/'qname.3
    'define qmod = ('qname.1' - 'qname.2')/'qname.3

return
