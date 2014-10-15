function formula (args)

'numargs  'args
 numargs = result

       n  = 1
while( n <= numargs )
       qname.n = subwrd(args,n)
       n  = n + 1
endwhile

* Formula:  lwgup = sfcem + lws*(1-emis)/emis
* -------------------------------------------
say 'define qmod = 'qname.1' + 'qname.3'*(1-'qname.2')/('qname.2')'
    'define qmod = 'qname.1' + 'qname.3'*(1-'qname.2')/('qname.2')'

return
