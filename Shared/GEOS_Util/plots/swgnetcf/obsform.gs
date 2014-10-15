function formula (args)

'numargs  'args
 numargs = result

       n  = 1
while( n <= numargs )
       oname.n = subwrd(args,n)
       n  = n + 1
endwhile

* Formula:  swgnetcf = swgnetc - swgnet
* -------------------------------------
say 'define qobs = 'oname.1'*(1-'oname.2') - 'oname.3
    'define qobs = 'oname.1'*(1-'oname.2') - 'oname.3

return
