function formula (args)

'numargs  'args
 numargs = result

       n  = 1
while( n <= numargs )
       oname.n = subwrd(args,n)
       n  = n + 1
endwhile

* Formula:  swgnetc = swgdwnc - swgupc = swgdwnc - albedo*swgdwnc = swgdwnc*(1-albedo)
* ------------------------------------------------------------------------------------
say 'define qobs = 'oname.1' * (1-'oname.2')'
    'define qobs = 'oname.1' * (1-'oname.2')'

return
