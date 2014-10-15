function twrite (args)

expid  = subwrd (args,1)
name   = subwrd (args,2)
output = subwrd (args,3)

           filename = output % "/" % expid % "_taylor_" % name % ".data"
'!/bin/rm 'filename

season.1 = djf
season.2 = jja
season.3 = mam
season.4 = son
season.5 = ann

       n = 1
while( n<= 5 )
         'fixname 'expid
                   alias = result
           std = "std"  % season.n % alias
           cor = "corr" % season.n % alias
       'd 'std
           val1 = subwrd(result,4)
       'd 'cor
           val2 = subwrd(result,4)
   write  (filename,val1)
   write  (filename,val2)
       n = n+1
endwhile

return
