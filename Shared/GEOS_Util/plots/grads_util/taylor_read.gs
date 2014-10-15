function tread (args)

expid        = subwrd (args,1)
name         = subwrd (args,2)
verification = subwrd (args,3)

filename = verification % "/taylor/" % expid % "_taylor_" % name % ".data"

season.1 = djf
season.2 = jja
season.3 = mam
season.4 = son
season.5 = ann

       n = 1
while( n<= 5 )
         'fixname 'expid
                   alias = result
          val1 = sublin( read(filename),2 )
          val2 = sublin( read(filename),2 )
           std = "std"  % season.n % alias
           cor = "corr" % season.n % alias
  'define 'std' = 'val1
  'define 'cor' = 'val2
       n = n+1
endwhile

close(filename)

return
