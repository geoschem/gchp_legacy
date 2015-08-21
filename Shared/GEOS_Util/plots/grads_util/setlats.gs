function setlats (args)

'run getenv LATSENV'
            latsenv = result

if( latsenv = NULL )
   'set lat -90 90'
else
   'set lat 'latsenv
endif

'getinfo latmin'
         latmin = result
'getinfo latmax'
         latmax = result

say 'Setting Lats: 'latmin' 'latmax
return
