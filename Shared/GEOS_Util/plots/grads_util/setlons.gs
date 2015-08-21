function setlons (args)

'getinfo xdim'
         xdim = result
*if( xdim > 288 )
*   'setx'
*else
*   'set lon 0 360'
*endif

'run getenv LONSENV'
            lonsenv = result

if( lonsenv = NULL )
   'set lon 0 360'
else
   'set lon 'lonsenv
endif

'getinfo lonmin'
         lonmin = result
'getinfo lonmax'
         lonmax = result

say 'Setting Lons: 'lonmin' 'lonmax
return
