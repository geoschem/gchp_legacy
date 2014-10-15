function landshade (args)
color = subwrd(args,1)
if( color = '' )
    color = 92
endif

'run getenv GEOSUTIL'
        geosutil = result

'getinfo latmin'
         latmin = result
'getinfo latmax'
         latmax = result
'getinfo lonmin'
         lonmin = result
'getinfo lonmax'
         lonmax = result
'getinfo time'
         time = result
'getinfo zpos'
         zpos = result
'getinfo file'
         curfile = result
'open 'geosutil'/plots/grads_util/lwmask1440721.tabl'
'getinfo numfiles'
         newfile = result

'set dfile 'newfile
'set t 1'
'set z 1'
'set lon -180 360'
'set lat -90 90'
'define mask = lwmask'
'close 'newfile
'set dfile 'curfile
'set t 'time
'set z 'zpos
'set lat 'latmin' 'latmax
'set lon 'lonmin' 'lonmax

'set grads off'
'set gxout shaded'
'set ccols 'color' 0'
'set clevs 0.5'
'd mask'

return
