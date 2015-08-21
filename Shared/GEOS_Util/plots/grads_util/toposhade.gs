function toposhade (args)

**************************************************
***  If  args = lowres  , TOPO => 1/8 -deg     ***
***              hires  , TOPO => 1/16-deg     ***
**************************************************

flag = subwrd(args,1)

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

 'd abs('latmax-latmin')'
         dlat = subwrd(result,4)
 'd abs('lonmax-lonmin')'
         dlon = subwrd(result,4)

'getinfo time'
         time = result
'getinfo zpos'
         zpos = result
'getinfo file'
         curfile = result


if( flag = hires )
   'sdfopen 'geosutil'/plots/grads_util/CONST_2D.5760x2881.nc4'
else
    if( flag = lowres )
       'sdfopen 'geosutil'/plots/grads_util/CONST_2D.2880x1441.nc4'
    else
        if( dlat < 30 | dlon < 30 )
           'sdfopen 'geosutil'/plots/grads_util/CONST_2D.5760x2881.nc4'
        else
           'sdfopen 'geosutil'/plots/grads_util/CONST_2D.2880x1441.nc4'
        endif
    endif
endif

'getinfo numfiles'
         newfile = result

'set dfile 'newfile
'set t 1'
'set z 1'

'define topo = phis/9.80665'
'define ddx = cdiff(topo,x)'

'minmax.simple ddx'
 ddxmax = subwrd(result,1)
 ddxmin = subwrd(result,2)

'define ddx = (ddx-'ddxmin')/('ddxmax'-'ddxmin')'

'set rgb 80   0   0   0'
'set rgb 81  18  18  18'
'set rgb 82  36  36  36'
'set rgb 83  54  54  54'
'set rgb 84  72  72  72'
'set rgb 85  90  90  90'
'set rgb 86 108 108 108'
'set rgb 87 126 126 126'
'set rgb 88 144 144 144'
'set rgb 89 162 162 162'
'set rgb 90 180 180 180'
'set rgb 91 198 198 198'
'set rgb 92 216 216 216'
'set rgb 93 234 234 234'
'set rgb 94 252 252 252'
'set rgb 95 198 198 198'
'set rgb 96 216 216 216'
'set rgb 97 234 234 234'
'set rgb 98 252 252 252'


 'set rgb 91 198 198 198'
 'set rgb 92 202 202 202'
 'set rgb 93 207 207 207'
 'set rgb 94 216 216 216'
 'set rgb 95 225 225 225'
 'set rgb 96 234 234 234'
 'set rgb 97 243 243 243'
 'set rgb 98 252 252 252'

'set ccols  0    98    97    96    95    94    93    92    91    90    89    88    87    86    85    84    83    82    81    80 '
'set clevs   0.05  0.10  0.15  0.20  0.25  0.30  0.35  0.40  0.45  0.50  0.55  0.60  0.65  0.70  0.75  0.80  0.85  0.90  0.95 '

'd maskout( ddx, abs(topo)-0.1 )'

'close 'newfile
'set dfile 'curfile
'set t 'time
'set z 'zpos

return
