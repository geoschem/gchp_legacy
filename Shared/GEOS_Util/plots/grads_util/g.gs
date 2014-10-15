function g (args)
string = subwrd(args,1)
cint   = subwrd(args,2)
minval = subwrd(args,3)

'run getenv GEOSUTIL'
        geosutil = result

'getinfo lat'
         lat = result
'getinfo lon'
         lon = result
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
'define mask = lwmask'
'close 'newfile
'set dfile 'curfile
'set t 'time
'set z 'zpos

'set grads off'
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd mask'

cval = cint

if( cint = '' & minval = '' )
'shades 'string' 0'
         cval = result
endif

if( cint != '' & minval = '' )
'shades 'string' 0 -cint 'cint
endif

if( cint = '' & minval != '' )
'shades 'string' 0 -minval 'minval
         cval = result
endif

if( cint != '' & minval != '' )
'shades 'string' 0 -cint 'cint' -minval 'minval
endif

'define dummy = regrid2( 'string', .25, .25, bs_p1, 'lon', 'lat' )'
'd maskout(dummy,abs(dummy)-'cval')'
say 'd maskout(dummy,abs(dummy)-'cval')'
'cbarn -snum 0.8'

