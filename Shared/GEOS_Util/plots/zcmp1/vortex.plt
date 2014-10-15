function qboplt (args)
nmod   =  subwrd(args,1)
nobs   =  subwrd(args,2)
mdesc  =  subwrd(args,3)
odesc  =  subwrd(args,4)
name   =  subwrd(args,5)
output =  subwrd(args,6)
clim   =  subwrd(args,7)

'c'
'set vpage off'
'set grads off'
'set grid  on'
'set gxout contour'
'set lev 100 .1'
'setlevs'

'set clab  on'
'set clevs  0'
'set ccolor 2'
'set cthick 8'
'set cstyle 1'
'd obsclim'

'set clab off'
'set clevs  0'
'set ccolor 2'
'set cthick 3'
'set cstyle 3'
'd obsclimp'
'set clevs  0'
'set ccolor 2'
'set cthick 3'
'set cstyle 3'
'd obsclimm'

'set clab  on'
'set clevs  0'
'set ccolor 4'
'set cthick 8'
'set cstyle 1'
'd modclim'

'set clab off'
'set clevs  0'
'set ccolor 4'
'set cthick 3'
'set cstyle 3'
'd modclimp'
'set clevs  0'
'set ccolor 4'
'set cthick 3'
'set cstyle 3'
'd modclimm'

'draw ylab Pressure (hPa)'

'set vpage off'
'set string 1 c 6'
'set strsiz .15'
'draw string 6.0  8.2 (Zonal Mean U-Wind, Lat: -50,-70 Average)'
'set strsiz .13'
'set string 4 c 6'
'draw string 6.0  7.5 'mdesc' ('nmod')'
'set string 2 c 6'
'draw string 6.0  7.0 'odesc' ('nobs') 'clim

'myprint -name 'output'/VORTEX_'name
