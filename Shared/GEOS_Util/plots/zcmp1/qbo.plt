function qboplt (args)
qmod = subwrd(args,1)
qobs = subwrd(args,2)
mdesc = subwrd(args,3)
odesc = subwrd(args,4)
name   = subwrd(args,5)
output = subwrd(args,6)

levs = "100    1"
levs = "1000 0.1"

*'set xlopts 1 6 .14'
*'set ylopts 1 6 .13'

'c'
'set vpage off'
'vpage 1 1 1 2'
'set grads off'
'set grid  off'
'set clab  off'
'setlevs'
'set lev 'levs
'set gxout shaded'
'set csmooth on'
'set CCOLS 55 49 47 45 44 36 34 33 32  0  21 22 23 24 25 26 27 28 69'
'set CLEVS -36 -32 -28 -24 -20 -16 -12 -8 -4 4  8 12 16 20 24 28 32 36'
'd 'qmod
'set gxout contour'
'set clevs -4 4'
'set ccolor 1'
'd 'qmod
'cbarn -xmid 6.0'

'vpage 1 2 1 2'
'set grads off'
'set grid  off'
'set clab  off'
'setlevs'
'set lev 'levs
'set gxout shaded'
'set csmooth on'
'set CCOLS 55 49 47 45 44 36 34 33 32  0  21 22 23 24 25 26 27 28 69'
'set CLEVS -36 -32 -28 -24 -20 -16 -12 -8 -4 4  8 12 16 20 24 28 32 36'
'd 'qobs
'set gxout contour'
'set clevs -4 4'
'set ccolor 1'
'd 'qobs

'set vpage off'
'set string 1 c 6'
'set strsiz .15'
'draw string 6.0  8.1 (Zonal U-Wind, Lat: -10,10 Average)'
'set strsiz .13'
'draw string 6.0  7.7 'mdesc
'draw string 6.0  3.7 'odesc

'myprint -name 'output'/QBO_'name
