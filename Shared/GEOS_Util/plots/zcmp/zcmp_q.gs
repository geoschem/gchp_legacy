function zcmp (args)

expid  = subwrd(args,1)
season = subwrd(args,2)
output = subwrd(args,3)
num    = subwrd(args,4)

'set dfile 1'
'set x 1'
'set lev 1000 .02'
'setlevs'
'set grid off'
'set gxout shaded'
'set clevs   -3.1 -3.0 -2.9 -2.8 -2.7 -2.6 -2.5 -2.4 -2.3 -2.2 -2.1 -2 -1.8 -1.6 -1.4 -1.2 -1 -.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1  0 .2   .4   .6   .8   1   1.2  1.4'
'set ccols  59 58 57 56 55 54 53 52 51 50 42 43 44 45 46 47 48 49 39 38 37 36 35 34 33 32 31 21   22   23   24   25   26   27   28   29'
'd log10(sphuz1'season'*1e3)'
'cbarn -xmid 6 -ymid 0.5 -snum 0.53'

'set gxout contour'
'set ccolor 1'
'set clevs   -3.2 -3.1 -3.0 -2.9 -2.8 -2.7 -2.6 -2.5 -2.4 -2.3'
'd log10(sphuz1'season'*1e3)'
'draw ylab Pressure (mb)'
'set vpage off'
'set string 1 c 6'
'set strsiz .11'
'draw string 6 0.2 Log`b10`n[ QV(g/kg) ]'
'draw string 6 8.42 'expid'  'season' ('num')'
'set strsiz .10'
'xlabel 1 6 8.2'
'set grid on'

'myprint -name 'output'/zonal.log_qv.'season
'c'
