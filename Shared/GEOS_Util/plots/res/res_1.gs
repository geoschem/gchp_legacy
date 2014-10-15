function  res_1 (args)

 expid  = subwrd(args,1)
 season = subwrd(args,2)
 output = subwrd(args,3)
 obsid  = subwrd(args,4)
 obsdes = subwrd(args,5)
 nmod   = subwrd(args,6)
 nobs   = subwrd(args,7)

'set rgb 84 204 204 204'
'set rgb 85 137 137 137'

* Streamfunction
* --------------
'set clab on'
'set dfile 1'
'set x 1'
'sety'
'setz'
'set zlog off'
'set vpage 0.20  5.3 4.1 7.8'
'set GrADS off'
'set clopts -1 -1 .13'
'set xlopts 1 4 .2'
'set ylopts 1 4 .2'
'set gxout shaded'
'set clevs 0'
'set ccols 84 0'
'set t 1'
'd str'season'mod'
'set gxout contour'
'set ccolor 1'
'set cint 2'
'd str'season'mod'
'set strsiz .15'
'set string 1 c'
'draw title Meridional Streamfunction  'season' ('nmod')'
'draw ylab Pressure (mb)'
 
'set vpage 5.20 10.30 4.1 7.8'
'set GrADS off'
'set clopts -1 -1 .13'
'set xlopts 1 4 .2'
'set ylopts 1 4 .2'
'set gxout shaded'
'set clevs 0'
'set ccols 84 0'
'set t 1'
'd str'season'obs'
'set gxout contour'
'set ccolor 1'
'set cint 2'
'd str'season'obs'
'set strsiz .15'
'set string 1 c'
'draw title 'obsdes' Streamfunction  'season' ('nobs')'

* Residual Circulation
* --------------------
'set clab off'
'set vpage 0.20  5.3 0.5 4.2'
'set GrADS off'
'setlevs'
'set ylevs 1000 700 500 300 200 100 70 50 30 20 10 7 5 3 2 1 0.4 0.2 0.1 0.04 0.02'
'set clopts -1 -1 .13'
'set xlopts 1 4 .2'
'set ylopts 1 4 .2'
'set gxout shaded'
'set clevs 0'
'set ccols 84 0'
'd res'season'mod'
'set gxout contour'
'set ccolor 1'
'set clevs -50 -40 -30 -20 -10 -5 -4 -3 -2 -1 -0.5 -0.4 -0.3 -0.2 -0.1 -.05 0 0.05 .1 .2 .3 .4 .5 1 2 3 4 5 10 20 30 40 50'
'd res'season'mod'
'draw title Residual Circulation Streamfunction  'season' ('nmod')'
'draw ylab Pressure (mb)'
 
'set vpage 5.20 10.30 0.5 4.2'
'set GrADS off'
'set ylevs 1000 700 500 300 200 100 70 50 30 20 10 7 5 3 2 1 0.4 0.2 0.1 0.04 0.02'
'set clopts -1 -1 .13'
'set xlopts 1 4 .2'
'set ylopts 1 4 .2'
'set gxout shaded'
'set clevs 0'
'set ccols 84 0'
'd res'season'obs'
'set gxout contour'
'set ccolor 1'
'set clevs -50 -40 -30 -20 -10 -5 -4 -3 -2 -1 -0.5 -0.4 -0.3 -0.2 -0.1 -.05 0 0.05 .1 .2 .3 .4 .5 1 2 3 4 5 10 20 30 40 50'
'd res'season'obs'
'draw title 'obsdes' Residual Circulation  'season' ('nobs') '
 
'set vpage off'
'set string 1 c 6'
'set strsiz .11'
'xlabel 1 5.4 8.1'
'draw string 5.43666 0.375552 Experiment:  'expid

'myprint -name 'output'/zonal_'obsid'_str_res.'season
'set clab on'
'c'
