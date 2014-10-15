function gwd_1 (args)

expid  = subwrd(args,1)
season = subwrd(args,2)
output = subwrd(args,3)
num    = subwrd(args,4)
ptop   = subwrd(args,5)
debug  = subwrd(args,6)

'run getinfo undef'
             undef = result
'set display color white'
'set clab on'
'set clopts -1 -1 .13'
'set xlopts 1 3 .2'
'set ylopts 1 3 .2'
'set x 1'

'set rgb 84 204 204 204'
'set rgb 85 137 137 137'

* DUDT 1000-ptop mb
* -----------------
'vpage 1 1 2 2'
'set lev 1000 'ptop
'setlevs'
'set t 1'
'run qminmax dudtz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set clevs -0.5 0.5'
'set ccols 84 0 85'
'd dudtz'season'*86400'
'set gxout contour'
'set ccolor 1'
'set cint 10'
'black'
'd dudtz'season'*86400'
'set clevs  -5 -4 -3 -2 -1 -0.5 0.5 1 2 3 4 5'
'set ccolor 1'
'd dudtz'season'*86400'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'draw title U-Wind Tendency from Total GWD (m/sec/day)'
'draw ylab Pressure (mb)'

'vpage 2 1 2 2'
'set lev 1000 'ptop
'setlevs'
'set t 1'
'run qminmax dudtoroz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set clevs -0.5 0.5'
'set ccols 84 0 85'
'd dudtoroz'season'*86400'
'set gxout contour'
'set ccolor 1'
'set cint 10'
'black'
'd dudtoroz'season'*86400'
'set clevs  -5 -4 -3 -2 -1 -0.5 0.5 1 2 3 4 5'
'set ccolor 1'
'd dudtoroz'season'*86400'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'draw title U-Wind Tendency from Orographic GWD (m/sec/day)'
'draw ylab Pressure (mb)'

'vpage 2 2 2 2'
'set lev 1000 'ptop
'setlevs'
'set t 1'
'run qminmax dudtoroz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set clevs -0.5 0.5'
'set ccols 84 0 85'
'd dudtrayz'season'*86400'
'set gxout contour'
'set ccolor 1'
'set cint 10'
'black'
'd dudtrayz'season'*86400'
'set clevs  -5 -4 -3 -2 -1 -0.5 0.5 1 2 3 4 5'
'set ccolor 1'
'd dudtrayz'season'*86400'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'draw title U-Wind Tendency from Rayleigh Friction (m/sec/day)'
'draw ylab Pressure (mb)'

'vpage 1 2 2 2'
'set lev 1000 'ptop
'setlevs'
'set t 1'
'run qminmax dudtbkgz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set clevs -0.5 0.5'
'set ccols 84 0 85'
'd dudtbkgz'season'*86400'
'set gxout contour'
'set ccolor 1'
'set cint 10'
'black'
'd dudtbkgz'season'*86400'
'set clevs  -5 -4 -3 -2 -1 -0.5 0.5 1 2 3 4 5'
'set ccolor 1'
'd dudtbkgz'season'*86400'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'draw title U-Wind Tendency from Background GWD'
'draw ylab Pressure (mb)'

'set vpage off'
'set strsiz .13'
'set string 1 c 6'

'xlabel 1 5.5 8.3'
'draw string 5.5 8.1 'season' ('num')'

'myprint -name 'output'/gwdudtz_1.'season

if( debug = "debug" )
    say "Hit  ENTER  to continue ..."
         pull flag
endif
'c'

* DUDT 1000-10 mb
* ---------------
'vpage 1 1 2 2'
'set lev 1000 10'
'set ylevs 1000 820 650 500 400 300 200 140 100 70 50 40 30 20 14 10'
'set t 1'
'run qminmax dudtz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set clevs -0.1 0.1'
'set ccols 84 0 85'
'd dudtz'season'*86400'
'set gxout contour'
'set ccolor 1'
'set cint 1'
'black'
'd dudtz'season'*86400'
'set clevs  -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5'
'set ccolor 1'
'd dudtz'season'*86400'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'draw title U-Wind Tendency from Total GWD (m/sec/day)'
'draw ylab Pressure (mb)'

'vpage 2 1 2 2'
'set lev 1000 10'
'set ylevs 1000 820 650 500 400 300 200 140 100 70 50 40 30 20 14 10'
'set t 1'
'run qminmax dudtoroz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set clevs -0.1 0.1'
'set ccols 84 0 85'
'd dudtoroz'season'*86400'
'set gxout contour'
'set ccolor 1'
'set cint 1'
'black'
'd dudtoroz'season'*86400'
'set clevs  -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5'
'set ccolor 1'
'd dudtoroz'season'*86400'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'draw title U-Wind Tendency from Orographic GWD (m/sec/day)'
'draw ylab Pressure (mb)'

'vpage 2 2 2 2'
'set lev 1000 10'
'set ylevs 1000 820 650 500 400 300 200 140 100 70 50 40 30 20 14 10'
'set t 1'
'run qminmax dudtrayz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set clevs -0.1 0.1'
'set ccols 84 0 85'
'd dudtrayz'season'*86400'
'set gxout contour'
'set ccolor 1'
'set cint 1'
'black'
'd dudtrayz'season'*86400'
'set clevs  -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5'
'set ccolor 1'
'd dudtrayz'season'*86400'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'draw title U-Wind Tendency from Rayleigh Friciton (m/sec/day)'
'draw ylab Pressure (mb)'

'vpage 1 2 2 2'
'set lev 1000 10'
'set ylevs 1000 820 650 500 400 300 200 140 100 70 50 40 30 20 14 10'
'set t 1'
'run qminmax dudtbkgz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set clevs -0.1 0.1'
'set ccols 84 0 85'
'd dudtbkgz'season'*86400'
'set gxout contour'
'set ccolor 1'
'set cint 1'
'black'
'd dudtbkgz'season'*86400'
'set clevs  -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5'
'set ccolor 1'
'd dudtbkgz'season'*86400'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'draw title U-Wind Tendency from Background GWD'
'draw ylab Pressure (mb)'

'set vpage off'
'set strsiz .13'
'set string 1 c 6'

'xlabel 1 5.5 8.3'
'draw string 5.5 8.1 'season' ('num')'

'myprint -name 'output'/gwdudtz_2.'season

if( debug = "debug" )
    say "Hit  ENTER  to continue ..."
         pull flag
endif
'c'

* DVDT 1000-ptop mb
* -----------------
'vpage 1 1 2 2'
'set lev 1000 'ptop
'setlevs'
'set t 1'
'run qminmax dvdtz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set clevs -0.2 0.2'
'set ccols 84 0 85'
'd dvdtz'season'*86400'
'set gxout contour'
'set ccolor 1'
'set cint 1'
'black'
'd dvdtz'season'*86400'
'set clevs  -.5 -.2 0.2 .5'
'set ccolor 1'
'd dvdtz'season'*86400'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'draw title V-Wind Tendency from Total GWD (m/sec/day)'
'draw ylab Pressure (mb)'

'vpage 2 1 2 2'
'set lev 1000 'ptop
'setlevs'
'set t 1'
'run qminmax dvdtoroz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set clevs -0.2 0.2'
'set ccols 84 0 85'
'd dvdtoroz'season'*86400'
'set gxout contour'
'set ccolor 1'
'set cint 1'
'black'
'd dvdtoroz'season'*86400'
'set clevs  -.5 -.2 0.2 .5'
'set ccolor 1'
'd dvdtoroz'season'*86400'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'draw title V-Wind Tendency from Orographic GWD (m/sec/day)'
'draw ylab Pressure (mb)'

'vpage 2 2 2 2'
'set lev 1000 'ptop
'setlevs'
'set t 1'
'run qminmax dvdtrayz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set clevs -0.2 0.2'
'set ccols 84 0 85'
'd dvdtrayz'season'*86400'
'set gxout contour'
'set ccolor 1'
'set cint 1'
'black'
'd dvdtrayz'season'*86400'
'set clevs  -.5 -.2 0.2 .5'
'set ccolor 1'
'd dvdtrayz'season'*86400'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'draw title V-Wind Tendency from Rayleigh Friction (m/sec/day)'
'draw ylab Pressure (mb)'

'vpage 1 2 2 2'
'set lev 1000 'ptop
'setlevs'
'set t 1'
'run qminmax dvdtbkgz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set clevs -0.2 0.2'
'set ccols 84 0 85'
'd dvdtbkgz'season'*86400'
'set gxout contour'
'set ccolor 1'
'set cint 1'
'black'
'd dvdtbkgz'season'*86400'
'set clevs  -.5 -.2 0.2 .5'
'set ccolor 1'
'd dvdtbkgz'season'*86400'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'draw title V-Wind Tendency from Background GWD'
'draw ylab Pressure (mb)'

'set vpage off'
'set strsiz .13'
'set string 1 c 6'

'xlabel 1 5.5 8.3'
'draw string 5.5 8.1 'season' ('num')'

'myprint -name 'output'/gwdvdtz_1.'season

'set display color white'
return
