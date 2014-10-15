function gwd_t (args)

expid  = subwrd(args,1)
season = subwrd(args,2)
output = subwrd(args,3)
num    = subwrd(args,4)
ptop   = subwrd(args,5)

'run getinfo undef'
             undef = result
'set display color white'
'set clab on'
'set xlopts 1 3 .2'
'set ylopts 1 3 .2'
'set x 1'

'vpage 1 1 2 2'
'set lev 1000 'ptop
'setlevs'
'sety'
'run qminmax dtdtz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set ccols 55  49  47  45  44  36  34  33  32  0  21  22  23  24  25  26  27  28 69'
'set clevs -2.5 -2 -1.5 -1 -.8 -.6 -.4 -.2 -0.1 0.1 .2 .4 .6 .8 1 1.5 2 2.5'
'd maskout( dtdtz'season'*86400, dtdtz'season' )'
'set ccols 55  49  47  45  44  36  34  33  32  0  21  22  23  24  25  26  27  28 69'
'set clevs -2.5 -2 -1.5 -1 -.8 -.6 -.4 -.2 -0.1 0.1 .2 .4 .6 .8 1 1.5 2 2.5'
'd maskout( dtdtz'season'*86400*100, -dtdtz'season' )'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'cbarn -xmid 6.0'
'draw title Temperature Tendency from Total GWD (K/day)'
'draw ylab Pressure (mb)'


'vpage 2 1 2 2'
'set lev 1000 'ptop
'setlevs'
'sety'
'run qminmax dtdtoroz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set ccols 55  49  47  45  44  36  34  33  32  0  21  22  23  24  25  26  27  28 69'
'set clevs -2.5 -2 -1.5 -1 -.8 -.6 -.4 -.2 -0.1 0.1 .2 .4 .6 .8 1 1.5 2 2.5'
'd maskout( dtdtoroz'season'*86400*100, -dtdtoroz'season' )'
'set ccols 55  49  47  45  44  36  34  33  32  0  21  22  23  24  25  26  27  28 69'
'set clevs -2.5 -2 -1.5 -1 -.8 -.6 -.4 -.2 -0.1 0.1 .2 .4 .6 .8 1 1.5 2 2.5'
'd maskout( dtdtoroz'season'*86400, dtdtoroz'season' )'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'cbarn -xmid 6.0'
'draw title Temperature Tendency from Orographic GWD (K/day)'
'draw ylab Pressure (mb)'

'vpage 1 2 2 2'
'set lev 1000 'ptop
'setlevs'
'sety'
'run qminmax dtdtbkgz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set ccols 55  49  47  45  44  36  34  33  32  0  21  22  23  24  25  26  27  28 69'
'set clevs -2.5 -2 -1.5 -1 -.8 -.6 -.4 -.2 -0.1 0.1 .2 .4 .6 .8 1 1.5 2 2.5'
'd maskout( dtdtbkgz'season'*86400, dtdtbkgz'season' )'
'set ccols 55  49  47  45  44  36  34  33  32  0  21  22  23  24  25  26  27  28 69'
'set clevs -2.5 -2 -1.5 -1 -.8 -.6 -.4 -.2 -0.1 0.1 .2 .4 .6 .8 1 1.5 2 2.5'
'd maskout( dtdtbkgz'season'*86400*100, -dtdtbkgz'season' )'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'cbarn -xmid 6.0'
'draw title Temperature Tendency from Background GWD (K/day)'
'draw ylab Pressure (mb)'

'vpage 2 2 2 2'
'set lev 1000 'ptop
'setlevs'
'sety'
'run qminmax dtdtrayz'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set ccols 55  49  47  45  44  36  34  33  32  0  21  22  23  24  25  26  27  28 69'
'set clevs -2.5 -2 -1.5 -1 -.8 -.6 -.4 -.2 -0.1 0.1 .2 .4 .6 .8 1 1.5 2 2.5'
'd maskout( dtdtrayz'season'*86400*100, -dtdtrayz'season' )'
'set ccols 55  49  47  45  44  36  34  33  32  0  21  22  23  24  25  26  27  28 69'
'set clevs -2.5 -2 -1.5 -1 -.8 -.6 -.4 -.2 -0.1 0.1 .2 .4 .6 .8 1 1.5 2 2.5'
'd maskout( dtdtrayz'season'*86400, dtdtrayz'season' )'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'cbarn -xmid 6.0'
'draw title Temperature Tendency from Rayleigh Friction (K/day)'
'draw ylab Pressure (mb)'

'set vpage off'
'set strsiz .13'
'set string 1 c 6'

'xlabel 1 5.5 8.3'
'set strsiz .095'
'draw string 5.5 8.1 'season' ('num')  (Note: Negative values have been multiplied by 100)'

'myprint -name 'output'/gwdtdtz_1.'season
pull flag
'c'
