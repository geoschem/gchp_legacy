function gwd_2 (args)

expid  = subwrd(args,1)
season = subwrd(args,2)
output = subwrd(args,3)
num    = subwrd(args,4)
debug  = subwrd(args,5)

'run getinfo undef'
             undef = result

'setlons'
'set z 1'
'set clab off'
'set grid off'
'set xlopts 1 3 .10' 
'set ylopts 1 3 .10' 

'run getenv GEOSUTIL'
            geosutil = result
'run getinfo file'
        dfile = result
'open 'geosutil'/plots/grads_util/lwmask1440721.tabl'
'getinfo numfiles'
         newfile = result
'set dfile 'newfile
'set t 1'
'set z 1'
'define lmask = lwmask'
'close 'newfile
'set dfile 'dfile
'set t 1'
'set z 1'


* Total Surface Stress
* --------------------

'run qminmax mag(tauorox'season'+taubkgx'season',tauoroy'season'+taubkgy'season')'
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
'set vpage 0 11 0 8.5'
'set parea 0.4 5.4 4.0 8.5'
'set grads off'
'set grid  off'

 if( qmin != undef | qmax != undef )
'set gxout shaded'

'set ccols 92 0'
'set clevs 0.5'
'd lmask'

'define ss = mag( tauorox'season'+taubkgx'season' , tauoroy'season'+taubkgy'season' )'
'shades ss 0 -cint 0.01'
'define ss2 = regrid2( ss,0.25,0.25,bs_p1,0,-90 )'
'd maskout( ss2, abs(ss2)-0.01 )'
'cbarn -scalex 0.55 -scaley 0.55 -xmid 2.90 -ymid 4.35 -snum 0.45'
'set gxout contour'
'set clevs 0.01'
'set ccolor 1'
'd ss'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'set strsiz .10'
'draw string 2.8  7.95 Total Surface Stress from GWD (N/m`a2`n)'


* Orographic Surface Stress
* -------------------------
'run qminmax mag(tauorox'season',tauoroy'season')'
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
'set parea off'
'set vpage 0 11 0 8.5'
'set parea 5.9 10.9  4.0 8.5'
'set grads off'
'set grid  off'

 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd lmask'
'define ss = mag( tauorox'season' , tauoroy'season' )'
'shades ss 0 -cint 0.01'
'define ss2 = regrid2( ss,0.25,0.25,bs_p1,0,-90 )'
'd maskout( ss2, abs(ss2)-0.01 )'
'cbarn -scalex 0.55 -scaley 0.55 -xmid 8.40 -ymid 4.35 -snum 0.45'
'set gxout contour'
'set clevs 0.01'
'set ccolor 1'
'd ss'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'set strsiz .10'
'draw string 8.2  7.95 Orographic Surface Stress from GWD (N/m`a2`n)'

* Moist-Processes Surface Stress
* ------------------------------
'set parea off'
'set vpage 0 11 0 8.5'
'set parea 1.0 4.8  0.6 3.9'
'set grads off'
'set grid  off'

'set gxout shaded'
'define ss = mag( taubkgx'season' , taubkgy'season' )'
'makez ss z'
'set x 1'
'set axlim -50 850'
'set cthick 6'
'set cmark 0'
'd ssz*1e6'
'set cmark 0'
'set cthick 1'
'set ccolor 2'
'd lev-lev'
'setlons'
'set strsiz .10'
'draw string 2.8  4.10 Zonal-Mean BKG Surface Stress from GWD (N/m`a2`n x 10`a-6`n)'

* Background Surface Stress
* -------------------------
'run qminmax mag(taubkgx'season',taubkgy'season')'
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
'set parea off'
'set vpage 0 11 0 8.5'
'set parea 5.9 10.9  0.2 4.7'
'set grads off'
'set grid  off'

 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd lmask'
'define ss = mag( taubkgx'season' , taubkgy'season' )*100'
'shades ss 0 -cint 0.01'
'define ss2 = regrid2( ss,0.25,0.25,bs_p1,0,-90 )'
'd maskout( ss2, abs(ss2)-0.01 )'
'cbarn -scalex 0.55 -scaley 0.55 -xmid 8.40 -ymid 0.55 -snum 0.45'
'set gxout contour'
'set clevs 0.01'
'set ccolor 1'
'd ss'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'set strsiz .10'
'draw string 8.2  4.10 Background Surface Stress from GWD (N/m`a2`n x 10`a-2`n)'

'set vpage off'
'set strsiz .13'
'set string 1 c 6'

'xlabel 1 5.5 8.3'
'draw string 5.5 8.1 'season' ('num')'

'myprint -name 'output'/gwds1.'season

if( debug = "debug" )
    say "Hit  ENTER  to continue"
    pull flag
endif
'c'


* Total Surface Stress
* --------------------
'run qminmax mag(tauorox'season'+taubkgx'season',tauoroy'season'+taubkgy'season')'
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
'set vpage 0 11 0 8.5'
'set parea 0.4 5.4 4.0 8.5'
'set grads off'
'set grid  off'
'set map 1 1 3'

 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd lmask'

'define us = tauorox'season'+taubkgx'season
'define vs = tauoroy'season'+taubkgy'season
'define sp = mag(us,vs)'
'define mask = sp-0.01'
'define us = -maskout(us,mask)'
'define vs = -maskout(vs,mask)'
'd skip(us,2) ; skip(vs,2)'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'set strsiz .10'
'draw string 2.8  7.95 Total Surface Stress from GWD (N/m`a2`n)'


* Orographic Surface Stress
* -------------------------
'run qminmax mag(tauorox'season',tauoroy'season')'
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
'set parea off'
'set vpage 0 11 0 8.5'
'set parea 5.9 10.9  4.0 8.5'
'set grads off'
'set grid  off'

 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd lmask'

'define us = tauorox'season
'define vs = tauoroy'season
'define sp = mag(us,vs)'
'define mask = sp-0.01'
'define us = -maskout(us,mask)'
'define vs = -maskout(vs,mask)'
'd skip(us,2) ; skip(vs,2)'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'set strsiz .10'
'draw string 8.2  7.95 Orographic Surface Stress from GWD (N/m`a2`n)'



* Background Surface Stress
* -------------------------
'run qminmax mag(taubkgx'season',taubkgy'season')'
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
'set parea off'
'set vpage 0 11 0 8.5'
'set parea 5.9 10.9  0.2 4.7'
'set grads off'
'set grid  off'

 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd lmask'

'define us = taubkgx'season'*100'
'define vs = taubkgy'season'*100'
'define sp = mag(us,vs)'
'define mask = sp-0.01'
'define us = -maskout(us,mask)'
'define vs = -maskout(vs,mask)'
'd skip(us,2) ; skip(vs,2)'
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'set strsiz .10'
'draw string 8.2  4.10 Background Surface Stress from GWD (N/m`a2`n x 10`a-2`n)'

'set vpage off'
'set strsiz .13'
'set string 1 c 6'

'xlabel 1 5.5 8.3'
'draw string 5.5 8.1 'season' ('num')'

'myprint -name 'output'/gwds2.'season

if( debug = "debug" )
    say "Hit  ENTER  to continue"
    pull flag
endif
'c'


* Orographic STD'
* --------------'
'run qminmax sgh'season
     qmin = subwrd(result,1)
     qmax = subwrd(result,2)
'set vpage 0 11 0 8.5'
'set parea 0.4 5.4 4.0 8.5'
'set grads off'
'set grid  off'
'set map 1 1 3'

 if( qmin != undef | qmax != undef )
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd lmask'

'define ss = sgh'season
'shades ss 0 -cint 50'
'define ss2 = regrid2( ss,0.25,0.25,bs_p1,0,-90 )'
'd maskout( ss2, abs(ss2)-50 )'
'cbarn -scalex 0.55 -scaley 0.55 -xmid 2.90 -ymid 4.35 -snum 0.45'

'set gxout contour'
'set clevs 50'
'set ccolor 1'
'd sgh'season''
 else
   'set strsiz .25'
   'set string  1 c 6'
   'draw string 5.5 5 Not Exported'
 endif
'set strsiz .10'
'draw string 2.8  7.95 Orographic Stnd.Dev. for GWD (m)'


'set vpage off'
'set parea off'
'set strsiz .13'
'set string 1 c 6'

'xlabel 1 5.5 8.3'
'draw string 5.5 8.1 'season' ('num')'

'myprint -name 'output'/gwds3.'season

'set clab on'
