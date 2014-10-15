function rmsz  (args)
field  = subwrd(args,1)
expdsc = subwrd(args,2)

'run getenv "GEOSUTIL"'
             geosutil = result
'run getenv "SOURCE"'
             source   = result

'numargs  'args
 numargs = result
        num = 0
while ( num < numargs )
        num = num + 1
if( subwrd(args,num) = '-source'   ) ; source = subwrd(args,num+1) ; endif
endwhile

if( geosutil = "NULL" ) ; say 'Environment Variable GEOSUTIL must be set!' ; return ; endif
if( source   = "NULL" ) ; say 'Environment Variable SOURCE   must be set or passed in <-source SOURCE> !' ; return ; endif

* Check for OUTPUT directory
* --------------------------
'!mkdir -p 'source'/plots'
output   = source  % '/' % plots


* Define Number of Forecast Days and Time Interval (hrs)
* ------------------------------------------------------
'run getenv "NDAY"'
             nday = result
'run getenv "TINC"'
             tinc = result
if( nday = "NULL" ) ; nday = 5 ; endif
if( tinc = "NULL" ) ; tinc = 6 ; endif
                      nmax = 1 + nday*(24/tinc)

* Initialize Plot Values
* ----------------------
if( field = "h" )
    name  = "Heights"
    unit  = "(m)"
    label = "hght"
    clevs = "5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100"
endif
if( field = "u" )
    name  = "U-Wind"
    unit  = "(m/sec)"
    label = "uwnd"
    clevs = ".5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5 6.0 7.0 8.0 9.0 10. 11. 12. 13. 14."
endif
if( field = "v" )
    name  = "V-Wind"
    unit  = "(m/sec)"
    label = "vwnd"
    clevs = ".5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5 6.0 7.0 8.0 9.0 10. 11. 12. 13. 14."
endif
if( field = "t" )
    name  = "Temperature"
    unit  = "(K)"
    label = "tmpu"
    clevs = ".1 .2 .3 .4 .5 .7 .9 1.3 1.5 1.7 2.0 2.3 2.5 2.7 3.0 3.5 4.0 4.5 5.0 5.5"
endif


'getinfo numfiles'
         numfiles = result

'getinfo xpos'
         xpos = result
if( xpos =  1 ) ; region = "Global"                                     ;  reg = "GLO"  ; endif
if( xpos =  2 ) ; region = "Northern Hemisphere ExtraTropics"           ;  reg = "NHE"  ; endif
if( xpos =  3 ) ; region = "Tropics"                                    ;  reg = "TRO"  ; endif
if( xpos =  4 ) ; region = "Southern Hemisphere ExtraTropics"           ;  reg = "SHE"  ; endif
if( xpos =  5 ) ; region = "N.W. Quadrant (Lons:-180,0  Lats: 0, 90)"   ;  reg = "NWQ"  ; endif
if( xpos =  6 ) ; region = "N.E. Quadrant (Lons: 0,180  Lats: 0, 90)"   ;  reg = "NEQ"  ; endif
if( xpos =  7 ) ; region = "S.W. Quadrant (Lons:-180,0  Lats: 0,-90)"   ;  reg = "SWQ"  ; endif
if( xpos =  8 ) ; region = "S.E. Quadrant (Lons: 0,180  Lats: 0,-90)"   ;  reg = "SEQ"  ; endif
if( xpos =  9 ) ; region = "North America (Lons:-140,-60  Lats: 20,60)" ;  reg = "NAM"  ; endif
if( xpos = 10 ) ; region = "Europe (Lons:-10,30  Lats: 30,60)"          ;  reg = "EUR"  ; endif

'setz'
'getinfo tdim'
         tdim = result
         tbeg = 1-(nmax-tdim)
'set t ' tbeg ' 'tdim
'define qave1 = lev-lev'
'define qave2 = lev-lev'
'define qave3 = lev-lev'
'define qave4 = lev-lev'
'define qave5 = lev-lev'

n = 1
while ( n <= numfiles )
'set dfile 'n
'define qave1 = qave1 + 'field'rms.'n
'define qave2 = qave2 + 'field'rms_dis.'n
'define qave3 = qave3 + 'field'rms_dsp.'n
'define qave4 = qave4 + 'field'rms_ran.'n
'define qave5 = qave5 + 'field'rms_bar.'n
n = n + 1
endwhile

'define qave1 = qave1/'numfiles
'define qave4 = qave4/'numfiles
'define qave5 = qave5/'numfiles
'define amplt = qave2/(qave2+qave3)'
'define phase = qave3/(qave2+qave3)'

'set csmooth on'

'vpage 1 1 1 2'
'set grads off'
'set xaxis 0 'nday' .5'
'set gxout shaded'
'set clevs 'clevs
'set ccols 59   58   57   56   55   49   47   45   44   37   36   34   33   32  31  21  22  23  24  25  26  27  28'
'd qave1'
'set gxout contour'
'set ccolor 1'
'set clab off'
'set clevs 'clevs
'd qave1'
'cbarn -snum 0.9 -xmid 4.65 -ymid 0.20'

'getinfo date'
         date = result
        month = substr(date,6,3)

'set string 1 c 6'
'set strsiz .13'
'draw string 4.65 0.65 Forecast Day ('month')'
'draw string 4.91406 4.92 'name':  Root Mean Square Error 'unit
'draw ylab Pressure (mb)'


'vpage 1 2 1 2'
'set grads off'
'set gxout shaded'
'set axlim 0 105'
'set xaxis 0 'nday' .5'
'set clevs 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100'
'set ccols 59   58   57   56   55   49   47   45   44   37   36   34   33   32  31  21  22  23  24  25  26  27  28'
'd phase*100'
'set gxout contour'
'set ccolor 1'
'set clab off'
'set clevs 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100'
'd phase*100'
'cbarn -snum 0.9 -xmid 4.65 -ymid 0.20'
'set string 1 c 6'
'set strsiz .13'
'draw string 4.65 0.65 Forecast Day ('month')'
'draw string 4.91406 4.92 'name':  Mean Square Error  (Phase %)'
'draw ylab Pressure (mb)'

'set vpage off'
'set grads off'
'set string 1 c 6'
'set strsiz .15'
'draw string 4.64844 10.4413 'region
'draw string 4.64844 10.7413 'expdsc

'myprint -name 'output'/stats_'label'_rms_'reg'_z_'month
