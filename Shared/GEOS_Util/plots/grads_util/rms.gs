function rms   (args)
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
    name  = field
    label = field
    axlim = NULL

if( field = "p" )
    name  = "Sea-Level Pressure"
    unit  = "(mb)"
    label = "slp"
    axlim = "-2 8"
endif
if( field = "h" )
    name  = "Heights"
    unit  = "(m)"
    label = "hght"
    axlim = "-10 110"
endif
if( field = "u" )
    name  = "U-Wind"
    unit  = "(m/sec)"
    label = "uwnd"
    axlim = "-2 16"
endif
if( field = "v" )
    name  = "V-Wind"
    unit  = "(m/sec)"
    label = "vwnd"
    axlim = "-2 16"
endif
if( field = "t" )
    name  = "Temperature"
    unit  = "(K)"
    label = "tmpu"
    axlim = "-1 6"
endif

if( field = 'tau' ) ; name = 'Total Aerosol'  ; endif
if( field = 'du'  ) ; name = 'Dust'           ; endif
if( field = 'ss'  ) ; name = 'Sea Salt'       ; endif
if( field = 'bc'  ) ; name = 'Black Carbon'   ; endif
if( field = 'oc'  ) ; name = 'Organic Carbon' ; endif
if( field = 'su'  ) ; name = 'Sulfate'        ; endif


'run getenv "GEOSUTIL"'
             geosutil = result

'getinfo xpos'
         xpos  = result
'getinfo level'
         level = result
'getinfo numfiles'
         numfiles = result

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

'getinfo tdim'
         tdim = result
         tbeg = 1-(nmax-tdim)
'set t ' tbeg ' 'tdim

if( axlim = NULL )
qmax = 0
n = 1
while ( n <= numfiles )
'set dfile 'n
'set t 'tdim
'd 'field'rms.'n
val = subwrd(result,4)
if( val > qmax ) ; qmax = val ; endif
n = n + 1
endwhile
qmax =   qmax * 1.2
qmin = - qmax * 0.1
axlim = qmin' 'qmax
endif
say 'axlim: 'axlim

'define qave = lev-lev'
n = 1
while ( n <= numfiles )
'set grads off'
'set dfile 'n
'set axlim 'axlim
'set ylab %.2f'
'set t 'tbeg' 'tdim
'set xaxis 0 'nday' .5'
'set cmark 0'
'set ccolor 2'
'set cthick 3'
'd 'field'rms.'n
'define qave = qave + 'field'rms.'n
n = n + 1
endwhile
'define qave = qave/'numfiles
'set cmark 0'
'set cthick 8'
'set ccolor 1'
'd qave'

'define qave = lev-lev'
n = 1
while ( n <= numfiles )
'set grads off'
'set dfile 'n
'set axlim 'axlim
'set ylab %.2f'
'set t 'tbeg' 'tdim
'set xaxis 0 'nday' .5'
'set cmark 0'
'set ccolor 4'
'set cthick 3'
'd sqrt('field'rms_dis.'n')'
'define qave = qave + 'field'rms_dis.'n')'
n = n + 1
endwhile
'define qave = qave/'numfiles
'set cmark 0'
'set cthick 8'
'set ccolor 1'
'd sqrt(qave)'

'getinfo date'
         date = result
        month = substr(date,6,3)

'draw xlab Forecast Day ('month')'
'draw ylab Root Mean Square Error 'unit

'set  vpage off'
'set  grads off'
'set  string 1 c 6'
'set  strsiz .15'
'draw string 6.25967 8.00 'level'-mb 'name'  'region
'draw string 6.25967 8.35 'expdsc


'lines 'geosutil'/plots/fcst/rms.stack 1'

'myprint -name 'output'/stats_'label'_rms_'reg'_'level'_'month' -rotate 90'

 return
