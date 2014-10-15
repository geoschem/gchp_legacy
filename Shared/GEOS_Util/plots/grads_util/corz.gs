function corz   (args)
field  = subwrd (args,1)
expdsc = subwrd (args,2)

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
    clevs = ".70 .73 .76 .80 .82 .84 .86 .87 .88 .89 .90 .91 .92 .93 .94 .95 .96 .97 .98 .99 1.00"
endif
if( field = "u" )
    name  = "U-Wind"
    unit  = "(m/sec)"
    label = "uwnd"
    clevs = ".70 .73 .76 .80 .82 .84 .86 .87 .88 .89 .90 .91 .92 .93 .94 .95 .96 .97 .98 .99 1.00"
endif
if( field = "v" )
    name  = "V-Wind"
    unit  = "(m/sec)"
    label = "vwnd"
    clevs = ".70 .73 .76 .80 .82 .84 .86 .87 .88 .89 .90 .91 .92 .93 .94 .95 .96 .97 .98 .99 1.00"
endif
if( field = "t" )
    name  = "Temperature"
    unit  = "(K)"
    label = "tmpu"
    clevs = ".70 .73 .76 .80 .82 .84 .86 .87 .88 .89 .90 .91 .92 .93 .94 .95 .96 .97 .98 .99 1.00"
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
'define  qave = lev-lev'

n = 1
while ( n <= numfiles )
'set dfile 'n
'define qave = qave + 'field'cor.'n
n = n + 1
endwhile

'define qave = qave/'numfiles

'set csmooth on'

'set grads off'
'set xaxis 0 'nday' .5'
'set gxout shaded'
'set clevs 'clevs
'set ccols 59   58   57   56   55   49   47   45   44   37   36   34   33   32  31  21  22  23  24  25  26  27  28'
'd qave'
'set gxout contour'
'set ccolor 1'
'set clab off'
'set clevs 'clevs
'd qave'
'cbarn -snum 0.8 -xmid 5.9 -ymid 0.20'

'getinfo date'
         date = result
        month = substr(date,6,3)

'set string 1 c 6'
'set strsiz .13'
'draw string 5.87 0.65 Forecast Day ('month')'
'draw ylab Pressure (mb)'


'set vpage off'
'set grads off'
'set string 1 c 6'
'set strsiz .14'
'draw string 5.87278 8.47 'expdsc
'draw string 5.87278 8.37 'name':  Anomaly Correlation'
'set strsiz .12'
'draw string 5.87278 8.12 'region

'myprint -name 'output'/stats_'label'_cor_'reg'_z_'month' -rotate 90'

 return
