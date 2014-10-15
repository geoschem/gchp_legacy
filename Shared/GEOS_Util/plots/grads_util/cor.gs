function cor    (args)
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
    name  = field
    label = field

if( field = "p" )
    name  = "Sea-Level Pressure"
    label = "slp"
endif
if( field = "h" )
    name  = "Heights"
    label = "hght"
endif
if( field = "u" )
    name  = "U-Wind"
    label = "uwnd"
endif
if( field = "v" )
    name  = "V-Wind"
    label = "vwnd"
endif
if( field = "t" )
    name  = "Temperature"
    label = "tmpu"
endif
if( field = "q" )
    name  = "Specific Humidity"
    label = "sphu"
endif

if( field = 'tau' ) ; name = 'Total Aerosol'  ; endif
if( field = 'du'  ) ; name = 'Dust'           ; endif
if( field = 'ss'  ) ; name = 'Sea Salt'       ; endif
if( field = 'bc'  ) ; name = 'Black Carbon'   ; endif
if( field = 'oc'  ) ; name = 'Organic Carbon' ; endif
if( field = 'su'  ) ; name = 'Sulfate'        ; endif


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

* Plot each individual forecast while computing mean and Fisher Transform (to force Gaussian Distribution)
* --------------------------------------------------------------------------------------------------------
'define qave = lev-lev'
'define zave = lev-lev'
n = 1
while ( n <= numfiles )
'set grads off'
'set dfile 'n
'set axlim 0.6 1.1'
'set ylab %.2f'
'set t 'tbeg' 'tdim
'set xaxis 0 'nday' .5'
'set cmark 0'
'set ccolor 2'
'set cthick 1'
'd 'field'cor.'n
'define qave = qave + 'field'cor.'n
'define z'n' = 0.5*log( (1+'field'cor.'n')/(1-'field'cor.'n'+5.0e-6) )'
'define zave = zave + z'n
n = n + 1
endwhile

'define qave = qave/'numfiles
'define zave = zave/'numfiles

* Compute Standard Deviations
* ---------------------------
'define qvar = lev-lev'
'define zvar = lev-lev'
n = 1
while ( n <= numfiles )
'define qvar = qvar + pow( 'field'cor.'n'-qave,2)'
'define zvar = zvar + pow(           z'n'-zave,2)'
n = n + 1
endwhile

if( numfiles > 1 )
   'define qvar = qvar/('numfiles'-1)'
   'define zvar = zvar/('numfiles'-1)'
endif
   'define qstd = sqrt( qvar )'
   'define zstd = sqrt( zvar )'


* Compute Fisher Mean and +/- 1 Standard Deviation
* ------------------------------------------------
'define rave = (exp(2*zave)-1)/(exp(2*zave)+1)'
'define rU   = (exp(2*(zave+zstd))-1)/(exp(2*(zave+zstd))+1)'
'define rL   = (exp(2*(zave-zstd))-1)/(exp(2*(zave-zstd))+1)'


* Estimate Statistically Significant Range (based on 30 Forecasts and 95% confidence)
* -----------------------------------------------------------------------------------
'define se   = sqrt( 2*pow(zstd,2)/'numfiles' )'
'define dx   = 1.3*se'
'define rUc  = (exp(2*(zave+dx  ))-1)/(exp(2*(zave+dx  ))+1)'
'define rLc  = (exp(2*(zave-dx  ))-1)/(exp(2*(zave-dx  ))+1)'


'set t 'tbeg' 'tdim


* Plot Straight Mean and +/- 1 Std Dev
* ------------------------------------
'set xaxis 0 'nday' .5'
'set cmark 0'
'set cthick 8'
'set cstyle 1'
'set ccolor 1'
*'d qave'
'set cmark 0'
'set cthick 3'
'set cstyle 3'
'set ccolor 1'
*'d qave+qstd'
'set cmark 0'
'set cthick 3'
'set cstyle 3'
'set ccolor 1'
*'d qave-qstd'

* Plot Fisher Mean and +/- 1 Std Dev
* ----------------------------------
'set cmark 0'
'set cthick 8'
'set cstyle 1'
'set ccolor 1'
'd rave'
'set cmark 0'
'set cthick 6'
'set cstyle 3'
'set ccolor 1'
'd rU'
'set cmark 0'
'set cthick 6'
'set cstyle 3'
'set ccolor 1'
'd rL'

* Plot 95% Confidence Intervals
* -----------------------------
'set cmark 0'
'set cthick 8'
'set cstyle 2'
'set ccolor 4'
'd rUc'
'set cmark 0'
'set cthick 8'
'set cstyle 2'
'set ccolor 4'
'd rLc'


* Plot End Point Values
* ---------------------
'set t 'tdim
'd rave'
val0 = subwrd(result,4)
'd rU'
valU = subwrd(result,4)
'd rL'
valL = subwrd(result,4)
'd rUc'
valUc = subwrd(result,4)
'd rLc'
valLc = subwrd(result,4)

'getinfo date'
         date = result
        month = substr(date,6,3)
'set t 'tbeg' 'tdim

'q w2xy 'date' 'val0
    y0 = subwrd(result,6)
'q w2xy 'date' 'valU
    yU = subwrd(result,6)
'q w2xy 'date' 'valL
    yL = subwrd(result,6)
'q w2xy 'date' 'valUc
    yUc = subwrd(result,6)
'q w2xy 'date' 'valLc
    yLc = subwrd(result,6)

'draw xlab Forecast Day ('month')'
'draw ylab Anomaly Correlation'

'set  vpage off'
'set  grads off'
'set  string 1 c 6'
'set  strsiz .15'
'draw string 6.25967 8.00 'level'-mb 'name'  'region
'draw string 6.25967 8.35 'expdsc

'set string 1 l 5'
'set strsiz .08'
'draw string 10.5477 'y0 ' 'val0
'draw string 10.5477 'yU ' 'valU
'draw string 10.5477 'yL ' 'valL
'set string 4 l 5'
'draw string 10.5477 'yUc' 'valUc
'draw string 10.5477 'yLc' 'valLc

'!remove cor.stk'
'!cat 'geosutil'/plots/fcst/cor.stack | sed -e "s/@NN/'numfiles'/g" > cor.stk'
'lines cor.stk 1'

'myprint -name 'output'/stats_'label'_cor_'reg'_'level'_'month' -rotate 90'

 return
