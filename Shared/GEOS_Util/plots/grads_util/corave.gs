function cor    (args)
field  = subwrd (args,1)
exptot = subwrd (args,2)
expnum = subwrd (args,3)
expcol = subwrd (args,4)
fpm    = subwrd (args,5) ; files_per_month = fpm

'rgbset'

axmin = 0.60
axmax = 1.10

filebeg  = 1 + (expnum-1)*files_per_month

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

if( nday = 5 )
    axmin = 0.72
    axmax = 1.08
    axmin = 0.80
    axmax = 1.05
    axmin = 0.6
    axmax = 1.1
endif
if( nday = 4 )
    axmin = 0.90
    axmax = 1.04
endif
if( nday = 3 )
    axmin = 0.96
    axmax = 1.01
    axmin = 0.90
    axmax = 1.04
endif
if( nday = 2 )
    axmin = 0.97
    axmax = 1.01
endif
if( nday = 1.5 )
    axmin = 0.980
    axmax = 1.005
endif


* Initialize Plot Values
* ----------------------
if( field = "p" )
    name  = "Sea-Level Pressure"
    unit  = "(mb)"
    label = "slp"
    clevs = ".50 .53 .56 .60 .63 .66 .70 .73 .76 .80 .82 .84 .86 .88 .90 .92 .94 .96 .98 1.00"
endif
if( field = "h" )
    name  = "Heights"
    unit  = "(m)"
    label = "hght"
    clevs = ".50 .53 .56 .60 .63 .66 .70 .73 .76 .80 .82 .84 .86 .88 .90 .92 .94 .96 .98 1.00"
endif
if( field = "u" )
    name  = "U-Wind"
    unit  = "(m/sec)"
    label = "uwnd"
    clevs = ".50 .53 .56 .60 .63 .66 .70 .73 .76 .80 .82 .84 .86 .88 .90 .92 .94 .96 .98 1.00"
endif
if( field = "v" )
    name  = "V-Wind"
    unit  = "(m/sec)"
    label = "vwnd"
    clevs = ".50 .53 .56 .60 .63 .66 .70 .73 .76 .80 .82 .84 .86 .88 .90 .92 .94 .96 .98 1.00"
endif
if( field = "t" )
    name  = "Temperature"
    unit  = "(K)"
    label = "tmpu"
    clevs = ".50 .53 .56 .60 .63 .66 .70 .73 .76 .80 .82 .84 .86 .88 .90 .92 .94 .96 .98 1.00"
endif
if( field = "q" )
    name  = "Specific Humidity"
    unit  = "(g/g)"
    label = "sphu"
    clevs = ".50 .53 .56 .60 .63 .66 .70 .73 .76 .80 .82 .84 .86 .88 .90 .92 .94 .96 .98 1.00"
endif


'getinfo xpos'
         xpos  = result
'getinfo level'
         level = result
'getinfo numfiles'
         numfiles = result

fileend  = numfiles
fileend  = filebeg + files_per_month - 1
numfiles = fileend-filebeg+1

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


* Initialize Plot
* ---------------
            n = filebeg
'set dfile 'n

'getinfo tdim'
         tdim = result
'getinfo tinc'
         tinc = result

         tmax = 1 + 5*(24/tinc)
         tbeg = 1 -(tmax-tdim)

         tdim = tbeg + nday*(24/tinc)
'set t ' tbeg ' 'tdim
'define zave = lev-lev'

* Plot each individual forecast while computing mean and Fisher Transform (to force Gaussian Distribution)
* --------------------------------------------------------------------------------------------------------
        n  = filebeg
while ( n <= fileend )
'set dfile 'n
'set t 'tbeg' 'tdim
'define z'n' = 0.5*log( (1+'field'cor.'n')/(1-'field'cor.'n'+5.0e-6) )'
n = n + 1
endwhile

        n  = filebeg
while ( n <= fileend )
'set dfile 'n
'set t 'tbeg' 'tdim
'define zave = zave + z'n
n = n + 1
endwhile
'define zave = zave/'numfiles

* Compute Standard Deviations
* ---------------------------
'define zvar = lev-lev'
        n  = filebeg
while ( n <= fileend )
'define zvar = zvar + pow(           z'n'-zave,2)'
n = n + 1
endwhile

if( numfiles > 1 )
   'define zvar = zvar/('numfiles'-1)'
endif
   'define zstd = sqrt( zvar )'


* Compute Fisher Mean and +/- 1 Standard Deviation
* ------------------------------------------------
'define rave = (exp(2*zave)-1)/(exp(2*zave)+1)'
'define rU   = (exp(2*(zave+zstd))-1)/(exp(2*(zave+zstd))+1)'
'define rL   = (exp(2*(zave-zstd))-1)/(exp(2*(zave-zstd))+1)'


* Estimate Statistically Significant Range (based on two-tailed Students T-Test)
* ------------------------------------------------------------------------------
 dof = numfiles-1    ;* Degrees of Freedom (dof)
'astudt 'dof' 0.05'  ;* 95% Confidence
'astudt 'dof' 0.10'  ;* 90% Confidence
'q defval astudtout 1 1'
critval=subwrd(result,3)

'define se  = sqrt( 2*zvar/'numfiles' )'
'define dx  = se*'critval
'define rUc = (exp(2*(zave+dx))-1)/(exp(2*(zave+dx))+1)'
'define rLc = (exp(2*(zave-dx))-1)/(exp(2*(zave-dx))+1)'



* Plot Fisher Mean and +/- 1 Std Dev
* ----------------------------------
'set t 'tbeg' 'tdim
'set grads off'
'set ylab %.3f'
'set axlim 'axmin' 'axmax
'set xaxis 0 'nday' .5'
'set cmark 0'
'set cthick 8'
'set cstyle 1'

'set ccolor 'expcol
'd rave'

* Plot 95% Confidence Intervals
* -----------------------------
if( expnum = 1 )
'set cmark 0'
'set cthick 2'
'set cstyle 3'
'set ccolor 4'
 'd rUc'
'set cmark 0'
'set cthick 2'
'set cstyle 3'
'set ccolor 4'
 'd rLc'
endif


* Store End Point Values
* ----------------------
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
'set  strsiz .17'
'draw string 6.25967 8.00 'level'-mb 'name'  'region

* Plot Fisher Mean and +/- 1 Std Dev Values
* -----------------------------------------
'set string 1 l 5'
'set strsiz .08'
 'draw string 10.5477 'y0 ' 'val0
*'draw string 10.5477 'yU ' 'valU
*'draw string 10.5477 'yL ' 'valL

* Plot 95% Confidence Intervals Values
* ------------------------------------
if( expnum = 1 )
'set string 4 l 5'
 'draw string 10.5477 'yUc' 'valUc
 'draw string 10.5477 'yLc' 'valLc
endif

if( expnum = exptot )
'lines stats.stk 1'
'myprint -name 'output'/stats_'label'_cor_'reg'_'level'_'month' -rotate 90'
endif

return
