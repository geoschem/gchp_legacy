function corcmp (args)

'numargs  'args
 numargs = result

'run getenv SOURCE'
        SOURCE = result

field = h
desc  = ''

       num = 0
while( num < numargs )
       num = num + 1
if( subwrd(args,num)='-field'  ) ; field  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-numexp' ) ; numexp = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-lev'    ) ; lev    = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-desc'   ) ; desc   = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-debug'  ) ; debug  = subwrd(args,num+1) ; endif
endwhile
                                   mexps  = numexp-1
       num = 0
while( num < numargs )
       num = num + 1
       m = 0
       while( m<=mexps )
       if( subwrd(args,num)='-desc'm  ) ; expdsc.m = subwrd(args,num+1) ; endif
       m = m + 1
       endwhile
endwhile

***********************************************************
*****                                                 *****
*****  Note:  numexp is the Total Number of  EXPs     *****
*****                (including the Control, EXP0)    *****
*****                                                 *****
*****         mexps is the Number of Comparisons      *****
*****                ( numexp - 1 )                   *****
*****                                                 *****
***********************************************************

'getinfo numfiles'
files_per_month = result/numexp

expcmp.0 = 1
expcol.0 = 1
       m = 1
while( m <= mexps )
expcmp.m = m+1
expcol.m = m+1
       m = m+1
endwhile


say '   Exp0: 'expcmp.0'  Color: 'expcol.0
       m = 1
while( m <= mexps )
say '   Exp'm': 'expcmp.m'  Color: 'expcol.m
       m = m+1
endwhile

say 'MEXPs = 'mexps
say 'FPM: 'files_per_month

'rgbset'
'run setenv "ANTIALIAS" NULL'


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


filebeg  = 1
fileend  = filebeg + files_per_month - 1
numfiles = fileend-filebeg+1


* Compute Beginning Times Relative to Control (1st File) across ALL Experiments
* -----------------------------------------------------------------------------
       m = 0
while( m<=mexps )
'set dfile 1'
'set t 1'
            n   = filebeg
            n.m = n + m*numfiles
'set dfile 'n.m

'q dims'
   tline    = sublin(result,5)
   toff.m.n = subwrd(tline,9)
say 'toffset:  m = 'm'  n = 'n'  toff = 'toff.m.n
m = m+1
endwhile

m = 0
while( m<=mexps )
        n   = filebeg
while ( n  <= fileend )
        n.m = n + m*numfiles
'set dfile 1'
'set t 1'
'set dfile 'n.m
'q dims'
tline       = sublin(result,5)
toffset.n.m = subwrd(tline,9)
*say 'toffset:  m: 'm'  n: 'n'  file(n.m) = 'n.m'  toffset = 'toffset.n.m
n = n + 1
endwhile
m = m + 1
endwhile

* Determine Months from Ensembles
* -------------------------------
month  = ''
months = ''
        n   = filebeg
while ( n  <= fileend )
'set dfile 'n
'set t 1'
'getinfo date'
         date = result
        dummy = substr(date,6,3)
    if( dummy != month )
        month  = dummy
        if( months = '' )
            months = month
         else
            months = months'-'month
         endif
     endif
n = n + 1
endwhile
say 'Months Used in Forecasts: 'months

* Define NDAY and NDAYMAX across ALL Experiments
* ----------------------------------------------
 ndaymax = 999
       m = 0
while( m<=mexps )
            n   = filebeg
            n.m = n + m*numfiles
'set dfile 'n.m

'run getinfo tinc'
             tinc = result
         if( tinc = "NULL" ) ; tinc = 6 ; endif

'run getinfo tdim'
             tdum = result - toff.m.n
             nday = tdum * tinc / 24
         if( nday < ndaymax ) ; ndaymax = nday ; endif
m = m+1
endwhile

'run getenv "NDAY"'
             nday = result
         if( nday = "NULL" ) ; nday = ndaymax ; endif
         if( nday > ndaymax) ; nday = ndaymax ; endif
say 'NDAY: ' nday

* Determine TDIM based on TINC from Experiment Files for Diff Calculations
* ------------------------------------------------------------------------
   m = 0
while( m<=mexps )
            n   = filebeg
            n.m = n + m*numfiles
'set dfile 'n.m
'getinfo tdim'
         tdim = result
'getinfo tinc'
         tinc = result

         ndayloc = (tdim-toff.m.n) * tinc / 24
         tmax    = toff.m.n + ndayloc*(24/tinc)
         tbeg.m  = toff.m.n -(tmax-tdim)
         tdim.m  = tbeg.m + nday*(24/tinc)

if( tdim.m < tdim.0 )
    tdif.m = tdim.m
    ddif.m = n.m
else
    tdif.m = tdim.0
    ddif.m = 1
endif
say 'tbeg.'m': 'tbeg.m'  tdim'm': 'tdim.m'  tinc: 'tinc'  tdif.'m': 'tdif.m

m = m+1
endwhile

* Find LCD for TDIM among all Experiment Files for Synoptic Average Stats
* -----------------------------------------------------------------------
 tmin = tdim.0
dfile = 1
    m = 1
while( m<=mexps )
            n   = filebeg
            n.m = n + m*numfiles
            d.m = 1 + m*numfiles
if( tdim.m < tmin )
    tmin   = tdim.m
    dfile  = d.m
endif
m = m+1
endwhile

'set dfile 1'
'getinfo undef'
         undef = result
'    set undef ' undef

* Initialize Variables to Zero
* ----------------------------
   m = 0
while( m<=mexps )
            n   = filebeg
            n.m = n + m*numfiles
'set dfile 'n.m
'set t  'tbeg.m' 'tdim.m
'define  zave'm' = lev-lev'
'define  zvar'm' = lev-lev'
m = m+1
endwhile

   m = 0
while( m<=mexps )
            n   = filebeg
            n.m = n + m*numfiles
'set dfile 'ddif.m
'set t  'tbeg.m' 'tdif.m
'define zaved'm' = lev-lev'
'define zvard'm' = lev-lev'
m = m+1
endwhile

* Plot each individual forecast while computing mean and Fisher Transform (to force Gaussian Distribution)
* --------------------------------------------------------------------------------------------------------

*say 'Redfine field'
m = 0
while( m<=mexps )
        n   = filebeg
while ( n  <= fileend )
        n.m = n + m*numfiles
        d.m = 1 + m*numfiles
*'set dfile 'n.m
'set dfile 'd.m
'set t  'tbeg.m' 'tdim.m
 toffset = 1-toffset.n.m
*say 'dfile 'd.m'  tbeg: 'tbeg.m'  tend: 'tdim.m'  toffset: 'toffset'  'field'cor'n.m' = 'field'cor.'n.m'(t+'toffset')'

'define 'field'cor'n.m' = 'field'cor.'n.m'(t+'toffset')'
n = n + 1
endwhile
m = m + 1
endwhile
*pull flag

* Define New Fisher Transform Variable (to force Gaussian Distribution)
* ---------------------------------------------------------------------
*say 'Compute Fisher Transform'
m = 0
while( m<=mexps )
        n   = filebeg
while ( n  <= fileend )
        n.m = n + m*numfiles
        d.m = 1 + m*numfiles
'set dfile 'd.m
'set t  'tbeg.m' 'tdim.m
'define     q'm' = 'field'cor'n.m
*'d q'm
'define z'n'e'm' = 0.5*log( (1+ q'm')/(1- q'm'+5.0e-6) )'
n = n + 1
endwhile
m = m + 1
endwhile
*pull flag

*say 'Compute CTL and DUM'
m = 0
while( m<=mexps )
        n   = filebeg
while ( n  <= fileend )
        n.m = n + m*numfiles
'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'define      ctl  = 'field'cor'n
'define      dum  = 'field'cor'n.m
'define     dq'm' = 0.5*(ctl-dum)'
'define zd'n'e'm' = 0.5*log( (1+dq'm')/(1-dq'm') )'
n = n + 1
endwhile
m = m + 1
endwhile
*pull flag

* Compute Mean
* ------------
*say 'Compute ZAVE'
m = 0
while( m<=mexps )
        n  = filebeg
while ( n <= fileend )
        n.m = n + m*numfiles
        d.m = 1 + m*numfiles
'set dfile 'd.m
'set t 'tbeg.m' 'tdim.m
'define  zave'm' =  zave'm' +  z'n'e'm
n = n + 1
endwhile
'define  zave'm' =  zave'm'/'numfiles
*'d zave'm
m = m + 1
endwhile
*pull flag

*say 'Compute ZAVED'
m = 0
while( m<=mexps )
        n  = filebeg
while ( n <= fileend )
'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'define zaved'm' = zaved'm' + zd'n'e'm
n = n + 1
endwhile
'define zaved'm' = zaved'm'/'numfiles
m = m + 1
endwhile
*pull flag


* Compute Variance and Standard Deviations
* ----------------------------------------
m = 0
while( m<=mexps )
        n  = filebeg
while ( n <= fileend )
        n.m = n + m*numfiles
        d.m = 1 + m*numfiles
'set dfile 'd.m
'set t 'tbeg.m' 'tdim.m
'define  zvar'm' =  zvar'm' + pow(  z'n'e'm'- zave'm',2 )'
n = n + 1
endwhile
'define  zvar'm' =  zvar'm'/('numfiles'-1)'
'define  zstd'm' = sqrt(  zvar'm' )'
m = m + 1
endwhile

m = 0
while( m<=mexps )
        n  = filebeg
while ( n <= fileend )
'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'define zvard'm' = zvard'm' + pow( zd'n'e'm'-zaved'm',2 )'
n = n + 1
endwhile
'define zvard'm' = zvard'm'/('numfiles'-1)'
'define zstdd'm' = sqrt( zvard'm' )'
m = m + 1
endwhile


* Compute Fisher Mean
* -------------------
m = 0
while( m<=mexps )
        n  = filebeg
        n.m = n + m*numfiles
        d.m = 1 + m*numfiles
'set dfile 'd.m
'set t 'tbeg.m' 'tdim.m
'define rave'm' = (exp(2*zave'm')-1) / (exp(2*zave'm')+1)'
m = m + 1
endwhile


* Compute Confidence Intervals for Two-Tailed Students T-Test Distributions
* -------------------------------------------------------------------------
 dof = numfiles-1    ;* Degrees of Freedom (dof)

'astudt 'dof' 0.32'  ;* 68% Confidence
'q defval astudtout 1 1'
critval68 = subwrd(result,3)

'astudt 'dof' 0.10'  ;* 90% Confidence
'q defval astudtout 1 1'
critval90 = subwrd(result,3)

'astudt 'dof' 0.05'  ;* 95% Confidence
'q defval astudtout 1 1'
critval95 = subwrd(result,3)

'astudt 'dof' 0.01'  ;* 99% Confidence
'q defval astudtout 1 1'
critval99 = subwrd(result,3)


* Estimate Statistically Significant Range for Synoptic Variability from Average of All Experiment
* ------------------------------------------------------------------------------------------------
'set dfile 'dfile
'set t 1   'tmin
'define zvarave = lev-lev'
       m = 1
while( m<=mexps )
*'set dfile 'ddif.m
*'set t 'tbeg.m' 'tdif.m
'define zvarave = zvarave + zvar'm
      m = m + 1
endwhile
'define zvarave = zvarave / 'mexps
'define se = sqrt( (zvar0 + zvarave)/'numfiles' )'
'define dx = se*'critval90
'define rUave = (exp( 2*(zave0+dx))-1)/(exp( 2*(zave0+dx))+1)'
'define rLave = (exp( 2*(zave0-dx))-1)/(exp( 2*(zave0-dx))+1)'


* Estimate Statistically Significant Range for Zero-Mean Hypothesis in a Paired t-Test
* ------------------------------------------------------------------------------------
m = 1
while( m<=mexps )
'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'define se  = sqrt( zvard'm'/'numfiles' )'

'define dx       = se*'critval68
'define rUp68'm' = 2*(exp( 2*dx)-1)/(exp( 2*dx)+1)'
'define rLp68'm' = 2*(exp(-2*dx)-1)/(exp(-2*dx)+1)'

'define dx       = se*'critval90
'define rUp90'm' = 2*(exp( 2*dx)-1)/(exp( 2*dx)+1)'
'define rLp90'm' = 2*(exp(-2*dx)-1)/(exp(-2*dx)+1)'

'define dx       = se*'critval95
'define rUp95'm' = 2*(exp( 2*dx)-1)/(exp( 2*dx)+1)'
'define rLp95'm' = 2*(exp(-2*dx)-1)/(exp(-2*dx)+1)'

'define dx       = se*'critval99
'define rUp99'm' = 2*(exp( 2*dx)-1)/(exp( 2*dx)+1)'
'define rLp99'm' = 2*(exp(-2*dx)-1)/(exp(-2*dx)+1)'

m = m + 1
endwhile


* Find Minimum End-Point Value
* ----------------------------
minval = 1.0
m = 0
while( m<=mexps )
        n  = filebeg
        n.m = n + m*numfiles
        d.m = n + m*numfiles
'set dfile 'n.m
'set dfile 'd.m
'set t 'tdim.m
'd rave'm
val = subwrd(result,4)
if( val < minval ) ; minval = val ; endif
m = m + 1
endwhile

'd rLave'
val = subwrd(result,4)
if( val < minval ) ; minval = val ; endif

        offset = 0.0025
        axmin  = (1.000 - offset) * minval
        axmax  =  1.000 + offset  * minval

    if( minval > 0.60 )
        axmax  = 1.10
        axmin  = 0.60
    endif
    if( minval > 0.65 )
        axmax  = 1.10
        axmin  = 0.65
    endif
    if( minval > 0.70 )
        axmax  = 1.10
        axmin  = 0.70
    endif
    if( minval > 0.75 )
        axmax  = 1.10
        axmin  = 0.75
    endif
    if( minval > 0.80 )
        axmax  = 1.05
        axmin  = 0.80
    endif
    if( minval > 0.85 )
        axmax  = 1.05
        axmin  = 0.85
    endif
    if( minval > 0.90 )
        axmax  = 1.04
        axmin  = 0.90
    endif
    if( minval > 0.96 )
        axmax  = 1.01
        axmin  = 0.96
    endif
    if( minval > 0.98 )
        axmax  = 1.004
        axmin  = 0.980
    endif
    if( minval > 0.99 )
        axmax  = 1.004
        axmin  = 0.990
    endif

say ' AXMAX: 'axmax
say ' AXMIN: 'axmin
say 'MINVAL: 'minval

* Plot Fisher Mean for Experiments
* --------------------------------
        m  = 0
while( m<=mexps )
        n  = filebeg
        n.m = n + m*numfiles
        d.m = n + m*numfiles
'set dfile 'n.m
'set dfile 'd.m
'set t 'tbeg.m' 'tdim.m
'set vpage off'
'set grads off'
'set parea 2.25 9.75 4.0 7.5'
'set axlim 'axmin' 'axmax
'set xaxis 0 'nday' .5'
'set ylab %.3f'
'set xlopts 0'
'set cmark  0'
'set cthick 6'
'set cstyle 1'
'set ccolor 'expcol.m
'd rave'm
m = m + 1
endwhile
'draw ylab Anomaly Correlation'


* Plot 90% Confidence Intervals for Synoptic Variance from Average of All Experiment
* ----------------------------------------------------------------------------------
'set cmark 0'
'set cthick 2'
'set cstyle 3'
'set ccolor 4'
'd rUave'

'set cmark 0'
'set cthick 2'
'set cstyle 3'
'set ccolor 4'
'd rLave'

* Compute and Display Fisher Mean End-Point Values
* ------------------------------------------------
'getinfo year'
         year = result
'getinfo date'
         date = result
m = 0
while( m<=mexps )
        n  = filebeg
        n.m = n + m*numfiles
'set dfile 'n.m
'set t 'tdim.m
'd rave'm
valr.m = subwrd(result,4)
'q w2xy 'date' 'valr.m
    y.m = subwrd(result,6)
  col.m = expcol.m
m = m + 1
endwhile

* Sort Fisher Mean End-Point Values
* ---------------------------------
m = 0
while( m<=mexps )
  n = m+1
  while( n<=mexps )
  if( y.n < y.m )
      dum = y.m
      y.m = y.n
      y.n = dum
      dum    = valr.m
      valr.m = valr.n
      valr.n = dum
       dum   = col.m
       col.m = col.n
       col.n = dum
  endif
  n = n+1
  endwhile
m = m+1
endwhile

* Plot Fisher Mean End-Point Values
* ---------------------------------
yloc = 0
m = 0
while( m<=mexps )
'set string 'col.m' l 4'
'set strsiz .08'
if( y.m-yloc < 0.1 )
    yloc = yloc + 0.1
else 
    yloc = y.m
endif
'draw string 9.80 'yloc' 'valr.m
m = m + 1
endwhile

* Compute Upper and Lower Bounds for 90% Confidence Interval Values for Synoptic Variability
* ------------------------------------------------------------------------------------------
'set dfile 'dfile
'set t 'tmin
'd rUave'
valrU = subwrd(result,4)
'd rLave'
valrL = subwrd(result,4)

* Compute Upper & Lower Bounds for Confidence Interval Values for Paired Hypothesis Test
* --------------------------------------------------------------------------------------
m = 1
while( m<=mexps )
'set dfile 'ddif.m
'set t 'tdif.m

'd rUp68'm
valrUp68.m = subwrd(result,4)
'd rLp68'm
valrLp68.m = subwrd(result,4)

'd rUp90'm
valrUp90.m = subwrd(result,4)
'd rLp90'm
valrLp90.m = subwrd(result,4)

'd rUp95'm
valrUp95.m = subwrd(result,4)
'd rLp95'm
valrLp95.m = subwrd(result,4)

'd rUp99'm
valrUp99.m = subwrd(result,4)
'd rLp99'm
valrLp99.m = subwrd(result,4)

'set t 'tbeg.m' 'tdif.m
'minmax rave'm'-rave0'
raveMX.m = subwrd(result,1)
raveMN.m = subwrd(result,2)
'set t 'tdif.m
say 'tdif.'m': 'tdif.m' 68% Confidence rUp: 'valrUp68.m' rLp: 'valrLp68.m' raveMN: 'raveMN.m'  raveMX: 'raveMX.m
say 'tdif.'m': 'tdif.m' 90% Confidence rUp: 'valrUp90.m' rLp: 'valrLp90.m' raveMN: 'raveMN.m'  raveMX: 'raveMX.m
say 'tdif.'m': 'tdif.m' 95% Confidence rUp: 'valrUp95.m' rLp: 'valrLp95.m' raveMN: 'raveMN.m'  raveMX: 'raveMX.m
say 'tdif.'m': 'tdif.m' 99% Confidence rUp: 'valrUp99.m' rLp: 'valrLp99.m' raveMN: 'raveMN.m'  raveMX: 'raveMX.m
m = m + 1
endwhile


* Plot Difference plus Significance
* ---------------------------------
axfac = 1.2
axmax = valrUp90.1*axfac
axmin = valrLp90.1*axfac

* Compute Axis Limits based on Error Bars
* ---------------------------------------
if( mexps>1 )
    m = 2
    while( m<=mexps )
    if( valrUp90.m*axfac > axmax ) ; axmax = valrUp90.m*axfac ; endif
    if( valrLp90.m*axfac < axmin ) ; axmin = valrLp90.m*axfac ; endif
    m = m + 1
    endwhile
else
    axmax = valrUp99.1*axfac
    axmin = valrLp99.1*axfac
endif

* Modify Axis Limits based on Actual Differences
* ----------------------------------------------
    m = 1
    while( m<=mexps )
    if( raveMX.m*axfac > axmax ) ; axmax = raveMX.m*axfac ; endif
    if( raveMN.m*axfac < axmin ) ; axmin = raveMN.m*axfac ; endif
    m = m + 1
    endwhile

axmax = axmax * 1000
axmin = axmin * 1000

m = 1
while( m<=mexps )
'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'set parea 2.25 9.75 1.0 4.0'
'set ylab %.2f'
'set axlim 'axmin' 'axmax
'set xaxis 0 'nday' .5'
'set gxout bar'
'set baropts outline'
'set bargap 0'
'set xlopts 1'

if( mexps=1 )
   'set ccolor 3'
   'set cstyle 1'
   'set cthick 3'
   'set bargap 10'
   'd rUp68'm'*1000;rLp68'm'*1000'

   'set ccolor 2'
   'set cstyle 1'
   'set cthick 7'
   'set bargap 30'
   'd rUp90'm'*1000;rLp90'm'*1000'

   'set ccolor 11'
   'set cstyle 1'
   'set cthick 5'
   'set bargap 55'
   'd rUp95'm'*1000;rLp95'm'*1000'

   'set gxout errbar'
   'set ccolor 1'
   'set cstyle 1'
   'set cthick 5'
   'set bargap 80'
   'd rUp99'm'*1000;rLp99'm'*1000'
else
   'set ccolor 'expcol.m
   'd rUp90'm'*1000;rLp90'm'*1000'
endif

m = m + 1
endwhile

m = 1
while( m<=mexps )
'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'set gxout line'
'set cstyle 1'
'set ccolor 1'
'set cmark  0'
'set cthick 1'
'd rave0-rave0'
'set cstyle 1'
'set ccolor 'expcol.m
'set cmark  3'
'set cthick 6'
'define drave = rave'm'-rave0'
'd drave*1000'
m = m + 1
endwhile

* Compute and Display Fisher Mean End-Point Values
* ------------------------------------------------
'getinfo year'
         year = result
'getinfo date'
         date = result
m = 1
while( m<=mexps )
        n  = filebeg
        n.m = n + m*numfiles
'set dfile 'n.m
'set t 'tdim.m
'd  (rave'm'-rave0)*1000'
valr.m = subwrd(result,4)
'q w2xy 'date' 'valr.m
    y.m = subwrd(result,6)
  col.m = expcol.m
m = m + 1
endwhile

* Sort Fisher Mean End-Point Values
* ---------------------------------
m = 1
while( m<=mexps )
  n = m+1
  while( n<=mexps )
  if( y.n < y.m )
      dum = y.m
      y.m = y.n
      y.n = dum
      dum    = valr.m
      valr.m = valr.n
      valr.n = dum
       dum   = col.m
       col.m = col.n
       col.n = dum
  endif
  n = n+1
  endwhile
m = m+1
endwhile

* Plot Fisher Mean End-Point Values
* ---------------------------------
yloc = 0
m = 1
while( m<=mexps )
'set string 'col.m' l 4'
'set strsiz .08'
if( y.m-yloc < 0.1 )
    yloc = yloc + 0.1
else 
    yloc = y.m
endif
'draw string 9.80 'yloc' 'valr.m
m = m + 1
endwhile

'draw ylab Difference (x10`a-3`n)'
*'draw xlab Forecast Day'

* Plot Labels
* -----------
'set  vpage off'
'set  grads off'
'set  string 1 c 6 0'
'set  strsiz .17'
'draw string 6.0 8.15 'desc
'draw string 6.0 7.8 'level'-mb 'name'  'region
'set  strsiz .12'
'draw string 6.0 0.72 Forecast Day'

'set  string 1 l 8 0'
'set  strsiz .32'
'run uppercase 'field
                FIELD = result 
'draw string 0.70 8.15 'reg
'draw string 0.70 7.78 'FIELD


'set  string 1 c 6 90'
'set  strsiz .18'
'draw string 0.80 4.1 'months' 'year

'set  string 1 l 6 0'

maxlength = 0
m = 0
while( m<=mexps )
length = getlength(expdsc.m' ('numfiles')')
say ''m'  'length
if( length > maxlength )
             maxlength = length
endif
m = m + 1
endwhile
say 'maxlength: 'maxlength

nplotx = 3
nploty = 3
pagex  = 9.000
pagey  = 0.375
footer = 0.150
deltax = 0.700
deltay = 0.300

xsize = maxlength * 0.9/16 + deltax
ysize = ( pagey-footer - (nploty-1)*deltay )/nploty

say 'xsize = 'xsize
say 'ysize = 'xsize

totalxsize = ( 3 * xsize ) + ( 2 * deltax )
bordrx = 1.5 * ( 11 - totalxsize )/2
say 'totalxsize = 'totalxsize
say 'borderx = 'bordrx

'set  string 1 l 5'
'set  strsiz .09'

nx = 1
while (nx <= nplotx)
ny = 1
while (ny <= nploty)

nplot = ny + (nx-1)*nploty
    m = nplot-1
if( m <= mexps )

xbeg = bordrx + (nx-1)*(xsize+deltax)
yend = pagey  - (ny-1)*(ysize+deltay)
xend = xbeg + xsize
ybeg = yend - ysize

say 'xbeg: 'xbeg'  xend: 'xend

if( xbeg < bordrx ) ; xbeg = bordrx ; endif
if( xend > pagex  ) ; xend = pagex  ; endif
if( ybeg < footer ) ; ybeg = footer ; endif

'set  line 'expcol.m' 1 6'
'draw line 'xbeg-0.5' 'ybeg' 'xbeg-0.1' 'ybeg
'draw string 'xbeg' 'ybeg' 'expdsc.m' ('numfiles')'

endif
ny = ny + 1
endwhile
nx = nx + 1
endwhile

'!/bin/mkdir -p 'SOURCE'/corcmp'

if( nday = ndaymax )
   'myprint -name 'SOURCE'/corcmp/stats_'label'_corcmp_'reg'_'level'_'months' -rotate 90 -density 100x100'
else
   'myprint -name 'SOURCE'/corcmp/stats_'label'_corcmp_'reg'_'level'_'months'_'nday'DAY -rotate 90 -density 100x100'
endif

if( debug = "TRUE" )
    say "Hit ENTER for next plot"
    pull flag
endif
'c'

return

function getlength (string)
tb = ""
i = 1
while (i<=80)
blank = substr(string,i,1)
if( blank = tb )
length = i-1
i = 81
else
i = i + 1
endif
endwhile
return length

