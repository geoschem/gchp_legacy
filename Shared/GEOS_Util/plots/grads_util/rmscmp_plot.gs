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
*****  Note:  numexp is the Total Number of           *****
*****         Experiments (including the Control)     *****
*****         being compared.                         *****
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
    axmin = -2
    axmax =  8
endif
if( field = "h" )
    name  = "Heights"
    unit  = "(m)"
    label = "hght"
    clevs = ".50 .53 .56 .60 .63 .66 .70 .73 .76 .80 .82 .84 .86 .88 .90 .92 .94 .96 .98 1.00"
    axmin = -10
    axmax =  110
endif
if( field = "u" )
    name  = "U-Wind"
    unit  = "(m/sec)"
    label = "uwnd"
    clevs = ".50 .53 .56 .60 .63 .66 .70 .73 .76 .80 .82 .84 .86 .88 .90 .92 .94 .96 .98 1.00"
    axmin = -2
    axmax =  16
endif
if( field = "v" )
    name  = "V-Wind"
    unit  = "(m/sec)"
    label = "vwnd"
    clevs = ".50 .53 .56 .60 .63 .66 .70 .73 .76 .80 .82 .84 .86 .88 .90 .92 .94 .96 .98 1.00"
    axmin = -2
    axmax =  16
endif
if( field = "t" )
    name  = "Temperature"
    unit  = "(K)"
    label = "tmpu"
    clevs = ".50 .53 .56 .60 .63 .66 .70 .73 .76 .80 .82 .84 .86 .88 .90 .92 .94 .96 .98 1.00"
    axmin = -1
    axmax =  6
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
             tdum = result - 1
             nday = tdum * tinc / 24
         if( nday < ndaymax ) ; ndaymax = nday ; endif
m = m+1
endwhile

'run getenv "NDAY"'
             nday = result
         if( nday = "NULL" ) ; nday = ndaymax ; endif
         if( nday > ndaymax) ; nday = ndaymax ; endif

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

         ndayloc = (tdim-1) * tinc / 24

         tmax = 1 + ndayloc*(24/tinc)
         tbeg.m = 1 -(tmax-tdim)
         tdim.m = tbeg.m + nday*(24/tinc)

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
if( tdim.m < tmin )
    tmin   = tdim.m
    dfile  = n.m
endif
m = m+1
endwhile

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

* Define New Fisher Transform Variable (to force Gaussian Distribution)
* ---------------------------------------------------------------------
m = 0
while( m<=mexps )
        n   = filebeg
while ( n  <= fileend )
        n.m = n + m*numfiles
'set dfile 'n.m
'set t  'tbeg.m' 'tdim.m
'define     q'm' = 'field'rms.'n.m
'define z'n'e'm' = q'm
n = n + 1
endwhile
m = m + 1
endwhile

m = 0
while( m<=mexps )
        n   = filebeg
while ( n  <= fileend )
        n.m = n + m*numfiles
'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'define      ctl  = 'field'rms.'n
'define      dum  = 'field'rms.'n.m
'define     dq'm' = (ctl-dum)'
'define zd'n'e'm' = dq'm
n = n + 1
endwhile
m = m + 1
endwhile

* Compute Mean
* ------------
m = 0
while( m<=mexps )
        n  = filebeg
while ( n <= fileend )
        n.m = n + m*numfiles
'set dfile 'n.m
'set t 'tbeg.m' 'tdim.m
'define  zave'm' =  zave'm' +  z'n'e'm
n = n + 1
endwhile
'define  zave'm' =  zave'm'/'numfiles
m = m + 1
endwhile

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


* Compute Variance and Standard Deviations
* ----------------------------------------
m = 0
while( m<=mexps )
        n  = filebeg
while ( n <= fileend )
'set dfile 'n.m
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
'set dfile 'n.m
'set t 'tbeg.m' 'tdim.m
'define rave'm' = zave'm
m = m + 1
endwhile


* Estimate Statistically Significant Range for Synoptic Variability from Average of All Experiment
* ------------------------------------------------------------------------------------------------
 dof = numfiles-1
'astudt 'dof' 0.2'  ;* 90% Confidence
'astudt 'dof' 0.1'  ;* 95% Confidence
'q defval astudtout 1 1'
critval=subwrd(result,3)

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
'define dx = se*'critval
'define rUave = zave0+dx'
'define rLave = zave0-dx'

* Estimate Statistically Significant Range for Zero-Mean Hypothesis in a Paired t-Test)
* -------------------------------------------------------------------------------------
m = 1
while( m<=mexps )
'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'define se  = sqrt( zvard'm'/'numfiles' )'
'define dx  = se*'critval
'define rUp'm' =  dx'
'define rLp'm' = -dx'
m = m + 1
endwhile


* Find Maximum End-Point Value
* ----------------------------
maxval = 0.0
m = 0
while( m<=mexps )
        n  = filebeg
        n.m = n + m*numfiles
'set dfile 'n.m
'set t 'tdim.m
'd rave'm
val = subwrd(result,4)
if( val > maxval ) ; maxval = val ; endif
m = m + 1
endwhile

'd rLave'
val = subwrd(result,4)
if( val > maxval ) ; maxval = val ; endif

maxval = 1.02 * maxval

        axmax  =   1.08 * maxval
        axmin  = - 0.08 * maxval

say ' AXMAX: 'axmax
say ' AXMIN: 'axmin
say 'MINVAL: 'minval

* Plot Fisher Mean for Experiments
* --------------------------------
        m  = 0
while( m<=mexps )
        n  = filebeg
        n.m = n + m*numfiles
'set dfile 'n.m
'set t 'tbeg.m' 'tdim.m
'set vpage off'
'set grads off'
'set parea 2.25 9.75 4.0 7.5'
'set axlim 'axmin' 'axmax
'set xaxis 0 'nday' .5'
'set ylab %.3f'
'set xlopts 0'
'set cmark  0'
'set cthick 8'
'set cstyle 1'
'set ccolor 'expcol.m
'd rave'm
m = m + 1
endwhile
'draw ylab Root Mean Square Error'


* Plot 95% Confidence Intervals for Synoptic Variance from Average of All Experiment
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
        month = substr(date,6,3)
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
'set string 'col.m' l 5'
'set strsiz .08'
if( y.m-yloc < 0.1 )
    yloc = yloc + 0.1
else 
    yloc = y.m
endif
'draw string 9.80 'yloc' 'valr.m
m = m + 1
endwhile

* Compute Upper and Lower Bounds for 95% Confidence Interval Values for Synoptic Variability
* ------------------------------------------------------------------------------------------
'set dfile 'dfile
'set t 'tmin
'd rUave'
valrU = subwrd(result,4)
'd rLave'
valrL = subwrd(result,4)

'q w2xy 'date' 'valrU
    yU = subwrd(result,6)
'q w2xy 'date' 'valrL
    yL = subwrd(result,6)

'set string 4 l 5'
*'draw string 9.80 'yU' 'valrU
*'draw string 9.80 'yL' 'valrL


* Compute Upper & Lower Bounds for 95% Confidence Interval Values for Paired Hypothesis Test
* ------------------------------------------------------------------------------------------
m = 1
while( m<=mexps )
'set dfile 'ddif.m
'set t 'tdif.m
'd rUp'm
valrUp.m = subwrd(result,4)
'd rLp'm
valrLp.m = subwrd(result,4)

'set t 'tbeg.m' 'tdif.m
'minmax rave'm'-rave0'
raveMX.m = subwrd(result,1)
raveMN.m = subwrd(result,2)
'set t 'tdif.m
say 'tdif.'m': 'tdif.m' rUp: 'valrUp.m' rLp: 'valrLp.m' raveMN: 'raveMN.m'  raveMX: 'raveMX.m
m = m + 1
endwhile


* Plot Difference plus Significance
* ---------------------------------
grey = 91

axfac = 1.2
axmax = valrUp.1*axfac
axmin = valrLp.1*axfac
m = 2
while( m<=mexps )
if( valrUp.m*axfac > axmax ) ; axmax = valrUp.m*axfac ; endif
if( valrLp.m*axfac < axmin ) ; axmin = valrLp.m*axfac ; endif
m = m + 1
endwhile
say 'EXP 0: axmin: 'axmin'  axmax: 'axmax
m = 1
while( m<=mexps )
if( raveMX.m*axfac > axmax ) ; axmax = raveMX.m*axfac ; endif
if( raveMN.m*axfac < axmin ) ; axmin = raveMN.m*axfac ; endif
say 'EXP 'm': axmin: 'axmin'  axmax: 'axmax
m = m + 1
endwhile

axmax = axmax * 1000
axmin = axmin * 1000
say 'AXmax: 'axmax
say 'AXmin: 'axmin

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
'set xlopts 1'
'set ccolor 'grey
'set ccolor 'expcol.m
'd rUp'm'*1000;rLp'm'*1000'
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
'set cthick 8'
'define drave = rave'm'-rave0'
'd drave*1000'

'q w2xy 'date' 'valrUp.m
    yrUp = subwrd(result,6)
'q w2xy 'date' 'valrLp.m
    yrLp = subwrd(result,6)

'set string 1 l 5'
*'draw string 9.80 'yrUp' 'valrUp.m
*'draw string 9.80 'yrLp' 'valrLp.m
m = m + 1
endwhile

'draw ylab Difference (x10`a-3`n)'
'draw xlab Forecast Day ('month' 'year')'

* Plot Labels
* -----------
'set  vpage off'
'set  grads off'
'set  string 1 c 6'
'set  strsiz .17'
'draw string 6.0 8.15 'desc
'draw string 6.0 7.8 'level'-mb 'name'  'region

'!remove rmscmp.stk'
'!touch  rmscmp.stk'
'!echo 'numexp' >> rmscmp.stk'
m = 0
while( m<=mexps )
'!echo 1 6 'expcol.m'                >> rmscmp.stk'
'!echo     'expdsc.m' \('numfiles'\) >> rmscmp.stk'
m = m + 1
endwhile
'!echo 2.32 >> rmscmp.stk'
'!echo 6.03 >> rmscmp.stk'
'!echo 5.62 >> rmscmp.stk'
'!echo 7.43 >> rmscmp.stk'
'lines         rmscmp.stk 1'

'!/bin/mkdir -p 'SOURCE'/corcmp'

if( nday = ndaymax )
   'myprint -name 'SOURCE'/corcmp/stats_'label'_rmscmp_'reg'_'level'_'month' -rotate 90 -density 100x100'
else
   'myprint -name 'SOURCE'/corcmp/stats_'label'_rmscmp_'reg'_'level'_'month'_'nday'DAY -rotate 90 -density 100x100'
endif

if( debug = "TRUE" )
    say "Hit ENTER for next plot"
    pull flag
endif
'c'

return
