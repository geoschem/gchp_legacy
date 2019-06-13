function rmscmp (args)

'numargs  'args
 numargs = result

'run getenv SOURCE'
        SOURCE = result

field = h
desc  = ''
rms   = 0

       num = 0
while( num < numargs )
       num = num + 1
if( subwrd(args,num)='-field'  ) ; field  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-numexp' ) ; numexp = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-lev'    ) ; lev    = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-desc'   ) ; desc   = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-debug'  ) ; debug  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-rms'    ) ; rms    = subwrd(args,num+1) ; endif
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

if( rms = 0 ) ; say 'Standard RMS plots'       ; endif
if( rms = 1 ) ; say 'RMS for RANDOM Component' ; endif
if( rms = 2 ) ; say 'RMS for BIAS Component'   ; endif
if( rms = 3 ) ; say 'RMS for AMPLITUDE Error'  ; endif
if( rms = 4 ) ; say 'RMS for PHASE Error'      ; endif

* Note:  RMS_0**2 = RMS_1(RAN  DOM)**2  +  RMS_2(BAR=BIAS)**2
*                 = RMS_3(DIS=AMPL)     +  RMS_4(DSP=PHAZ)

'rgbset'
'run setenv "ANTIALIAS" NULL'


* Initialize Plot Values
* ----------------------
    scale = 1

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
    name  = "Specific Humidity (g/kg)"
    scale = 1e3
    unit  = "(g/kg)"
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
if( rms != 0 )
'define  zave0'm' = lev-lev'
'define  zvar0'm' = lev-lev'
endif
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
if( rms != 0 )
'define zaved0'm' = lev-lev'
'define zvard0'm' = lev-lev'
endif
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

'define 'field'rms'n.m'    = 'scale'*'field'rms.'n.m'(t+'toffset')'
'define 'field'rmsran'n.m' = 'scale'*'field'rms_ran.'n.m'(t+'toffset')'
'define 'field'rmsbar'n.m' = 'scale'*'field'rms_bar.'n.m'(t+'toffset')'
'define 'field'rmsdsp'n.m' = 'scale'*'scale'*'field'rms_dsp.'n.m'(t+'toffset')'
'define 'field'rmsdis'n.m' = 'scale'*'scale'*'field'rms_dis.'n.m'(t+'toffset')'
n = n + 1
endwhile
m = m + 1
endwhile
*pull flag

* Define New Fisher Transform Variable (to force Gaussian Distribution)
* ---------------------------------------------------------------------
* Note:  RMS_0**2 = RMS_1(RAN  DOM)**2  +  RMS_2(BAR=BIAS)**2
*                 = RMS_3(DIS=AMPL)     +  RMS_4(DSP=PHAZ)

*say 'Compute Fisher Transform'
m = 0
while( m<=mexps )
        n   = filebeg
while ( n  <= fileend )
        n.m = n + m*numfiles
        d.m = 1 + m*numfiles
'set dfile 'd.m
'set t  'tbeg.m' 'tdim.m
if( rms = 0 ) 
                'define  q'm' =           'field'rms'n.m
else
                'define q0'm' =           'field'rms'n.m
endif
if( rms = 1 ) ; 'define q'm' =            'field'rmsran'n.m       ; 'define z0'n'e'm' = q0'm ; endif
if( rms = 2 ) ; 'define q'm' =            'field'rmsbar'n.m       ; 'define z0'n'e'm' = q0'm ; endif
if( rms = 3 ) ; 'define q'm' = sqrt( abs( 'field'rmsdis'n.m' ) )' ; 'define z0'n'e'm' = q0'm ; endif
if( rms = 4 ) ; 'define q'm' = sqrt( abs( 'field'rmsdsp'n.m' ) )' ; 'define z0'n'e'm' = q0'm ; endif
                                                                    'define  z'n'e'm' =  q'm
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
if( rms = 0 )
                'define ctl  =       'field'rms'n
                'define dum  =       'field'rms'n.m
else
                'define ctl0 =       'field'rms'n
                'define dum0 =       'field'rms'n.m
endif
if( rms = 1 ) ; 'define ctl  =            'field'rmsran'n       ; 'define dum  =            'field'rmsran'n.m        ; 'define dq0'm' = (ctl0-dum0)' ; 'define zd0'n'e'm' = dq0'm ; endif
if( rms = 2 ) ; 'define ctl  =            'field'rmsbar'n       ; 'define dum  =            'field'rmsbar'n.m        ; 'define dq0'm' = (ctl0-dum0)' ; 'define zd0'n'e'm' = dq0'm ; endif
if( rms = 3 ) ; 'define ctl  = sqrt( abs( 'field'rmsdis'n' ) )' ; 'define dum  = sqrt( abs( 'field'rmsdis'n.m' ) ) ' ; 'define dq0'm' = (ctl0-dum0)' ; 'define zd0'n'e'm' = dq0'm ; endif
if( rms = 4 ) ; 'define ctl  = sqrt( abs( 'field'rmsdsp'n' ) )' ; 'define dum  = sqrt( abs( 'field'rmsdsp'n.m' ) ) ' ; 'define dq0'm' = (ctl0-dum0)' ; 'define zd0'n'e'm' = dq0'm ; endif
                                                                                                                       'define  dq'm' = (ctl  -dum)' ; 'define  zd'n'e'm' =  dq'm
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
        d.m = 1 + m*numfiles
'set dfile 'd.m
'set t 'tbeg.m' 'tdim.m
if( rms != 0 ) ; 'define zave0'm' =  zave0'm' +  z0'n'e'm ; endif
                 'define  zave'm' =   zave'm' +   z'n'e'm
n = n + 1
endwhile
if( rms != 0 ) ; 'define  zave0'm' =  zave0'm'/'numfiles ; endif
                 'define   zave'm' =   zave'm'/'numfiles
m = m + 1
endwhile

m = 0
while( m<=mexps )
        n  = filebeg
while ( n <= fileend )
'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
if( rms != 0 ) ; 'define zaved0'm' = zaved0'm' + zd0'n'e'm ; endif
                 'define  zaved'm' =  zaved'm' +  zd'n'e'm
n = n + 1
endwhile
if( rms != 0 ) ; 'define zaved0'm' = zaved0'm'/'numfiles ; endif
                 'define  zaved'm' =  zaved'm'/'numfiles
m = m + 1
endwhile


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
if( rms != 0 ) ; 'define  zvar0'm' =  zvar0'm' + pow(  z0'n'e'm'- zave0'm',2 )' ; endif
                 'define  zvar'm'  =  zvar'm'  + pow(  z'n'e'm' - zave'm',2 )'
n = n + 1
endwhile
if( rms != 0 ) ; 'define  zvar0'm' =  zvar0'm'/('numfiles'-1)' ; 'define  zstd0'm' = sqrt(  zvar0'm' )' ; endif
                 'define  zvar'm'  =  zvar'm' /('numfiles'-1)' ; 'define  zstd'm'  = sqrt(  zvar'm' )'
m = m + 1
endwhile

m = 0
while( m<=mexps )
        n  = filebeg
while ( n <= fileend )
'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
if( rms != 0 ) ; 'define zvard0'm' = zvard0'm' + pow( zd0'n'e'm'-zaved0'm',2 )' ; endif
                 'define zvard'm'  = zvard'm'  + pow( zd'n'e'm' -zaved'm',2 )'
n = n + 1
endwhile
if( rms != 0 ) ; 'define zvard0'm' = zvard0'm'/('numfiles'-1)' ; 'define zstdd0'm' = sqrt( zvard0'm' )' ; endif
                 'define zvard'm'  = zvard'm' /('numfiles'-1)' ; 'define zstdd'm'  = sqrt( zvard'm' )'
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
if( rms != 0 ) ; 'define rave0'm' = zave0'm ; endif
                 'define rave'm'  = zave'm
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
if( rms != 0 )
'define zvarave0 = lev-lev'
endif
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
'define rUave = zave0+dx'
'define rLave = zave0-dx'


* Estimate Statistically Significant Range for Zero-Mean Hypothesis in a Paired t-Test
* ------------------------------------------------------------------------------------
m = 1
while( m<=mexps )
'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'define se  = sqrt( zvard'm'/'numfiles' )'

'define dx       = se*'critval68
'define rUp68'm' =  dx'
'define rLp68'm' = -dx'

'define dx       = se*'critval90
'define rUp90'm' =  dx'
'define rLp90'm' = -dx'

'define dx       = se*'critval95
'define rUp95'm' =  dx'
'define rLp95'm' = -dx'

'define dx       = se*'critval99
'define rUp99'm' =  dx'
'define rLp99'm' = -dx'

m = m + 1
endwhile


* Find Maximum End-Point Value
* ----------------------------
maxval = 0.0
m = 0
while( m<=mexps )
        n  = filebeg
        n.m = n + m*numfiles
        d.m = n + m*numfiles
'set dfile 'n.m
'set dfile 'd.m
'set t 'tbeg.m' 'tdif.m
if( rms != 0 )
'minmax rave0'm
else
'minmax rave'm
endif
val = subwrd(result,1)

if( val > maxval ) ; maxval = val ; endif
'set t 'tdim.m
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
if( rms != 0 & m = 0 )
'set cmark  0'
'set cthick 6'
'set cstyle 1'
'set ccolor 99'
'd rave0'm
endif
m = m + 1
endwhile

if( rms = 0 )
'draw ylab Root Mean Square Error'
else
    if( rms = 1 ) ; 'draw ylab RMS for RANDOM Error'     ; endif
    if( rms = 2 ) ; 'draw ylab RMS for BIAS Error'       ; endif
    if( rms = 3 ) ; 'draw ylab RMS for AMPLITUDE Error'  ; endif
    if( rms = 4 ) ; 'draw ylab RMS for PHASE Error'      ; endif
endif



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
if( rms != 0 & m = 0 )
'd rave0'm
valr0.m = subwrd(result,4)
'q w2xy 'date' 'valr0.m
    y0.m = subwrd(result,6)
endif
m = m + 1
endwhile

* Sort Fisher Mean End-Point Values
* ---------------------------------
if( rms != 0 )
    nexps = mexps + 1
    y.nexps = y0.0
  col.nexps = 99
valr.nexps = valr0.0
else
    nexps = mexps
endif
m = 0
while( m<=nexps )
  n = m+1
  while( n<=nexps )
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
while( m<=nexps )
'set string 'col.m' l 4'
'set strsiz .08'
say 'if( 'y.m'-'yloc' < 0.1 )'
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


* Compute Upper & Lower Bounds for 90% Confidence Interval Values for Paired Hypothesis Test
* ------------------------------------------------------------------------------------------
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

* Plot Labels
* -----------
'set  vpage off'
'set  grads off'
'set  string 1 c 6'
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

if( rms != 0 )
    nexps = mexps + 1
    expdsc.nexps = expdsc.0' RMS`bTOT`n'
    expcol.nexps = 99
    say 'expdsc.'nexps' = 'expdsc.nexps
else
    nexps = mexps
endif
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
if( m <= nexps )

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

    if( rms = 0 ) ; rms_label = ''            ; endif
    if( rms = 1 ) ; rms_label = '_RANDOM'     ; endif
    if( rms = 2 ) ; rms_label = '_BIAS'       ; endif
    if( rms = 3 ) ; rms_label = '_AMPLITUDE'  ; endif
    if( rms = 4 ) ; rms_label = '_PHASE'      ; endif

if( nday = ndaymax )
   'myprint -name 'SOURCE'/corcmp/stats_'label'_rmscmp'rms_label'_'reg'_'level'_'months' -rotate 90 -density 100x100'
else
   'myprint -name 'SOURCE'/corcmp/stats_'label'_rmscmp'rms_label'_'reg'_'level'_'months'_'nday'DAY -rotate 90 -density 100x100'
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

