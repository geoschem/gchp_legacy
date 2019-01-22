function rmscmpz (args)

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
if( field = "h" )
    name  = "Heights"
    unit  = "(m)"
    label = "hght"
    cint  =  4
    axmin = -10
    axmax =  110
endif
if( field = "u" )
    name  = "U-Wind"
    unit  = "(m/sec)"
    label = "uwnd"
    cint  = 0.5
    axmin = -2
    axmax =  16
endif
if( field = "v" )
    name  = "V-Wind"
    unit  = "(m/sec)"
    label = "vwnd"
    cint  = 0.5
    axmin = -2
    axmax =  16
endif
if( field = "t" )
    name  = "Temperature"
    unit  = "(K)"
    label = "tmpu"
    cint  = 0.2
    axmin = -1
    axmax =  6
endif
if( field = "q" )
    name  = "Specific Humidity"
    unit  = "(g/g)"
    label = "sphu"
endif


'getinfo xpos'
         xpos  = result
'getinfo ypos'
         ypos  = result

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
'set lev 1000 100'

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
'set lev 1000 100'
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
'set dfile '1
'set lev 1000 100'
'set t  'tbeg.0' 'tdim.0
'define  zero = 0.0'

say 'TBEG: 'tbeg.0'  TEND: 'tdim.0
say 'XPOS: 'xpos  '  YPOS: 'ypos
say ' '
say 'Initialize zave and zvar ...'
   m = 0
while( m<=mexps )
            n   = filebeg
            n.m = n + m*numfiles
            d.m = 1 + m*numfiles
'set dfile 'd.m
'set lev 1000 100'
'set t  'tbeg.m' 'tdim.m
'define  zave'm' = 0.0'
'define  zvar'm' = 0.0'
*pause '   DFILE: 'n.m'   Defined zave'm' and zvar'm
m = m+1
endwhile


say 'Initialize zaved and zvard ...'
   m = 0
while( m<=mexps )
            n   = filebeg
            n.m = n + m*numfiles
'set dfile 'ddif.m
'set lev 1000 100'
'set t  'tbeg.m' 'tdif.m
'define zaved'm' = 0.0'
'define zvard'm' = 0.0'
*pause '   DFILE: 'ddif.m'   Defined zaved'm' and zvard'm
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
'set dfile 'd.m
'set lev 1000 100'
'set t  'tbeg.m' 'tdim.m
 toffset = 1-toffset.n.m
*say 'dfile 'd.m'  tbeg: 'tbeg.m'  tend: 'tdim.m'  toffset: 'toffset'  'field'cor'n.m' = 'field'cor.'n.m'(t+'toffset')'

'define 'field'rms'n.m'    = 'field'rms.'n.m'(t+'toffset')'
'define 'field'rmsran'n.m' = 'field'rms_ran.'n.m'(t+'toffset')'
'define 'field'rmsbar'n.m' = 'field'rms_bar.'n.m'(t+'toffset')'
'define 'field'rmsdsp'n.m' = 'field'rms_dsp.'n.m'(t+'toffset')'
'define 'field'rmsdis'n.m' = 'field'rms_dis.'n.m'(t+'toffset')'
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
'set lev 1000 100'
'set t  'tbeg.m' 'tdim.m
if( rms = 0 ) ; 'define q'm' =       'field'rms'n.m        ; endif
if( rms = 1 ) ; 'define q'm' =       'field'rmsran'n.m     ; endif
if( rms = 2 ) ; 'define q'm' =       'field'rmsbar'n.m     ; endif
if( rms = 3 ) ; 'define q'm' = sqrt( 'field'rmsdis'n.m' )' ; endif
if( rms = 4 ) ; 'define q'm' = sqrt( 'field'rmsdsp'n.m' )' ; endif
'define z'n'e'm' = q'm
*say '   DFILE: 'n.m'   Defined q'm' and z'n'e'm
n = n + 1
endwhile
m = m + 1
endwhile
*pause ' Finished New Fisher Transform Variable znem'

say ' Define New Fisher Transform Variable zdnem ...'
m = 0
while( m<=mexps )
        n   = filebeg
while ( n  <= fileend )
        n.m = n + m*numfiles
        d.m = 1 + m*numfiles

'set dfile 'n
'set dfile 'ddif.m
'set lev 1000 100'
'set t 'tbeg.m' 'tdif.m
if( rms = 0 ) ; 'define ctl  =       'field'rms'n        ; endif
if( rms = 1 ) ; 'define ctl  =       'field'rmsran'n     ; endif
if( rms = 2 ) ; 'define ctl  =       'field'rmsbar'n     ; endif
if( rms = 3 ) ; 'define ctl  = sqrt( 'field'rmsdis'n' )' ; endif
if( rms = 4 ) ; 'define ctl  = sqrt( 'field'rmsdsp'n' )' ; endif
*say '   DFILE: 'n'  CTL defined for TBEG = 'tbeg.0' to TEND: 'tdif.0

'set dfile 'd.m
'set lev 1000 100'
'set t 'tbeg.m' 'tdif.m
if( rms = 0 ) ; 'define dum  =       'field'rms'n.m        ; endif
if( rms = 1 ) ; 'define dum  =       'field'rmsran'n.m     ; endif
if( rms = 2 ) ; 'define dum  =       'field'rmsbar'n.m     ; endif
if( rms = 3 ) ; 'define dum  = sqrt( 'field'rmsdis'n.m' )' ; endif
if( rms = 4 ) ; 'define dum  = sqrt( 'field'rmsdsp'n.m' )' ; endif
*say   '   DFILE: 'n.m'  DUM defined for TBEG = 'tbeg.0' to TEND: 'tdif.0

'set dfile 'ddif.m
'set lev 1000 100'
'set t 'tbeg.m' 'tdif.m
if( m = 0 )
*say 'Computing DumDiff for EXP: 'm'   File: 'ddif.m' for TBEG = 'tbeg.0' to TEND: 'tdif.0
   'define dumdiff = dum'
else
*say 'Computing DumDiff for EXP: 'm'   File: 'n.m' for TBEG = 'tbeg.0' to TEND: 'tdif.0
   'makezdif2 -q1 dum -file1 'd.m' -q2 zero    -file2   1 -ptop 100 -name dum'
endif

'set dfile 'n
'set dfile 'ddif.m
'set lev 1000 100'
'set t 'tbeg.m' 'tdif.m
'define     dq'm' = (ctl-dumdiff)'
'define zd'n'e'm' = dq'm
n = n + 1
endwhile
m = m + 1
endwhile
*pause ' Finished makezdif2'

* Compute Mean
* ------------
say ' Compute mean zave ...'
m = 0
while( m<=mexps )
        n  = filebeg
while ( n <= fileend )
        n.m = n + m*numfiles
        d.m = 1 + m*numfiles
'set dfile 'd.m
'set lev 1000 100'
'set t 'tbeg.m' 'tdim.m
'define  zave'm' =  zave'm' +  z'n'e'm
n = n + 1
endwhile
'define  zave'm' =  zave'm'/'numfiles
m = m + 1
endwhile
*say 'Hit Enter to continue ...'
*pull flag

say ' Compute mean zdave ...'
m = 0
while( m<=mexps )
        n  = filebeg
while ( n <= fileend )
'set dfile 'ddif.m
'set lev 1000 100'
'set t 'tbeg.m' 'tdif.m
'define zaved'm' = zaved'm' + zd'n'e'm
n = n + 1
endwhile
'define zaved'm' = zaved'm'/'numfiles
m = m + 1
endwhile
*say 'Hit Enter to continue ...'
*pull flag


* Compute Variance and Standard Deviations
* ----------------------------------------
say ' Compute variance zvar and zstd ...'
m = 0
while( m<=mexps )
        n  = filebeg
while ( n <= fileend )
        n.m = n + m*numfiles
        d.m = 1 + m*numfiles
'set dfile 'd.m
'set lev 1000 100'
'set t 'tbeg.m' 'tdim.m
'define  zvar'm' =  zvar'm' + pow(  z'n'e'm'- zave'm',2 )'
n = n + 1
endwhile
'define  zvar'm' =  zvar'm'/('numfiles'-1)'
'define  zstd'm' = sqrt(  zvar'm' )'
m = m + 1
endwhile
*pull flag

say ' Compute variance zdvar and zdstd ...'
m = 0
while( m<=mexps )
        n  = filebeg
while ( n <= fileend )
'set dfile 'ddif.m
'set lev 1000 100'
'set t 'tbeg.m' 'tdif.m
'define zvard'm' = zvard'm' + pow( zd'n'e'm'-zaved'm',2 )'
n = n + 1
endwhile
'define zvard'm' = zvard'm'/('numfiles'-1)'
'define zstdd'm' = sqrt( zvard'm' )'
m = m + 1
endwhile
*pull flag


* Compute Fisher Mean
* -------------------
say ' Compute Fisher Mean rave ...'
m = 0
while( m<=mexps )
        n  = filebeg
        n.m = n + m*numfiles
        d.m = 1 + m*numfiles
'set dfile 'd.m
'set lev 1000 100'
'set t 'tbeg.m' 'tdim.m
'define rave'm' = zave'm
m = m + 1
endwhile
*pull flag

 dof = numfiles-1    ;* Degrees of Freedom (dof)

'astudt 'dof' 0.01'  ;* 99% Confidence
'q defval astudtout 1 1'
critval99=subwrd(result,3)

'astudt 'dof' 0.05'  ;* 95% Confidence
'q defval astudtout 1 1'
critval95 = subwrd(result,3)

'astudt 'dof' 0.32'  ;* 68% Confidence
'q defval astudtout 1 1'
critval68=subwrd(result,3)

* Estimate Statistically Significant Range for Zero-Mean Hypothesis in a Paired t-Test)
* -------------------------------------------------------------------------------------
say ' Estimate T-Test Range rUpm rLpm ...'
m = 1
while( m<=mexps )
'set dfile 'ddif.m
'set lev 1000 100'
'set t 'tbeg.m' 'tdif.m
'define se  = sqrt( zvard'm'/'numfiles' )'
'define dx99  = se*'critval99
'define rUp99'm' =  dx99'
'define rLp99'm' = -dx99'
'define dx95  = se*'critval95
'define rUp95'm' =  dx95'
'define rLp95'm' = -dx95'
'define dx68  = se*'critval68
'define rUp68'm' =  dx68'
'define rLp68'm' = -dx68'
m = m + 1
endwhile


* Plot Fisher Mean for Experiments
* --------------------------------
say '  Plot Fisher Mean for Experiments'
'getinfo year'
         year = result
'getinfo date'
         date = result

            n = filebeg
'set dfile 'n
'set lev 1000 100'
'set t 'tdim.0
'minmax rave'0
 qmax = subwrd(result,1)
 qcnt = qmax/10

        m  = 0
while( m<=mexps )
        n  = filebeg
        n.m = n + m*numfiles
        d.m = 1 + m*numfiles
'set dfile 'd.m
'set lev 1000 100'
'set t 'tbeg.m' 'tdim.m
'set vpage off'
'set grads off'
'set gxout contour'
'set parea 2.25 9.75 1.0 7.5'
'set clab  on'
'set xaxis 0 'nday' .5'
'set cmark  0'
'set cthick 8'
'set cstyle 1'
'set ccolor rainbow'
'set rbrange 0 'qmax
'set cint 'qcnt
'd rave'm

'draw ylab Pressure (hPa)'
'set  string 1 c 6 0'
'set  strsiz .17'

    if( rms = 0 ) ; rms_label = 'Root Mean Square Error'   ; endif
    if( rms = 1 ) ; rms_label = 'RMS for RANDOM Error'     ; endif
    if( rms = 2 ) ; rms_label = 'RMS for BIAS Error'       ; endif
    if( rms = 3 ) ; rms_label = 'RMS for AMPLITUDE Error'  ; endif
    if( rms = 4 ) ; rms_label = 'RMS for PHASE Error'      ; endif

'draw string 6.0 8.15 'expdsc.m' ('numfiles')  'rms_label' (CINT: 'qcnt')'
'draw string 6.0 7.8  'name'  'region
'set  strsiz .12'
'draw string 6.0 0.72 Forecast Day'

'set  string 1 c 6 90'
'set  strsiz .18'
'draw string 0.80 4.25 'months' 'year

say 'EXP'm'  Field: 'name'  Region: 'region

'!/bin/mkdir -p 'SOURCE'/corcmp'

    if( rms = 0 ) ; rms_label = ''            ; endif
    if( rms = 1 ) ; rms_label = '_RANDOM'     ; endif
    if( rms = 2 ) ; rms_label = '_BIAS'       ; endif
    if( rms = 3 ) ; rms_label = '_AMPLITUDE'  ; endif
    if( rms = 4 ) ; rms_label = '_PHASE'      ; endif

if( nday = ndaymax )
   'myprint -name 'SOURCE'/corcmp/'expdsc.m'_'expdsc.m'_stats_'label'_rmscmp'rms_label'_'reg'_z_'months' -rotate 90 -density 100x100'
else 
   'myprint -name 'SOURCE'/corcmp/'expdsc.m'_'expdsc.m'_stats_'label'_rmscmp'rms_label'_'reg'_z_'months'_'nday'DAY -rotate 90 -density 100x100'
endif
if( debug = "TRUE" )
    say "Hit ENTER for next plot"
    pull flag
endif
'c'

m = m + 1
endwhile


* Plot Difference plus Significance
* ---------------------------------

    m = 1
    while( m<=mexps )
       n  = filebeg
       d.m = 1 + m*numfiles

mfile = 1 + m*files_per_month

flag = ''
while( flag = '' )
'set vpage off'
'set grads off'
'set grid  off'
'set gxout contour'
'set parea 2.25 9.75 1.0 7.5'
'set xaxis 0 'nday' .5'
'set string 1 c 6 0'

'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'makezdif3 -q1  rave'm' -file1 'mfile' -q2 rave0  -file2 1  -ptop 100 -name rave'
'getinfo numfiles'
         newfile = result
'close ' newfile

'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'makezdif3 -q1   rUp99'm' -file1 'mfile' -q2  zero  -file2 1  -ptop 100 -name  rUp99'
'getinfo numfiles'
         newfile = result
'close ' newfile

'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'makezdif3 -q1   rUp95'm' -file1 'mfile' -q2  zero  -file2 1  -ptop 100 -name  rUp95'
'getinfo numfiles'
         newfile = result
'close ' newfile

'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'makezdif3 -q1   rUp68'm' -file1 'mfile' -q2  zero  -file2 1  -ptop 100 -name  rUp68'
'getinfo numfiles'
         newfile = result
'close ' newfile

'set gxout shaded'
'rgbset'

'set t 'tdif.m
'minmax rUp95diff'
 dcint = subwrd(result,1) * 1000 / 6

* WMP
* Fixed Color Scales to better judge magnitude of change...
'run uppercase 'field
                FIELD = result
*if (FIELD='H') ; dcint = 750.000 ; endif
*if (FIELD='U') ; dcint =  90.000 ; endif
*if (FIELD='V') ; dcint =  90.000 ; endif
*if (FIELD='T') ; dcint =  30.000 ; endif
*if (FIELD='Q') ; dcint =   0.012 ; endif

'set dfile 'ddif.m
'set t 'tbeg.m' 'tdif.m
'set csmooth on'

* Draw 95% confidence colored shading and colorbar
'set gxout shaded'
'set datawarn off'
'shades 'dcint
'd maskout( ravediff*1000, abs(ravediff)-rUp95diff )'
'cbarn -xmid 6 -snum 0.6'

* Draw 68% confidence colored contours
'set gxout contour'
'set clab off'
'set cthick 8'
'set datawarn off'
'shades 'dcint
'd maskout( ravediff*1000, abs(ravediff)-rUp68diff )'

* Draw 99% confidence black contours without labels
'set gxout contour'
'set clab off'
'set cthick 6'
'set cint 'dcint
'set ccolor 1'
'set datawarn off'
'd maskout( ravediff*1000,  abs(ravediff)-rUp99diff )'

* reset some background values
'set datawarn on'
'set clab on'

* Re-Draw Individual Plots
* ------------------------
'set gxout contour'
'set clab  on'
'set xaxis 0 'nday' .5'
'set cmark  0'
'set cthick 6'
'set ccolor rainbow'
'set rbrange 0 'qmax
'set cint 'qcnt
'set cstyle 2'
*'d ravem'
'set cmark  0'
'set cthick 6'
'set ccolor rainbow'
'set rbrange 0 'qmax
'set cint 'qcnt
'set cstyle 3'
*'d raveo'

'draw ylab Pressure (hPa)'
'set  string 1 c 6 0'

'set  strsiz .132'
'draw string 6.0 8.15 'expdsc.m' - 'expdsc.0' ('numfiles')   'region

    if( rms = 0 ) ; rms_label = 'Root Mean Square Error Difference'   ; endif
    if( rms = 1 ) ; rms_label = 'RMS for RANDOM Error Difference'     ; endif
    if( rms = 2 ) ; rms_label = 'RMS for BIAS Error Difference'       ; endif
    if( rms = 3 ) ; rms_label = 'RMS for AMPLITUDE Error Difference'  ; endif
    if( rms = 4 ) ; rms_label = 'RMS for PHASE Error Difference'      ; endif

'draw string 6.0 7.8 'name'   'rms_label' (x10`a-3`n)'
'set  strsiz .12'
'draw string 6.0 0.72 Forecast Day'

'set  string 1 l 8 0'
'set  strsiz .32'
'draw string 0.70 7.38 'reg
'draw string 0.70 7.01 'FIELD

'set  string 1 c 6 90'
'set  strsiz .18'
'draw string 0.80 4.25 'months' 'year

say 'EXP'm'  Field: 'name'  Region: 'region

'!/bin/mkdir -p 'SOURCE'/corcmp'

    if( rms = 0 ) ; rms_label = ''            ; endif
    if( rms = 1 ) ; rms_label = '_RANDOM'     ; endif
    if( rms = 2 ) ; rms_label = '_BIAS'       ; endif
    if( rms = 3 ) ; rms_label = '_AMPLITUDE'  ; endif
    if( rms = 4 ) ; rms_label = '_PHASE'      ; endif

if( nday = ndaymax )
   'myprint -name 'SOURCE'/corcmp/'expdsc.0'_'expdsc.m'_stats_'label'_rmscmp'rms_label'_'reg'_z_'months' -rotate 90 -density 100x100'
else
   'myprint -name 'SOURCE'/corcmp/'expdsc.0'_'expdsc.m'_stats_'label'_rmscmp'rms_label'_'reg'_z_'months'_'nday'DAY -rotate 90 -density 100x100'
endif

if( debug = "TRUE" )
    say "Hit ENTER for next plot"
    pull flag
else
    flag = 'c'
endif
'c'
endwhile

m = m + 1
endwhile
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

