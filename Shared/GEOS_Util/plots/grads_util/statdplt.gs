function stdiff (args)

********************************************************************************
****                                                                        ****
**** Note:  Forecast           =>  F                                        ****
****        Analysis           =>  A                                        ****
****                                                                        ****
****        Mean Square Error  =>  MSE  =   1/N * SUM[ (F-A)**2 ]           ****
****        Mean Error         =>  BIAS =   1/N * SUM[ (F-A) ]              ****
****        Mean Error Squared =>  MES  =   BIAS**2                         ****
****        Root Mean  Square  =>  RMS  = SQRT[ MSE ]                       ****
****        Variance           =>  VAR  =   1/N * SUM[ (F-A-BIAS)**2 ]      ****
****        Standard Deviation =>  STD  = SQRT[ VAR ]                       ****
****                                                                        ****
****        F Mean             =>  FBAR =   1/N * SUM[  F  ]                ****
****        A Mean             =>  ABAR =   1/N * SUM[  A  ]                ****
****        F Variance         =>  FVAR =   1/N * SUM[ (F-FBAR)**2 ]        ****
****        A Variance         =>  AVAR =   1/N * SUM[ (A-ABAR)**2 ]        ****
****        CoVariance         =>  COV  =   1/N * SUM[ (F-FBAR)*(A-ABAR) ]  ****
****        Amplitude Error    =>  AMP  = [ FSTD - ASTD ]**2                ****
****                                    + [ FBAR - ABAR ]**2                ****
****        Phase     Error    =>  PHZ  = 2*[ FSTD*ASTD - COV ]             ****
****                                                                        ****
****        Mean Square Error  =   BIAS**2         + Variance               ****
****                           =   Amplitude Error + Phase Error            ****
****                      MSE  =   VAR + MES                                ****
****                           =   AMP + PHZ                                ****
****                                                                        ****
********************************************************************************

field = subwrd  (args,1)
type  = subwrd  (args,2)
exp1  = subwrd  (args,3)
exp2  = subwrd  (args,4)
numf  = subwrd  (args,5)
flag  = subwrd  (args,6)
 
'fixname 'exp1
          tag1 = result
'fixname 'exp2
          tag2 = result
          tag2 = EXP

say 'TYPE: 'type
say 'exp1: 'exp1' tag1: 'tag1
say 'exp2: 'exp2' tag2: 'tag2

********************************************************************************
* if( type = std ) ; string = "Std.Dev.  (Forecast-Analysis)"           ; endif
* if( type = var ) ; string = "Variance  (Forecast-Analysis)"           ; endif
* if( type = rms ) ; string = "RMS  (Forecast-Analysis)"                ; endif
* if( type = fma ) ; string = "MEAN  (Forecast-Analysis)"               ; endif
* if( type = fmc ) ; string = "MEAN  (Forecast-Climatology)"            ; endif
* if( type = mse ) ; string = "Mean_Square_Error  (Forecast-Analysis)"  ; endif
* if( type = mes ) ; string = "Mean_Error_Squared  (Forecast-Analysis)" ; endif
* if( type = rmes) ; string = "Root_Mean_Error_Squared  (Forecast-Analysis)" ; endif
********************************************************************************

'run getinfo level'
             level = result
'run getinfo time'
             time  = result
'run getinfo tinc'
             tinc = result

if( tinc = "NULL" ) ; tinc = 6 ; endif
                      hour = (time-1)*tinc
if( hour < 10  )    ; hour = 0hour ; endif
if( hour < 100 )    ; hour = 0hour ; endif

'run getenv "SYSCMP_TDIM"'
                    tdim  = result

'set datawarn off'
'set grid  off'
'set grads off'
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd mask'

********************************************************************************
*         TVAL = SQRT(N) x NAME_DIFF_MEAN / NAME_DIFF_STD     *
*                                                             *
*       astudt (N-1) 0.10  [90% Confidence]                   *
*       astudt (N-1) 0.05  [95% Confidence]                   *
*       astudt (N-1) 0.04  [96% Confidence]                   *
*       astudt (N-1) 0.02  [98% Confidence]                   *
*       astudt (N-1) 0.01  [99% Confidence]                   *
*                                                             *
*       q defval astudtout 1 1                                *
*       critval=subwrd(result,3)                              *
*                                                             *
*       and then CONTOUR TVAL using CLEVS = critval          
********************************************************************************

    tipe = type
if( type = Dres ) 
    type = Dmse
endif

'set gxout shaded'

* ---------------------------------------------------------------

'set t 'tdim
'define delDmes = 'field'Dmes'tag2
'define delDvar = 'field'Dvar'tag2
'define delDamp = 'field'Damp'tag2
'define delDphz = 'field'Dphz'tag2
'define delDmse = 'field'Dmse'tag2

if( tipe = Dres )
    if( flag = 1 )
       'define  delDmse = delDmse - delDmes - delDvar'
    endif
    if( flag = 2 )
       'define  delDmse = delDmse - delDamp - delDphz'
    endif
endif

'define dumm = regrid2(  delDmse,.25, .25, bs_p1, 0, -90 )'
       dummy  = getstuff( 'dumm' )
   diff_cint  = subwrd(dummy,1)
   diff_scale = subwrd(dummy,2)
        diffm = subwrd(dummy,3)

* ---------------------------------------------------------------

'set t 'time
'define delDmes = 'field'Dmes'tag2
'define delDvar = 'field'Dvar'tag2
'define delDamp = 'field'Damp'tag2
'define delDphz = 'field'Dphz'tag2
'define delDmse = 'field'Dmse'tag2

if( tipe = Dres )
    if( flag = 1 )
       'define  delDmse = delDmse - delDmes - delDvar'
    endif
    if( flag = 2 )
       'define  delDmse = delDmse - delDamp - delDphz'
    endif
endif

'define diff = regrid2(  del'type',.25, .25, bs_p1, 0, -90 )'

* Compute Confidence Interval
* ---------------------------
    numfm1 = numf - 1

if( type = Dmse ) ; 'define tval = sqrt('numfm1') * 'field'Dmse'tag2' / sqrt('field'DDmse'tag2')' ; endif
if( type = Dmes ) ; 'define tval = sqrt('numfm1') * 'field'Dmes'tag2' / sqrt('field'DDmse'tag2')' ; endif
if( type = Dvar ) ; 'define tval = sqrt('numfm1') * 'field'Dvar'tag2' / sqrt('field'DDmse'tag2')' ; endif
if( type = Damp ) ; 'define tval = sqrt('numfm1') * 'field'Damp'tag2' / sqrt('field'DDmse'tag2')' ; endif
if( type = Dphz ) ; 'define tval = sqrt('numfm1') * 'field'Dphz'tag2' / sqrt('field'DDmse'tag2')' ; endif

   'define tval = regrid2( tval,0.25,0.25,bs_p1,0,-90 )'

         ttest = 0.10
    confidence = 100 * (1-ttest)
   'astudt 'numfm1' 'ttest
   'q defval astudtout 1 1'
      critval=subwrd(result,3)
      say 'critval: 'critval

if( tipe = Dres )
   'shades 'diff_cint
else
   'shades 'diff_cint' -quad'
            diff_cint = result
endif

'define  diff0 = maskout( maskout( diff*'diff_scale',abs(diff*'diff_scale')-'diff_cint' ), abs(tval)-'critval')'
'define  diff  =          maskout( diff*'diff_scale',abs(diff*'diff_scale')-'diff_cint' )'

'd diff'

* Create New File containing Ratio: DIFF/DIFF0 for contouring region of significance
* ----------------------------------------------------------------------------------
'getinfo file'
      curfile = result

if( tipe != Dres )
'define diffr = diff/diff0'
'getinfo undef'
         undef = result
'set undef 0.0'
'set sdfwrite -5d diffr.nc4'
'sdfwrite diffr'
'undefine diffr'

'sdfopen  diffr.nc4'
'getinfo  numfiles'
          diffile = result
'set dfile 'diffile
'q ctlinfo'
   ctlinfo = result

          fname = 'diffr.ctl'
'!remove 'fname
             n = 1
             line = sublin(ctlinfo,n)
             write(fname,line)
      while( line != 'endvars' )
             n = n + 1
             line = sublin(ctlinfo,n)
             word = subwrd(line,1)
         if( word = 'undef' )
             line = 'undef 1e15'
         endif
             write(fname,line,append)
      endwhile
      close = close(fname)

'close 'diffile
'open  'fname
'getinfo numfiles'
         diffile = result
'set dfile 'diffile

'set gxout contour'
'set clab  off'
'set ccolor 1'
'set cthick 1'
'set clevs 0.5'
'd diffr'
'close 'diffile
endif

* Create New File containing DIFF with zeroes rather than UNDEF for Global Mean Metrics
* -------------------------------------------------------------------------------------
'define diff2 = diff0/'diff_scale
'set sdfwrite -5d diff'type'.nc4'
'set undef 0.0'
'sdfwrite diff2'
'sdfopen  diff'type'.nc4'
'getinfo  numfiles'
          diffile = result
'set dfile 'diffile
'q ctlinfo'
   ctlinfo = result
   
          fname = 'diff'type'.ctl'
'!remove 'fname
             n = 1
             line = sublin(ctlinfo,n)
             write(fname,line)
      while( line != 'endvars' )
             n = n + 1
             line = sublin(ctlinfo,n)
             word = subwrd(line,1)
         if( word = 'undef' )
             line = 'undef 1e15'
         endif
             write(fname,line,append)
      endwhile
      close = close(fname)

'close 'diffile
'open  'fname
'getinfo numfiles'
         diffile = result
'set dfile 'diffile
'define NHEM  = aave( diff2.'diffile',lon=0,lon=360,lat= 20,lat= 80 )'
'define SHEM  = aave( diff2.'diffile',lon=0,lon=360,lat=-80,lat=-20 )'
'define GLOB  = aave( diff2.'diffile',lon=0,lon=360,lat=-90,lat= 90 )'
'define TROP  = aave( diff2.'diffile',lon=0,lon=360,lat=-20,lat= 20 )'
'close 'diffile

'set dfile 'curfile
'set undef 'undef
* -------------------------------------------------------------


'd NHEM'
   NHEM = subwrd(result,4)
'd SHEM'
   SHEM = subwrd(result,4)
'd GLOB'
   GLOB = subwrd(result,4)
'd TROP'
   TROP = subwrd(result,4)

if( NHEM = 9.999e+20 ) ; NHEM = 0 ; endif
if( SHEM = 9.999e+20 ) ; SHEM = 0 ; endif
if( GLOB = 9.999e+20 ) ; GLOB = 0 ; endif
if( TROP = 9.999e+20 ) ; TROP = 0 ; endif

if( type = mes  ) ; 'cbarn -scale 0.55 -xmid 2.9 -ymid 4.25 -scaley 0.8 ' ; endif
if( type = var  ) ; 'cbarn -scale 0.55 -xmid 8.4 -ymid 4.25 -scaley 0.8 ' ; endif
if( type = mse  ) ; 'cbarn -scale 0.55 -xmid 5.6 -ymid 0.30 -scaley 0.8 ' ; endif

if( type = rmes ) ; 'cbarn -scale 0.55 -xmid 2.9 -ymid 4.25 -scaley 0.8 ' ; endif
if( type = std  ) ; 'cbarn -scale 0.55 -xmid 8.4 -ymid 4.25 -scaley 0.8 ' ; endif
if( type = rms  ) ; 'cbarn -scale 0.55 -xmid 5.6 -ymid 0.30 -scaley 0.8 ' ; endif

if( type = Dmes ) ; 'cbarn -scale 0.55 -xmid 2.9 -ymid 4.25 -scaley 0.8 ' ; endif
if( type = Damp ) ; 'cbarn -scale 0.55 -xmid 2.9 -ymid 4.25 -scaley 0.8 ' ; endif
if( type = Dvar ) ; 'cbarn -scale 0.55 -xmid 8.4 -ymid 4.25 -scaley 0.8 ' ; endif
if( type = Dphz ) ; 'cbarn -scale 0.55 -xmid 8.4 -ymid 4.25 -scaley 0.8 ' ; endif
if( tipe = Dmse ) ; 'cbarn -scale 0.55 -xmid 2.9 -ymid 0.30 -scaley 0.8 ' ; endif
if( tipe = Dres ) ; 'cbarn -scale 0.55 -xmid 8.4 -ymid 0.30 -scaley 0.8 ' ; endif

'run getenv MONTHLAB'
            month = result
say 'MONTH_LABEL: 'month

'getinfo year'
         year  = result

if( type = Dmes )
'set vpage off'
'set string 1 l 5'
'set strsiz 0.08'
     x1 = 0.70
     y1 = 3.70
y2 = y1 - 0.15
y3 = y2 - 0.15
y4 = y3 - 0.15
*'draw string 'x1' 'y1' NHEM: 'NHEM
*'draw string 'x1' 'y2' SHEM: 'SHEM
*'draw string 'x1' 'y3' GLOB: 'GLOB
*'draw string 'x1' 'y4' TROP: 'TROP
'set string 1 c 6'
'set strsiz 0.11'
'draw string 3.00  7.9 (BIAS)`a2`n:  MES`b1`n-MES`b2`n  (x10**'diffm')'
endif

if( type = Damp )
'set vpage off'
'set string 1 l 5'
'set strsiz 0.08'
     x1 = 0.70
     y1 = 3.70
y2 = y1 - 0.15
y3 = y2 - 0.15
y4 = y3 - 0.15
*'draw string 'x1' 'y1' NHEM: 'NHEM
*'draw string 'x1' 'y2' SHEM: 'SHEM
*'draw string 'x1' 'y3' GLOB: 'GLOB
*'draw string 'x1' 'y4' TROP: 'TROP
'set string 1 c 6'
'set strsiz 0.11'
'draw string 3.00  7.9 AMPL Error Difference  (x10**'diffm')'
endif

if( type = Dvar )
'set vpage off'
'set string 1 l 5'
'set strsiz 0.08'
     x1 = 9.40
     y1 = 3.70
y2 = y1 - 0.15
y3 = y2 - 0.15
y4 = y3 - 0.15
*'draw string 'x1' 'y1' NHEM: 'NHEM
*'draw string 'x1' 'y2' SHEM: 'SHEM
*'draw string 'x1' 'y3' GLOB: 'GLOB
*'draw string 'x1' 'y4' TROP: 'TROP
'set string 1 c 6'
'set strsiz 0.11'
'draw string 8.355  7.9 (Std_Dev)`a2`n:  VAR`b1`n- VAR`b2`n  (x10**'diffm')'
endif

if( type = Dphz )
'set vpage off'
'set string 1 l 5'
'set strsiz 0.08'
     x1 = 9.40
     y1 = 3.70
y2 = y1 - 0.15
y3 = y2 - 0.15
y4 = y3 - 0.15
*'draw string 'x1' 'y1' NHEM: 'NHEM
*'draw string 'x1' 'y2' SHEM: 'SHEM
*'draw string 'x1' 'y3' GLOB: 'GLOB
*'draw string 'x1' 'y4' TROP: 'TROP
'set string 1 c 6'
'set strsiz 0.11'
'draw string 8.355  7.9 PHASE Error Difference  (x10**'diffm')'
endif

if( tipe = Dmse )
'set vpage off'
'set string 1 l 5'
'set strsiz 0.08'
     x1 = 8.40
     y1 = 1.62
y2 = y1 - 0.15
y3 = y2 - 0.15
y4 = y3 - 0.15
*'draw string 'x1' 'y1' NHEM: 'NHEM
*'draw string 'x1' 'y2' SHEM: 'SHEM
*'draw string 'x1' 'y3' GLOB: 'GLOB
*'draw string 'x1' 'y4' TROP: 'TROP
'set string 1 c 6'
'set strsiz 0.13'
'draw string 3.000 3.9 (RMS)`a2`n:  MSE`b1`n-MSE`b2`n  (x10**'diffm')'
endif

if( tipe = Dres )
'set vpage off'
'set string 1 l 5'
'set strsiz 0.08'
     x1 = 8.40
     y1 = 1.62
y2 = y1 - 0.15
y3 = y2 - 0.15
y4 = y3 - 0.15
*'draw string 'x1' 'y1' NHEM: 'NHEM
*'draw string 'x1' 'y2' SHEM: 'SHEM
*'draw string 'x1' 'y3' GLOB: 'GLOB
*'draw string 'x1' 'y4' TROP: 'TROP
'set string 1 c 6'
'set strsiz 0.10'
if( flag = 1 ) ; 'draw string 8.355 3.9 (RMS)`a2`n-[ (BIAS)`a2`n+(STD)`a2`n ] Residual (x10**'diffm')'  ; endif
if( flag = 2 ) ; 'draw string 8.355 3.9 (RMS)`a2`n-[ AMPLITUDE + PHASE Error ] Residual (x10**'diffm')' ; endif
endif

if( type = var)
'set vpage off'
'set string 1 c 6'
'set strsiz 0.13'
'draw string 3.00  7.9 'level'-mb 'field'  SQRT[ BIAS`a2`n`b1`n-BIAS`a2`n`b2`n ]'
'draw string 8.355 7.9 'level'-mb 'field'  SQRT[ Std_Dev`a2`n`b1`n-Std_Dev`a2`n`b2`n ]'
'draw string 5.650 3.9 SQRT[ RMS`a2`n`b1`n-RMS`a2`n`b2`n ]'
endif

if( type = std | type = var | type = Dvar | type = Dphz )
'set vpage off'
'set string 1 c 6'

'set strsiz 0.16'
'run uppercase 'field
                UFIELD = result
'draw string 5.50 8.40 'level'-mb 'UFIELD'   'month' 'year'   Forecast Hour: 'hour
'set strsiz 0.12'
'draw string 5.50 8.18 1:'exp2' minus 2:'exp1'  'numf'-member Ensemble   (Contour > 'confidence'% Confidence)'

endif

return

function getstuff( q )

'minmax.simple 'q
   qmax = subwrd(result,1)
   qmin = subwrd(result,2)
   cint = (qmax-qmin)/18

'stats 'q
  cint = subwrd(result,2)

say 'Inside getstuff for 'q', cint: 'cint
if( cint = 0 )
    fact = 1
   icint = 0
   scale = 0
    cmax = 0
    cmin = 0
else

'd log10('cint')'
   log10cint = subwrd(result,4)
'getint 'log10cint
         scale = result

if( scale <= 0 )
   'd pow(10,abs('scale'))'
    fact = subwrd(result,4)
else
   'd pow(10,-'scale')'
    fact = subwrd(result,4)
endif

     'getint 'cint*fact
      icint = result

say ' scale: 'scale'  fact: 'fact'  icint: 'icint
while( icint < 1  )
   if( scale <= 0 )
       fact = fact*10
      scale = scale - 1
   else
       fact = fact/10
      scale = scale + 1
   endif

   'getint 'cint*fact
     icint = result
say ' scale: 'scale'  fact: 'fact'  icint: 'icint
endwhile

'minmax.simple 'q
   qmax = subwrd(result,1)
   qmin = subwrd(result,2)

'getint 'qmax*fact/icint
        dqmax = result
'getint 'qmin*fact/icint
        dqmin = result
cmax = icint*dqmax
cmin = icint*dqmin

say 'qmax: 'qmax'  cmax: 'cmax
say 'qmin: 'qmin'  cmin: 'cmin

endif
return icint' 'fact' 'scale' 'cmax' 'cmin
