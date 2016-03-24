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
****        Mean Square Error  =   Variance + BIAS**2                       ****
****                      MSE  =   VAR      + MES                           ****
****                                                                        ****
********************************************************************************

field = subwrd  (args,1)
type  = subwrd  (args,2)
exp1  = subwrd  (args,3)
exp2  = subwrd  (args,4)
numf  = subwrd  (args,5)
cint  = subwrd  (args,6)
 
'fixname 'exp1
          tag1 = result
'fixname 'exp2
          tag2 = result
          tag2 = EXP

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

'run getinfo time'
             time  = result
'run getinfo tinc'
             tinc = result
if( tinc = "NULL" ) ; tinc = 6 ; endif
'run getinfo time'
             time = result
             hour = (time-1)*tinc
if( hour < 10  ) ; hour = 0hour ; endif
if( hour < 100 ) ; hour = 0hour ; endif

'run getenv "SYSCMP_TDIM"'
                    tdim  = result
say 'TDIM: 'tdim

'set datawarn off'
'set grid  off'
'set grads off'
'set gxout shaded'

*******************************************************************************
*         TVAL = SQRT(N) x NAME_DIFF_MEAN / NAME_DIFF_STD     *
*                                                             *
*       astudt (N-1) 0.20  [90% Confidence]                   *
*       astudt (N-1) 0.10  [95% Confidence]                   *
*       astudt (N-1) 0.08  [96% Confidence]                   *
*       astudt (N-1) 0.04  [98% Confidence]                   *
*       astudt (N-1) 0.02  [99% Confidence]                   *
*                                                             *
*       q defval astudtout 1 1                                *
*       critval=subwrd(result,3)                              *
*                                                             *
*       and then CONTOUR TVAL using CLEVS = critval          
*******************************************************************************

    say 'TYPE: 'type
    say 'exp1: 'exp1' tag1: 'tag1
    say 'exp2: 'exp2' tag2: 'tag2

'set t 'tdim
'define diff  = 'field''type''tag2'z'
       dummy  = getstuff( 'diff' )
   diff_cint  = subwrd(dummy,1)
   diff_scale = subwrd(dummy,2)
        diffm = subwrd(dummy,3)
'set t 'time

* Compute Confidence Interval
* ---------------------------
    numfm1 = numf - 1

if( type = dbia ) ; 'define tval = sqrt('numfm1') * 'field'Zbia'tag2'z / sqrt('field'bia'tag2'z)' ; endif
if( type = drms ) ; 'define tval = sqrt('numfm1') * 'field'Zmse'tag2'z / sqrt('field'mse'tag2'z)' ; endif
if( type = dstd ) ; 'define tval = sqrt('numfm1') * 'field'Zvar'tag2'z / sqrt('field'var'tag2'z)' ; endif

         ttest = 0.10
    confidence = 100 * (1-(ttest/2))
   'astudt 'numfm1' 'ttest
   'q defval astudtout 1 1'
      critval=subwrd(result,3)
      say 'critval: 'critval

'define  diff  = 'field''type''tag2'z'
'define  diff0 = maskout( maskout( diff*'diff_scale',abs(diff*'diff_scale')-'diff_cint' ), abs(tval)-'critval')'
'define  diff1 =          maskout( diff*'diff_scale',abs(diff*'diff_scale')-'diff_cint' )'

'shades 'diff_cint
'd       diff*'diff_scale

* Create New File containing Ratio: DIFF/DIFF0 for contouring region of significance
* ----------------------------------------------------------------------------------
'define diffr = diff1/diff0'
'getinfo file'
         curfile = result
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
'set csmooth off'
'set clab    off'
'set ccolor 1'
'set cthick 1'
'set clevs 0.999999'
'd diffr'
'close 'diffile

'set dfile 'curfile
'set undef 'undef
* -------------------------------------------------------------


if( type = dbia ) ; 'cbarn -scale 0.55 -xmid 2.9 -ymid 4.1 -scaley 0.8 ' ; endif
if( type = dstd ) ; 'cbarn -scale 0.55 -xmid 8.4 -ymid 4.1 -scaley 0.8 ' ; endif
if( type = drms ) ; 'cbarn -scale 0.55 -xmid 5.6 -ymid 0.3 -scaley 0.8 ' ; endif

'getinfo month' 
         month = result
'getinfo year'
         year  = result

if( type = dbia )
'set vpage off'
'set string 1 c 6'
'set strsiz 0.12'
'draw string 3.00  7.75 Zonal Mean 'field'  |BIAS`b1`n|-|BIAS`b2`n|  (x10**'diffm')'
endif

if( type = dstd )
'set vpage off'
'set string 1 c 6'
'set strsiz 0.12'
'draw string 8.355 7.75 Zonal Mean 'field'  (Std_Dev`b1`n-Std_Dev`b2`n)  (x10**'diffm')'
endif

if( type = drms )
'set vpage off'
'set string 1 c 6'
'set strsiz 0.12'
'draw string 5.650 3.90 Zonal Mean 'field'  (RMS`b1`n-RMS`b2`n)  (x10**'diffm')'
endif

if( type = dstd )
'set vpage off'
'set string 1 c 6'

'set strsiz 0.165'
'draw string 5.50 8.20 'month' 'year'   Forecast Hour: 'hour

'set string 1 l 6'
'set strsiz 0.15'
'draw string 0.38 2.46 1: 'exp2
'draw string 0.38 2.00 2: 'exp1
'set strsiz 0.12'
'draw string 0.75 2.23 minus'
'set strsiz 0.13'
'draw string 0.38 1.50 'numf'-member Ensemble'
'set strsiz 0.09'
'draw string 0.40 1.20 (Contour > 'confidence'% Confidence)'
endif

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
