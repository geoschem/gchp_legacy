function statpltz (args)
field = subwrd (args,1)

'numargs  'args
 numargs = result

'getinfo tdim'
         tdim = result
'getinfo time'
         time = result

       flag = 1
        num = 0
while ( num < numargs )
        num = num + 1
if( subwrd(args,num) = '-desc'   ) ; DESC0 = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-nfcst'  ) ; nfcst = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-flag'   ) ; flag  = subwrd(args,num+1) ; endif
endwhile
'fixname 'DESC0
          DESC = result

'getinfo pagex'
         pagex = result
     if( pagex = 8.5 ) ; 'run setenv ORIENTATION PORTRAIT'  ; endif
     if( pagex = 11  ) ; 'run setenv ORIENTATION LANDSCAPE' ; endif

'getinfo level'
         level = result
'set datawarn off'

* Initialize Plot Values
* ----------------------
    CCOLS = '59   57   55   47   44   37   36   34   33    0     21   22   23   24   25   26   27   28   29'
    CLAB  =  off

    scale = 1.0
     cint = NULL

if( field = "h" )
    name  = "Heights"
    unit  = "(m)"
    label = "hght"
endif

if( field = "u" )
    name  = "U-Wind"
    unit  = "(m/sec)"
    label = "uwnd"
endif

if( field = "v" )
    name  = "V-Wind"
    unit  = "(m/sec)"
    label = "vwnd"
endif

if( field = "t" )
    name  = "Temperature"
    unit  = "(K)"
    label = "tmpu"
endif

if( field = "q" )
    name  = "Specific Humidity"
    unit  = "(g/g)"
    label = "sphu"
endif


'set mproj latlon'
'set vpage off'
'set parea off'
'set xlopts 1 3 .14'
'set ylopts 1 3 .14'

*****************************************************************************************
****                                  Make Plots
*****************************************************************************************

sbar = 0.32

'set vpage 0 11 0 8.5'
'set xlopts 1 3 0.08'
'set ylopts 1 3 0.08'

'parea 1 1 3 2'
xmid = subwrd(result,1)
ybot = subwrd(result,2)
ytop = subwrd(result,3)

'set grads off'
'set grid  off'
'set clab 'CLAB
'set gxout contour'
'set t 'tdim
   dummy = getstuff( field'fm'DESC'z' )
   F_CINT  = subwrd(dummy,1)
   F_scale = subwrd(dummy,2)
        Fm = subwrd(dummy,3)
      cmax = subwrd(dummy,4)
      cmin = subwrd(dummy,5)
'set t 'time
'set ccolor rainbow'
'set cint 'F_CINT
'set rbrange 'cmin' 'cmax
'd 'field'fm'DESC'z*'F_scale
'set string 1 c 6'
'set strsiz 0.08'
'draw string 'xmid' 'ytop' Forecast  (x10**'Fm')  CINT: 'F_CINT'  CMIN: 'cmin


'parea 2 1 3 2'
xmid = subwrd(result,1)
ybot = subwrd(result,2)
ytop = subwrd(result,3)

'set grads off'
'set grid  off'
'set gxout shaded'
'set t 'tdim
   dummy = getstuff( field'fmc'DESC'z' )
   FMC_CINT  = subwrd(dummy,1)
   FMC_scale = subwrd(dummy,2)
        FMCm = subwrd(dummy,3)
'set t 'time
'shades 'FMC_CINT
'd 'field'fmc'DESC'z*'FMC_scale
'cbarn -sbar 'sbar' -snum 0.45 -xmid 'xmid' -ymid 'ybot' -scaley 0.4'
'set string 1 c 6'
'set strsiz 0.08'
'draw string 'xmid' 'ytop' Forecast-Climatology  (x10**'FMCm')'


'parea 3 1 3 2'
xmid = subwrd(result,1)
ybot = subwrd(result,2)
ytop = subwrd(result,3)

'set grads off'
'set grid  off'
'set gxout shaded'
'set t 'tdim
   dummy = getstuff( field'fma'DESC'z' )
   FMA_CINT  = subwrd(dummy,1)
   FMA_scale = subwrd(dummy,2)
        FMAm = subwrd(dummy,3)
'set t 'time
'shades 'FMA_CINT
'd 'field'fma'DESC'z*'FMA_scale
'cbarn -sbar 'sbar' -snum 0.45 -xmid 'xmid' -ymid 'ybot' -scaley 0.4'
'set string 1 c 6'
'set strsiz 0.08'
'draw string 'xmid' 'ytop' Mean (Forecast-Analysis)  (x10**'FMAm')'


'parea 1 2 3 2'
xmid = subwrd(result,1)
ybot = subwrd(result,2)
ytop = subwrd(result,3)

'set grads off'
'set grid  off'
'set gxout shaded'
'set t 'tdim
   dummy = getstuff( field'rms'DESC'z' )
   FMA_CINT  = subwrd(dummy,1)
   FMA_scale = subwrd(dummy,2)
        FMAm = subwrd(dummy,3)
'set t 'time
'shades 'field'rms'DESC'z*'FMA_scale' 0 -minval 0 -cint 'FMA_CINT
'd 'field'rms'DESC'z*'FMA_scale
'cbarn -sbar 'sbar' -snum 0.45 -xmid 'xmid' -ymid 'ybot' -scaley 0.4'
'set string 1 c 6'
'set strsiz 0.08'
'draw string 'xmid' 'ytop' Root Mean Square Error (F-A)  (x10**'FMAm')'


'parea 2 2 3 2'
xmid = subwrd(result,1)
ybot = subwrd(result,2)
ytop = subwrd(result,3)

'set grads off'
'set grid  off'
'set gxout shaded'
'set t 'time
'shades 'field'rms'DESC'z*'FMA_scale' 0 -minval 0 -cint 'FMA_CINT
if( flag = 1 ) ; 'd 'field'ramp'DESC'z*'FMA_scale ; endif
if( flag = 2 ) ; 'd 'field'rmes'DESC'z*'FMA_scale ; endif
'cbarn -sbar 'sbar' -snum 0.45 -xmid 'xmid' -ymid 'ybot' -scaley 0.4'
'set string 1 c 6'
'set strsiz 0.08'
if( flag = 1 ) ; 'draw string 'xmid' 'ytop' Root Amplitude Error (F-A)  (x10**'FMAm')'    ; endif
if( flag = 2 ) ; 'draw string 'xmid' 'ytop' Root Mean Error Squared (F-A)  (x10**'FMAm')' ; endif


'parea 3 2 3 2'
xmid = subwrd(result,1)
ybot = subwrd(result,2)
ytop = subwrd(result,3)

'set grads off'
'set grid  off'
'set gxout shaded'
'set t 'time
'shades 'field'rms'DESC'z*'FMA_scale' 0 -minval 0 -cint 'FMA_CINT
if( flag = 1 ) ; 'd 'field'rphz'DESC'z*'FMA_scale ; endif
if( flag = 2 ) ; 'd 'field'std'DESC'z*'FMA_scale  ; endif
'cbarn -sbar 'sbar' -snum 0.45 -xmid 'xmid' -ymid 'ybot' -scaley 0.4'
'set string 1 c 6'
'set strsiz 0.08'
if( flag = 1 ) ; 'draw string 'xmid' 'ytop' Root Phase Error (F-A)  (x10**'FMAm')'   ; endif
if( flag = 2 ) ; 'draw string 'xmid' 'ytop' Standard Deviation (F-A)  (x10**'FMAm')' ; endif


'set vpage off'
'set string 1 c 6'
'set strsiz .13'

'getinfo time'
         time = result
'getinfo tinc'
         tinc = result
         hour = (time-1)*tinc

'run getenv MONTHLAB'
            month = result
say 'MONTH_LABEL: 'month

'getinfo year'
         year  = result

'draw string 5.5  8.4 'DESC0'   'month' 'year'   'nfcst'-member Ensemble'
'draw string 5.5  8.12 Field: 'field'  (Zonal Average)   Hour: 'hour

'set strsiz .10'

*say 'Hit Enter to Continue ...'
*pull flag

return

function getstuff( q )

'q gxout'
   gxout = sublin(result,4)
   gxout = subwrd(gxout,6)

'set gxout shaded'
'shades 'q' 0'
         cint = result

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
'set gxout 'gxout
return icint' 'fact' 'scale' 'cmax' 'cmin
