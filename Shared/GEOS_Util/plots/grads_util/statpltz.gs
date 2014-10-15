function statpltz (args)
field = subwrd (args,1)

'numargs  'args
 numargs = result

        num = 0
while ( num < numargs )
        num = num + 1
if( subwrd(args,num) = '-desc'   ) ; DESC  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-nfcst'  ) ; nfcst = subwrd(args,num+1) ; endif
endwhile

'getinfo pagex'
         pagex = result
     if( pagex = 8.5 ) ; 'setenv ORIENTATION PORTRAIT'  ; endif
     if( pagex = 11  ) ; 'setenv ORIENTATION LANDSCAPE' ; endif

'getinfo level'
         level = result

* Initialize Plot Values
* ----------------------
    CCOLS = '59   57   55   47   44   37   36   34   33    0     21   22   23   24   25   26   27   28   29'
    CLAB  =  off

    scale = 1.0

if( field = "h" )
    name  = "Heights"
    unit  = "(m)"
    label = "hght"
    RCOLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
    cint  = 1000
    rbrange = '1000 16000'
    CLEVS = '-135 -120 -105 -90 -75 -60 -45 -30 -15 15 30 45 60 75 90 105 120 135'
    CCINT = 15
    DLEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2 4 6 8 10 12 14 16 18'
    DCINT = 2
    RLEVS = '2  4  6  8  10  15  20  25  30  40  50  60  70  80  90  100  120  140'
    RCINT = 2
endif

if( field = "u" )
    name  = "U-Wind"
    unit  = "(m/sec)"
    label = "uwnd"
    cint  = 5
    rbrange = '-10 30'
    RCOLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
    CLEVS = '-9 -8 -7 -6 -5 -4 -3 -2 -1 1 2 3 4 5 6 7 8 9'
    CCINT = 1
    DLEVS = '-2.7 -2.4 -2.1 -1.8 -1.5 -1.2 -.9 -.6 -.3 .3 .6 .9 1.2 1.5 1.8 2.1 2.4 2.7'
    DCINT = 0.3
    RLEVS = '1  2  3   4   5   6   7   8  9  10  11  12  13  14   15   16   17 18'
    RCINT = 1
endif

if( field = "v" )
    name  = "V-Wind"
    unit  = "(m/sec)"
    label = "vwnd"
    cint  = 0.5
    rbrange = '-3 3'
    RCOLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
    CLEVS = '-1.35 -1.20 -1.05 -.90 -.75 -.60 -.45 -.30 -.15 .15 .30 .45 .60 .75 .90 1.05 1.20 1.35'
    CCINT = 0.15
    DLEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
    DCINT = 0.1
    RLEVS = '1  2  3   4   5   6   7   8  9  10  11  12  13  14   15   16   17 18'
    RCINT = 1
endif

if( field = "t" )
    name  = "Temperature"
    unit  = "(K)"
    label = "tmpu"
    RCOLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
    cint  = 5
    rbrange = '230 270'
    CLEVS = '-2.7 -2.4 -2.1 -1.8 -1.5 -1.2 -.9 -.6 -.3 .3 .6 .9 1.2 1.5 1.8 2.1 2.4 2.7'
    CCINT = 0.3
    DLEVS = '-1.35 -1.20 -1.05 -.90 -.75 -.60 -.45 -.30 -.15 .15 .30 .45 .60 .75 .90 1.05 1.20 1.35'
    DCINT = 0.15
    RLEVS = '.2  .4  .6 .8  1  1.2 1.4 1.6 1.8 2 2.3 2.5 2.7  3   3.5  4   4.5  5.0  '
    RCINT = 0.2
endif

if( field = "q" )
    name  = "Specific Humidity"
    unit  = "(g/g)"
    label = "sphu"
    scale = 1000
    RCOLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
    cint  = 1
    rbrange = '1 16'
    CLEVS = '-1.35 -1.20 -1.05 -.90 -.75 -.60 -.45 -.30 -.15 .15 .30 .45 .60 .75 .90 1.05 1.20 1.35'
    CCINT = 0.15
    DLEVS = '-1.35 -1.20 -1.05 -.90 -.75 -.60 -.45 -.30 -.15 .15 .30 .45 .60 .75 .90 1.05 1.20 1.35'
    DCINT = 0.15
    DLEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
    DCINT = 0.1
    RLEVS = '.1  .2  .3   .4   .5   .6   .7   .8  .9  1.0  1.2  1.4  1.6  1.8   2.0   2.2   2.4 2.6'
    RCINT = 0.1
endif


'set mproj latlon'
'set vpage off'
'set parea off'
*'set xlopts 1 3 .14'
*'set ylopts 1 3 .14'


'vpage 1 1 2 2'
'set grads off'
'set grid  off'
'set clab 'CLAB
'set gxout contour'
'set ccolor rainbow'
'set cint 'cint
'set rbrange 'rbrange
'd 'field'fmeanz*'scale


'define diff = 'field'fmcz'
'vpage 2 1 2 2'
'set grads off'
'set grid  off'
'set gxout shaded'
'set CCOLS 'CCOLS
'set CLEVS 'CLEVS
'd diff*'scale
'cbarn -xmid 6.0'


'define diff = 'field'fmaz'
'vpage 1 2 2 2'
'set grads off'
'set grid  off'
'set gxout shaded'
'set CCOLS 'CCOLS
'set CLEVS 'DLEVS
'd diff*'scale
'cbarn -xmid 6.0'


'define diff = 'field'stdz'
'vpage 2 2 2 2'
'set grads off'
'set grid  off'
'set gxout shaded'
'set CCOLS 'RCOLS
'set CLEVS 'RLEVS
'd diff*'scale
'cbarn -xmid 6.0'


'set vpage off'
'set string 1 c 6'
'set strsiz .13'

'getinfo time'
         time = result
'getinfo tinc'
         tinc = result
         hour = (time-1)*tinc

'getinfo month'
         month = result
'getinfo year'
         year  = result

'draw string 5.5  8.4 'DESC'   'month' 'year'   'nfcst'-member Ensemble'
'draw string 5.5  8.12 Field: 'field'  (Zonal Average)   Hour: 'hour

'set strsiz .10'
'draw string 3.12 7.86 Forecast'
'draw string 8.46 7.86 Forecast-Climatology'
'draw string 3.12 3.89 Mean (Forecast-Analysis)'
'draw string 8.46 3.89 Standard Deviation (F-A)'
