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

* if( type = std ) ; string = "Std.Dev.  (Forecast-Analysis)"           ; endif
* if( type = var ) ; string = "Variance  (Forecast-Analysis)"           ; endif
* if( type = rms ) ; string = "RMS  (Forecast-Analysis)"                ; endif
* if( type = fma ) ; string = "MEAN  (Forecast-Analysis)"               ; endif
* if( type = fmc ) ; string = "MEAN  (Forecast-Climatology)"            ; endif
* if( type = mse ) ; string = "Mean_Square_Error  (Forecast-Analysis)"  ; endif
* if( type = mes ) ; string = "Mean_Error_Squared  (Forecast-Analysis)" ; endif
* if( type = rmes) ; string = "Root_Mean_Error_Squared  (Forecast-Analysis)" ; endif

'getinfo level'
         level = result

'run getinfo tinc'
             tinc = result
if( tinc = "NULL" ) ; tinc = 6 ; endif
'run getinfo time'
             time = result
             hour = (time-1)*tinc
if( hour < 10  ) ; hour = 0hour ; endif
if( hour < 100 ) ; hour = 0hour ; endif


'set datawarn off'
'set grid  off'
'set grads off'
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd mask'
'define  del'type' = 'field''type''tag1'-'field''type''tag2
'define  sum'type' = 'field''type''tag1'+'field''type''tag2
'define adel'type' = abs(del'type')'
'define sign'type' = maskout( del'type'/adel'type',adel'type'-1e-20)'

if( type = mes | type = var | type = mse )
   'define sqrt'type' = sqrt(adel'type')*sign'type
   'define diff = regrid2( sqrt'type',.25, .25, bs_p1, 0, -90 )'
else
   'define diff = regrid2(  del'type',.25, .25, bs_p1, 0, -90 )'
endif


'define NHEM  = aave( diff,lon=0,lon=360,lat= 20,lat= 80 )'
'define SHEM  = aave( diff,lon=0,lon=360,lat=-80,lat=-20 )'
'define GLOB  = aave( diff,lon=0,lon=360,lat=-90,lat= 90 )'
'define TROP  = aave( diff,lon=0,lon=360,lat=-20,lat= 20 )'

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

if( cint = '' )
        'stats del'type
         aSTD = subwrd(result,2)
if( type = mes | type = var | type = mse )
         'd sqrt( 'aSTD' )/3'
         aSTD = subwrd(result,4)
endif

'getint 'aSTD
      cint = result
      say 'N: 'n'  CINT: 'cint'  aSTD: 'aSTD' 'GLOB
 if( aSTD = 0 | aSTD = 9.999e+20 ) ; cint = 1 ; endif
         n = 0
     while( cint = 0 )
     say 'N: 'n'  CINT: 'cint'  aSTD: 'aSTD
*    say 'Hit ENTER to continue'
*    pull flag
            n = n + 1
        aSTD = 10*aSTD
       'getint 'aSTD
         cint = result
      endwhile
      'd pow(10,'n')'
      fact = subwrd(result,4)
      cint = cint/fact
endif

clevs = ''
n = -9
while ( n <= 9 )
  val = n * cint
if( n != 0 ) ; clevs = clevs' 'val ; endif
    n = n + 1
endwhile
'set ccols 59   57   55   47   44   37   36   34   33    0     21   22   23   24   25   26   27   28   29'

'set clevs 'clevs
'd maskout( diff,abs(diff)-'cint' )'

if( type = mes  ) ; 'cbarn -scale 0.55 -xmid 2.9 -ymid 4.25 -scaley 0.8 ' ; endif
if( type = var  ) ; 'cbarn -scale 0.55 -xmid 8.4 -ymid 4.25 -scaley 0.8 ' ; endif
if( type = mse  ) ; 'cbarn -scale 0.55 -xmid 5.6 -ymid 0.30 -scaley 0.8 ' ; endif

if( type = rmes ) ; 'cbarn -scale 0.55 -xmid 2.9 -ymid 4.25 -scaley 0.8 ' ; endif
if( type = std  ) ; 'cbarn -scale 0.55 -xmid 8.4 -ymid 4.25 -scaley 0.8 ' ; endif
if( type = rms  ) ; 'cbarn -scale 0.55 -xmid 5.6 -ymid 0.30 -scaley 0.8 ' ; endif

'getinfo month' 
         month = result
'getinfo year'
         year  = result

if( type = rmes | type = mes )
'set vpage off'
'set string 1 l 5'
'set strsiz 0.08'
     x1 = 0.70
     y1 = 3.70
y2 = y1 - 0.15
y3 = y2 - 0.15
y4 = y3 - 0.15
'draw string 'x1' 'y1' NHEM: 'NHEM
'draw string 'x1' 'y2' SHEM: 'SHEM
'draw string 'x1' 'y3' GLOB: 'GLOB
'draw string 'x1' 'y4' TROP: 'TROP
endif

if( type = rms | type = mse )
'set vpage off'
'set string 1 l 5'
'set strsiz 0.08'
     x1 = 8.40
     y1 = 1.62
y2 = y1 - 0.15
y3 = y2 - 0.15
y4 = y3 - 0.15
'draw string 'x1' 'y1' NHEM: 'NHEM
'draw string 'x1' 'y2' SHEM: 'SHEM
'draw string 'x1' 'y3' GLOB: 'GLOB
'draw string 'x1' 'y4' TROP: 'TROP
endif

if( type = std | type = var)
'set vpage off'
'set string 1 c 6'

'set strsiz 0.165'
'draw string 5.50 8.30 'month' 'year'   Forecast Hour: 'hour

'set string 1 l 6'
'set strsiz 0.15'
'draw string 0.38 2.46 'exp1
'draw string 0.38 2.00 'exp2
'set strsiz 0.12'
'draw string 0.42 2.23 minus'
'set strsiz 0.13'
'draw string 0.38 1.50 'numf'-member Ensemble'


'set string 1 l 5'
'set strsiz 0.08'
     x1 = 9.40
     y1 = 3.70
y2 = y1 - 0.15
y3 = y2 - 0.15
y4 = y3 - 0.15
'draw string 'x1' 'y1' NHEM: 'NHEM
'draw string 'x1' 'y2' SHEM: 'SHEM
'draw string 'x1' 'y3' GLOB: 'GLOB
'draw string 'x1' 'y4' TROP: 'TROP
endif

if( type = std )
'set vpage off'
'set string 1 c 6'
'set strsiz 0.13'
'draw string 3.00  7.9 'level'-mb 'field'  |BIAS`b1`n|-|BIAS`b2`n|'
'draw string 8.355 7.9 'level'-mb 'field'  Std_Dev`b1`n-Std_Dev`b2`n'
'draw string 5.650 3.9 RMS`b1`n-RMS`b2`n'
endif

if( type = var)
'set vpage off'
'set string 1 c 6'
'set strsiz 0.13'
'draw string 3.00  7.9 'level'-mb 'field'  SQRT[ BIAS`a2`n`b1`n-BIAS`a2`n`b2`n ]'
'draw string 8.355 7.9 'level'-mb 'field'  SQRT[ Std_Dev`a2`n`b1`n-Std_Dev`a2`n`b2`n ]'
'draw string 5.650 3.9 SQRT[ RMS`a2`n`b1`n-RMS`a2`n`b2`n ]'
endif

