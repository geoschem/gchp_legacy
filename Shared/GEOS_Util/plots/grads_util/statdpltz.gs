function stdiff (args)
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
         
if( type = std ) ; string = "Std.Dev.  (Forecast-Analysis):"           ; endif
if( type = var ) ; string = "Variance  (Forecast-Analysis):"           ; endif
if( type = rms ) ; string = "RMS  (Forecast-Analysis):"                ; endif
if( type = fma ) ; string = "MEAN  (Forecast-Analysis):"               ; endif
if( type = fmc ) ; string = "MEAN  (Forecast-Climatology):"            ; endif
if( type = mse ) ; string = "Mean_Square_Error  (Forecast-Analysis):"  ; endif
if( type = mes ) ; string = "Mean_Error_Squared  (Forecast-Analysis):" ; endif
if( type = rmes) ; string = "Root_Mean_Error_Squared  (Forecast-Analysis)" ; endif

'run getinfo tinc'
             tinc = result
if( tinc = "NULL" ) ; tinc = 6 ; endif
'run getinfo time'
             time = result
             hour = (time-1)*tinc
if( hour < 10  ) ; hour = 0hour ; endif
if( hour < 100 ) ; hour = 0hour ; endif

                  scale = 1
if( field = q ) ; scale = 1000 ; endif

'set datawarn off'
'set grid  off'
'set grads off'
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'define diff = ('field''type''tag1'z-'field''type''tag2'z)*'scale

if( cint = '' )
        'set gxout stat'
        'd diff'
         info  = sublin(result,13)
         aGLOB = subwrd(info,2)
        'set gxout shaded'
'getint 'aGLOB
      cint = result
 if( aGLOB = 0 ) ; cint = 1 ; endif
         n = 0
     while( cint = 0 )
     say 'N: 'n'  CINT: 'cint'  aGLOB: 'aGLOB
*    say 'Hit ENTER to continue'
*    pull flag
            n = n + 1
        aGLOB = 10*aGLOB
       'getint 'aGLOB
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
'd diff'

if( type = rmes ) ; 'cbarn -scale 0.55 -xmid 2.9 -ymid 4.1 -scaley 0.8 ' ; endif
if( type = std  ) ; 'cbarn -scale 0.55 -xmid 8.4 -ymid 4.1 -scaley 0.8 ' ; endif
if( type = rms  ) ; 'cbarn -scale 0.55 -xmid 5.6 -ymid 0.3 -scaley 0.8 ' ; endif

'getinfo month'
         month = result
'getinfo year'
         year  = result

if( type = std )
'set vpage off'
'set string 1 c 6'

'set strsiz 0.165'
'draw string 5.50 8.20 'month' 'year'   Forecast Hour: 'hour

'set string 1 l 6'
'set strsiz 0.15'
'draw string 0.38 2.46 'exp1
'draw string 0.38 2.00 'exp2
'set strsiz 0.12' 
'draw string 0.42 2.23 minus'
'set strsiz 0.13' 
'draw string 0.38 1.50 'numf'-member Ensemble'

'set string 1 c 6'
'set strsiz 0.13'
'draw string 3.00  7.75 Zonal Mean 'field'  |F`bMean`n-A`bMean`n|'
'draw string 8.355 7.75 Zonal Mean 'field'  Standard Deviation (F-A)'
'draw string 5.650 3.9 RMS'
endif

