function tcor   (args)
field  = subwrd (args,1)
fday   = subwrd (args,2)
expdsc = subwrd (args,3)

* ------------------------------------------------------------------
* Note:  fday is the current forecast day to plot (eg, 1 2 3 4 or 5)
* ------------------------------------------------------------------

'run getenv "GEOSUTIL"'
             geosutil = result
'run getenv "SOURCE"'
             source   = result

'numargs  'args
 numargs = result
        num = 0
while ( num < numargs )
        num = num + 1
if( subwrd(args,num) = '-source'   ) ; source = subwrd(args,num+1) ; endif
endwhile

if( geosutil = "NULL" ) ; say 'Environment Variable GEOSUTIL must be set!' ; return ; endif
if( source   = "NULL" ) ; say 'Environment Variable SOURCE   must be set or passed in <-source SOURCE> !' ; return ; endif

* Check for OUTPUT directory
* --------------------------
'!mkdir -p 'source'/plots'
output   = source  % '/' % plots


* Define Plot Labels and Contour Levels
* -------------------------------------
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

* Extract Date and Plot Characteristics (Location, Level, etc.)
* -------------------------------------------------------------
'set t 1'
'getinfo date'
         date = result
        month = substr(date,6,3)

'getinfo xpos'
         xpos  = result
'getinfo level'
         level = result
'getinfo numfiles'
         numfiles = result


* Define Total Time in Plot (Assuming 5-day forecast for each File)
* -----------------------------------------------------------------
'run getenv "TINC"'
             tinc = result
if( tinc = "NULL" ) ; tinc = 6 ; endif
                      freq = 24/tinc
                      tmax = 1 + (numfiles+5)*freq


* Loop over Files to Plot
* -----------------------
file = 1
ave  = 0

while (file<=numfiles)

      'set   dfile 'file
              time = 1+freq*fday
      'set t 'time
      'd 'field'cor'
       val = subwrd(result,4)
      'getinfo date'
               date = result
      'set dfile 1'
      'set axlim 0.60 1.05'
      'set t 1 'tmax
      'set ylab %.2f'
      'set cmark 0'
      'set ccolor 2'
      'd 1-lev+lev'

       ave = ave + val
       'q w2xy 'date' 'val
           x = subwrd(result,3)
           y = subwrd(result,6)
           'set line 'fday
           'draw mark 3 'x' 'y' 0.1'
            if( t>1 )
           'set  line 'fday' 1 1'
           'draw line 'x0' 'y0' 'x' 'y
            endif
           x0 = x
           y0 = y

         file = file + 1
endwhile

'define gg = 'ave/numfiles
'set t 1'
'd gg'
   gg = subwrd(result,4)
'q w2xy 'date' 'gg
    y = subwrd(result,6)
'set t 1 'tmax
'set cmark 0'
'set ccolor 4'
'd gg'

'set string 1 l 5'
'set strsiz .08'
'draw string 10.5477 'y' 'gg 
   
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

if( fday = 5 )
   'set string 1 c 6'
   'set strsiz .14'
   'draw string 6.25967 8.00 'level'-mb 'name'  'region
   'draw string 6.25967 8.35 'expdsc
   'draw ylab Anomaly Correlation'
   
   'myprint -name 'output'/stats_'label'_tcor_'reg'_'level'_'month' -rotate 90'
endif
   
return
