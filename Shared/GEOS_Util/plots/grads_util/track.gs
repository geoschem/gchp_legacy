function track (args)

********************************************************
****
****  Usage:  track filename <tbeg> <tend>
****
********************************************************


* Determine if tracking file exists
* ---------------------------------
if (args="")
    name   = dum.track
    ioflag = 0
    tbeg   = 1
    'getinfo tdim'
    tend   = result
    write(name,tend)
else
    name   = subwrd(args,1)
    tbeg   = subwrd(args,2)
    tend   = subwrd(args,3)
    if(tbeg="" ) ; tbeg=1 ; endif
    if(tend="" )
       'getinfo tdim'
       tend   = result
    endif
    ioflag = 1-sublin( read(name),1 )
    if(ioflag!= 0)
       close  = close(name)
    else
       write(name,tend)
    endif
endif

* Get tbeg & tend for time loop
* -----------------------------
if (ioflag!=0)
    dummy = sublin( read(name),2 )
    t = 1
    while (t<tbeg)
    lon.t = sublin( read(name),2 )
    lat.t = sublin( read(name),2 )
    slp.t = sublin( read(name),2 )
    wmax  = sublin( read(name),2 )
    t = t + 1
    endwhile
else
    t = tbeg
endif

if( tbeg!=tend )
'set dbuff on'
endif


********************************************************
****
****                  Create New Track
****
********************************************************

if (ioflag=0)

frame = 1000

while (t<tend+1)
  'set t 't
   frame = frame + 1

* Draw Background BaseMap
* -----------------------
    'set gxout contour'
    'set ccolor 0'
    'set clab  off'
    'set grads off'
    'd slp'
    'set rgb 99 236 236 236'
    'basemap L 99 1'
    'set ccolor rainbow'

* Plot Main Picture (User Defined within trackplot.gs)
* ----------------------------------------------------
  'trackplot.gs'

* Draw Track to Previous Position
* -------------------------------
  i = 1
  while (i<t) 
    'q w2xy 'lon.i' 'lat.i
    x = subwrd(result,3)
    y = subwrd(result,6)
    'set line 1'
    'draw mark 3 'x' 'y' 0.1'
    'set line 8'
    'draw mark 3 'x' 'y' 0.05'
    if (i>1) 
      'set line 1 1 3'
      'draw line 'xold' 'yold' 'x' 'y
      'set line 8 1 1'
      'draw line 'xold' 'yold' 'x' 'y
    endif
    xold = x
    yold = y
    i = i + 1
  endwhile

'swap'
 
* Re-establish World Environment after SWAP
* -----------------------------------------
    'set gxout contour'
    'set ccolor 0'
    'set clab  off'
    'set grads off'
    'd slp'
    'set ccolor rainbow'

* Draw Mark at Current Position
* -----------------------------
say 'Click on center position'
    'q pos'
     x = subwrd(result,3)
     y = subwrd(result,4)
    'q xy2w 'x' 'y
    lon.t = subwrd(result,3)
    lat.t = subwrd(result,6)
   'define s = sqrt( us*us + vs*vs )'
   'set gxout stat'
   'set grads off'
   'd slp/100'
    tmp  = sublin(result,8)
    pmin = subwrd(tmp,4)
    slp.t = pmin
   'd s'
    tmp  = sublin(result,8)
    wmax = subwrd(tmp,5)
    write(name,lon.t,append)
    write(name,lat.t,append)
    write(name,slp.t,append)
    write(name,wmax ,append)

* Final Picture
* -------------
    if(t=tend)

* Draw Background BaseMap
* -----------------------
    'set gxout contour'
    'set ccolor 0'
    'set clab  off'
    'set grads off'
    'd slp'
    'set rgb 99 236 236 236'
    'basemap L 99 1'
    'set ccolor rainbow'

* Plot Main Picture (User Defined within trackplot.gs)
* ----------------------------------------------------
  'trackplot.gs'

* Draw Track to Previous Position
* -------------------------------
  i = 1
  while (i<=t) 
    'q w2xy 'lon.i' 'lat.i
    x = subwrd(result,3)
    y = subwrd(result,6)
    'set line 1'
    'draw mark 3 'x' 'y' 0.1'
    'set line 8'
    'draw mark 3 'x' 'y' 0.05'
    if (i>1) 
      'set line 1 1 3'
      'draw line 'xold' 'yold' 'x' 'y
      'set line 8 1 1'
      'draw line 'xold' 'yold' 'x' 'y
    endif
    xold = x
    yold = y
    i = i + 1
  endwhile

'swap'
endif
 
t = t + 1

endwhile 

* 'wi geos1.'frame'.gif'
   close = close(name)

endif

********************************************************
****
****                    Display Track
****
********************************************************

if (ioflag!=0)

frame = 1000

while (t<tend+1)
  'set t 't
   frame = frame + 1

* Retrieve Track Position
* -----------------------
    lon.t = sublin( read(name),2 )
    lat.t = sublin( read(name),2 )
    slp.t = sublin( read(name),2 )
    wmax  = sublin( read(name),2 )

* Draw Background BaseMap
* -----------------------
    'set gxout contour'
    'set ccolor 0'
    'set clab  off'
    'set grads off'
    'd slp'
    'set rgb 99 236 236 236'
    'basemap L 99 1'
    'set ccolor rainbow'

* Plot Main Picture (User Defined within trackplot.gs)
* ----------------------------------------------------
  'trackplot.gs'

* Draw Track to Current Position
* ------------------------------
  i = 1
  while (i<=t) 
    'q w2xy 'lon.i' 'lat.i
    x = subwrd(result,3)
    y = subwrd(result,6)
    'set line 1'
    'draw mark 3 'x' 'y' 0.1'
    'set line 8'
    'draw mark 3 'x' 'y' 0.05'
    if (i>1) 
      'set line 1 1 3'
      'draw line 'xold' 'yold' 'x' 'y
      'set line 8 1 1'
      'draw line 'xold' 'yold' 'x' 'y
    endif
    xold = x
    yold = y
    i = i + 1
  endwhile
 
  'swap'
 
t = t + 1

endwhile 

* 'wi geos1.'frame'.gif'
   close = close(name)

* Write SLP Trace file
* --------------------
'!remove sedfile'
'!touch  sedfile'
'!remove 'name'.ctl'
'!remove 'name'.data'

'run getenv "GEOSUTIL"'
             geosutil = result

'q ctlinfo'
         n = 1
      while( n >0 )
             line = sublin(result,n)
             word = subwrd(line,1)
         if( word = 'tdef' )
             dt = subwrd(line,5)
             n  = 0
          else
             n  = n + 1
          endif
      endwhile

'set t 'tbeg
'getinfo date'
      begdate = result

'set gxout fwrite'
'set fwrite 'name'.data'
tdim = tend-tbeg+1
i = 1
while( i<=tdim )
'd 'slp.i
i = i + 1
endwhile
'disable fwrite'
'set gxout contour'

'!echo "s?track.data?"'name'.data?g >> sedfile'
'!echo "s?DT?"'dt'?g >> sedfile'
'!echo "s?TDIM?"'tdim'?g >> sedfile'
'!echo "s?BDATE?"'begdate'?g >> sedfile'
'!/bin/cp 'geosutil'/plots/grads_util/track.tmpl .'
'!sed -f   sedfile track.tmpl > 'name'.ctl'

endif
