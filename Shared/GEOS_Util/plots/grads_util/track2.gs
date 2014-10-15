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
    pmin  = sublin( read(name),2 )
    wmax  = sublin( read(name),2 )
    t = t + 1
    endwhile
else
    t = tbeg
endif

if( tbeg!=tend )
*'set dbuff on'
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
    pmin  = sublin( read(name),2 )
    wmax  = sublin( read(name),2 )

* Draw Background BaseMap
* -----------------------
*   'set gxout contour'
*   'set ccolor 0'
*   'set clab  off'
*   'set grads off'
*   'd slp'
*   'set rgb 99 236 236 236'
*   'basemap L 99 1'
*   'set ccolor rainbow'

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
    'set line 4'
    'draw mark 3 'x' 'y' 0.05'
    if (i>1) 
      'set line 1 1 3'
      'draw line 'xold' 'yold' 'x' 'y
      'set line 4 1 1'
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

endif
