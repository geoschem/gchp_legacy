function path (args)

name   = subwrd(args,1)
ccol   = subwrd(args,2)
tmax = sublin( read(name),2 )

t = 1

while (t<tmax+1)

'set grads off'
  if( t=1 ) 
    'set gxout contour'
    'set cint 2000'
    'd slp/100'
  endif
lon.t = sublin( read(name),2 )
lat.t = sublin( read(name),2 )
pmin  = sublin( read(name),2 )
wmax  = sublin( read(name),2 )

    'q w2xy 'lon.t' 'lat.t
     x = subwrd(result,3)
     y = subwrd(result,6)
    'set line 1'
    'draw mark 3 'x' 'y' 0.1'
    'set line 'ccol
    'draw mark 3 'x' 'y' 0.05'

  if( t>1 ) 
      'set line 'ccol' 1 1'
      'draw line 'xold' 'yold' 'x' 'y
  endif
 
  xold = x
  yold = y
     t = t + 1
endwhile 
