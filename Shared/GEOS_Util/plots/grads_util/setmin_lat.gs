function setmin (args)

name = subwrd(args,1)

'minmax 'name
qmax = subwrd(result,1)
qmin = subwrd(result,2)
ymax = subwrd(result,4)
ymin = subwrd(result,6)

*if( qmax < 0    ) ; qmax = -qmax ; endif
*if( qmin < 0    ) ; qmin = -qmin ; endif
*if( qmin > qmax ) ; ymax =  ymin ; endif

'set y 'ymin
'getinfo 'lat
lat = result
'set lat 'lat
return  lat
