*function arrow (label,loc,name,flag)
function arrow (args)

label = subwrd(args,1)
  loc = subwrd(args,2)
 name = subwrd(args,3)
 flag = subwrd(args,4)

*
* script to draw an arrow w/label
*

if (flag!=1)
say "Set Arrow Location (Begin Arrow to End Arrow):"
'getinfo pagex'
pagex = result
'getinfo pagey'
pagey = result
'set rband 21 box 0 0 'pagex' 'pagey
'q pos '
x1 = subwrd(result,3)
y1 = subwrd(result,4)
x2 = subwrd(result,8)
y2 = subwrd(result,9)
write(name,x1,append)
write(name,y1,append)
write(name,x2,append)
write(name,y2,append)
write(name,label,append)
write(name,loc,append)
else
x1 = sublin (read(name),2)
y1 = sublin (read(name),2)
x2 = sublin (read(name),2)
y2 = sublin (read(name),2)
label = sublin (read(name),2)
loc   = sublin (read(name),2)
endif

*pi = 3.141592654
*'d sin(30*'pi'/180)'
*sin30 = subwrd(result,4)
*'d cos(30*'pi'/180)'
*cos30 = subwrd(result,4)
*'d sin(60*'pi'/180)'
*sin60 = subwrd(result,4)
*'d cos(60*'pi'/180)'
*cos60 = subwrd(result,4)

*'d sqrt( pow('x1'-'x2',2) + pow('y1'-'y2',2) )'
*dist = subwrd(result,4)
*say 'Distance = 'dist

*xl = x2 - 0.1 * dist * sin60 / cos30
*yl = y2 - 0.1 * dist * cos60 / cos30

'set line 1'
'draw line 'x1' 'y1' 'x2' 'y2

*'set line 3'
*'draw line 'xl' 'yl' 'x2' 'y2

if( y2 > y1 )
  if( x1 < x2 )
     'set string 1 r 4'
  else
     'set string 1 l 4'
  endif
    'draw string 'x1' 'y1-0.1' 'label
else
  if( x1 < x2 )
     'set string 1 r 4'
  else
     'set string 1 l 4'
  endif
    'draw string 'x1' 'y1+0.1' 'label
endif

return
