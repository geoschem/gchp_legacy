function getmin (args)
name = subwrd(args,1)

'q gxinfo'
gxout = sublin(result,1)
gxout = subwrd(gxout ,4)
 
'set gxout stat'
'd 'name
min = sublin(result,8)
min = subwrd(min,4)
'set gxout 'gxout

return min
