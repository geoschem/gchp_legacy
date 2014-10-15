function getmax (args)
name = subwrd(args,1)
'q gxinfo'
gxout = sublin(result,1)
gxout = subwrd(gxout ,4)
 
'set gxout stat'
'd 'name
max = sublin(result,8)
max = subwrd(max,5)
'set gxout 'gxout

return max
