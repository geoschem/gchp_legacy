function make (args)
iname = subwrd(args,1)
oname = subwrd(args,2)
 file = subwrd(args,3)
  lev = subwrd(args,4)
'set dfile 'file
'set lat -90 90'
'set lon 0 360'
'sett'

'getinfo zdim'
zdim = result
if( zdim = '1' )
'set z 1'
else
'set lev 'lev
endif

'define   'oname''file' = 'iname
'seasonal 'oname''file' 'lev
'undefine 'oname''file
return
