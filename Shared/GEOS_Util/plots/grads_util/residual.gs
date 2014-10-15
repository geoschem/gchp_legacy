function residual

'setx'
'sety'
'setz'
'sett'
 say ' '
 say 'Making Zonal Means ...'
'makez tmpu z'
'makez vwnd z'
'makez vptp z'

'set x 1'
'define pl = lev'
'define pk = pow(pl,2/7)'

'set gxout fwrite'

'getinfo tmax'
         tmax = result
'getinfo zmax'
         zmax = result

* Write data
* ----------
t=1
while(t<=tmax)
  'set t 't
   say 'Writing Data T = 't

   z=1
   while(z<=zmax)
  'set z 'z
  'd vwndz'
  'd tmpuz'
  'd vptpz'
  'd pl'
  'd pk'
   z=z+1
   endwhile

t=t+1
endwhile
