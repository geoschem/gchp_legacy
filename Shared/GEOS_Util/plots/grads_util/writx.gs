function write (name)

'set gxout fwrite'
'set x 1'
'set lat -90 90'

'getinfo zdim'
         zdim = result
'getinfo tdim'
         tdim = result
t = 1
while(t<=tdim)
'set t 't

z = 1
while(z<=zdim)
'set z 'z
'getinfo level'
         lev = result
say 'Writing 'name' for Level: 'lev' and Time: 't
'd 'name
z = z+1
endwhile
say '   '

t = t+1
endwhile
return
