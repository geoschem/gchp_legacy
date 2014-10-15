function writit (name)
p.1  = 1000
p.2  =  925
p.3  =  850
p.4  =  700
p.5  =  600
p.6  =  500
p.7  =  400
p.8  =  300
p.9  =  250
p.10 =  200
p.11 =  150
p.12 =  100
p.13 =   70
p.14 =   50
p.15 =   30
p.16 =   10

'getinfo zdim'
         zdim = result
L = 1
while(L<=16)

z = 1
while(z<=zdim)
'set z 'z
'getinfo level'
         lev = result
if(lev=p.L)
say 'Writing 'name' for Level: 'lev
'd 'name
endif
z = z+1
endwhile

L = L+1
endwhile
say '   '
return
