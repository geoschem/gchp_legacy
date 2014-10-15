function writegrads(args)

'run getenv ZFLIP'
            zflip = result

*************************************
* Note: To Write Data in Reverse Order
*       run setenv ZFLIP ON
*************************************

'setx'
'sety'
'set gxout fwrite'

'getinfo tdim'
         tdim  = result
'getinfo nvars'
         nvars = result

t=1
while(t<=tdim)
  'set t 't

n=1
while(n<=nvars)

'run getvarz 'n
    var  = result
   name  = subwrd(result,1)
   zdim  = subwrd(result,2)

   if( zdim = 0 )
       say 'Writing Variable: 'name
      'd 'name
   else

      if(zflip != 'ON' )
         z=1
         while(z<=zdim)
            'set z 'z
            'getinfo level'
             lev = result
             say 'Writing Variable: 'name' for Level: 'lev
            'd 'name
         z=z+1
         endwhile
      else
         z=zdim
         while(z>=1)
            'set z 'z
            'getinfo level'
             lev = result
             say 'Writing Variable: 'name' for Level: 'lev
            'd 'name
         z=z-1
         endwhile
      endif

   endif
       say '  '


n=n+1
endwhile
t=t+1
endwhile

return
