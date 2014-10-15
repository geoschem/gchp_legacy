function compare(args)

'run getenv ZFLIP'
            zflip = result

*************************************
* Note: To Compare Data in Reverse Order
*       run setenv ZFLIP ON
*************************************

field = subwrd(args,1)

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

if( field = name | field = '' )

   if( zdim = 0 )
      'run compare_plot 'name
       say ' '
       say 'Writing Variable: 'name
       say "Hit  ENTER  to continue ..."
       pull flag
      'c'
   else

      if(zflip != 'ON' )
         z=1
         while(z<=zdim)
            'set z 'z
            'getinfo level'
             lev = result
            'run compare_plot 'name
             say ' '
             say 'Writing Variable: 'name' for Level: 'lev
             say "Hit  ENTER  to continue ..."
             pull flag
            'c'
         z=z+1
         endwhile
      else
         z=zdim
         while(z>=1)
            'set z 'z
            'getinfo level'
             lev = result
            'run compare_plot 'name
             say ' '
             say 'Writing Variable: 'name' for Level: 'lev
             say "Hit  ENTER  to continue ..."
             pull flag
            'c'
         z=z-1
         endwhile
      endif

   endif
       say '  '

endif

n=n+1
endwhile
t=t+1
endwhile

return
