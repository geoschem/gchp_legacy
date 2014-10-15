function writeclim
'set t 1'

'getinfo nvars'
         nvars = result
say 'nvars = 'nvars


n=1
while(n<=nvars)

    var = getldim(n)
   name = subwrd(var,1)
   levs = subwrd(var,2)

'fixname 'name
          alias = result

* Ensure NAME has no underscores
* ------------------------------
*   test = name
*   max  = 20
*   end  = 0
*  alias = ""
*        k = 1
*while ( k<max )
*  bit = substr(test,k,1)
*  if( bit = "" )
*        k = max
*  else
*        if( bit != "_" ) ; alias = alias % bit ; endif
*        k = k+1
*  endif
*endwhile

if(  name !=  alias )
say 'Alias = 'alias
'setx'
'sety'
'setz'
'sett'
'rename 'name' 'alias
endif


'run uppercase 'alias
                name = result

say 'var  = 'var
say 'name = 'name
say 'levs = 'levs

'!remove    'name'_clim.data'
'set gxout fwrite'
'set fwrite 'name'_clim.data'

   if( levs = 0 )

      'setx'
      'sety'
      'set z 1'
      'sett'

       say 'Computing Seasonal for: 'name
           'seasonal 'name

       t=1
       while(t<=12)
      'set t 't
       say 'Writing Variable:    'name' for Time = 't
      'd    'name'clim'

       t=t+1
       endwhile

   else

      'setx'
      'sety'
      'setz'
      'sett'

       say 'Computing Seasonal for: 'name
           'seasonal 'name

       t=1
       while(t<=12)
      'set t 't
       say 'Writing Data for Time = 't

       z=1
       while(z<=levs)
          'set z 'z
          'getinfo level'
           lev = result
           say 'Writing Variable: 'name' for Level: 'lev
          'd    'name'clim'
       z=z+1
       endwhile

       t=t+1
       endwhile

   endif

'disable fwrite'
say '  '

n=n+1
endwhile

return

function getldim (args)
*
* Returns the NAME and NUMLEVs for Variable # of Input
*
* eg) getvar 7
*     Returns the 7th variable and its number of levels
*
loc = subwrd(args,1)
say 'loc = 'loc
'q file'
varinfo = sublin(result,6+loc)
name    = subwrd(varinfo,1)
levs    = subwrd(varinfo,2)

return name' 'levs
