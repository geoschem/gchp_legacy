function shades (args)

*******************************************************
****                 INPUT Variables               ****
*******************************************************

'numargs  'args
 numargs = result

cntval = NULL
maxval = NULL
minval = NULL

        num = 0
while ( num < numargs )
        num = num + 1

if( subwrd(args,num) = '-cint'   ) ; cntval = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-maxval' ) ; maxval = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-minval' ) ; minval = subwrd(args,num+1) ; endif

endwhile

*************************************************************************************
* Utilitiy for EASY shading
*
* eg 1)  shades  0.5
*        produces shades for a +/- difference map with CINT=0.5
*
* eg 2)  shades  uwnd 0
*        produces shades for a field map with time-varying CINT
*
* eg 3)  shades  uwnd 3
*        produces shades for a field map with constant CINT after t=3
*
* eg 4)  shades  uwnd 3 TAG
*        produces TAG-Identified shades for a field map with constant CINT after t=3
*
*************************************************************************************

              name = subwrd(args,1)
 'lowercase  'name
              name = result

  'query file'
  numvar = sublin(result,6)
  numvar = subwrd(numvar,5)

* Determine whether ARG1 is a Field (flag=true) or a CINT (flag=false)
* --------------------------------------------------------------------
  flag = false
  n = 1
  while ( n<numvar+1 )
  field = sublin(result,6+n)
  field = subwrd(field,1)
        if( name=field )
        flag = true
        endif
  n = n+1
  endwhile

  if (flag=false)
  'query define'
         define = result
  n = 1
  field = sublin(result,n)
  field = subwrd(field,1)
  while ( field != "" & flag = "false" )
        if( name=field )
        flag = true
        endif
  n = n+1
  field = sublin(result,n)
  field = subwrd(field,1)
  endwhile
  endif
* ----------------------------------

    fixed = subwrd(args,2)
if( fixed = '' )

   if(flag=true)
             field = subwrd(args,1)
       'set gxout stat'
       'd 'field
       MINMAX = sublin(result,8)
      'set gxout shaded'
       if( minval = NULL )
           MIN  = subwrd(MINMAX,4)
       else
           MIN  = minval
       endif
       if( maxval = NULL )
           MAX  = subwrd(MINMAX,5)
       else
           MAX  = maxval
       endif
       if( cntval = NULL )
           cint = (MAX-MIN)/18
       else
           cint = cntval
       endif
       say 'Inside Shades1: MIN: 'MIN'  MAX: 'MAX'  CINT: 'CINT
   else
       if( cntval = NULL )
           cint = subwrd(args,1)
       else
           cint = cntval
       endif
   endif

    clevs = ''
    n = -9
    while ( n <= 9 )
      val = n * cint
    if( n != 0 ) ; clevs = clevs' 'val ; endif
        n = n + 1
    endwhile

    'set clevs 'clevs
    'set ccols 59   57   55   47   44   37   36   34   33    0     21   22   23   24   25   26   27   28   29'
     say 'Inside Shades1: CLEVS: 'clevs
     return cint

else

             field = subwrd(args,1)
             tag   = subwrd(args,3)
    'getinfo time'
             time = result
    if( time <= fixed | fixed = 0 )

       '!remove SHADES'tag'.txt'
       'set gxout stat'
       'd 'field
       MINMAX = sublin(result,8)
      'set gxout shaded'

       if( minval = NULL )
           MIN  = subwrd(MINMAX,4)
       else
           MIN  = minval
       endif
       if( maxval = NULL )
           MAX  = subwrd(MINMAX,5)
       else
           MAX  = maxval
       endif
       DQ     = (MAX-MIN)/19

       'getint 'DQ
                TEST = result
       say 'Initial CINT: 'TEST
       say 'MIN: 'MIN'  MAX: 'MAX'  DQ: 'DQ

     if( TEST = 0 & DQ != 0 )
               n = 0
         while( TEST = 0 )
         say 'N: 'n'  CINT: 'TEST
*        say 'Hit ENTER to continue'
*        pull flag
                n = n + 1
               DQ = 10*DQ
           'getint 'DQ
             TEST = result
          endwhile
          'd pow(10,'n')'
          fact = subwrd(result,4)
          if( cntval = NULL )
              CINT = TEST/fact
          else
              CINT = cntval
          endif
      else
          if( cntval = NULL )
              CINT = DQ
          else
              CINT = cntval
          endif
      endif
      say 'MIN: 'MIN'  MAX: 'MAX'  CINT: 'CINT

       clevs = ''
       n = 1 
       while ( n <= 19 )
       val.n = MIN + n * CINT
       clevs = clevs' 'val.n
           n = n + 1
       endwhile
       '!echo 'clevs' > SHADES'tag'.txt'
       'set gxout shaded'

    else

       'run getenv SHADES'tag
               clevs = result
    endif

   'set  ccols 0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
   'set  clevs  'clevs
    say 'CLEVS: 'clevs
    return CINT

endif

return CINT
