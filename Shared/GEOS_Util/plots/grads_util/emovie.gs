function emovie (args)
name = subwrd(args,1)

*
* This routine will animate any multi-quantity plot
* produced by another run_script.
*
* ARGS:  The name of the run_script you wish to animate
* eg:  run /home/takacs/grads/movie /home/takacs/grads/fourpanel.plt
*
'numargs  'args
 numargs = result

if( numargs = 0 ) 
    say 'Usage: emovie name [-tbeg tbeg]'
    say '                   [-tend tend]'
    say '                   [-print    ]'
    say '                   [-pause    ]'
    return
endif

      tbeg = 0
      tend = 0
    rotate = false
     print = false
     pause = false
       num = 0
while( num < numargs )
       num = num + 1
if( subwrd(args,num)='-tbeg'   ) ; tbeg   = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-tend'   ) ; tend   = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-print'  ) ; print  = true               ; endif
if( subwrd(args,num)='-pause'  ) ; pause  = true               ; endif
if( subwrd(args,num)='-rotate' ) ; rotate = true               ; endif
if( subwrd(args,num)='-name'   ) ; name   = subwrd(args,num+1) ; endif
endwhile

if( tbeg != 0 )
    t     = tbeg
else
    t     = 1
endif

if( tend != 0 )
    tmax  = tend
else
    tmax  = gettmax()
endif

say 't = 't
say 'tmax = 'tmax
say 'print = 'print

frame = 1000

'run getenv "GEOSUTIL"'
             geosutil = result

'set grid on'
if( print != true ) ; 'set dbuff on' ; endif
'set csmooth on'
while (t<tmax+1)
      'set t 't
      frame = frame + 1
      'exec 'args
if( print != true )
      if( pause = true )
          say 'Hit -Enter- to continue ...'
          pull flag
      endif
     'swap'
else
*    'printim 'name'.'frame'.png x900 y720'
      if( rotate = true )
     'myprint -name 'name'.'frame' -rotate 90'
      else
     'myprint -name 'name'.'frame' -rotate 0'
      endif
      if( pause = true )
          say 'Hit -Enter- to continue ...'
          pull flag
      endif
     'c'
endif
      t = t+1
endwhile

function gettmax(args)
  'query file '
  tmax  = sublin(result,5)
  tmax  = subwrd(tmax,12)
return tmax
