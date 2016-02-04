function myprint (args)

'numargs  'args
 numargs = result

'run getenv "LOGNAME"'
             LOGNAME = result

'run getenv "LOCHOST"'
             HOST = result
'run getenv "GEOSUTIL"'
             GEOSUTIL = result

'run getenv "ORIENTATION"'
             ORIENTATION = result
         if( ORIENTATION = 'NULL' )
            'getinfo pagex'
                     pagex = result
                 if( pagex = 8.5 ) ; ORIENTATION = 'PORTRAIT'  ; endif
                 if( pagex = 11  ) ; ORIENTATION = 'LANDSCAPE' ; endif
         endif

'run getenv "ANTIALIAS"'
             ANTIALIAS = result
         if( ANTIALIAS = 'NULL' )
             ANTIALIAS = ''
         else
             ANTIALIAS = '-antialias'
         endif

ASPRAT  = 0.7533
DENSITY = 90x90
BW      = NULL
NAME    = NULL
XDIM    = NULL
YDIM    = NULL
TYPE    = png
ROTATE  = 0

        n   = 0
        num = 0
while ( num < numargs )
        num = num + 1

if( subwrd(args,num) = '-name'    ) ; NAME    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-type'    ) ; TYPE    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-xdim'    ) ; XDIM    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-ydim'    ) ; YDIM    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-rotate'  ) ; ROTATE  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-density' ) ; DENSITY = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-asprat'  ) ; ASPRAT  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-bw'      ) ; BW      = TRUE               ; endif

endwhile

if( NAME = 'NULL' ) ; return ; endif

if( XDIM = 'NULL' & YDIM = 'NULL' )
    if( ORIENTATION = 'PORTRAIT' )
               XDIM =  675
               YDIM =  900
             ROTATE =    0
    endif
    if( ORIENTATION = 'LANDSCAPE' )
               XDIM =  1200
               YDIM =  XDIM * ASPRAT
             ROTATE =   90
    endif
endif
if( XDIM != 'NULL' & YDIM = 'NULL' )
    if( ORIENTATION = 'PORTRAIT' )
               YDIM =  XDIM / ASPRAT
             ROTATE =    0
    endif
    if( ORIENTATION = 'LANDSCAPE' )
               YDIM =  XDIM * ASPRAT
             ROTATE =   90
    endif
endif
if( XDIM = 'NULL' & YDIM != 'NULL' )
    if( ORIENTATION = 'PORTRAIT' )
               XDIM =  YDIM * ASPRAT
             ROTATE =    0
    endif
    if( ORIENTATION = 'LANDSCAPE' )
               XDIM =  YDIM / ASPRAT
             ROTATE =   90
    endif
endif

if( (LOGNAME = 'dao_ops_' & HOST = 'discover') )

say 'printim 'NAME'.gif 'TYPE' x'XDIM' y'YDIM
    'printim 'NAME'.gif 'TYPE' x'XDIM' y'YDIM

else

'enable  print 'NAME'.out'
'print'
'disable print'
if( BW = 'NULL' )
    '!'GEOSUTIL'/plots/grads_util/make_gif -name 'NAME' -rotate 'ROTATE' 'ANTIALIAS' -density 'DENSITY'     -xdim 'XDIM' -ydim 'YDIM' &'
else
    '!'GEOSUTIL'/plots/grads_util/make_gif -name 'NAME' -rotate 'ROTATE' 'ANTIALIAS' -density 'DENSITY' -bw -xdim 'XDIM' -ydim 'YDIM' &'
endif

endif

return
