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

DENSITY = 90x90
BW      = NULL
NAME    = NULL
TYPE    = png
ROTATE  = 0

if( ORIENTATION = 'PORTRAIT' )
           XDIM =  688
           YDIM =  916
         ROTATE =    0
endif
if( ORIENTATION = 'LANDSCAPE' )
           XDIM =  900
           YDIM =  680
         ROTATE =   90
endif

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
if( subwrd(args,num) = '-bw'      ) ; BW      = TRUE               ; endif

endwhile

if( NAME = 'NULL' ) ; return ; endif

if( (LOGNAME = 'dao_ops_' & HOST = 'discover') )

say 'printim 'NAME'.gif 'TYPE' x'XDIM' y'YDIM
    'printim 'NAME'.gif 'TYPE' x'XDIM' y'YDIM

else

'enable  print 'NAME'.out'
'print'
'disable print'
if( BW = 'NULL' )
    '!'GEOSUTIL'/plots/grads_util/make_gif -name 'NAME' -rotate 'ROTATE' 'ANTIALIAS' -density 'DENSITY'     &'
else
    '!'GEOSUTIL'/plots/grads_util/make_gif -name 'NAME' -rotate 'ROTATE' 'ANTIALIAS' -density 'DENSITY' -bw &'
endif

endif

return
