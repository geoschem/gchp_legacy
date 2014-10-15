function pltcor (args)

*******************************************************
****                 INPUT Variables               ****
*******************************************************

'numargs  'args
 numargs = result

SOURCE = NULL
DESC   = Stats

        num = 0
while ( num < numargs )
        num = num + 1

if( subwrd(args,num) = '-source' ) ; SOURCE = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-desc'   ) ; DESC   = subwrd(args,num+1) ; endif

endwhile

if( SOURCE != NULL )
   'run setenv "SOURCE" 'SOURCE
else
   'run getenv "PWD"'
            PWD = result
   'run setenv "SOURCE" 'PWD
endif
   'run getenv "SOURCE"'
            SOURCE = result

   'run getenv "GEOSUTIL"'
            geosutil = result

*******************************************************
****                    Begin ...                  ****
*******************************************************

'reinit'
'exec setup1'
'set display color white'
'rgbset'
'c'

x = 1
while ( x<=10 )
'set  x 'x
'corz h 'DESC
'c'
'corz u 'DESC
'c'
'corz v 'DESC
'c'
'corz t 'DESC
'c'
'corz q 'DESC
'c'
x = x + 1
endwhile

return
