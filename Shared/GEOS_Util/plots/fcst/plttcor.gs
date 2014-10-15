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
'exec setup2'
'set display color white'
'rgbset'
'c'

z = 1
x = 1
while ( x<=10 )
'set x 'x
'set z 'z
     n = 1
     while ( n<=5 )
     'tcor p 'n' 'DESC
     n = n + 1
     endwhile
'c'
x = x + 1
endwhile

z = 1
while ( z<=10 )
x = 1
while ( x<=10 )
'set x 'x
'set z 'z
     n = 1
     while ( n<=5 )
     'tcor h 'n' 'DESC
     n = n + 1
     endwhile
'c'
     n = 1
     while ( n<=5 )
     'tcor u 'n' 'DESC
     n = n + 1
     endwhile
'c'
     n = 1
     while ( n<=5 )
     'tcor v 'n' 'DESC
     n = n + 1
     endwhile
'c'
     n = 1
     while ( n<=5 )
     'tcor t 'n' 'DESC
     n = n + 1
     endwhile
'c'
     n = 1
     while ( n<=5 )
     'tcor q 'n' 'DESC
     n = n + 1
     endwhile
'c'
x = x + 1
endwhile
z = z + 1
endwhile

return
