function pltrms (args)

*******************************************************
****                 INPUT Variables               ****
*******************************************************

'numargs  'args
 numargs = result

SOURCE = NULL
FIELD  = NULL
DESC   = Stats

        num = 0
while ( num < numargs )
        num = num + 1

if( subwrd(args,num) = '-source' ) ; SOURCE = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-desc'   ) ; DESC   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-field'  ) ; FIELD  = subwrd(args,num+1) ; endif

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

'set dfile 1'
'getinfo nvars'
         nvars = result
n=1
while(n<=nvars)

'run getvarz 'n
    var  = result
   name  = subwrd(result,1)
   levs  = subwrd(result,2)
   len   = strlen(name)
   root  = substr(name,1,len-3)
 suffix  = substr(name,len-2,len)

if( FIELD = 'NULL' | FIELD = root )

if( suffix = 'rms' )

    zmax = levs
if( zmax = 0 )
    zmax = 1
endif 

z = 1
while ( z<=zmax )
x = 1
while ( x<=10 )
'set x 'x
'set z 'z
'rms 'root' 'DESC
'c'
x = x + 1
endwhile
z = z + 1
endwhile

endif
endif
n = n + 1
endwhile

return
