function pltcorcmp (args)

*******************************************************
****                 INPUT Variables               ****
*******************************************************

'numargs  'args
 numargs = result

SOURCE = NULL
DESC   = ''
rcfile = 'stats.rc'
fields = 'p u v t q h'

        num = 0
while ( num < numargs )
        num = num + 1

if( subwrd(args,num) = '-source' ) ; SOURCE = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-desc'   ) ; DESC   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-rc'     ) ; rcfile = subwrd(args,num+1) ; endif

if( subwrd(args,num) = '-fields' )
     fields = ''
           k = 1
    while( k > 0 )
           L = num + k
        field  = subwrd(args,L)
    if( field  = '' )
        k = -1
    else
        bit = substr(field,1,1)
        if( bit = '-' )
              k = -1
        else
              fields = fields % ' ' % field
              k = k+1
        endif
    endif
    endwhile
endif

endwhile
'numargs 'fields
 numfields = result

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

if( DESC = '' )
'getresource 'SOURCE'/'rcfile' DESC' ;   DESC = result
if( DESC = "NULL" ) ; DESC = '' ; endif
endif

*******************************************************
****            Open Control Experiment            ****
*******************************************************

n = 0

'run getenv SOURCE'
        SOURCE = result

if( SOURCE = "NULL" )
   'run getenv "PWD"'
    SOURCE = result
   'run setenv "SOURCE" 'SOURCE
endif

'getresource 'SOURCE'/'rcfile' EXP'n ;  exp.n = result
'getresource 'SOURCE'/'rcfile' DSC'n ; desc.n = result
 if( exp.n != NULL | desc.n  != NULL )
     n = n+1
 else
     say 'You must supply a CONTROL and COMPARISON experiment list'
     say 'in the file:  stats.rc'
     return
 endif

while( n >= 0 )
'getresource 'SOURCE'/'rcfile' EXP'n ;  exp.n = result
'getresource 'SOURCE'/'rcfile' DSC'n ; desc.n = result
 if( exp.n != NULL | desc.n  != NULL )
     n = n+1
 else
     ntot = n
        n = -999
 endif
endwhile

'corcmp_open.gs 'exp.0' 'desc.0

*******************************************************
****            Determine Fields in File           ****
*******************************************************

'set dfile 1'
'q ctlinfo'
say 'ctlinfo 'result
'getinfo nvars'
         nvars = result
m=0
n=1
while(n<=nvars)

  string = 'cor'
 lstring = strlen(string)
'run getvarz 'n
    var  = result
   name  = subwrd(result,1)
   levs  = subwrd(result,2)
   len   = strlen(name)
   root  = substr(name,1,len-lstring)
 suffix  = substr(name,len-lstring+1,len)
 if( suffix = string )
          m =  m + 1
    field.m = root
     levs.m = levs
 endif
n = n + 1
endwhile
numflds = m

'reinit'

*******************************************************
****                    Begin ...                  ****
*******************************************************

level.1  = 1000
level.2  =  850
level.3  =  700
level.4  =  500
level.5  =  400
level.6  =  300
level.7  =  250
level.8  =  200
level.9  =  150
level.10 =  100

n = 1
while ( n<=numflds )
              field = field.n
               name = field
            numlevs = levs.n

                    zmax = 1
if( numlevs > 1 ) ; zmax = 10 ; endif

       j = 1
while( j<=numfields )
if( field = subwrd(fields,j) )
    z = 1
    while ( z<=zmax )
    x = 1
    while ( x<=10 )
    'run 'geosutil'/plots/grads_util/corcmp -x 'x' -lev 'level.z' -field 'field' -rc 'rcfile' -desc 'DESC' -debug FALSE'
    'c'
    x = x + 1
    endwhile
    z = z + 1
    endwhile
    j = numfields + 1
else
    j = j + 1
endif
endwhile

n = n + 1
endwhile

return
