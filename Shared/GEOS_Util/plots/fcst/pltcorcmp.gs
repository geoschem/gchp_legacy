function pltcorcmp (args)

*******************************************************
****                 INPUT Variables               ****
*******************************************************

'numargs  'args
 numargs = result

SOURCE = NULL
DESC   = ''
rms    = 0
rcfile = 'stats.rc'
fields = 'p u v t q h'

        num = 0
while ( num < numargs )
        num = num + 1

if( subwrd(args,num) = '-source' ) ; SOURCE = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-desc'   ) ; DESC   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-rc'     ) ; rcfile = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-rms'    ) ; rms    = subwrd(args,num+1) ; endif

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
   'getresource 'rcfile' DESC'
                         DESC = result
   if( DESC = "NULL" ) ; DESC = 'Forecast_Statisitcs' ; endif
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

'getresource 'rcfile' EXP'n ;  exp.n = result
'getresource 'rcfile' DSC'n ; desc.n = result
 if( exp.n != NULL | desc.n  != NULL )
     n = n+1
 else
     say 'You must supply a CONTROL and COMPARISON experiment list'
     say 'in the file:  stats.rc'
     return
 endif

while( n >= 0 )
'getresource 'rcfile' EXP'n ;  exp.n = result
'getresource 'rcfile' DSC'n ; desc.n = result
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
'getinfo zdim'
         zdim  = result
m=0
n=1
while(n<=nvars)

  string = 'cor'
 lstring = strlen(string)
'run getvarz 'n
    var  = result
   name  = subwrd(result,1)
   levs  = subwrd(result,2)
if(levs  = 0 ) ; levs = 1 ; endif
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

z = 1
while( z<=zdim )
'set z 'z
'getinfo level'
         level.z = result
z = z + 1
endwhile

'reinit'

*******************************************************
****    Make Plots Hardwired for Web Page Levels   ****
*******************************************************

n = 1
while ( n<=numflds )
              field = field.n
            numlevs = levs.n

       j = 1
while( j<=numfields )
if( field = subwrd(fields,j) )
    z = 1
    while ( z<=numlevs )
    if( level.z = 100 | level.z = 150 | level.z = 200 | level.z = 250 | level.z = 300 | level.z = 400 | level.z = 500 | level.z = 600 | level.z = 700 | level.z = 750 | level.z = 800 | level.z = 850 | level.z = 900 | level.z = 925 | level.z = 950 | level.z = 975 | level.z = 1000 )
      x = 1
      while ( x<=10 )
      'run 'geosutil'/plots/grads_util/corcmp -x 'x' -lev 'level.z' -field 'field' -rc 'rcfile' -rms 'rms' -desc 'DESC' -debug FALSE'
      'c'
      x = x + 1
      endwhile
    endif
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
