function pltsys (args)

****************************************************************
*****                                                      *****
*****  This script is the driver for calling: pltsys       *****
*****  pltsys computes the ensemble mean systematic error  *****
*****  for forecasts defined in the stats.rc file.         *****
*****                                                      *****
*****  Optional Arguments: -exp -field -level -rc          *****
*****  Examples:                                           *****
*****         1) pltsys.gs                                 *****
*****         2) pltsys.gs -exp 0                          *****
*****         3) pltsys.gs -field v -level 500             *****
*****         4) pltsys.gs -exp 1 -field h                 *****
*****         5) pltsys.gs -exp 1 -field h -rc rcfile      *****
*****                                                      *****
*****  Notes:                                              *****
*****  --------------------------------------------------  *****
*****  Defaults: plot all fields for all experiments       *****
*****       exp: 0 1 2 3 ... n                             *****
*****     field: p u v t q h chi psi                       *****
*****     level: 1000 850 700 500 400 300 250 200 150 100  *****
*****    rcfile: stats.rc                                  *****
*****                                                      *****
****************************************************************

'run getenv GEOSUTIL'
        geosutil = result
'run getenv SOURCE'
        SOURCE   = result

if( SOURCE = "NULL" )
   'run getenv "PWD"'
    SOURCE = result
   'run setenv "SOURCE" 'SOURCE
endif

****************************************************************
****************************************************************

'numargs  'args
 numargs = result

rcfile = 'stats.rc'
field  = 'NULL'
level0 = 'NULL'
target = ''

       num = 0
while( num < numargs )
       num = num + 1
if( subwrd(args,num)='-exp'   ) ; target = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-field' ) ; field  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-level' ) ; level0 = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-rc'    ) ; rcfile = subwrd(args,num+1) ; endif
endwhile

*******************************************************
****             Read Stats Resource File          ****
*******************************************************

n = 0

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
     ntot = n-1
        n = -999
 endif
endwhile

if( target = '' )
    k    = 0
    ktot = ntot
else
    k    = target
    ktot = target
endif

while( k <= ktot )

*******************************************************
****                Open kth-Experiment            ****
*******************************************************

'run setenv "ANTIALIAS" TRUE'

'reinit'
'set display color white'
'rgbset'
'c'

'!'geosutil'/plots/grads_util/make_globl_ctl1 'exp.k
'pltsys_open 'desc.k
        numfiles = result

* Remove possible BLANKS from Experiment Description
* --------------------------------------------------
DESC = ''
length = getlength(desc.k)
i = 1
while( i<=length )
  bit = substr(desc.k,i,1)
  if( bit != ' ' ) 
      if( DESC = '' ) 
          DESC = bit
      else
          DESC = DESC''bit
      endif
  endif
i = i+1
endwhile
'!/bin/mkdir -p 'SOURCE'/'DESC

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

'run getvarz 'n
    var  = result
   name  = subwrd(result,1)
   levs  = subwrd(result,2)
   len   = strlen(name)
   root  = substr(name,1,len-1)
 suffix  = substr(name,len,len)
 if( ( field  = 'NULL' )  | ( field = root ) | ( field = chi & root = u ) | ( field = psi & root = u ) )
     if( suffix = 'f' )
         if( field = chi )
             root  = chi
         endif
         if( field = psi )
             root  = psi
         endif
              m =  m + 1
        field.m = root
         levs.m = levs
     endif
 endif
n = n + 1
endwhile
numflds = m

*******************************************************
****         Begin Systematic Error Plots ...      ****
*******************************************************

'set datawarn off'
'set t 1'
'getinfo month'
         month = result

n = 1
while ( n<=numflds )
              field = field.n
               name = field
'set dfile 1'
'setlons'
'sety'

  if( field = p   ) ; name = slp  ; endif
  if( field = h   ) ; name = hght ; endif
  if( field = u   ) ; name = uwnd ; endif
  if( field = v   ) ; name = vwnd ; endif
  if( field = t   ) ; name = tmpu ; endif
  if( field = q   ) ; name = sphu ; endif
  if( field = chi ) ; name = chi  ; endif
  if( field = psi ) ; name = psi  ; endif

if( level0 = 'NULL' )
        numlevs = levs.n
    if( numlevs > 1 )
        'set z 1'
        'getinfo level'
        levels = result
        z = 2
        while( z<=numlevs )
           'set z 'z
           'getinfo level'
                    level  = result
                    levels = levels' 'level
        z = z + 1
        endwhile
        'setz'
    else
        numlevs = 1
        levels = '1000'
       'set z 1'
    endif
else
        numlevs = 1
        levels = level0
       'set lev 'level0
endif
say 'NUMLEVS = 'numlevs
say '   LEVS = 'levels

'statmak 'field


* Horizonal Plots
* ---------------
z = 1
while ( z<=numlevs )
              level = subwrd(levels,z)
'set dfile 1'
'set lev 'level
say 'Z: 'z
say 'LEVEL: 'level
'c'
'movie statplt "'field' -desc 'DESC' -nfcst 'numfiles'" -print -rotate 90 -name 'SOURCE'/'DESC'/stats_'name'_all_GLO_'level'_'month
'c'
z = z + 1
'!sleep 15 ; convert -loop 0 -delay 30 'SOURCE'/'DESC'/stats_'name'_all_GLO_'level'_'month'.*.gif 'SOURCE'/'DESC'/stats_'name'_all_GLO_'level'_'month'.gif &'
endwhile

* Zonal Mean Plots
* ----------------
if( numlevs > 1 )
'set dfile 1'
'set x 1'
'sety'
'setz'
'c'
'movie statpltz "'field' -desc 'DESC' -nfcst 'numfiles'" -print -rotate 90 -name 'SOURCE'/'DESC'/stats_'name'_all_GLO_z_'month
'c'
'!sleep 15 ; convert -loop 0 -delay 30 'SOURCE'/'DESC'/stats_'name'_all_GLO_z_'month'.*.gif 'SOURCE'/'DESC'/stats_'name'_all_GLO_z_'month'.gif &'
endif

n = n + 1
endwhile

************** 

k = k+1
endwhile

return

function getlength (string)
tb = "" 
i = 1 
while (i<=80)
blank = substr(string,i,1)
if( blank = tb )
length = i-1
i = 81
else
i = i + 1
endif
endwhile
return length
