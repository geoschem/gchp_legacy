function corcmp (args)

****************************************************************
*****                                                      *****
*****  This script is the driver for calling: corcmp       *****
*****  corcmp compares multiple experiments using          *****
*****  paired T-tests.                                     *****
*****                                                      *****
*****  Optional Arguments: -field, -lev, -x, -rc           *****
*****  Examples:                                           *****
*****         1) corcmp_plot.gs                            *****
*****         2) corcmp_plot.gs -x 4 -desc A_GCM_Test      *****
*****         3) corcmp_plot.gs -lev 200                   *****
*****         4) corcmp_plot.gs -field u                   *****
*****         5) corcmp_plot.gs -x 2 -field h -lev 500     *****
*****         6) corcmp_plot.gs -x 2 -field h -rc rcfile   *****
*****                                                      *****
*****  List of Experiments                                 *****
*****  -------------------                                 *****
*****  exp.n:  Experiment Locations containing Stats       ***** 
*****  desc.n: Experiment Descriptions                     *****
*****  Note:   Control Experiment is associated with n=0   *****
*****                                                      *****
****************************************************************

'numargs  'args
 numargs = result

rcfile = "stats.rc"
desc   = ''
debug  = TRUE
field  = h
  lev  = 500
  rms  = 0
    x  = 2

       num = 0
while( num < numargs )
       num = num + 1
if( subwrd(args,num)='-field'  ) ; field  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-x'      ) ; x      = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-lev'    ) ; lev    = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-rms'    ) ; rms    = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-level'  ) ; lev    = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-desc'   ) ; desc   = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-rc'     ) ; rcfile = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-debug'  ) ; debug  = subwrd(args,num+1) ; endif

endwhile

****************************************************************
****************************************************************

n = 0

'run getenv SOURCE'
        SOURCE = result

if( SOURCE = "NULL" )
   'run getenv "PWD"'
    SOURCE = result
   'run setenv "SOURCE" 'SOURCE
endif

if( desc = '' )
'getresource 'rcfile' DESC' ;   desc = result
if( desc = "NULL" ) ; desc = '' ; endif
endif

'getresource 'rcfile' EXP'n ;  exp.n = result
'getresource 'rcfile' DSC'n ; desc.n = result
 if( exp.n != NULL | desc.n  != NULL )
     n = n+1
 else
     say 'You must supply a CONTROL and COMPARISON experiment list'
     say 'in a stats rc file using the -rc option.'
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

************************************************************
*****                                                  *****
*****           Open Experiment Datasets               *****
*****                                                  *****
************************************************************

'getinfo numfiles'
         numfiles = result

if( numfiles = "NULL" )
'reinit'
'set display color white'
'c'

* Open Control Experiment
* -----------------------
    args  = ''
        n = 0
while(  n <= ntot-1 )
     args  = args' 'exp.n' 'desc.n
         n = n + 1
endwhile

      'corcmp_open.gs 'args

endif

************************************************************
*****                                                  *****
*****    Check to see if ALL Experiments contain LEV   *****
*****                                                  *****
************************************************************

'getinfo numfiles'
files_per_month = result/ntot
num = 0
while( num <= ntot-1 )
     dfile = 1 + num*files_per_month
'set dfile 'dfile
say 'Check Level: 'lev' for EXP'num' ...'
'set lev 'lev
'getinfo level'
         level = result
if( level != lev )
    return
endif
num = num + 1
endwhile

************************************************************
*****                                                  *****
*****    Create Description String and Call: corcmp    *****
*****                                                  *****
************************************************************

       num = 0
while( num <= ntot )
       dsc = dsc' -desc'num' 'desc.num
       num = num + 1
endwhile

'set lev 'lev
'set x   'x

if( rms = 0 )
   'corcmp_plot.gs -field 'field' -numexp 'ntot' 'dsc' -desc 'desc'            -debug 'debug
   'rmscmp_plot.gs -field 'field' -numexp 'ntot' 'dsc' -desc 'desc' -rms 'rms' -debug 'debug
else
   'rmscmp_plot.gs -field 'field' -numexp 'ntot' 'dsc' -desc 'desc' -rms 'rms' -debug 'debug
endif

return
