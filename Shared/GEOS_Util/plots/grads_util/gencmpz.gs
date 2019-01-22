function gencmpz (args)

*******************************************************
****                 INPUT Variables               ****
*******************************************************

'numargs  'args
 numargs = result

NAME   = NULL
DEBUG  = FALSE
PTOPS  = NULL

        n   = 0
        num = 0
while ( num < numargs )
        num = num + 1

if( subwrd(args,num) = '-EXPID'  ) ; EXPID  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-EXPORT' ) ; EXPORT = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-OUTPUT' ) ; OUTPUT = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-NAME'   ) ; NAME   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-DEBUG'  ) ; DEBUG  = subwrd(args,num+1) ; endif

* Read PTOP
* ---------
if( subwrd(args,num) = '-PTOP' )
     PTOPS = ''
           k = 1
    while( k > 0 )
           L = num + k
        PTOP  = subwrd(args,L)
    if( PTOP  = '' )
        k = -1
    else
        bit = substr(PTOP,1,1)
        if( bit = '-' )
              k = -1
        else
              PTOPS = PTOPS % ' ' % PTOP
              k = k+1
        endif
    endif
    endwhile
endif


* Read EXPORTS with format  EXPORT:GC[:OPT]
* -----------------------------------------
if( subwrd(args,num) = '-EXPORT' )
              n = n + 1
       EXPORT.n = subwrd(args,num+n   )
           word = subwrd(args,num+n+1 )
            bit = checkbit(word)
       while( bit != '-' )
              n = n + 1
       EXPORT.n = subwrd(args,num+n   )
           word = subwrd(args,num+n+1 )
            bit = checkbit(word)
       endwhile
endif


* Read SEASONS
* ------------
if( subwrd(args,num) = '-SEASON' )
     seasons = ''
           k = 1
    while( k > 0 )
           L = num + k
        season  = subwrd(args,L)
    if( season  = '' )
        k = -1
    else
        bit = substr(season,1,1)
        if( bit = '-' )
              k = -1
        else
              seasons = seasons % ' ' % season
              k = k+1
        endif
    endif
    endwhile
endif

endwhile

* Construct GCs from Input EXPORTS, Check for OPTIONAL EXPORTS
* ------------------------------------------------------------
        m  = 0
        k  = 1
while ( k <= n )

        dummy = EXPORT.k
        EXPORT.k = ''
         j = 1
       bit = substr(dummy,j,1)
       while(bit != ':' & bit != '')
        EXPORT.k = EXPORT.k''bit
         j = j + 1
       bit = substr(dummy,j,1)
       endwhile
  
       if( bit != '' )
           m = m + 1
           j = j + 1
         GC.m = ''
         bit = substr(dummy,j,1)
         while(bit != ':' & bit != '')
         GC.m = GC.m''bit
           j = j + 1
         bit = substr(dummy,j,1)
         endwhile
       endif

       if( bit != '' )
           OPT.m = TRUE
       else
           OPT.m = FALSE
       endif

k = k + 1
endwhile

* Initialize
* ----------
'reinit'
'set display color white'
'set csmooth on'
'set clab off'
'c'
'rgbset'

'run uppercase 'seasons
                seasons = result


* Set number of EXPORTS & GCs
* ---------------------------
if( n = m )
    nexp = n
else
    say 'Number of EXPORTS does not equal number of GCs!'
    say 'Number of EXPORTS: 'n
    say '              GCS: 'm
    return
endif
say ' '
n = 1
while( n<=nexp )
say 'n = 'n'  EXPORT: 'EXPORT.n'   GC: 'GC.n'  Optional: 'OPT.n
n = n + 1
endwhile


* Get Model Variables
* -------------------
      mexp = 0
        n  = 1
while ( n <= nexp )
'run getvar 'EXPORT.n' 'GC.n
        mname.n = subwrd(result,1)
        mfile.n = subwrd(result,2)
       mscale.n = subwrd(result,3)
       expdsc.n = subwrd(result,4)
    if( mfile.n != 'NULL' )
            mexp = mexp + 1
    else
      if( OPT.n = 'FALSE' ) 
          return
      endif
    endif
         n  = n + 1
endwhile


* Compute PTOP Info
* ----------------- 
if( PTOPS = NULL )
    'getinfo zdim'
             zdim = result
    'set z ' zdim
    'getinfo level'
             PTOPS = result
    'setz'
     say 'Setting PTOPS = 'PTOPS
endif
'numargs 'PTOPS
         NPTOPS = result
PTOPMIN = 1000
n = 1
while( n<=NPTOPS )
    PTOP = subwrd(PTOPS,n)
     len = get_length (PTOP)
     bit = substr(PTOP,len-1,1)
      if( bit = ":" )
          PTOP = substr(PTOP,1,len-2)
      endif
if( PTOP < PTOPMIN )
           PTOPMIN = PTOP
endif
n = n + 1
endwhile


'run getenv "GEOSUTIL"'
         geosutil = result
                                                                                                   
'run getenv "VERIFICATION"'
         verification = result

'run getenv "ANALYSIS"'
         analysis = result


* Model Experiment Data
* ---------------------
'set dfile 'mfile.1
'setdates'
'run getenv "BEGDATE"'
             begdate  = result
'run getenv "ENDDATE"'
             enddate  = result
'setlons'
'sety'
'sett'

'run getlevs 'mname.1
     numlevs = result
if(  numlevs = 1 )
    'set z 1'
else
    'setz'
endif

* Get Dimension of Model Environment
* ----------------------------------
'getinfo lonmin'
         lonmin = result
'getinfo lonmax'
         lonmax = result
'getinfo latmin'
         latmin = result
'getinfo latmax'
         latmax = result


* Ensure NAME has no underscores
* ------------------------------
        m=1
while ( m<mexp+1 )
'fixname 'mname.m
          alias.m = result
     say 'Alias #'m' = 'alias.m
      if( mname.m != alias.m )
         'set lon -180 360'
         'rename 'mname.m ' 'alias.m''mfile.m
         'setlons'
      endif
      m = m+1
endwhile


* Perform Model Formula Calculation
* ---------------------------------
 say ' '
'q dims'
 say 'Model Environment:'
 say result

if( nexp = 1 )
      NAME = EXPORT.1
        GC =     GC.1
    EXPORT = EXPORT.1
    if( mname.1 != alias.1 )
       'seasonalf -FUNCTION 'alias.1''mfile.1'*'mscale.1' -NAME 'mod0
    else
       'seasonalf -FUNCTION 'alias.1'.'mfile.1'*'mscale.1' -NAME 'mod0
    endif
    modfile = result
else
    mstring = mod0
    m  = 1
    while ( m <= mexp )
       if( mname.m != alias.m )
           mstring = mstring' 'alias.m''mfile.m'*'mscale.m
       else
           mstring = mstring' 'alias.m'.'mfile.m'*'mscale.m
       endif
           m  = m + 1
    endwhile
   'run 'geosutil'/plots/formulas/'NAME'.gs 'mstring
    modfile = result
    EXPORT = NAME
        GC = GC.1
endif

 
*******************************************************************
****   Loop over Possible Experiment Datasets for Comparison   ****
*******************************************************************

'!/bin/mv HISTORY.T HISTORY.Tmp'
'run getenv "CMPEXP"'
         cmpexp = result
            num = 1

          dummy = get_cmpexp (cmpexp,num)
            exp = subwrd(dummy,1)
           type = subwrd(dummy,2)

while( exp != 'NULL' )
say ' '
say 'Comparing with: 'exp

* analysis = false  EXP=M CMP=M  => ALEVS   
* analysis = false  EXP=M CMP=A  => DLEVS   
* analysis = true   EXP=A CMP=A  => ALEVS   
* analysis = true   EXP=A CMP=M  => DLEVS   

if( analysis != "false" )
    if( type = A )
       'run setenv "LEVTYPE" 'ALEVS
    else
       'run setenv "LEVTYPE" 'DLEVS
    endif
else
    if( type = A )
       'run setenv "LEVTYPE" 'DLEVS
    else
       'run setenv "LEVTYPE" 'ALEVS
    endif
endif

'!chckfile 'exp'/.HOMDIR'
 'run getenv CHECKFILE'
         CHECKFILE  = result
     if( CHECKFILE != 'NULL' )
        '!/bin/cp `cat 'exp'/.HOMDIR`/HISTORY.rc .'
     else
        '!/bin/cp 'exp'/HISTORY.rc .'
     endif
'!remove CHECKFILE.txt'

'!cat HISTORY.rc | sed -e "s/,/ , /g" | sed -e "s/*/@/g" > HISTORY.T'

* Get CMPEXP Variables
* --------------------
     found = TRUE
      oexp = 0
        n  = 1
while ( n <= nexp )
'run getvar 'EXPORT.n' 'GC.n' 'exp
        oname.n = subwrd(result,1)
        ofile.n = subwrd(result,2)
       oscale.n = subwrd(result,3)
       obsdsc.n = subwrd(result,4)
       obsnam.n = subwrd(result,5)
    if( ofile.n != 'NULL' )
            oexp = oexp + 1
    else
      if( OPT.n = 'FALSE' ) 
          found =  FALSE
      endif
    endif
         n  = n + 1
endwhile


* Continue if all EXPORT(s) are found
* -----------------------------------
if( found = "TRUE" )

'set dfile 'ofile.1
    'getdates'
     begdateo = subwrd(result,1)
     enddateo = subwrd(result,2)

'set lon 'lonmin' 'lonmax
'set lat 'latmin' 'latmax

if(  numlevs = 1 )
    'set z 1'
else
    'setz'
endif

* Ensure NAME has no underscores
* ------------------------------
        m=1
while ( m<oexp+1 )
'fixname 'oname.m
          olias.m = result
      say 'Olias #'m' = 'olias.m
      if( oname.m != olias.m )
         'set lon -180 360'
         'rename 'oname.m ' 'olias.m''ofile.m
         'setlons'
      endif
      m = m+1
endwhile

* Perform Model Formula Calculation
* ---------------------------------
 say ' '
'q dims'
 say 'CMPEXP Environment:'
 say result

if( nexp = 1 )
      NAME = EXPORT.1
        GC =     GC.1
    EXPORT = EXPORT.1
    if( oname.1 != olias.1 )
       'seasonalf -FUNCTION 'olias.1''ofile.1'*'oscale.1' -NAME exp'num
    else
       'seasonalf -FUNCTION 'olias.1'.'ofile.1'*'oscale.1' -NAME exp'num
    endif
    expfile = result
else
    mstring = 'exp'num
    m  = 1
    while ( m <= oexp )
       if( oname.m != olias.m )
           mstring = mstring' 'olias.m''ofile.m'*'oscale.m
       else
           mstring = mstring' 'olias.m'.'ofile.m'*'oscale.m
       endif
           m  = m + 1
    endwhile
   'run 'geosutil'/plots/formulas/'NAME'.gs 'mstring
    expfile = result
    EXPORT = NAME
        GC = GC.1
endif


* Loop over Seasons to Process
* ----------------------------
                 m = 1
          while( m > 0 )
              season = subwrd(seasons,m)
          if( season = '' )
                   m = -1
          else
                   m = m+1

say 'Processing Season: 'season

'set dfile 'mfile.1
'count "'season'" 'begdate' 'enddate
 nmod =  result

'set dfile 'ofile.1
'count "'season'" 'begdateo' 'enddateo
 nobs =  result


* Set EXPORT Model and Observation Scaling Factors
* ------------------------------------------------
facm = 1
faco = facm

* Create Temporary File at 1x1 degree resolution with consistent levels
* ---------------------------------------------------------------------
'set dfile 'mfile.1
'set lat -90 90'
'setlons'

'set dfile 'modfile
if(  numlevs = 1 )
    'set z 1'
else
    'setz'
endif
'set t 1'
'define mod = mod0'season'*'facm
'makez  mod z'

'set dfile 'expfile
if(  numlevs = 1 )
    'set z 1'
else
    'setz'
endif
'set t 1'
'define obs = exp'num''season'*'faco
'makez  obs z'


* Make ZPLT
* ---------
n = 1
while( n<=NPTOPS )

           PTOP = subwrd(PTOPS,n)
            len = get_length (PTOP)
            bit = substr(PTOP,len-1,1)
             if( bit = ":" )
                 PTOP = substr(PTOP,1,len-2)
                 ZLOG = ON
             else
                if( PTOP >= 10 )
                    ZLOG  = OFF
                else
                    ZLOG  = ON
                endif
             endif

      'set dfile 'mfile.1
      'set t 1'
      'makezdif -q1 mod -q2 obs -file1 'modfile' -file2 'expfile' -ptop 'PTOP
       qmax = subwrd(result,1)
       qmin = subwrd(result,2)

                       flag = ""
               while ( flag = "" )

'run genpltz.gs -EXPID 'EXPID' -EXPORT 'EXPORT' -GC 'GC' -ALIAS 'mname.1' -QFILE 'mfile.1' -OFILE 'ofile.1' -ONAME 'obsnam.1' -OBDATE 'begdateo' -OEDATE 'enddateo' -NMOD 'nmod' -NOBS 'nobs' -QDESC 'expdsc.1' -ODESC 'obsdsc.1' -OUTPUT 'OUTPUT' -SEASON 'season' -PTOP 'PTOP' -MAX 'qmax' -MIN 'qmin' -ZLOG 'ZLOG

                if( DEBUG = "debug" )
                    say "Hit  ENTER  to repeat plot"
                    say "Type 'next' for  next plot, 'done' for next field"
                    pull flag
                else
                    flag = "next"
                endif
              'c'
               endwhile

n = n + 1
endwhile

* End Seasonal Test
* -----------------
endif

* End Seasonal Loop
* -----------------
endwhile

* End Check for "Found" All GETVAR EXPORTS
* ----------------------------------------
endif

* Check next Comparison Experiment Dataset
* ----------------------------------------
*endif

    num = num + 1
  dummy = get_cmpexp (cmpexp,num)
    exp = subwrd(dummy,1)
   type = subwrd(dummy,2)

endwhile
'!/bin/mv HISTORY.Tmp HISTORY.T'

return

function getdate (date,month,year)
       num = 1
       bit = substr(date,num,1)
while( bit != '' )
       num = num+1
       bit = substr(date,num,1)
endwhile
       loc = num-7
     month = substr(date,loc  ,3)
      year = substr(date,loc+3,4)
return month' 'year

* Get Next EXP from CMPEXP List
* -----------------------------
function get_cmpexp (cmpexp,num)
      exp  = subwrd(cmpexp,num)
      len = get_length (exp)
      bit = substr(exp,len-1,1)
      if( bit = ":" )
          type = substr(exp,len,1)
          exp  = substr(exp,1,len-2)
      else
          type = M
      endif
return exp' 'type

function get_length (string)
tb = ""
i = 1
while (i<=256)
blank = substr(string,i,1)
if( blank = tb )
length = i-1
i = 999
else
i = i + 1
endif
endwhile
return length

* To Prevent Problem with BIT: E
* ------------------------------
function checkbit (word)
      bit = substr(word,1,1)
      dum = bit'TEST'
      if( dum = "ETEST" ) ; bit = A ; endif
return bit

