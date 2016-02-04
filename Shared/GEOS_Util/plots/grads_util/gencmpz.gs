function gencmpz (args)

*******************************************************
****                 INPUT Variables               ****
*******************************************************

'numargs  'args
 numargs = result

NAME   = NULL
DEBUG  = FALSE
ZLOG   = NULL
PTOP   = NULL

        n   = 0
        num = 0
while ( num < numargs )
        num = num + 1

if( subwrd(args,num) = '-EXPID'  ) ; EXPID  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-OUTPUT' ) ; OUTPUT = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-DEBUG'  ) ; DEBUG  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-ZLOG'   ) ; ZLOG   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-PTOP'   ) ; PTOP   = subwrd(args,num+1) ; endif

* Read EXPORTS
* ------------
if( subwrd(args,num) = '-EXPORT' )
              n = n + 1
         EXPORT = subwrd(args,num+n   )
           word = subwrd(args,num+n+1 )
            bit = checkbit(word)
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

* Construct GC from Input EXPORT
* ------------------------------
        EX = ''
         j = 1
       bit = substr(EXPORT,j,1)
       while(bit != ':' & bit != '')
        EX = EX''bit
         j = j + 1
       bit = substr(EXPORT,j,1)
       endwhile
       if( EX != EXPORT )
         j = j + 1
        GC = ''
       bit = substr(EXPORT,j,1)
       while(bit != '')
        GC = GC''bit
         j = j + 1
       bit = substr(EXPORT,j,1)
       endwhile
       EXPORT = EX
       endif

'run uppercase 'seasons
                seasons = result


* Initialize
* ----------
'reinit'
'set display color white'
'set csmooth on'
'set clab off'
'c'
'rgbset'


* Determine Variable Name and Location
* ------------------------------------
'run getvar 'EXPORT' 'GC
     mname  = subwrd(result,1)
     mfile  = subwrd(result,2)
     scale  = subwrd(result,3)
     expdsc = subwrd(result,4)

if( mfile = "NULL" ) ; return ; endif

if( PTOP = NULL )
    'getinfo zdim'
             zdim = result
    'set z ' zdim
    'getinfo level'
             PTOP = result
    'setz'
     say 'Setting PTOP = 'PTOP
endif

'run getenv "GEOSUTIL"'
         geosutil = result
                                                                                                   
'run getenv "VERIFICATION"'
         verification = result

'run getenv "ANALYSIS"'
         analysis = result


* Model Experiment Data
* ---------------------
'set dfile 'mfile
'setdates'
'run getenv "BEGDATE"'
             begdate  = result
'run getenv "ENDDATE"'
             enddate  = result
'setlons'
'sety'
'setz'
'sett'

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

                     ' alias ' mname
                      malias = result
'chckname            'malias
'seasonalf -FUNCTION 'malias'*'scale' -NAME mod0'
 modfile  = subwrd(result,1)
 
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

'run getvar 'EXPORT' 'GC' 'exp
           oname = subwrd(result,1)
           ofile = subwrd(result,2)
          oscale = subwrd(result,3)
          obsdsc = subwrd(result,4)
          obsnam = subwrd(result,5)

* Skip if EXPORT not found
* ------------------------
if( ofile != "NULL" )

'set dfile 'ofile
    'getdates'
     begdateo = subwrd(result,1)
     enddateo = subwrd(result,2)

'set lon 'lonmin' 'lonmax
'set lat 'latmin' 'latmax
'setz'

                     ' alias ' oname
                      oalias = result
'chckname            'oalias
'seasonalf -FUNCTION 'oalias'*'oscale' -NAME exp'num
 expfile  = subwrd(result,1)
 

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

'set dfile 'mfile
'count "'season'" 'begdate' 'enddate
 nmod =  result

'set dfile 'ofile
'count "'season'"'
 nobs =  result


* Set EXPORT Model and Observation Scaling Factors
* ------------------------------------------------
facm = 1
faco = facm

* Create Temporary File at 1x1 degree resolution with consistent levels
* ---------------------------------------------------------------------
'set dfile 'mfile
'set lat -90 90'
'setlons'

'set dfile 'modfile
'setz'
'set t 1'
'define mod = mod0'season'*'facm
'makez  mod z'

'set dfile 'expfile
'setz'
'set t 1'
'define obs = exp'num''season'*'faco
'makez  obs z'

'set dfile 'mfile
'set t 1'
'makezdif -q1 mod -q2 obs -file1 'modfile' -file2 'expfile' -ptop 'PTOP
qmax = subwrd(result,1)
qmin = subwrd(result,2)


* Make ZPLT
* ---------
                       flag = ""
               while ( flag = "" )

'run genpltz.gs -EXPID 'EXPID' -EXPORT 'EXPORT' -GC 'GC' -ALIAS 'mname' -QFILE 'mfile' -OFILE 'ofile' -ONAME 'obsnam' -OBDATE 'begdateo' -OEDATE 'enddateo' -NMOD 'nmod' -NOBS 'nobs' -QDESC 'expdsc' -ODESC 'obsdsc' -OUTPUT 'OUTPUT' -SEASON 'season' -PTOP 'PTOP' -MAX 'qmax' -MIN 'qmin' -ZLOG 'ZLOG

                if( DEBUG = "debug" )
                    say "Hit  ENTER  to repeat plot"
                    say "Type 'next' for  next plot, 'done' for next field"
                    pull flag
                else
                    flag = "next"
                endif
              'c'
               endwhile

* End Seasonal Test
* -----------------
endif

* End Seasonal Loop
* -----------------
endwhile


* Check next Comparison Experiment Dataset
* ----------------------------------------
endif

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

