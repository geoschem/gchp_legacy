function gencmp (args)

*******************************************************
****                 INPUT Variables               ****
*******************************************************

'numargs  'args
 numargs = result

NAME  = NULL
DEBUG = FALSE
LEVEL = 0

        n   = 0
        num = 0
while ( num < numargs )
        num = num + 1

if( subwrd(args,num) = '-EXPID'  ) ; EXPID  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-OUTPUT' ) ; OUTPUT = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-DEBUG'  ) ; DEBUG  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-NAME'   ) ; NAME   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-LEVEL'  ) ; LEVEL  = subwrd(args,num+1) ; endif

* Read EXPORTS
* ------------
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
* -----------
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

*******************************************************

* Construct GCs from Input EXPORTS
* --------------------------------
        m  = 0
        k  = 1
while ( k <= n )
        EX = ''
         j = 1
       bit = substr(EXPORT.k,j,1)
       while(bit != ':' & bit != '')
        EX = EX''bit
         j = j + 1
       bit = substr(EXPORT.k,j,1)
       endwhile
       if( EX != EXPORT.k )
         m = m + 1
         j = j + 1
       GC.m = ''
       bit = substr(EXPORT.k,j,1)
       while(bit != '')
       GC.m = GC.m''bit
         j = j + 1
       bit = substr(EXPORT.k,j,1)
       endwhile
       EXPORT.k = EX
       endif
k = k + 1
endwhile

* Initialize
* ----------
'reinit'
'set display color white'
'set csmooth on'
'c'

'uppercase 'seasons
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

* Get Model Variables
* -------------------
        n  = 1
while ( n <= nexp )
'run getvar 'EXPORT.n' 'GC.n
        qname.n = subwrd(result,1)
        qfile.n = subwrd(result,2)
       qscale.n = subwrd(result,3)
       expdsc.n = subwrd(result,4)
    if( qfile.n = 'NULL' ) ; return ; endif
         n  = n + 1
endwhile


* Get Environment Variables
* -------------------------
'run getenv "GEOSUTIL"'
         geosutil = result

'run getenv "VERIFICATION"'
         verification = result

'run getenv "ANALYSIS"'
         analysis  = result

* Model Experiment Data
* ---------------------
'set dfile 'qfile.1
if( LEVEL = 0 )
   'set z 1'
else
   'set lev 'LEVEL
endif
'getinfo  level'
          modlev = result

'getinfo xdim'
         xdim  = result
'getinfo ydim'
         ydim  = result
'getinfo undef'
         undef = result

'setlons'
'getinfo lonmin'
         lonmin = result
'getinfo lonmax'
         lonmax = result
'setlats'

* Create Environment Variables for Seasonal Utility
* -------------------------------------------------
'setdates'
'run getenv "BEGDATE"'
             begdate  = result
'run getenv "ENDDATE"'
             enddate  = result
'sett'

* Ensure NAME has no underscores
* ------------------------------
        m=1
while ( m<nexp+1 )
'fixname 'qname.m
          alias.m = result
     say 'Alias #'m' = 'alias.m
      if( qname.m != alias.m )
         'set lon -180 360'
         'rename 'qname.m ' 'alias.m
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
   'seasonalf -FUNCTION 'alias.1'*'qscale.1' -NAME 'mod
    climfile = result
else
    mstring = mod
    m  = 1
    while ( m <= nexp )
           mstring = mstring' 'alias.m'.'qfile.m'*'qscale.m
           m  = m + 1
    endwhile
   'run 'geosutil'/plots/formulas/'NAME'.gs 'mstring
    climfile = result
    EXPORT = NAME
        GC = GC.1
endif


* Loop over Possible Experiment Datasets for Comparison
* -----------------------------------------------------
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
        n  = 1
while ( n <= nexp )
'run getvar 'EXPORT.n' 'GC.n' 'exp
        oname.n = subwrd(result,1)
      obsfile.n = subwrd(result,2)
       oscale.n = subwrd(result,3)
       obsdsc.n = subwrd(result,4)
       obsnam.n = subwrd(result,5)
         n  = n + 1
endwhile

* Skip if EXPORT not found
* ------------------------
if( obsfile.1 != "NULL" )

           'set dfile 'obsfile.1
            if( LEVEL = 0 )
               'set z 1'
            else
               'set lev 'LEVEL
            endif
           'getinfo  level'
                     obslev = result
           'getdates'
            begdateo = subwrd(result,1)
            enddateo = subwrd(result,2)

           'run setenv   "BEGDATEO" 'begdateo
           'run setenv   "ENDDATEO" 'enddateo

* Ensure NAME has no underscores
* ------------------------------
        m=1
while ( m<nexp+1 )
'fixname 'oname.m
          olias.m = result
      if( oname.m != olias.m )
         'set lon -180 360'
         'rename 'oname.m ' 'olias.m
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
   'seasonalf -FUNCTION 'olias.1'*'oscale.1' -NAME 'obs
    climfile = result
else
    mstring = obs
    m  = 1
    while ( m <= nexp )
           mstring = mstring' 'olias.m'.'obsfile.m'*'oscale.m
           m  = m + 1
    endwhile
   'run 'geosutil'/plots/formulas/'NAME'.gs 'mstring
    climfile = result
    EXPORT = NAME
        GC = GC.1
endif

               'run getenv "CLIMATE"'
                        climate = result
                        anafile = obsfile.1
                        anadsc  = obsdsc.1
                        ananam  = obsnam.1

                 k = 1
          while( k > 0 )
              season = subwrd(seasons,k)
          if( season = '' )
                   k = -1
          else
                   k = k+1

                  'set dfile 'qfile.1
                  'count "'season'" 'begdate' 'enddate
                   nmod = result
                  'set dfile 'anafile
                  'count "'season'"'
                   nobs = result

                 'define obs'season' = obs'season
                 'run setenv "CLIMATE" 'climate

                       flag = ""
               while ( flag = "" )
              'run genplt.gs 'EXPID' 'EXPORT' 'GC' 'season' 'OUTPUT' 'LEVEL' 'nmod' 'nobs' 'qfile.1' 'anafile' 'ananam' 'anadsc' 'DEBUG' 'expdsc.1
                if( DEBUG = "debug" )
                    say "Hit  ENTER  to repeat plot"
                    say "Type 'next' for  next plot, 'done' for next field"
                    pull flag
                else
                    flag = "next"
                endif
               endwhile
              'c'
          endif
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

