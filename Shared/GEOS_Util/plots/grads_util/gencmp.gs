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
'c'

'uppercase 'seasons
            seasons = result

* Set number of EXPORTS & GCs
* ---------------------------
if( n = m )
    numGCs = n
else
    say 'Number of EXPORTS does not equal number of GCs!'
    say 'Number of EXPORTS: 'n
    say '              GCS: 'm
    return
endif
say ' '
n = 1
while( n<=numGCs )
say 'n = 'n'  EXPORT: 'EXPORT.n'   GC: 'GC.n'  Optional: 'OPT.n
n = n + 1
endwhile


* Get Model Variables
* -------------------
      mexp = 0
        n  = 1
while ( n <= numGCs )
'run getvar 'EXPORT.n' 'GC.n
        qname.n = subwrd(result,1)
        qfile.n = subwrd(result,2)
       qscale.n = subwrd(result,3)
        qdesc.n = subwrd(result,4)
         qtag.n = subwrd(result,5)
    if( qfile.n != 'NULL' )
            mexp = mexp + 1
    else
      if( OPT.n = 'FALSE' )
          return
      endif
    endif
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
while ( m<mexp+1 )
'fixname 'qname.m
          alias.m = result
     say 'Alias #'m' = 'alias.m
      if( qname.m != alias.m )
         'set lon -180 360'
         'rename 'qname.m ' 'alias.m''qfile.m
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

if( numGCs = 1 )
      NAME = EXPORT.1
        GC =     GC.1
    EXPORT = EXPORT.1
    if( qname.1 != alias.1 )
       'seasonalf -FUNCTION 'alias.1''qfile.1'*'qscale.1' -NAME 'mod
    else
       'seasonalf -FUNCTION 'alias.1'.'qfile.1'*'qscale.1' -NAME 'mod
    endif
    climfile = result
else
    mstring = mod
    m  = 1
    while ( m <= mexp )
       if( qname.m != alias.m )
           mstring = mstring' 'alias.m''qfile.m'*'qscale.m
       else
           mstring = mstring' 'alias.m'.'qfile.m'*'qscale.m
       endif
           m  = m + 1
    endwhile
   'run 'geosutil'/plots/formulas/'NAME'.gs 'mstring
    climfile = result
    EXPORT = NAME
        GC = GC.1
endif


***********************************************************************************
*              Loop over Possible Experiment Datasets for Comparison
***********************************************************************************

'!/bin/mv HISTORY.T HISTORY.Tmp'
'run getenv "CMPEXP"'
         cmpexp = result
         numexp = 1

          dummy = get_cmpexp (cmpexp,numexp)
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
        m  = 1
while ( m <= numGCs )
'run getvar 'EXPORT.m' 'GC.m' 'exp
        oname.numexp.m = subwrd(result,1)
      obsfile.numexp.m = subwrd(result,2)
       oscale.numexp.m = subwrd(result,3)
       obsdsc.numexp.m = subwrd(result,4)
       obsnam.numexp.m = subwrd(result,5)
    if( obsfile.numexp.m != 'NULL' )
            oexp = oexp + 1
    else
      if( OPT.numexp.m = 'FALSE' )
                 found =  FALSE
      endif
    endif
         m  = m + 1
endwhile

* Continue if all EXPORT(s) are found
* -----------------------------------
if( found = "TRUE" )

           'set dfile 'obsfile.numexp.1
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
while ( m<oexp+1 )
'fixname 'oname.numexp.m
          olias.numexp.m = result
     say 'Olias #'m' = 'olias.numexp.m
      if( oname.numexp.m != olias.numexp.m )
         'set lon -180 360'
         'rename 'oname.numexp.m ' 'olias.numexp.m''obsfile.numexp.m
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

if( numGCs = 1 )
      NAME = EXPORT.1
        GC =     GC.1
    EXPORT = EXPORT.1
    if( oname.numexp.1 != olias.numexp.1 )
       'seasonalf -FUNCTION 'olias.numexp.1''obsfile.numexp.1'*'oscale.numexp.1'  -NAME obs'numexp
    else
       'seasonalf -FUNCTION 'olias.numexp.1'.'obsfile.numexp.1'*'oscale.numexp.1' -NAME obs'numexp
    endif
    climfile = result
else
    mstring = obs''numexp
    m  = 1
    while ( m <= oexp )
       if( oname.numexp.m != olias.numexp.m )
           mstring = mstring' 'olias.numexp.m''obsfile.numexp.m'*'oscale.numexp.m
       else
           mstring = mstring' 'olias.numexp.m'.'obsfile.numexp.m'*'oscale.numexp.m
       endif
           m  = m + 1
    endwhile
   'run 'geosutil'/plots/formulas/'NAME'.gs 'mstring
    climfile = result
    EXPORT = NAME
        GC = GC.1
endif

               'run getenv "CLIMATE"'
                        climate = result
                        anafile = obsfile.numexp.1
                        anadsc  =  obsdsc.numexp.1
                        ananam  =  obsnam.numexp.1

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
                  'count "'season'" 'begdateo' 'enddateo
                   nobs.numexp = result

                 'define obs'season' = obs'numexp''season
                 'run setenv "CLIMATE" 'climate

                       flag = ""
               while ( flag = "" )
              'run genplt.gs 'EXPID' 'EXPORT' 'GC' 'season' 'OUTPUT' 'LEVEL' 'nmod' 'nobs.numexp' 'qfile.1' 'anafile' 'ananam' 'anadsc' 'DEBUG' 'qdesc.1
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

 numexp = numexp + 1
  dummy = get_cmpexp (cmpexp,numexp)
    exp = subwrd(dummy,1)
   type = subwrd(dummy,2)

endwhile
 numexp = numexp - 1

'!/bin/mv HISTORY.Tmp HISTORY.T'

* ---------------------------------------------------------
* Now that we have computed plots for each experiment,
* we can compute the Closeness plots to MERRA-2
* ---------------------------------------------------------

* Find MERRA2 experiment
* ----------------------
  MERRA2  = 0
       n  = 1
while( n <= numexp )
say "obsnam.numexp.1 = "n"  "obsnam.numexp.1
if( obsnam.numexp.1 = "MERRA-2" )
    MERRA2 = n
endif
         n = n + 1
endwhile
say "MERRA2 = "MERRA2

if( MERRA2 != 0 )

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

'set dfile 'qfile.1
'set gxout shaded'
'rgbset'
'run setenv "LEVTYPE" 'DLEVS

* Closeness Plot (Experiment_vs_Comparison to MERRA-2)
* ----------------------------------------------------
       n  = 1
while( n <= numexp )
if( obsnam.n.1 != "NULL" & obsnam.n.1 != "merra" & obsnam.n.1 != "MERRA-2" )
say 'Closeness plot between  exp: 'qtag.1
say '                       cexp: 'obsnam.n.1
say '                        obs: 'obsnam.MERRA2.1
say ''

'define zobs'MERRA2''season' = regrid2( obs'MERRA2''season',0.25,0.25,bs_p1,0,-90 )'
'define zobs'n''season'      = regrid2( obs'n''season'     ,0.25,0.25,bs_p1,0,-90 )'
'define zmod'season'         = regrid2( mod'season'        ,0.25,0.25,bs_p1,0,-90 )'

        flag = ""
while ( flag = "" )

'closeness -CVAR 'zobs''n' -MVAR 'zmod' -OVAR 'zobs''MERRA2' -CNAME 'obsnam.n.1' -MNAME 'NAME' -ONAME 'obsnam.MERRA2.1' -CDESC 'obsdsc.n.1' -MDESC 'qdesc.1' -ODESC 'obsdsc.MERRA2.1' -MFILE 'qfile.1' -MBEGDATE 'begdate' -MENDDATE 'enddate' -OFILE 'obsfile.MERRA2.1' -OBEGDATE 'begdateo' -OENDDATE 'enddateo' -EXPID 'EXPID' -PREFIX 'NULL' -SEASON 'season' -OUTPUT 'OUTPUT' -CLIMATE 'climate' -GC 'GC.1' -MATH 'NULL' -LEVEL 'LEVEL

'myprint -name 'OUTPUT'/hdiag_'obsnam.n.1'_'NAME'.'GC.1'_'LEVEL'_closeness_'obsnam.MERRA2.1'.'season


 if( DEBUG = "debug" )
     say "Hit ENTER to repeat plot, or NON-BLANK to continue"
     pull flag
 else
     flag = "next"
 endif
'c'
endwhile ;* END While_FLAG Loop
endif
       n  = n + 1
endwhile ;* END While_N Loop

* End Season Test
* ---------------
endif
* ---------------
endwhile ;* END While_m>0 Loop

endif ;* END MERRA-2 Test

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

