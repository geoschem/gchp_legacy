function genplot (args)

*******************************************************
****                 INPUT Variables               ****
*******************************************************

'numargs  'args
 numargs = result

PREFIX = NULL
TAYLOR = FALSE
LAND   = FALSE
OCEAN  = FALSE
SCALE  = 1.0
RC     = NULL
LEVEL  = NULL
MATH   = NULL

          m = 0
          n = 0
        num = 0
while ( num < numargs )
        num = num + 1

if( subwrd(args,num) = '-DIR'    ) ; DIR    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-NAME'   ) ; NAME   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-MATH'   ) ; MATH   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-EXPID'  ) ; EXPID  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-OUTPUT' ) ; OUTPUT = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-DEBUG'  ) ; DEBUG  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-SCALE'  ) ; SCALE  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-PREFIX' ) ; PREFIX = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-RC'     ) ; RC     = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-LEVEL'  ) ; LEVEL  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-LAND'   ) ; LAND   = TRUE               ; endif
if( subwrd(args,num) = '-OCEAN'  ) ; OCEAN  = TRUE               ; endif
if( subwrd(args,num) = '-TAYLOR' ) ; TAYLOR = TRUE               ; endif

* Read Model EXPORT:GC
* --------------------
if( subwrd(args,num) = '-EXPORT' )
              m = m + 1
       EXPORT.m = subwrd(args,num+m   )
           word = subwrd(args,num+m+1 )
            bit = checkbit(word)
       while( bit != '-' )
              m = m + 1
       EXPORT.m = subwrd(args,num+m   )
           word = subwrd(args,num+m+1 )
            bit = checkbit(word)
       endwhile
endif

* Read Verification OBS:OBSGC
* ---------------------------
if( subwrd(args,num) = '-OBS' )
              n = n + 1
          OBS.n = subwrd(args,num+n   )
           word = subwrd(args,num+n+1 )
            bit = checkbit(word)
       while( bit != '-' )
              n = n + 1
          OBS.n = subwrd(args,num+n   )
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


* Construct Model GCs from Model EXPORTS
* --------------------------------------
     numm  = m
        m  = 0
        k  = 1
while ( k <= numm )
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


* Construct OBS GCs from OBS EXPORTS
* ----------------------------------
    numn = n
if( numn = 0 )
    numn = numm
           n  = 1
   while ( n <= numn )
       OBS.n = EXPORT.n
     OBSGC.n =     GC.n
           n = n + 1
   endwhile
else
        m  = 0
        k  = 1
while ( k <= numn )
        EX = ''
         j = 1
       bit = substr(OBS.k,j,1)
       while(bit != ':' & bit != '')
        EX = EX''bit
         j = j + 1
       bit = substr(OBS.k,j,1)
       endwhile
       if( EX != OBS.k )
         m = m + 1
         j = j + 1
       OBSGC.m = ''
       bit = substr(OBS.k,j,1)
       while(bit != '')
       OBSGC.m = OBSGC.m''bit
         j = j + 1
       bit = substr(OBS.k,j,1)
       endwhile
       OBS.k = EX
       endif
k = k + 1
endwhile
endif

**************************************************
**************************************************

* Initialize
* ----------
'reinit'
'set display color white'
'set clab off'
'c'

'run getenv "GEOSUTIL"'
             geosutil = result

'run getenv "VERIFICATION"'
             verification = result

'run uppercase 'seasons
                seasons = result

**************************************************
****            Echo Calling Sequence         ****
**************************************************

say ' '
say 'NAME   = 'NAME
say 'EXPID  = 'EXPID
say 'OUTPUT = 'OUTPUT
say 'DEBUG  = 'DEBUG
say 'SCALE  = 'SCALE
say 'RC     = 'RC
say 'LAND   = 'LAND
say 'OCEAN  = 'OCEAN
say 'TAYLOR = 'TAYLOR
say 'SEASON = 'seasons
say ' '
m = 1
while( m<=numm )
say 'EXPORT.'m' = 'EXPORT.m
say '    GC.'m' = 'GC.m
m = m + 1
endwhile
say ' '
n = 1
while( n<=numn )
say '   OBS.'n' = 'OBS.n
say ' OBSGC.'n' = 'OBSGC.n
n = n + 1
endwhile
say ' '

**************************************************
**************************************************

* Get Model Variables
* -------------------
        m  = 1
while ( m <= numm )
'run getvar 'EXPORT.m' 'GC.m
        qname.m = subwrd(result,1)
        qfile.m = subwrd(result,2)
        qscal.m = subwrd(result,3)
        qdesc.m = subwrd(result,4)
    if( qname.m = 'NULL' ) ; return ; endif
         m  = m + 1
endwhile


* Set proper ZDIM
* ---------------
 'set dfile 'qfile.1
 'getlevs   'qname.1
    nlevs = result
if( nlevs != 'NULL' )
   'run setenv "ZDIM" 'nlevs
endif


* Ensure NAMES have no underscores
* --------------------------------
        m=1
while ( m<numm+1 )
'fixname 'qname.m
          alias.m = result
say 'Alias #'m' = 'alias.m
        m = m+1
endwhile

* Set Geographic Environment from Model Dataset
* ---------------------------------------------
'set dfile 'qfile.1
'setlons'
'set lat -90 90'
'getinfo dlon'
         dlon = result
'getinfo dlat'
         dlat = result
'getinfo lonmin'
         lonmin = result
'getinfo latmin'
         latmin = result

* Check for Model Name Consistency
* --------------------------------
 m = 1
while( m <= numm )
'set dfile 'qfile.m
if( LEVEL = "NULL" )
   'set z 1'
else
   'set lev 'LEVEL
endif
'sett'
if( qname.m != alias.m ) ; 'rename 'qname.m ' 'alias.m ; endif
m = m + 1
endwhile

* Set BEGDATE and ENDDATE for Seasonal Calculations
* -------------------------------------------------
'setdates'

* Extract Beginning and Ending Dates for Plots
* --------------------------------------------
'run getenv "BEGDATE"'
             begdate  = result
'run getenv "ENDDATE"'
             enddate  = result
if( begdate = "NULL" )
   'set dfile 'qfile.1
   'set t    '1
   'getinfo date'
         begdate = result
endif
if( enddate = "NULL" )
   'set dfile 'qfile.1
   'getinfo tdim'
            tdim     = result
   'set t  'tdim
   'getinfo date'
         enddate = result
endif


* Land/Water Masks
* ----------------
if( LAND = 'TRUE' | OCEAN = 'TRUE' )
   'setmask     mod'
   'define omaskmod = maskout( 1, lwmaskmod-0.5 )'
   'define lmaskmod = maskout( 1, 0.5-lwmaskmod )'
endif


* Perform Model Formula Calculation
* ---------------------------------
'set dfile 'qfile.1
'sett'
if( numm = 1 )
   'define qmod = 'alias.1'.'qfile.1'*'qscal.1
else
    filename  = geosutil'/plots/'NAME'/modform.gs'
    ioflag    = sublin( read(filename),1 )
    if(ioflag = 0)
       close  = close(filename)
       mstring = ''
       m  = 1
       while ( m <= numm )
          if( qname.m != alias.m )
              mstring = mstring' 'alias.m'*'qscal.m
          else
              mstring = mstring' 'alias.m'.'qfile.m'*'qscal.m
          endif
              m  = m + 1
       endwhile
      'run 'geosutil'/plots/'NAME'/modform 'mstring
    else
       mstring = NAME
       m  = 1
       while ( m <= numm )
          if( qname.m != alias.m )
              mstring = mstring' 'alias.m'*'qscal.m
          else
              mstring = mstring' 'alias.m'.'qfile.m'*'qscal.m
          endif
              m  = m + 1
       endwhile
      'run 'geosutil'/plots/'DIR'/'NAME'.gs 'mstring
      'define qmod = 'NAME'.1'
    endif
endif

if(    LAND  = 'TRUE'   |  OCEAN = 'TRUE' )
   if( LAND  = 'TRUE' ) ; 'define qmod = maskout( 'SCALE'*qmod,lmaskmod )' ; endif
   if( OCEAN = 'TRUE' ) ; 'define qmod = maskout( 'SCALE'*qmod,omaskmod )' ; endif
else
                          'define qmod =          'SCALE'*qmod'
endif

'seasonal qmod'




* Loop over Possible Experiment Datasets for Comparison
* -----------------------------------------------------
'!/bin/mv HISTORY.T HISTORY.Tmp'
'run getenv "CMPEXP"'
         cmpexp = result
            num = 1
            exp = subwrd( cmpexp,num )
while( exp != 'NULL' )
say ' '
say 'Comparing with: 'exp

'!chckfile 'exp'/.HOMDIR'
 'run getenv CHECKFILE'
         CHECKFILE  = result
     if( CHECKFILE != 'NULL' )
        '!/bin/cp `cat 'exp'/.HOMDIR`/HISTORY.rc .'
     else
        '!/bin/cp 'exp'/HISTORY.rc .'
     endif
'!remove CHECKFILE.txt'

'!cat HISTORY.rc | sed -e "s/,/ , /g" > HISTORY.T'

* Get EXP Comparison Variables
* ----------------------------
FOUND = TRUE
        m  = 1
while ( m <= numm )
'run getvar 'EXPORT.m' 'GC.m' 'exp
        oname.m = subwrd(result,1)
        ofile.m = subwrd(result,2)
        oscal.m = subwrd(result,3)
        odesc.m = subwrd(result,4)
         otag.m = subwrd(result,5)
    if( oname.m = 'NULL' ) ; FOUND = FALSE ; endif
         m  = m + 1
endwhile

if( FOUND = TRUE )
'setlons'
'set lat -90 90'

* Land/Water Masks
* ----------------
if( LAND = 'TRUE' | OCEAN = 'TRUE' )
   'set dfile 'ofile.1
if( LEVEL = "NULL" )
   'set z 1'
else
   'set lev 'LEVEL
endif
   'set t 1'
   'setmask     obs'
   'define omaskobs = maskout( 1, lwmaskobs-0.5 )'
   'define lmaskobs = maskout( 1, 0.5-lwmaskobs )'
endif

'set dfile 'ofile.1
if( LEVEL = "NULL" )
   'set z 1'
else
   'set lev 'LEVEL
endif
    'getdates'
     begdateo = subwrd(result,1)
     enddateo = subwrd(result,2)

* Perform OBS Formula Calculation
* -------------------------------
if( numm = 1 )
   'define qobs = 'oname.1'.'ofile.1'*'oscal.1
else
    filename  = geosutil'/plots/'NAME'/modform.gs'
    ioflag    = sublin( read(filename),1 )
    if(ioflag = 0)
       close  = close(filename)
       ostring = ''
       n  = 1
       while ( n <= numm )
          ostring = ostring' 'oname.n'.'ofile.n'*'oscal.n
               n  = n + 1
       endwhile
      'run 'geosutil'/plots/'NAME'/modform 'ostring
      'define qobs = qmod'
    else
       ostring = NAME
       n  = 1
       while ( n <= numm )
          ostring = ostring' 'oname.n'.'ofile.n'*'oscal.n
               n  = n + 1
       endwhile
      'run 'geosutil'/plots/'DIR'/'NAME'.gs 'ostring
      'define qobs = 'NAME'.1'
    endif
endif

* Check for MERRA Resolution problem
* ----------------------------------
'getinfo xdim'
         xdim = result
if( xdim != 540 )

    if(    LAND  = 'TRUE'   |  OCEAN = 'TRUE' )
       if( LAND  = 'TRUE' ) ; 'define qobs = maskout( 'SCALE'*qobs,lmaskobs )' ; endif
       if( OCEAN = 'TRUE' ) ; 'define qobs = maskout( 'SCALE'*qobs,omaskobs )' ; endif
    else
                              'define qobs =          'SCALE'*qobs'
    endif

else

   'define qobs = regrid2( qobs,'dlon','dlat',bs_p1,'lonmin','latmin')'
    if(    LAND  = 'TRUE'   |  OCEAN = 'TRUE' )
       if( LAND  = 'TRUE' ) ; 'define qobs = maskout( 'SCALE'*qobs,lmaskmod )' ; endif
       if( OCEAN = 'TRUE' ) ; 'define qobs = maskout( 'SCALE'*qobs,omaskmod )' ; endif
    else
                              'define qobs =          'SCALE'*qobs'
    endif

endif

* Compute Seasonal Means
* ----------------------
'seasonal qobs'


'run getenv "CLIMATE"'
             climate = result

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
'run setenv "LEVTYPE" 'ALEVS

* Horizontal Plot
* ---------------
        flag = ""
while ( flag = "" )
'makplot 'NAME'  'EXPID' 'PREFIX' 'season' 'OUTPUT' 'qfile.1' 'qdesc.1' 'ofile.1' 'otag.1' 'odesc.1' 'begdate' 'enddate' 'begdateo' 'enddateo' 'climate' 'GC.1' 'MATH
 if( DEBUG = "debug" )
     say "Hit ENTER to repeat plot, or NON-BLANK to continue"
     pull flag
 else
     flag = "next"
 endif
'c'
endwhile

* Zonal Mean Plot
* ---------------
        flag = ""
while ( flag = "" )
'makplotz 'NAME'  'EXPID' 'PREFIX' 'season' 'OUTPUT' 'qfile.1' 'qdesc.1' 'ofile.1' 'otag.1' 'odesc.1' 'begdate' 'enddate' 'begdateo' 'enddateo' 'climate' 'GC.1' 'MATH
 if( DEBUG = "debug" )
     say "Hit ENTER to repeat plot, or NON-BLANK to continue"
     pull flag
 else
     flag = "next"
 endif
'c'
endwhile


* End Season Test
* ---------------
endif
* End Season Loop
* ---------------
endwhile
* End FOUND Test
* --------------
endif

* Check next Comparison Experiment Dataset
* ----------------------------------------
  num = num + 1
  exp = subwrd( cmpexp,num )
endwhile
'!/bin/mv HISTORY.Tmp HISTORY.T'




* Loop over Verification Datasets
* -------------------------------
'getnumrc 'geosutil'/plots/'NAME
     rcinfo = result
     numrc  = subwrd( rcinfo,1 )
say 'Initial RCINFO: 'rcinfo

if( numrc = 0 )
   'getnumrc 'geosutil'/plots/'DIR
    rcinfo = result
    numrc  = subwrd( rcinfo,1 )
say '  Final RCINFO: 'rcinfo
endif

         k  = 1
while(   k <= numrc )
        loc = k + 1
     rcfile = subwrd( rcinfo,loc )
     RCFILE = rcfile
     if(RC != 'NULL') ; RCFILE = geosutil'/plots/'NAME'/VERIFICATION.'RC'.rc' ; endif
 if( RCFILE =  rcfile )


* Get Verification Variables
* --------------------------
FOUND = TRUE
        n  = 1
while ( n <= numn )
'run getobs 'OBS.n' 'OBSGC.n' 'rcfile
        oname.n = subwrd(result,1)
        ofile.n = subwrd(result,2)
        oscal.n = subwrd(result,3)
        odesc.n = subwrd(result,4)
         otag.n = subwrd(result,5)
    if( oname.n = 'NULL' ) ; FOUND = FALSE ; endif
         n  = n + 1
endwhile

if( FOUND = TRUE )
'setlons'
'set lat -90 90'

* Land/Water Masks
* ----------------
if( LAND = 'TRUE' | OCEAN = 'TRUE' )
   'set dfile 'ofile.1
if( LEVEL = "NULL" )
   'set z 1'
else
   'set lev 'LEVEL
endif
   'set t 1'
   'setmask     obs'
   'define omaskobs = maskout( 1, lwmaskobs-0.5 )'
   'define lmaskobs = maskout( 1, 0.5-lwmaskobs )'
endif


'set dfile 'ofile.1
if( LEVEL = "NULL" )
   'set z 1'
else
   'set lev 'LEVEL
endif
    'getdates'
     begdateo = subwrd(result,1)
     enddateo = subwrd(result,2)

* Perform OBS Formula Calculation
* -------------------------------
if( numn = 1 )
   'define qobs = 'oname.1'.'ofile.1'*'oscal.1
else
    filename  = geosutil'/plots/'NAME'/obsform.gs'
    ioflag    = sublin( read(filename),1 )
    if(ioflag = 0)
       close  = close(filename)
       ostring = ''
       n  = 1
       while ( n <= numn )
          ostring = ostring' 'oname.n'.'ofile.n'*'oscal.n
               n  = n + 1
       endwhile
      'run 'geosutil'/plots/'NAME'/obsform 'ostring
    else
       ostring = NAME
       n  = 1
       while ( n <= numn )
          ostring = ostring' 'oname.n'.'ofile.n'*'oscal.n
               n  = n + 1
       endwhile
      'run 'geosutil'/plots/'DIR'/'NAME'.gs 'ostring
      'define qobs = 'NAME'.1'
    endif
endif

* Check for MERRA Resolution problem
* ----------------------------------
'getinfo xdim'
         xdim = result
if( xdim != 540 )

    if(    LAND  = 'TRUE'   |  OCEAN = 'TRUE' )
       if( LAND  = 'TRUE' ) ; 'define qobs = maskout( qobs,lmaskobs )' ; endif
       if( OCEAN = 'TRUE' ) ; 'define qobs = maskout( qobs,omaskobs )' ; endif
    endif

else

   'define qobs = regrid2( qobs,'dlon','dlat',bs_p1,'lonmin','latmin')'
    if(    LAND  = 'TRUE'   |  OCEAN = 'TRUE' )
       if( LAND  = 'TRUE' ) ; 'define qobs = maskout( qobs,lmaskmod )' ; endif
       if( OCEAN = 'TRUE' ) ; 'define qobs = maskout( qobs,omaskmod )' ; endif
    endif

endif

* Compute Seaonal Means
* ---------------------
'seasonal qobs'

'run getenv "CLIMATE"'
             climate = result


* Perform Taylor Plots
* --------------------
'set dfile 'qfile.1
'run getenv "TAYLOR"'
         taylor = result
if(      taylor = 'true' & TAYLOR = 'TRUE' )

'taylor qmodmdjf qobsdjf djf 'EXPID
'taylor qmodmjja qobsjja jja 'EXPID
'taylor qmodmson qobsson son 'EXPID
'taylor qmodmmam qobsmam mam 'EXPID
'taylor qmodmann qobsann ann 'EXPID

'taylor_write 'EXPID' 'NAME' 'OUTPUT
'taylor_read   GFDL   'NAME' 'verification
'taylor_read   CAM3   'NAME' 'verification
'taylor_read   e0203  'NAME' 'verification
                                                                                                   
"taylor_plt 4 CAM3 GFDL e0203 "EXPID" "OUTPUT" "NAME" '"EXPID" "NAME" vs "obsnam"' "DEBUG
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

'set dfile 'qfile.1
'set gxout shaded'
'rgbset'
'run setenv "LEVTYPE" 'DLEVS

* Horizontal Plot
* ---------------
        flag = ""
while ( flag = "" )
'makplot 'NAME'  'EXPID' 'PREFIX' 'season' 'OUTPUT' 'qfile.1' 'qdesc.1' 'ofile.1' 'otag.1' 'odesc.1' 'begdate' 'enddate' 'begdateo' 'enddateo' 'climate' 'GC.1' 'MATH
 if( DEBUG = "debug" )
     say "Hit ENTER to repeat plot, or NON-BLANK to continue"
     pull flag
 else
     flag = "next"
 endif
'c'
endwhile

* Zonal Mean Plot
* ---------------
        flag = ""
while ( flag = "" )
'makplotz 'NAME'  'EXPID' 'PREFIX' 'season' 'OUTPUT' 'qfile.1' 'qdesc.1' 'ofile.1' 'otag.1' 'odesc.1' 'begdate' 'enddate' 'begdateo' 'enddateo' 'climate' 'GC.1' 'MATH
 if( DEBUG = "debug" )
     say "Hit ENTER to repeat plot, or NON-BLANK to continue"
     pull flag
 else
     flag = "next"
 endif
'c'
endwhile


* End Season Test
* ---------------
endif
* End Season Loop
* ---------------
endwhile
* End FOUND Test
* --------------
endif


* End RC=NULL Test
* ----------------
endif
* Update Verification Loop Index
* ------------------------------
k = k + 1

* End Verification Loop
* ---------------------
endwhile



*******************************************************
****            No Verification Case               ****
*******************************************************

if( numrc = 0 )

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

* Horizontal Plot
* ---------------
        flag = ""
while ( flag = "" )

'uniplot 'NAME'  'EXPID' 'PREFIX' 'season' 'OUTPUT' 'qfile.1' 'qdesc.1' 'begdate' 'enddate' 'begdateo' 'enddateo' 'climate
 if( DEBUG = "debug" )
     say "Hit ENTER to repeat plot, or NON-BLANK to continue"
     pull flag
 else
     flag = "next"
 endif
'c'
endwhile

* Zonal Mean Plot
* ---------------
        flag = ""
while ( flag = "" )
'uniplotz 'NAME'  'EXPID' 'PREFIX' 'season' 'OUTPUT' 'qfile.1' 'qdesc.1' 'begdate' 'enddate' 'begdateo' 'enddateo' 'climate
 if( DEBUG = "debug" )
     say "Hit ENTER to repeat plot, or NON-BLANK to continue"
     pull flag
 else
     flag = "next"
 endif
'c'
endwhile


* End Season Test
* ---------------
endif
* End Season Loop
* ---------------
endwhile


* End Test for NUMRC
* ------------------
endif
'quit'
return

* To Prevent Problem with BIT: E
* ------------------------------
function checkbit (word)
      bit = substr(word,1,1)
      dum = bit'TEST'
      if( dum = "ETEST" ) ; bit = A ; endif
return bit
