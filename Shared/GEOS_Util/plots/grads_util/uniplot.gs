function uniplot (args)
                           k = 1
name    = subwrd(args,k) ; k = k+1
expid   = subwrd(args,k) ; k = k+1
prefix  = subwrd(args,k) ; k = k+1
season  = subwrd(args,k) ; k = k+1
output  = subwrd(args,k) ; k = k+1
mfile   = subwrd(args,k) ; k = k+1
mdesc   = subwrd(args,k) ; k = k+1
bdate   = subwrd(args,k) ; k = k+1
edate   = subwrd(args,k) ; k = k+1
bdateo  = subwrd(args,k) ; k = k+1
edateo  = subwrd(args,k) ; k = k+1
climate = subwrd(args,k) ; k = k+1

'run getenv "GEOSUTIL"'
             geosutil = result

if( prefix != NULL )
    PFX = prefix'_'
else
    PFX = ''
endif

say ''
                        'getresource 'geosutil'/plots/'name'/plot.rc 'PFX'TITLE'
if( result = 'NULL' ) ; 'getresource 'geosutil'/plots/'name'/plot.rc      TITLE' ; endif
                                                                          title  = result

                        'getresource 'geosutil'/plots/'name'/plot.rc 'PFX'CLEVS'
if( result = 'NULL' ) ; 'getresource 'geosutil'/plots/'name'/plot.rc      CLEVS' ; endif
                                                                          clevs  = result

                        'getresource 'geosutil'/plots/'name'/plot.rc 'PFX'DLEVS'
if( result = 'NULL' ) ; 'getresource 'geosutil'/plots/'name'/plot.rc      DLEVS' ; endif
                                                                          dlevs  = result

                        'getresource 'geosutil'/plots/'name'/plot.rc 'PFX'CCOLS'
if( result = 'NULL' ) ; 'getresource 'geosutil'/plots/'name'/plot.rc      CCOLS' ; endif
                                                                          ccols  = result

                        'getresource 'geosutil'/plots/'name'/plot.rc 'PFX'DCOLS'
if( result = 'NULL' ) ; 'getresource 'geosutil'/plots/'name'/plot.rc      DCOLS' ; endif
                                                                          dcols  = result
say ''

'set vpage off'
'set parea off'
'set grid  off'
'set mproj scaled'
'set frame on'
'set xlopts 1 3 .11'
'set ylopts 1 3 .11'

* Count Seasons
* -------------
'set dfile 'mfile
'count "'season'" 'bdate' 'edate
 nmod = result

'set t 1'

'set vpage 0 11 0 8.5'
'set parea 0.8 10.5 1.2 7.8'
'set grads off'
'set clevs 'clevs
'set ccols 'ccols
'd qmod'season
'cbarn -snum 0.75'

'stats qmod'season
 avgmod = subwrd(result,1)
 stdmod = subwrd(result,2)

'set vpage off'
'set parea off'
'set string 1 c 6'
'set strsiz .11'
*'xlabel 1 5.5 8.35'
'draw string 5.5  8.35 EXPID: 'expid'  'mdesc
'draw string 5.5  8.0 'title' 'season' ('nmod')'

* Print Beginning and Ending Dates
* --------------------------------
                date = getdate (bdate)
bmnthm = subwrd(date,1)
byearm = subwrd(date,2)
                date = getdate (edate)
emnthm = subwrd(date,1)
eyearm = subwrd(date,2)

'set string 1 l 4'
'set strsiz .08'
'draw string 0.80  8.24 Beg: 'bmnthm' 'byearm
'draw string 0.80  8.10 End: 'emnthm' 'eyearm
'set string 1 c 6'
* --------------------------------

'set string 1 c 4'
'set strsiz .08'
'draw string 9.82 8.24 Mean: 'avgmod
'draw string 9.82 8.10  Std: 'stdmod

if( prefix != NULL )
   'myprint -name 'output'/'name'_'prefix'.'season
else
   'myprint -name 'output'/'name'.'season
endif

'set mproj latlon'
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
