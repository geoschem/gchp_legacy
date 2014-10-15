function makplotz (args)
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

say ''
'getresource 'geosutil'/plots/'name'/plot.rc TITLE'
          title = result
'getresource 'geosutil'/plots/'name'/plot.rc AXLIM'
          axlim = result
'getresource 'geosutil'/plots/'name'/plot.rc YLAB'
          ylab  = result
'getresource 'geosutil'/plots/'name'/plot.rc GRID'
          grid  = result
say ''

axmin = subwrd(axlim,1)
axmax = subwrd(axlim,2)

'set vpage off'
'set parea off'
'set mproj scaled'
'set grid  off'
'set frame on'
'set xlopts 1 3 .11'
'set ylopts 1 3 .11'

* Count Seasons
* -------------
'set dfile 'mfile
'count "'season'" 'bdate' 'edate
 nmod = result

'setlons'
'set lat -90 90'
'makez qmod'season' z'

'set t   1'
'set lon 0'

'set vpage 0 11 0 8.5'
'set parea 1.8 9.5 1.5 7.5'
'set ylab  'ylab
'set axlim 'axlim
if( grid != 'NULL' ) ; 'set grid  'grid  ; endif

'set grads off'
'set cmark  0'
'set cstyle 1'
'set ccolor 1'
'd qmod'season'z'

* Draw Zero Line
* --------------
if( axmin*axmax < 0 )
'set cmark  0'
'set cstyle 1'
'set cthick 1'
'set ccolor 2'
'd lon-lon'
endif

'setlons'

'set vpage off'
'set string 1 c 6'
'set strsiz .11'
*'xlabel 1 5.5 8.15'
'draw string 5.5 8.15 EXPID: 'expid'  'mdesc
'draw string 5.5 7.8 'title' 'season' ('nmod')'

* Print Beginning and Ending Dates
* --------------------------------
                date = getdate (bdate)
bmnthm = subwrd(date,1)
byearm = subwrd(date,2)
                date = getdate (edate)
emnthm = subwrd(date,1)
eyearm = subwrd(date,2)

'set string 4 l 4'
'set strsiz .08'
'draw string 1.70  0.87 Beg: 'bmnthm' 'byearm
'draw string 1.70  0.70 End: 'emnthm' 'eyearm
'set string 1 l 4'
'draw string 1.70  1.02 Mod Dates:'
'set string 1 c 6'
* --------------------------------


if( prefix != NULL )
   'myprint -name 'output'/'name'_z_'prefix'.'season
else
   'myprint -name 'output'/'name'_z.'season
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
