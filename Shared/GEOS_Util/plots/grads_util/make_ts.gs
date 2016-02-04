function makets (args)

'numargs  'args
 numargs = result

       num =  0
while( num < numargs )
       num = num + 1
if( subwrd(args,num)='-FUNCTION') ; function = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-NAME'  )   ; name     = subwrd(args,num+1) ; endif
endwhile

* Variable is in File
* -------------------
    say ''
    say 'Creating Time_Series File for Data:'
    say '-----------------------------------'

'getinfo file'
       infile = result

'q gxout'
   gxout = sublin(result,4)
   gxout = subwrd(gxout ,6)

'run getenv "GEOSUTIL"'
             geosutil = result

'getinfo lonmin'
         lonmin = result
'getinfo lonmax'
         lonmax = result

'getinfo latmin'
         latmin = result
'getinfo latmax'
         latmax = result

'getinfo tmin'
         tmin = result
'getinfo tmax'
         tmax = result

'set t 'tmin
'getinfo date'
      begdate = result

'getinfo zmin'
         zmin = result
'getinfo zmax'
         zmax = result
'set z  'zmin' 'zmax
         zdim = zmax-zmin+1

'sety'
'set t 'tmin' 'tmax
        tdim = tmax-tmin+1

'getinfo xdim'
         xdim = result
'getinfo ydim'
         ydim = result

'getinfo undef'
         undef = result
'getinfo dlat' 
         dlat  = result
     if( dlat  < 0 )
         dlat  = -dlat
     endif
'getinfo dlon' 
         dlon  = result
     if( dlon  < 0 )
         dlon  = -dlon
     endif
       lontot  = lonmax - lonmin + dlon/2


* Get INFO from CTL
* -----------------
'q ctlinfo'
         n = 1
      while( n >0 )
             line = sublin(result,n)
             word = subwrd(line,1)
         if( word = 'tdef' )
             dt = subwrd(line,5)
             n  = 0
          else
             n  = n + 1
          endif
      endwhile
         n = 1
      while( n >0 )
             line = sublin(result,n)
             word = subwrd(line,1)
         if( word = 'ydef' )
           lat0 = subwrd(line,4)
             n  = 0
          else
             n  = n + 1
          endif
      endwhile
         n = 1
      while( n >0 )
             line = sublin(result,n)
             word = subwrd(line,1)
         if( word = 'xdef' )
           lon0 = subwrd(line,4)
             n  = 0
          else
             n  = n + 1
          endif
      endwhile
* -----------------

      levs = ''
             z  = zmin
      while( z <= zmax )
            'set z 'z
            'getinfo level'
             levs = levs' 'result
             z = z + 1
      endwhile
       
    say ''
    say 'DFILE: 'infile
    say 'UNDEF: 'undef
    say ' LON0: 'lon0
    say ' DLON: 'dlon
    say ' YDEF: 'ydim
    say ' LAT0: 'lat0
    say ' DLAT: 'dlat
    say ' XDEF: 'xdim
    say ' ZDEF: 'zdim
    say ' TDEF: 'tdim
    say 'BDATE: 'begdate
    say '   DT: 'dt
    say ' LEVS: 'levs
    say ''
    say ' LONMIN:LONMAX = 'lonmin':'lonmax
    say ''

  '!remove sedfile'
  '!touch  sedfile'
  '!remove 'name'.ctl'
  '!remove 'name'.data'
   'set gxout fwrite'
   'set       fwrite 'name'.data'
   'set undef 'undef

* ----------------------------------------------------------------------------

* Initialize Dimensions
* ---------------------
   'set t 1 '
   'set x 1 'xdim
   'set y 1 'ydim
   'set z   'zmin' 'zmax

* Compute Time Series
* -------------------
           t =tmin
    while( t<=tmax )
       say '          Computing Global Mean for Function: 'function' with NAME: 'name'  for t = 't
      'set t 't
      'set x 1'
      'set y 1'
      'set z 1'
       if( function = "NULL" )
           'd 0'
       else
           'd aave('function',x=1,x='xdim',y=1,y='ydim')'
       endif
    t = t+1
    endwhile
    say ''

        kmax = tmax-tmin+1

* ----------------------------------------------------------------------------

    say ''
   'disable fwrite'

   'set gxout 'gxout

'!echo "s?UNDEF?"'undef'?g >> sedfile'
'!echo "s?DX?"'dlon'?g >> sedfile'
'!echo "s?DY?"'dlat'?g >> sedfile'
'!echo "s?DT?"'dt'?g >> sedfile'
'!echo "s?LON0?"'lon0'?g >> sedfile'
'!echo "s?LAT0?"'lat0'?g >> sedfile'
'!echo "s?XDIM?"'1'?g >> sedfile'
'!echo "s?YDIM?"'1'?g >> sedfile'
'!echo "s?ZDIM?"'1'?g >> sedfile'
'!echo "s?TDIM?"'kmax'?g >> sedfile'
'!echo "s?BDATE?"'begdate'?g >> sedfile'
'!echo "s?LEVS?"'levs'?g  >> sedfile'
'!echo "s?q.data?"'name'.data?g >> sedfile'
'!echo "s?qdata?"'name'?g >> sedfile'
'!/bin/cp 'geosutil'/plots/grads_util/make_ts.tmpl .'
'!sed -f   sedfile make_ts.tmpl > 'name'.ctl'

   'open 'name'.ctl'
   'getinfo    numfiles'
               newfile = result
   'set dfile 'newfile

   'set lat 'latmin' 'latmax
   'set lon 'lonmin' 'lonmax
   'set t 1 'kmax
   'setz'
   'define 'name'g = 'name

return newfile' 'kmax
