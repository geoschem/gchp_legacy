function makefile (args)

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
    say 'Creating File for Data:'
    say '-----------------------'

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
'getinfo dlon' 
         dlon  = result
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

* ----------------------------------------------------------------------------

* Initialize Month Counters
* -------------------------
   'set t 1 '
   'set x 1 'xdim
   'set y 1 'ydim
   'set z   'zmin' 'zmax
           k =1
    while( k<=12 )
     month.k = 0
    'define loc'k' = lat-lat + lon-lon + lev-lev'
           k =k+1
    endwhile

* Compute Climatological Function
* -------------------------------
           k =1
           t =tmin
    while( t<=tmax )
      'set t 't
      'set y 1 'ydim
      'set z   'zmin' 'zmax
      'set x 1 'xdim
      'getinfo date'
               date = result
       say '          Computing Function: 'function' with NAME: 'name'  for t = 't'  'date
      'define loc'k' =   loc'k' + 'function
             month.k = month.k  + 1
                   k =       k  + 1
             if( k>12 ) ; k = 1 ; endif
    t = t+1
    endwhile
    say ''

           k =1
    while( k<=12 )
       if( month.k != 0 ) 
       say 'define loc'k' = loc'k' / 'month.k
           'define loc'k' = loc'k' / 'month.k
       endif
       k =k+1
    endwhile

* Write Climatological Function
* -----------------------------
           k =1
           t =tmin
    while( t<=tmin+11 )
       if( month.k != 0 ) 

          'set t 't
          'getinfo year'
                   year  = result
          'getinfo month'
                   month = result
                   month = getnum(month)

          'set sdfwrite -5d 'name'.'year''month'.nc4'
          'set undef 'undef
          'set x 1   'xdim
          'set y 1   'ydim
          'set z     'zmin' 'zmax
          'define    'qdata' = loc'k
          'sdfwrite  'qdata

           k =k+1
       endif
    t = t+1
    endwhile
        kmax =k-1

* ----------------------------------------------------------------------------

    say ''
           k =1
    while( k<=12 )
   'undefine loc'k
           k =k+1
    endwhile

  '!remove sedfile'
  '!touch  sedfile'

* Only Allow 6 Decimal Locations for DLON (For MERRA 540 Resolution)
* ------------------------------------------------------------------
          n  = 1
        bit  = substr(dlon,1,1)
    if( bit != '.' )
        while( bit != '.' & bit != '' )
                 n  = n + 1
               bit  = substr(dlon,n,1)
        endwhile
    endif
           n  = n + 6
        dlon  = substr(dlon,1,n)
  say ' DLON: 'dlon' (Final)'

'!echo "s?@DT?"'dt'?g >> sedfile'
'!echo "s?@XDIM?"'xdim'?g >> sedfile'
'!echo "s?@YDIM?"'ydim'?g >> sedfile'
'!echo "s?@TDIM?"'kmax'?g >> sedfile'
'!echo "s?@BLON?"'lon0'?g >> sedfile'
'!echo "s?@BLAT?"'lat0'?g >> sedfile'
'!echo "s?@BDATE?"'begdate'?g >> sedfile'
'!echo "s?@DLON?"'dlon'?g >> sedfile'
'!echo "s?@DLAT?"'dlat'?g >> sedfile'
'!echo "s?q.data?"'name'.%y4%m2.nc4?g >> sedfile'
'!/bin/cp 'geosutil'/plots/grads_util/makefile.tmpl .'
'!sed -f   sedfile makefile.tmpl > 'name'.ddf'

   'xdfopen 'name'.ddf'
   'getinfo    numfiles'
               newfile = result
   'set dfile 'newfile

   'set lat 'latmin' 'latmax
   'set lon 'lonmin' 'lonmax
   'set t 1 'kmax
   'setz'

return newfile' 'kmax


function getnum(month)
            if( month = JAN ) ; num = 01 ; endif
            if( month = FEB ) ; num = 02 ; endif
            if( month = MAR ) ; num = 03 ; endif
            if( month = APR ) ; num = 04 ; endif
            if( month = MAY ) ; num = 05 ; endif
            if( month = JUN ) ; num = 06 ; endif
            if( month = JUL ) ; num = 07 ; endif
            if( month = AUG ) ; num = 08 ; endif
            if( month = SEP ) ; num = 09 ; endif
            if( month = OCT ) ; num = 10 ; endif
            if( month = NOV ) ; num = 11 ; endif
            if( month = DEC ) ; num = 12 ; endif
return num

