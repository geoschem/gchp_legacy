function makequad (args)

'numargs  'args
 numargs = result

     zonal = false
       num =  0
while( num < numargs )
       num = num + 1
if( subwrd(args,num)='-q1'     ) ; name1  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-file1'  ) ; file1  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-scale1' ) ; scale1 = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-q2'     ) ; name2  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-file2'  ) ; file2  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-scale2' ) ; scale2 = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-qq'     ) ; nameqq = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-tag'    ) ; tag    = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-zonal'  ) ; zonal  = true               ; endif
endwhile

* Variable is in File
* -------------------
    say ''
    say 'Computing Quadratic Data:'
    say '-------------------------'

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

    t = tmin
    while( t<=tmax )
    say '          Computing Quadratic Data for 'name1' & 'name2' ('nameqq') with Tag: 'tag'  for t = 't
   'set t 't

          'getinfo year'
                   year  = result
          'getinfo month'
                   month = result
                   month = getnum(month)
 
   'sety'
   'set z 'zmin' 'zmax

       'set x 1 'xdim
       'define  qundef = lon-lon + lat-lat + 'undef
    if( name1 = NULL )
       'define  locq1  =  qundef'
    else
       'set dfile 'file1
       'define  locq1  = 'name1'*'scale1
    endif
    if( name2 = NULL )
       'define  locq2  =  qundef'
    else
       'set dfile 'file2
       'define  locq2  = 'name2'*'scale2
    endif
    if( nameqq = NULL )
       'define  locqq  =  qundef'
    else
       'define  locqq  = 'nameqq'*'scale1'*'scale2
    endif

   'set x 1'
   'define locq1z = ave(locq1,x=1,x='xdim')'
   'define locq2z = ave(locq2,x=1,x='xdim')'

   'set x 1 'xdim
   'define q1sq2s = (locq1-locq1z)*(locq2-locq2z)'
   'define q1pq2p =  locqq'

if( zonal = 'false' )
   'set sdfwrite -5d 'nameqq's'tag'.'year''month'.nc4'
   'set undef 'undef
   'sdfwrite q1sq2s'

   'set sdfwrite -5d 'nameqq'p'tag'.'year''month'.nc4'
   'set undef 'undef
   'sdfwrite q1pq2p'
else
   'set x 1'
   'define q1sq2sz = ave(q1sq2s,x=1,x='xdim')'
   'define q1pq2pz = ave(q1pq2p,x=1,x='xdim')'

   'set sdfwrite -5d 'nameqq'sz'tag'.'year''month'.nc4'
   'set undef 'undef
   'sdfwrite q1sq2sz'

   'set sdfwrite -5d 'nameqq'pz'tag'.'year''month'.nc4'
   'set undef 'undef
   'sdfwrite q1pq2pz'
endif

    t = t+1
    endwhile

* ----------------------------------------------------------------------------
* ----         Open grads fwrite file to compute seasonal means        -------
* ----------------------------------------------------------------------------

    say ''
   'undefine locq1'
   'undefine locq2'
   'undefine locqq'
   'undefine q1sq2s'
   'undefine q1pq2p'
if( zonal = 'true' )
   'undefine q1sq2sz'
   'undefine q1pq2pz'
endif


if( zonal = 'true' )
  '!remove sedfile'
  '!touch  sedfile'
  '!echo "s?@DT?"'dt'?g >> sedfile'
  '!echo "s?@TDIM?"'tdim'?g >> sedfile'
  '!echo "s?@BDATE?"'begdate'?g >> sedfile'
  '!echo "s?q.data?"'nameqq'sz'tag'.%y4%m2.nc4?g >> sedfile'
  '!/bin/cp 'geosutil'/plots/grads_util/makequad.tmpl .'
  '!sed -f   sedfile makequad.tmpl > 'nameqq'sz'tag'.ddf'

  '!remove sedfile'
  '!touch  sedfile'
  '!echo "s?@DT?"'dt'?g >> sedfile'
  '!echo "s?@TDIM?"'tdim'?g >> sedfile'
  '!echo "s?@BDATE?"'begdate'?g >> sedfile'
  '!echo "s?q.data?"'nameqq'pz'tag'.%y4%m2.nc4?g >> sedfile'
  '!/bin/cp 'geosutil'/plots/grads_util/makequad.tmpl .'
  '!sed -f   sedfile makequad.tmpl > 'nameqq'pz'tag'.ddf'
else
  '!remove sedfile'
  '!touch  sedfile'
  '!echo "s?@DT?"'dt'?g >> sedfile'
  '!echo "s?@TDIM?"'tdim'?g >> sedfile'
  '!echo "s?@BDATE?"'begdate'?g >> sedfile'
  '!echo "s?q.data?"'nameqq's'tag'.%y4%m2.nc4?g >> sedfile'
  '!/bin/cp 'geosutil'/plots/grads_util/makequad.tmpl .'
  '!sed -f   sedfile makequad.tmpl > 'nameqq's'tag'.ddf'

  '!remove sedfile'
  '!touch  sedfile'
  '!echo "s?@DT?"'dt'?g >> sedfile'
  '!echo "s?@TDIM?"'tdim'?g >> sedfile'
  '!echo "s?@BDATE?"'begdate'?g >> sedfile'
  '!echo "s?q.data?"'nameqq'p'tag'.%y4%m2.nc4?g >> sedfile'
  '!/bin/cp 'geosutil'/plots/grads_util/makequad.tmpl .'
  '!sed -f   sedfile makequad.tmpl > 'nameqq'p'tag'.ddf'
endif


if( zonal = 'true' )
   'xdfopen   'nameqq'sz'tag'.ddf'
   'getinfo    numfiles'
               newfile = result
   'set dfile 'newfile
   'set lon 'lon0
   'sety'
   'setz'
   'getinfo  tdim'
             tdim = result
   'set t 1 'tdim
    if( name1 = name2 ) 
   'define   Var'name1's'tag' = q1sq2sz'
   'seasonal Var'name1's'tag
   'undefine Var'name1's'tag
    else
   'define   Cov'name1''name2's'tag' = q1sq2sz'
   'seasonal Cov'name1''name2's'tag
   'undefine Cov'name1''name2's'tag
    endif
   'close 'newfile

   'xdfopen   'nameqq'pz'tag'.ddf'
   'getinfo    numfiles'
               newfile = result
   'set dfile 'newfile
   'set lon 'lon0
   'sety'
   'setz'
   'getinfo  tdim'
             tdim = result
   'set t 1 'tdim
    if( name1 = name2 ) 
   'define   Var'name1'p'tag' = q1pq2pz'
   'seasonal Var'name1'p'tag
   'undefine Var'name1'p'tag
    else
   'define   Cov'name1''name2'p'tag' = q1pq2pz'
   'seasonal Cov'name1''name2'p'tag
   'undefine Cov'name1''name2'p'tag
    endif
   'close 'newfile
else
   'xdfopen   'nameqq's'tag'.ddf'
   'getinfo    numfiles'
               newfile = result
   'set dfile 'newfile
   'set lon 'lonmin' 'lonmax
   'sety'
   'setz'
   'getinfo  tdim'
             tdim = result
   'set t 1 'tdim
    if( name1 = name2 ) 
   'define   Var'name1's'tag' = q1sq2s'
   'seasonal Var'name1's'tag
   'undefine Var'name1's'tag
    else
   'define   Cov'name1''name2's'tag' = q1sq2s'
   'seasonal Cov'name1''name2's'tag
   'undefine Cov'name1''name2's'tag
    endif
   'close 'newfile

   'xdfopen   'nameqq'p'tag'.ddf'
   'getinfo    numfiles'
               newfile = result
   'set dfile 'newfile
   'set lon 'lonmin' 'lonmax
   'sety'
   'setz'
   'getinfo  tdim'
             tdim = result
   'set t 1 'tdim
    if( name1 = name2 ) 
   'define   Var'name1'p'tag' = q1pq2p'
   'seasonal Var'name1'p'tag
   'undefine Var'name1'p'tag
    else
   'define   Cov'name1''name2'p'tag' = q1pq2p'
   'seasonal Cov'name1''name2'p'tag
   'undefine Cov'name1''name2'p'tag
    endif
   'close 'newfile
endif

   'set dfile 'infile
   'set lat 'latmin' 'latmax
   'set lon 'lonmin' 'lonmax
   'set t   '  tmin' '  tmax

*'q define'
*   say 'Defined variables: 'result
return

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
