function makef (args)

name  = subwrd(args,1)
alias = subwrd(args,2)

* Variable is in File
* -------------------
    say 'Computing Data for Function: 'name
    say '                       Name: 'alias

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

  '!remove sedfile'
  '!touch  sedfile'
  '!remove 'alias'.ctl'
  '!remove 'alias'.data'
   'set gxout fwrite'
   'set       fwrite 'alias'.data'

    t = tmin
    while( t<=tmax )
    say '          Computing Data for Variable: 'alias'  for t = 't
   'set t 't
   'sety'
   'set z 'zmin' 'zmax
   'set x 1 'xdim
*  'set lon 'lonmin' 'lonmax
   'd 'name
    t = t+1
    endwhile
    say ''
   'disable fwrite'
   'set gxout 'gxout

'!echo "s?q.data?"'alias'.data?g >> sedfile'
'!echo "s?UNDEF?"'undef'?g >> sedfile'
'!echo "s?DX?"'dlon'?g >> sedfile'
'!echo "s?DY?"'dlat'?g >> sedfile'
'!echo "s?DT?"'dt'?g >> sedfile'
'!echo "s?LON0?"'lon0'?g >> sedfile'
'!echo "s?LAT0?"'lat0'?g >> sedfile'
'!echo "s?XDIM?"'xdim'?g >> sedfile'
'!echo "s?YDIM?"'ydim'?g >> sedfile'
'!echo "s?ZDIM?"'zdim'?g >> sedfile'
'!echo "s?TDIM?"'tdim'?g >> sedfile'
'!echo "s?BDATE?"'begdate'?g >> sedfile'
'!echo "s?LEVS?"'levs'?g  >> sedfile'
'!/bin/cp 'geosutil'/plots/grads_util/makef.tmpl .'
'!sed -f   sedfile makef.tmpl > 'alias'.ctl'

   'open 'alias'.ctl'
   'getinfo    numfiles'
               newfile = result
   'set dfile 'newfile
   'set lon 'lonmin' 'lonmax
   'sety'
   'setz'
   'getinfo  tdim'
             tdim = result
   'set t 1 'tdim
   'define 'alias' = qdata'
   'close 'newfile

   'set dfile 'infile
   'set lat 'latmin' 'latmax
   'set lon 'lonmin' 'lonmax
   'set t   '  tmin' '  tmax

return
