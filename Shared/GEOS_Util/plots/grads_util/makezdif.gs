function makezdif (args)

'numargs  'args
 numargs = result

       abs = FALSE
      name = 'q'
      ptop =  0
       num =  0
while( num < numargs )
       num = num + 1
if( subwrd(args,num)='-q1'    ) ; q1     = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-q2'    ) ; q2     = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-file1' ) ; file1  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-file2' ) ; file2  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-name'  ) ; name   = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-ptop'  ) ; ptop   = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-abs'   ) ; abs    = TRUE               ; endif
endwhile

'run getenv "GEOSUTIL"'
             geosutil = result

* Get Current Environment Settings
* --------------------------------
'getinfo file'
        dfile = result

'q gxout'
   gxout = sublin(result,4)
   gxout = subwrd(gxout ,6)

'getinfo zfreq'
         zfreq = result
     if( zfreq = 'varying' )
         'getinfo zmin'
                  zmin = result
         'getinfo zmax'
                  zmax = result
     endif
     if( zfreq = 'fixed' )
         'getinfo zpos'
                  zmin = result
                  zmax = result
     endif

'getinfo tfreq'
         tfreq = result
     if( tfreq = 'varying' )
        'getinfo tmin'
                 tmin = result
        'getinfo tmax'
                 tmax = result
     endif
     if( tfreq = 'fixed' )
        'getinfo time'
                 tmin = result
                 tmax = result
     endif

'getinfo lonmin'
         lonbeg = result
'getinfo lonmax'
         lonend = result
'getinfo latmin'
         latbeg = result
'getinfo latmax'
         latend = result

* Create Array of Target Pressure Levels
* --------------------------------------
'set dfile 'file1
'getinfo zdim'
         zdim1 = result

'set dfile 'file2
'getinfo zdim'
         zdim2 = result
'set z  'zdim2
        'getinfo level'
                 level = result
if( level > ptop ) ; ptop = level ; endif

levs = ''
nlev = 0
       z = 1
while( z<=zdim2 )
      'set z 'z
      'getinfo level'
               level   = result
               level.z = result
           if( level  >= ptop )
                levs    = levs % level.z % " "
                nlev    = nlev + 1
           endif
       z = z + 1
endwhile

say ''
say 'Number of Target Pressure Levels: 'nlev
say '                 Pressure Levels: 'levs
say ''


* Create Temporary File at 0.25x0.25 degree resolution with consistent levels
* ---------------------------------------------------------------------------
'setlons'
'getinfo lon'
         lon = result
if( lon < 0 )
   'set lon   -180 179.75'
        lon = -180
else
   'set lon   0 359.75'
        lon = 0
endif
'set lat -90 90'

'!remove 'name'.data'
 z = 1
while( z<=nlev )
       say 'Checking for: 'level.z
       say '------------- '
      'set dfile 'file2
      'set lev 'level.z
      'define qtmp = 'q2' + lon-lon'
      'define qobs = regrid2( qtmp,0.25,0.25,bs_p1,0,-90)'
    'undefine qtmp'
       if( z=1 )
          'set gxout stat'
          'd 'q2
           undef = sublin(result,6)
           undef = subwrd(undef,4)
           say 'UNDEF = 'undef
          'set gxout fwrite'
          'set fwrite 'name'.data'
       endif


      'set dfile 'file1
       k = 1
      'set z 'k
      'getinfo level'
               level = result
       while(  level > level.z & k < zdim1 )
               k = k + 1
              'set z 'k
              'getinfo level'
                       level = result
       endwhile

       if( level = level.z )
             say 'Using Level  :'k' ('level')'
             say '-----------  '
            'define qtmp = 'q1' + lon-lon'
            'define qmod = regrid2( qtmp,0.25,0.25,bs_p1,0,-90)'
          'undefine qtmp'
                  if( abs = TRUE )
            '         d abs(qmod-qobs)'
                  else
            '         d qmod-qobs'
                  endif
       else
            'define qk = 'q1' + lon-lon'
             if( k > 1 )
                 km1 = k-1
                'set z 'km1
                'getinfo level'
                         levm1 = result
                'define   qkm1 = 'q1' + lon-lon'
                'define   qint = qkm1 + (qk-qkm1)*( log('level.z'/'levm1') / log('level'/'levm1') )'
                 say 'Using Levels :'k' ('level') and 'km1' ('levm1')'
                 say '------------ '
             else
                 kp1 = k+1
                'set z 'kp1
                'getinfo level'
                         levp1 = result
                'define   qkp1 = 'q1' + lon-lon'
                'define   qint = qkp1 + (qk-qkp1)*( log('level.z'/'levp1') / log('level'/'levp1') )'
                 say 'Using Levels :'k' ('level') and 'kp1' ('levp1')'
                 say '------------ '
             endif
            'define qmod = regrid2( qint,0.25,0.25,bs_p1,0,-90)'
                  if( abs = TRUE )
            '         d abs(qmod-qobs)'
                  else
            '         d qmod-qobs'
                  endif
       endif
             say ' '
z = z + 1
endwhile
'disable fwrite'

'!remove sedfile'
'!remove 'name'.ctl'
'!echo "s@GRADSDATA@"'name'.data@g > sedfile'
'!echo "s@UNDEF@"'undef'@g        >> sedfile'
'!echo "s@ZDIM2@"'nlev'@g         >> sedfile'
'!echo "s@LEVS@"'levs'@g          >> sedfile'
'!sed -f  sedfile 'geosutil'/plots/grads_util/zdiff.template > 'name'.ctl'

'open 'name'.ctl'
'getinfo    numfiles'
            newfile = result
'set dfile 'newfile
'set t 1'
'setx'
'sety'
'setlons'
'setlats'
'setz'
'makez q z'
'define 'name'z = qz'

maxval = -1e15
minval =  1e15
'set x 1'
 z = 1
 while( z <= nlev )
'set z 'z
'minmax.simple qz'

 qmax = subwrd(result,1)
 qmin = subwrd(result,2)
 qave = subwrd(result,3)
 qrms = subwrd(result,4)
 qstd = subwrd(result,5)
 qfac = 2

*facmax = (qmax-qave)/qstd
*facmin = (qave-qmin)/qstd
*say 'z: 'z'  facmax: 'facmax'  facmin: 'facmin

 if( qmax > qave + qfac*qstd )
     qmax = qave + qfac*qstd
 endif
 if( qmin < qave - qfac*qstd )
     qmin = qave - qfac*qstd
 endif
 
 if( qmax > maxval ) ; maxval = qmax ; endif
 if( qmin < minval ) ; minval = qmin ; endif
 z = z + 1
 endwhile
 say ' '

*'close 'newfile
'!remove ZDIFILE.txt'
'run setenv "ZDIFILE" 'newfile

* Reset Initial Environment Settings
* ----------------------------------
'set gxout 'gxout
'set dfile 'dfile
'set lon 'lonbeg' 'lonend
'set lat 'latbeg' 'latend
'set z 'zmin' 'zmax
'set t 'tmin' 'tmax

return maxval' 'minval
