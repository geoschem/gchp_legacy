function zdiff (args)

qname1   = subwrd(args,1)
qfile1   = subwrd(args,2)
qname2   = subwrd(args,3)
qfile2   = subwrd(args,4)
ptop     = subwrd(args,5)

* Create Array of Target Pressure Levels
* --------------------------------------
'set dfile 'qfile2
'getinfo zdim'
         zdim2 = result
'set z  'zdim2
        'getinfo level'
                 level   = result
if( level > ptop ) 
            ptop = level
endif

levs = ""
mlev = 0
       z = 1
while( z<=zdim2 )
      'set z 'z
      'getinfo level'
               level   = result
               level.z = result
           if( level   >= ptop )
                levs    = levs % level.z % " "
                mlev    = mlev + 1
           endif
       z = z + 1
endwhile

say ' '
say 'zdiff: qname1 = 'qname1
say 'zdiff: qfile1 = 'qfile1
say 'zdiff: qname2 = 'qname2
say 'zdiff: qfile2 = 'qfile2
say 'zdiff:   ptop = 'ptop
say ' '

* Create Temporary File at 1x1 degree resolution with consistent levels
* ---------------------------------------------------------------------
'set dfile 'qfile1
'getinfo zdim'
         zdim1 = result

'set lat -90 90'
'setlons'
'getinfo lon'
         lon = result
if( lon < 0 ) 
   'set lon   -180 179'
        lon = -180
else
   'set lon   0 359'
        lon = 0
endif

 z = 1
while( z<=mlev )
       say 'Checking for: 'level.z
      'set dfile 'qfile2 
      'set lev 'level.z
      'define qtemp2 = 'qname2' + lon-lon'
      'define   qobs = regrid2(  qtemp2 ,1,1,bs_p1,'lon',-90)'
    'undefine qtemp2'

      'set dfile 'qfile1
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
            'define qtemp1 = 'qname1' + lon-lon'
            'define   qmod = regrid2(  qtemp1 ,1,1,bs_p1,'lon',-90)'
          'undefine qtemp1'
            '     d qmod - qobs'
       else
            'define qk = 'qname1' + lon-lon'
             if( k > 1 )
                 km1 = k-1
                'set z 'km1
                'getinfo level'
                         levm1 = result
                'define   qkm1 = 'qname1' + lon-lon'
                'define   qint = qkm1 + (qk-qkm1)*( log('level.z'/'levm1') / log('level'/'levm1') )'
                 say 'Using Levels :'k' ('level') and 'km1' ('levm1')'
             else
                 kp1 = k+1
                'set z 'kp1
                'getinfo level'
                         levp1 = result
                'define   qkp1 = 'qname1' + lon-lon'
                'define   qint = qkp1 + (qk-qkp1)*( log('level.z'/'levp1') / log('level'/'levp1') )'
                 say 'Using Levels :'k' ('level') and 'kp1' ('levp1')'
             endif
            'define qmod = regrid2( qint,1,1,bs_p1,'lon',-90)'
            '     d qmod - qobs'
       endif
             say ' '
z = z + 1
endwhile
