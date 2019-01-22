function statmak (args)
field  = subwrd  (args,1)
tag    = subwrd  (args,2)

say ' '
say 'Inside STATMAK_SYS, field: 'field
say '                      tag: 'tag
say ' '

if( field = q )
    scale = 1000
else
    scale = 1
endif

'numargs  'args
 numargs = result

* Query Level Environment
* -----------------------
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

'run getenv "GEOSUTIL"'
             geosutil = result

'getinfo numfiles'
         numfiles = result

'open 'geosutil'/plots/grads_util/lwmask1440721.tabl'
'getinfo numfiles'
         newfile = result

'set dfile 'newfile
'set t 1'
'set z 1'
'define mask = lwmask'
'close 'newfile

'set dfile 1'
'set t 1'
'setlons'
'sety'
'set z 'zmin' 'zmax


* Define Number of Forecast Days and Time Interval (hrs)
* ------------------------------------------------------
'run getinfo tinc'
             tinc = result
         if( tinc = "NULL" ) ; tinc = 6 ; endif

'run getinfo tdim'
             tdim = result
             tdum = tdim - 1
             ndaymax = tdum * tinc / 24
                nmax = 1 + ndaymax*(24/tinc)

'run getenv "NDAY"'
             nday = result
         if( nday = "NULL" ) ; nday = ndaymax ; endif
         if( nday > ndaymax) ; nday = ndaymax ; endif

         tbeg = 1-(nmax-tdim)
'set t ' tbeg ' 'tdim

* Compute forecast statistics
* ---------------------------
*  fma: forecast minus analysis
*  fmc: forecast minus climatology
*  mes: mean error squared
*  mse: mean square error
*  rms: root mean square error
*  std: standard deviation
* --------------------------------------------------

'define 'field'fm  = lat-lat+lon-lon'
'define 'field'am  = lat-lat+lon-lon'
'define 'field'cm  = lat-lat+lon-lon'
'define 'field'fma = lat-lat+lon-lon'
'define 'field'fmc = lat-lat+lon-lon'
'define 'field'mes = lat-lat+lon-lon'
'define 'field'mse = lat-lat+lon-lon'
'define 'field'rms = lat-lat+lon-lon'
'define 'field'std = lat-lat+lon-lon'
'define 'field'var = lat-lat+lon-lon'

'define 'field'varf = lat-lat+lon-lon'
'define 'field'vara = lat-lat+lon-lon'
'define 'field'stdf = lat-lat+lon-lon'
'define 'field'stda = lat-lat+lon-lon'
'define 'field'cov  = lat-lat+lon-lon'
'define 'field'rnd  = lat-lat+lon-lon'

n = 1
while ( n <= numfiles )
say 'Processing Field: 'field' for File: 'n'  Tag: 'tag

* Note: Add and Subtract uf & vf to force similar UNDEF locations
* ---------------------------------------------------------------
   if( field = chi )
       'define  uaa'n' = ua.'n'+uf.'n'-uf.'n
       'define  vaa'n' = va.'n'+vf.'n'-vf.'n
       'define chif'n' = fish_chi(uf.'n',vf.'n')'
       'define chia'n' = fish_chi(uaa'n',vaa'n')'
       'define chic'n' = fish_chi(uc.'n',vc.'n')'
       'define chif'n' = chif'n'-aave(chif'n',g)'
       'define chia'n' = chia'n'-aave(chia'n',g)'
       'define chic'n' = chic'n'-aave(chic'n',g)'
   endif
   if( field = psi )
       'define  uaa'n' = ua.'n'+uf.'n'-uf.'n
       'define  vaa'n' = va.'n'+vf.'n'-vf.'n
       'define psif'n' = fish_psi(uf.'n',vf.'n')'
       'define psia'n' = fish_psi(uaa'n',vaa'n')'
       'define psic'n' = fish_psi(uc.'n',vc.'n')'
       'define psif'n' = psif'n'-aave(psif'n',g)'
       'define psia'n' = psia'n'-aave(psia'n',g)'
       'define psic'n' = psic'n'-aave(psic'n',g)'
   endif

   if( field  = chi | field  = psi )
       'define 'field'fm  = 'field'fm  +     'field'f'n
       'define 'field'am  = 'field'am  +     'field'a'n
       'define 'field'cm  = 'field'cm  +     'field'c'n
       'define 'field'fma = 'field'fma +     'field'f'n'-'field'a'n
       'define 'field'fmc = 'field'fmc +     'field'f'n'-'field'c'n
       'define 'field'mse = 'field'mse + pow('field'f'n'-'field'a'n',2)'
   endif
   if( field != chi & field != psi )
       'define 'field'fs  = 'field'f.'n'*'scale
       'define 'field'as  = 'field'a.'n'*'scale
       'define 'field'cs  = 'field'c.'n'*'scale

       'define 'field'fm  = 'field'fm  +     'field'fs'
       'define 'field'am  = 'field'am  +     'field'as'
       'define 'field'cm  = 'field'cm  +     'field'cs'
       'define 'field'fma = 'field'fma +     'field'fs-'field'as'
       'define 'field'fmc = 'field'fmc +     'field'fs-'field'cs'
       'define 'field'mse = 'field'mse + pow('field'fs-'field'as,2)'
   endif

n = n + 1
endwhile

'define 'field'fm'tag'  = 'field'fm /'numfiles
'define 'field'am'tag'  = 'field'am /'numfiles
'define 'field'cm'tag'  = 'field'cm /'numfiles
'define 'field'fma'tag' = 'field'fma/'numfiles
'define 'field'fmc'tag' = 'field'fmc/'numfiles
'define 'field'mse'tag' = 'field'mse/'numfiles

n = 1
while ( n <= numfiles )
'define 'field'mes  = 'field'mes  + pow('field'fm'tag'-'field'am'tag',2)'
'define 'field'varf = 'field'varf + pow('field'f.'n'*'scale'-'field'fm'tag',2)'
'define 'field'vara = 'field'vara + pow('field'a.'n'*'scale'-'field'am'tag',2)'
'define 'field'cov  = 'field'cov  +    ('field'f.'n'*'scale'-'field'fm'tag') * ('field'a.'n'*'scale'-'field'am'tag')'
'define 'field'rnd  = 'field'rnd  + pow( ('field'f.'n'*'scale'-'field'fm'tag') - ('field'a.'n'*'scale'-'field'am'tag') , 2)'
n = n + 1
endwhile

'define 'field'cov'tag'   =     'field'cov /'numfiles
'define 'field'rnd'tag'   =     'field'rnd /'numfiles
'define 'field'varf'tag'  =     'field'varf/'numfiles
'define 'field'vara'tag'  =     'field'vara/'numfiles
'define 'field'stdf'tag'  = sqrt('field'varf'tag')'
'define 'field'stda'tag'  = sqrt('field'vara'tag')'

'define 'field'ampl'tag'  = pow( 'field'stdf'tag'-'field'stda'tag',2 ) + pow( 'field'fm'tag'-'field'am'tag',2 )' 
'define 'field'phaz'tag'  =  2*( 'field'stdf'tag'*'field'stda'tag' - 'field'cov'tag')' 
'define 'field'ramp'tag'  = sqrt( 'field'ampl'tag' )'
'define 'field'rphz'tag'  = sqrt( 'field'phaz'tag' )'
'define 'field'rrnd'tag'  = sqrt( 'field'rnd'tag' )'

'define 'field'mes'tag'  =      'field'mes/'numfiles
'define 'field'rmes'tag' = sqrt('field'mes'tag')'
'define 'field'rms'tag'  = sqrt('field'mse'tag')'
'define 'field'var'tag'  =      'field'mse'tag'-'field'mes'tag
'define 'field'std'tag'  = sqrt('field'mse'tag'-'field'mes'tag')'

if( zfreq = 'varying' )
'makez 'field'fm'tag'  z'
'makez 'field'am'tag'  z'
'makez 'field'cm'tag'  z'
'makez 'field'fma'tag' z'
'makez 'field'fmc'tag' z'
'makez 'field'mes'tag' z'
'makez 'field'mse'tag' z'
'makez 'field'ampl'tag' z'
'makez 'field'phaz'tag' z'

'define 'field'rmes'tag'z = sqrt('field'mes'tag'z)'
'define 'field'ramp'tag'z = sqrt('field'ampl'tag'z)'
'define 'field'rphz'tag'z = sqrt('field'phaz'tag'z)'
'define 'field'rms'tag'z  = sqrt('field'mse'tag'z)'
'define 'field'var'tag'z  =      'field'mse'tag'z-'field'mes'tag'z'
'define 'field'std'tag'z  = sqrt('field'mse'tag'z-'field'mes'tag'z)'
endif

return
