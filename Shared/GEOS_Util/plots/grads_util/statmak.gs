function statmak (args)
field  = subwrd  (args,1)
tag    = subwrd  (args,2)

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
             tdum = result - 1
             ndaymax = tdum * tinc / 24
                nmax = 1 + ndaymax*(24/tinc)

'run getenv "NDAY"'
             nday = result
         if( nday = "NULL" ) ; nday = ndaymax ; endif
         if( nday > ndaymax) ; nday = ndaymax ; endif

'getinfo tdim'
         tdim = result
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
'define 'field'fmean = lat-lat+lon-lon'
'define 'field'amean = lat-lat+lon-lon'
'define 'field'cmean = lat-lat+lon-lon'
'define 'field'fma   = lat-lat+lon-lon'
'define 'field'fmc   = lat-lat+lon-lon'
'define 'field'mes   = lat-lat+lon-lon'
'define 'field'mse   = lat-lat+lon-lon'
'define 'field'rms   = lat-lat+lon-lon'
'define 'field'std   = lat-lat+lon-lon'
'define 'field'var   = lat-lat+lon-lon'

n = 1
while ( n <= numfiles )
say 'Processing Field: 'field' for File: 'n

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
       'define 'field'fmean = 'field'fmean +     'field'f'n
       'define 'field'amean = 'field'amean +     'field'a'n
       'define 'field'cmean = 'field'cmean +     'field'c'n
       'define 'field'fma   = 'field'fma   +     'field'f'n'-'field'a'n
       'define 'field'fmc   = 'field'fmc   +     'field'f'n'-'field'c'n
       'define 'field'mse   = 'field'mse   + pow('field'f'n'-'field'a'n',2)'
   endif
   if( field != chi & field != psi )
       'define 'field'fmean = 'field'fmean +     'field'f.'n
       'define 'field'amean = 'field'amean +     'field'a.'n
       'define 'field'cmean = 'field'cmean +     'field'c.'n
       'define 'field'fma   = 'field'fma   +     'field'f.'n'-'field'a.'n
       'define 'field'fmc   = 'field'fmc   +     'field'f.'n'-'field'c.'n
       'define 'field'mse   = 'field'mse   + pow('field'f.'n'-'field'a.'n',2)'
   endif

n = n + 1
endwhile

'define 'field'fmean'tag' = 'field'fmean/'numfiles
'define 'field'amean'tag' = 'field'amean/'numfiles
'define 'field'cmean'tag' = 'field'cmean/'numfiles
'define 'field'fma'tag'   = 'field'fma  /'numfiles
'define 'field'fmc'tag'   = 'field'fmc  /'numfiles
'define 'field'mse'tag'   = 'field'mse  /'numfiles

n = 1
while ( n <= numfiles )
'define 'field'mes = 'field'mes + pow('field'fmean'tag'-'field'amean'tag',2)'
n = n + 1
endwhile

'define 'field'mes'tag'  =      'field'mes/'numfiles
'define 'field'rmes'tag' = sqrt('field'mes'tag')'
'define 'field'rms'tag'  = sqrt('field'mse'tag')'
'define 'field'var'tag'  =      'field'mse'tag'-'field'mes'tag
'define 'field'std'tag'  = sqrt('field'mse'tag'-'field'mes'tag')'

if( zfreq = 'varying' )
'makez 'field'fmean'tag' z'
'makez 'field'amean'tag' z'
'makez 'field'cmean'tag' z'
'makez 'field'fma'tag'   z'
'makez 'field'fmc'tag'   z'
'makez 'field'mes'tag'   z'
'makez 'field'mse'tag'   z'
'define 'field'rmes'tag'z = sqrt('field'mes'tag'z)'
'define 'field'rms'tag'z  = sqrt('field'mse'tag'z)'
'define 'field'var'tag'z  =      'field'mse'tag'z-'field'mes'tag'z'
'define 'field'std'tag'z  = sqrt('field'mse'tag'z-'field'mes'tag'z)'
endif

return
