function statmak (args)
field  = subwrd  (args,1)
tag    = subwrd  (args,2)
ctl    = subwrd  (args,3)

say ' '
say 'Inside STATMAK, field: 'field
say '                  tag: 'tag
say '                  ctl: 'ctl
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

'q file'
say 'Default File: 'result
'q dims'
say '        DIMS: 'result


* Define Number of Forecast Days and Time Interval (hrs)
* ------------------------------------------------------
'run getinfo tinc'
             tinc = result
         if( tinc = "NULL" ) ; tinc = 6 ; endif

'run getenv "SYSCMP_TDIM"'
                    tdim  = result

             tdum = result - 1
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

* Define FMA variables
* --------------------
say 'Defining FMA variables for Field: 'field' and tag: 'tag
say '--------------------------------- '
 
'define 'field'fma'tag' = lat-lat+lon-lon'
        n  = 1
while ( n <= numfiles )

* Note: Add and Subtract uf & vf to force similar UNDEF locations
* ---------------------------------------------------------------
   if( field = chi )
       'define  uaa'n' = ua.'n'+uf.'n'-uf.'n
       'define  vaa'n' = va.'n'+vf.'n'-vf.'n
       'define chif'n' = fish_chi(uf.'n',vf.'n')'
       'define chia'n' = fish_chi(uaa'n',vaa'n')'
       'define chif'n' = chif'n'-aave(chif'n',g)'
       'define chia'n' = chia'n'-aave(chia'n',g)'
   endif
   if( field = psi )
       'define  uaa'n' = ua.'n'+uf.'n'-uf.'n
       'define  vaa'n' = va.'n'+vf.'n'-vf.'n
       'define psif'n' = fish_psi(uf.'n',vf.'n')'
       'define psia'n' = fish_psi(uaa'n',vaa'n')'
       'define psif'n' = psif'n'-aave(psif'n',g)'
       'define psia'n' = psia'n'-aave(psia'n',g)'
   endif
   if( field  = chi | field  = psi )
       'define f = 'field'f'n
       'define a = 'field'a'n
   else
       'define f = 'field'f.'n
       'define a = 'field'a.'n
   endif

  'define 'field'fma'tag''n'  = (f-a)*'scale
  'define 'field'Xbia'tag''n' =      'field'fma'tag''n
  'define 'field'Xmse'tag''n' = pow( 'field'fma'tag''n',2 )'
  'define 'field'fma'tag' = 'field'fma'tag' + 'field'fma'tag''n

   n = n + 1
endwhile
  'define 'field'fma'tag' = 'field'fma'tag' / 'numfiles

        n  = 1
while ( n <= numfiles )
  'define 'field'Xvar'tag''n' = pow( 'field'fma'tag''n'-'field'fma'tag',2 )'
   n = n + 1
endwhile

* Define Zonal-Mean FMA variables
* -------------------------------
if( zfreq = 'varying' )
    say 'Defining Zonal-Mean FMA variables for Field: 'field' and tag: 'tag
    say '-------------------------------------------- '

   'define 'field'fmaz = lat-lat'
    n = 1
    while ( n <= numfiles )
      'makez  'field'fma'tag''n' z'
      'set lon 0'
      'define 'field'fmaz = 'field'fmaz + 'field'fma'tag''n' z'
      'setlons'
    n = n + 1
    endwhile
   'set lon 0'
   'define 'field'fma'tag'z = 'field'fmaz /'numfiles
   'setlons'

    n = 1
    while ( n <= numfiles )
      'makez  'field'Xbia'tag''n' z'
      'makez  'field'Xmse'tag''n' z'
      'makez  'field'Xvar'tag''n' z'
    n = n + 1
    endwhile
else
    say 'Skipping: Defining Zonal-Mean FMA variables for Field: 'field' and tag: 'tag
    say '------------------------------------------------------ '
endif

* Compute TAG vs CTL Variables: X, Y, and Z
* -----------------------------------------
if( tag != ctl )
    say 'Computing X,Y,Z Mean Variables for Field: 'field' and TAGs: 'tag' and 'ctl
    say '----------------------------------------- '

  'run getenv "ZMINFILE" '
               zminfile = result
  'open '      zminfile
  'getinfo     numfiles'
               newfile = result
  'set dfile ' newfile
   if( zfreq = 'varying' )
  'set lev 1000 100'
   endif

  'define 'field'Xbia'tag' = lat-lat+lon-lon'
  'define 'field'Ybia'tag' = lat-lat+lon-lon'
  'define 'field'Zbia'tag' = lat-lat+lon-lon'
  'define 'field'Xmse'tag' = lat-lat+lon-lon'
  'define 'field'Ymse'tag' = lat-lat+lon-lon'
  'define 'field'Zmse'tag' = lat-lat+lon-lon'
  'define 'field'Xvar'tag' = lat-lat+lon-lon'
  'define 'field'Yvar'tag' = lat-lat+lon-lon'
  'define 'field'Zvar'tag' = lat-lat+lon-lon'

* Compute Means of Z-Variables
* ----------------------------
   n = 1
   while ( n <= numfiles )
     'define 'field'Zbia'tag''n' = 'field'Xbia'tag''n'-'field'Xbia'ctl''n
     'define 'field'Zmse'tag''n' = 'field'Xmse'tag''n'-'field'Xmse'ctl''n
     'define 'field'Zvar'tag''n' = 'field'Xvar'tag''n'-'field'Xvar'ctl''n

     'define 'field'Xbia'tag'    = 'field'Xbia'tag' + 'field'Xbia'tag''n
     'define 'field'Ybia'tag'    = 'field'Ybia'tag' + 'field'Xbia'ctl''n
     'define 'field'Zbia'tag'    = 'field'Zbia'tag' + 'field'Zbia'tag''n
     'define 'field'Xmse'tag'    = 'field'Xmse'tag' + 'field'Xmse'tag''n
     'define 'field'Ymse'tag'    = 'field'Ymse'tag' + 'field'Xmse'ctl''n
     'define 'field'Zmse'tag'    = 'field'Zmse'tag' + 'field'Zmse'tag''n
     'define 'field'Xvar'tag'    = 'field'Xvar'tag' + 'field'Xvar'tag''n
     'define 'field'Yvar'tag'    = 'field'Yvar'tag' + 'field'Xvar'ctl''n
     'define 'field'Zvar'tag'    = 'field'Zvar'tag' + 'field'Zvar'tag''n
   n = n + 1
   endwhile

  'define 'field'Xbia'tag' = 'field'Xbia'tag' / 'numfiles
  'define 'field'Ybia'tag' = 'field'Ybia'tag' / 'numfiles
  'define 'field'Zbia'tag' = 'field'Zbia'tag' / 'numfiles
  'define 'field'Xmse'tag' = 'field'Xmse'tag' / 'numfiles
  'define 'field'Ymse'tag' = 'field'Ymse'tag' / 'numfiles
  'define 'field'Zmse'tag' = 'field'Zmse'tag' / 'numfiles
  'define 'field'Xvar'tag' = 'field'Xvar'tag' / 'numfiles
  'define 'field'Yvar'tag' = 'field'Yvar'tag' / 'numfiles
  'define 'field'Zvar'tag' = 'field'Zvar'tag' / 'numfiles

* Compute Variances of Z-Variables
* --------------------------------
   say 'Computing X,Y,Z Variance Variables for Field: 'field' and TAGs: 'tag' and 'ctl
   say '-------------------------------------------- '
  'define 'field'bia'tag' = lat-lat+lon-lon'
  'define 'field'mse'tag' = lat-lat+lon-lon'
  'define 'field'var'tag' = lat-lat+lon-lon'
   n = 1
   while ( n <= numfiles )
     'define 'field'bia'tag' = 'field'bia'tag' + pow( 'field'Zbia'tag''n'-'field'Zbia'tag',2 )'
     'define 'field'mse'tag' = 'field'mse'tag' + pow( 'field'Zmse'tag''n'-'field'Zmse'tag',2 )'
     'define 'field'var'tag' = 'field'var'tag' + pow( 'field'Zvar'tag''n'-'field'Zvar'tag',2 )'
   n = n + 1
   endwhile
  'define 'field'bia'tag' = 'field'bia'tag'/'numfiles
  'define 'field'mse'tag' = 'field'mse'tag'/'numfiles
  'define 'field'var'tag' = 'field'var'tag'/'numfiles

  'define 'field'dbia'tag' =  abs('field'Xbia'tag')- abs('field'Ybia'tag')'
  'define 'field'drms'tag' = sqrt('field'Xmse'tag')-sqrt('field'Ymse'tag')'
  'define 'field'dstd'tag' = sqrt('field'Xvar'tag')-sqrt('field'Yvar'tag')'

if( zfreq = 'varying' )
  'set lon 0'
  'define 'field'Xbia'tag'z = lat-lat'
  'define 'field'Ybia'tag'z = lat-lat'
  'define 'field'Zbia'tag'z = lat-lat'
  'define 'field'Xmse'tag'z = lat-lat'
  'define 'field'Ymse'tag'z = lat-lat'
  'define 'field'Zmse'tag'z = lat-lat'
  'define 'field'Xvar'tag'z = lat-lat'
  'define 'field'Yvar'tag'z = lat-lat'
  'define 'field'Zvar'tag'z = lat-lat'

* Compute Means of Z-Variables
* ----------------------------
   say 'Computing Zonal Mean of X,Y,Z Variables for Field: 'field' and TAGs: 'tag' and 'ctl
   say '-------------------------------------------------- '
   n = 1
   while ( n <= numfiles )
     'setlons'
     'makez  'field'Zbia'tag''n' z'
     'makez  'field'Zmse'tag''n' z'
     'makez  'field'Zvar'tag''n' z'

     'set lon 0'
     'define 'field'Xbia'tag'z    = 'field'Xbia'tag'z + 'field'Xbia'tag''n'z'
     'define 'field'Ybia'tag'z    = 'field'Ybia'tag'z + 'field'Xbia'ctl''n'z'
     'define 'field'Zbia'tag'z    = 'field'Zbia'tag'z + 'field'Zbia'tag''n'z'
     'define 'field'Xmse'tag'z    = 'field'Xmse'tag'z + 'field'Xmse'tag''n'z'
     'define 'field'Ymse'tag'z    = 'field'Ymse'tag'z + 'field'Xmse'ctl''n'z'
     'define 'field'Zmse'tag'z    = 'field'Zmse'tag'z + 'field'Zmse'tag''n'z'
     'define 'field'Xvar'tag'z    = 'field'Xvar'tag'z + 'field'Xvar'tag''n'z'
     'define 'field'Yvar'tag'z    = 'field'Yvar'tag'z + 'field'Xvar'ctl''n'z'
     'define 'field'Zvar'tag'z    = 'field'Zvar'tag'z + 'field'Zvar'tag''n'z'
   n = n + 1
   endwhile

  'define 'field'Xbia'tag'z = 'field'Xbia'tag'z / 'numfiles
  'define 'field'Ybia'tag'z = 'field'Ybia'tag'z / 'numfiles
  'define 'field'Zbia'tag'z = 'field'Zbia'tag'z / 'numfiles
  'define 'field'Xmse'tag'z = 'field'Xmse'tag'z / 'numfiles
  'define 'field'Ymse'tag'z = 'field'Ymse'tag'z / 'numfiles
  'define 'field'Zmse'tag'z = 'field'Zmse'tag'z / 'numfiles
  'define 'field'Xvar'tag'z = 'field'Xvar'tag'z / 'numfiles
  'define 'field'Yvar'tag'z = 'field'Yvar'tag'z / 'numfiles
  'define 'field'Zvar'tag'z = 'field'Zvar'tag'z / 'numfiles

* Compute Variances of Z-Variables
* --------------------------------
   say 'Computing Variance of Zonal Mean of X,Y,Z Variance Variables for Field: 'field' and TAGs: 'tag' and 'ctl
   say '----------------------------------------------------------------------- '
  'define 'field'bia'tag'z = lat-lat'
  'define 'field'mse'tag'z = lat-lat'
  'define 'field'var'tag'z = lat-lat'
   n = 1
   while ( n <= numfiles )
     'define 'field'bia'tag'z = 'field'bia'tag'z + pow( 'field'Zbia'tag''n'z-'field'Zbia'tag'z,2 )'
     'define 'field'mse'tag'z = 'field'mse'tag'z + pow( 'field'Zmse'tag''n'z-'field'Zmse'tag'z,2 )'
     'define 'field'var'tag'z = 'field'var'tag'z + pow( 'field'Zvar'tag''n'z-'field'Zvar'tag'z,2 )'
   n = n + 1
   endwhile
  'define 'field'bia'tag'z = 'field'bia'tag'z/'numfiles
  'define 'field'mse'tag'z = 'field'mse'tag'z/'numfiles
  'define 'field'var'tag'z = 'field'var'tag'z/'numfiles

  'define 'field'dbia'tag'z =  abs('field'Xbia'tag'z)- abs('field'Ybia'tag'z)'
  'define 'field'drms'tag'z = sqrt('field'Xmse'tag'z)-sqrt('field'Ymse'tag'z)'
  'define 'field'dstd'tag'z = sqrt('field'Xvar'tag'z)-sqrt('field'Yvar'tag'z)'

* End Z-Freq Varying Test
* -----------------------
endif


* End CTL Test
* ------------
'close 'newfile
endif


'set dfile 1'
'set t 'tbeg' 'tdim
'setlons'
'sety'
'set z 'zmin' 'zmax

return
