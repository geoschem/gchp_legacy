function statmak (args)
field  = subwrd  (args,1)
tag    = subwrd  (args,2)
ctl    = subwrd  (args,3)

'numargs  'args
 numargs = result

say ' '
say 'Inside STATMAK, field: 'field
say '                  tag: 'tag
say '                  ctl: 'ctl
say ' '

'run getenv "GEOSUTIL"'
             geosutil = result

if( field = q )
    scale = 1000
else
    scale = 1
endif

* Get Total Number of Files for Subset Experiments
* ------------------------------------------------------------------------
'getinfo numfiles'
         numfiles = result

* Query Level Environment
* -----------------------
'getinfo zfreq'
         zfreq = result

if( zfreq = 'fixed' )
   'getinfo level'
            levmin = result
            levmax = result

   'open 'geosutil'/plots/grads_util/lwmask1440721.tabl'
   'getinfo numfiles'
            newfile = result

   'set dfile 'newfile
   'set t 1'
   'set z 1'
   'define mask = lwmask'
   'close 'newfile
   'set dfile 1'
endif

* Set Proper DFILE for Varying Levels
* -----------------------------------
if( zfreq = 'varying' )
  'getinfo zmin'
           zmin = result
  'getinfo zmax'
           zmax = result
  'set z ' zmin
  'getinfo level'
           levmin = result
  'set z ' zmax
  'getinfo level'
           levmax = result
endif

* Get Proper File for TINC or ZMIN
* --------------------------------
'run getenv "TINCFILE" '
             tincfile = result
'open '      tincfile
'getinfo     numfiles'
             newfile = result
'set dfile ' newfile
'set t 1'
'setlons'
'sety'
'set lev 'levmin' 'levmax

if( zfreq = 'varying' )
   'close 'newfile
   'run getenv "ZMINFILE" '
                zminfile = result
   'open '      zminfile
   'getinfo     numfiles'
                newfile = result
   'set dfile ' newfile
   'set lev 1000 100'
endif


* Define Number of Forecast Days and Time Interval (hrs)
* ------------------------------------------------------

'run getenv "SYSCMP_TINC"'
                    tinc  = result
'run getenv "SYSCMP_TDIM"'
                    tdim  = result
                    tdum  = tdim - 1
                    ndaymax = tdum * tinc / 24
                       nmax = 1 + ndaymax*(24/tinc)

'run getenv "NDAY"'
             nday = result
         if( nday = "NULL" ) ; nday = ndaymax ; endif
         if( nday > ndaymax) ; nday = ndaymax ; endif

         tbeg = 1-(nmax-tdim)
'set t ' tbeg ' 'tdim

'q file'
say 'Default File: 'result
'q dims'
say 'Default File DIMS: 'result


********************************************************************************
****                                                                        ****
**** Note:  Forecast           =>  F                                        ****
****        Analysis           =>  A                                        ****
****                                                                        ****
****        Mean Square Error  =>  MSE  =   1/N * SUM[ (F-A)**2 ]           ****
****        Mean Error         =>  BIAS =   1/N * SUM[ (F-A) ]              ****
****        Mean Error Squared =>  MES  =   BIAS**2                         ****
****        Root Mean  Square  =>  RMS  = SQRT[ MSE ]                       ****
****        Variance           =>  VAR  =   1/N * SUM[ (F-A-BIAS)**2 ]      ****
****        Standard Deviation =>  STD  = SQRT[ VAR ]                       ****
****                                                                        ****
****        F Mean             =>  FBAR =   1/N * SUM[  F  ]                ****
****        A Mean             =>  ABAR =   1/N * SUM[  A  ]                ****
****        F Variance         =>  FVAR =   1/N * SUM[ (F-FBAR)**2 ]        ****
****        A Variance         =>  AVAR =   1/N * SUM[ (A-ABAR)**2 ]        ****
****        CoVariance         =>  COV  =   1/N * SUM[ (F-FBAR)*(A-ABAR) ]  ****
****        Amplitude Error    =>  AMP  = [ FSTD - ASTD ]**2                ****
****                                    + [ FBAR - ABAR ]**2                ****
****        Phase     Error    =>  PHZ  = 2*[ FSTD*ASTD - COV ]             ****
****                                                                        ****
****        Mean Square Error  =   BIAS**2         + Variance               ****
****                           =   Amplitude Error + Phase Error            ****
****                      MSE  =   VAR + MES                                ****
****                           =   AMP + PHZ                                ****
****                                                                        ****
********************************************************************************

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

  'define 'field'f'tag''n'    =   f  *'scale
  'define 'field'a'tag''n'    =   a  *'scale
  'define 'field'fma'tag''n'  = (f-a)*'scale

  'define 'field'Xmse'tag''n' = pow( 'field'fma'tag''n',2 )'

   n = n + 1
endwhile


* Compute MeanErrorSquared MES and MeanSquareError MSE for f, a, and fma
* ----------------------------------------------------------------------
*               fbar  =  1/N * SUM[   F      ]
*               abar  =  1/N * SUM[   A      ]
*               fma1  =  1/N * SUM[ (F-A)    ]
*               fma2  =  1/N * SUM[ (F-A)**2 ]
* ----------------------------------------------------------------------
       'define 'field'fma1'tag' = lat-lat+lon-lon'
       'define 'field'fma2'tag' = lat-lat+lon-lon'
       'define 'field'fbar'tag' = lat-lat+lon-lon'
       'define 'field'abar'tag' = lat-lat+lon-lon'
        n  = 1
while ( n <= numfiles )
       'define 'field'fbar'tag' = 'field'fbar'tag' +      'field'f'tag''n
       'define 'field'abar'tag' = 'field'abar'tag' +      'field'a'tag''n
       'define 'field'fma1'tag' = 'field'fma1'tag' +      'field'fma'tag''n
       'define 'field'fma2'tag' = 'field'fma2'tag' + pow( 'field'fma'tag''n',2 )'
        n = n + 1
endwhile
       'define 'field'fbar'tag' = 'field'fbar'tag' / 'numfiles
       'define 'field'abar'tag' = 'field'abar'tag' / 'numfiles
       'define 'field'fma1'tag' = 'field'fma1'tag' / 'numfiles
       'define 'field'fma2'tag' = 'field'fma2'tag' / 'numfiles

* ----------------------------------------------------------------------
*               Xmes  =  { 1/N * SUM[ (F-A)    ] }**2  =  [ fbar-abar ]**2
*               Xmse  =  { 1/N * SUM[ (F-A)**2 ] }
* ----------------------------------------------------------------------
       'define 'field'Xmes'tag' = pow( 'field'fma1'tag',2 )'
       'define 'field'Xmse'tag' =      'field'fma2'tag


* Compute Variance VAR for f, a, and fma
* --------------------------------------
*               fvar  =  1/N * SUM[ (F-FBAR)**2 ]
*               avar  =  1/N * SUM[ (A-ABAR)**2 ]
*               cvar  =  1/N * SUM[ (F-FBAR)*(A-ABAR ]
*               Xvar  =  1/N * SUM[ ( (F-A)-(FBAR-ABAR) )**2 ] 
*               fstd  =  SQRT[ fvar ]
*               astd  =  SQRT[ avar ]
* ----------------------------------------------------------------------
        n  = 1
while ( n <= numfiles )
       'define 'field'fvar'tag''n' =  pow( 'field'f'tag''n'  -'field'fbar'tag',2 )'
       'define 'field'avar'tag''n' =  pow( 'field'a'tag''n'  -'field'abar'tag',2 )'
       'define 'field'cvar'tag''n' =     ( 'field'f'tag''n'  -'field'fbar'tag') * ( 'field'a'tag''n'  -'field'abar'tag') '
       'define 'field'Xvar'tag''n' =  pow( 'field'fma'tag''n'-'field'fma1'tag',2 )'
       'define 'field'fstd'tag''n' = sqrt( 'field'fvar'tag''n' )'
       'define 'field'astd'tag''n' = sqrt( 'field'avar'tag''n' )'
       'define 'field'ampl'tag''n' =  pow( 'field'fstd'tag''n'-'field'astd'tag''n',2 ) + pow( 'field'fbar'tag'-'field'abar'tag',2 )'
       'define 'field'phaz'tag''n' =   2*( 'field'fstd'tag''n'*'field'astd'tag''n' - 'field'cvar'tag''n' )'
        n = n + 1
endwhile

       'define 'field'fvar'tag' = lat-lat+lon-lon'
       'define 'field'avar'tag' = lat-lat+lon-lon'
       'define 'field'cvar'tag' = lat-lat+lon-lon'
       'define 'field'Xvar'tag' = lat-lat+lon-lon'
        n  = 1
while ( n <= numfiles )
       'define 'field'fvar'tag' = 'field'fvar'tag' + 'field'fvar'tag''n
       'define 'field'avar'tag' = 'field'avar'tag' + 'field'avar'tag''n
       'define 'field'cvar'tag' = 'field'cvar'tag' + 'field'cvar'tag''n
       'define 'field'Xvar'tag' = 'field'Xvar'tag' + 'field'Xvar'tag''n
        n = n + 1
endwhile
       'define 'field'fvar'tag' = 'field'fvar'tag' / 'numfiles
       'define 'field'avar'tag' = 'field'avar'tag' / 'numfiles
       'define 'field'cvar'tag' = 'field'cvar'tag' / 'numfiles
       'define 'field'Xvar'tag' = 'field'Xvar'tag' / 'numfiles

       'define 'field'fstd'tag' = sqrt( 'field'fvar'tag' )'
       'define 'field'astd'tag' = sqrt( 'field'avar'tag' )'

       'define 'field'ampl'tag' = pow( 'field'fstd'tag'-'field'astd'tag',2 ) + pow( 'field'fbar'tag'-'field'abar'tag',2 )'
       'define 'field'phaz'tag' =  2*( 'field'fstd'tag'*'field'astd'tag' - 'field'cvar'tag' )'

* --------------------------------------------------------------
* --------------------------------------------------------------

* Define Zonal-Mean F, A, and FMA variables
* -----------------------------------------
if( zfreq = 'varying' )
    say 'Defining Zonal-Mean FMA variables for Field: 'field' and tag: 'tag
    say '-------------------------------------------- '

   'makez 'field'Xmes'tag' z'
   'makez 'field'Xmse'tag' z'
   'makez 'field'fvar'tag' z'
   'makez 'field'avar'tag' z'
   'makez 'field'cvar'tag' z'
   'makez 'field'Xvar'tag' z'
   'makez 'field'ampl'tag' z'
   'makez 'field'phaz'tag' z'

else
    say 'Skipping: Defining Zonal-Mean FMA variables for Field: 'field' and tag: 'tag
    say '------------------------------------------------------ '
endif

* --------------------------------------------------------------

* Compute TAG & CTL Variables: X & Y, and Diff Variable: Z = X-Y
* For: mean-error-squared(mes), mean-square-error(mse), variance(var)
* -------------------------------------------------------------------
if( tag != ctl )
    say 'Computing Difference Variables for Field: 'field' and TAGs: 'tag' and 'ctl
    say '----------------------------------------- '

* Define Difference-Variables
* ---------------------------
* RMS:    Dmse  =   mse_tag - mse_ctl   =  { 1/N * SUM[ (F-A)   **2      ] }_tag
*                                       -  { 1/N * SUM[ (F-A)   **2      ] }_ctl
* BIAS:   Dmes  =   mes_tag - mes_ctl   =  { 1/N * SUM[ (F-A)            ] }_tag **2
*                                       -  { 1/N * SUM[ (F-A)            ] }_ctl **2
* VAR:    Dvar  =  Xvar_tag - Xvar_ctl  =  { 1/N * SUM[ ( (F-A)-(FBAR-ABAR) )**2 ] }_tag
*                                       -  { 1/N * SUM[ ( (F-A)-(FBAR-ABAR) )**2 ] }_ctl
* AMP:    Damp  =  ampl_tag - ampl_ctl
* PHZ:    Dphz  =  phaz_tag - phaz_ctl
* ----------------------------------------------------------------------
           n  = 1
   while ( n <= numfiles )
          'define 'field'Dmse'tag''n' = 'field'Xmse'tag''n' - 'field'Xmse'ctl''n
          'define 'field'Dfvr'tag''n' = 'field'fvar'tag''n' - 'field'fvar'ctl''n
          'define 'field'Davr'tag''n' = 'field'avar'tag''n' - 'field'avar'ctl''n
          'define 'field'Dcvr'tag''n' = 'field'cvar'tag''n' - 'field'cvar'ctl''n
          'define 'field'Dvar'tag''n' = 'field'Xvar'tag''n' - 'field'Xvar'ctl''n
          'define 'field'Dmes'tag''n' = 'field'Xmes'tag'    - 'field'Xmes'ctl
          'define 'field'Damp'tag''n' = 'field'ampl'tag''n' - 'field'ampl'ctl''n
          'define 'field'Dphz'tag''n' = 'field'phaz'tag''n' - 'field'phaz'ctl''n
           n = n + 1
   endwhile

* Define D-Variable Means
* ----------------------
          'define 'field'Dmse'tag' = lat-lat+lon-lon'
          'define 'field'Dfvr'tag' = lat-lat+lon-lon'
          'define 'field'Davr'tag' = lat-lat+lon-lon'
          'define 'field'Dcvr'tag' = lat-lat+lon-lon'
          'define 'field'Dvar'tag' = lat-lat+lon-lon'
          'define 'field'Dmes'tag' = lat-lat+lon-lon'
          'define 'field'Damp'tag' = lat-lat+lon-lon'
          'define 'field'Dphz'tag' = lat-lat+lon-lon'
           n  = 1
   while ( n <= numfiles )
          'define 'field'Dmse'tag' = 'field'Dmse'tag' + 'field'Dmse'tag''n
          'define 'field'Dfvr'tag' = 'field'Dfvr'tag' + 'field'Dfvr'tag''n
          'define 'field'Davr'tag' = 'field'Davr'tag' + 'field'Davr'tag''n
          'define 'field'Dcvr'tag' = 'field'Dcvr'tag' + 'field'Dcvr'tag''n
          'define 'field'Dvar'tag' = 'field'Dvar'tag' + 'field'Dvar'tag''n
          'define 'field'Dmes'tag' = 'field'Dmes'tag' + 'field'Dmes'tag''n
          'define 'field'Damp'tag' = 'field'Damp'tag' + 'field'Damp'tag''n
          'define 'field'Dphz'tag' = 'field'Dphz'tag' + 'field'Dphz'tag''n
           n = n + 1
   endwhile
          'define 'field'Dmse'tag' = 'field'Dmse'tag' / 'numfiles
          'define 'field'Dfvr'tag' = 'field'Dfvr'tag' / 'numfiles
          'define 'field'Davr'tag' = 'field'Davr'tag' / 'numfiles
          'define 'field'Dcvr'tag' = 'field'Dcvr'tag' / 'numfiles
          'define 'field'Dvar'tag' = 'field'Dvar'tag' / 'numfiles
          'define 'field'Dmes'tag' = 'field'Dmes'tag' / 'numfiles
          'define 'field'Damp'tag' = 'field'Damp'tag' / 'numfiles
          'define 'field'Dphz'tag' = 'field'Dphz'tag' / 'numfiles

*         'define 'field'Damp'tag' = 'field'ampl'tag' - 'field'ampl'ctl
*         'define 'field'Dphz'tag' = 'field'phaz'tag' - 'field'phaz'ctl

* Define Variances of MSE Differences
* -----------------------------------
          'define 'field'DDmse'tag' = lat-lat+lon-lon'
           n  = 1
   while ( n <= numfiles )
          'define 'field'DDmse'tag' = 'field'DDmse'tag' + pow( 'field'Dmse'tag''n'-'field'Dmse'tag',2 )'
           n = n + 1
   endwhile
          'define 'field'DDmse'tag' = 'field'DDmse'tag' / 'numfiles

* --------------------------------------------------------------
* --------------------------------------------------------------

if( zfreq = 'varying' )
   say 'Computing Zonal Mean of Difference Variables for Field: 'field' and TAGs: 'tag' and 'ctl
   say '------------------------------------------------------ '
   'makez 'field'Dmes'tag'  z'
   'makez 'field'Dmse'tag'  z'
   'makez 'field'Dvar'tag'  z'
   'makez 'field'Damp'tag'  z'
   'makez 'field'Dphz'tag'  z'

           n  = 1
   while ( n <= numfiles )
          'makez 'field'Dmse'tag''n' z'
           n = n + 1
   endwhile

          'set lon 0'
          'define 'field'DDmse'tag'z = lat-lat'
           n  = 1
   while ( n <= numfiles )
          'define 'field'DDmse'tag'z = 'field'DDmse'tag'z + pow( 'field'Dmse'tag''n'z-'field'Dmse'tag'z,2 )'
           n = n + 1
   endwhile
          'define 'field'DDmse'tag'z = 'field'DDmse'tag'z / 'numfiles
endif

* --------------------------------------------------------------
* --------------------------------------------------------------

* End CTL Test
* ------------
endif
'close 'newfile


'set dfile 1'
'set t 'tbeg' 'tdim
'setlons'
'sety'
'set lev 'levmin' 'levmax

return
