function tpw (args)

expid    = subwrd(args,1)
output   = subwrd(args,2)
debug    = subwrd(args,3)

* Define Seasons to Process
* -------------------------
seasons  = ''
       k = 4
while( k > 0 )
    season = subwrd(args,k)
if( season = '' )
    k = -1
else
    seasons = seasons % ' ' % season
k = k+1
endif
endwhile
'uppercase 'seasons
            seasons = result

'run getenv "TAYLOR"'
         taylor = result
if(      taylor = 'true' )

* Initialize
* ----------
'reinit'
'set display color white'
'set clab off'
'c'

'run getenv "GEOSUTIL"'     ; geosutil     = result
'run getenv "VERIFICATION"' ; verification = result


* Get Radiation Variables
* -----------------------
'run getvar TQV AGCM'
        qname.1 = subwrd(result,1)
        qfile.1 = subwrd(result,2)
        scale.1 = subwrd(result,3)
        expdsc  = subwrd(result,4)

'run getvar LWI SURFACE'
        qname.2 = subwrd(result,1)
        qfile.2 = subwrd(result,2)
        scale.2 = subwrd(result,3)

if( qname.1 = "NULL" ) ; return ; endif


* Ensure NAMES have no underscores
* --------------------------------
      num=2
        m=1
while ( m<num+1 )
'fixname 'qname.m
          alias.m = result
say 'Alias #'m' = 'alias.m
        m = m+1
endwhile


* Experiment Datasets
* -------------------
'set dfile 'qfile.1
'setlons'
'set lat -90 90'
'setdates'
'sett'

* Land/Water Masks
* ----------------
lw = 2
if( qname.lw = "NULL" )
   'setmask mod'
   'define  maskmod = lwmaskmod'
   'define omaskmod = maskout( 1, maskmod-0.5 )'
   'define lmaskmod = maskout( 1, 0.5-maskmod )'
else

* Initialize Mask using Dataset Values (Note: Adjust if Needed)
* -------------------------------------------------------------
'run getenv "LMASK"' ; lmask = result
'run getenv "OMASK"' ; omask = result
'run getenv "IMASK"' ; imask = result
               if( lmask = "NULL" ) ; lmask = 1 ; endif
               if( omask = "NULL" ) ; omask = 0 ; endif
               if( imask = "NULL" ) ; imask = 2 ; endif

   'set dfile 'qfile.lw
   'set z 1'
   'sett'
    if( qname.lw != alias.lw ) ; 'rename 'qname.lw ' 'alias.lw ; endif
   'define   omask = maskout( 1 , -abs('alias.lw'-'omask') )'
   'define   lmask = maskout( 1 , -abs('alias.lw'-'lmask') )'

   'seasonal omask'
   'seasonal lmask'
   'set t 1'
   'define   omaskmod = omask'season
   'define   lmaskmod = lmask'season
endif


* Verification Datasets
* ---------------------
'   open 'verification'/Precipitation_moisture/ssmi/ssmi.tabl'
'getinfo  numfiles'
          ssmifile = result

'set dfile 'ssmifile
'setmask obs'

'set dfile 'ssmifile
'set z 1'
'getdates'
'define   tpwo = maskout( tpw,lwmaskobs-0.5 )'
'seasonal tpwo'


* Model Data Sets
* ---------------
'set dfile 'qfile.1
'set z 1'
'sett'
if( qname.1 != alias.1 ) ; 'rename 'qname.1 ' 'alias.1 ; endif
'define   tpwm = maskout( 'alias.1'*'scale.1',omaskmod )*0.1'
'seasonal tpwm'


* Perform Taylor Plots
* --------------------
'set dfile 'qfile.1
'taylor tpwmdjf tpwodjf djf 'expid
'taylor tpwmjja tpwojja jja 'expid
'taylor tpwmson tpwoson son 'expid
'taylor tpwmmam tpwomam mam 'expid
'taylor tpwmann tpwoann ann 'expid

'taylor_write 'expid' TPW 'output
'taylor_read   GFDL   TPW 'verification
'taylor_read   CAM3   TPW 'verification
                                                                                                   
"taylor_plt 3 CAM3 GFDL "expid" "output" TPW 'Total Precipitable Water vs SSMI' "debug

endif
