function uflux (args)

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
                                                                                                                            

* Get Model Variables
* -------------------
'run getvar TAUX SURFACE'
        qname.1 = subwrd(result,1)
        qfile.1 = subwrd(result,2)
        scale.1 = subwrd(result,3)
        expdsc  = subwrd(result,4)

'run getvar TAUY SURFACE'
        qname.2 = subwrd(result,1)
        qfile.2 = subwrd(result,2)
        scale.2 = subwrd(result,3)

'run getvar LWI SURFACE'
        qname.3 = subwrd(result,1)
        qfile.3 = subwrd(result,2)
        scale.3 = subwrd(result,3)

if( qname.1 = "NULL" ) ; return ; endif
if( qname.2 = "NULL" ) ; return ; endif


* Ensure NAMES have no underscores
* --------------------------------
      num=3
        m=1
while ( m<num+1 )
'fixname 'qname.m
          alias.m = result
        m = m+1
endwhile


* Experiment Datasets
* -------------------
'set dfile 'qfile.1
'setlons'
'set lat -90 90'
'setdates'
'sett'

* Land/Water Mask
* ---------------
lw = 3 
if( qname.lw = "NULL" )
   'setmask mod'
   'set t 1'
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

   'set dfile 'qfile.2
   'set z 1'
   'sett'
    if( qname.2 != alias.2 ) ; 'rename 'qname.2 ' 'alias.2 ; endif
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
'   open 'verification'/surface_fields/coads/coadsgrads2.tabl'
'getinfo  numfiles'
          obsfile = result

'set dfile 'obsfile
'set z 1'
'getdates'
'define   tauxo = taux3'
'define   tauyo = tauy3'
'seasonal tauxo'
'seasonal tauyo'


* Model Data Sets
* ---------------
'set dfile 'qfile.1
'set z 1'
'sett'
if( qname.1 != alias.1 ) ; 'rename 'qname.1 ' 'alias.1 ; endif
'define   tauxm = 'alias.1'*'scale.1
'seasonal tauxm'

'set dfile 'qfile.2
'set z 1'
'sett'
if( qname.2 != alias.2 ) ; 'rename 'qname.2 ' 'alias.2 ; endif
'define   tauym = 'alias.2'*'scale.2
'seasonal tauym'


* Perform Taylor Plots
* --------------------
'set dfile 'qfile.1

'taylor tauxmdjf tauxodjf djf 'expid
'taylor tauxmjja tauxojja jja 'expid
'taylor tauxmson tauxoson son 'expid
'taylor tauxmmam tauxomam mam 'expid
'taylor tauxmann tauxoann ann 'expid
'taylor_write 'expid' TAUX 'output
'taylor_read   GFDL   TAUX 'verification
'taylor_read   CAM3   TAUX 'verification
'taylor_read   e0203  TAUX 'verification
"taylor_plt 4  CAM3   GFDL e0203 "expid" "output" TAUX 'Eastward Surface Stress (Over Oceans) vs COADS' "debug

'taylor tauymdjf tauyodjf djf 'expid
'taylor tauymjja tauyojja jja 'expid
'taylor tauymson tauyoson son 'expid
'taylor tauymmam tauyomam mam 'expid
'taylor tauymann tauyoann ann 'expid
'taylor_write 'expid' TAUY 'output
'taylor_read   GFDL   TAUY 'verification
'taylor_read   CAM3   TAUY 'verification
'taylor_read   e0203  TAUY 'verification
"taylor_plt 4  CAM3   GFDL e0203 "expid" "output" TAUY 'Northward Surface Stress (Over Oceans) vs COADS' "debug

endif
