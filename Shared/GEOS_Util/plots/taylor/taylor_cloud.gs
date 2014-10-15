function cloud (args)

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
'run getvar CLDTT SOLAR'
        qname.1 = subwrd(result,1)
        qfile.1 = subwrd(result,2)
        scale.1 = subwrd(result,3)
        expdsc  = subwrd(result,4)
'run getvar CLDLO SOLAR'
        qname.2 = subwrd(result,1)
        qfile.2 = subwrd(result,2)
        scale.2 = subwrd(result,3)
'run getvar CLDMD SOLAR'
        qname.3 = subwrd(result,1)
        qfile.3 = subwrd(result,2)
        scale.3 = subwrd(result,3)
'run getvar CLDHI SOLAR'
        qname.4 = subwrd(result,1)
        qfile.4 = subwrd(result,2)
        scale.4 = subwrd(result,3)

'run getvar LWI SURFACE'
        qname.5 = subwrd(result,1)
        qfile.5 = subwrd(result,2)
        scale.5 = subwrd(result,3)

if( qname.1 = "NULL" ) ; return ; endif
if( qname.2 = "NULL" ) ; return ; endif
if( qname.3 = "NULL" ) ; return ; endif
if( qname.4 = "NULL" ) ; return ; endif

* Ensure NAMES have no underscores
* --------------------------------
      num=5
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
lw = 5
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
          coadfile = result
'   open 'verification'/Clouds_radiation/isccp/isccp.tabl'
'getinfo  numfiles'
          isccpfile = result

'set dfile 'coadfile
'setmask coads'

'set dfile 'isccpfile
'setmask isccp'

'set dfile 'coadfile
'set z 1'
'getdates'
'seasonal cloud  coads'

'set dfile 'isccpfile
'set z 1'
'getdates'
'seasonal cldtot isccp'
'seasonal cldlow isccp'
'seasonal cldmid isccp'
'seasonal cldhi  isccp'


* Model Data Set
* --------------
'set dfile 'qfile.1
'set z 1'
'sett'
if( qname.1 != alias.1 ) ; 'rename 'qname.1 ' 'alias.1 ; endif
'define   cldtt = 'alias.1'*'scale.1
'seasonal cldtt    mod'
'set dfile 'qfile.2
'set z 1'
'sett'
if( qname.2 != alias.2 ) ; 'rename 'qname.2 ' 'alias.2 ; endif
'define   cldlo = 'alias.2'*'scale.2
'seasonal cldlo    mod'
'set dfile 'qfile.3
'set z 1'
'sett'
if( qname.3 != alias.3 ) ; 'rename 'qname.3 ' 'alias.3 ; endif
'define   cldmd = 'alias.3'*'scale.3
'seasonal cldmd    mod'
'set dfile 'qfile.4
'set z 1'
'sett'
if( qname.4 != alias.4 ) ; 'rename 'qname.4 ' 'alias.4 ; endif
'define   cldhi = 'alias.4'*'scale.4
'seasonal cldhi    mod'


* Perform Taylor Plots
* --------------------
'set dfile 'qfile.1

'taylor cldttdjfmod clouddjfcoads djf 'expid
'taylor cldttjjamod cloudjjacoads jja 'expid
'taylor cldttsonmod cloudsoncoads son 'expid
'taylor cldttmammod cloudmamcoads mam 'expid
'taylor cldttannmod cloudanncoads ann 'expid
'taylor_write 'expid' CLDTT_COADS 'output
'taylor_read   GFDL   CLDTT_COADS 'verification
'taylor_read   CAM3   CLDTT_COADS 'verification
'taylor_read   e0203  CLDTT_COADS 'verification
"taylor_plt  4 CAM3   GFDL e0203 "expid" "output" CLDTT_COADS 'Total Clouds vs COADS' "debug

'taylor cldttdjfmod cldtotdjfisccp*0.01 djf 'expid
'taylor cldttjjamod cldtotjjaisccp*0.01 jja 'expid
'taylor cldttsonmod cldtotsonisccp*0.01 son 'expid
'taylor cldttmammod cldtotmamisccp*0.01 mam 'expid
'taylor cldttannmod cldtotannisccp*0.01 ann 'expid
'taylor_write 'expid' CLDTT_ISCCP 'output
'taylor_read   GFDL   CLDTT_ISCCP 'verification
'taylor_read   CAM3   CLDTT_ISCCP 'verification
'taylor_read   e0203  CLDTT_ISCCP 'verification
"taylor_plt  4 CAM3   GFDL e0203 "expid" "output" CLDTT_ISCCP 'Total Clouds vs ISCCP' "debug

'taylor cldlodjfmod cldlowdjfisccp*0.01 djf 'expid
'taylor cldlojjamod cldlowjjaisccp*0.01 jja 'expid
'taylor cldlosonmod cldlowsonisccp*0.01 son 'expid
'taylor cldlomammod cldlowmamisccp*0.01 mam 'expid
'taylor cldloannmod cldlowannisccp*0.01 ann 'expid
'taylor_write 'expid' CLDLO_ISCCP 'output
'taylor_read   GFDL   CLDLO_ISCCP 'verification
'taylor_read   CAM3   CLDLO_ISCCP 'verification
'taylor_read   e0203  CLDLO_ISCCP 'verification
"taylor_plt  4 CAM3   GFDL e0203 "expid" "output" CLDLO_ISCCP 'Low-Level Clouds vs ISCCP' "debug

'taylor cldmddjfmod cldmiddjfisccp*0.01 djf 'expid
'taylor cldmdjjamod cldmidjjaisccp*0.01 jja 'expid
'taylor cldmdsonmod cldmidsonisccp*0.01 son 'expid
'taylor cldmdmammod cldmidmamisccp*0.01 mam 'expid
'taylor cldmdannmod cldmidannisccp*0.01 ann 'expid
'taylor_write 'expid' CLDMD_ISCCP 'output
'taylor_read   GFDL   CLDMD_ISCCP 'verification
'taylor_read   CAM3   CLDMD_ISCCP 'verification
'taylor_read   e0203  CLDMD_ISCCP 'verification
"taylor_plt  4 CAM3   GFDL e0203 "expid" "output" CLDMD_ISCCP 'Mid-Level Clouds vs ISCCP' "debug

'taylor cldhidjfmod cldhidjfisccp*0.01 djf 'expid
'taylor cldhijjamod cldhijjaisccp*0.01 jja 'expid
'taylor cldhisonmod cldhisonisccp*0.01 son 'expid
'taylor cldhimammod cldhimamisccp*0.01 mam 'expid
'taylor cldhiannmod cldhiannisccp*0.01 ann 'expid
'taylor_write 'expid' CLDHI_ISCCP 'output
'taylor_read   GFDL   CLDHI_ISCCP 'verification
'taylor_read   CAM3   CLDHI_ISCCP 'verification
'taylor_read   e0203  CLDHI_ISCCP 'verification
"taylor_plt  4 CAM3   GFDL e0203 "expid" "output" CLDHI_ISCCP 'High-Level Clouds vs ISCCP' "debug

endif
