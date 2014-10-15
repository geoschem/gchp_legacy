function hflux (args)

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
'run getvar SH SURFACE'
        qname.1 = subwrd(result,1)
        qfile.1 = subwrd(result,2)
        scale.1 = subwrd(result,3)
        expdsc  = subwrd(result,4)
    if( qname.1 = 'NULL' ) ; return ; endif

'run getvar LWI SURFACE'
        qname.2 = subwrd(result,1)
        qfile.2 = subwrd(result,2)
        scale.2 = subwrd(result,3)



* Ensure NAMES have no underscores
* --------------------------------
      num=2
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


* Land/Water Masks
* ----------------
if( qname.2 = "NULL" )
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
   'define   omask = maskout( 1 , -abs('alias.2'-'omask') )'
   'define   lmask = maskout( 1 , -abs('alias.2'-'lmask') )'

   'seasonal omask'
   'seasonal lmask'
   'set t 1'
   'define   omaskmod = omask'season
   'define   lmaskmod = lmask'season
endif

* Verification Datasets
* ---------------------
'   open 'verification'/GSSTF_Surf_Flux/GSSTF.Jul1987_Dec2000_Clim.01.ctl'
'getinfo  numfiles'
          gsstfile = result
'   open 'verification'/surface_fields/coads/coadsgrads2.tabl'
'getinfo  numfiles'
          coadfile = result

'set dfile 'gsstfile
'set z 1'
'getdates'
'define   hfluxo1 = hflux'
'seasonal hfluxo1'

'set dfile 'coadfile
'set z 1'
'getdates'
'define   hfluxo2 = sensib3'
'seasonal hfluxo2'


* Model Data Sets
* ---------------
'set dfile 'qfile.1
'set z 1'
'sett'
if( qname.1 != alias.1 ) ; 'rename 'qname.1 ' 'alias.1 ; endif
'define   hflux = 'alias.1'*'scale.1
'seasonal hflux'


* Perform Taylor Plots
* --------------------
'set dfile 'qfile.1
'getinfo lon'
         lon = result
'define obs1 = regrid2(hfluxo1djf,1,1,bs_p1,'lon',-90)'
'define obs2 = regrid2(hfluxo2djf,1,1,bs_p1,'lon',-90)'
'define hfluxdjfo = (obs1+obs2)/2'
'define obs1 = regrid2(hfluxo1jja,1,1,bs_p1,'lon',-90)'
'define obs2 = regrid2(hfluxo2jja,1,1,bs_p1,'lon',-90)'
'define hfluxjjao = (obs1+obs2)/2'
'define obs1 = regrid2(hfluxo1mam,1,1,bs_p1,'lon',-90)'
'define obs2 = regrid2(hfluxo2mam,1,1,bs_p1,'lon',-90)'
'define hfluxmamo = (obs1+obs2)/2'
'define obs1 = regrid2(hfluxo1son,1,1,bs_p1,'lon',-90)'
'define obs2 = regrid2(hfluxo2son,1,1,bs_p1,'lon',-90)'
'define hfluxsono = (obs1+obs2)/2'
'define obs1 = regrid2(hfluxo1ann,1,1,bs_p1,'lon',-90)'
'define obs2 = regrid2(hfluxo2ann,1,1,bs_p1,'lon',-90)'
'define hfluxanno = (obs1+obs2)/2'
                                                                                                                   
'taylor hfluxdjf hfluxdjfo djf 'expid
'taylor hfluxjja hfluxjjao jja 'expid
'taylor hfluxson hfluxsono son 'expid
'taylor hfluxmam hfluxmamo mam 'expid
'taylor hfluxann hfluxanno ann 'expid

'taylor_write 'expid' HFLUX 'output
'taylor_read   GFDL   HFLUX 'verification
'taylor_read   CAM3   HFLUX 'verification
'taylor_read   e0203  HFLUX 'verification
                                                                                                   
"taylor_plt 4 CAM3 GFDL e0203 "expid" "output" HFLUX 'Sensible Heat Flux vs GSSTF/COADS' "debug

endif


