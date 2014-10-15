function eflux (args)

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
'run getvar LHFX SURFACE'
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
'define   efluxo1 = eflux'
'seasonal efluxo1'

'set dfile 'coadfile
'set z 1'
'getdates'
'define   efluxo2 = latent3'
'seasonal efluxo2'


* Model Data Sets
* ---------------
'set dfile 'qfile.1
'set z 1'
'sett'
if( qname.1 != alias.1 ) ; 'rename 'qname.1 ' 'alias.1 ; endif
'define   eflux = 'alias.1'*'scale.1
'seasonal eflux'


* Perform Taylor Plots
* --------------------
'set dfile 'qfile.1
'getinfo lon'
         lon = result
'define obs1 = regrid2(efluxo1djf,1,1,bs_p1,'lon',-90)'
'define obs2 = regrid2(efluxo2djf,1,1,bs_p1,'lon',-90)'
'define efluxdjfo = (obs1+obs2)/2'
'define obs1 = regrid2(efluxo1jja,1,1,bs_p1,'lon',-90)'
'define obs2 = regrid2(efluxo2jja,1,1,bs_p1,'lon',-90)'
'define efluxjjao = (obs1+obs2)/2'
'define obs1 = regrid2(efluxo1mam,1,1,bs_p1,'lon',-90)'
'define obs2 = regrid2(efluxo2mam,1,1,bs_p1,'lon',-90)'
'define efluxmamo = (obs1+obs2)/2'
'define obs1 = regrid2(efluxo1son,1,1,bs_p1,'lon',-90)'
'define obs2 = regrid2(efluxo2son,1,1,bs_p1,'lon',-90)'
'define efluxsono = (obs1+obs2)/2'
'define obs1 = regrid2(efluxo1ann,1,1,bs_p1,'lon',-90)'
'define obs2 = regrid2(efluxo2ann,1,1,bs_p1,'lon',-90)'
'define efluxanno = (obs1+obs2)/2'

'taylor efluxdjf efluxdjfo djf 'expid
'taylor efluxjja efluxjjao jja 'expid
'taylor efluxson efluxsono son 'expid
'taylor efluxmam efluxmamo mam 'expid
'taylor efluxann efluxanno ann 'expid

'taylor_write 'expid' EFLUX 'output
'taylor_read   GFDL   EFLUX 'verification
'taylor_read   CAM3   EFLUX 'verification
'taylor_read   e0203  EFLUX 'verification
                                                                                                   
"taylor_plt 4 CAM3 GFDL e0203 "expid" "output" EFLUX 'Latent Heat Flux vs GSSTF/COADS' "debug

endif
