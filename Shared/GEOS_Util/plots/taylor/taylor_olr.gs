function olr (args)

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
"reinit"
'set display color white'
'set clab off'
'c'

'run getenv "GEOSUTIL"'     ; geosutil     = result
'run getenv "VERIFICATION"' ; verification = result


* Get Radiation Variables
* -----------------------
'run getvar OLR IRRAD'
        qname.1 = subwrd(result,1)
        qfile.1 = subwrd(result,2)
        scale.1 = subwrd(result,3)
        expdsc  = subwrd(result,4)
'run getvar OLC IRRAD'
        qname.2 = subwrd(result,1)
        qfile.2 = subwrd(result,2)
        scale.2 = subwrd(result,3)
'run getvar OSR SOLAR'
        qname.3 = subwrd(result,1)
        qfile.3 = subwrd(result,2)
        scale.3 = subwrd(result,3)
'run getvar OSRCLR SOLAR'
        qname.4 = subwrd(result,1)
        qfile.4 = subwrd(result,2)
        scale.4 = subwrd(result,3)
'run getvar RSR SOLAR'
        qname.5 = subwrd(result,1)
        qfile.5 = subwrd(result,2)
        scale.5 = subwrd(result,3)
'run getvar RSC SOLAR'
        qname.6 = subwrd(result,1)
        qfile.6 = subwrd(result,2)
        scale.6 = subwrd(result,3)

numvar = 6
    rc = 0
     n = 1
while( n<=numvar )
if( qname.n = 'NULL' ) ; rc = 1 ; endif
 n = n + 1
endwhile
if( rc = 1 ) ; return ; endif


* Ensure NAMES have no underscores
* --------------------------------
      num=6
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


* Verification Datasets
* ---------------------
'   open 'verification'/Clouds_radiation/erbe/erbe2x25.tabl'
'getinfo  numfiles'
          erbefile = result

'set dfile 'erbefile
'set z 1'
'getdates'
'seasonal olr    obs'
'seasonal olrclr obs'
'seasonal osr    obs'
'seasonal osrclr obs'
'seasonal nsr    obs'
'seasonal nsrclr obs'

'define   swcf = nsrclr - nsr'
'seasonal swcf   obs'

'define swcfdjfobs = nsrclrdjfobs-nsrdjfobs'
'define swcfjjaobs = nsrclrjjaobs-nsrjjaobs'

* Model Data Sets
* ---------------
'set dfile 'qfile.1
'set z 1'
'setdates'
'sett'
if( qname.1 != alias.1 ) ; 'rename 'qname.1 ' 'alias.1 ; endif
'define   olr = 'alias.1'*'scale.1
'seasonal olr    mod'

'set dfile 'qfile.2
'set z 1'
'sett'
if( qname.2 != alias.2 ) ; 'rename 'qname.2 ' 'alias.2 ; endif
'define   olrclr = 'alias.2'*'scale.2
'seasonal olrclr    mod'

'set dfile 'qfile.3
'set z 1'
'sett'
if( qname.3 != alias.3 ) ; 'rename 'qname.3 ' 'alias.3 ; endif
'define   osr = 'alias.3'*'scale.3
'seasonal osr    mod'

'set dfile 'qfile.4
'set z 1'
'sett'
if( qname.4 != alias.4 ) ; 'rename 'qname.4 ' 'alias.4 ; endif
'define   osrclr = 'alias.4'*'scale.4
'seasonal osrclr    mod'

'set dfile 'qfile.5
'set z 1'
'sett'
if( qname.5 != alias.5 ) ; 'rename 'qname.5 ' 'alias.5 ; endif
'define   swndtoa = 'alias.5'*'scale.5
'seasonal swndtoa    mod'

'set dfile 'qfile.6
'set z 1'
'sett'
if( qname.6 != alias.6 ) ; 'rename 'qname.6 ' 'alias.6 ; endif
'define   swndtoac = 'alias.6'*'scale.6
'seasonal swndtoac    mod'


* The Following is a redefinition for GFDL Data
* ---------------------------------------------
if( expid = 'GFDL' )
 'define swndtoadjfmod  = swndtoadjfmod  - osrdjfmod'
 'define swndtoajjamod  = swndtoajjamod  - osrjjamod'
 'define swndtoasonmod  = swndtoasonmod  - osrsonmod'
 'define swndtoamammod  = swndtoamammod  - osrmammod'
 'define swndtoaannmod  = swndtoaannmod  - osrannmod'

 'define swndtoacdjfmod = swndtoacdjfmod - osrclrdjfmod'
 'define swndtoacjjamod = swndtoacjjamod - osrclrjjamod'
 'define swndtoacsonmod = swndtoacsonmod - osrclrsonmod'
 'define swndtoacmammod = swndtoacmammod - osrclrmammod'
 'define swndtoacannmod = swndtoacannmod - osrclrannmod'
endif
* ---------------------------------------------


* Compute Cloud Forcing
* ---------------------
'set dfile 'qfile.1
'set z 1'
'set t 1'

'define lwcfdjfmod = olrclrdjfmod-olrdjfmod'
'define lwcfjjamod = olrclrjjamod-olrjjamod'
'define lwcfsonmod = olrclrsonmod-olrsonmod'
'define lwcfmammod = olrclrmammod-olrmammod'
'define lwcfannmod = olrclrannmod-olrannmod'

'define lwcfdjfobs = olrclrdjfobs-olrdjfobs'
'define lwcfjjaobs = olrclrjjaobs-olrjjaobs'
'define lwcfsonobs = olrclrsonobs-olrsonobs'
'define lwcfmamobs = olrclrmamobs-olrmamobs'
'define lwcfannobs = olrclrannobs-olrannobs'

'define swcfdjfmod = swndtoacdjfmod-swndtoadjfmod'
'define swcfjjamod = swndtoacjjamod-swndtoajjamod'
'define swcfsonmod = swndtoacsonmod-swndtoasonmod'
'define swcfmammod = swndtoacmammod-swndtoamammod'
'define swcfannmod = swndtoacannmod-swndtoaannmod'

'define swcfdjfobs = nsrclrdjfobs-nsrdjfobs'
'define swcfjjaobs = nsrclrjjaobs-nsrjjaobs'
'define swcfsonobs = nsrclrsonobs-nsrsonobs'
'define swcfmamobs = nsrclrmamobs-nsrmamobs'
'define swcfannobs = nsrclrannobs-nsrannobs'


* Perform Taylor Plots
* --------------------
'taylor olrdjfmod olrdjfobs djf 'expid
'taylor olrjjamod olrjjaobs jja 'expid
'taylor olrsonmod olrsonobs son 'expid
'taylor olrmammod olrmamobs mam 'expid
'taylor olrannmod olrannobs ann 'expid
'taylor_write 'expid' OLR 'output
'taylor_read   GFDL   OLR 'verification
'taylor_read   CAM3   OLR 'verification
'taylor_read   e0203  OLR 'verification
"taylor_plt 4  CAM3   GFDL e0203 "expid" "output" OLR 'Outgoing Longwave Radiation vs ERBE' "debug

'taylor olrclrdjfmod olrclrdjfobs djf 'expid
'taylor olrclrjjamod olrclrjjaobs jja 'expid
'taylor olrclrsonmod olrclrsonobs son 'expid
'taylor olrclrmammod olrclrmamobs mam 'expid
'taylor olrclrannmod olrclrannobs ann 'expid
'taylor_write 'expid' OLRCLR 'output
'taylor_read   GFDL   OLRCLR 'verification
'taylor_read   CAM3   OLRCLR 'verification
'taylor_read   e0203  OLRCLR 'verification
"taylor_plt 4  CAM3   GFDL e0203 "expid" "output" OLRCLR 'Outgoing Longwave Radiation (Clear Sky) vs ERBE' "debug

'taylor lwcfdjfmod lwcfdjfobs djf 'expid
'taylor lwcfjjamod lwcfjjaobs jja 'expid
'taylor lwcfsonmod lwcfsonobs son 'expid
'taylor lwcfmammod lwcfmamobs mam 'expid
'taylor lwcfannmod lwcfannobs ann 'expid
'taylor_write 'expid' LWCF 'output
'taylor_read   GFDL   LWCF 'verification
'taylor_read   CAM3   LWCF 'verification
'taylor_read   e0203  LWCF 'verification
"taylor_plt 4  CAM3   GFDL e0203 "expid" "output" LWCF 'Longwave Radiation Cloud Forcing vs ERBE' "debug


'taylor swndtoadjfmod nsrdjfobs djf 'expid
'taylor swndtoajjamod nsrjjaobs jja 'expid
'taylor swndtoasonmod nsrsonobs son 'expid
'taylor swndtoamammod nsrmamobs mam 'expid
'taylor swndtoaannmod nsrannobs ann 'expid
'taylor_write 'expid' NSR 'output
'taylor_read   GFDL   NSR 'verification
'taylor_read   CAM3   NSR 'verification
'taylor_read   e0203  NSR 'verification
"taylor_plt 4  CAM3   GFDL e0203 "expid" "output" NSR 'Net Downward Shortave Radiation vs ERBE' "debug

'taylor swndtoacdjfmod nsrclrdjfobs djf 'expid
'taylor swndtoacjjamod nsrclrjjaobs jja 'expid
'taylor swndtoacsonmod nsrclrsonobs son 'expid
'taylor swndtoacmammod nsrclrmamobs mam 'expid
'taylor swndtoacannmod nsrclrannobs ann 'expid
'taylor_write 'expid' NSRCLR 'output
'taylor_read   GFDL   NSRCLR 'verification
'taylor_read   CAM3   NSRCLR 'verification
'taylor_read   e0203  NSRCLR 'verification
"taylor_plt 4  CAM3   GFDL e0203 "expid" "output" NSRCLR 'Net Downward Shortave Radiation (Clear Sky) vs ERBE' "debug

'taylor swcfdjfmod swcfdjfobs djf 'expid
'taylor swcfjjamod swcfjjaobs jja 'expid
'taylor swcfsonmod swcfsonobs son 'expid
'taylor swcfmammod swcfmamobs mam 'expid
'taylor swcfannmod swcfannobs ann 'expid
'taylor_write 'expid' SWCF 'output
'taylor_read   GFDL   SWCF 'verification
'taylor_read   CAM3   SWCF 'verification
'taylor_read   e0203  SWCF 'verification
"taylor_plt 4  CAM3   GFDL e0203 "expid" "output" SWCF 'Shortwave Radiation Cloud Forcing vs ERBE' "debug

endif
