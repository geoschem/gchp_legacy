function tskin (args)

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
'run getvar TS SURFACE'
        qname.1 = subwrd(result,1)
        qfile.1 = subwrd(result,2)
        scale.1 = subwrd(result,3)
        expdsc  = subwrd(result,4)
    if( qfile.1 = 'NULL' ) ; return ; endif

'run getvar LWI SURFACE'
        qname.2 = subwrd(result,1)
        qfile.2 = subwrd(result,2)
        scale.2 = subwrd(result,3)

* Ensure NAME has no underscores
* ------------------------------
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

* Set BEGDATE and ENDDATE for Seasonal Calculations
* -------------------------------------------------
'setdates'

'set z 1'
'sett'
if( qname.1 != alias.1 ) ; 'rename 'qname.1 ' 'alias.1 ; endif
'define   tskinm = 'alias.1'*'scale.1
'seasonal tskinm'


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
    if( qname.2 != alias.2 ) ; 'rename 'qname.2' 'alias.2 ; endif
   'define   omask = maskout( 1 , -abs('alias.2'-'omask') )'
   'define   lmask = maskout( 1 , -abs('alias.2'-'lmask') )'

   'seasonal omask'
   'seasonal lmask'
   'set t 1'
   'define   omaskmod = omask'season
   'define   lmaskmod = lmask'season
endif


* Loop over Verification Datasets
* -------------------------------
'getnumrc 'geosutil'/plots/tskin'
    rcinfo = result
    numrc  = subwrd( rcinfo,1 )
       k  = 1
while( k <= numrc )
      loc = k + 1
   rcfile = subwrd( rcinfo,loc )

rcfile = geosutil'/plots/tskin/VERIFICATION.SRB.rc'
say 'rcfile = 'rcfile

'run getobs TS SURFACE 'rcfile
          oname1 = subwrd(result,1)
        obsfile1 = subwrd(result,2)
         oscale1 = subwrd(result,3)
         obsdsc1 = subwrd(result,4)
         obsnam1 = subwrd(result,5)
      if( oname1 = 'NULL' ) ; return ; endif

'set dfile 'obsfile1
'set z 1'
'getdates'
'define   tskino = 'oname1'*'oscale1
'seasonal tskino'

'run getobs LWI SURFACE 'rcfile
          oname2 = subwrd(result,1)
        obsfile2 = subwrd(result,2)
         oscale2 = subwrd(result,3)
         obsdsc2 = subwrd(result,4)
         obsnam2 = subwrd(result,5)
           lmask = 0
           omask = 1
      if( oname2 = 'NULL' )
         'set dfile 'obsfile1
         'set z 1'
         'setmask    obs'
         'define  omask    =  maskout( 1 , -abs(lwmaskobs-'omask') )'
         'define  lmask    =  maskout( 1 , -abs(lwmaskobs-'lmask') )'
         'define  lmaskobs = lmask'
      else
         'set dfile 'obsfile2
         'set z 1'
         'set t 1'
         'define   mask = 'oname2'*'oscale2
         'define  omask =  maskout( 1 , -abs(mask-'omask') )'
         'define  lmask =  maskout( 1 , -abs(mask-'lmask') )'
         'define  lmaskobs = lmask'
      endif

* Perform Taylor Plots
* --------------------
'set dfile 'qfile.1

'taylor tskinmdjf tskinodjf djf 'expid
'taylor tskinmjja tskinojja jja 'expid
'taylor tskinmson tskinoson son 'expid
'taylor tskinmmam tskinomam mam 'expid
'taylor tskinmann tskinoann ann 'expid

'taylor_write 'expid' TSKIN 'output
'taylor_read   GFDL   TSKIN 'verification
'taylor_read   CAM3   TSKIN 'verification
'taylor_read   e0203  TSKIN 'verification
                                                                                                   
"taylor_plt 4 CAM3 GFDL e0203 "expid" "output" TSKIN 'Skin Temperature (Over Land) vs "obsnam1"' "debug

* Check next Verification Dataset
* -------------------------------
k = k + 1
endwhile

endif
