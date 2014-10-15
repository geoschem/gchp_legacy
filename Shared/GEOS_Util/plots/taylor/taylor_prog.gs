function taylor_prog (args)

EXPORT   = subwrd(args,1)
GC       = subwrd(args,2)
level    = subwrd(args,3)

expid    = subwrd(args,4)
output   = subwrd(args,5)
debug    = subwrd(args,6)
 

* Initialize
* ----------
'reinit'
'set display color white'
'set csmooth on'
'c'

'run getenv "GEOSUTIL"'     ; geosutil     = result
'run getenv "VERIFICATION"' ; verification = result


* Determine Variable Name and Data File
* -------------------------------------
'run getvar 'EXPORT' 'GC
         qname  = subwrd(result,1)
         qfile  = subwrd(result,2)
         scale  = subwrd(result,3)
         expdsc = subwrd(result,4)


* Return if EXPORT not found
* --------------------------
if( qfile = "NULL" )
    return
endif


* Model Experiment Data
* ---------------------
'set dfile 'qfile
'set lev 'level
'getinfo  level'
          modlev = result

'setlons'
'getinfo lonmin'
         lonmin = result
'getinfo lonmax'
         lonmax = result
'set lat -90 90'


* Create Environment Variables for Seasonal Utility
* -------------------------------------------------
'setdates'
'sett'
          'alias ' qname
           alias = result
'chckname 'alias

'define mod = 'alias'*'scale

if( EXPORT = "ZLE"  ) 
   'define mod = mod-ave(mod,lon='lonmin',lon='lonmax',-b)'
endif

'q dims'
 say 'Model Environment:'
 say result
'seasonal mod'


* Loop over Possible Verification Datasets for Comparison
* -------------------------------------------------------

' getnumrc 'geosutil'/plots/taylor'
     rcinfo = result
     numrc  = subwrd( rcinfo,1 )
       num  = 1
       cnt  = 0
while( num <= numrc )
        loc = num + 1
     rcfile = subwrd( rcinfo,loc )
              OBS = EXPORT
     'run getobs 'OBS' 'GC' 'rcfile
               oname = subwrd(result,1)
             obsfile = subwrd(result,2)
              oscale = subwrd(result,3)
              obsdsc = subwrd(result,4)
              obsnam = subwrd(result,5)

* Check for valid OBS
* -------------------
if( oname != 'NULL' )
            cnt =  cnt + 1
           'set dfile 'obsfile
           'set lev 'level

           'getdates'
            begdateo = subwrd(result,1)
            enddateo = subwrd(result,2)

           'run setenv   "BEGDATEO" 'begdateo
           'run setenv   "ENDDATEO" 'enddateo

                                  'define obs'cnt' = 'oname'*'oscale
           if( EXPORT = "ZLE" ) ; 'define obs'cnt' = (obs'cnt'-ave(obs'cnt',lon='lonmin',lon='lonmax',-b))' ; endif

           'q dims'
            say 'OBS'cnt'  Environment:'
            say result
           'seasonal obs'cnt

* End check for valid OBS
* -----------------------
endif

* Check next Verification Dataset
* -------------------------------
num = num + 1
endwhile


* Average Verification Datasets for TAYLOR plot
* ---------------------------------------------
   'define obsdjf = regrid2( obs1djf,1,1,bs_p1,'lonmin',-90)'
   'define obsjja = regrid2( obs1jja,1,1,bs_p1,'lonmin',-90)'
   'define obsmam = regrid2( obs1mam,1,1,bs_p1,'lonmin',-90)'
   'define obsson = regrid2( obs1son,1,1,bs_p1,'lonmin',-90)'
   'define obsann = regrid2( obs1ann,1,1,bs_p1,'lonmin',-90)'

       num  = 2
while( num <= cnt )
   'define tmpdjf = regrid2( obs'num'djf,1,1,bs_p1,'lonmin',-90)'
   'define tmpjja = regrid2( obs'num'jja,1,1,bs_p1,'lonmin',-90)'
   'define tmpmam = regrid2( obs'num'mam,1,1,bs_p1,'lonmin',-90)'
   'define tmpson = regrid2( obs'num'son,1,1,bs_p1,'lonmin',-90)'
   'define tmpann = regrid2( obs'num'ann,1,1,bs_p1,'lonmin',-90)'

   'define obsdjf = obsdjf + tmpdjf'
   'define obsjja = obsjja + tmpjja'
   'define obsmam = obsmam + tmpmam'
   'define obsson = obsson + tmpson'
   'define obsann = obsann + tmpann'

    num = num + 1
endwhile
   'define obsdjf = obsdjf / 'cnt
   'define obsjja = obsjja / 'cnt
   'define obsmam = obsmam / 'cnt
   'define obsson = obsson / 'cnt
   'define obsann = obsann / 'cnt

say 'Taylor Averaging Counter = 'cnt


* Perform Taylor Plots
* --------------------
   'set dfile 'qfile
   'set clab off'
   'taylor moddjf obsdjf djf 'expid
   'taylor modjja obsjja jja 'expid
   'taylor modson obsson son 'expid
   'taylor modmam obsmam mam 'expid
   'taylor modann obsann ann 'expid

                       NAME = EXPORT"_"level
if( EXPORT = "Q"   ) ; NAME = "QV_"level ; endif
if( EXPORT = "ZLE" ) ; NAME = "HE_"level ; endif
if( EXPORT = "SLP" ) ; NAME = "SLP"      ; endif

   'taylor_write 'expid'  'NAME' 'output
   'taylor_read   GFDL    'NAME' 'verification
   'taylor_read   ncep    'NAME' 'verification
   'taylor_read   era40   'NAME' 'verification
   'taylor_read   CAM3    'NAME' 'verification
   'taylor_read   e0203   'NAME' 'verification
   'taylor_read   merrasc 'NAME' 'verification

   "taylor_plt 7  ncep era40 merrasc CAM3 GFDL e0203 "expid" "output" "NAME" '"NAME" vs NCEP/ERA40 Re-Analysis' "debug

return
