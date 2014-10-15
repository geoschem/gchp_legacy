function setmask (args)

tag   = subwrd(args,1)
dfile = subwrd(args,2)
if(     dfile = "" )
'getinfo file'
        dfile = result
endif


* Get z dimension
* ---------------
'getinfo zfreq'
zfreq = result
if( zfreq = 'varying' )
'getinfo zmin'
z1 = result
'getinfo zmax'
z2 = result
endif
if( zfreq = 'fixed' )
'getinfo zpos'
z1 = result
z2 = result
endif

* Get t dimension
* ---------------
'getinfo tfreq'
tfreq = result
if( tfreq = 'varying' )
'getinfo tmin'
t1 = result
'getinfo tmax'
t2 = result
endif
if( tfreq = 'fixed' )
'getinfo time'
t1 = result
t2 = result
endif


* Get Dimension of Environment
* ----------------------------
'getinfo lonmin'
         lonbeg = result
'getinfo lonmax'
         lonend = result
'getinfo latmin'
         latbeg = result
'getinfo latmax'
         latend = result

'getinfo dlon'
         dlon = result
'getinfo dlat'
         dlat = result

* Determine Dateline
* ------------------
'set dfile 'dfile
'set x 1'
'getinfo lonmin'
 lonmin = result
    dateline = de
if( lonmin = -180 )
    dateline = dc
endif
if( lonmin =    0 )
    dateline = dc
endif

'getinfo xdim'
   xdim = result
'getinfo ydim'
   ydim = result

if( xdim < 100 )
    xdim = '0'xdim
endif
if( ydim < 100 )
    ydim = '0'ydim
endif


* Get Dimension of DFILE Based on Environment
* -------------------------------------------
'set lon 'lonbeg
'getinfo  lon'
          lonb  = result
'set lon 'lonend
'getinfo  lon'
          lone  = result

'set lat 'latbeg
'getinfo  lat'
          latb  = result
'set lat 'latend
'getinfo  lat'
          late  = result

while( lone > 360 )
       lone = lone - dlon
       lonb = lonb - dlon
endwhile
while( lonb < 0 )
       lonb = lonb + dlon
endwhile
while( late > 90  )
       late = late - dlat
endwhile
while( latb < -90 )
       latb = latb + dlat
endwhile


say ' '
say 'Creating Land/Water Mask for File: 'dfile
say '---------------------------------- '
say '                DLON = 'dlon
say '                DLAT = 'dlat
say '         Env. Lonbeg = 'lonbeg
say '         Env. Lonend = 'lonend
say '         Env. Latbeg = 'latbeg
say '         Env. Latend = 'latend
say '         Setting lon 'lonb' 'lone
say '         Setting lat 'latb' 'late

'run getenv "VERIFICATION"'
             verification = result

    dateline = dc
   'open 'verification'/lwmask/lwmask_1080721_'dateline'.tabl'
   'getinfo numfiles'
               file = result
   'set dfile 'file
   'set lon 'lonb' 'lone
   'set lat 'latb' 'late

   'set t 1'
   'set z 1'
   'define lwmask'tag' = regrid2( lwmask,'dlon','dlat',bs_p1,'lonb','latb' )'

   'set dfile 'dfile
   'set lon 'lonbeg' 'lonend
   'set lat 'latbeg' 'latend
   'set z 'z1' 'z2
   'set t 't1' 't2
   'close 'file

return
