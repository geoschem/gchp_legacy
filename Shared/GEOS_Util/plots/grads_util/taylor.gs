function taylor (args)

* Initialize Fields
* -----------------
f0  = subwrd(args,1)
r0  = subwrd(args,2)
sea = subwrd(args,3)
exp = subwrd(args,4)

'fixname 'exp
          tag = result

* Get x dimension
* ---------------
'getinfo xfreq'
xfreq = result
if( xfreq = 'varying' )
'getinfo xmin'
x1 = result
'getinfo xmax'
x2 = result
endif
if( xfreq = 'fixed' )
'getinfo xpos'
x1 = result
x2 = result
endif

* Get y dimension
* ---------------
'getinfo yfreq'
yfreq = result
if( yfreq = 'varying' )
'getinfo ymin'
y1 = result
'getinfo ymax'
y2 = result
endif
if( yfreq = 'fixed' )
'getinfo ypos'
y1 = result
y2 = result
endif


* Regrid to Uniform 1x1 and Compute Taylor Values
* -----------------------------------------------
'set lat -90 90'
'getinfo lonmin'
         lonmin = result
'getinfo lonmax'
         lonmax = result
'define    r = regrid2( 'r0',1,1,bs_p1,'lonmin',-90)'
'define    f = regrid2( 'f0',1,1,bs_p1,'lonmin',-90)'
'define    f = maskout( f,abs(r))'
'define    r = maskout( r,abs(f))'

'set lon 'lonmin
'set lat -90'
'define fmean'tag' = aave(f,lon='lonmin',lon='lonmax',lat=-90,lat=90)'
'define rmean      = aave(r,lon='lonmin',lon='lonmax',lat=-90,lat=90)'

'set lon 'lonmin' 'lonmax
'set lat -90 90'
'define msqe = (f-r)*(f-r)'
'define evar = (f-fmean'tag'-r+rmean)*(f-fmean'tag'-r+rmean)'
'define fvar = (f-fmean'tag')*(f-fmean'tag')'
'define rvar = (r-rmean)*(r-rmean)'
'define cvar = (f-fmean'tag')*(r-rmean)'

'set lon 'lonmin
'set lat -90'
'define  rms'tag' = sqrt( aave(msqe,lon='lonmin',lon='lonmax',lat=-90,lat=90) )'
'define ebar = fmean'tag'-rmean'

'define fstd'tag' = sqrt( aave(fvar,lon='lonmin',lon='lonmax',lat=-90,lat=90) )'
'define rstd      = sqrt( aave(rvar,lon='lonmin',lon='lonmax',lat=-90,lat=90) )'
'define eprm'tag' = sqrt( aave(evar,lon='lonmin',lon='lonmax',lat=-90,lat=90) )'
'define numer     =       aave(cvar,lon='lonmin',lon='lonmax',lat=-90,lat=90) )'
'define denom     =       fstd'tag'*rstd'
'define corr'tag' = exp( log(numer)-log(denom) )'

'define  std'sea''tag' = exp( log( fstd'tag' )-log( rstd ) )'
'define corr'sea''tag' = corr'tag

* Reset initial space environment
* -------------------------------
'set x 'x1' 'x2
'set y 'y1' 'y2
