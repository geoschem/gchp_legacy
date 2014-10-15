function makequadz (args)

'numargs  'args
 numargs = result

       num =  0
while( num < numargs )
       num = num + 1
if( subwrd(args,num)='-U'     ) ;  uname  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-V'     ) ;  vname  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-T'     ) ;  tname  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-Q'     ) ;  qname  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-H'     ) ;  hname  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-W'     ) ;  wname  = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-UV'    ) ; uvname = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-VT'    ) ; vtname = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-VQ'    ) ; vqname = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-WT'    ) ; wtname = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-WQ'    ) ; wqname = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-TAG'   ) ; tag    = subwrd(args,num+1) ; endif
endwhile

name = 'quadz'tag

* Variable is in File
* -------------------
    say ''
    say 'Computing Quadratic Data:'
    say '-------------------------'

'getinfo file'
       infile = result

'q gxout'
   gxout = sublin(result,4)
   gxout = subwrd(gxout ,6)

'run getenv "GEOSUTIL"'
             geosutil = result

'getinfo lonmin'
         lonmin = result
'getinfo lonmax'
         lonmax = result

'getinfo latmin'
         latmin = result
'getinfo latmax'
         latmax = result

'getinfo tmin'
         tmin = result
'getinfo tmax'
         tmax = result

'set t 'tmin
'getinfo date'
      begdate = result

'getinfo zmin'
         zmin = result
'getinfo zmax'
         zmax = result
'set z  'zmin' 'zmax
         zdim = zmax-zmin+1

'sety'
'set t 'tmin' 'tmax
        tdim = tmax-tmin+1

'getinfo xdim'
         xdim = result
'getinfo ydim'
         ydim = result

'getinfo undef'
         undef = result
'getinfo dlat' 
         dlat  = result
'getinfo dlon' 
         dlon  = result
       lontot  = lonmax - lonmin + dlon/2

* Get INFO from CTL
* -----------------
'q ctlinfo'
         n = 1
      while( n >0 )
             line = sublin(result,n)
             word = subwrd(line,1)
         if( word = 'tdef' )
             dt = subwrd(line,5)
             n  = 0
          else
             n  = n + 1
          endif
      endwhile
         n = 1
      while( n >0 )
             line = sublin(result,n)
             word = subwrd(line,1)
         if( word = 'ydef' )
           lat0 = subwrd(line,4)
             n  = 0
          else
             n  = n + 1
          endif
      endwhile
         n = 1
      while( n >0 )
             line = sublin(result,n)
             word = subwrd(line,1)
         if( word = 'xdef' )
           lon0 = subwrd(line,4)
             n  = 0
          else
             n  = n + 1
          endif
      endwhile
* -----------------

      levs = ''
             z  = zmin
      while( z <= zmax )
            'set z 'z
            'getinfo level'
             levs = levs' 'result
             z = z + 1
      endwhile
       
    say ''
    say 'DFILE: 'infile
    say 'UNDEF: 'undef
    say ' LON0: 'lon0
    say ' DLON: 'dlon
    say ' YDEF: 'ydim
    say ' LAT0: 'lat0
    say ' DLAT: 'dlat
    say ' XDEF: 'xdim
    say ' ZDEF: 'zdim
    say ' TDEF: 'tdim
    say 'BDATE: 'begdate
    say '   DT: 'dt
    say ' LEVS: 'levs
    say ''
    say ' LONMIN:LONMAX = 'lonmin':'lonmax
    say ''

  '!remove sedfile'
  '!touch  sedfile'
  '!remove quad.ctl'
  '!remove 'name'.data'
   'set gxout fwrite'
   'set       fwrite 'name'.data'

* ----------------------------------------------------------------------------

    t = tmin
    while( t<=tmax )
    say '          Computing Quadratic Data for Tag: 'tag'  for t = 't
   'set t 't
   'sety'
   'set z 'zmin' 'zmax

   'set lon 'lonmin' 'lonmax
   'define locu = 'uname'*u'tag'scale'
   'define locv = 'vname'*v'tag'scale'
   'define loct = 'tname'*t'tag'scale'
   'define locq = 'qname'*q'tag'scale'
   'define locw = 'wname'*w'tag'scale'
   'define loch = 'hname'*h'tag'scale'

   'define locvaru = Var_'uname'*pow(u'tag'scale,2)'
   'define locvarv = Var_'vname'*pow(v'tag'scale,2)'
   'define locvart = Var_'tname'*pow(t'tag'scale,2)'
   'define locvarq = Var_'qname'*pow(q'tag'scale,2)'
   'define locvarw = Var_'wname'*pow(w'tag'scale,2)'
   'define locvarh = Var_'hname'*pow(h'tag'scale,2)'

   'chckname 'uvname
   'chckname 'vtname
   'chckname 'vqname
   'chckname 'wtname
   'chckname 'wqname

   'define loccovuv = 'uvname'*u'tag'scale*v'tag'scale'
   'define loccovvt = 'vtname'*v'tag'scale*t'tag'scale'
   'define loccovvq = 'vqname'*v'tag'scale*q'tag'scale'
   'define loccovwt = 'wtname'*w'tag'scale*t'tag'scale'
   'define loccovwq = 'wqname'*w'tag'scale*q'tag'scale'

   'set x 1'
    if( lontot > 360 )
        'define locuz = ave(locu,lon='lonmin',lon='lonmax',-b)'
        'define locvz = ave(locv,lon='lonmin',lon='lonmax',-b)'
        'define loctz = ave(loct,lon='lonmin',lon='lonmax',-b)'
        'define locqz = ave(locq,lon='lonmin',lon='lonmax',-b)'
        'define locwz = ave(locw,lon='lonmin',lon='lonmax',-b)'
        'define lochz = ave(loch,lon='lonmin',lon='lonmax',-b)'

        'define temp = ave( (locu-locuz)*(locu-locuz),lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( (locv-locvz)*(locv-locvz),lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( (loct-loctz)*(loct-loctz),lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( (locq-locqz)*(locq-locqz),lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( (locw-locwz)*(locw-locwz),lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( (loch-lochz)*(loch-lochz),lon='lonmin',lon='lonmax',-b)' ; 'd temp'

        'define temp = ave( (locu-locuz)*(locv-locvz),lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( (locv-locvz)*(loct-loctz),lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( (locv-locvz)*(locq-locqz),lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( (locw-locwz)*(loct-loctz),lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( (locw-locwz)*(locq-locqz),lon='lonmin',lon='lonmax',-b)' ; 'd temp'

        'define temp = ave( locvaru,lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( locvarv,lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( locvart,lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( locvarq,lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( locvarw,lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( locvarh,lon='lonmin',lon='lonmax',-b)' ; 'd temp'

        'define temp = ave( loccovuv,lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( loccovvt,lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( loccovvq,lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( loccovwt,lon='lonmin',lon='lonmax',-b)' ; 'd temp'
        'define temp = ave( loccovwq,lon='lonmin',lon='lonmax',-b)' ; 'd temp'

    else
        'define locuz = ave(locu,lon='lonmin',lon='lonmax')'
        'define locvz = ave(locv,lon='lonmin',lon='lonmax')'
        'define loctz = ave(loct,lon='lonmin',lon='lonmax')'
        'define locqz = ave(locq,lon='lonmin',lon='lonmax')'
        'define locwz = ave(locw,lon='lonmin',lon='lonmax')'
        'define lochz = ave(loch,lon='lonmin',lon='lonmax')'

        'define temp = ave( (locu-locuz)*(locu-locuz),lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( (locv-locvz)*(locv-locvz),lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( (loct-loctz)*(loct-loctz),lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( (locq-locqz)*(locq-locqz),lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( (locw-locwz)*(locw-locwz),lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( (loch-lochz)*(loch-lochz),lon='lonmin',lon='lonmax')' ; 'd temp'

        'define temp = ave( (locu-locuz)*(locv-locvz),lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( (locv-locvz)*(loct-loctz),lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( (locv-locvz)*(locq-locqz),lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( (locw-locwz)*(loct-loctz),lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( (locw-locwz)*(locq-locqz),lon='lonmin',lon='lonmax')' ; 'd temp'

        'define temp = ave( locvaru,lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( locvarv,lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( locvart,lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( locvarq,lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( locvarw,lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( locvarh,lon='lonmin',lon='lonmax')' ; 'd temp'

        'define temp = ave( loccovuv,lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( loccovvt,lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( loccovvq,lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( loccovwt,lon='lonmin',lon='lonmax')' ; 'd temp'
        'define temp = ave( loccovwq,lon='lonmin',lon='lonmax')' ; 'd temp'

    endif

    t = t+1
    endwhile

* ----------------------------------------------------------------------------

    say ''
   'disable fwrite'
   'undefine temp'
   'undefine locu'
   'undefine locv'
   'undefine loct'
   'undefine locq'
   'undefine locw'
   'undefine loch'

   'undefine locuz'
   'undefine locvz'
   'undefine loctz'
   'undefine locqz'
   'undefine locwz'
   'undefine lochz'

   'undefine locvaru'
   'undefine locvarv'
   'undefine locvart'
   'undefine locvarq'
   'undefine locvarw'
   'undefine locvarh'

   'undefine loccovuv'
   'undefine loccovvt'
   'undefine loccovvq'
   'undefine loccovwt'
   'undefine loccovwq'

   'set gxout 'gxout

'!echo "s?qz.data?"'name'.data?g >> sedfile'
'!echo "s?UNDEF?"'undef'?g >> sedfile'
'!echo "s?DX?"'dlon'?g >> sedfile'
'!echo "s?DY?"'dlat'?g >> sedfile'
'!echo "s?DT?"'dt'?g >> sedfile'
'!echo "s?LON0?"'lon0'?g >> sedfile'
'!echo "s?LAT0?"'lat0'?g >> sedfile'
'!echo "s?YDIM?"'ydim'?g >> sedfile'
'!echo "s?ZDIM?"'zdim'?g >> sedfile'
'!echo "s?TDIM?"'tdim'?g >> sedfile'
'!echo "s?BDATE?"'begdate'?g >> sedfile'
'!echo "s?LEVS?"'levs'?g  >> sedfile'
'!/bin/cp 'geosutil'/plots/grads_util/makequadz.tmpl .'
'!sed -f   sedfile makequadz.tmpl > 'name'.ctl'

   'open 'name'.ctl'
   'getinfo    numfiles'
               newfile = result
   'set dfile 'newfile
   'set lon 'lon0
   'sety'
   'setz'
   'getinfo  tdim'
             tdim = result
   'set t 1 'tdim

   'define Var'uname's'tag' = varus'
   'define Var'vname's'tag' = varvs'
   'define Var'tname's'tag' = varts'
   'define Var'qname's'tag' = varqs'
   'define Var'wname's'tag' = varws'
   'define Var'hname's'tag' = varhs'

   'define Cov'uname''vname's'tag' = covuvs'
   'define Cov'vname''tname's'tag' = covvts'
   'define Cov'vname''qname's'tag' = covvqs'
   'define Cov'wname''tname's'tag' = covwts'
   'define Cov'wname''qname's'tag' = covwqs'

   'define Var'uname'p'tag' = varup'
   'define Var'vname'p'tag' = varvp'
   'define Var'tname'p'tag' = vartp'
   'define Var'qname'p'tag' = varqp'
   'define Var'wname'p'tag' = varwp'
   'define Var'hname'p'tag' = varhp'

   'define Cov'uname''vname'p'tag' = covuvp'
   'define Cov'vname''tname'p'tag' = covvtp'
   'define Cov'vname''qname'p'tag' = covvqp'
   'define Cov'wname''tname'p'tag' = covwtp'
   'define Cov'wname''qname'p'tag' = covwqp'

   'close 'newfile

   'set dfile 'infile
   'set lat 'latmin' 'latmax
   'set lon 'lonmin' 'lonmax
   'set t   '  tmin' '  tmax

return
