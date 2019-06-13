function dc (args)

* Examine Input Arguments
* -----------------------
'numargs  'args
 numargs = result

*******************************************************
* Print USAGE if no arguments
* ---------------------------
if( numargs = 0 )
say
say ' Usage Option 1: dc q1         (A-Grid/D-Grid Scalar)'
say ' Usage Option 2: dc q1;q2      (A-Grid Vector)'
say
say 'For Scalar Countour Plots, contour values may be set by supplying optional parameters:'
say 'example:  dc q1 -cint 5'
say
say 'For Scalar Shaded   Plots, contour values and minimum value may be set by supplying optional parameters:'
say 'example:  dc q1 -cint 5 -minval 20'
say
say 'For Vector Plots, CINT is used to control the vector magnitude which produces a vector arrow size of 1:'
say 'example:  dc q1;q2 -cint 5 (this will use:  set arrscl 1 5)'
say
say 'You may plot the entire globe, or specify a particular face:'
say 'example:  dc q1 -cint 5 -face N  (N=1,2,3,4,5, or 6)'
say
say 'You may shade the background topography when the input field is contoured (i.e., not shaded or grfill)'
say 'example:  dc q1 -cint 5 -toposhade -face N  (N=1,2,3,4,5, or 6)'
say
return
endif
*******************************************************

arg1 = subwrd(args,1)

face      = GLOBAL
cint      = NULL
maxval    = NULL
minval    = NULL
toposhade = NULL

lonnp =  -90
lonsp =    0

        num = 0
while ( num < numargs )
        num = num + 1
if( subwrd(args,num) = '-face'      ) ; face   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-cint'      ) ; cint   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-maxval'    ) ; maxval = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-minval'    ) ; minval = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-toposhade' ) ; toposhade = 'TRUE'          ; endif
endwhile

'run uppercase 'face 
                face = result

* Generic Plot Attributes
* -----------------------
'run getinfo file'
             file  = result
'run getinfo time'
             time  = result
'run getinfo level'
             level = result
'run getinfo ens'
             ens   = result

'q gxout'
   gxout = sublin(result,4)
   gxout = subwrd(gxout,6)

'set arrlab off'

pi = 3.141592654

* Extract Fields to Plot
* ----------------------
n = 1
k = 1
bit = substr(arg1,n,1)
q.1 = ''
while( bit != '' )
   if( bit  = ';' )
         k  = k + 1
      q.k  = ''
   else
      q.k  = q.k''bit
   endif
   n = n + 1
   bit = substr(arg1,n,1)
endwhile

*******************************************************
* Set Defaults for Grid and Vectors
*******************************************************
*
* Usage Options (k=1): dc q1  (A-Grid Scalar)
if( k = 1 )
    q.2    = NULL
    scalar = true
    vector = false
endif

* Usage Options (k=2): dc q1;q2 (A-Grid Vector)
* -------------------- 
if( k = 2 )
    scalar = false
    vector = true
endif

*******************************************************

say ''
say 'Input Fields:'
say '-------------'
n = 1
while( n<=k )
say 'n: 'n' 'q.n
n = n + 1
endwhile
say ''

*******************************************************
* Define Background Topography
*******************************************************

'run getenv "GEOSUTIL"'
             geosutil = result
'sdfopen 'geosutil'/plots/grads_util/FRAC_720x4320.nc4'
'getinfo    numfiles'
            topofile = result
'set dfile 'topofile
'set xyrev off'
'set xflip off'
'set yflip off'
'setx'
'sety'
'set t 1'
'set z 1'
'set e 1'
'define  topo = frland + frlandice'
      topoval = 0.6
'getinfo xdim'
         xdimtopo = result
'getinfo ydim'
         ydimtopo = result

n = 1
while( n<=6 )
     ytopo.n = xdimtopo * n
           n = n + 1
endwhile

'define  xxtopo = lon'
'define  yytopo = lat'
'define ynptopo =  yytopo-'ytopo.2
'define ysptopo =  yytopo-'ytopo.5

*******************************************************
*                 Define Boundaries
*******************************************************

'set dfile 'file
'set t     'time
'set lev   'level

'set mproj off'
'set display color white'
'rgbset'

'set grid  off'
'set xlab  off'
'set ylab  off'
'set clab  off'

'set xyrev off'
'set xflip off'
'set yflip off'
'setx'
'sety'
'sete'
'getinfo xdim'
         xdim = result
'getinfo ydim'
         ydim = result
'getinfo edim'
         edim = result

* Compute Y-Edge Boundaries
* -------------------------
n = 1
while(  n<=6 )
      y.n = xdim * n
say 'Boundary Edges:  y.'n' = 'xdim*n
        n = n + 1
endwhile
say ''

if( edim = 1 )
   'define  xx = lon'
   'define  yy = lat'
   'define ynp = yy-'y.2
   'define ysp = yy-'y.5
else
   'getinfo lonmin'
            lonmin = result
   'getinfo latmin'
            latmin = result
   'getinfo lonmax'
            lonmax = result
   'getinfo latmax'
            latmax = result

    dlon = (lonmax-lonmin)/(xdim-1)
    dlat = (latmax-latmin)/(ydim-1)

   'define  xx = (lon-'lonmin')/'dlon' + 1'
   'define  yy = (lat-'latmin')/'dlat' + 1'
   'define ynp = yy'
   'define ysp = yy'
endif

*******************************************************
*          Define Scalar Contours and Shades
*******************************************************

if( scalar = true )
    if( cint = NULL )
       'shades 'q.1' 0' 
    else
        if( cint < 0 )
           acint = -1 * cint
          'shades 'acint
        else
           if( minval = NULL )
             'shades 'q.1' 0 -cint 'cint
           else
             'shades 'q.1' 0 -cint 'cint' -minval 'minval
           endif
        endif
    endif
   'run getenv SHADES_CLEVS'
                      clevs = result
   'run getenv SHADES_CCOLS'
                      ccols = result
    if( cint = NULL )
   'run getenv SHADES_CINT'
                      cint  = result
    endif
    say 'CLEVS: 'clevs
    say 'CCOLS: 'ccols
    say 'CINT:  'cint
endif

*******************************************************
* Face 1
*******************************************************

if( face = GLOBAL ) 
   'set vpage 0 11 0 8.5'
   'set parea 0.5 3.0 3.0 5.5'
endif
'set grads off'

* Shade Topo
* ----------
if( toposhade = TRUE & gxout = Contour & (face = GLOBAL | face = 1) )
'set dfile 'topofile
'set gxout shaded'
'setx'
'set y 1 'ytopo.1'.5'
'set e 1'
'set clevs 0.5'
'set ccols 0 92'
'd topo'
'set cthick 4'
'set dfile 'file
endif
* ----------

'setx'
'sety'
'sete'

if( scalar = true )
    if( gxout != Contour )
       'set gxout 'gxout
        if( cint = NULL )
           'shades 'q.1' 0' 
        else
            if( cint < 0 )
               acint = -1 * cint
              'shades 'acint
            else
            if( minval = NULL )
           'shades 'q.1' 0 -cint 'cint
            else
           'shades 'q.1' 0 -cint 'cint' -minval 'minval
            endif
            endif
        endif
    endif
    if( gxout = Contour )
       'set ccols 'ccols
       'set clevs 'clevs
    endif
endif

if( edim = 1 )
      y1 = y.1 + 0.0
     'set y 1 'y1
      say 'Face 1, y = 1 to 'y1
else
      y1 = y.1 + 0.0
     'set  y 1 'y1
     'set  e 1 '
      say 'Face 1, y = 1 to 'y1'  e = 1'
*    'define pi = 3.141592654'
*    'define uuz = -10*( sin(lats*pi/180)*cos(lons*pi/180) - cos(lats*pi/180) )'
*    'define vvz =  10*( sin(lons*pi/180) )'
endif

if( face = GLOBAL | face = 1 )

if( scalar = true )
   'set gxout 'gxout
   'd 'q.1
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    'd 'q.1';'q.2
endif

* Contour Topo
* ------------
'set gxout contour'
'set dfile 'topofile
'setx'
'set y 1 'ytopo.1'.5'
'set e 1'
'set clevs 'topoval
'set ccolor 1'
'set cthick 6'
'd topo'
'set cthick 4'
'set dfile 'file

endif

*******************************************************
* Face 2
*******************************************************

if( face = GLOBAL )
   'set parea 3.0 5.5 3.00 5.50'
endif
'set xlab off'
'set ylab off'

* Shade Topo
* ----------
if( toposhade = TRUE & gxout = Contour & (face = GLOBAL | face = 2) )
'set dfile 'topofile
'set gxout shaded'
'setx'
ytopo1 = ytopo.1 + 1.0
ytopo2 = ytopo.2 + 0.5
'set y 'ytopo1' 'ytopo2
'set e 1'
'set clevs 0.5'
'set ccols 0 92'
'd topo'
'set cthick 4'
'set dfile 'file
endif
* ----------

'setx'
'sety'
'sete'

if( scalar = true )
    if( gxout != Contour )
       'set gxout 'gxout
        if( cint = NULL )
           'shades 'q.1' 0' 
        else
            if( cint < 0 )
               acint = -1 * cint
              'shades 'acint
            else
            if( minval = NULL )
           'shades 'q.1' 0 -cint 'cint
            else
           'shades 'q.1' 0 -cint 'cint' -minval 'minval
            endif
            endif
        endif
    endif
    if( gxout = Contour )
       'set ccols 'ccols
       'set clevs 'clevs
    endif
endif

if( edim = 1 )
      y1 = y.1 + 1.0
      y2 = y.2 + 0.5
     'set y 'y1' 'y2
      say 'Face 2, y = 'y1' to 'y2
else
      y1 = y.1 + 0.0
     'set  y 1 'y1
     'set  e 2 '
      say 'Face 2, y = 1 to 'y1'  e = 2'
*    'define pi = 3.141592654'
*    'define uuz = -10*( sin(lats*pi/180)*cos(lons*pi/180) - cos(lats*pi/180) )'
*    'define vvz =  10*( sin(lons*pi/180) )'
endif


if( face = GLOBAL | face = 2 )

if( scalar = true )
   'set gxout 'gxout
   'd 'q.1
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    'd 'q.1';'q.2
endif

* Contour Topo
* ------------
'set gxout contour'
'set dfile 'topofile
'setx'
ytopo1 = ytopo.1 + 1.0
ytopo2 = ytopo.2 + 0.5
'set y 'ytopo1' 'ytopo2
'set e 1'
'set clevs 'topoval
'set ccolor 1'
'set cthick 6'
'd topo'
'set cthick 4'
'set dfile 'file

endif

*******************************************************
* Face 4
*******************************************************

if( face = GLOBAL )
   'set parea 5.5 8.0 3.0 5.5'
endif
'set xlab off'
'set ylab off'

'set xyrev on'
'set yflip on'

* Shade Topo
* ----------
if( toposhade = TRUE & gxout = Contour & (face = GLOBAL | face = 4) )
'set dfile 'topofile
'set gxout shaded'
'setx'
'set y 'ytopo.3'.5 'ytopo.4'.5'
'set e 1'
'set clevs 0.5'
'set ccols 0 92'
'd topo'
'set cthick 4'
'set dfile 'file
endif
* ----------

'setx'
'sety'
'sete'

if( scalar = true )
    if( gxout != Contour )
       'set gxout 'gxout
        if( cint = NULL )
           'shades 'q.1' 0' 
        else
            if( cint < 0 )
               acint = -1 * cint
              'shades 'acint
            else
            if( minval = NULL )
           'shades 'q.1' 0 -cint 'cint
            else
           'shades 'q.1' 0 -cint 'cint' -minval 'minval
            endif
            endif
        endif
    endif
    if( gxout = Contour )
       'set ccols 'ccols
       'set clevs 'clevs
    endif
endif

if( edim = 1 )
      y3 = y.3 + 1.0
      y4 = y.4 + 0.5
     'set y 'y3' 'y4
      say 'Face 4, y = 'y3' to 'y4
else
      y1 = y.1 + 0.0
     'set  y 1 'y1
     'set  e 4 '
      say 'Face 4, y = 1 to 'y1'  e = 4'
*    'define pi = 3.141592654'
*    'define uuz = -10*( sin(lats*pi/180)*cos(lons*pi/180) - cos(lats*pi/180) )'
*    'define vvz =  10*( sin(lons*pi/180) )'
endif

if( face = GLOBAL | face = 4 )
if( scalar = true )
   'set gxout 'gxout
   'd 'q.1
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    'd 'q.1';'q.2
endif

* Contour Topo
* ------------
'set gxout contour'
'set dfile 'topofile
'setx'
'set y 'ytopo.3'.5 'ytopo.4'.5'
'set e 1'
'set clevs 'topoval
'set ccolor 1'
'set cthick 6'
'd topo'
'set cthick 4'
'set dfile 'file

endif

*******************************************************
* Face 5
*******************************************************


if( face = GLOBAL )
   'set parea 8.0 10.5 3.0 5.5'
endif
'set xlab off'
'set ylab off'

'set yflip on'

* Shade Topo
* ----------
if( toposhade = TRUE & gxout = Contour & (face = GLOBAL | face = 5) )
'set dfile 'topofile
'set gxout shaded'
'setx'
'set y 'ytopo.4'.5 'ytopo.5'.5'
'set e 1'
'set clevs 0.5'
'set ccols 0 92'
'd topo'
'set cthick 4'
'set dfile 'file
endif
* ----------

'setx'
'sety'
'sete'

if( scalar = true )
    if( gxout != Contour )
       'set gxout 'gxout
        if( cint = NULL )
           'shades 'q.1' 0' 
        else
            if( cint < 0 )
               acint = -1 * cint
              'shades 'acint
            else
            if( minval = NULL )
           'shades 'q.1' 0 -cint 'cint
            else
           'shades 'q.1' 0 -cint 'cint' -minval 'minval
            endif
            endif
        endif
    endif
    if( gxout = Contour )
       'set ccols 'ccols
       'set clevs 'clevs
    endif
endif

if( edim = 1 )
     'set y 'y.4'.5 'y.5'.5'
      say 'Face 5, y = 'y.4' to 'y.5'.5'
else
      y1 = y.1 + 0.0
     'set  y 1 'y1
     'set  e 5 '
      say 'Face 5, y = 1 to 'y1'  e = 5'
*    'define pi = 3.141592654'
*    'define uuz = -10*( sin(lats*pi/180)*cos(lons*pi/180) - cos(lats*pi/180) )'
*    'define vvz =  10*( sin(lons*pi/180) )'
endif

if( face = GLOBAL | face = 5 )
if( scalar = true )
   'set gxout 'gxout
   'd 'q.1
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    'd 'q.1';'q.2
endif

* Contour Topo
* ------------
'set gxout contour'
'set dfile 'topofile
'setx'
'set y 'ytopo.4'.5 'ytopo.5'.5'
'set e 1'
'set clevs 'topoval
'set ccolor 1'
'set cthick 6'
'd topo'
'set cthick 4'
'set dfile 'file

endif

*******************************************************
* North Pole (Face 3) for Face-1
*******************************************************

if( face = GLOBAL | face = 3 | face = 31 )

if( face = GLOBAL )
   'set parea 0.5 3.0 5.5 8.0'
   'set xyrev on'
   'set xflip on'
   'set yflip off'
endif
'set xlab off'
'set ylab off'

* Shade Topo
* ----------
if( toposhade = TRUE & gxout = Contour )
'set dfile 'topofile
'set gxout shaded'
'setx'
'set y 'ytopo.2'.5 'ytopo.3'.5'
'set e 1'
'set clevs 0.5'
'set ccols 0 92'
'd maskout( maskout( topo,ynptopo-xxtopo+1) , 'xdimtopo'-ynptopo-xxtopo+2 )'
'set cthick 4'
'set dfile 'file
endif
* ----------

'setx'
'sety'
'sete'

if( scalar = true )
    if( gxout != Contour )
       'set gxout 'gxout
        if( cint = NULL )
           'shades 'q.1' 0' 
        else
            if( cint < 0 )
               acint = -1 * cint
              'shades 'acint
            else
            if( minval = NULL )
           'shades 'q.1' 0 -cint 'cint
            else
           'shades 'q.1' 0 -cint 'cint' -minval 'minval
            endif
            endif
        endif
    endif
    if( gxout = Contour )
       'set ccols 'ccols
       'set clevs 'clevs
    endif
endif

if( edim = 1 )
     'set y 'y.2'.5 'y.3'.5'
      say 'North Pole (Face 3) for Face 1:  y = 'y.2'.5 to 'y.3'.5'
else
      y1 = y.1 + 0.0
     'set  y 1 'y1
     'set  e 3 '
      say 'North Pole (Face 3) for Face 1:  y = 1 to 'y1'  e = 3'
*    'define pi = 3.141592654'
*    'define uuz = -10*( sin(lats*pi/180)*cos(lons*pi/180) - cos(lats*pi/180) )'
*    'define vvz =  10*( sin(lons*pi/180) )'
endif


if( scalar = true )
   'set gxout 'gxout
   'd maskout( maskout( 'q.1',ynp-xx+1) , 'xdim'-ynp-xx+2 )'
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
        if( face != GLOBAL ) 
           'define lonz = (lons-'lonnp')*3.141592654/180'
           'define   uz = -'q.1'*sin(lonz) - 'q.2'*cos(lonz)'
           'define   vz =  'q.1'*cos(lonz) - 'q.2'*sin(lonz)'
        else
           'define   uz =  'q.1
           'define   vz =  'q.2
        endif
       'd maskout( maskout( uz,ynp-xx+1),'xdim'-ynp-xx+2 );maskout( maskout(vz,ynp-xx+1),'xdim'-ynp-xx+2 )'
endif

* Contour Topo
* ------------
'set gxout contour'
'set dfile 'topofile
*if( face = GLOBAL )
*'set xyrev on'
*'set xflip on'
*'set yflip off'
*endif
'setx'
'set y 'ytopo.2'.5 'ytopo.3'.5'
'set e 1'
'set clevs 'topoval
'set ccolor 1'
'set cthick 6'
'd maskout( maskout( topo,ynptopo-xxtopo+1) , 'xdimtopo'-ynptopo-xxtopo+2 )'
'set cthick 4'
'set dfile 'file

endif

*******************************************************
* North Pole (Face 3) for Face-2
*******************************************************

if( face = GLOBAL | face = 3 | face = 32 )

if( face = GLOBAL )
   'set parea 3.0 5.5 5.5 8.0'
   'set xyrev off'
   'set xflip off'
   'set yflip off'
endif

'set xlab off'
'set ylab off'

* Shade Topo
* ----------
if( toposhade = TRUE & gxout = Contour )
'set dfile 'topofile
'set gxout shaded'
'setx'
'set y 'ytopo.2'.5 'ytopo.3'.5'
'set e 1'
'set clevs 0.5'
'set ccols 0 92'
'd maskout( maskout( topo,xxtopo-ynptopo) , 'xdimtopo'-ynptopo-xxtopo )'
'set cthick 4'
'set dfile 'file
endif
* ----------

'setx'
'sety'
'sete'

if( scalar = true )
    if( gxout != Contour )
       'set gxout 'gxout
        if( cint = NULL )
           'shades 'q.1' 0' 
        else
            if( cint < 0 )
               acint = -1 * cint
              'shades 'acint
            else
            if( minval = NULL )
           'shades 'q.1' 0 -cint 'cint
            else
           'shades 'q.1' 0 -cint 'cint' -minval 'minval
            endif
            endif
        endif
    endif
    if( gxout = Contour )
       'set ccols 'ccols
       'set clevs 'clevs
    endif
endif


if( edim = 1 )
     'set y 'y.2'.5 'y.3'.5'
      say 'North Pole (Face 3) for Face 2:  y = 'y.2'.5 to 'y.3'.5'
else
      y1 = y.1 + 0.0
     'set  y 1 'y1
     'set  e 3 '
      say 'North Pole (Face 3) for Face 2:  y = 1 to 'y1'  e = 3'
*    'define pi = 3.141592654'
*    'define uuz = -10*( sin(lats*pi/180)*cos(lons*pi/180) - cos(lats*pi/180) )'
*    'define vvz =  10*( sin(lons*pi/180) )'
endif


if( scalar = true )
   'set gxout 'gxout
   'd maskout( maskout( 'q.1',xx-ynp  ) , 'xdim'-ynp-xx+1 )'
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
        if( face != GLOBAL )
           'define lonz = (lons-'lonnp')*3.141592654/180'
           'define uz   = -'q.1'*sin(lonz) - 'q.2'*cos(lonz)'
           'define vz   =  'q.1'*cos(lonz) - 'q.2'*sin(lonz)'
        else
           'define uz   =  'q.1
           'define vz   =  'q.2
        endif
       'd maskout( maskout(uz,xx-ynp),'xdim'-ynp-xx+1 );maskout( maskout(vz,xx-ynp),'xdim'-ynp-xx+1 )'
endif

* Contour Topo
* ------------
'set gxout contour'
'set dfile 'topofile
'setx'
'set y 'ytopo.2'.5 'ytopo.3'.5'
'set e 1'
'set clevs 'topoval
'set ccolor 1'
'set cthick 6'
'd maskout( maskout( topo,xxtopo-ynptopo) , 'xdimtopo'-ynptopo-xxtopo )'
'set cthick 4'
'set dfile 'file

endif

*******************************************************
* North Pole (Face 3) for Face-4
*******************************************************

if( face = GLOBAL | face = 3 | face = 34 )

if( face = GLOBAL )
   'set parea 5.5 8.0 5.5 8.0'
   'set xyrev on'
   'set xflip off'
   'set yflip on'
endif
'set xlab off'
'set ylab off'

* Shade Topo
* ----------
if( toposhade = TRUE & gxout = Contour )
'set dfile 'topofile
'set gxout shaded'
'setx'
'set y 'ytopo.2'.5 'ytopo.3'.5'
'set e 1'
'set clevs 0.5'
'set ccols 0 92'
'd maskout( maskout( topo,xxtopo-ynptopo+1) , ynptopo+xxtopo-'xdimtopo' )'
'set cthick 4'
'set dfile 'file
endif
* ----------

'setx'
'sety'
'sete'

if( scalar = true )
    if( gxout != Contour )
       'set gxout 'gxout
        if( cint = NULL )
           'shades 'q.1' 0' 
        else
            if( cint < 0 )
               acint = -1 * cint
              'shades 'acint
            else
            if( minval = NULL )
           'shades 'q.1' 0 -cint 'cint
            else
           'shades 'q.1' 0 -cint 'cint' -minval 'minval
            endif
            endif
        endif
    endif
    if( gxout = Contour )
       'set ccols 'ccols
       'set clevs 'clevs
    endif
endif

if( edim = 1 )
     'set y 'y.2'.5 'y.3'.5'
      say 'North Pole (Face 3) for Face 4:  y = 'y.2'.5 to 'y.3'.5'
else
      y1 = y.1 + 0.0
     'set  y 1 'y1
     'set  e 3 '
      say 'North Pole (Face 3) for Face 4:  y = 1 to 'y1'  e = 3'
*    'define pi = 3.141592654'
*    'define uuz = -10*( sin(lats*pi/180)*cos(lons*pi/180) - cos(lats*pi/180) )'
*    'define vvz =  10*( sin(lons*pi/180) )'
endif


if( scalar = true )
   'set gxout 'gxout
   'd maskout( maskout( 'q.1', xx-ynp+1 ), ynp+xx-'xdim' )'
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
        if( face != GLOBAL )
           'define lonz = (lons-'lonnp')*3.141592654/180'
           'define uz   = -'q.1'*sin(lonz) - 'q.2'*cos(lonz)'
           'define vz   =  'q.1'*cos(lonz) - 'q.2'*sin(lonz)'
        else
           'define uz   =  'q.1
           'define vz   =  'q.2
        endif
       'd maskout( maskout(uz,xx-ynp+1 ),ynp+xx-'xdim' );maskout( maskout( vz,xx-ynp+1 ),ynp+xx-'xdim' )'
endif

* Contour Topo
* ------------
'set gxout contour'
'set dfile 'topofile
if( face = GLOBAL )
'set xyrev on'
'set xflip off'
'set yflip on'
endif
'setx'
'set y 'ytopo.2'.5 'ytopo.3'.5'
'set e 1'
'set clevs 'topoval
'set ccolor 1'
'set cthick 6'
'd maskout( maskout( topo,xxtopo-ynptopo+1) , ynptopo+xxtopo-'xdimtopo' )'
'set cthick 4'
'set dfile 'file

endif

*******************************************************
* North Pole (Face 3) for Face-5
*******************************************************

if( face = GLOBAL | face = 3 | face = 35 )

if( face = GLOBAL )
   'set parea 8.0 10.5 5.5 8.0'
   'set xyrev off'
   'set xflip on'
   'set yflip on'
endif

'set xlab off'
'set ylab off'

* Shade Topo
* ----------
if( toposhade = TRUE & gxout = Contour )
'set dfile 'topofile
'set gxout shaded'
'setx'
'set y 'ytopo.2'.5 'ytopo.3'.5'
'set e 1'
'set clevs 0.5'
'set ccols 0 92'
'd maskout( maskout( topo,ynptopo-xxtopo) , ynptopo+xxtopo-'xdimtopo'-1 )'
'set cthick 4'
'set dfile 'file
endif
* ----------

'setx'
'sety'
'sete'

if( scalar = true )
    if( gxout != Contour )
       'set gxout 'gxout
        if( cint = NULL )
           'shades 'q.1' 0' 
        else
            if( cint < 0 )
               acint = -1 * cint
              'shades 'acint
            else
            if( minval = NULL )
           'shades 'q.1' 0 -cint 'cint
            else
           'shades 'q.1' 0 -cint 'cint' -minval 'minval
            endif
            endif
        endif
    endif
    if( gxout = Contour )
       'set ccols 'ccols
       'set clevs 'clevs
    endif
endif

if( edim = 1 )
     'set y 'y.2'.5 'y.3'.5'
      say 'North Pole (Face 3) for Face 5:  y = 'y.2'.5 to 'y.3'.5'
else
      y1 = y.1 + 0.0
     'set  y 1 'y1
     'set  e 3 '
      say 'North Pole (Face 3) for Face 5:  y = 1 to 'y1'  e = 3'
*    'define pi = 3.141592654'
*    'define uuz = -10*( sin(lats*pi/180)*cos(lons*pi/180) - cos(lats*pi/180) )'
*    'define vvz =  10*( sin(lons*pi/180) )'
endif


if( scalar = true )
   'set gxout 'gxout
   'd maskout( maskout( 'q.1', ynp-xx ), ynp+xx-'xdim'-1 )'
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
        if( face != GLOBAL )
           'define lonz = (lons-'lonnp')*3.141592654/180'
           'define uz   = -'q.1'*sin(lonz) - 'q.2'*cos(lonz)'
           'define vz   =  'q.1'*cos(lonz) - 'q.2'*sin(lonz)'
        else
           'define uz   =  'q.1
           'define vz   =  'q.2
        endif
       'd maskout( maskout( uz,ynp-xx ),ynp+xx-'xdim'-1);maskout( maskout( vz,ynp-xx),ynp+xx-'xdim'-1 )'
endif

* Contour Topo
* ------------
'set gxout contour'
'set dfile 'topofile
if( face = GLOBAL )
'set xyrev off'
'set xflip on'
'set yflip on'
endif
'setx'
'set y 'ytopo.2'.5 'ytopo.3'.5'
'set e 1'
'set clevs 'topoval
'set ccolor 1'
'set cthick 6'
'd maskout( maskout( topo,ynptopo-xxtopo) , ynptopo+xxtopo-'xdimtopo'-1 )'
'set cthick 4'
'set dfile 'file

endif

*******************************************************
* South Pole (Face 6) for Face-1
*******************************************************

if( face = GLOBAL | face = 6 | face = 61 )

if( face = GLOBAL )
   'set parea 0.5 3.0 0.5 3.0'
   'set xyrev off'
   'set xflip off'
   'set yflip off'
endif

'set xlab off'
'set ylab off'

* Shade Topo
* ----------
if( toposhade = TRUE & gxout = Contour )
'set dfile 'topofile
'set gxout shaded'
'setx'
'set y 'ytopo.5'.5 'ydimtopo
'set e 1'
'set clevs 0.5'
'set ccols 0 92'
'd maskout( maskout( topo,ysptopo-xxtopo+1) , ysptopo+xxtopo-'xdimtopo' )'
'set cthick 4'
'set dfile 'file
endif
* ----------

'setx'
'sety'
'sete'

if( scalar = true )
    if( gxout != Contour )
       'set gxout 'gxout
        if( cint = NULL )
           'shades 'q.1' 0' 
        else
            if( cint < 0 )
               acint = -1 * cint
              'shades 'acint
            else
            if( minval = NULL )
           'shades 'q.1' 0 -cint 'cint
            else
           'shades 'q.1' 0 -cint 'cint' -minval 'minval
            endif
            endif
        endif
    endif
    if( gxout = Contour )
       'set ccols 'ccols
       'set clevs 'clevs
    endif
endif

if( edim = 1 )
     'set y 'y.5'.5 'ydim
      say 'South Pole (Face 6) for Face 1:  y = 'y.5'.5 to 'ydim
else
      y1 = y.1 + 0.0
     'set  y 1 'y1
     'set  e 6 '
      say 'South Pole (Face 6) for Face 1:  y = 1 to 'y1'  e = 6'
*    'define pi = 3.141592654'
*    'define uuz = -10*( sin(lats*pi/180)*cos(lons*pi/180) - cos(lats*pi/180) )'
*    'define vvz =  10*( sin(lons*pi/180) )'
endif

if( scalar = true )
   'set gxout 'gxout
   'd maskout( maskout( 'q.1', ysp-xx+1 ), xx+ysp-'xdim' )'
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
        if( face != GLOBAL )
           'define lonz = (lons-'lonsp')*3.141592654/180'
           'define uz   = -'q.1'*sin(lonz) + 'q.2'*cos(lonz)'
           'define vz   = -'q.1'*cos(lonz) - 'q.2'*sin(lonz)'
        else
           'define uz   =  'q.1
           'define vz   =  'q.2
        endif
       'd maskout( maskout( uz,ysp-xx+1 ),xx+ysp-'xdim');maskout( maskout(vz,ysp-xx+1),xx+ysp-'xdim' )'
endif

* Contour Topo
* ------------
'set gxout contour'
'set dfile 'topofile
'setx'
'set y 'ytopo.5'.5 'ydimtopo
'set e 1'
'set clevs 'topoval
'set ccolor 1'
'set cthick 6'
'd maskout( maskout( topo,ysptopo-xxtopo+1) , ysptopo+xxtopo-'xdimtopo' )'
'set cthick 4'
'set dfile 'file

endif

*******************************************************
* South Pole (Face 6) for Face-2
*******************************************************

if( face = GLOBAL | face = 6 | face = 62 )

if( face = GLOBAL )
   'set parea 3.0 5.5 0.5 3.0'
   'set xyrev on'
   'set xflip on'
   'set yflip off'
endif
'set xlab off'
'set ylab off'

* Shade Topo
* ----------
if( toposhade = TRUE & gxout = Contour )
'set dfile 'topofile
'set gxout shaded'
'setx'
'set y 'ytopo.5'.5 'ydimtopo
'set e 1'
'set clevs 0.5'
'set ccols 0 92'
'd maskout( maskout( topo,xxtopo-ysptopo) , ysptopo+xxtopo-'xdimtopo' )'
'set cthick 4'
'set dfile 'file
endif
* ----------

'setx'
'sety'
'sete'

if( scalar = true )
    if( gxout != Contour )
       'set gxout 'gxout
        if( cint = NULL )
           'shades 'q.1' 0' 
        else
            if( cint < 0 )
               acint = -1 * cint
              'shades 'acint
            else
            if( minval = NULL )
           'shades 'q.1' 0 -cint 'cint
            else
           'shades 'q.1' 0 -cint 'cint' -minval 'minval
            endif
            endif
        endif
    endif
    if( gxout = Contour )
       'set ccols 'ccols
       'set clevs 'clevs
    endif
endif

if( edim = 1 )
     'set y 'y.5'.5 'ydim
      say 'South Pole (Face 6) for Face 2:  y = 'y.5'.5 to 'ydim
else
      y1 = y.1 + 0.0
     'set  y 1 'y1
     'set  e 6 '
      say 'South Pole (Face 6) for Face 2:  y = 1 to 'y1'  e = 6'
*    'define pi = 3.141592654'
*    'define uuz = -10*( sin(lats*pi/180)*cos(lons*pi/180) - cos(lats*pi/180) )'
*    'define vvz =  10*( sin(lons*pi/180) )'
endif

if( scalar = true )
   'set gxout 'gxout
   'd maskout( maskout( 'q.1', xx-ysp ), ysp+xx-'xdim' )'
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
        if( face != GLOBAL )
           'define lonz = (lons-'lonsp')*3.141592654/180'
           'define uz   = -'q.1'*sin(lonz) + 'q.2'*cos(lonz)'
           'define vz   = -'q.1'*cos(lonz) - 'q.2'*sin(lonz)'
        else
           'define uz   =  'q.1
           'define vz   =  'q.2
        endif
       'd maskout( maskout( uz,xx-ysp),ysp+xx-'xdim');maskout( maskout(vz,xx-ysp),ysp+xx-'xdim' )'
endif

* Contour Topo
* ------------
'set gxout contour'
'set dfile 'topofile
'setx'
'set y 'ytopo.5'.5 'ydimtopo
'set e 1'
'set clevs 'topoval
'set ccolor 1'
'set cthick 6'
'd maskout( maskout( topo,xxtopo-ysptopo) , ysptopo+xxtopo-'xdimtopo' )'
'set cthick 4'
'set dfile 'file

endif

*******************************************************
* South Pole (Face 6) for Face-4
*******************************************************

if( face = GLOBAL | face = 6 | face = 64 )

if( face = GLOBAL )
   'set parea 5.5 8.0 0.5 3.0'
   'set xyrev off'
   'set xflip on'
   'set yflip on'
endif

'set xlab off'
'set ylab off'

* Shade Topo
* ----------
if( toposhade = TRUE & gxout = Contour )
'set dfile 'topofile
'set gxout shaded'
'setx'
ytopo5 = ytopo.5 + 0.5
'set y 'ytopo5' 'ydimtopo
'set e 1'
'set clevs 0.5'
'set ccols 0 92'
'd maskout( maskout( topo,xxtopo-ysptopo) , 'xdimtopo'-xxtopo-ysptopo )'
'set cthick 4'
'set dfile 'file
endif
* ----------

'setx'
'sety'
'sete'

if( scalar = true )
    if( gxout != Contour )
       'set gxout 'gxout
        if( cint = NULL )
           'shades 'q.1' 0' 
        else
            if( cint < 0 )
               acint = -1 * cint
              'shades 'acint
            else
            if( minval = NULL )
           'shades 'q.1' 0 -cint 'cint
            else
           'shades 'q.1' 0 -cint 'cint' -minval 'minval
            endif
            endif
        endif
    endif
    if( gxout = Contour )
       'set ccols 'ccols
       'set clevs 'clevs
    endif
endif

if( edim = 1 )
      y5 = y.5 + 1.0
     'set y 'y5' 'ydim
      say 'South Pole (Face 6) for Face 4:  y = 'y5' to 'ydim
else
      y1 = y.1 + 0.0
     'set  y 1 'y1
     'set  e 6 '
      say 'South Pole (Face 6) for Face 4:  y = 1 to 'y1'  e = 6'
*    'define pi = 3.141592654'
*    'define uuz = -10*( sin(lats*pi/180)*cos(lons*pi/180) - cos(lats*pi/180) )'
*    'define vvz =  10*( sin(lons*pi/180) )'
endif

if( scalar = true )
   'set gxout 'gxout
   'd maskout( maskout( 'q.1', xx-ysp ), 'xdim'-xx-ysp )'
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
        if( face != GLOBAL )
           'define lonz = (lons-'lonsp')*3.141592654/180'
           'define uz   = -'q.1'*sin(lonz) + 'q.2'*cos(lonz)'
           'define vz   = -'q.1'*cos(lonz) - 'q.2'*sin(lonz)'
        else
           'define uz   =  'q.1
           'define vz   =  'q.2
        endif
       'd maskout( maskout( uz,xx-ysp),'xdim'-xx-ysp);maskout( maskout( vz,xx-ysp),'xdim'-xx-ysp )'
endif

* Contour Topo
* ------------
'set gxout contour'
'set dfile 'topofile
'setx'
ytopo5 = ytopo.5 + 0.5
'set y 'ytopo5' 'ydimtopo
'set e 1'
'set clevs 'topoval
'set ccolor 1'
'set cthick 6'
'd maskout( maskout( topo,xxtopo-ysptopo) , 'xdimtopo'-xxtopo-ysptopo )'
'set cthick 4'
'set dfile 'file

endif

*******************************************************
* South Pole (Face 6) for Face-5
*******************************************************

if( face = GLOBAL | face = 6 | face = 65 )

if( face = GLOBAL )
   'set parea 8.0 10.5 0.5 3.0'
   'set xyrev on'
   'set xflip off'
   'set yflip on'
endif

'set xlab off'
'set ylab off'

* Shade Topo
* ----------
if( toposhade = TRUE & gxout = Contour )
'set dfile 'topofile
'set gxout shaded'
'setx'
'set y 'ytopo.5'.5 'ydimtopo
'set e 1'
'set clevs 0.5'
'set ccols 0 92'
'd maskout( maskout( topo,ysptopo-xxtopo+1) , 'xdimtopo'-ysptopo-xxtopo+1 )'
'set cthick 4'
'set dfile 'file
endif
* ----------

'setx'
'sety'
'sete'

if( scalar = true )
    if( gxout != Contour )
       'set gxout 'gxout
        if( cint = NULL )
           'shades 'q.1' 0' 
        else
            if( cint < 0 )
               acint = -1 * cint
              'shades 'acint
            else
            if( minval = NULL )
           'shades 'q.1' 0 -cint 'cint
            else
           'shades 'q.1' 0 -cint 'cint' -minval 'minval
            endif
            endif
        endif
    endif
    if( gxout = Contour )
       'set ccols 'ccols
       'set clevs 'clevs
    endif
endif

if( edim = 1 )
     'set y 'y.5'.5 'ydim
      say 'South Pole (Face 6) for Face 5:  y = 'y.5'.5 to 'ydim
else
      y1 = y.1 + 0.0
     'set  y 1 'y1
     'set  e 6 '
      say 'South Pole (Face 6) for Face 5:  y = 1 to 'y1'  e = 6'
*    'define pi = 3.141592654'
*    'define uuz = -10*( sin(lats*pi/180)*cos(lons*pi/180) - cos(lats*pi/180) )'
*    'define vvz =  10*( sin(lons*pi/180) )'
endif

if( scalar = true )
   'set gxout 'gxout
   'd maskout( maskout( 'q.1', ysp-xx+1 ), 'xdim'-ysp-xx+1 )'
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
        if( face != GLOBAL )
           'define lonz = (lons-'lonsp')*3.141592654/180'
           'define uz   = -'q.1'*sin(lonz) + 'q.2'*cos(lonz)'
           'define vz   = -'q.1'*cos(lonz) - 'q.2'*sin(lonz)'
        else
           'define uz   =  'q.1
           'define vz   =  'q.2
        endif
       'd maskout( maskout( uz,ysp-xx+1 ),'xdim'-ysp-xx+1);maskout( maskout( vz,ysp-xx+1),'xdim'-ysp-xx+1 )'
endif

* Contour Topo
* ------------
'set gxout contour'
'set dfile 'topofile
'setx'
'set y 'ytopo.5'.5 'ydimtopo
'set e 1'
'set clevs 'topoval
'set ccolor 1'
'set cthick 6'
'd maskout( maskout( topo,ysptopo-xxtopo+1) , 'xdimtopo'-ysptopo-xxtopo+1 )'
'set cthick 4'
'set dfile 'file
'setx'
'sety'

endif

*******************************************************
*******************************************************

'close 'topofile
if( gxout != Contour ) ; 'set gxout 'gxout ; endif
'set arrlab on'

'set dfile 'file
'set t     'time
'set lev   'level
'set e     'ens
'setx'
'sety'
if( face = GLOBAL )
   'set parea off'
endif

