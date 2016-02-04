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
say ' Usage Option 3: dc q1;q2;A    (A-Grid Vector)'
say ' --------------- dc q1;q2;D    (D-Grid Vector)'
say ' Usage Option 4: dc q1;q2;A;1  (A-Grid Vector, Plot 1st Component Scalar)'
say ' --------------- dc q1;q2;A;2  (A-Grid Vector, Plot 2nd Component Scalar)'
say ' --------------- dc q1;q2;D;1  (D-Grid Vector, Plot 1st Component Scalar)'
say ' --------------- dc q1;q2;D;2  (D-Grid Vector, Plot 2nd Component Scalar)'
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
return
endif
*******************************************************

arg1 = subwrd(args,1)

face   = NULL
cint   = NULL
maxval = NULL
minval = NULL

        num = 0
while ( num < numargs )
        num = num + 1
if( subwrd(args,num) = '-face'   ) ; face   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-cint'   ) ; cint   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-maxval' ) ; maxval = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-minval' ) ; minval = subwrd(args,num+1) ; endif
endwhile


* Generic Plot Attributes
* -----------------------
'run getinfo file'
             file  = result
'run getinfo time'
             time  = result
'run getinfo level'
             level = result

'q gxout'
   gxout = sublin(result,4)
   gxout = subwrd(gxout,6)

'set arrlab off'


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
* Usage Options (k=1): dc q1  (A-Grid/D-Grid Scalar)
if( k = 1 )
    q.2    = NULL
    q.3    = A
    q.4    = 1
    scalar = true
    vector = false
endif

* Usage Options (k=2): dc q1;q2 (A-Grid Vector)
* -------------------- 
if( k = 2 )
    q.3    = A
    scalar = false
    vector = true
endif

* Usage Options (k=3): dc q1;q2;A  (A-Grid Vector)
* -------------------- dc q1;q2;D  (D-Grid Vector)
if( k = 3 )
    scalar = false
    vector = true
endif

* Usage Options (k=4): dc q1;q2;A;1  (A-Grid Vector, 1st Component Scalar)
* -------------------- dc q1;q2;A;2  (A-Grid Vector, 2nd Component Scalar)
* -------------------- dc q1;q2;D;1  (D-Grid Vector, 1st Component Scalar)
* -------------------- dc q1;q2;D;2  (D-Grid Vector, 2nd Component Scalar)
if( k = 4 )
    scalar = true
    vector = false
endif

say 'Input Fields:'
say '-------------'
n = 1
while( n<=k )
say 'n: 'n' 'q.n
n = n + 1
endwhile

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
'define  topo = frland + frlandice'
      topoval = 0.6
'getinfo xdim'
         xdimtopo = result
'getinfo ydim'
         ydimtopo = result
'getinfo lonmin'
         lon0topo = result
'getinfo latmin'
         lat0topo = result

'define  dlontopo = 360/ 'xdimtopo
'define  dlattopo = 180/('ydimtopo-1')'
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
* Define Default Contours and Shades
*******************************************************

'set dfile 'file
'set t     'time
'set lev   'level

'set vpage off'
'set parea off'
'set mproj off'
'set display color white'
'rgbset'

'set vpage 0 11 0 8.5'
'set grid  off'
'set xlab  off'
'set ylab  off'
'set clab  off'

'set xyrev off'
'set xflip off'
'set yflip off'
'setx'
'sety'
'getinfo xdim'
         xdim = result
'getinfo ydim'
         ydim = result
'getinfo lonmin'
         lon0 = result
'getinfo latmin'
         lat0 = result

'define  dlon = 360/ 'xdim
'define  dlat = 180/('ydim-1')'

n = 1
while( n<=6 )
     y.n = xdim * n
       n = n + 1
say 'y.'n' = 'xdim*n
endwhile

'define  xx = lon'
'define  yy = lat'
'define ynp = yy-'y.2
'define ysp = yy-'y.5

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
       'set vpage 0.5 5.5 1.50 5.50'
       'set parea 2.5 3.5 4.00 4.50'
       'set grads off'
       'set annot  0'
       'setx'
       'sety'
        if( cint = NULL )
           'd 'q.1
        else
           'set cint 'cint
           'd 'q.1
        endif
       'q contours'
       line  = sublin( result, 1 )
       nmax  = subwrd( line,   5 )
       ccols = ''
       clevs = ''
        n = 1
       while( n<=nmax )
       line.n = sublin( result, n+1 )
       ccol.n = subwrd( line.n, 1 )
       clev.n = subwrd( line.n, 2 )
       ccols  = ccols' 'ccol.n
       clevs  = clevs' 'clev.n
       n = n + 1
       endwhile
       say ' NMAX: 'nmax
       say 'CCOLS: 'ccols
       say 'CLEVS: 'clevs
      'set ccolor 0'
       if( cint = NULL )
          'd 'q.1
       else
          'set cint 'cint
          'd 'q.1
       endif
      'set annot  1'
    endif
endif

*******************************************************
* Face 1
*******************************************************

'set vpage 0 11 0 8.5'
if( face = NULL ) ; 'set parea 0.5 3.0 3.0 5.5' ; endif
'set grads off'
'setx'
'sety'

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

y1 = y.1 + 0.0
'set y 1 'y1
say 'Face 1, y = 1 to 'y1

if( face = NULL | face = 1 )

if( scalar = true )
   'set gxout 'gxout
    if( q.3 = A )
        if( q.4 = 1 ) ; 'd 'q.1 ; endif
        if( q.4 = 2 ) ; 'd 'q.2 ; endif
    endif
    if( q.3 = D )
        if( q.4 = 1 ) ; 'd  'q.1 ; endif
        if( q.4 = 2 ) ; 'd  'q.2 ; endif
    endif
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    if( q.3 = A ) ; 'd 'q.1'; 'q.2 ; endif
    if( q.3 = D ) ; 'd 'q.1'; 'q.2 ; endif
endif

'set gxout contour'
'set dfile 'topofile
'setx'
'set y 1 'ytopo.1'.5'
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

if( face = NULL ) ; 'set parea 3.0 5.5 3.00 5.50' ; endif
'set xlab off'
'set ylab off'
'setx'
'sety'

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

y1 = y.1 + 1.0
y2 = y.2 + 0.5
'set y 'y1' 'y2
say 'Face 2, y = 'y1' to 'y2

if( face = NULL | face = 2 )

if( scalar = true )
   'set gxout 'gxout
    if( q.3 = A )
        if( q.4 = 1 ) ; 'd 'q.1 ; endif
        if( q.4 = 2 ) ; 'd 'q.2 ; endif
    endif
    if( q.3 = D )
        if( q.4 = 1 ) ; 'd  'q.1 ; endif
        if( q.4 = 2 ) ; 'd  'q.2 ; endif
    endif
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    if( q.3 = A ) ; 'd 'q.1'; 'q.2 ; endif
    if( q.3 = D ) ; 'd 'q.1'; 'q.2 ; endif
endif

'set gxout contour'
'set dfile 'topofile
'setx'
ytopo1 = ytopo.1 + 1.0
ytopo2 = ytopo.2 + 0.5
'set y 'ytopo1' 'ytopo2
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

if( face = NULL ) ; 'set parea 5.5 8.0 3.0 5.5' ; endif
'set xlab off'
'set ylab off'
'setx'
'sety'

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

y3 = y.3 + 1.0
y4 = y.4 + 0.5
'set y 'y3' 'y4
say 'Face 3, y = 'y3' to 'y4
'set xyrev on'
'set yflip on'

if( face = NULL | face = 4 )
if( scalar = true )
   'set gxout 'gxout
    if( q.3 = A )
        if( q.4 = 1 ) ; 'd 'q.1 ; endif
        if( q.4 = 2 ) ; 'd 'q.2 ; endif
    endif
    if( q.3 = D )
        if( q.4 = 1 ) ; 'd  'q.2 ; endif
        if( q.4 = 2 ) ; 'd -'q.1 ; endif
    endif
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    if( q.3 = A ) ; 'd 'q.1'; 'q.2 ; endif
    if( q.3 = D ) ; 'd 'q.2';-'q.1 ; endif
endif

'set gxout contour'
'set dfile 'topofile
'setx'
'set y 'ytopo.3'.5 'ytopo.4'.5'
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


if( face = NULL ) ; 'set parea 8.0 10.5 3.0 5.5' ; endif
'set xlab off'
'set ylab off'
'setx'
'sety'

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

'set yflip on'
'set y 'y.4'.5 'y.5'.5'

if( face = NULL | face = 5 )
if( scalar = true )
   'set gxout 'gxout
    if( q.3 = A )
        if( q.4 = 1 ) ; 'd 'q.1 ; endif
        if( q.4 = 2 ) ; 'd 'q.2 ; endif
    endif
    if( q.3 = D )
        if( q.4 = 1 ) ; 'd  'q.2 ; endif
        if( q.4 = 2 ) ; 'd -'q.1 ; endif
    endif
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    if( q.3 = A ) ; 'd 'q.1'; 'q.2 ; endif
    if( q.3 = D ) ; 'd 'q.2';-'q.1 ; endif
endif

'set gxout contour'
'set dfile 'topofile
'setx'
'set y 'ytopo.4'.5 'ytopo.5'.5'
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

if( face = NULL | face = 3 | face = 31 )

if( face = NULL ) ; 'set parea 0.5 3.0 5.5 8.0' ; endif
'set xlab off'
'set ylab off'
'setx'
'sety'

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

if( face != 3 )
'set xyrev on'
'set xflip on'
'set yflip off'
endif

'set y 'y.2'.5 'y.3'.5'

if( scalar = true )
   'set gxout 'gxout
    if( q.3 = A )
        if( q.4 = 1 ) ; 'd maskout( maskout( 'q.1',ynp-xx+1) , 'xdim'-ynp-xx+2 )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout( 'q.2',ynp-xx+1) , 'xdim'-ynp-xx+2 )' ; endif
    endif
    if( q.3 = D )
        if( q.4 = 1 ) ; 'd maskout( maskout(-'q.2',ynp-xx+1) , 'xdim'-ynp-xx+2 )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout( 'q.1',ynp-xx+1) , 'xdim'-ynp-xx+2 )' ; endif
    endif
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    if( q.3 = A ) ; 'd maskout( maskout( 'q.1',ynp-xx+1) , 'xdim'-ynp-xx+2 );maskout( maskout( 'q.2',ynp-xx+1) , 'xdim'-ynp-xx+2 )' ; endif
    if( q.3 = D ) ; 'd maskout( maskout(-'q.2',ynp-xx+1) , 'xdim'-ynp-xx+2 );maskout( maskout( 'q.1',ynp-xx+1) , 'xdim'-ynp-xx+2 )' ; endif
endif

'set gxout contour'
'set dfile 'topofile
if( face != 3 )
'set xyrev on'
'set xflip on'
'set yflip off'
endif
'setx'
'set y 'ytopo.2'.5 'ytopo.3'.5'
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

if( face = NULL | face = 3 | face = 32 )

if( face != 3 )
'set xyrev off'
'set xflip off'
'set yflip off'
endif

if( face = NULL ) ; 'set parea 3.0 5.5 5.5 8.0' ; endif
'set xlab off'
'set ylab off'
'setx'
'sety'

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

'set y 'y.2'.5 'y.3'.5'

if( scalar = true )
   'set gxout 'gxout
    if( q.3 = A )
        if( q.4 = 1 ) ; 'd maskout( maskout( 'q.1',xx-ynp  ) , 'xdim'-ynp-xx+1 )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout( 'q.2',xx-ynp  ) , 'xdim'-ynp-xx+1 )' ; endif
    endif
    if( q.3 = D )
        if( q.4 = 1 ) ; 'd maskout( maskout( 'q.1',xx-ynp  ) , 'xdim'-ynp-xx+1 )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout( 'q.2',xx-ynp  ) , 'xdim'-ynp-xx+1 )' ; endif
    endif
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    if( q.3 = A ) ; 'd maskout( maskout( 'q.1',xx-ynp  ) , 'xdim'-ynp-xx+1 );maskout( maskout( 'q.2',xx-ynp  ) , 'xdim'-ynp-xx+1 )' ; endif
    if( q.3 = D ) ; 'd maskout( maskout( 'q.1',xx-ynp  ) , 'xdim'-ynp-xx+1 );maskout( maskout( 'q.2',xx-ynp  ) , 'xdim'-ynp-xx+1 )' ; endif
endif

'set gxout contour'
'set dfile 'topofile
if( face != 3 )
'set xyrev off'
'set xflip off'
'set yflip off'
endif
'setx'
'set y 'ytopo.2'.5 'ytopo.3'.5'
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

if( face = NULL | face = 3 | face = 34 )

if( face = NULL ) ; 'set parea 5.5 8.0 5.5 8.0' ; endif
'set xlab off'
'set ylab off'
'setx'
'sety'

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

if( face != 3 )
'set xyrev on'
'set xflip off'
'set yflip on'
endif

'set y 'y.2'.5 'y.3'.5'

if( scalar = true )
   'set gxout 'gxout
    if( q.3 = A )
        if( q.4 = 1 ) ; 'd maskout( maskout( 'q.1', xx-ynp+1 ), ynp+xx-'xdim' )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout( 'q.2', xx-ynp+1 ), ynp+xx-'xdim' )' ; endif
    endif
    if( q.3 = D )
        if( q.4 = 1 ) ; 'd maskout( maskout( 'q.2', xx-ynp+1 ), ynp+xx-'xdim' )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout(-'q.1', xx-ynp+1 ), ynp+xx-'xdim' )' ; endif
    endif
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    if( q.3 = A ) ; 'd maskout( maskout( 'q.1', xx-ynp+1 ), ynp+xx-'xdim' );maskout( maskout( 'q.2', xx-ynp+1 ), ynp+xx-'xdim' )' ; endif
    if( q.3 = D ) ; 'd maskout( maskout( 'q.2', xx-ynp+1 ), ynp+xx-'xdim' );maskout( maskout(-'q.1', xx-ynp+1 ), ynp+xx-'xdim' )' ; endif
endif

'set gxout contour'
'set dfile 'topofile
if( face != 3 )
'set xyrev on'
'set xflip off'
'set yflip on'
endif
'setx'
'set y 'ytopo.2'.5 'ytopo.3'.5'
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

if( face = NULL | face = 3 | face = 35 )

if( face = NULL ) ; 'set parea 8.0 10.5 5.5 8.0' ; endif
'set xlab off'
'set ylab off'
'setx'
'sety'

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

if( face != 3 )
'set xyrev off'
'set xflip on'
'set yflip on'
endif

'set y 'y.2'.5 'y.3'.5'

if( scalar = true )
   'set gxout 'gxout
    if( q.3 = A )
        if( q.4 = 1 ) ; 'd maskout( maskout( 'q.1', ynp-xx ), ynp+xx-'xdim'-1 )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout( 'q.2', ynp-xx ), ynp+xx-'xdim'-1 )' ; endif
    endif
    if( q.3 = D )
        if( q.4 = 1 ) ; 'd maskout( maskout(-'q.1', ynp-xx ), ynp+xx-'xdim'-1 )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout(-'q.2', ynp-xx ), ynp+xx-'xdim'-1 )' ; endif
    endif
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    if( q.3 = A ) ; 'd maskout( maskout( 'q.1', ynp-xx ), ynp+xx-'xdim'-1 );maskout( maskout( 'q.2', ynp-xx ), ynp+xx-'xdim'-1 )' ; endif
    if( q.3 = D ) ; 'd maskout( maskout(-'q.1', ynp-xx ), ynp+xx-'xdim'-1 );maskout( maskout(-'q.2', ynp-xx ), ynp+xx-'xdim'-1 )' ; endif
endif

'set gxout contour'
'set dfile 'topofile
if( face != 3 )
'set xyrev off'
'set xflip on'
'set yflip on'
endif
'setx'
'set y 'ytopo.2'.5 'ytopo.3'.5'
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

if( face = NULL | face = 6 | face = 61 )

if( face = NULL ) ; 'set parea 0.5 3.0 0.5 3.0' ; endif
'set xlab off'
'set ylab off'
'setx'
'sety'

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

if( face != 6 )
'set xyrev off'
'set xflip off'
'set yflip off'
endif
'set y 'y.5'.5 'ydim

if( scalar = true )
   'set gxout 'gxout
    if( q.3 = A )
        if( q.4 = 1 ) ; 'd maskout( maskout( 'q.1', ysp-xx+1 ), xx+ysp-'xdim' )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout( 'q.2', ysp-xx+1 ), xx+ysp-'xdim' )' ; endif
    endif
    if( q.3 = D )
        if( q.4 = 1 ) ; 'd maskout( maskout( 'q.1', ysp-xx+1 ), xx+ysp-'xdim' )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout( 'q.2', ysp-xx+1 ), xx+ysp-'xdim' )' ; endif
    endif
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    if( q.3 = A ) ; 'd maskout( maskout( 'q.1', ysp-xx+1 ), xx+ysp-'xdim' );maskout( maskout( 'q.2', ysp-xx+1 ), xx+ysp-'xdim' )' ; endif
    if( q.3 = D ) ; 'd maskout( maskout( 'q.1', ysp-xx+1 ), xx+ysp-'xdim' );maskout( maskout( 'q.2', ysp-xx+1 ), xx+ysp-'xdim' )' ; endif
endif

'set gxout contour'
'set dfile 'topofile
if( face != 6 )
'set xyrev off'
'set xflip off'
'set yflip off'
endif
'setx'
'set y 'ytopo.5'.5 'ydimtopo
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

if( face = NULL | face = 6 | face = 62 )

if( face = NULL ) ; 'set parea 3.0 5.5 0.5 3.0' ; endif
'set xlab off'
'set ylab off'
'setx'
'sety'

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

if( face != 6 )
'set xyrev on'
'set xflip on'
'set yflip off'
endif
'set y 'y.5'.5 'ydim

if( scalar = true )
   'set gxout 'gxout
    if( q.3 = A )
        if( q.4 = 1 ) ; 'd maskout( maskout( 'q.1', xx-ysp ), ysp+xx-'xdim' )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout( 'q.2', xx-ysp ), ysp+xx-'xdim' )' ; endif
    endif
    if( q.3 = D )
        if( q.4 = 1 ) ; 'd maskout( maskout(-'q.2', xx-ysp ), ysp+xx-'xdim' )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout( 'q.1', xx-ysp ), ysp+xx-'xdim' )' ; endif
    endif
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    if( q.3 = A ) ; 'd maskout( maskout( 'q.1', xx-ysp ), ysp+xx-'xdim' );maskout( maskout( 'q.2', xx-ysp ), ysp+xx-'xdim' )' ; endif
    if( q.3 = D ) ; 'd maskout( maskout(-'q.2', xx-ysp ), ysp+xx-'xdim' );maskout( maskout( 'q.1', xx-ysp ), ysp+xx-'xdim' )' ; endif
endif

'set gxout contour'
'set dfile 'topofile
if( face != 6 )
'set xyrev on'
'set xflip on'
'set yflip off'
endif
'setx'
'set y 'ytopo.5'.5 'ydimtopo
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

if( face = NULL | face = 6 | face = 64 )

if( face = NULL ) ; 'set parea 5.5 8.0 0.5 3.0' ; endif
'set xlab off'
'set ylab off'
'setx'
'sety'

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

if( face != 6 )
'set xyrev off'
'set xflip on'
'set yflip on'
endif

y5 = y.5 + 1.0
'set y 'y5' 'ydim

if( scalar = true )
   'set gxout 'gxout
    if( q.3 = A )
        if( q.4 = 1 ) ; 'd maskout( maskout( 'q.1', xx-ysp ), 'xdim'-xx-ysp )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout( 'q.2', xx-ysp ), 'xdim'-xx-ysp )' ; endif
    endif
    if( q.3 = D )
        if( q.4 = 1 ) ; 'd maskout( maskout(-'q.1', xx-ysp ), 'xdim'-xx-ysp )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout(-'q.2', xx-ysp ), 'xdim'-xx-ysp )' ; endif
    endif
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    if( q.3 = A ) ; 'd maskout( maskout( 'q.1', xx-ysp ), 'xdim'-xx-ysp );maskout( maskout( 'q.2', xx-ysp ), 'xdim'-xx-ysp )' ; endif
    if( q.3 = D ) ; 'd maskout( maskout(-'q.1', xx-ysp ), 'xdim'-xx-ysp );maskout( maskout(-'q.2', xx-ysp ), 'xdim'-xx-ysp )' ; endif
endif

'set gxout contour'
'set dfile 'topofile
if( face != 6 )
'set xyrev off'
'set xflip on'
'set yflip on'
endif
'setx'
ytopo5 = ytopo.5 + 0.5
'set y 'ytopo5' 'ydimtopo
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

if( face = NULL | face = 6 | face = 65 )

if( face = NULL ) ; 'set parea 8.0 10.5 0.5 3.0' ; endif
'set xlab off'
'set ylab off'
'setx'
'sety'

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

if( face != 6 )
'set xyrev on'
'set xflip off'
'set yflip on'
endif
'set y 'y.5'.5 'ydim

if( scalar = true )
   'set gxout 'gxout
    if( q.3 = A )
        if( q.4 = 1 ) ; 'd maskout( maskout( 'q.1', ysp-xx+1 ), 'xdim'-ysp-xx+1 )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout( 'q.2', ysp-xx+1 ), 'xdim'-ysp-xx+1 )' ; endif
    endif
    if( q.3 = D )
        if( q.4 = 1 ) ; 'd maskout( maskout( 'q.2', ysp-xx+1 ), 'xdim'-ysp-xx+1 )' ; endif
        if( q.4 = 2 ) ; 'd maskout( maskout(-'q.1', ysp-xx+1 ), 'xdim'-ysp-xx+1 )' ; endif
    endif
endif
if( vector = true )
   'set ccolor 1'
    if( cint != NULL ) ; 'set arrscl 1 'cint ; endif
    if( q.3 = A ) ; 'd maskout( maskout( 'q.1', ysp-xx+1 ), 'xdim'-ysp-xx+1 );maskout( maskout( 'q.2', ysp-xx+1 ), 'xdim'-ysp-xx+1 )' ; endif
    if( q.3 = D ) ; 'd maskout( maskout( 'q.2', ysp-xx+1 ), 'xdim'-ysp-xx+1 );maskout( maskout(-'q.1', ysp-xx+1 ), 'xdim'-ysp-xx+1 )' ; endif
endif

if( face = NULL & scalar = true & gxout != Contour  ) ; 'cbarn -ymid 1.00 -snum 0.7' ; endif
'set gxout contour'
'set dfile 'topofile
if( face != 6 )
'set xyrev on'
'set xflip off'
'set yflip on'
endif
'setx'
'set y 'ytopo.5'.5 'ydimtopo
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

if( face != NULL & scalar = true & gxout != Contour  ) ; 'cbarn -ymid 0.50 -xmid 6.0 -snum 0.7' ; endif
'set vpage off'
'set parea off'
'close 'topofile
if( gxout != Contour ) ; 'set gxout 'gxout ; endif
'set arrlab on'
