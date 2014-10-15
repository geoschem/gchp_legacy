*
*  Script to plot a colorbar
*
*  The script will assume a colorbar is wanted even if there is
*  not room -- it will plot on the side or the bottom if there is
*  room in either place, otherwise it will plot along the bottom and
*  overlay labels there if any.  This can be dealt with via
*  the 'set parea' command.  In version 2 the default parea will
*  be changed, but we want to guarantee upward compatibility in
*  sub-releases.
*
*
*       - the extreme colors are plotted as triangles
*       - the colors are boxed in white
*       - optional input arguments (in any order) during a run execution:
*
*       run cbarn < -scale  nn >
*                 < -scalex nn >
*                 < -scaley nn >
*                 < -sbar   nn >
*                 < -snum   nn >
*                 < -xmid   nn >
*                 < -ymid   nn >
*                 < -horz  >
*                 < -vert l or r (Default r:right) >
*
*       scale  - scales both the colorbar and the numbers
*       scalex - scales the x-dimension of the colorbar
*       scaley - scales the y-dimension of the colorbar
*       sbar   - scales only the colorbar
*       snum   - scales only the numbers
*       horz   - FORCES a horizontal bar
*       vert   - FORCES a vertical   bar (left or right, default:right)
*       xmid   - the x position on the virtual page the center the bar
*       ymid   - the x position on the virtual page the center the bar
*
*       if arguments are not specified, they are selected
*       as in the original algorithm
*

function colorbar (args)

'numargs  'args
 numargs = result

    scale  = 1
    scalex = 1
    scaley = 1
      sbar = 1
      snum = 0.8
      xmid = ''
      ymid = ''
      vert = 0
      horz = 1
       num = 0
while( num < numargs )
       num = num + 1
if( subwrd(args,num)='-sbar'  ) ; sbar   = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-snum'  ) ; snum   = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-xmid'  ) ; xmid   = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-ymid'  ) ; ymid   = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-vert'  ) 
    vpos = subwrd(args,num+1)
    if( vpos != 'l' ) ; vpos = 'r' ; endif
    vert = 1
    horz = 0
endif
if( subwrd(args,num)='-horz'  ) ; vert   = 0 ; horz = 1       ; endif
if( subwrd(args,num)='-scalex') ; scalex = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-scaley') ; scaley = subwrd(args,num+1) ; endif
if( subwrd(args,num)='-scale' )
                       scale  = subwrd(args,num+1) 
                       sbar   = scale 
                       snum   = scale 
endif
endwhile

 say 'Numargs = 'numargs
 say '   sbar = '   sbar
 say '   snum = '   snum
 say '   xmid = '   xmid
 say '   ymid = '   ymid
 say '   horz = '   horz
 say '   vert = '   vert
 say ' scale  = '   scale 
 say ' scalex = '   scalex
 say ' scaley = '   scaley

*
*  Check shading information
*
  'query shades'
  shdinfo = result
  if (subwrd(shdinfo,1)='None')
    say 'Cannot plot color bar: No shading information'
    return
  endif

*
*  Get plot size info
*
  'query gxinfo'
  rec2 = sublin(result,2)
  rec3 = sublin(result,3)
  rec4 = sublin(result,4)
  xsiz = subwrd(rec2,4)
  ysiz = subwrd(rec2,6)
  ylo  = subwrd(rec4,4)

  if( vpos = 'r' )
      xhi  = subwrd(rec3,6)
      xd   = xsiz - xhi
  else
      xhi  = 0
      xd   = subwrd(rec3,4)
  endif

  ylolim  = 0.60
  xdlim1  = 1.00
  xdlim2  = 1.50
  barsf   = 0.80
  yoffset = 0.20*scaley
  stroff  = 0.05

  strxsiz = 0.12*snum
  strysiz = 0.13*snum

*  Decide if horizontal or vertical color bar
*  and set up constants.
*
  if (ylo<ylolim & xd<xdlim1)
    say "Not enough room in plot for a colorbar"
    return
  endif
  cint = subwrd(shdinfo,5)
*
*       logic for setting the bar orientation with user overides
*
  if (ylo<ylolim | xd>xdlim1)
    vchk = 1
    if(vert = 0) ; vchk = 0 ; endif
  else
    vchk = 0
    if(vert = 1) ; vchk = 1 ; endif
  endif
*
*       vertical bar
*

  if (vchk = 1 )

    if(xmid = '') ; xmid = xhi+xd/2 ; endif
    xwid = 0.2
    ywid = 0.5*scaley

    xl = xmid-xwid/2
    xr = xl + xwid
    if (ywid*cint > ysiz*barsf)
      ywid = ysiz*barsf/cint
    endif
      ywid = ywid*sbar*scaley
    if(ymid = '') ; ymid = ysiz/2 ; endif
    yb = ymid - ywid*cint/2
    'set string 1 l 5'
    vert = 1

  else

*
*       horizontal bar
*

    ywid = 0.4
    xwid = 0.8

    if(ymid = '') ; ymid = ylo/2-ywid/2 ; endif
    yt = ymid + yoffset
    yb = ymid
    if(xmid = '') ; xmid = xsiz/2 ; endif
    if (xwid*cint > xsiz*barsf)
      xwid = xsiz*barsf/cint
    endif

    xwid = xwid*sbar*scalex
    xl = xmid - xwid*cint/2
    'set string 1 tc 5'
    vert = 0
  endif


*
*  Plot colorbar
*


  'set strsiz 'strxsiz' 'strysiz
  num = 0
  while (num<cint)
    rec = sublin(shdinfo,num+2)
    col = subwrd(rec,1)
    val = subwrd(rec,3)

    if( val < 0 )
        offset = 1
    else
        offset = 0
    endif

* Note:  Only take NDOT values after decimal point  (LT)
         ndot    = 2
         dotloc  = 0
         counter = 1
         while ( counter<20 )
                 dot = substr(val,counter+offset,1)
             if( dot = '.' ) ; dotloc = counter ; endif
                              counter = counter + 1
         endwhile
         if( dotloc=2 )
             dotloc = dotloc+2
             val = substr(val,1,dotloc+offset)
         else 
             if( dotloc=3 )
                 dotloc = dotloc+1
                 val = substr(val,1,dotloc+offset)
             else 
                 if( dotloc!=0 )
                     val = substr(val,1,dotloc+offset)
                 endif
             endif
         endif
*        if( dotloc!=0 )
*            dotloc = dotloc+ndot
*               val = substr(val,1,dotloc)
*        endif
         say 'color = 'col', value = 'val

    if (vert)
      yt = yb + ywid
    else
      xr = xl + xwid
    endif

    if(num!=0 & num!= cint-1)
    'set line 1 1 10'
    'draw rec 'xl' 'yb' 'xr' 'yt
    'set line 'col
    'draw recf 'xl' 'yb' 'xr' 'yt
    if (num<cint-1)
      if (vert)
        xp=xr+stroff
    say 'draw string 'xp' 'yt' 'val
        'draw string 'xp' 'yt' 'val
      else
        yp=yb-stroff
        'draw string 'xr' 'yp' 'val
      endif
    endif
    endif

    if(num = 0 )

      if(vert = 1)

        xm=(xl+xr)*0.5
        'set line 1 1 10'
        'draw line 'xl' 'yt' 'xm' 'yb
        'draw line 'xm' 'yb' 'xr' 'yt
        'draw line 'xr' 'yt' 'xl' 'yt

        'set line 'col
        'draw polyf 'xl' 'yt' 'xm' 'yb' 'xr' 'yt' 'xl' 'yt

      else

        ym=(yb+yt)*0.5
        'set line 1 1 10'
        'draw line 'xl' 'ym' 'xr' 'yb
        'draw line 'xr' 'yb' 'xr' 'yt
        'draw line 'xr' 'yt' 'xl' 'ym

        'set line 'col
       'draw polyf 'xl' 'ym' 'xr' 'yb' 'xr' 'yt' 'xl' 'ym

      endif

    endif

    if (num<cint-1)
      if (vert)
         xp=xr+stroff
        'draw string 'xp' 'yt' 'val
      else
         yp=yb-stroff
        'draw string 'xr' 'yp' 'val
      endif
    endif

    if(num = cint-1 )

      if( vert = 1)
        'set line 1 1 10'
        'draw line 'xl' 'yb' 'xm' 'yt
        'draw line 'xm' 'yt' 'xr' 'yb
        'draw line 'xr' 'yb' 'xl' 'yb

        'set line 'col
        'draw polyf 'xl' 'yb' 'xm' 'yt' 'xr' 'yb' 'xl' 'yb
      else

        'set line 1 1 10'
        'draw line 'xr' 'ym' 'xl' 'yb
        'draw line 'xl' 'yb' 'xl' 'yt
        'draw line 'xl' 'yt' 'xr' 'ym

        'set line 'col
        'draw polyf 'xr' 'ym' 'xl' 'yb' 'xl' 'yt' 'xr' 'ym


      endif

    endif

    if (num<cint-1)
      if (vert)
        xp=xr+stroff
        'draw string 'xp' 'yt' 'val
      else
        yp=yb-stroff
       'draw string 'xr' 'yp' 'val
      endif
    endif

    num = num + 1
    if (vert); yb = yt;
    else; xl = xr; endif;
  endwhile
return

