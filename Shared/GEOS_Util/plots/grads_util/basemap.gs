* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* basemap.gs
*
* This script overlays a land or ocean mask that exactly fits
* the low-resolution coastal outlines. The masks are composed
* of about 700 polygons that are specified in the accompanying
* ascii files lpoly.asc and opoly.asc. It will work with any
* scaled or latlon map projection.
*
* If you are using Grads version 1.8 or higher, this script
* will also work properly with the robinson projection and
* polar stereographic projections from 0-90, 15-90, and 20-90
* (North and South).
*
* Other projections will work but are not guaranteed because
* GrADS may not clip the basemap properly. A solution to this
* problem is to use "set mpvals" to override the dimension
* environment limits. For example:
*    set mproj nps
*    set lon -180 180
*    set lat 0 90
*    set mpvals -180 180 60 90
*    display <something>
*    basemap L
* The resulting plot will be a properly clipped square.
*
* Written by J.M. Adams, December 2000
* Updated by J.M. Adams,  January 2001
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function main(args)
                                                                                                                                                   
* There are defaults for the colors, but user must specify which mask
  if (args='')
    say 'Usage: basemap L(and)/O(cean) <fill_color> <outline_color>'
    return
  else
    type    = subwrd(args,1)
    color   = subwrd(args,2)
    outline = subwrd(args,3)
    if (color = '')   ; color = 15  ; endif
    if (outline = '') ; outline = 0 ; endif
  endif
                                                                                                                                                   
'run getenv "GEOSUTIL"'
             GEOSUTIL = result
  if (type = 'L' | type = 'l')
    file = GEOSUTIL'/plots/grads_util/lpoly.asc'
  endif
  if (type = 'O' | type = 'o')
    file = GEOSUTIL'/plots/grads_util/opoly.asc'
  endif
 
* Make sure there's a plot already drawn
  'q gxinfo'
  line5 = sublin(result,5)
  line6 = sublin(result,6)
  xaxis = subwrd(line5,3)
  yaxis = subwrd(line5,6)
  proj  = subwrd(line6,3)
  if (xaxis = 'None' | yaxis = 'None')
    say 'Error: Please display a variable before using basemap'
    return
  endif
* See what version of Grads is running
  'q config'
  line = sublin(result,1)
  word = subwrd(line,2)
  version = substr(word,2,3)
  if (version >= 1.8)
    newgrads = 1
  else
    newgrads = 0
  endif
 
* See if map projection will be supported
  okmap = 0
  if (newgrads = 0)
    if (proj != 1 & proj != 2)
      say 'Error: Only scaled or latlon projections are supported with GrADS v'version
      return
    endif
  else
    if (proj >= 3)
    endif
  endif
 
* Clip image accordingly
  'q gxinfo'
  line3 = sublin(result,3)
  line4 = sublin(result,4)
  x1 = subwrd(line3,4)
  x2 = subwrd(line3,6)
  y1 = subwrd(line4,4)
  y2 = subwrd(line4,6)
  'set clip 'x1' 'x2' 'y1' 'y2
* Read the first record from the polygon file
  result = read(file)
  rc = sublin(result,1)
  rc = subwrd(rc,1)
  if (rc!=0)
    say 'Error reading 'file
    return
  endif
  nwcmd = sublin(result,2)
 
* Read subsequent records, allowing for read input buffer overflow
  flag = 1
  while (flag)
    ignore = 0
    wcmd = nwcmd
    while(1)
      result = read(file)
      rc = sublin(result,1)
      rc = subwrd(rc,1)
      if (rc!=0)
        flag = 0
        break
      else
        nwcmd = sublin(result,2)
        if (subwrd(nwcmd,5) != 'draw')
          wcmd = wcmd % nwcmd
        else
          break
        endif
      endif
    endwhile
*   Get the lat/lon range of the current dimension environment
    'q dims'
    line1 = sublin(result,2)
    line2 = sublin(result,3)
    minlon = subwrd(line1,6)
    maxlon = subwrd(line1,8)
    minlat = subwrd(line2,6)
    maxlat = subwrd(line2,8)
 
*   The range of the polygon is specified in the first four words of the record
    minwx = subwrd(wcmd,1)
    maxwx = subwrd(wcmd,2)
    minwy = subwrd(wcmd,3)
    maxwy = subwrd(wcmd,4)
 
*   If the polygon is outside the current dimension, ignore it
    if (minwx >= maxlon) ; ignore = 1 ; endif
    if (maxwx <= minlon) ; ignore = 1 ; endif
    if (minwy >= maxlat) ; ignore = 1 ; endif
    if (maxwy <= minlat) ; ignore = 1 ; endif
 
    if (!ignore)
      count = 7
      nvert = 1
      if (newgrads)
        cmd = 'draw mappoly '
      else
        cmd = 'draw polyf '
      endif
      while (1)
        countx = count
        county = count + 1
        wx = subwrd(wcmd,countx)
        wy = subwrd(wcmd,county)
        if ((wx = '') | (wy = ''))
          break
        endif
*       Convert world coordinates to screen coordinates if necessary
        if (newgrads)
          sx = wx
          sy = wy
        else
          'q w2xy 'wx' 'wy
          sx = subwrd(result,3)
          sy = subwrd(result,6)
        endif
 
*       Append the coordinates to the draw command
        cmd = cmd%sx' 'sy' '
        count = count + 2
      endwhile
 
*     Draw the polygon
      'set line 'color
      cmd
    endif
  endwhile
 
* Draw the continental outline
  'set mpt * 'outline
  'draw map'
 
* Close the polygon file
  close(file)
  return
 

