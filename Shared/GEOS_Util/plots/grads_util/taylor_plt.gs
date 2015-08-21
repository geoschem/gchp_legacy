function plot (args)

* eg: taylor_plt num expid_1 expid_2 ... expid_num output_dir output_name 'Output Title in Quotes' debug
*
* where:  num is the number of experiments being compared
*         expid_1 expid_2 ... expid_num are the 'tagged' names for each experiment
*         'blah blah blah' .. Title Description for Taylor Plot
*         debug ............. Debug Flag
*
* For each experiment we have already computed the Taylor statistics:
*
* eg: taylor olrdjfmod olrdjfobs djf expid
*     taylor olrmammod olrmamobs mam expid
*     taylor olrjjamod olrjjaobs jja expid
*     taylor olrsonmod olrsonobs son expid
*     taylor olrannmod olrannobs ann expid

n = 1
dum = subwrd(args,n)
while( dum != "" )
n = n + 1
dum = subwrd(args,n)
endwhile
n = n - 1
debug = subwrd(args,n)

* Get Current DFILE Environment
* -----------------------------
'getinfo file'
        dfile = result

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

* Open HiRes File for Taylor Plot
* -------------------------------
'run getenv "GEOSUTIL"'
             geosutil = result
'open   'geosutil'/plots/grads_util/lwmask1440721.tabl'
'getinfo numfiles'
            file = result
'set dfile 'file
'set t 1'
'set z 1'


* Determine Number (num) of Experiments to Compare (5 Max)
* --------------------------------------------------------
num = subwrd(args,1)
expid = subwrd(args,num+1)

k = 1
while( k <= num )
          name.k = subwrd(args,k+1)
'fixname 'name.k
          name.k = substr(result,1,8)
say 'k = 'k' name.'k' = 'name.k
k = k+1
endwhile

output = subwrd(args,num+2)
name   = subwrd(args,num+3)
title  = titles(args,1)

'set vpage off'
'rgbset'

* Set Experiment Colors and Plot Areas
* ------------------------------------
n = 1
while( n<=num   )
   if( n =num   ) ; col.n = 2  ; endif
   if( n =num-1 ) ; col.n = 4  ; endif
   if( n =num-2 ) ; col.n = 3  ; endif
   if( n =num-3 ) ; col.n = 54 ; endif
   if( n =num-4 ) ; col.n = 68 ; endif
   if( n =num-5 ) ; col.n = 1  ; endif
   if( n =num-6 ) ; col.n = 15 ; endif
n = n + 1
endwhile

season.1 = "djf"
season.2 = "mam"
season.3 = "jja"
season.4 = "son"
season.5 = "ann"


* Loop over Normal and Zoomed Plots
* ---------------------------------
       type = 1
while( type<= 2 )

if( type = 1 )
    parea.1 = "0.75 3.50 7.00 9.75"
    parea.2 = "4.50 7.25 7.00 9.75"
    parea.3 = "0.75 3.50 3.75 6.50"
    parea.4 = "4.50 7.25 3.75 6.50"
    parea.5 = "0.75 3.50 0.50 3.25"
else
    parea.1 = "0.75 3.50 7.00 9.75"
    parea.2 = "5.00 7.75 7.00 9.75"
    parea.3 = "0.75 3.50 3.75 6.50"
    parea.4 = "5.00 7.75 3.75 6.50"
    parea.5 = "0.75 3.50 0.50 3.25"
endif

* Make Taylor Diagram Plot
* ------------------------
       m = 1
while( m<= 5 )
'set parea 'parea.m
'set x 1'
'set y 1'
xvals = ' '
yvals = ' '
cvals = ' '
       n = 1
while( n<=num )
'                         d  corr'season.m''name.n
  c.n = subwrd(result,4)
'getxy std'season.m''name.n' corr'season.m''name.n
  x.n = subwrd(result,1)
  y.n = subwrd(result,2)
xvals = xvals %  x.n % ' '
yvals = yvals %  y.n % ' '
cvals = cvals %  c.n % ' '
    n = n+1
endwhile

'minmax  'xvals
   xmax = subwrd(result,1)
   xmin = subwrd(result,2)
'minmax  'yvals
   ymax = subwrd(result,1)
   ymin = subwrd(result,2)
'minmax  'cvals
   cmax = subwrd(result,1)
   cmin = subwrd(result,2)

       n = 1
while( n<=num )
'taylor.graph'type' std'season.m''name.n' corr'season.m''name.n' 'col.n' 'xmin' 'xmax' 'ymin' 'ymax' 'cmin' 'cmax
       n = n+1
endwhile
m = m + 1
endwhile


* Draw Labels for Each Experiment
* -------------------------------
 yloc = 2.9
 xbeg = 4.7
 xend = 5.2
 xloc =  xend+0.3
 xmid = (xbeg+xend)/2
'set vpage off'
 n = num
while( n>=1 )
  'set string 1 l 5'
  'set line 'col.n' 1 6'
  'draw line 'xbeg' 'yloc' 'xend' 'yloc
  'draw mark 3 'xmid' 'yloc' 0.1'
   if( n=num )
      'draw string 'xloc' 'yloc' 'expid
   else
      'draw string 'xloc' 'yloc' 'name.n
   endif
  'draw string 'xloc' 'yloc' 'name.n
   yloc = yloc-0.2
   n = n - 1
endwhile


* Draw Titles
* -----------
'set string 1 c 6'
'set strsiz .15'
'set ccolor 1'
'draw string 4.46369 10.7421 GMAO Model Development'
'set strsiz .13'
'draw string 4.46369 10.4421 'title

'set string 1 c 6'

if( type = 1 )
   'draw string 3.08345 9.60 DJF'
   'draw string 6.83517 9.60 MAM'
   'draw string 3.08345 6.28 JJA'
   'draw string 6.83517 6.28 SON'
   'draw string 3.08345 3.03 ANN'
else
   'set strsiz .10'
   'draw string 0.50 9.60 DJF'
   'draw string 4.75 9.60 MAM'
   'draw string 0.50 6.28 JJA'
   'draw string 4.75 6.28 SON'
   'draw string 0.50 3.03 ANN'
endif

'myprint -name 'output'/taylor_'name'_'type


* End Loop for Normal and Zoomed Plots
* ------------------------------------
say 'Type = 'type
if( debug = "debug" )
    say 'Hit Enter to Continue'
    pull flag
endif
'c'
  type = type + 1
endwhile

'set dfile 'dfile
'set x 'x1' 'x2
'set y 'y1' 'y2

return


function titles (args,num)

*************************************************************
*****                                                   *****
*****  Usage: title_num = titles (args,num)             *****
*****                                                   *****
*****  Extracts a title (defined between single quotes) *****
*****  from an argument list.  NUM is the desired title *****
*****  you want.  ARGS can contain any arbritray number *****
*****  of titles and non-title information.             *****
*****                                                   *****
*************************************************************

      max = 120
      loc = 1
        m = 0

* Position loc to be at desired title
* -----------------------------------
while ( m != num )
  title = subwrd(args,loc)
  first = substr(title,1,1)
  if( first = "'" ) ; m=m+1 ; endif
  loc = loc + 1
  say 'm = 'm'  loc = 'loc
endwhile

* Construct title information
* ---------------------------
end = 0
while (end=0 & loc<max )
tnxt = subwrd(args,loc)
title = title % ' ' % tnxt
              n = 1
      while ( n<max )
      last = substr(tnxt,n,1)
      if( last="'" | last="" )
              n = max
      else
              n = n+1
      endif
      if( last="'" & m=num ) ; end = 1 ; endif
      endwhile
loc = loc + 1
endwhile

* Extract title between quotes
* ----------------------------
               n = 2
       while ( n<max )
       last = substr(title,n,1)
       if( last = "'" )
           len = n-2
             n = max
       else
             n = n+1
       endif
       endwhile
       title = substr( title,2,len )
return title
