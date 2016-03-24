function  res_2 (args)

 expid  = subwrd(args,1)
 season = subwrd(args,2)
 output = subwrd(args,3)
 obsid  = subwrd(args,4)
 obsdsc = subwrd(args,5)
 nmod   = subwrd(args,6)
 nobs   = subwrd(args,7)
 expdsc = subwrd(args,8)
 begdateo = subwrd(args,9)
 enddateo = subwrd(args,10)

* Get Dates for Plots
* -------------------
'run getenv "BEGDATE"'
             begdate  = result
'run getenv "ENDDATE"'
             enddate  = result

'run getenv "CINTDIFF"'
             CINTDIFF = result

'run getenv "CLIMATE"'
             climate = result
if(begdate = begdateo & enddate = enddateo )
             climate = 'Actual'
endif


'rgbset'
'set rgb 84 204 204 204'
'set rgb 85 137 137 137'

********************************************************

if( CINTDIFF != NULL )
* --------------------

'getinfo file'
        dfile = result

* Get Bounding Values
* -------------------
'getinfo xdim'
         xdim = result
'getinfo ydim'
         ydim = result
'getinfo zdim'
         zdim = result

* Set x dimension
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

* Set y dimension
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

* Set z dimension
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

'run getenv "ZDIFILE" '
             zdifile = result
'set dfile ' zdifile
'q file'
say 'ZDIFILE: 'result
'set x 1'
'set t 1'
'sety'
'setz'

'minmax strdifz'
        dqmax = subwrd(result,1)
        dqmin = subwrd(result,2)

* Reset initial space-time environment
* ------------------------------------
'set dfile 'dfile
'set x 'x1' 'x2
'set y 'y1' 'y2
'set z 'z1' 'z2
'set t 1'

endif

********************************************************
*                   Streamfunction
********************************************************

'set vpage off'
'set parea off'

* Make Plot: Top Panel
* --------------------
'set vpage 0 8.5 0.0 11'
'set parea 1.5 7.0 7.70 10.50'
'set grads off'

'set gxout shaded'
'set clevs 0'
'set ccols 84 0'
'set t 1'
'd str'season'mod'
'set gxout contour'
'set clopts 1 3 0.06'
'set ccolor 1'
'set cint 2'
'set cmax  40'
'set cmin -40'
'd str'season'mod'


* Make Plot: Middle Panel
* -----------------------
'set parea off'
'set vpage 0 8.5 0.0 11'
'set parea 1.5 7.0 4.30 7.10'
'set grads off'

'set gxout shaded'
'set clevs 0'
'set ccols 84 0'
'set t 1'
'd str'season'obs'
'set gxout contour'
'set clopts 1 3 0.06'
'set ccolor 1'
'set cint 2'
'set cmax  40'
'set cmin -40'
'd str'season'obs'


* Make Plot: Bottom Panel
* -----------------------
'set parea off'
'set vpage 0 8.5 0.0 11'
'set parea 1.5 7.0 0.90 3.70'
'set grads off'

'set gxout shaded'
'set clevs 0'
'set ccols 84 0'
'set t 1'
'd str'season'obs'

if( CINTDIFF != NULL )
* --------------------

    say 'DQMAX: 'dqmax
    say 'DQMIN: 'dqmin

   'd abs('dqmin')'
           dqmin = subwrd(result,4)
   'd abs('dqmax')'
           dqmax = subwrd(result,4)
   if( dqmin > dqmax ) ; dqmax = dqmin ; endif
   if( dqmax > 0 )
      'd log10('dqmax')'
       dn = subwrd(result,4)
   else
       dn = 0
   endif
   say '    Log Factor: 'dn
   if( dn<0 ) ; dn = dn-2 ; endif
   'getint 'dn
            dn = result
   if( dn>0 )
       if( dn<=2 )
           dn = 0
        else
           dn = dn+2
        endif
   endif
   if( dn<0 )
       dm = -dn
   else
       dm =  dn
   endif

   say 'Scaling Factor: 'dn

     if( dn>0 )
       'd 0.1*'dqmax'/1e'dm
        cint = subwrd(result,4)
        say 'dn> 0,  CINT: 'cint
       'shades 'cint
       'd maskout( strdifz/1e'dm',abs(strdifz/1e'dm')-'cint' )'
     else
       'd 0.1*'dqmax'*1e'dm
        cint = subwrd(result,4)
        say 'dn< 0,  CINT: 'cint
       'shades 'cint
       'd maskout( strdifz*1e'dm',abs(strdifz*1e'dm')-'cint' )'
     endif

else

     dn = 0
    'shades 0.3'
    'd maskout( strdifz,abs(strdifz)-0.3 )'

endif

'cbarn -scale 0.9 -snum 0.5'
'set gxout contour'
'set clopts 1 3 0.06'
'set ccolor 1'
'set cint 2'
'set cmax  40'
'set cmin -40'
'd str'season'obs'
 

'set vpage off'
'set parea off'
'set vpage 0 8.5 0.0 11'

'set string 1 c 6'
'set strsiz .11'

'draw string 4.25 10.6 EXPID: 'expid'  'expdsc'  'season' ('nmod')'
'draw string 4.25  7.23 'obsdsc'  'season' ('nobs') ('climate')'

if( dn != 0 )
'draw string 4.25  3.850 Middle Plot:Contour  +  Difference (Top-Middle):Shaded (x 10**'dn')'
else
'draw string 4.25  3.850 Middle Plot:Contour  +  Difference (Top-Middle):Shaded'
endif

                date = getdate (begdate)
bmnthm = subwrd(date,1)
byearm = subwrd(date,2)
                date = getdate (enddate)
emnthm = subwrd(date,1)
eyearm = subwrd(date,2)
                date = getdate (begdateo)
bmntho = subwrd(date,1)
byearo = subwrd(date,2)
                date = getdate (enddateo)
emntho = subwrd(date,1)
eyearo = subwrd(date,2)

'set string 1 l 4'
'set strsiz .08'
'draw string 0.10 10.37 Beg: 'bmnthm' 'byearm
'draw string 0.10 10.24 End: 'emnthm' 'eyearm
'draw string 0.10  6.97 Beg: 'bmntho' 'byearo
'draw string 0.10  6.84 End: 'emntho' 'eyearo
'set string 1 c 6'

'myprint -name 'output'/zonal_'obsid'_str.'season

'set clab on'
'set clopts 1 3 0.09'

return

function getdate (date,month,year)
       num = 1
       bit = substr(date,num,1)
while( bit != '' )
       num = num+1
       bit = substr(date,num,1)
endwhile
       loc = num-7
     month = substr(date,loc  ,3)
      year = substr(date,loc+3,4)
return month' 'year

