function pltmap (args)

*******************************************************
****     exp1 is 1st experiment to compare         ****
****     exp2 is 2nd experiment to compare         ****
****     cint Optional argument to hardwire CINT   ****
*******************************************************

'setenv "ANTIALIAS" TRUE'
'set display color white'
'rgbset'
'c'

exp1  = subwrd(args,1)
exp2  = subwrd(args,2)
cint0 = subwrd(args,3)

types = 'rms std fma mes mse'
types = 'rms std'

'numargs   'types
 numtypes = result

k = 1
while ( k<=numtypes )
              type = subwrd(types,k)
              cint = cint0

*******************************************************
****              Horizontal Means                 ****
*******************************************************

if( cint != '' )
 if( type = fma ) ; cint = 0.5 ;endif
 if( type = rms ) ; cint = 0.5 ;endif
 if( type = std ) ; cint = 0.5 ;endif
 if( type = mse ) ; cint = 50  ;endif
 if( type = mes ) ; cint = 50  ;endif
endif

'set dfile 1'
'getinfo month'
         month = result
'setlons'
'sety'
'set lev 1000'
'movie statdplt.gs "p 'type' 'exp1' 'exp2' 'cint'" -name stats_slp_'type'_GLO_1000_'month' -print -rotate 90'
'c'

fields = 'h u v t'
levels = '1000 850 700 500 400 300 250 200 150 100'

'numargs  'levels
 numlevs = result

'numargs  'fields
 numflds = result

* Loop over Fields
* ----------------
n = 1
while ( n<=numflds )
              field = subwrd(fields,n)
              cint  = cint0
'set dfile 1'
'setlons'
'sety'

if( field = h )
     name = hght 
 if( cint != '' )
     if( type = fma ) ; cint = 5   ;endif
     if( type = rms ) ; cint = 5   ;endif
     if( type = std ) ; cint = 5   ;endif
     if( type = mse ) ; cint = 500 ;endif
     if( type = mes ) ; cint = 70  ;endif
 endif
endif

if( field = u )
     name = uwnd 
 if( cint != '' )
     if( type = fma ) ; cint = 0.8 ;endif
     if( type = rms ) ; cint = 0.8 ;endif
     if( type = std ) ; cint = 0.8 ;endif
     if( type = mse ) ; cint = 8   ;endif
     if( type = mes ) ; cint = 2   ;endif
 endif
endif

if( field = v )
     name = vwnd
 if( cint != '' )
     if( type = fma ) ; cint = 0.8 ;endif
     if( type = rms ) ; cint = 0.8 ;endif
     if( type = std ) ; cint = 0.8 ;endif
     if( type = mse ) ; cint = 8   ;endif
     if( type = mes ) ; cint = 2   ;endif
 endif
endif

if( field = t )
     name = tmpu
 if( cint != '' )
     if( type = fma ) ; cint = 0.2 ;endif
     if( type = rms ) ; cint = 0.2 ;endif
     if( type = std ) ; cint = 0.2 ;endif
     if( type = mse ) ; cint = 2   ;endif
     if( type = mes ) ; cint = 0.2 ;endif
 endif
endif

* Loop over Levels
* ----------------
z = 1
while ( z<=numlevs )
              level = subwrd(levels,z)
'set dfile 1'
'set lev 'level
'c'
'movie statdplt.gs "'field' 'type' 'exp1' 'exp2' 'cint'" -name stats_'name'_'type'_GLO_'level'_'month' -print -rotate 90'
'c'
z = z + 1
endwhile


*******************************************************
****                Vertical Means                 ****
*******************************************************

if( field = h )
 if( cint != '' )
     cint = 2
     if( type = mse ) ; cint = 100 ;endif
     if( type = mes ) ; cint = 10  ;endif
 endif
endif
if( field = u )
 if( cint != '' )
     cint = 0.2
     if( type = mse ) ; cint = 5   ;endif
     if( type = mes ) ; cint = 0.5 ;endif
 endif
endif
if( field = v )
 if( cint != '' )
     cint = 0.2
     if( type = mse ) ; cint = 5   ;endif
     if( type = mes ) ; cint = 0.5 ;endif
 endif
endif
if( field = t )
 if( cint != '' )
     cint = 0.1
     if( type = mse ) ; cint = 1   ;endif
     if( type = mes ) ; cint = 0.2 ;endif
 endif
endif

'set dfile 1'
'set x 1'
'sety'
'setz'
'c'
'movie statdpltz.gs "'field' 'type' 'exp1' 'exp2' 'cint'" -name stats_'name'_'type'_GLO_z_'month' -print -rotate 90'
'c'

* End Field Loop
* --------------
n = n + 1
endwhile

* End Type Loop
* -------------
k = k + 1
endwhile

return
