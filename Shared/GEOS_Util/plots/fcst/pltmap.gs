function pltmap (args)

*******************************************************
****                 INPUT Variables               ****
*******************************************************

'numargs  'args
 numargs = result

SOURCE = NULL
DESC   = Stats

        num = 0
while ( num < numargs )
        num = num + 1

if( subwrd(args,num) = '-source' ) ; SOURCE = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-desc'   ) ; DESC   = subwrd(args,num+1) ; endif

endwhile

if( SOURCE != NULL ) 
   'run setenv "SOURCE" 'SOURCE
else
   'run getenv "PWD"'
            PWD = result
   'run setenv "SOURCE" 'PWD
endif
   'run getenv "SOURCE"'
            SOURCE = result

   'run getenv "GEOSUTIL"'
            geosutil = result

*******************************************************
****                    Begin ...                  ****
*******************************************************

'run setenv "ANTIALIAS" TRUE'

'reinit'
'exec setup3'
'set display color white'
'rgbset'
'c'

'set t 1'
'getinfo month'
         month = result

'set lev 1000'
'statmak p'
'c'
'movie statplt "p -desc 'DESC'" -print -rotate 90 -name stats_slp_all_GLO_1000_'month
'c'

fields = 'h u v t q'
levels = '1000 850 700 500 400 300 250 200 150 100'

'numargs  'levels
 numlevs = result

'numargs  'fields
 numflds = result

n = 1
while ( n<=numflds )
              field = subwrd(fields,n)
          if( field = h ) ; name = hght ; endif
          if( field = u ) ; name = uwnd ; endif
          if( field = v ) ; name = vwnd ; endif
          if( field = t ) ; name = tmpu ; endif
          if( field = q ) ; name = sphu ; endif

'set dfile 1'
'setlons'
'sety'
'setz'
'statmak 'field

z = 1
while ( z<=numlevs )
              level = subwrd(levels,z)
'set dfile 1'
'set lev 'level
'c'
'movie statplt "'field' -desc 'DESC'" -print -rotate 90 -name stats_'name'_all_GLO_'level'_'month
'c'
z = z + 1
endwhile

'set dfile 1'
'set x 1'
'sety'
'setz'
'c'
'movie statpltz "'field' -desc 'DESC'" -print -rotate 90 -name stats_'name'_all_GLO_z_'month
'c'

n = n + 1
endwhile


return
