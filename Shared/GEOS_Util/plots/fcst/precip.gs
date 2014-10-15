function precip

'set grads off'

'run getenv "GEOSUTIL"'
         geosutil = result

'set dfile 1'
'setx'
'sety'
'getinfo xdim'
         xdim  = result
'getinfo ydim'
         ydim  = result
'getinfo numfiles'
         numfiles = result

'set x 1'
'set y 1'
'sett'
'define precipave = lev-lev'
                                                                                                                                 
n = 1
while ( n <= numfiles )
'set dfile 'n
'set axlim 0.0 4.0'
'set ylab %.2f'
'sett'
'set xaxis 0 5 .5'
'set cmark 0'
'set ccolor 2'
'set cthick 3'
'setx'
'sety'
'define temp1 = (cn_prcp+an_prcp+ls_prcp)'
'define temp2 = (cn_prcp                )'
'define temp3 = (        an_prcp        )'
'define temp4 = (                ls_prcp)'
'define temp5 = (                   evap)'
'define temp6 = (                    tpw)'
'set x 1'
'set y 1'
'define gg1 = aave(temp1,x=1,x='xdim',y=1,y='ydim')*86400'
'define gg2 = aave(temp2,x=1,x='xdim',y=1,y='ydim')*86400'
'define gg3 = aave(temp3,x=1,x='xdim',y=1,y='ydim')*86400'
'define gg4 = aave(temp4,x=1,x='xdim',y=1,y='ydim')*86400'
'define gg5 = aave(temp5,x=1,x='xdim',y=1,y='ydim')*86400'
'define gg6 = aave(temp6,x=1,x='xdim',y=1,y='ydim')*0.1'
'd gg1'
'set cmark 0'
'set ccolor 4'
'set cthick 3'
'd gg2'
'set cmark 0'
'set ccolor 5'
'set cthick 3'
'd gg3'
'set cmark 0'
'set ccolor 3'
'set cthick 3'
'd gg4'
'set cmark 0'
'set ccolor 1'
'set cthick 3'
'd gg5'
'set cmark 0'
'set ccolor 6'
'set cthick 3'
'd gg6'
n = n + 1
endwhile
'draw ylab Globally Averaged Precipitation & Evaporation (mm/day)'
'draw xlab Forecast Day'
'lines 'geosutil'/plots/fcst/precip.stack 1'
                                                                                                                                 
'set string 1 c 6'
'set strsiz .14'
'draw string 6.22324 8.0 e0289 GEOSagcm-Eros_7_21 (144x91x72)  Forecasts (F-00z) for Jan 2004'
