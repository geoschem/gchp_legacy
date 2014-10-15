#!/bin/sh
************************************************
'reinit'
'set display color white'
'clear'
*
'open CMIP_sst.ctl'
'open CMIP_ice.ctl'
'set grads off'
*
timex = 1

while( timex <= 366)
*PLOT SST 
'subplot 2 1 1'
'set mproj latlon'
'set lon -180 180'
'set lat -90 90'
'set gxout grfill'
'set clevs 275.0 280.0 285.0 290.0 295.0 297.5 300.0 302.5 305.0'
'set t ' timex
'display sst'
'run basemap L'
'query time'

cctime=subwrd(result,3)
coln=substr(cctime,3,1)
if coln = ':'
dmy   =substr(cctime,7,9)
else
*
YYYY = substr(cctime,9,12)
*
TEMPstr  = substr(cctime,4,5)
DD  = substr(TEMPstr,1,2)
MM  = substr(TEMPstr,3,5)
endif
'draw title ' DD' ' MM'  ' YYYY' '
'run cbar  0.7 1 9.0 6.1'
*
*PLOT FRACI
*NH
'subplot 2 2 3'
'set lon -180 180'
'set gxout grfill'
'set clevs 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.80 0.90 1.0'
'set mproj nps'
'set lat 45 90'
'display ice.2'
'run basemap L'
*SH
'subplot 2 2 4'
'set clevs 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.80 0.90 1.0'
'set mproj sps'
'set lat -90 -45'
'display ice.2'
'run basemap L'
'run cbar  0.63 1 5.4 2.2'
*
*****************************************
if (timex<10)
'printim CMIP_00'timex'.gif x600 y400'
endif

if (timex >= 10 & timex <100)
'printim CMIP_0'timex'.gif x600 y400'
endif

if (timex >= 100)
'printim CMIP_'timex'.gif x600 y400'
endif

timex = timex + 1
'clear'
endwhile
**
*****************************************
*grads -blcx XX.gs
*convert -delay 60 -loop 0 CMIP_*.png  CMIP_Jan1983_Dec1983.gif
*mv CMIP_Jan1983_Dec1983.gif /discover/nobackup/sakella/GEOSadas-5_11_0_p1/GEOSadas/src/GMAO_Shared/GEOS_Util/pre/NSIDC-OSTIA_SST-ICE_blend/new_code/merra2/src/grads_util/yrly_animations/done/
*rm -f CMIP_*.png
*****************************************


