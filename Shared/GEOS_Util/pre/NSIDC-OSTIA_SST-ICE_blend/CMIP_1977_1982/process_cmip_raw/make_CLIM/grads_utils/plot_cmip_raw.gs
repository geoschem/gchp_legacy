************************************************
'reinit'
'set display color white'
'clear'
'set grads off'
*
*SST
'open cmip_raw_sst.ctl'
'subplot 2 1 1'
'set mproj latlon'
'set lon -180 180'
'set lat   -90 90'
'set gxout grfill'
'set clevs 280.0 282.5 285.0 287.5 290.0 292.5 295.0 297.5 300.0 302.5 305.0'
'display sst'
*'run basemap L'
'draw title CMIP RAW (1/4 deg) SST 01/01'
'run cbar 0.6 1 9.1 6.2'
*................................................................
'open cmip_raw_sic.ctl'
'set dfile 2'
'subplot 2 1 2'
'set mproj latlon'
'set lon -180 180'
'set lat   -90 90'
'set gxout grfill'
'set clevs 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.80 0.90 1.0'
'display ice'
*'run basemap L'
'run cbar 0.6 1 9.1 2.2'
*................................................................
*NH ICE
*'subplot 2 2 3'
*'set mproj nps'
*'set lon -180 180'
*'set lat 30 90'
*'set gxout grfill'
*'set clevs 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.80 0.90 1.0'
*'display ice'
*'run basemap L'
*SH
*'subplot 2 2 4'
*'set mproj sps'
*'set lon -180 180'
*'set lat -90 -30'
*'set gxout grfill'
*'set clevs 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.80 0.90 1.0'
*'display ice'
*'run basemap L'
*'run cbar 0.5 1 5.4 2.1'
*................................................................
*................................................................
*
*
*'printim cmip_raw_0101.png x1000 y800'
*

