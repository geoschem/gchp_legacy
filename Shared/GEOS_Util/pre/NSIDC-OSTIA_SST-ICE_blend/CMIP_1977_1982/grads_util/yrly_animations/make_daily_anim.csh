#/bin/csh

set ESMADIR = /discover/nobackup/sakella/GEOSadas-5_11_0_p1/
set myLOC   = $ESMADIR/GEOSadas/src/GMAO_Shared/GEOS_Util/pre/NSIDC-OSTIA_SST-ICE_blend/new_code/merra2/src/before_1982/grads_util/yrly_animations

# set grada env properly from Larry;'s pkg
cd /discover/nobackup/sakella/GEOSadas/src/GMAO_Shared/GEOS_Util/plots/
configure
source .quickplotrc 

cd $myLOC

# now make those plots
set YYYY	= 1982

grads -blcx $myLOC/plot_CMIP.gs

convert -delay 60 -loop 0 $myLOC/CMIP_*.gif  CMIP_Jan${YYYY}_Dec${YYYY}.gif

#mv $myLOC/Reynolds_Jan${YYYY}_Dec${YYYY}.gif $myLOC/done/

#rm  -f Reynolds_*.gif
##rm -f $myLOC/Reynolds_*.gif
#

