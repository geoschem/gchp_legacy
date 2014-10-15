#!/usr/bin/python
#-----------------------------------------------------------------
'''
this script writes out lower boundary conditions (sst & Sea Ice Concentration) based on:
1. CMIP daily data, 2. CMIP daily climatology, 3. Reynolds daily climatology.

[1] was generated using: a. mid-month 1x1 deg files from PCMDII, b. interp to 0.25x0.25 MERRA res., and c. generated linearly interpolated daily bin files.
See /discover/nobackup/sakella/processData/MERRA2_27June2013/UTILS/FOR_CMIP/

[2] & [3] contain climatology for each day of the year based on daily CMIP & Reynolds respectively.
for [3], see /discover/nobackup/sakella/processData/MERRA2_27June2013/UTILS/

also see /archive/u/sakella/MERRA2/bc_data/

Santha Akella 14 AUG 2013.
'''
#-----------------------------------------------------------------
import os
import sys

import  pylab            as      pylab
import  numpy            as      numpy
import  time             as      time

import  matplotlib.dates        as      md
import  matplotlib.pyplot       as      plt

from    datetime        import  datetime, timedelta

sys.path.append('/discover/nobackup/sakella/GEOSadas-5_11_0_p1/GEOSadas/src/GMAO_Shared/GEOS_Util/pre/NSIDC-OSTIA_SST-ICE_blend/new_code/merra2/src/before_1982/')
from read_bin import read_bin
import write_cmip_bin
#-----------------------------------------------------------------
data_path_cmip_daily	= '/discover/nobackup/sakella/processData/MERRA2_27June2013/UTILS/FOR_CMIP/RAW_YRLY/'            # processed files
cmip_clim_path		= '/discover/nobackup/sakella/processData/MERRA2_27June2013/UTILS/FOR_CMIP/CLIM/'		 # full clim or add _1982/' for 1982
reynolds_clim_path	= '/discover/nobackup/sakella/processData/MERRA2_27June2013/UTILS/CLIM/'			 # full clim or add _1982/' for 1982
#
cmip_sst_file_pre      = 'cmip_raw_sst_'        # Processed files
cmip_sic_file_pre      = 'cmip_raw_sic_'
#
cmip_clim_pre          = 'cmip_clim_'           # Daily Climatology file Prefix
reynolds_clim_pre      = 'clim_'                # Daily Climatology file Prefix
#-----------------------------------------------------------------
start_date    = datetime(1982,01,01,00,0,0)
end_date      = datetime(1982,12,31,00,0,0)

iDebug 	      = 0			# [1]: yes, [0]: none. plot fields and save them to files
#-----------------------------------------------------------------
date_proc       = start_date
while( date_proc <= end_date):

	# form files names 
	# daily CMIP
	fName_sst_cmip            = data_path_cmip_daily + date_proc.strftime('%Y')+'/' + cmip_sst_file_pre + date_proc.strftime('%Y%m%d') + '.bin'
        fName_sic_cmip            = data_path_cmip_daily + date_proc.strftime('%Y')+'/' + cmip_sic_file_pre + date_proc.strftime('%Y%m%d') + '.bin'
	# form CMIP daily climatology file names
        fName_cmip_sst_clim	  = cmip_clim_path + cmip_clim_pre + 'sst_2052' + date_proc.strftime('%m%d') + '.bin'
        fName_cmip_sic_clim	  = cmip_clim_path + cmip_clim_pre + 'sic_2052' + date_proc.strftime('%m%d') + '.bin'
	# form Reynolds daily climatology file names
	fName_reynolds_sst_clim   = reynolds_clim_path + reynolds_clim_pre + 'sst_2052' + date_proc.strftime('%m%d') + '.bin'
        fName_reynolds_sic_clim   = reynolds_clim_path + reynolds_clim_pre + 'sic_2052' + date_proc.strftime('%m%d') + '.bin'

	print 'Processing date: [%s] Using:'%(date_proc.strftime('%Y%m%d'))

	# get cmip DAILY sst & sic
	[nymd1, nhms1, nymd2, nhms2, NLON, NLAT, cmip_daily_sst] = read_bin(fName_sst_cmip)
	[nymd1, nhms1, nymd2, nhms2, NLON, NLAT, cmip_daily_sic] = read_bin(fName_sic_cmip)

	# get cmip CLIM sst & sic
	[nymd1, nhms1, nymd2, nhms2, NLON, NLAT, cmip_clim_sst] = read_bin(fName_cmip_sst_clim)
	[nymd1, nhms1, nymd2, nhms2, NLON, NLAT, cmip_clim_sic] = read_bin(fName_cmip_sic_clim)

	# get Reynolds CLIM sst & sic
	[nymd1, nhms1, nymd2, nhms2, NLON, NLAT, reynolds_clim_sst] = read_bin(fName_reynolds_sst_clim)
	[nymd1, nhms1, nymd2, nhms2, NLON, NLAT, reynolds_clim_sic] = read_bin(fName_reynolds_sic_clim)

	SSTx = cmip_daily_sst - cmip_clim_sst + reynolds_clim_sst
	SICx = cmip_daily_sic - cmip_clim_sic + reynolds_clim_sic

	# make Antarctic values same as cmip daily
	iLand_sst = numpy.where(reynolds_clim_sst < 200.0)
	iLand_sic = numpy.where(reynolds_clim_sic <= 1.e-6)
	
	SSTx[iLand_sst] = cmip_daily_sst[iLand_sst]
	SICx[iLand_sic] = cmip_daily_sic[iLand_sic]
	
	# SANITY CHECKS...
	SSTx[ SICx > 1.0] = 273.15
	SSTx[ SICx < 0.0] = 273.15

	SICx[ SICx > 1.0] = 1.0
	SICx[ SICx < 0.0] = 0.0
	#-----------------------------------------------------------------
	if( iDebug):
		plt.close('all')
		coastLineFile   =       '/discover/nobackup/sakella/Stuff/matlabStuff/coastLine.dat'
        	[x_coast, y_coast]      = pylab.loadtxt(coastLineFile, unpack=True)
		
		dlon = 360.0/numpy.float32(NLON)
		dlat = 180.0/numpy.float32(NLAT)
                lon  = numpy.linspace(-180.0+dlon/2, 180.0-dlon/2, NLON)
                lat  = numpy.linspace(-90.0 +dlat/2,  90.0-dlat/2, NLAT)
		#..............................
        	plt.figure(num=1)

        	plt.subplot(241), plt.pcolormesh(lon, lat, numpy.transpose(cmip_clim_sst), vmin=285.0, vmax=305.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
        	plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight'), plt.title(r'CMIP CLIM')

        	plt.subplot(242), plt.pcolormesh(lon, lat, numpy.transpose(reynolds_clim_sst), vmin=285.0, vmax=305.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
        	plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight'), plt.title(r'Reynolds CLIM')

        	plt.subplot(243), plt.pcolormesh(lon, lat, numpy.transpose(cmip_daily_sst), vmin=285.0, vmax=305.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
        	plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight'), plt.title(r'Raw CMIP')

        	plt.subplot(244), plt.pcolormesh(lon, lat, numpy.transpose(SSTx), vmin=285.0, vmax=305.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
        	plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight'), plt.title(r'Modified CMIP')
		#
        	plt.subplot(245), plt.pcolormesh(lon, lat, numpy.transpose(cmip_clim_sic), vmin=0.0, vmax=1.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
        	plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')

        	plt.subplot(246), plt.pcolormesh(lon, lat, numpy.transpose(reynolds_clim_sic), vmin=0.0, vmax=1.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
        	plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')

        	plt.subplot(247), plt.pcolormesh(lon, lat, numpy.transpose(cmip_daily_sic), vmin=0.0, vmax=1.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
        	plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')

        	plt.subplot(248), plt.pcolormesh(lon, lat, numpy.transpose(SICx), vmin=0.0, vmax=1.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
        	plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
		#..............................
		#plt.show()
		fName_print_fig = 'for_mod_cmip_clim_' + date_proc.strftime('%Y%m%d') + '.png'
		plt.savefig(fName_print_fig,pad_inches=0.5,bbox_inches='tight',dpi=100, format='png')# orientation='landscape')
	#-----------------------------------------------------------------
	tomr_proc       = date_proc + timedelta(days=1)

	# write SST & SIC to a binary file- that GCM can read.
	write_cmip_bin.write_bin(date_proc.strftime('%Y%m%d'),                                                \
	                         numpy.float(date_proc.strftime('%Y')), numpy.float(date_proc.strftime('%m')), numpy.float(date_proc.strftime('%d')), \
	                         numpy.float(tomr_proc.strftime('%Y')), numpy.float(tomr_proc.strftime('%m')), numpy.float(tomr_proc.strftime('%d')), \
	                         SSTx, SICx)
	#-----------------------------------------------------------------
	print ' '
	date_proc = date_proc + timedelta(days=1)

	del cmip_daily_sst, cmip_daily_sic, cmip_clim_sst, cmip_clim_sic, reynolds_clim_sst, reynolds_clim_sic
	del SSTx, SICx
#-----------------------------------------------------------------


