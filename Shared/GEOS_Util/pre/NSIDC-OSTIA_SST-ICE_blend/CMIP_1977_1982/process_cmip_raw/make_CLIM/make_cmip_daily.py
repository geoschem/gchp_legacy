#!/usr/bin/python
#-----------------------------------------------------------------
'''
purpose is to :
               read mid-monthly cmip sst & sic 
               create time interpolated (linearly) daily data files.
'''
#-----------------------------------------------------------------
import os
import sys

import  pylab           as      pylab
import  numpy           as      numpy
import  time            as      time

import  matplotlib.dates        as      dates
import  matplotlib.pyplot       as      plt
from    datetime                import  datetime, timedelta
from    scipy                   import  interpolate

sys.path.append('/home/sakella/python_docs/myStuff/')
from read_netCDF import read_netCDF as get_field_netCDF_file

sys.path.append('/discover/nobackup/sakella/processData/MERRA2_27June2013/UTILS/FOR_CMIP/')
from get_cmip_sst_sic import get_cmip_sst_sic as get_cmip_sst_sic
from write_bin  import write_bin

coastLineFile   =       '/discover/nobackup/sakella/Stuff/matlabStuff/coastLine.dat'
[x_coast, y_coast]      = pylab.loadtxt(coastLineFile, unpack=True)
#-----------------------------------------------------------------
start_day	= datetime(1977,01,01,00,0,0)
end_day		= datetime(1977,12,31,00,0,0)

iDebug		= 0
iPlot		= 8		# produce a plot of raw 1x1 deg. CMIP data for a certain month.
iPlot_polar_stereo = 0		# plot SIC in [1]: stereo polar. [0]: latlon projection
#-----------------------------------------------------------------
cmip_data_path	= '/discover/nobackup/sakella/processData/MERRA2_27June2013/before_1982/CMIP/data/360x180/'
cmip_sst_pre	= 'amipbc_sst_360x180_'
cmip_sic_pre	= 'amipbc_sic_360x180_'
cmip_file_suff	= '.nc'
#
#MERRA-2 GRID: 1/4 deg
nlon_merra  = 1440
nlat_merra  =  720
#-----------------------------------------------------------------
# Form files that need to be pulled in
#SST
sst_fName_before_yr	= cmip_data_path + cmip_sst_pre + (start_day.year-1).__str__() + cmip_file_suff
sst_fName_proc_yr	= cmip_data_path + cmip_sst_pre +  start_day.year.__str__()    + cmip_file_suff
sst_fName_next_yr	= cmip_data_path + cmip_sst_pre + (start_day.year+1).__str__() + cmip_file_suff
#SIC
sic_fName_before_yr	= cmip_data_path + cmip_sic_pre + (start_day.year-1).__str__() + cmip_file_suff
sic_fName_proc_yr	= cmip_data_path + cmip_sic_pre +  start_day.year.__str__()    + cmip_file_suff
sic_fName_next_yr	= cmip_data_path + cmip_sic_pre + (start_day.year+1).__str__() + cmip_file_suff
#============================================================================================================================
# GET THE DATA FROM ALL 3 YEARS.
print 'You want to create daily files for [%s]. Gathering RAW CMIP DATA...'%(start_day.year)
[lon, lat, time_before_yr, sst_before_yr, sic_before_yr] = get_cmip_sst_sic(sst_fName_before_yr, sic_fName_before_yr, iDebug)
[lon, lat, time_proc_yr,   sst_proc_yr,   sic_proc_yr]   = get_cmip_sst_sic(sst_fName_proc_yr,   sic_fName_proc_yr,   iDebug)
[lon, lat, time_next_yr,   sst_next_yr,   sic_next_yr]   = get_cmip_sst_sic(sst_fName_next_yr,   sic_fName_next_yr,   iDebug)

# rearrange for easier processing.
# start with time stamps
time_all			= numpy.zeros([len(time_proc_yr)+2], dtype=numpy.float64, order = 'F')
time_all[0]	                = time_before_yr[11]
time_all[1:len(time_proc_yr)+1]	= time_proc_yr
time_all[len(time_proc_yr)+1]	= time_next_yr[0]
# SST
sst_all					= numpy.zeros([numpy.shape(sst_proc_yr)[0]+2, numpy.shape(sst_proc_yr)[1], numpy.shape(sst_proc_yr)[2]], dtype=numpy.float64, order = 'F')
sst_all[0,::]				= numpy.squeeze(sst_before_yr[11,::])
sst_all[1:len(time_proc_yr)+1,::]	= sst_proc_yr
sst_all[len(time_proc_yr)+1,::]		= numpy.squeeze(sst_next_yr[11,::])
# SIC
sic_all					= numpy.zeros([numpy.shape(sic_proc_yr)[0]+2, numpy.shape(sic_proc_yr)[1], numpy.shape(sic_proc_yr)[2]], dtype=numpy.float64, order = 'F')
sic_all[0,::]				= numpy.squeeze(sic_before_yr[11,::])
sic_all[1:len(time_proc_yr)+1,::]	= sic_proc_yr
sic_all[len(time_proc_yr)+1,::]		= numpy.squeeze(sic_next_yr[11,::])

print ' '
#-----------------------------------------------------------------
if( iDebug):
	print 'time_all: [%s]'%(dates.num2date(time_all))
 
	plt.figure()
	plt.subplot(211)
	plt.pcolor(lon, lat, numpy.squeeze(sst_all[iPlot,:,:]), vmin=275.0, vmax=305.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
	plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
	plt.title(r'SST [%s/%s]'%(dates.num2date(time_all)[iPlot].month, dates.num2date(time_all)[iPlot].year))

	plt.subplot(212)
	plt.pcolor(lon, lat, numpy.squeeze(sic_all[iPlot,:,:]), vmin=0.0, vmax=1.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
	plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
	plt.title(r'SIC [%s/%s]'%(dates.num2date(time_all)[iPlot].month, dates.num2date(time_all)[iPlot].year))
	#       
	plt.show()
	#fName_print = 'raw_cmip_' + '%s_%s'%(dates.num2date(timeX)[iPlot_month].month, dates.num2date(timeX)[iPlot_month].year) + '.png'
	#plt.savefig(fName_print, dpi=100, format='png')
	#plt.close('all')

del time_before_yr,time_proc_yr,time_next_yr, sst_before_yr,sst_proc_yr,sst_next_yr, sic_before_yr,sic_proc_yr,sic_next_yr
#============================================================================================================================
# NOW INTERPOLATE TO 1/4x/1/4 deg grid for MERRA
print 'Interpolating RAW CMIP DATA to MERRA GRID...'

dlon_merra = 360.0/nlon_merra
dlat_merra = 180.0/nlat_merra

lon_merra   = numpy.linspace(-180.0+dlon_merra/2, 180.0-dlon_merra/2, nlon_merra)
lat_merra   = numpy.linspace( -90.0+dlat_merra/2,  90.0-dlat_merra/2, nlat_merra)

#CMIP Grid
xx_in, yy_in	= numpy.meshgrid(lon, lat)
#MERRA Grid
xx_out, yy_out	= numpy.meshgrid(lon_merra, lat_merra)

sst_all_merra_res = numpy.zeros([numpy.shape(sst_all)[0], nlat_merra, nlon_merra], dtype=numpy.float64, order = 'F')
sic_all_merra_res = numpy.zeros([numpy.shape(sic_all)[0], nlat_merra, nlon_merra], dtype=numpy.float64, order = 'F')
#-----------------------------------------------------------------
for iMon in range(0, numpy.shape(sst_all)[0]):
	
	X1 = numpy.squeeze(sst_all[iMon,::])
	Y1 = numpy.squeeze(sic_all[iMon,::])

	X2 = interpolate.griddata( (xx_in.ravel(), yy_in.ravel()), X1.ravel(), (xx_out, yy_out), method='nearest')#, fill_value = sst_fill)	
	Y2 = interpolate.griddata( (xx_in.ravel(), yy_in.ravel()), Y1.ravel(), (xx_out, yy_out), method='nearest')#, fill_value = sst_fill)	
	
	sst_all_merra_res[iMon,::] = X2
	sic_all_merra_res[iMon,::] = Y2
	
	del X1, Y1, X2, Y2
#============================================================================================================================
iDebug = 0 	# set/unset to debug- so that we get plots.
print ' '
print 'Interpolating MID-MONTH DATA to DAILY DATA on MERRA GRID...'

proc_day = start_day
while( proc_day <= end_day):

	tI = pylab.interp( dates.date2num(proc_day), time_all, numpy.linspace(0, len(time_all)-1,  len(time_all)))	
	tL = int(numpy.floor(tI))
	tH = int(numpy.ceil (tI))

	print 'Processing for [%s]; CMIP data is on [%s] AND [%s]'%(proc_day, dates.num2date(time_all[tL]), dates.num2date(time_all[tH]))

	dX = numpy.squeeze(sst_all_merra_res[tH,::]) - numpy.squeeze(sst_all_merra_res[tL,::])	# this calculation can be done once a month. I am being dumb here!
	dY = numpy.squeeze(sic_all_merra_res[tH,::]) - numpy.squeeze(sic_all_merra_res[tL,::])
	dT = time_all[tH] - time_all[tL]

	if( dT > 0.0):
		X  = numpy.squeeze(sst_all_merra_res[tL,::]) + (dates.date2num(proc_day) - time_all[tL]) * dX/dT
		Y  = numpy.squeeze(sic_all_merra_res[tL,::]) + (dates.date2num(proc_day) - time_all[tL]) * dY/dT
	else:	# mid-month day
		X  = numpy.squeeze(sst_all_merra_res[tL,::])
		Y  = numpy.squeeze(sic_all_merra_res[tL,::])

	# dump interpolated SST(X) & SIC(Y) to a bin file- that GCM can read.
	proc_tomr = proc_day + timedelta(days=1)
	write_bin(proc_day.strftime('%Y%m%d'), \
                  numpy.float(proc_day.strftime('%Y')), numpy.float(proc_day.strftime('%m')), numpy.float(proc_day.strftime('%d')),  \
		  numpy.float(proc_tomr.strftime('%Y')),numpy.float(proc_tomr.strftime('%m')),numpy.float(proc_tomr.strftime('%d')), \
		  numpy.transpose(X), numpy.transpose(Y))

	del tI, tL, tH, dX, dY, dT, X, Y
	proc_day = proc_day + timedelta(days=1)
#============================================================================================================================
print '...Finished!'
#

