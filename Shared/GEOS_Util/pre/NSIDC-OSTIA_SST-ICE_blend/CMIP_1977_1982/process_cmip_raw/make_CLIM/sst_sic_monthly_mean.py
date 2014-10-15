#!/usr/bin/python
#-----------------------------------------------------------------
'''
make a plot of monthly, regional mean sst & sic. for 1/4 deg daily CMIP. 
'''
#-----------------------------------------------------------------
import os
import sys

import  pylab            as      pylab
import  numpy            as      numpy
import  time             as      time

import  matplotlib.dates	as      md
import  matplotlib.pyplot	as      plt

from    datetime	import  datetime, timedelta
from	calendar 	import monthrange

sys.path.append('/discover/nobackup/sakella/processData/MERRA2_27June2013/UTILS/')
from get_monthly_mean_sst_sic import get_monthly_mean_sst_sic
#-----------------------------------------------------------------
iDebug		= 0

lat_north	= 30.0 #45.0
lat_south	=-30.0 #-45.0
#-----------------------------------------------------------------
reynolds_raw_path  = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/REYN/'
cmip_data_path     = '/discover/nobackup/sakella/processData/MERRA2_27June2013/UTILS/FOR_CMIP/RAW_YRLY/'            # processed files
cmip_clim_path     = '/discover/nobackup/sakella/processData/MERRA2_27June2013/UTILS/FOR_CMIP/CLIM_1982/'
#
reynolds_raw_file_pre  = 'avhrr-only-v2.' 	# Use AVHRR ONLY: for 1982
cmip_sst_file_pre      = 'cmip_raw_sst_'	# Processed files
cmip_sic_file_pre      = 'cmip_raw_sic_'
#
cmip_clim_pre          = 'cmip_clim_'          	# Daily Climatology file Prefix
#
reynolds_file_suff = '.nc'
#-----------------------------------------------------------------
start_date    = datetime(1980,01,01,00,0,0)
end_date      = datetime(1980,12,31,00,0,0)
nMonths	      = (end_date.month-start_date.month) + 1
#
nh_cmip_sst_mean	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
tr_cmip_sst_mean	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
sh_cmip_sst_mean	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
#
nh_clim_sst_mean	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
tr_clim_sst_mean	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
sh_clim_sst_mean	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
#
nh_cmip_sic_mean	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
tr_cmip_sic_mean	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
sh_cmip_sic_mean	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
#
nh_clim_sic_mean	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
tr_clim_sic_mean	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
sh_clim_sic_mean	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
#-----------------------------------------------------------------
date_proc       = start_date

for iMonth in range(1, nMonths+1):

	# SST
	sum_cmip_NH_sst = 0.0 
	sum_cmip_TR_sst = 0.0
	sum_cmip_SH_sst = 0.0	
	 
	sum_Clim_NH_sst = 0.0 
	sum_Clim_TR_sst = 0.0
	sum_Clim_SH_sst = 0.0	
	# SIC
	sum_cmip_NH_sic = 0.0 
	sum_cmip_TR_sic = 0.0
	sum_cmip_SH_sic = 0.0	

	sum_Clim_NH_sic = 0.0 
	sum_Clim_TR_sic = 0.0
	sum_Clim_SH_sic = 0.0	
	#--------------------------------------------------------------------------------------------------------------------
	for iDay in range(1, monthrange(start_date.year, iMonth)[1]+1):
		print 'Processing for...[%s], Month Num = [%i]'%(date_proc.strftime('%Y%m%d'), iMonth)

		#....................................................................................................................
		# form Reynolds RAW data file name
		# 1982 below works fine.
		#fName_Reynolds_raw  = reynolds_raw_path + date_proc.strftime('%Y')+'/' + reynolds_raw_file_pre + date_proc.strftime('%Y%m%d') + reynolds_file_suff   # avhrr-only
		# 1977, '78, '79, '81- because there is no Reynolds file. use 1982 files.
		#fName_Reynolds_raw  = reynolds_raw_path + '1982/' + reynolds_raw_file_pre + '1982'+ date_proc.strftime('%m%d') + reynolds_file_suff   # avhrr-only
		#when doing 1980 use this line [closest leap yr from reynolds- 1984].
		fName_Reynolds_raw  = reynolds_raw_path + '1984/' + reynolds_raw_file_pre + '1984'+ date_proc.strftime('%m%d') + reynolds_file_suff   # avhrr-only
		print'Raw File...[%s]'%(fName_Reynolds_raw)
		#....................................................................................................................
		# form daily processed file names
		fName_sst_proc   = cmip_data_path + date_proc.strftime('%Y')+'/' + cmip_sst_file_pre + date_proc.strftime('%Y%m%d') + '.bin'
		fName_sic_proc   = cmip_data_path + date_proc.strftime('%Y')+'/' + cmip_sic_file_pre + date_proc.strftime('%Y%m%d') + '.bin'
		print'Processed Files...[%s, %s]'%(fName_sst_proc, fName_sic_proc)
		#....................................................................................................................
		# form daily climatology file names
		fName_sst_clim   = cmip_clim_path + cmip_clim_pre + 'sst_2052' + date_proc.strftime('%m%d') + '.bin'
		fName_sic_clim   = cmip_clim_path + cmip_clim_pre + 'sic_2052' + date_proc.strftime('%m%d') + '.bin'
		print'Clim Files...[%s, %s]'%(fName_sst_clim, fName_sic_clim)
		#....................................................................................................................

		[Cmip_NH_sst, Cmip_TR_sst, Cmip_SH_sst, Cmip_NH_sic, Cmip_TR_sic, Cmip_SH_sic,   \
                 Clim_NH_sst, Clim_TR_sst, Clim_SH_sst, Clim_NH_sic, Clim_TR_sic, Clim_SH_sic]       = \
                 get_monthly_mean_sst_sic(fName_Reynolds_raw, fName_sst_proc, fName_sic_proc, fName_sst_clim, fName_sic_clim, date_proc, lat_north, lat_south, iDebug)
		#--------------------------------------------------------------------------------------------------------------------
		sum_cmip_NH_sst = sum_cmip_NH_sst + Cmip_NH_sst
		sum_cmip_TR_sst = sum_cmip_TR_sst + Cmip_TR_sst
		sum_cmip_SH_sst = sum_cmip_SH_sst + Cmip_SH_sst
		 
		sum_cmip_NH_sic = sum_cmip_NH_sic + Cmip_NH_sic
		sum_cmip_TR_sic = sum_cmip_TR_sic + Cmip_TR_sic
		sum_cmip_SH_sic = sum_cmip_SH_sic + Cmip_SH_sic
		#
		sum_Clim_NH_sst = sum_Clim_NH_sst + Clim_NH_sst
		sum_Clim_TR_sst = sum_Clim_TR_sst + Clim_TR_sst
		sum_Clim_SH_sst = sum_Clim_SH_sst + Clim_SH_sst
	
		sum_Clim_NH_sic = sum_Clim_NH_sic + Clim_NH_sic
		sum_Clim_TR_sic = sum_Clim_TR_sic + Clim_TR_sic
		sum_Clim_SH_sic = sum_Clim_SH_sic + Clim_SH_sic
		#	
		date_proc = date_proc + timedelta(days=1)	
	#--------------------------------------------------------------------------------------------------------------------
	nh_cmip_sst_mean[iMonth-1] = sum_cmip_NH_sst/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	tr_cmip_sst_mean[iMonth-1] = sum_cmip_TR_sst/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	sh_cmip_sst_mean[iMonth-1] = sum_cmip_SH_sst/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	

	nh_cmip_sic_mean[iMonth-1] = sum_cmip_NH_sic/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	tr_cmip_sic_mean[iMonth-1] = sum_cmip_TR_sic/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	sh_cmip_sic_mean[iMonth-1] = sum_cmip_SH_sic/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	#
	nh_clim_sst_mean[iMonth-1] = sum_Clim_NH_sst/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	tr_clim_sst_mean[iMonth-1] = sum_Clim_TR_sst/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	sh_clim_sst_mean[iMonth-1] = sum_Clim_SH_sst/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	

	nh_clim_sic_mean[iMonth-1] = sum_Clim_NH_sic/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	tr_clim_sic_mean[iMonth-1] = sum_Clim_TR_sic/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	sh_clim_sic_mean[iMonth-1] = sum_Clim_SH_sic/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	#--------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------
for iMonth in range(0, nMonths):
	print 'Mon Num...[%i], CMIP...SST means[NH, TR, SH]= %5.2f, %5.2f, %5.2f'%(iMonth+1, nh_cmip_sst_mean[iMonth],   tr_cmip_sst_mean[iMonth],   sh_cmip_sst_mean[iMonth])
	print 'Mon Num...[%i], CMIP...SIC means[NH, TR, SH]= %5.2f, %5.2f, %5.2f'%(iMonth+1, nh_cmip_sic_mean[iMonth],   tr_cmip_sic_mean[iMonth],   sh_cmip_sic_mean[iMonth])
#-----------------------------------------------------------------
nh_file_sst_cmip         = str(start_date.year) + '_nh_sst_cmip.txt'
pylab.savetxt(nh_file_sst_cmip, nh_cmip_sst_mean) 

tr_file_sst_cmip         = str(start_date.year) + '_tr_sst_cmip.txt'
pylab.savetxt(tr_file_sst_cmip, tr_cmip_sst_mean) 

sh_file_sst_cmip         = str(start_date.year) + '_sh_sst_cmip.txt'
pylab.savetxt(sh_file_sst_cmip, sh_cmip_sst_mean) 
#
nh_file_sic_cmip         = str(start_date.year) + '_nh_sic_cmip.txt'
pylab.savetxt(nh_file_sic_cmip, nh_cmip_sic_mean) 

tr_file_sic_cmip         = str(start_date.year) + '_tr_sic_cmip.txt'
pylab.savetxt(tr_file_sic_cmip, tr_cmip_sic_mean) 

sh_file_sic_cmip         = str(start_date.year) + '_sh_sic_cmip.txt'
pylab.savetxt(sh_file_sic_cmip, sh_cmip_sic_mean) 
#...............................................
pylab.savetxt('nh_CMIP_clim_sst_mean.txt', nh_clim_sst_mean)
pylab.savetxt('tr_CMIP_clim_sst_mean.txt', tr_clim_sst_mean)
pylab.savetxt('sh_CMIP_clim_sst_mean.txt', sh_clim_sst_mean)

pylab.savetxt('nh_CMIP_clim_sic_mean.txt', nh_clim_sic_mean)
pylab.savetxt('tr_CMIP_clim_sic_mean.txt', tr_clim_sic_mean)
pylab.savetxt('sh_CMIP_clim_sic_mean.txt', sh_clim_sic_mean)
#-----------------------------------------------------------------
plt.subplot(321),
plt.plot(nh_cmip_sst_mean, 'b.-', label=r'daily'), plt.hold('true'), plt.plot(nh_clim_sst_mean, 'kx-', label=r'clim'), plt.legend(loc=1), plt.ylabel(r'nh'), plt.title(r'SST')
plt.subplot(322),
plt.plot(nh_cmip_sic_mean, 'b.-', label=r'daily'), plt.hold('true'), plt.plot(nh_clim_sic_mean, 'kx-', label=r'clim'), plt.title(r'SIC')

plt.subplot(323),
plt.plot(tr_cmip_sst_mean, 'b.-', label=r'daily'), plt.hold('true'), plt.plot(tr_clim_sst_mean, 'kx-', label=r'clim'), plt.ylabel(r'tr')
plt.subplot(324),
plt.plot(tr_cmip_sic_mean, 'b.-', label=r'daily'), plt.hold('true'), plt.plot(tr_clim_sic_mean, 'kx-', label=r'clim')

plt.subplot(325),
plt.plot(sh_cmip_sst_mean, 'b.-', label=r'daily'), plt.hold('true'), plt.plot(sh_clim_sst_mean, 'kx-', label=r'clim'), plt.ylabel(r'sh')
plt.subplot(326),
plt.plot(sh_cmip_sic_mean, 'b.-', label=r'daily'), plt.hold('true'), plt.plot(sh_clim_sic_mean, 'kx-', label=r'clim')
plt.show()
#-----------------------------------------------------------------
#.....
