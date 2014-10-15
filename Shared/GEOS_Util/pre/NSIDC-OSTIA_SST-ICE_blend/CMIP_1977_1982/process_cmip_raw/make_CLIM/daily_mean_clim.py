#!/usr/bin/python
#-----------------------------------------------------------------
'''
make a mean of sst & sic for each day of the year (365 or 366) days.
'''
#----------------------------------------------------------------
import os
import sys

import  pylab            as      pylab
import  numpy            as      numpy
import  time             as      time
import  calendar         as      calendar

import  matplotlib.dates        as      md
import  matplotlib.pyplot       as      plt

from    datetime        import  datetime, timedelta

sys.path.append('/discover/nobackup/sakella/processData/MERRA2_27June2013/UTILS/FOR_CMIP/')
from compute_mean_clim import compute_mean_clim
from write_bin  import write_bin
#-----------------------------------------------------------------
iDebug             = 0
iPlot_polar_stereo = 0

#start_year	= 1977
start_year	= 1982
end_year	= 1982
#-----------------------------------------------------------------
cmip_data_path = '/discover/nobackup/sakella/processData/MERRA2_27June2013/UTILS/FOR_CMIP/RAW_YRLY/'		# processed files
cmip_file_sst  = 'cmip_raw_sst_'
cmip_file_sic  = 'cmip_raw_sic_'
cmip_file_suff = '.bin'
#-----------------------------------------------------------------
iDay	= 1
#-----------------------------------------------------------------
for iMon in range(1, 13):
	start_date	= datetime(start_year,iMon,01,00,0,0)
	output_day	  = start_date.day
	nDays_inThisMonth = calendar.monthrange(start_year, iMon)[1]

	while(output_day <= nDays_inThisMonth):
		output_year	= start_date.year
		output_mon	= start_date.month

		#INITIALIZE here.
		nYears  = 1
		fNames_sst 	= [ ]
		fNames_sic 	= [ ]
		while(output_year <= end_year):			# Loop over same calendar day. From Start : End Year.
			proc_year	= output_year
			proc_mon	= output_mon
			proc_day	= output_day
			proc_date	= datetime(proc_year, proc_mon, proc_day)
			clim_today	= datetime(2052,      proc_mon, proc_day)		# to write a climatology file
			#-----------------------------------------------------------------
			# form file names
			fName_sst  = cmip_data_path + proc_date.strftime('%Y')+'/' + cmip_file_sst + proc_date.strftime('%Y%m%d') + cmip_file_suff
			fName_sic  = cmip_data_path + proc_date.strftime('%Y')+'/' + cmip_file_sic + proc_date.strftime('%Y%m%d') + cmip_file_suff
			#xstat_dmget = os.system('dmget '  + fName_sst + ' ' + fName_sic)
			#-----------------------------------------------------------------
			fNames_sst.append(fName_sst)
			fNames_sic.append(fName_sic)
			print '[nYrs=%i, iDays=%i], Gathering...[%s]'%(nYears, iDay, fName_sst)
			nYears = nYears + 1
			output_year = output_year + 1
		#-----------------------------------------------------------------
		output_day = output_day + 1
		iDay	   = iDay + 1
		#print '[%s]'%(fNames_sst)
		iSST = 1
		mean_sst = compute_mean_clim(iSST, fNames_sst, nYears-1, iDebug, iPlot_polar_stereo)
		iSST = 0
		mean_sic = compute_mean_clim(iSST, fNames_sic, nYears-1, iDebug, iPlot_polar_stereo)
		print 'Computed Mean over [%i] years for day [%i] of 365 days year.'%(nYears-1, iDay-1)
		#write SST & SIC CLIM to a binary file- that GCM can read.
		clim_tomr       = clim_today + timedelta(days=1)
		print 'CLIM...[%s] to [%s]'%(clim_today, clim_tomr)
		write_bin(clim_today.strftime('%Y%m%d'), \
			  numpy.float(clim_today.strftime('%Y')), numpy.float(clim_today.strftime('%m')), numpy.float(clim_today.strftime('%d')), \
                          numpy.float(clim_tomr.strftime('%Y')),  numpy.float(clim_tomr.strftime('%m')),  numpy.float(clim_tomr.strftime('%d')),  \
			  mean_sst, mean_sic)
		print '========================to next Day============================================'
	print '========================to next Month=========================================='
#
#================================================================
# NOW FEB 29TH.
#================================================================

start_year	= 1980
end_year	= 1980
start_date 	= datetime(start_year, 2, 29, 00, 0, 0)

#INITIALIZE sum here.
nYears  	= 1
fNames_sst 	= [ ]
fNames_sic 	= [ ]
#.......................................
output_year	= start_date.year
output_mon	= start_date.month
output_day	= start_date.day
while(output_year <= end_year):
	proc_date	= datetime(output_year, output_mon, output_day)
	clim_today	= datetime(2052,        output_mon, output_day)		# to write a climatology file
	#-----------------------------------------------------------------
	# form file names
	fName_sst  = cmip_data_path + proc_date.strftime('%Y')+'/' + cmip_file_sst + proc_date.strftime('%Y%m%d') + cmip_file_suff
	fName_sic  = cmip_data_path + proc_date.strftime('%Y')+'/' + cmip_file_sic + proc_date.strftime('%Y%m%d') + cmip_file_suff
	#xstat_dmget = os.system('dmget '  + fName_sst + ' ' + fName_sic)
	#-----------------------------------------------------------------
	fNames_sst.append(fName_sst)
	fNames_sic.append(fName_sic)
	print '[nYrs=%i], Gathering...[%s]'%(nYears, fName_sic)
	nYears = nYears + 1
	output_year = output_year + 4
	#-----------------------------------------------------------------
iSST = 1
mean_sst = compute_mean_clim(iSST, fNames_sst, nYears-1, iDebug, iPlot_polar_stereo)
iSST = 0
mean_sic = compute_mean_clim(iSST, fNames_sic, nYears-1, iDebug, iPlot_polar_stereo)
print 'Computed Mean over [%i] years for Feb 29th'%(nYears-1)
#write SST & SIC CLIM to a binary file- that GCM can read.
clim_tomr       = clim_today + timedelta(days=1)
print 'CLIM...[%s] to [%s]'%(clim_today, clim_tomr)
write_bin(clim_today.strftime('%Y%m%d'), \
          numpy.float(clim_today.strftime('%Y')), numpy.float(clim_today.strftime('%m')), numpy.float(clim_today.strftime('%d')), \
          numpy.float(clim_tomr.strftime('%Y')),  numpy.float(clim_tomr.strftime('%m')),  numpy.float(clim_tomr.strftime('%d')),  \
          mean_sst, mean_sic)
#-----------------------------------------------------------------
#


