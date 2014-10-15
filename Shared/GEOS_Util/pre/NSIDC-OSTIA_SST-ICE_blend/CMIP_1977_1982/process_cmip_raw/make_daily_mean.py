#!/usr/bin/python
#-----------------------------------------------------------------
'''
make a mean- for each day of sst & sic, using data from 1977- 981 for CMIP SST & SIC.
We use ncfiles that have daily data. They were created using make_cmip_daily.py
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

sys.path.append('/discover/nobackup/sakella/processData/MERRA2_27June2013/before_1982/CMIP/')
from get_mean_sst_sic_CLIM import get_mean_sst_sic_CLIM
#-----------------------------------------------------------------
iDebug          = 0

nLAT		= 720		# nLAT
nLON		= 1440		# nLON
nDAYS		= 365		# for now, just have 365, later we will deal with Feb 29th. See bottom of script.

fNames 		= [ ]
#-----------------------------------------------------------------
data_path = '/archive/u/sakella/MERRA2/bc_data/'
file_pre  = 'daily_CMIP_'
file_suff = '.nc'
#-----------------------------------------------------------------
start_year	= 1977
end_year	= 1981
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

		#INITIALIZE sum here.
		nYears  = 1
		fNames 	= [ ]
		while(output_year <= end_year):
			proc_year	= output_year
			proc_mon	= output_mon
			proc_day	= output_day
			proc_date	= datetime(proc_year, proc_mon, proc_day)
			#-----------------------------------------------------------------
			# form file name
			fName  = data_path + proc_date.strftime('%Y')+'/' + 'RAW/' + file_pre + proc_date.strftime('%Y%m%d') + file_suff
			#-----------------------------------------------------------------
			fNames.append(fName)
			print '[nYrs=%i, iDays=%i], Gathering...[%s]'%(nYears, iDay, fName)
			nYears = nYears + 1
			output_year = output_year + 1
		#-----------------------------------------------------------------
		output_day = output_day + 1
		iDay	   = iDay + 1
		#print '[%s]'%(fNames)
		[mean_sst, mean_sic, lat, lon] = get_mean_sst_sic_CLIM(fNames, nYears-1, iDebug)
		print 'Computed Mean over [%i] years for day [%i] of 365 days year.'%(nYears-1, iDay-1)
		print '========================to next Day============================================'
	print '========================to next Month=========================================='
#
#================================================================
# NOW FEB 29TH.
#================================================================

start_year	= 1980
end_year	= 1981
start_date 	= datetime(start_year, 2, 29, 00, 0, 0)

#INITIALIZE sum here.
nYears  = 1
fNames 	= [ ]
#.......................................
output_year	= start_date.year
output_mon	= start_date.month
output_day	= start_date.day
while(output_year <= end_year):
	proc_date	= datetime(output_year, output_mon, output_day)
	#-----------------------------------------------------------------
	#form file name
	fName  = data_path + proc_date.strftime('%Y')+'/' + 'RAW/' + file_pre + proc_date.strftime('%Y%m%d') + file_suff
	#-----------------------------------------------------------------
	fNames.append(fName)
	print '[nYrs=%i], Gathering...[%s]'%(nYears, fName)
	nYears = nYears + 1
	output_year = output_year + 4
	#-----------------------------------------------------------------

[mean_sst, mean_sic, lat, lon] = get_mean_sst_sic_CLIM(fNames, nYears-1, iDebug)
print 'Computed Mean over [%i] years for Feb 29th'%(nYears-1)
#-----------------------------------------------------------------
#


