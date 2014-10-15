#!/usr/bin/python
#-----------------------------------------------------------------
'''
make a plot of monthly, regional mean sst & sic. for 1/4 deg daily CMIP; See 
make_cmip_daily.py: this was used to create daily nc files.
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

sys.path.append('/discover/nobackup/sakella/processData/MERRA2_27June2013/before_1982/CMIP/')
from get_mean_sst_sic import get_mean_sst_sic
from get_ls_mask      import get_ls_mask
#-----------------------------------------------------------------
iDebug		= 0

lat_north	= 30.0 #45.0
lat_south	=-30.0 #-45.0
#-----------------------------------------------------------------
data_path = '/archive/u/sakella/MERRA2/bc_data/'

#file_pre  = 'daily_CMIP_'
file_pre  = 'CMIP_clim_adj_'

file_suff = '.nc'
#-----------------------------------------------------------------
start_date    = datetime(1977,01,01,00,0,0)
end_date      = datetime(1977,12,31,00,0,0)

nMonths		   = (end_date.month-start_date.month) + 1
#
nh_sst_mean	  	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
tr_sst_mean	   	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
sh_sst_mean	   	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')

nh_sic_mean	   	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
tr_sic_mean	   	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
sh_sic_mean	   	= numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
#-----------------------------------------------------------------
# Make up a land sea mask based on 1/4 deg Reynolds.
fName_Reynolds_19820101 = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/REYN/1982/avhrr-only-v2.19820101.nc'

##fName_CMIP_19811231     = '/archive/u/sakella/MERRA2/bc_data/1981/RAW/daily_CMIP_19811231.nc'
#fName_CMIP_19811231     = '/archive/u/sakella/MERRA2/bc_data/1981/ncFiles/CMIP_clim_adj_19810101.nc'
fName_CMIP_19811231     = '/archive/u/sakella/MERRA2/bc_data/1977/ncFiles/CMIP_clim_adj_19770101.nc'

[sst_mask, sic_mask]  = get_ls_mask(fName_Reynolds_19820101, fName_CMIP_19811231, iDebug)
#-----------------------------------------------------------------
date_proc       = start_date

for iMonth in range(1, nMonths+1):

	# SST
	sum_NH_sst = 0.0 
	sum_TR_sst = 0.0
	sum_SH_sst = 0.0	
	# SIC
	sum_NH_sic = 0.0 
	sum_TR_sic = 0.0
	sum_SH_sic = 0.0	
	#--------------------------------------------------------------------------------------------------------------------
	for iDay in range(1, monthrange(start_date.year, iMonth)[1]+1):

		# form file name
		#fName  = data_path + date_proc.strftime('%Y')+'/' + 'RAW/' + file_pre + date_proc.strftime('%Y%m%d') + file_suff
		fName  = data_path + date_proc.strftime('%Y')+'/' + 'ncFiles/' + file_pre + date_proc.strftime('%Y%m%d') + file_suff
		#--------------------------------------------------------------------------------------------------------------------
		print 'Processing for...[%s], Month Num = [%i], file:[%s]'%(date_proc.strftime('%Y%m%d'), iMonth, fName)
		[NH_sst, TR_sst, SH_sst, NH_sic, TR_sic, SH_sic] = get_mean_sst_sic(fName, date_proc, lat_north, lat_south, sst_mask, sic_mask, iDebug)
		#--------------------------------------------------------------------------------------------------------------------
		sum_NH_sst = sum_NH_sst + NH_sst
		sum_TR_sst = sum_TR_sst + TR_sst
		sum_SH_sst = sum_SH_sst + SH_sst

		sum_NH_sic = sum_NH_sic + NH_sic
		sum_TR_sic = sum_TR_sic + TR_sic
		sum_SH_sic = sum_SH_sic + SH_sic
		#
		date_proc = date_proc + timedelta(days=1)	
	#--------------------------------------------------------------------------------------------------------------------
	nh_sst_mean[iMonth-1] = sum_NH_sst/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	tr_sst_mean[iMonth-1] = sum_TR_sst/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	sh_sst_mean[iMonth-1] = sum_SH_sst/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	

	nh_sic_mean[iMonth-1] = sum_NH_sic/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	tr_sic_mean[iMonth-1] = sum_TR_sic/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	sh_sic_mean[iMonth-1] = sum_SH_sic/(numpy.float64(monthrange(start_date.year, iMonth)[1]))	
	#--------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------
for iMonth in range(0, nMonths):
	print 'Mon Num...[%i], SST means[NH, TR, SH]= %5.2f, %5.2f, %5.2f'%(iMonth+ 1, nh_sst_mean[iMonth],   tr_sst_mean[iMonth],   sh_sst_mean[iMonth])
	print 'Mon Num...[%i], SIC means[NH, TR, SH]= %5.2f, %5.2f, %5.2f'%(iMonth+1,  nh_sic_mean[iMonth],   tr_sic_mean[iMonth],   sh_sic_mean[iMonth])
#-----------------------------------------------------------------
nh_file_sst         = str(start_date.year) + '_nh_sst_cmip.txt'
pylab.savetxt(nh_file_sst, nh_sst_mean) 

tr_file_sst         = str(start_date.year) + '_tr_sst_cmip.txt'
pylab.savetxt(tr_file_sst, tr_sst_mean) 

sh_file_sst         = str(start_date.year) + '_sh_sst_cmip.txt'
pylab.savetxt(sh_file_sst, sh_sst_mean) 
#
nh_file_sic         = str(start_date.year) + '_nh_sic_cmip.txt'
pylab.savetxt(nh_file_sic, nh_sic_mean) 

tr_file_sic         = str(start_date.year) + '_tr_sic_cmip.txt'
pylab.savetxt(tr_file_sic, tr_sic_mean) 

sh_file_sic         = str(start_date.year) + '_sh_sic_cmip.txt'
pylab.savetxt(sh_file_sic, sh_sic_mean) 
#-----------------------------------------------------------------
#.....
