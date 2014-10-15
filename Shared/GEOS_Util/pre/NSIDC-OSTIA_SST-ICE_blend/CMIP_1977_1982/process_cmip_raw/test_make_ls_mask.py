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

import  matplotlib.dates        as      md
import  matplotlib.pyplot       as      plt

from    datetime        import  datetime, timedelta
from    calendar        import monthrange

sys.path.append('/discover/nobackup/sakella/processData/MERRA2_27June2013/before_1982/CMIP/')
from get_mean_sst_sic import get_mean_sst_sic
from get_ls_mask      import get_ls_mask
#-----------------------------------------------------------------
iDebug          = 0

lat_north       = 30.0 #45.0
lat_south       =-30.0 #-45.0
#-----------------------------------------------------------------
data_path = '/archive/u/sakella/MERRA2/bc_data/'
file_pre  = 'daily_CMIP_'
file_suff = '.nc'
#-----------------------------------------------------------------
start_date    = datetime(1977,01,01,00,0,0)
end_date      = datetime(1977,12,31,00,0,0)

nMonths            = (end_date.month-start_date.month) + 1
#
nh_sst_mean             = numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
tr_sst_mean             = numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
sh_sst_mean             = numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')

nh_sic_mean             = numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
tr_sic_mean             = numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
sh_sic_mean             = numpy.empty([nMonths,1], dtype=numpy.float64, order = 'F')
#-----------------------------------------------------------------
# Make up a land sea mask based on 1/4 deg Reynolds.
fName_Reynolds_19820101 = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/REYN/1982/avhrr-only-v2.19820101.nc'
fName_CMIP_19811231     = '/archive/u/sakella/MERRA2/bc_data/1981/RAW/daily_CMIP_19811231.nc'
[sst_mask, sic_mask]    = get_ls_mask(fName_Reynolds_19820101, fName_CMIP_19811231, iDebug)
#-----------------------------------------------------------------
date_proc       = start_date
fName  = data_path + date_proc.strftime('%Y')+'/' + 'RAW/' + file_pre + date_proc.strftime('%Y%m%d') + file_suff
iDebug          = 1
[NH_sst, TR_sst, SH_sst, NH_sic, TR_sic, SH_sic] = get_mean_sst_sic(fName, date_proc, lat_north, lat_south, sst_mask, sic_mask, iDebug)

