'''
Create a land-sea mask for CMIP data using 1/4 deg Reynolds 
'''

import os
import sys

import  numpy                   as      numpy
import  pylab                   as      pylab
import  matplotlib.pyplot       as      plt

from    mpl_toolkits.basemap    import  Basemap

sys.path.append('/home/sakella/python_docs/myStuff/')
from read_netCDF                import read_netCDF as get_field_netCDF_file
#----------------------------------------------------------------
def get_ls_mask(fName_Reynolds, fName_CMIP, iDebug, verbose=False):

	#....................................................................................................
	# CMIP fields
	lon_cmip = numpy.float64( get_field_netCDF_file(fName_CMIP,  'longitude'))
	lat_cmip = numpy.float64( get_field_netCDF_file(fName_CMIP,  'latitude'))

	#sst_cmip = numpy.float64( get_field_netCDF_file(fName_CMIP,  'daily_CMIP_SST'))
	sst_cmip = numpy.float64( get_field_netCDF_file(fName_CMIP,  'CMIP_SST_adjusted_Reynolds'))

	#sic_cmip = numpy.float64( get_field_netCDF_file(fName_CMIP,  'daily_CMIP_SIC'))
	sic_cmip = numpy.float64( get_field_netCDF_file(fName_CMIP,  'CMIP_SIC_adjusted_Reynolds'))
	#....................................................................................................
	# Reynolds fields
	lon_Rey = numpy.float64( get_field_netCDF_file(fName_Reynolds,  'lon'))			# Reynolds- remember, lon is between (0, 360).
	lat_Rey = numpy.float64( get_field_netCDF_file(fName_Reynolds,  'lat'))
	sst1    = numpy.float64( get_field_netCDF_file(fName_Reynolds,  'sst')) + 273.15
	sic1    = numpy.float64( get_field_netCDF_file(fName_Reynolds,  'ice'))
	#....................................................................................................

	# FLIP Reynolds lon to be (-180, 180)
	lon_Rey = lon_Rey - 180.0

	# This way...
	#sst_Rey = numpy.zeros([numpy.shape(sst1)[0], numpy.shape(sst1)[1]], dtype=numpy.float64, order = 'F')
	#sic_Rey = numpy.zeros([numpy.shape(sic1)[0], numpy.shape(sic1)[1]], dtype=numpy.float64, order = 'F')
	#sst_Rey[numpy.isnan(sst1)] = sst1[numpy.isnan(sst1)]
	#sic_Rey[numpy.isnan(sic1)] = sic1[numpy.isnan(sic1)]
	# Does not work. 

	# Do following way. If you want to copy arrays, use sst_Rey[:] = sst1
	sst_Rey  = numpy.empty_like(sst1)
	sic_Rey  = numpy.empty_like(sic1)

	sst_Rey[:,720:1440] = numpy.squeeze(sst1[:,  0:720])
	sst_Rey[:,  0:720]  = numpy.squeeze(sst1[:,720:1440])

	sic_Rey[:,720:1440] = numpy.squeeze(sic1[:,  0:720])
	sic_Rey[:,  0:720]  = numpy.squeeze(sic1[:,720:1440])
	#
	# open ocean is = 1.0, land = 0.0
	mask1 = numpy.ones([numpy.shape(sst_Rey)[0], numpy.shape(sst_Rey)[1]], dtype=numpy.float64, order = 'F')
	mask1[sst_Rey._mask] = 0.0		# sst_Rey._mask is bool array (with true/flase). this mask is false, set mask = 0.0

	mask2 = numpy.ones([numpy.shape(sic_Rey)[0], numpy.shape(sic_Rey)[1]], dtype=numpy.float64, order = 'F')
	mask2[sic_Rey._mask] = 0.0
	#....................................................................................................
	if( iDebug):
		coastLineFile   =       '/discover/nobackup/sakella/Stuff/matlabStuff/coastLine.dat'
		[x_coast, y_coast]      = pylab.loadtxt(coastLineFile, unpack=True)

		plt.figure(num=1)
		plt.subplot(231)
		plt.pcolormesh(lon_cmip, lat_cmip, sst_cmip, vmin=275.0, vmax=305.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
		plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
		plt.title(r'CMIP SST orig')

		plt.subplot(232)
		plt.pcolormesh(lon_Rey, lat_Rey, sst_Rey, vmin=275.0, vmax=305.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
		plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
		plt.title(r'Reynolds SST')

		#.........
		sst_cmip_masked = sst_cmip
		sst_cmip_masked[mask1==0.0] = numpy.nan
		plt.subplot(233)
		plt.pcolormesh(lon_cmip, lat_cmip, sst_cmip_masked, vmin=275.0, vmax=305.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
		plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
		plt.title(r'masked cmip sst')
		#
		plt.subplot(234)
		plt.pcolormesh(lon_cmip, lat_cmip, sic_cmip, vmin=0.0, vmax=1.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
		plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
		plt.title(r'CMIP SIC')
		 
		plt.subplot(235)
		plt.pcolormesh(lon_Rey, lat_Rey, sic_Rey, vmin=0.0, vmax=1.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
		plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
		plt.title(r'Reynolds SIC')

		#.........
		sic_cmip_masked = sic_cmip
		sic_cmip_masked[mask2==0.0] = numpy.nan
		plt.subplot(236)
		plt.pcolormesh(lon_cmip, lat_cmip, sic_cmip_masked, vmin=0.0, vmax=1.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
		plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
		plt.title(r'masked cmip sic')
		#............................................................................................

		plt.show()
	#....................................................................................................

	return mask1, mask2
#----------------------------------------------------------------
if __name__ == '__main__':
        '''
        ??? asdasd sd d sd ???
        
        '''

