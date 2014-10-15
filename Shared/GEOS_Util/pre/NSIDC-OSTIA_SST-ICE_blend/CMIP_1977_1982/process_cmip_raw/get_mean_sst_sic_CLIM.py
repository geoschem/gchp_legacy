
'''
 to make a mean of sst, sic usingdata in files: fNames.
'''
import os
import sys

import  numpy                   as      numpy
import  pylab                   as      pylab
import  matplotlib.pyplot       as      plt

#from    netCDF4         	import  Dataset
from    mpl_toolkits.basemap    import  Basemap

sys.path.append('/home/sakella/python_docs/myStuff/')
from read_netCDF                import read_netCDF as get_field_netCDF_file
#----------------------------------------------------------------
def get_mean_sst_sic_CLIM(fNames, num_years, iDebug, verbose=False):

	if( num_years - fNames.__len__() != 0):
		print 'ERROR! Num of years does not match number of input files'
		print 'WRONG CLIMATOLOGY!!'

	#....................................................................................................
	if( iDebug):
		print 'Working on...[%s]'%(fNames[0])
	# Initialize
	lon     = numpy.float64( get_field_netCDF_file(fNames[0],  'longitude'))
	lat     = numpy.float64( get_field_netCDF_file(fNames[0],  'latitude'))

	sum_sic = numpy.float64( get_field_netCDF_file(fNames[0],  'daily_CMIP_SIC'))
	sum_sst = numpy.float64( get_field_netCDF_file(fNames[0],  'daily_CMIP_SST'))
	#....................................................................................................
	# Accumulate sum for computing mean
	for iYears in range(1, num_years):
		if( iDebug):
			print 'Working on...[%s]'%(fNames[iYears])

		sum_sic = sum_sic + numpy.float64( get_field_netCDF_file(fNames[iYears],  'daily_CMIP_SIC'))
		sum_sst = sum_sst + numpy.float64( get_field_netCDF_file(fNames[iYears],  'daily_CMIP_SST'))
	#....................................................................................................
	mean_sst = sum_sst/num_years
	mean_sic = sum_sic/num_years

	mean_sic[numpy.isnan(mean_sst)] = mean_sst[numpy.isnan(mean_sst)]
	#....................................................................................................
	xx = fNames[0]
	if( iDebug):
		plt.subplot(211), plt.imshow(mean_sst, origin='lower', vmin=275.0, vmax=305.0), plt.colorbar(), plt.axis('off')
		plt.title(r'Mean SST [%s/%s]'%(xx[len(xx)-7:len(xx)-5], xx[len(xx)-5:len(xx)-3]))

		# now plot SIC in polar stereo.
		plt.subplot(223)
                nps = Basemap(projection='npstere',boundinglat=45.0,lon_0=360.,resolution='l')
                nps.drawcoastlines()
                nps.fillcontinents(color='0.9')
                nps.drawmeridians(numpy.arange(-180, 180, 30), color='0.5')
                nps.drawparallels(numpy.arange(-90,  90,  30), color='0.5')
                xn, yn = nps( *numpy.meshgrid(lon, lat))
                plt.hold(True)
                nps.pcolormesh(xn, yn, mean_sic, vmin=0.0, vmax=1.0,shading='flat')
                plt.colorbar()
		plt.title(r'Mean NH-SIC')
		#.................
	        plt.subplot(224)
                sps = Basemap(projection='spstere',boundinglat=-40.0,lon_0=90,resolution='l')
                sps.drawcoastlines()
                sps.fillcontinents(color='0.9')
                sps.drawmeridians(numpy.arange(-180, 180, 30), color='0.5')
                sps.drawparallels(numpy.arange(-90,  90,  30), color='0.5')
                xs, ys = sps( *numpy.meshgrid(lon, lat))
                sps.pcolormesh(xs, ys, mean_sic, vmin=0.0, vmax=1.0,shading='flat')
                plt.colorbar()
		plt.title(r'Mean SH-SIC')

		fName_print_fig = 'daily_mean_' + xx[len(xx)-7:len(xx)-3] + '.png'
		plt.savefig(fName_print_fig,pad_inches=0.5,bbox_inches='tight',dpi=120, format='png')# orientation='landscape')
		plt.close('all')
	#....................................................................................................
	from    netCDF4         	import  Dataset
	# write mean sst & sic to a nc file
	nc_fName = 'daily_mean_' + xx[len(xx)-7:len(xx)-3] + '.nc'
	out_ncfile = Dataset( nc_fName, 'w')

	out_ncfile.createDimension('latitude',  len(lat))
        out_ncfile.createDimension('longitude', len(lon))
        out_ncfile.createDimension('level',     1)
        out_ncfile.createDimension('time',      None)
	
	lats = out_ncfile.createVariable('latitude',  numpy.dtype('float64').char, ('latitude', ))
        lons = out_ncfile.createVariable('longitude', numpy.dtype('float64').char, ('longitude',))

        lats.units = 'degrees_north'
        lons.units = 'degrees_east'

        lats[:] = lat
        lons[:] = lon

        SST = out_ncfile.createVariable('daily_mean_CMIP_SST',numpy.dtype('float64').char,('time','level','latitude','longitude'))
        ICE = out_ncfile.createVariable('daily_mean_CMIP_SIC',numpy.dtype('float64').char,('time','level','latitude','longitude'))

        SST.units = 'deg_K'
        ICE.units  = 'none'

	SST[0,:,::] = mean_sst
	ICE[0,:,::] = mean_sic

	out_ncfile.close()
	#....................................................................................................
	del xx

	return mean_sst, mean_sic, lat, lon
#----------------------------------------------------------------
if __name__ == '__main__':
        '''
        ??? asdasd sd d sd ???
        
        '''

