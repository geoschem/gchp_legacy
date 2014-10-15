
'''
subset fields [in fName] for different regions and return a mean value to be used for monthly mean computation.
'''

import os
import sys

import  numpy			as      numpy
import  pylab			as      pylab
import  matplotlib.pyplot	as      plt

from	mpl_toolkits.basemap	import	Basemap

sys.path.append('/home/sakella/python_docs/myStuff/')
from read_netCDF                import read_netCDF as get_field_netCDF_file
#----------------------------------------------------------------
def get_mean_sst_sic(fName, date_proc, lat_north, lat_south, sst_mask, sic_mask, iDebug, verbose=False):

	#....................................................................................................
	lon	= numpy.float64( get_field_netCDF_file(fName,  'longitude'))
	lat	= numpy.float64( get_field_netCDF_file(fName,  'latitude'))

        #sst = numpy.float64( get_field_netCDF_file(fName,  'daily_CMIP_SST'))
        sst = numpy.float64( get_field_netCDF_file(fName,  'CMIP_SST_adjusted_Reynolds'))

        #sic = numpy.float64( get_field_netCDF_file(fName,  'daily_CMIP_SIC'))
        sic = numpy.float64( get_field_netCDF_file(fName,  'CMIP_SIC_adjusted_Reynolds'))
	#....................................................................................................
	# Apply land-sea masks for SST & SIC
        sst[sst_mask==0.0] = numpy.nan
        sic[sic_mask==0.0] = numpy.nan
	#....................................................................................................
	i_north	=	int( pylab.interp(lat_north, lat, pylab.linspace(0, len(lat)-1, len(lat))))
	i_south	=	int( pylab.interp(lat_south, lat, pylab.linspace(0, len(lat)-1, len(lat))))

	sst1_north	= sst[i_north+1: len(lat),  :]
	sst1_south	= sst[0        : i_south+1, :]
	sst1_trop	= sst[i_south+2: i_north,   :]

	sic1_north	= sic[i_north+1: len(lat),  :]
	sic1_south	= sic[0        : i_south+1, :]
	sic1_trop	= sic[i_south+2: i_north,   :]
	
	sst2_north  = sst1_north[ ~numpy.isnan(sst1_north)]
	sst2_south  = sst1_south[ ~numpy.isnan(sst1_south)]
	sst2_trop   = sst1_trop [ ~numpy.isnan(sst1_trop )]

	sic2_north  = sic1_north[ (~numpy.isnan(sic1_north)) & (sic1_north > 1.0E-08)]
	sic2_south  = sic1_south[ (~numpy.isnan(sic1_south)) & (sic1_south > 1.0E-08)]
	sic2_trop   = sic1_trop [ (~numpy.isnan(sic1_trop )) & (sic1_trop  > 1.0E-08)]

	sst_north_mean	= numpy.mean( sst2_north, dtype=numpy.float64)
	sst_south_mean	= numpy.mean( sst2_south, dtype=numpy.float64)
	sst_trop_mean	= numpy.mean( sst2_trop , dtype=numpy.float64)

	if( len(sic2_north) > 0):
		sic_north_mean	= numpy.mean( sic2_north, dtype=numpy.float64)
	else:
		sic_north_mean	= 0.0
	#		
	if( len(sic2_south) > 0):
		sic_south_mean	= numpy.mean( sic2_south, dtype=numpy.float64)
	else:
		sic_south_mean	= 0.0
	#
	if( len(sic2_trop) > 0):
		sic_trop_mean	= numpy.mean( sic2_trop , dtype=numpy.float64)
	else:
		sic_trop_mean	= 0.0
	#....................................................................................................
	if( iDebug):
		plt.subplot(211)
		plt.pcolormesh(lon, lat, sst, vmin=280.0, vmax=305.0), plt.colorbar(), plt.axis('off')
		plt.title(r'CMIP SST on [%s]'%(date_proc.strftime('%Y%m%d')))

		plt.subplot(223)
		nps = Basemap(projection='npstere',boundinglat=lat_north,lon_0=360.,resolution='l')	
		nps.drawcoastlines()
		nps.fillcontinents(color='0.9')
		nps.drawmeridians(numpy.arange(-180, 180, 30), color='0.5')
		nps.drawparallels(numpy.arange(-90,  90,  30), color='0.5')
		xn, yn = nps( *numpy.meshgrid(lon, lat))	
		plt.hold(True)
		nps.pcolormesh(xn, yn, sic, vmin=0.0, vmax=1.0,shading='flat')
		plt.colorbar()
		#------------------------------------------------------------------------------------------------
		plt.subplot(224)
		sps = Basemap(projection='spstere',boundinglat=lat_south,lon_0=90,resolution='l')	# lat_south = -40
		sps.drawcoastlines()
		sps.fillcontinents(color='0.9')
		sps.drawmeridians(numpy.arange(-180, 180, 30), color='0.5')
		sps.drawparallels(numpy.arange(-90,  90,  30), color='0.5')
		xs, ys = sps( *numpy.meshgrid(lon, lat))	
		sps.pcolormesh(xs, ys, sic, vmin=0.0, vmax=1.0,shading='flat')	
		plt.colorbar()
		#------------------------------------------------------------------------------------------------
		plt.show()	
			
		fName_print_fig = 'cmip_' + date_proc.strftime('%Y%m%d') + '.png'
		plt.savefig(fName_print_fig,pad_inches=0.5,bbox_inches='tight',dpi=80, format='png')# orientation='landscape')
	#....................................................................................................

	return sst_north_mean, sst_trop_mean, sst_south_mean, sic_north_mean, sic_trop_mean, sic_south_mean
#----------------------------------------------------------------
if __name__ == '__main__':
        '''
        ??? asdasd sd d sd ???
        
        '''


