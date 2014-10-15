
import  pylab            as      pylab
import  numpy            as      numpy
import  time             as      time
import  calendar         as      calendar

import  matplotlib.dates        as      dates
import  matplotlib.pyplot       as      plt

from    datetime        import  datetime, timedelta
from    netCDF4         import  Dataset
from    mpl_toolkits.basemap    import  Basemap
#----------------------------------------------------------------
def  write_cmip_daily(dump_day, lon, lat, sst, sic, iDebug, iPlot_polar_stereo, verbose=False):

	#........................................................
	nrecs = 1
	nlevs = 1               # 1 level
	#........................................................

	dump_day_str = (dump_day.year).__str__() + dump_day.strftime('%m') + dump_day.strftime('%d')
	nc_fName = 'daily_CMIP_' + dump_day_str + '.nc'
	#........................................................
	# http://www.unidata.ucar.edu/software/netcdf/examples/programs/pres_temp_4D_wr.py

	# open a new netCDF file for writing.
	out_ncfile = Dataset( nc_fName, 'w')	

	# create the lat and lon dimensions.
	out_ncfile.createDimension('latitude',  len(lat))
	out_ncfile.createDimension('longitude', len(lon))

	# create level dimension.
	out_ncfile.createDimension('level',nlevs)	

	# create time dimension (record, or unlimited dimension)
	out_ncfile.createDimension('time',None)
	
	# Define the coordinate variables. They will hold the coordinate
	# information, that is, the latitudes and longitudes.
	# Coordinate variables only given for lat and lon.
	lats = out_ncfile.createVariable('latitude',  numpy.dtype('float64').char, ('latitude', ))
	lons = out_ncfile.createVariable('longitude', numpy.dtype('float64').char, ('longitude',))

	# Assign units attributes to coordinate var data. This attaches a
	# text attribute to each of the coordinate variables, containing the units.
	lats.units = 'degrees_north'
	lons.units = 'degrees_east'

	# write data to coordinate vars.
	lats[:] = lat
	lons[:] = lon

	# create the temperature  & ice variables 
	SST = out_ncfile.createVariable('daily_CMIP_SST',numpy.dtype('float64').char,('time','level','latitude','longitude'))
	ICE = out_ncfile.createVariable('daily_CMIP_SIC',numpy.dtype('float64').char,('time','level','latitude','longitude'))

	# set the units attribute.
	SST.units = 'deg_K'
	ICE.units  = 'none'

	# write data to variables along record (unlimited) dimension.
	# same data is written for each record.
	for nrec in range(nrecs):
   		SST[nrec,:,::] = sst
    		ICE[nrec,:,::] = sic

	# close the file.
	out_ncfile.close()
	#........................................................
	print 'Written out fields to...[%s]'%(nc_fName)
	
	if( iDebug):
		coastLineFile   =       '/discover/nobackup/sakella/Stuff/matlabStuff/coastLine.dat'
		[x_coast, y_coast]      = pylab.loadtxt(coastLineFile, unpack=True)	

		plt.figure()
		plt.subplot(211)
                plt.pcolor(lon, lat, numpy.squeeze(sst[0,0,:,:]), vmin=275.0, vmax=305.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
                plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
                plt.title(r'SST [%s]'%(dump_day_str))
		#........................................................
		if( iPlot_polar_stereo):
			plt.subplot(223)
                	nps = Basemap(projection='npstere',boundinglat=45.0,lon_0=360.,resolution='l')
                	nps.drawcoastlines()
                	nps.fillcontinents(color='0.9')
                	nps.drawmeridians(numpy.arange(-180, 180, 30), color='0.5')
                	nps.drawparallels(numpy.arange(-90,  90,  30), color='0.5')
                	xn, yn = nps( *numpy.meshgrid(lon, lat))
                	plt.hold(True)
                	nps.pcolormesh(xn, yn, numpy.squeeze(sic[0,0,:,:]), vmin=0.0, vmax=1.0,shading='flat')
                	plt.colorbar()
                	plt.title(r'NH-SIC')
                	#.................
                	plt.subplot(224)
                	sps = Basemap(projection='spstere',boundinglat=-40.0,lon_0=90,resolution='l')
                	sps.drawcoastlines()
                	sps.fillcontinents(color='0.9')
               		sps.drawmeridians(numpy.arange(-180, 180, 30), color='0.5')
                	sps.drawparallels(numpy.arange(-90,  90,  30), color='0.5')
                	xs, ys = sps( *numpy.meshgrid(lon, lat))
                	sps.pcolormesh(xs, ys, numpy.squeeze(sic[0,0,:,:]), vmin=0.0, vmax=1.0,shading='flat')
                	plt.colorbar()
                	plt.title(r'SH-SIC')
		else:
			plt.subplot(212)
                	plt.pcolor(lon, lat, numpy.squeeze(sic[0,0,:,:]), vmin=0.0, vmax=1.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
                	plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
                	plt.title(r'SIC [%s]'%(dump_day_str))
                #.................
                fName_print_fig = 'daily_cmip_' + dump_day_str + '.png'
                plt.savefig(fName_print_fig,pad_inches=0.5,bbox_inches='tight',dpi=100, format='png')# orientation='landscape')

		plt.close('all')
#----------------------------------------------------------------
if __name__ == '__main__':
        '''
        ??? asdasd sd d sd ???
        
        '''

