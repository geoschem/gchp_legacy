
import os
import sys
import  pylab            as      pylab
import  numpy            as      numpy
import  time             as      time
import  matplotlib.pyplot       as      plt

from    mpl_toolkits.basemap    import  Basemap

sys.path.append('/discover/nobackup/sakella/processData/MERRA2_27June2013/UTILS/')
from read_bin import read_bin
#----------------------------------------------------------------
def compute_mean_clim(iSST, fNames, num_years, iDebug, iPlot_polar_stereo, verbose=False):

	if( num_years - fNames.__len__() != 0):
                print 'ERROR! Num of years does not match number of input files'
                print 'WRONG CLIMATOLOGY!!'

        #....................................................................................................
        if( iDebug):
                print 'Working on...[%s]'%(fNames[0])
        # Initialize
	[nymd1, nhms1, nymd2, nhms2, NLON, NLAT, XX] = read_bin(fNames[0])			# X= SST/SIC

	SUM_X = XX
	#....................................................................................................
        # Accumulate sum for computing mean
	for iYears in range(1, num_years):
                if( iDebug):
                        print 'Working on...[%s]'%(fNames[iYears])
		
		[nymd1, nhms1, nymd2, nhms2, NLON, NLAT, X] = read_bin(fNames[iYears])	# X= SST/SIC
		SUM_X = SUM_X + X
        #....................................................................................................
        mean_X = SUM_X/numpy.float32(num_years)

	ff = fNames[0]
	if( iDebug):
		coastLineFile   	= '/discover/nobackup/sakella/Stuff/matlabStuff/coastLine.dat'
		[x_coast, y_coast]      = pylab.loadtxt(coastLineFile, unpack=True)

		dlon = 360.0/numpy.float32(NLON)
		dlat = 180.0/numpy.float32(NLAT)
		lon  = numpy.linspace(-180.0+dlon/2, 180.0-dlon/2, NLON)
		lat  = numpy.linspace(-90.0 +dlat/2,  90.0-dlat/2, NLAT)

		if( iPlot_polar_stereo):
			plt.subplot(121) 
			nps = Basemap(projection='npstere',boundinglat=45.0,lon_0=360.,resolution='l')
			nps.drawcoastlines()
                	nps.fillcontinents(color='0.9')
                	nps.drawmeridians(numpy.arange(-180, 180, 30), color='0.5')
                	nps.drawparallels(numpy.arange(-90,  90,  30), color='0.5')
                	xn, yn = nps( *numpy.meshgrid(lon, lat))
                	plt.hold(True)
			#nps.pcolormesh(xn, yn, numpy.transpose(mean_X-XX), vmin=-1.0, vmax=1.0,shading='flat')
                	#plt.colorbar(), plt.title(r' NH Daily Anolmaly- [%s]'%(ff[len(ff)-8:len(ff)-4]))
			if( iSST):
				nps.pcolormesh(xn, yn, numpy.transpose(mean_X), vmin=280.0, vmax=305.0,shading='flat')
			else:
				nps.pcolormesh(xn, yn, numpy.transpose(mean_X), vmin=0.0,   vmax=1.0,shading='flat')

                	plt.colorbar(), plt.title(r' NH Daily CLIM- [%s]'%(ff[len(ff)-8:len(ff)-4]))
			#......................................................................................
			plt.subplot(122) 
			sps = Basemap(projection='spstere',boundinglat=-40.0,lon_0=90,resolution='l')
                	sps.drawcoastlines()
                	sps.fillcontinents(color='0.9')
                	sps.drawmeridians(numpy.arange(-180, 180, 30), color='0.5')
                	sps.drawparallels(numpy.arange(-90,  90,  30), color='0.5')
                	xs, ys = sps( *numpy.meshgrid(lon, lat))
                	#sps.pcolormesh(xs, ys, numpy.transpose(mean_X-XX), vmin=-1.0, vmax=1.0,shading='flat')
                	#plt.colorbar(), plt.title(r' SH Daily Anomaly- [%s]'%(ff[len(ff)-8:len(ff)-4]))
			if( iSST):
                		sps.pcolormesh(xs, ys, numpy.transpose(mean_X), vmin=280.0, vmax=305.0,shading='flat')
			else:
                		sps.pcolormesh(xs, ys, numpy.transpose(mean_X), vmin=0.0,   vmax=1.0,shading='flat')
                	plt.colorbar(), plt.title(r' SH Daily CLIM- [%s]'%(ff[len(ff)-8:len(ff)-4]))
		else:
			plt.subplot(111) 
			#plt.pcolormesh(lon, lat, numpy.transpose(mean_X-XX), vmin=-1.0, vmax=1.0)
			#plt.title(r' Daily Anomaly- [%s]'%(ff[len(ff)-8:len(ff)-4]))
			if( iSST):
				plt.pcolormesh(lon, lat, numpy.transpose(mean_X), vmin=280.0, vmax=305.0)
			else:
				plt.pcolormesh(lon, lat, numpy.transpose(mean_X), vmin=0.0,   vmax=1.0)
			plt.title(r' Daily CLIM- [%s]'%(ff[len(ff)-8:len(ff)-4]))
			plt.colorbar(), plt.axis('off'), plt.axis('tight')
			plt.hold('true')
			plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
		#......................................................................................
		if( iSST):
			#fName_print_fig = 'daily_SST_anom_' + ff[len(ff)-8:len(ff)-4] + '.png'	
			fName_print_fig = 'daily_SST_CLIM_' + ff[len(ff)-8:len(ff)-4] + '.png'	
		else:
			#fName_print_fig = 'daily_SIC_anom_' + ff[len(ff)-8:len(ff)-4] + '.png'	
			fName_print_fig = 'daily_SIC_CLIM_' + ff[len(ff)-8:len(ff)-4] + '.png'	
		plt.savefig(fName_print_fig,pad_inches=0.5,bbox_inches='tight',dpi=120, format='png')# orientation='landscape')
		plt.close('all')
        #....................................................................................................
	return mean_X
#----------------------------------------------------------------
if __name__ == '__main__':
        '''
        ??? asdasd sd d sd ???
        
        '''



