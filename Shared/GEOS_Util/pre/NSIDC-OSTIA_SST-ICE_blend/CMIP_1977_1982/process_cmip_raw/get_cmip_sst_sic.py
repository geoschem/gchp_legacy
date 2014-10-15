
'''
read in sst & sic from raw cmip files. fields are mid-month fields.
'''
import os
import sys

import  numpy                   as      numpy
import  pylab                   as      pylab
import  matplotlib.pyplot       as      plt
import  matplotlib.dates        as	dates

from 	datetime		import	datetime
from    mpl_toolkits.basemap    import  Basemap

sys.path.append('/home/sakella/python_docs/myStuff/')
from read_netCDF                import read_netCDF as get_field_netCDF_file
#----------------------------------------------------------------
def get_cmip_sst_sic(fName_sst, fName_sic, iDebug, verbose=False):

	lon1           = get_field_netCDF_file(fName_sst, "longitude")
	lat            = get_field_netCDF_file(fName_sst, "latitude")
	time1          = get_field_netCDF_file(fName_sst, "time")

	sst1            = get_field_netCDF_file(fName_sst, "tosbcs")
	sic1            = get_field_netCDF_file(fName_sic, "sicbcs")/100.0  	# to get [0,1] concentration.
	
	#sst_fill       = 1.0e+20
	#ice_fill       = 1.0e+20

	#if( iDebug):
	#	print 'Min/Max of input  SST...[%f], [%f]'%(sst.flatten().min(), sst.flatten().max())
	#	print 'Min/Max of input  ICE...[%f], [%f]'%(sic.flatten().min(), sic.flatten().max())
	#----------------------------------------------------------------
	# data set start date:
	date0           = datetime(1979,01,01,00,0,0)
	timeX           = time1 + dates.date2num(date0)				# adjust time stamps

	sst1[sic1 > 1.0]   = 273.15
	sst1[sic1 < 0.0]   = 273.15						# not sure if this is okay! Jul 25, 2013.

	sic1[sic1 > 1.0]   = 1.0
	sic1[sic1 < 0.0]   = 0.0
	#----------------------------------------------------------------
	# lon is from [0.5, 359.5]. flip it to [-179.5 to 179.5].
	lon = lon1 - 180.0
	
	sst = numpy.zeros([numpy.shape(sst1)[0], numpy.shape(sst1)[1], numpy.shape(sst1)[2]], dtype=numpy.float64, order = 'F')
	sic = numpy.zeros([numpy.shape(sic1)[0], numpy.shape(sic1)[1], numpy.shape(sic1)[2]], dtype=numpy.float64, order = 'F')

	for iMon in range(0, numpy.shape(sst1)[0]):
		sst[iMon,:,180:360] = numpy.squeeze(sst1[iMon,:,  0:180])
		sst[iMon,:,  0:180] = numpy.squeeze(sst1[iMon,:,180:360])

		sic[iMon,:,180:360] = numpy.squeeze(sic1[iMon,:,  0:180])
		sic[iMon,:,  0:180] = numpy.squeeze(sic1[iMon,:,180:360])
	#----------------------------------------------------------------
	print 'Working on Year...[%s]'%(dates.num2date(timeX)[0].year)
	if( iDebug):
		iPlot_month = 8
		#print 'Working on Year...[%s]'%(dates.num2date(timeX)[iPlot_month].year)
		print 'Min/Max of input SST...[%f], [%f]'%(sst.flatten().min(), sst.flatten().max())
		print 'Min/Max of input ICE...[%f], [%f]'%(sic.flatten().min(), sic.flatten().max())

		coastLineFile   =       '/discover/nobackup/sakella/Stuff/matlabStuff/coastLine.dat'
		[x_coast, y_coast]      = pylab.loadtxt(coastLineFile, unpack=True)
		#
		plt.figure()
		plt.subplot(211)
		plt.pcolor(lon, lat, numpy.squeeze(sst[iPlot_month,:,:]), vmin=275.0, vmax=305.0), plt.colorbar(), plt.axis('off'), plt.hold('true')
		plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
                plt.title(r'SST [%s/%s]'%(dates.num2date(timeX)[iPlot_month].month, dates.num2date(timeX)[iPlot_month].year))

		plt.subplot(212)
		plt.pcolor(lon, lat, numpy.squeeze(sic[iPlot_month,:,:]), vmin=0.0, vmax=1.0), plt.colorbar(), plt.axis('off'), plt.hold('true') 
		plt.plot(x_coast, y_coast, 'k-'),plt.axis('tight')
                plt.title(r'SIC [%s/%s]'%(dates.num2date(timeX)[iPlot_month].month, dates.num2date(timeX)[iPlot_month].year))
		#	
		#plt.show()
		print ' '
		fName_print = 'raw_cmip_' + '%s_%s'%(dates.num2date(timeX)[iPlot_month].month, dates.num2date(timeX)[iPlot_month].year) + '.png'
		plt.savefig(fName_print, dpi=100, format='png')
		plt.close('all')
	#----------------------------------------------------------------
	return lon, lat, timeX, sst, sic
#----------------------------------------------------------------
if __name__ == '__main__':
        '''
        ??? asdasd sd d sd ???
        
        '''

