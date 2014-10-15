"""
    Very simple object-oriented Python interface to the GFIO library.
    It can be used to read and write COARDS compatible files.
 
"""

import os
from numpy    import linspace, ones, zeros, any, array, float32, tile
from datetime import datetime, timedelta

from GFIO_ import *

class GFIOHandle(object):
    """
    A simple container class to collect GFIO arrays.
    """
    def __init__ (self, name):
        self.name = name
    
class GFIOError(Exception):
    """
    Defines GFIO general exception errors.
    """
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

class GFIO(object):

    def __init__(self,filename=None,mode='r'):
        """
        Open a file GFIO file for reading or writting. All relevant
        metadata regarding variables and coordinates are retrieved
        and stored in the object. Examine the contents of __dict__
        to see what is available.
        """

        if filename is None:
            self.filename = None
            self.fid = None
            return
        
        self.filename = filename

#       Open an existing file
#       ---------------------
        if mode == 'r':
            self.fid, rc = gfioopen(filename,1)
        else:
            self.fid, rc = gfioopen(filename,0)

        if rc:
            raise GFIOError, "cannot open GFIO file " + filename

#       Fetch dimensions
#       ----------------
        self.im,self.jm,self.km,self.lm,self.nvars,self.ngatts,rc = gfiodiminquire(self.fid)
        if rc:
            raise GFIOError, "cannot get dimensions for GFIO file " + filename

#       Fetch metadata
#       --------------
        km_ = max(self.km,1)
        self.title,       self.source,  self.contact,   self.amiss,         \
        self.lon,         self.lat,     self.levs,      self.levunits,      \
        self.yyyymmdd,    self.hhmmss,  self.timinc,                        \
        self.vname,       self.vtitle,  self.vunits,    self.kmvar,         \
        self.valid_range, self.packing_range,rc                             \
            = gfioinquire(self.fid,self.im,self.jm,km_,self.lm,self.nvars)

        if rc==3:
            raise GFIOError, "cannot get COARDS metadata for GFIO file " \
                           + filename + ", rc = " + str(rc)

#       Create  variable property dictionaries
#       --------------------------------------
        self.vname  = self.vname.split(':')[:-1]
        self.vtitle = self.vtitle.split(':')[:-1]
        self.vunits = self.vunits.split(':')[:-1]

#       Hash with number of levels by variable name
#       -------------------------------------------
        self.kmvar_name = {}

        for i in range(len(self.vname)):
            self.kmvar_name[self.vname[i]] = self.kmvar[i]

#---
    def read(self,vname,nymd=None,nhms=None,kbeg=None,kount=None,squeeze=True):
        """
        Reads a variable at a given time/date, or the first time on file if
        nymd/nhms is not specified. By default it returns all vertical
        levels, but a range can be specified with the first vertical
        index (kbeg) and the number of vertical levels to be read (kount).

        Example:
        -------
        from gfio import GFIO
        f = GFIO('test_py.nc') # open existing file
        u = f.read('u')        # read variable u
        
        """

        if nymd is None:
            nymd, nhms, dt, rc = gfiogetbegdatetime(self.fid)
            if rc:
                raise GFIOError, "cannot get initial date/time for GFIO file "+self.filename

        if kbeg is None:
            try:
                kmv = self.kmvar_name[vname] 
            except:
                # print self.vname
                raise GFIOError, "variable <"+vname+"> not present in GFIO file"
            if kmv == 0:
                kbeg = 0 # 2D
                kount = 1
            else:
                kbeg = 1 # 3D
                kount = self.km
    
        var, rc = gfiogetvar(self.fid,vname,nymd,nhms,self.im,self.jm,kbeg,kount)
        if rc:
            raise IOError, "cannot read <"+vname+"> from GFIO file "+self.filename+' at %d %d'%(nymd,nhms)

        if squeeze:
            var = var.squeeze()

        return var

#---
    def create(self, filename, vname,
               nymd_beg, nhms_beg,
               lon=None, lat=None, refine=None, res=None,
               vtitle=None,
               vunits=None,
               timinc=60000,
               kmvar=None,
               levs=[1000.,],
               levunits='hPa',
               title='Produced with PyGFIO',
               source='NASA/GSFC/GMAO',
               contact='unknown',
               amiss=1.e+20,
               valid_range=None,
               packing_range=None,
               prec=0):

        """
        Creates a GFIO dataset.The following parameters are mandatory:

          filename  ---  output file name
          vname     ---  list of variables to write to file
          nymd_beg  ---  first date on file, e.g., 20080630
          nhms_beg  ---  first time on file, e.g., 12000000

        For the *horizontal* coordinates, either

          lon, lat  ---  list of longitudes/latitudes, in degrees

        or

          refine    ---  refinement for the standard GEOS-5 4x5
                         A-Grid. For example,
                         refine=1  produces a   4  x  5    grid
                         refine=2  produces a   2  x2.50   grid
                         refine=4  produces a   1  x1,25   grid
                         refine=8  produces a  0.50x0.625  grid
                         refine=16 produces a  0.25x0.3125 grid

        or even

          res       ---  single letter denoting GEOS-5 resolution,
                         res='a'  produces a   4  x  5    grid
                         res='b'  produces a   2  x2.50   grid
                         res='c'  produces a   1  x1,25   grid
                         res='d'  produces a  0.50x0.625  grid
                         res='e'  produces a  0.25x0.3125 grid


        For 3D datasets, you must specify:

          levs      ---  vertical levels
          levunits  ---  units of the vertical levels, per COARDS
                         conventions
        For datasets that will have more than 1 time step in it, you
        must specify

          timinc    ---  time step, in HHMMSS format (integer)

        It is also recommended that you specify

          vtitle    ---  list of variable long names (default=vname)
          vunits    ---  list of variable units (default='unknown')

        Optional parameters:

          amiss     ---  missing values
          valid_range
                    ---  array of shape (2,nvars)
                         Variable valid range; method write() will return 
                         an error if a value is outside of this range.
                         IMPORTANT: If packing is not desired for a given
                                    variable, YOU MUST set both components
                                    of valid_range to amiss; this is the
                                    default.
          packing_range
                    ---  array of shape (2,nvars)
                         Packing range to be used for 16-bit packing 
                         of each variable. IMPORTANT: If packing is not 
                         desired for a given variable, YOU MUST set both
                         components of packing_range to amiss.
                         NOTE:
                         * The packing algorithm sets all values
                           outside the packing range to missing.
                         * The larger the packing range, the greater
                           the loss of precision.

          prec      ---  Desired precision of data:
                           0 = 32 bit
                           1 = 64 bit

        Consult the GFIO documentation for additional information.

        Example:
        -------
        
        from gfio import GFIO

        filename = 'test_py.nc'
        vname = ['u','v']
        vtitle = ['zonal wind','meridional wind']
        vunits = ['m/s','m/s']
        nymd = 20080630
        nhms = 120000

        f = GFIO()
        f.create(filename, vname, nymd, nhms, res='c', 
                    vtitle=vtitle, vunits=vunits)

        u = zeros((f.im,f.jm))
        v =  ones((f.im,f.jm))

        f.write('u',nymd,nhms,u)
        f.write('v',nymd,nhms,v)
        f.close()

        """

        self.filename = filename
        nvars = len(vname)

#       Defaults
#       --------
        if lon is None or lat is None:

            if res is not None:
                if res=='a': refine = 1 
                if res=='b': refine = 2
                if res=='c': refine = 4
                if res=='d': refine = 8
                if res=='e': refine = 16
                
            if refine is None:
                raise IOError, 'must specify either lat/lon, res or refine'

            dx = 5. / refine
            dy = 4. / refine
            im = int(360. / dx)
            jm = int(180. / dy + 1)

            lon = linspace(-180.,180.,im,endpoint=False)
            lat = linspace(-90.,90.,jm)
            
        if vtitle is None:
            vtitle = vname
        if vunits is None:
            vunits = nvars * ['unknown',]
        if valid_range is None:
            valid_range = amiss * ones((2,nvars))
        if packing_range is None:
            packing_range = amiss * ones((2,nvars))
        if kmvar is None:
            kmvar = zeros(nvars)
            
#       Do not know howw to pass string array to f2py
#       ---------------------------------------------
        vname_ = ":".join(vname)   + ':'
        vtitle_ = ":".join(vtitle) + ':'
        vunits_ = ":".join(vunits) + ':'

        self.fid, rc = gfiocreate(filename,title,source,contact,amiss,\
                                  lon,lat,levs,levunits,\
                                  nymd_beg,nhms_beg,timinc,\
                                  vname_,vtitle_,vunits_,kmvar,\
                                  valid_range,packing_range,prec)
        
        if rc:
            raise GFIOError, "cannot create "+self.filename+", rc=%d"%rc

#       Save relevant metadata
#       ----------------------
        self.im, self.jm = (lon.size,lat.size)
        self.lon, self.lat = (lon,lat)
        self.levs, self.levunits = (levs,levunits)
        self.nvars = nvars
        self.vname, self.vtitle, self.vunits = (vname,vtitle,vunits)
        self.title, self.source, self.contact = (title,source,contact)
        self.kmvar = kmvar
        self.amiss = amiss
        self.timinc = timinc
        self.valid_range = valid_range
        self.packing_range = packing_range
        self.nymd_beg, self.nhms_beg = (nymd_beg,nhms_beg)

#       Hash with number of levels by variable name
#       -------------------------------------------
        self.kmvar_name = {}
        for i in range(self.nvars):
            self.kmvar_name[self.vname[i]] = self.kmvar[i]       

#---
    def write(self,vname,nymd,nhms,var,kbeg=None,kount=None):
        """
        Writes a variable to file at a given date/time. See
        method create() for an example.
        """

#       2D variables
#       ------------
        if self.kmvar_name[vname] == 0:
            kbeg = 0
            kount = 1
            
#       3D variables
#       ------------
        else:
            if kbeg is None:
                kbeg = 1
            if kount is None:
                kount = self.kmvar_name[vname]

        rc = gfioputvar(self.fid,vname,nymd,nhms,kbeg,var)
        if rc:
            raise GFIOError, "cannot write <%s> to file %s, rc=%d "%\
                  (vname,self.filename,rc)

#---
    def interp(self,vname,lon,lat,nymd=None,nhms=None,kbeg=None,kount=None,
               squeeze=True, algorithm='linear'):
        """
        Reads a variable at a given time/date, or the first time on file if
        nymd/nhms is not specified, and interpolates it to the observation
        locations given by the list of (lon,lat) on input. By default it returns 
        all vertical levels of the variable, but a range can be specified with 
        the first vertical index (kbeg) and the number of vertical levels to be 
        read (kount). Algorithm can be either 'linear' or 'nearest' for nearest
        neighbors. 

        Example:
	
	    v = self.interp('delp',lon,lat)

        When the variable being interpolated is 2-D, the output array will
	have shape (nobs,); when the variable being interpolated is 3-D, the
	output array will have shape (km,nobs), where nobs = len(lon), and
	km is the number of vertical levels being requested.
	    
        """

#       First read the gridded field
#       ----------------------------
        var = self.read(vname,nymd,nhms,kbeg,kount,squeeze=False)

#       Check consistency of longitudes
#       -------------------------------
        if any(lon<0):
            if any(self.lon>180.):
                raise GFIOError, \
                  "inconsistent longitudes, obs in [-180,180] but grid in [0,360]"
        elif any(lon>180):
            if any(self.lon<0.):
                raise ValueError, \
                   "inconsistent longitudes, obs in [0,360] but grid in [-180,180]"

#       Next interpolate (assumes global, zonally periodic grids)
#       ---------------------------------------------------------
        if algorithm == 'linear':
            ivar = gfiointerpxy(lon,lat,self.lon[0],var)
        elif algorithm == 'nearest':
            ivar = gfiointerpnn(lon,lat,self.lon[0],var)
        else:
            raise ValueError, 'Unknown algorithm <%s>, expecting "linear" or "nearest"'%algorithm 

        if squeeze:
            ivar = ivar.squeeze()

        return ivar

#---
    def coordNN(self,lon,lat):
        """
        Returns the (i,j) coordinates used when using nearest neighboprs interpolation.
        This is useful for some independent column approximatioin algorithms.
	    
        """

#       Check consistency of longitudes
#       -------------------------------
        if any(lon<0):
            if any(self.lon>180.):
                raise GFIOError, \
                  "inconsistent longitudes, obs in [-180,180] but grid in [0,360]"
        elif any(lon>180):
            if any(self.lon<0.):
                raise ValueError, \
                   "inconsistent longitudes, obs in [0,360] but grid in [-180,180]"

#       Get the coordinates
#       --------------------
        iCoord, jCoord = gfiocoordnn(lon,lat,self.im,self.jm,self.lon[0])

        return (iCoord,jCoord)

#---
    def close(self):
        """Closes a GFIO file."""
        if self.fid is not None:
            rc = gfioclose(self.fid)
	    self.fid=None

#---
    def __del__(self):
        if self.fid is not None:
            rc = gfioclose(self.fid)

    def attinquire( self, name ):
        type,count,rc = gfioattinquire(self.fid, name)
        return type,count,rc

    def getcharatt( self, name ):

        type,count,rc = self.attinquire( name )
        if( type == 2) :
            history, rc = gfiogetcharatt( self.fid, name, count )
            if rc == 0:
                return history.tostring()
        raise GFIOError, 'could not retrieve character global attribute <%s>'%name

    def getvarmissing( self, varname ):
        missingvalue, rc =  gfiogetvarmissvalue( self.fid, varname, 1 )
        if rc == 0:
            return missingvalue[0]
        else:
            raise GFIOError, 'could not retrieve missing value for variable <%s>'%varname

    def getvarcharatt( self, varname, att ):
        type,count,rc = gfiovarattinquire ( self.fid, varname, att )
        if( rc == 0 ):
            bufc, rc =  gfiogetvarcharatt( self.fid, varname, att, count )
            return bufc.tostring()
        raise GFIOError, 'could not retrieve character attribute <%s> for variable "%s"'%(att,varname)

#.........................................................................................................

class GFIOctl(object):
    """
    Generic class to implement an aggregation functionality similar to
    templates in a GrADS control file. For simplicity, this class is
    only implemented for reading files, not writing them.
    """

    def __init__ (self, ctlfile ):
        """
        Initialize an aggregated GFIO object.
        """
        # Parse CTL file
        # --------------
        CTL = open(ctlfile).readlines()
        dset, template, nt = (None,False, None)
        for line in CTL:
            tokens = line.replace('\r','').replace('\n','').split()
            keyw = tokens[0].upper()
            if keyw== 'DSET':
                dset = tokens[1]
            elif keyw == 'OPTIONS':
                if 'TEMPLATE' in line.upper():
                    template = True
            if keyw == 'TDEF':
                if len(tokens) == 5:
                    tdef, nt, linear, t0, dt = tokens
                elif len(tokens) == 6:
                    tdef, dim, nt, linear, t0, dt = tokens
                else:
                    raise ValueError, 'Invalid TDEF record: '+line

        # Consistency check
        # -----------------
        if dset is None or nt is None:
            raise ValueError, '<%s> does not seem to be a valid GrADS control file'%ctlfile
        else:
            if '^' in dset:
                dirn = os.path.dirname(ctlfile)
                dset = dset.replace('^',dirn+'/')
        if template is False:
            raise ValueError, '<%s> does not seem to be templated'%ctlfile

        # Handle time attributes
        # ----------------------
        dt = dt.lower()
        if 'hr' in dt:
            secs = int(dt.replace('hr','')) * 60 * 60
        elif 'mn' in dt:
            secs = int(dt.replace('mn','')) * 60
        elif 'dy' in dt:
            secs = int(dt.replace('dy','')) * 24 * 60 * 60
        else:
            raise ValueError, 'invalid time step <%s>'%dt 
        dt = timedelta(seconds=secs)

        # Save this
        # ---------
        self.dset = dset
        self.lm = int(nt)
        self.dt = dt
        self.tbeg = _gat2dt(t0)
        self.tend = self.tbeg + (self.lm-1) * self.dt
        self.Files = dict()

        # Open GFIO file for first time
        # -----------------------------
        filename = _strTemplate(self.dset,dtime=self.tbeg)
        self.Files[filename] = GFIO(filename)
        self.gfio = self.Files[filename]

        for att in ('title','source','contact','amiss',
                    'im','jm','km','lon','lat','levs','levunits',
                    'vname', 'vtitle', 'vunits', 'kmvar', 'kmvar_name',
                    'valid_range','packing_range',):
            self.__dict__[att] = self.gfio.__dict__[att]
        
#---
    def read(self,vname,nymd=None,nhms=None,dtime=None,**kwds):
        """
        Reads a variable at a given time/date, or the first time on file if
        nymd/nhms is not specified. By default it returns all vertical
        levels, but a range can be specified with the first vertical
        index (kbeg) and the number of vertical levels to be read (kount).
        The keyword arguments **kwds are passed to the GFIO method read().
        """

        if dtime != None:
            nymd = dtime.year*10000 + dtime.month*100 + dtime.day
            nhms = dtime.hour*10000 + dtime.minute*100 + int(dtime.second)
            
        if None in (nymd,nhms):
            pass # should we raise exception instead?
            gfio = self.gfio
        else:
            filename = _strTemplate(self.dset,nymd=nymd,nhms=nhms)
            if self.Files.has_key(filename) == False:
                gfio = GFIO(filename)
                self.Files[filename] = gfio
            else:
                gfio = self.Files[filename]

        self.gfio = gfio
        return gfio.read(vname,nymd=nymd,nhms=nhms,**kwds)

#---
    def tbracket (self, t):
        """
        Given (t1,t2) find bracketing times on file.
        """
        if t<self.tbeg:
            raise ValueError, '%s before %s'%(str(t),str(self.tbeg))
        elif t>self.tend:
            raise ValueError, '%s after %s'%(str(t),str(self.tend))

        dt = t - self.tbeg
        i = int(dt.total_seconds() / self.dt.total_seconds())
        t1 = self.tbeg + i * self.dt
        t2 = t1 + self.dt
        return (t1,t2)
    
#---
    def trange(self, t1, t2):
        """
        Return file times between t1 & t2.
        """
        ta, tb = self.tbracket(t1)
        tc, td = self.tbracket(t2)
        if tc==t2:
            td = tc
        T = []
        t = ta
        while t<=td:
            T.append(t)
            t += self.dt

        return T
                
#---
    def interpXY(self,vname,lon,lat,nymd=None,nhms=None,dtime=None,
		 Transpose=False, algorithm='linear',**kwds):
        """
        Reads a variable at a given time/date, or the first time on
        file if nymd/nhms is not specified, and interpolates it to the
        observation locations given by the list of (lon,lat) on
        input. By default it returns all vertical levels of the
        variable, but a range can be specified with the first vertical
        index (kbeg) and the number of vertical levels to be read
        (kount). Keyword arguments **kwds are passed to the GFIO
        interp() method.

        Example:
	
	    v = self.interpXY('delp',lon,lat)

        By default, when the variable being interpolated is 2-D, the
	output array will have shape (nobs,); when the variable being
	interpolated is 3-D, the output array will have shape
	(km,nobs), where nobs = len(lon), and km is the number of
	vertical levels being requested. If Transpose=True, the output
	array is transposed.
	    
        """

        if dtime != None:
            nymd = dtime.year*10000 + dtime.month*100 + dtime.day
            nhms = dtime.hour*10000 + dtime.minute*100 + int(dtime.second)
            
        if None in (nymd,nhms):
            pass # should we raise exception instead?
            gfio = self.gfio
        else:
            filename = _strTemplate(self.dset,nymd=nymd,nhms=nhms)
            if self.Files.has_key(filename) == False:
                gfio = GFIO(filename)
                self.Files[filename] = gfio
            else:
                gfio = self.Files[filename]

        self.gfio = gfio
        v = gfio.interp(vname,lon,lat,nymd=nymd,nhms=nhms,
                        algorithm=algorithm,**kwds)
	if Transpose: v = v.T  # transpose to (nobs,km)

        return v

#---
    def coordNN(self,lon,lat):
        """
        Returns the (i,j) coordinates used when using nearest neighboprs interpolation.
        This is useful for some independent column approximatioin algorithms.
        """

        return self.gfio.coordNN(lon,lat)
        
#---
    interp = interpXY  # backward compatibility

    def sample(self,vname,lon,lat,time,
               squeeze=True,Transpose=False,
               algorithm='linear',Verbose=False,**kwopts):
        """
        Interpolates *vname* to (lon,lat,time) trajectory;
        time must be in ascending order. Keyword arguments **kwds are
        passed to the interpXY method().

	By default, output arrays have shape (km,nobs), where *km* is the
	requested number of verical levels. If Transpose=True is specified,
	then the output arrays will be transposed to (nobs,km).
	
        """

        # Inputs must be 1D arrays
        # ------------------------
        if len(lon.shape)!=1 or len(lat.shape)!=1 or len(time.shape)!=1:
            raise ValueError, "lons, lats, time must be 1D arrays"
        
        # Find times bracketing the input time array
        # ------------------------------------------
        Times = self.trange(time.min(),time.max())
        dt, dt_secs = (self.dt, self.dt.total_seconds())

        # Loop over time, producing XY interpolation at each time
        # -------------------------------------------------------
        V, I = [], []
        for now in Times:
            if Verbose: print " [] XY Interpolating <%s> at "%vname,now
            i = (time>=now-dt) & (time<=now+dt)
            if any(i):
                v = self.interpXY(vname, lon[i], lat[i],dtime=now,
				  Transpose=True, # shape will be (nobs,km)
				  squeeze=False,algorithm=algorithm,**kwopts)
		shp = list(v.shape)
            else:
                v = None
            V.append(v)
            I.append(i)
            
        # Now perform the time interpolation
        # ----------------------------------
        N = len(lon)
        km = self.kmvar_name[vname]
        if km>1:
            shp = [N,km]
        else:
            shp = [N,]
        v  = zeros(shp,dtype=float32)
        v1, v2 = v.copy(), v.copy() # scratch space
        n = 0
        for now in Times[:-1]:
            v1[I[n]], v2[I[n+1]] = V[n], V[n+1]
            j = (time>=now) & (time<=now+dt)
            if any(j): 
                a = array([r.total_seconds()/dt_secs for r in time[j]-now],dtype=float32) 
                if len(shp)==2: # has vertical levels
                    a = tile(a,(shp[1],1)).T # replicate array
                v[j] = (1-a) * v1[j] + a * v2[j]
            n += 1

	if Transpose == False: v = v.T # back to GFIO's (km,nobs)
        if squeeze == True:    v = v.squeeze()


	return v

#---
    def sampleVars(self, lon, lat, tyme, npzFile=None, onlyVars=None,
                   squeeze=True,Transpose=False, algorithm='linear',Verbose=True):
        """
        Interpolates all variables to (lon,lat,tim) and optionally save
	them to file *npzFile*

            sample = sampleVars(...)

        where *sample* is a GFIOhandle object.
	        
	By default, output arrays have shape (km,nobs), where *km* is the
	requested number of verical levels. If Transpose=True is specified,
	then the output arrays will be transposed to (nobs,km).

        """
 
        # Instiate grads and open file
        # ----------------------------
        sample_ = GFIOHandle('sample')
        if onlyVars is None:
            onlyVars = self.vname

        # Loop over variables on file
        # ---------------------------
        for v in onlyVars:

            var = self.sample(v,lon,lat,tyme,squeeze=squeeze,
                              algorithm=algorithm, Verbose=Verbose)

            if Transpose and len(var.shape) == 2:
                var = var.T # shape should be (nobs,nz)

            sample_.__dict__[v] = var

        if npzFile is not None:
            savez(npzFile,**sample_.__dict__)            

        return sample_
    
#...........................................................................

__Months__ = ['JAN','FEB','MAR','APR','MAY','JUN',
              'JUL','AUG','SEP','OCT','NOV','DEC']

def _strTemplate(templ,expid=None,nymd=None,nhms=None,
                    yy=None,mm=None,dd=None,h=None,m=None,s=None,
                    dtime=None):
    """
    Expands GrADS template in string *templ*. On input,

       expid ---  experiment id, expands %s
       yy    ---  year, expands %y4 and %y2
       mm    ---  month, expands %m2 or %m3
       dd    ---  day, expands %d2
       h     ---  hour, expands %h2
       m     ---  minute, expands %n2
       s     ---  minute, expands %S2 (notice capital "S")

       nymd  ---  same as yy*10000 + mm*100 + dd
       nhms  ---  same as h *10000 + h*100  + s

       dtime ---  python datetime

    Unlike GrADS, notice that seconds are expanded using the %S2 token. 
    Input date/time can be either strings or integers.

    Examples:

    >>> templ = "%s.aer_f.eta.%m3%y2.%y4%m2%d2_%h2:%n2:%S2z.nc"
    >>> print strTemplate(templ,expid="e0054A",yy=2008,mm=6,dd=30,h=1,m=30,s=47)
    e0054A.aer_f.eta.jun08.20080630_01:30:47z.nc
    >>> print strTemplate(templ,expid="e0054A",nymd=20080630,nhms=13000)
    e0054A.aer_f.eta.jun08.20080630_01:30:00z.nc

    NOTE: This function exists in MAPL/config.py; it is copied here for
          dependency management.
          
    """

    MMM = ( 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 
            'jul', 'aug', 'sep', 'oct', 'nov', 'dec' ) 
    
    str_ = templ[:]

    if dtime is not None:
        yy = dtime.year
        mm = dtime.month
        dd = dtime.day
        h  = dtime.hour
        m  = dtime.minute
        s  = dtime.second

    if nymd is not None:
        nymd = int(nymd)
        yy = nymd/10000
        mm = (nymd - yy*10000)/100
        dd = nymd - (10000*yy + 100*mm )

    if nhms is not None:
        nhms = int(nhms)
        h = nhms/10000
        m = (nhms - h * 10000)/100
        s = nhms - (10000*h + 100*m)

    if expid is not None: 
        str_ = str_.replace('%s',expid)
    if yy is not None: 
        y2 = yy%100
        str_ = str_.replace('%y4',str(yy))
        str_ = str_.replace('%y2',"%02d"%y2)
    if mm is not None: 
        mm = int(mm)
        mmm = MMM[mm-1]
        str_ = str_.replace('%m2',"%02d"%mm)
        str_ = str_.replace('%m3',mmm)
    if dd is not None: 
        str_ = str_.replace('%d2',"%02d"%int(dd))
    if h  is not None: 
        str_ = str_.replace('%h2',"%02d"%int(h))
    if m  is not None: 
        str_ = str_.replace('%n2',"%02d"%int(m))
    if s  is not None: 
        str_ = str_.replace('%S2',"%02d"%int(s))

    return str_

#...........................................................................

def _gat2dt(gat):
    """
    Convert grads time to datetime.
    """
    time, date = gat.upper().split('Z')
    if time.count(':') > 0:
        h, m = time.split(":")
    else:
        h = time
        m = '0'
    mmm = date[-7:-4]
    dd, yy = date.split(mmm)
    mm = __Months__.index(mmm) + 1
    dt = datetime(int(yy),int(mm),int(dd),int(h),int(m))
    return dt

#...........................................................................

if __name__ == "__main__":

    from time import time as now
    from grads import GrADS
    
    f = GFIOctl('/nobackup/ARCTAS/opendap/arctas.ddf')

    lons = linspace(-45.,-30.,100)
    lats = linspace(30.,50.,100)
    dt = f.dt/10
    t0 = datetime(2008,6,29,12)
    times = array([ t0 + i * dt for i in range(100)])

    ga = GrADS(Window=False,Echo=0)
    fh = ga.open('/nobackup/ARCTAS/opendap/arctas.ddf')

    # Interpolating ps
    # ----------------
    t_ = now()
    ps = f.sample('ps',lons,lats,times,Verbose=True)
    print "---> Interpolating <ps> with GFIO took ", now()-t_
    t_ = now()
    ps_ = ga.sampleXYT('ps',lons,lats,tyme=times,Verbose=True)
    print "---> Interpolating <ps> with PyGrADS took ", now()-t_

    # Interpolating RH
    # ----------------
    print ""
    t_ = now()
    rh = f.sample('RH',lons,lats,times,Verbose=True)
    print "---> Interpolating RH with GFIO took ", now()-t_
    t_ = now()
    ga('set z 1 72')
    rh_ = ga.sampleXYT('rh',lons,lats,tyme=times,Verbose=True)
    rh_ = rh_.T         # transpose
    rh_ = rh_[-1::-1,:] # flip vertical
    print "---> Interpolating <RH> with PyGrADS took ", now()-t_


def HOLD():
    
#    f = GFIO('sample.nc')
    filename = 'test_py.nc'
    vname = ['u','v']
    vtitle = ['zonal wind','meridional wind']
    vunits = ['m/s','m/s']

    nymd = 20080630
    nhms = 120000
    
    f = GFIO()
    f.create(filename, vname, nymd, nhms, res='c', 
                vtitle=vtitle, vunits=vunits)

    u = zeros((f.im,f.jm))
    v = ones((f.im,f.jm))

    f.write('u',nymd,nhms,u)
    f.write('v',nymd,nhms,v)
    f.close()
    
        
