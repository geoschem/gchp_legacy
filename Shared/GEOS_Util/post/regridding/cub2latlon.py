import sys
import getopt
from netCDF4 import Dataset
import ESMF
import numpy as np
from mpi4py import MPI

# This call enables debug logging
# esmpy = ESMF.Manager(debug=True)

#print ("Hello ESMPy World from PET (processor) {0}!".format(ESMF.local_pet()))

def main(argv):
    params = process_command_line(argv)
    comm = MPI.COMM_WORLD

    # NetCDF files
    cs = Dataset(params['ifile'], 'r', format='NETCDF4')

    if ESMF.local_pet() == 0:
        ll = Dataset(params['ofile'], 'w', format='NETCDF4')
        transfer_metadata(cs, ll, params)
        ll.close()

    comm.Barrier()
    ll = Dataset(params['ofile'], 'r', format='NETCDF4')
    categorize_variables(cs, params)


    # Create ESMF grids
    nx = len(cs.dimensions['Xdim'])
    cs_grid = ESMF.Grid(tilesize=nx, name="cubed_sphere")
    mask = cs_grid.add_item(ESMF.GridItem.MASK)
    mask[:] = 1

    ll_grid = make_latlon_grid(ll)

    # create an object to regrid data from the source to the destination field
    field_cs = ESMF.Field(cs_grid, name='in')
    field_ll = ESMF.Field(ll_grid, name='out')

    # Subdomain bounds
    i0 = ll_grid.lower_bounds[0][0]
    i1 = ll_grid.upper_bounds[0][0]
    nlon = len(ll.dimensions['lon'])
    nlat = len(ll.dimensions['lat'])

    default_regrid = ESMF.Regrid(field_cs, field_ll, src_mask_values=np.array([0]),
                                 regrid_method=ESMF.RegridMethod.BILINEAR,
                                 line_type=ESMF.LineType.GREAT_CIRCLE,
                                 unmapped_action=ESMF.UnmappedAction.IGNORE)

    regridders = {}

    def regrid(f_in, f_out, default, missing):
        comm = MPI.COMM_WORLD

        any_missing = comm.allgather(missing in f_in.data)

        if any(any_missing):
            local_key = ''.join(list(rle((f_in.data==missing).flatten())))
            
            key_list = comm.allgather(local_key)
            key = ''.join(key_list)
            if key in regridders.keys():
                custom_regrid = regridders[key]
            else:
                mask[np.where(f_in.data != missing)] = 1
                mask[np.where(f_in.data == missing)] = 0
                custom_regrid = ESMF.Regrid(f_in, f_out, src_mask_values=np.array([0]),
                                            regrid_method=ESMF.RegridMethod.BILINEAR,
                                            line_type=ESMF.LineType.GREAT_CIRCLE,
                                            unmapped_action=ESMF.UnmappedAction.IGNORE)
                regridders[key] = custom_regrid

            f_out.data[...] = missing
            custom_regrid(f_in, f_out, zero_region=ESMF.Region.SELECT)
        else:
            default(f_in, f_out)


    def uv_to_xyz(u, v, trig, missing):
        clat = trig['clat']
        slat = trig['slat']
        clon = trig['clon']
        slon = trig['slon']
        u_xyz = {}
        
        u_xyz['x'] = -u*slon - v*slat*clon
        u_xyz['y'] = +u*clon - v*slat*slon
        u_xyz['z'] =  v*clat

        u_xyz['x'][abs(u) > missing/2] = missing
        u_xyz['y'][abs(u) > missing/2] = missing
        u_xyz['z'][abs(u) > missing/2] = missing

        return u_xyz

    def xyz_to_uv(u_xyz, trig, missing):
        clat = trig['clat']
        slat = trig['slat']
        clon = trig['clon']
        slon = trig['slon']
        u = -u_xyz['x']*slon + u_xyz['y']*clon
        v = -u_xyz['x']*slat*clon - u_xyz['y']*slat*slon + u_xyz['z']*clat

        u[u_xyz['x']==missing] = missing
        v[u_xyz['x']==missing] = missing

        return (u,v)

    cs_trig = {}
    cs_trig['clat'] = np.cos(np.deg2rad(cs['lats'][ESMF.local_pet(),:,:]))
    cs_trig['slat'] = np.sin(np.deg2rad(cs['lats'][ESMF.local_pet(),:,:]))
    cs_trig['clon'] = np.cos(np.deg2rad(cs['lons'][ESMF.local_pet(),:,:]))
    cs_trig['slon'] = np.sin(np.deg2rad(cs['lons'][ESMF.local_pet(),:,:]))

    ll_trig = {}
    ll_trig['clat'] = np.zeros((nlat,i1-i0))
    ll_trig['slat'] = np.zeros((nlat,i1-i0))
    ll_trig['clon'] = np.zeros((nlat,i1-i0))
    ll_trig['slon'] = np.zeros((nlat,i1-i0))
    for i in range(i0,i1):
        ll_trig['clat'][:,i-i0] = np.cos(np.deg2rad(ll['lat'][:]))
        ll_trig['slat'][:,i-i0] = np.sin(np.deg2rad(ll['lat'][:]))
        ll_trig['clon'][:,i-i0] = np.cos(np.deg2rad(ll['lon'][i]))
        ll_trig['slon'][:,i-i0] = np.sin(np.deg2rad(ll['lon'][i]))


    ll.close()
    for var in params['scalar_vars']:
        if params['debug'] and ESMF.local_pet()==0:
            print "Processing var: ",var
        if 'lev' in cs.variables[var].dimensions:
            nlev = len(cs.dimensions['lev'])
            has_lev = True
        else:
            nlev = 1
            has_lev = False

        for t in range(len(cs.dimensions['time'])):
            for level in range(nlev):

                if has_lev:
                    q = cs.variables[var][t,level,ESMF.local_pet(),:,:]
                else:
                    q = cs.variables[var][t,ESMF.local_pet(),:,:]

                missing = cs[var].missing_value

                field_cs.data[:,:] = q.transpose()

                regrid(field_cs, field_ll, default_regrid, missing)

                comm = MPI.COMM_WORLD
                q_global = comm.gather(field_ll.data)
                if comm.rank == 0:
                    ll = Dataset(params['ofile'],'a', format='NETCDF4')
                    qg = np.vstack(tuple(q_global)).transpose()
                    if has_lev:
                        ll.variables[var][t,level,:,:] = qg
                    else:
                        ll.variables[var][t,:,:] = qg

    for var in params['vector_vars']:
        if params['debug'] and ESMF.local_pet()==0:
            print "Processing var: ",var
        if 'lev' in cs.variables[var['east']].dimensions:
            nlev = len(cs.dimensions['lev'])
            has_lev = True
        else:
            nlev = 1
            has_lev = False

        for t in range(len(cs.dimensions['time'])):
            for level in range(nlev):

                if has_lev:
                    u = cs.variables[var['east']][t,level,ESMF.local_pet(),:,:]
                    v = cs.variables[var['north']][t,level,ESMF.local_pet(),:,:]
                else:
                    u = cs.variables[var['east']][t,ESMF.local_pet(),:,:]
                    v = cs.variables[var['north']][t,ESMF.local_pet(),:,:]
                    
                missing = cs.variables[var['east']].missing_value
                u_xyz_in  = uv_to_xyz(u, v, cs_trig, missing)
                u_xyz_out = {}
                
                for i in ['x','y','z']:
                    tmp = u_xyz_in[i].transpose().copy()
                    field_cs.data[...] = tmp
                    regrid(field_cs, field_ll, default_regrid, missing)
                    u_xyz_out[i] = field_ll.data.transpose().copy()
                
                u,v = xyz_to_uv(u_xyz_out, ll_trig, missing)

                comm = MPI.COMM_WORLD
                u_global = comm.gather(u)
                v_global = comm.gather(v)
                if comm.rank == 0:
                    ll = Dataset(params['ofile'],'a', format='NETCDF4')
                    ug = np.hstack(tuple(u_global))
                    vg = np.hstack(tuple(v_global))
                    if has_lev:
                        ll.variables[var['east']][t,level,:,:] = ug
                        ll.variables[var['north']][t,level,:,:] = vg
                    else:
                        ll.variables[var['east']][t,:,:] = ug
                        ll.variables[var['north']][t,:,:] = vg


    print 'number of regridders= ', len(regridders)
    cs.close()


def process_command_line(argv):
    help_string = 'mpirun -np 6 cub2latlon.py -i <inputfile> -o <outputfile> --nlats <lats> --nlons <lons> --vars <vars> [--debug]'
    try:
        opts, args = getopt.getopt(argv, "hi:o:d", ["ifile=","ofile=","nlons=","nlats=","vars=","debug"])
    except getopt.GetoptError:
        print help_string
        ESMF.finalize(ESMF.ESMF_END_ABORT)
        sys.exit(2)
        
    params = {}
    params['debug'] = False
    for opt, arg in opts:
        if opt == '-h':
            print help_string
            ESMF.finalize()
        elif opt in ("-i", "--ifile"):
            params['ifile'] = arg
        elif opt in ("-o", "--ofile"):
            params['ofile'] = arg
        elif opt in ("--nlats"):
            params['nlats'] = int(arg)
        elif opt in ("--nlons"):
            params['nlons'] = int(arg)
        elif opt in ("--vars"):
            params['vars'] = arg.split(",")
        elif opt in ("-d" "--debug"):
            params['debug'] = True
    return params


def make_latlon_grid(ll):
    dimensions = np.array([len(ll.dimensions['lon']),len(ll.dimensions['lat'])])
    grid = ESMF.Grid(dimensions, num_peri_dims=1, periodic_dim=0, pole_dim=1)

    # Set the lat,lon coordinates
    # Dim 0 is distributed
    grid.add_coords()
    lats = grid.get_coords(1)
    lons = grid.get_coords(0)

    i0 = grid.lower_bounds[0][0]
    i1 = grid.upper_bounds[0][0]

    # There is probably a more elegant way to do this in python
    for i in range(i0,i1):
        lons[i-i0,:] = ll['lon'][i]
        lats[i-i0,:] = ll['lat'][:]

    return grid



def categorize_variables(cs, params):
        params['vector_vars'] = []
        params['scalar_vars'] = []

        skip_vars = ['nf', 'ncontact', 'cubed_sphere', 'Xdim', 'Ydim', 'lons', 'lats', 
                     'contacts', 'orientation', 'anchor', 'lev', 'time', 'lon', 'lat']
        
        for name, variable in cs.variables.iteritems():
            # Eliminate grid-related variables that do not make sense for lat lon
            if name in skip_vars:
                continue
            # And eliminate variables that were not requested by the user
            elif ('vars' in params) and (name not in params['vars']):
                continue
            # Vector components handled in a special way ...
            else:
                # This block is a bit long/complex and should be refactored.
                long_name = variable.long_name.lower()
                if 'east' in long_name or 'north' in long_name:
                    if 'east' in long_name:
                        long_name_north = long_name.replace('east','north')
                        for n, var in cs.variables.iteritems():
                            if n in skip_vars:
                                continue
                            if var.long_name.lower() == long_name_north:
                                north_name = n
                                break
                        params['vector_vars'].append({'east':name, 'north':north_name})
                else:
                    params['scalar_vars'].append(name)

# from
# http://stackoverflow.com/questions/33250795/run-length-encoding-function-no-libraries-or-object-methods

def TF(x):
    if x:
        return 'T'
    else:
        return 'F'

def rle(iterable):
    it = iter(iterable)
    e, c = next(it), 1       # StopIteration silently handled before Py3.7
    for n in it:
        if n == e:
            c += 1
            continue
        yield str(c)+TF(e)         # "yield from (e, c)" if you need a flat list
        e, c = n, 1
    yield str(c)+TF(e)

def transfer_metadata(cs, ll, params):

    def add_grid_dimensions(ll, params):
        lat_dim = ll.createDimension('lat', params['nlats'])
        lon_dim = ll.createDimension('lon', params['nlons'])
        if 'lev' in cs.dimensions:
            lev_dim = ll.createDimension('lev', len(cs.dimensions['lev']))
        lev_dim = ll.createDimension('time', len(cs.dimensions['time']))


    def add_grid_variables(cs, ll, params):
        lon = ll.createVariable('lon','f8',('lon',))
        lon.long_name = 'longitude'
        lon.units = 'degrees_east'
        lon[:] = np.linspace(0.,360.,num=params['nlons'],endpoint=False)

        lat = ll.createVariable('lat','f8',('lat',))
        lat.long_name = 'latitude'
        lat.units = 'degrees_north'
        lat[:] = np.linspace(-90.,90.,num=params['nlats'])

        if 'lev' in cs.dimensions:
            lev = ll.createVariable('lev', 'f8', ('lev',))
            lev[:] = cs['lev'][:]

        time = ll.createVariable('time', 'i4', ('time',))
        time[:] = cs['time'][:]
        for name in cs['time'].ncattrs():
            time.setncattr(name, cs['time'].getncattr(name))

            
    def add_global_attributes(cs, ll):
        for name in cs.ncattrs():
            ll.setncattr(name, cs.getncattr(name))


    # Each variable goes from (ix,jy,f,*) ->  (ilon,jlat,*)
    def convert_dims(cs_var_dims):
        ll_var_dims = []
        for name in cs_var_dims:
            if (name == 'Ydim'):
                ll_var_dims.append('lat')
            elif (name == 'Xdim'):
                ll_var_dims.append('lon')
            elif (name in ['nf','ncontact','orientationStrLen']):
                continue
            else:
                ll_var_dims.append(name)
        return tuple(ll_var_dims)

                
    def add_variables(cs, ll, params):
        skip_vars = ['nf', 'ncontact', 'cubed_sphere', 'Xdim', 'Ydim', 'lons', 'lats', 
                     'contacts', 'orientation', 'anchor', 'lev', 'time', 'lon', 'lat']

        for name, variable in cs.variables.iteritems():
            # Eliminate grid-related variables that do not make sense for lat lon
            if name in skip_vars:
                continue
            # And eliminate variables that were not requested by the user
            elif ('vars' in params) and (name not in params['vars']):
                continue
            else:
                dims = convert_dims(variable.dimensions)
                chunksizes = []
                for i in range(len(dims)-2):
                    chunksizes.append(1)
                chunksizes.append(params['nlats'])
                chunksizes.append(params['nlons'])
                x = ll.createVariable(name, variable.datatype, dims, chunksizes=chunksizes)
                for name in variable.ncattrs():
                    if name in ['grid_mapping']:
                        continue
                    x.setncattr(name, variable.getncattr(name))





    add_grid_dimensions(ll, params)
    add_grid_variables(cs, ll, params)
    add_global_attributes(cs, ll)
    add_variables(cs, ll, params)

if __name__ == "__main__":
   main(sys.argv[1:])

