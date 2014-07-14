import scipy.io.netcdf
import pickle

def make_input_file(filename, 
                    nstr, juldat, sza, solvar,
                    ireflect, semis,
                    tavel, pavel, tz, pz, wkl, wbrodl):
    nmol, nlayers = wkl.shape

    # some sanity checks.
    len(tavel) == len(pavel) == len(wbrodl) == nlayers
    len(tz) == len(pz) == nlayers + 1
    
    # now make the file
    nc = scipy.io.netcdf.netcdf_file(filename, 'w')

    # attributes required
    nc.sw_ireflect = ireflect
    nc.sw_nstr = nstr
    nc.sw_juldat = juldat
    nc.sw_sza = sza
    nc.sw_solvar = solvar
    
    # now fill up the variables.
    nc.createDimension("layer", nlayers)
    nc.createDimension("level", nlayers + 1)
    nc.createDimension("band", 16)
    nc.createDimension("mol", nmol)
    nc.createVariable('pavel', 'd', ('layer',))[:] = pavel
    nc.createVariable('tavel', 'd', ('layer',))[:] = tavel
    nc.createVariable('pz', 'd', ('level',))[:] = pz
    nc.createVariable('tz', 'd', ('level',))[:] = tz
    nc.createVariable('wbrodl', 'd', ('layer',))[:] = wbrodl
    nc.createVariable('wkl', 'd', ('layer', 'mol'))[:] = wkl.T
    nc.createVariable('sw_semis', 'd', ('band',))[:] = semis

    nc.close()


inputs = pickle.load(open("inputs.dat", 'r'))
del inputs['iscat'], inputs['tbound'], inputs['numangs']
inputs['nstr'] = 2
inputs['juldat'] = 0
inputs['sza'] = 0
inputs['solvar'] = 1
make_input_file('input_mls_sw.nc', **inputs)
