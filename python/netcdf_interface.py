"""
A low-level interface to the RRTM code using the NetCDF interface.
"""

import os
import tempfile
import shutil
import subprocess
import scipy.io.netcdf
from common_interface import RRTMError


_module_dir = os.path.dirname(os.path.abspath(__file__))
_rrtm_lw = os.path.join(_module_dir, 'rrtm_nc')
_rrtm_sw = os.path.join(_module_dir, 'rrtm_sw_nc')


class RRTMNetCDFError(RRTMError):
    def __init__(self, binary, output):
        self.binary = binary
        self.output = output
    
    def __str__(self):
        return "Binary `%s` failed to produce output file. Output:\n\n%s" % (self.binary, self.output)


def run_lw_rrtm(args):
    """Runs the longwave RRTM code. `args` must be a dictionary containing
    the keys:

    ['iscat', 'numangs', 'semis', 'ireflect', 'tbound',
     'tavel', 'pavel', 'tz', 'pz', 'wkl', 'wbrodl']

    Returns a dictionary containing the fluxes and heating rate.
    """
    # a temporary directory to work in with input and output files.
    working_dir = tempfile.mkdtemp(suffix='.rrtm')
    try:
        ifile = os.path.join(working_dir, 'input.nc')
        ofile = os.path.join(working_dir, 'output.nc')
        _make_lw_input(ifile, args)
        try:
            output = subprocess.check_output(
                [_rrtm_lw, '-i', ifile, '-o', ofile])
        except subprocess.CalledProcessError as e:
            raise RRTMNetCDFError(_rrtm_lw, e.output)
        if not os.path.exists(ofile):
            raise RRTMNetCDFError(_rrtm_lw, output)
        # process the output file `ofile` and extract the returned data.
        nc = scipy.io.netcdf.netcdf_file(ofile, 'r')
        totuflux = nc.variables['totuflux'][:].copy()
        totdflux = nc.variables['totdflux'][:].copy()
        fnet = nc.variables['fnet'][:].copy()
        htr = nc.variables['htr'][:].copy()
        nc.close()
    finally:
        shutil.rmtree(working_dir)
    return {'totuflux': totuflux, 'totdflux':totdflux, 'fnet':fnet, 'htr':htr}


def run_sw_rrtm(args):
    """Runs the shortwave RRTM code. `args` must be a dictionary containing
    the keys:

    ['nstr', 'solvar', 'semis', 'ireflect', 'juldat', 'sza',
     'tavel', 'pavel', 'tz', 'pz', 'wkl', 'wbrodl']

    Returns a dictionary containing the fluxes and heating rate.
    """
    # a temporary directory to work in with input and output files.
    working_dir = tempfile.mkdtemp(suffix='.rrtm')
    try:
        ifile = os.path.join(working_dir, 'input.nc')
        ofile = os.path.join(working_dir, 'output.nc')
        _make_sw_input(ifile, args)
        try:
            output = subprocess.check_output(
                [_rrtm_sw, '-i', ifile, '-o', ofile])
        except subprocess.CalledProcessError as e:
            raise RRTMNetCDFError(_rrtm_sw, e.output)
        if not os.path.exists(ofile):
            raise RRTMNetCDFError(_rrtm_sw, output)
        # process the output file `ofile` and extract the returned data.
        nc = scipy.io.netcdf.netcdf_file(ofile, 'r')
        totuflux = nc.variables['totuflux'][:].copy()
        totdflux = nc.variables['totdflux'][:].copy()
        fnet = nc.variables['fnet'][:].copy()
        htr = nc.variables['htr'][:].copy()
        nc.close()
    finally:
        shutil.rmtree(working_dir)
    return {'totuflux': totuflux, 'totdflux':totdflux, 'fnet':fnet, 'htr':htr}


def _make_lw_input(filename, args):
    nmol, nlayers = args['wkl'].shape

    # some sanity checks.
    len(args['tavel']) == len(args['pavel']) == len(args['wbrodl']) == nlayers
    len(args['tz']) == len(args['pz']) == nlayers + 1
    
    # now make the file
    nc = scipy.io.netcdf.netcdf_file(filename, 'w')

    # attributes required
    nc.lw_iscat = args['iscat']
    nc.lw_numangs = args['numangs']
    nc.lw_tbound = args['tbound']
    nc.lw_ireflect = args['ireflect']
    nc.lw_semis = args['semis']
    
    # now fill up the variables.
    nc.createDimension("layer", nlayers)
    nc.createDimension("level", nlayers + 1)
    nc.createDimension("mol", nmol)
    nc.createVariable('pavel', 'd', ('layer',))[:] = args['pavel']
    nc.createVariable('tavel', 'd', ('layer',))[:] = args['tavel']
    nc.createVariable('pz', 'd', ('level',))[:] = args['pz']
    nc.createVariable('tz', 'd', ('level',))[:] = args['tz']
    nc.createVariable('wbrodl', 'd', ('layer',))[:] = args['wbrodl']
    nc.createVariable('wkl', 'd', ('layer', 'mol'))[:] = args['wkl'].T
    nc.close()


def _make_sw_input(filename, args):
    nmol, nlayers = args['wkl'].shape

    # some sanity checks.
    len(args['tavel']) == len(args['pavel']) == len(args['wbrodl']) == nlayers
    len(args['tz']) == len(args['pz']) == nlayers + 1
    
    # now make the file
    nc = scipy.io.netcdf.netcdf_file(filename, 'w')

    # attributes required
    nc.sw_ireflect = args['ireflect']
    nc.sw_nstr = args['nstr']
    nc.sw_juldat = args['juldat']
    nc.sw_sza = args['sza']
    nc.sw_solvar = args['solvar']
    nc.sw_semis = args['semis']
    
    # now fill up the variables.
    nc.createDimension("layer", nlayers)
    nc.createDimension("level", nlayers + 1)
    nc.createDimension("mol", nmol)
    nc.createVariable('pavel', 'd', ('layer',))[:] = args['pavel']
    nc.createVariable('tavel', 'd', ('layer',))[:] = args['tavel']
    nc.createVariable('pz', 'd', ('level',))[:] = args['pz']
    nc.createVariable('tz', 'd', ('level',))[:] = args['tz']
    nc.createVariable('wbrodl', 'd', ('layer',))[:] = args['wbrodl']
    nc.createVariable('wkl', 'd', ('layer', 'mol'))[:] = args['wkl'].T
    nc.close()
