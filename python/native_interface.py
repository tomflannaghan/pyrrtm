"""
A low-level interface to the RRTM code using the native python interface.
"""

import librrtm_wrapper as _rrtm_lw
import librrtm_sw_wrapper as _rrtm_sw

def run_lw_rrtm(args):
    """Runs the longwave RRTM code. `args` must be a dictionary containing
    the keys:

    ['iscat', 'numangs', 'semis', 'ireflect', 'tbound',
     'tavel', 'pavel', 'tz', 'pz', 'wkl', 'wbrodl']

    Returns a dictionary containing the fluxes and heating rate.
    """
    args['wkl'] = args['wkl'].T
    args = [args[k] for k in ['iscat', 'numangs', 
                              'tbound', 'ireflect', 'semis',
                              'tavel', 'pavel', 'tz', 'pz', 'wkl', 'wbrodl']]
    totuflux, totdflux, fnet, htr = _rrtm_lw.run_rrtm(*args)
    return {'totuflux': totuflux, 'totdflux':totdflux, 'fnet':fnet, 'htr':htr}

def run_sw_rrtm(args):
    """Runs the shortwave RRTM code. `args` must be a dictionary containing
    the keys:

    ['nstr', 'solvar', 'semis', 'ireflect', 'juldat', 'sza',
     'tavel', 'pavel', 'tz', 'pz', 'wkl', 'wbrodl']

    Returns a dictionary containing the fluxes and heating rate.
    """
    _rrtm_sw.initrrtm(args['nstr'], 0, 0, args['juldat'], 
                      args['sza'], args['solvar'])
    _rrtm_sw.initsurface(args['ireflect'], args['semis'])
    _rrtm_sw.initprofile(args['tavel'], args['pavel'], args['tz'], args['pz'], 
                         args['wkl'].T, args['wbrodl'])
    _rrtm_sw.execrun()
    totuflux, totdflux, fnet, htr = _rrtm_sw.getoutput(len(args['tavel']))
    return {'totuflux': totuflux, 'totdflux':totdflux, 'fnet':fnet, 'htr':htr}
