"""
A low-level interface to the RRTM code using the native python interface.
"""

import librrtm_wrapper as _rrtm_lw
import librrtm_sw_wrapper as _rrtm_sw
from common_interface import RRTMError


class RRTMNativeError(RRTMError):
    def __init__(self, err_message, lw=True):
        self.fname, self.line, self.msg = err_message.split(':')
        self.lw = lw

    def __str__(self):
        lw = 'lw' if self.lw else 'sw'
        s = "Error in %s %s:%s." % lw, self.fname, self.line
        if self.msg.strip() != '': s += self.msg.strip()
        return s


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
    try:
        totuflux, totdflux, fnet, htr = _rrtm_lw.run_rrtm(*args)
    except _rrtm_lw.LibRRTMError as e:
        raise RRTMNativeError(e.data, True)
    return {'totuflux': totuflux, 'totdflux':totdflux, 'fnet':fnet, 'htr':htr}

def run_sw_rrtm(args):
    """Runs the shortwave RRTM code. `args` must be a dictionary containing
    the keys:

    ['nstr', 'solvar', 'semis', 'ireflect', 'juldat', 'sza',
     'tavel', 'pavel', 'tz', 'pz', 'wkl', 'wbrodl']

    Returns a dictionary containing the fluxes and heating rate.
    """
    args['wkl'] = args['wkl'].T
    args = [args[k] for k in ['nstr', 'juldat', 'sza', 'solvar',
                              'ireflect', 'semis',
                              'tavel', 'pavel', 'tz', 'pz', 'wkl', 'wbrodl']]
    try:
        totuflux, totdflux, fnet, htr = _rrtm_sw.run_rrtm_sw(*args)
    except _rrtm_sw.LibRRTMSWError as e:
        raise RRTMNativeError(e.data, False)
    return {'totuflux': totuflux, 'totdflux':totdflux, 'fnet':fnet, 'htr':htr}
