#
# This code allows the toggling of native and netcdf interfaces while
# exposing a common lib to the rest of the package.
#
import functools

# The exception class.
class RRTMError (Exception):
    pass

from has_native import has_native # a ghost module made by the make file.
import netcdf_interface
if has_native:
    import native_interface

_using_native = False # default is not to use it.

def _make_toggle(funcname):
    netcdf = getattr(netcdf_interface, funcname)
    if has_native: 
        native = getattr(native_interface, funcname)
        @functools.wraps(netcdf)
        def wrapper(*args, **kwargs):
            if _using_native:
                return native(*args, **kwargs)
            else:
                return netcdf(*args, **kwargs)
        return wrapper
    else:
        return netcdf

run_lw_rrtm = _make_toggle('run_lw_rrtm')
run_sw_rrtm = _make_toggle('run_sw_rrtm')


def use_native(state=True):
    '''Use the native interface with RRTM (much less overhead -- significant
    when doing lots of cheap long wave calculations).
    
    Warning: Errors are not handled in the native interface, and will
    seg-fault or terminate the python interpreter.
    
    '''
    global _using_native
    if state: assert has_native
    _using_native = state
