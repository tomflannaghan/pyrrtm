#
# This code allows the toggling of native and netcdf interfaces while
# exposing a common lib to the rest of the package.
#

# The exception class.
class RRTMError (Exception):
    pass

from has_native import has_native # a ghost module made by the make file.
if has_native:
    import native_interface

# always import the netcdf interface as the default. This is because
# it has superior error handling. The native library is much much
# faster though (especially for longwave calculations).
import netcdf_interface
from netcdf_interface import run_lw_rrtm, run_sw_rrtm

def use_native(state=True):
    '''Use the native interface with RRTM (much less overhead -- significant
    when doing lots of cheap long wave calculations).
    
    Warning: Errors are not handled in the native interface, and will
    seg-fault or terminate the python interpreter.
    
    '''
    global run_lw_rrtm, run_sw_rrtm
    if state:
        assert has_native
        run_lw_rrtm = native_interface.run_lw_rrtm
        run_sw_rrtm = native_interface.run_sw_rrtm
    else:
        run_lw_rrtm = netcdf_interface.run_lw_rrtm
        run_sw_rrtm = netcdf_interface.run_sw_rrtm
