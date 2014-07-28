from python_wrapper import LW, SW, Output
from common_interface import has_native, use_native, RRTMError
import os

# read the version string from the version file.
with open(os.path.join(os.path.dirname(__file__), 'version'), 'r') as f:
    __version__ = f.read().strip()
