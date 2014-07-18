# The low level interface
import low_level
use_native = low_level.use_native
has_native = low_level.has_native

# The high level pythonic interface
import numpy
import chem
import collections
import scipy.linalg
import scipy.optimize

# the order of gases in the WKL array.
_species_names = ['H2O', 'CO2', 'O3', 'N2O', 'CO', 'CH4', 'O2']

# The data output class
Output = collections.namedtuple('Output', ['totuflux', 'totdflux', 'fnet', 'htr'])

# Array property to help catch programming errors early and keep things safe.

class _ArrayProperty (object):
    
    def __init__(self, name, shape, generator=None):
        self.name = name
        self.shape_pattern = shape
        self.generator = generator

    def _shape(self, obj):
        return tuple(getattr(obj, s) if isinstance(s, str) else s
                     for s in self.shape_pattern)
    
    def __get__(self, obj, objtype=None):
        if obj is None: return self
        attr = '_' + self.name
        arr = getattr(obj, attr, None)
        if arr is None:
            # if we have an initialiser, use it:
            if self.generator is None:
                raise AttributeError("Attribute %s not set." % self.name)
            else:
                getattr(obj, self.generator)()
                return getattr(obj, attr)
        else:
            return arr

    def __set__(self, obj, arr):
        attr = '_' + self.name
        if isinstance(arr, numpy.ndarray):
            if arr.shape != self._shape(obj): 
                raise Exception("Shape mismatch in `%s`" % self.name)
            setattr(obj, attr, arr)
        else:
            val = numpy.zeros(self._shape(obj), dtype="double") + arr
            setattr(obj, attr, val)

    def __delete__(self, obj):
        attr = '_' + self.name
        if hasattr(obj, attr): delattr(obj, attr)


# Base object for interfacing with the radiation code.
class _BaseRad (object):

    # profile parameters
    tavel = _ArrayProperty("tavel", ('_nlayers',), 'auto_tavel')
    pavel = _ArrayProperty("pavel", ('_nlayers',), 'auto_pavel')
    tz = _ArrayProperty("tz", ('_nlevels',), 'auto_tz')
    pz = _ArrayProperty("pz", ('_nlevels',), 'auto_pz')
    
    def __init__(self, nlayers):
        self._wbrodl = None
        self._species_vmr = {}
        self._nlayers = nlayers
        self._nlevels = nlayers + 1
    
    def set_species(self, species, value, unit='vmr'):
        '''Sets a species concentration. Units can be "vmr" or "mmr".'''
        species = species.upper()
        if species not in _species_names: raise Exception("Unknown species")
        value = numpy.array(value)
        if value.shape != (self._nlayers,): raise Exception("Shape error")
        # convert value to units of volume mixing ratio.
        if unit == 'vmr':
            pass
        elif unit == 'mmr':
            value = chem.mmr_to_vmr(species, value)
        elif unit == 'molecules/cm2':
            value = chem.column_density_to_mmr(
                species, value, self.tavel, self.pz, self.pavel)
            value = chem.mmr_to_vmr(species, value)
        else:
            raise Exception("Unknown unit.")
        # save the data
        self._species_vmr[species] = value

    def get_species(self, species, unit='vmr'):
        species = species.upper()
        if species not in _species_names: raise Exception("Unknown species")
        if species in self._species_vmr:
            value = self._species_vmr[species]
        else:
            value = numpy.zeros((self._nlayers,), dtype="double")
        # now process the unit
        if unit == 'vmr': return value
        elif unit == 'mmr': return chem.vmr_to_mmr(species, value)
        elif unit == 'molecules/cm2': 
            value = chem.vmr_to_mmr(species, value)
            return chem.mmr_to_column_density(
                species, value, self.tavel, self.pz, self.pavel)
        else: raise Exception("Unknown unit.")

    def auto_tavel(self):
        """Returns a mass-weighted layer average temperature, assuming that
        temperature is linearly interpolated in log-pressure coordinates.
        """
        T = self.tz
        p = self.pz
        log = numpy.log(p[1:]/p[:-1])
        self.tavel = T[:-1] \
                     - (T[1:] - T[:-1]) / ((p[1:] - p[:-1]) * -log) \
                     * ( p[1:] * log - p[1:] + p[:-1] )
    
    def auto_pavel(self):
        """Returns the mass-weighted layer average pressure."""
        self.pavel = (self.pz[1:] + self.pz[:-1]) / 2.

    def auto_pz(self):
        '''A heuristic method for estimating pz based on pavel. This is not a
        well posed problem and so the solution method slightly ad-hoc. As such, 
        always check the resulting pressure structure to ensure it is reasonable.
        '''

        # computes pz given surface pressure and pavel. very sensitive to ICs!
        def compute_pz(p0, pavel):
            ab = numpy.zeros((2, len(pavel) + 1))
            ab[:] = 0.5
            ab[0,0] = 1
            b = numpy.zeros(len(pavel) + 1)
            b[1:] = pavel
            b[0] = p0
            return scipy.linalg.solve_banded((1,0), ab, b)

        # constructs pz and tests how monotonic it is. the score is
        # something that should have a root at exactly zero. secondarily
        # it optimises for a smooth even grid spacing at the top of the
        # model.
        def trial(p0, pavel):
            pz = compute_pz(p0, pavel)
            score = (pz[1:] > pz[:-1]).sum() * numpy.sign(pz[-1] - pz[-2])
            error = (pz[-4] - 2 * pz[-3] + pz[-2]) / pavel[-2] / 1000.
            if len(pavel) % 2 == 1: error *= -1
            return score + numpy.clip(error, -0.1, 0.1)

        # find the root in trial
        p0 = scipy.optimize.bisect(trial, 1, 2000, args=(self.pavel,))
        self.pz = compute_pz(p0, self.pavel)


    def auto_tz_alternative(self):
        '''This method performs worse than the find_pz method. Again caution
        should be taken to check suitability. However, RRTM is fairly
        insensitive to tz so the results of this should be reasonable
        for most purposes. The `auto_tz` method should be used for
        most applications.

        '''
        pz = self.pz
        p_0 = pz[:-1]
        p_1 = pz[1:]
        log = numpy.log(p_1 / p_0)
        alpha = (p_1 * log - p_1 + p_0) / ((p_1 - p_0) * (log))
        print alpha

        # computes pz given surface pressure and pavel. very unstable!
        def compute_tz(t0, tavel):
            ab = numpy.zeros((2, len(tavel) + 1))
            ab[0,1:] = alpha
            ab[1,:-1] = 1 - alpha
            ab[0,0] = 1
            b = numpy.zeros(len(tavel) + 1)
            b[1:] = tavel
            b[0] = t0
            return scipy.linalg.solve_banded((1,0), ab, b)

        def trial(t0, tavel):
            tz = compute_tz(t0, tavel)
            #error = tz[-4] - 2 * tz[-3] + tz[-2]
            error = (tz[2:] - 2 * tz[1:-1] + tz[:-2]).var()
            return error

        # find the root in trial
        t0 = scipy.optimize.minimize(trial, 300, args=(self.tavel,)).x[0]
        self.tz = compute_tz(t0, self.tavel)

    def auto_tz(self):
        '''This is a cheaper method that finds a reasonable profile for tz but
        one that is not consistent with the layer average. For
        sufficient resolution data, this out-performs the consistent
        method.

        '''
        tavel = self.tavel
        tz = numpy.zeros(len(tavel)+1)
        tz[1:-1] = (tavel[1:] + tavel[:-1]) * 0.5
        tz[0] = 2 * tavel[0] - tavel[1]
        tz[-1] = 2 * tavel[-1] - tavel[-2]
        self.tz = tz
    
    def generate_wkl_wbrodl(self):
        # first create and fill wkl
        wkl = numpy.zeros((len(_species_names), self._nlayers), dtype="double")
        for i, s in enumerate(_species_names):
            if s in self._species_vmr:
                wkl[i] = self._species_vmr[s]
        # next generate wbrodl
        coldry = chem.column_density_rrtm(self.tavel, self.pz, self.pavel)
        if (wkl < 1.).all():
            wbrodl = coldry * (1 - wkl[1:].sum(0))
        elif (wkl > 1.).all():
            wbrodl = coldry - wkl[1:].sum(0)
        else:
            raise Exception("WKL units issue detected.")
        return wkl, wbrodl


class LW (_BaseRad):

    def __init__(self, nlayers):
        super(LW, self).__init__(nlayers)
        # set the default flags and parameters. all are sensible.
        self.iscat = 0
        self.numangs = 2
        self.semis = 1
        self.ireflect = 0
        self.tbound = -1

    def run(self):
        wkl, wbrodl = self.generate_wkl_wbrodl()
        out = low_level.run_lw_rrtm(
            dict(iscat=self.iscat, numangs=self.numangs, semis=self.semis,
                 ireflect=self.ireflect, tbound=self.tbound,
                 tavel=self.tavel, pavel=self.pavel, tz=self.tz, pz=self.pz,
                 wkl=wkl, wbrodl=wbrodl))
        return Output(totuflux=out['totuflux'], totdflux=out['totdflux'],
                      fnet=out['fnet'], htr=out['htr'])


class SW (_BaseRad):

    def __init__(self, nlayers):
        super(SW, self).__init__(nlayers)
        # set the default flags and parameters. all are sensible.
        self.semis = 1
        self.ireflect = 0
        self.nstr = 0
        self.solvar = 1
        self.juldat = 0
        self.sza = 0
    
    def run(self):
        wkl, wbrodl = self.generate_wkl_wbrodl()
        out = low_level.run_sw_rrtm(
            dict(solvar=self.solvar, juldat=self.juldat, sza=self.sza,
                 nstr=self.nstr, semis=self.semis, ireflect=self.ireflect,
                 tavel=self.tavel, pavel=self.pavel, tz=self.tz, pz=self.pz,
                 wkl=wkl, wbrodl=wbrodl))
        return Output(totuflux=out['totuflux'], totdflux=out['totdflux'],
                      fnet=out['fnet'], htr=out['htr'])

