import functools
import numpy
import unittest
import scipy.io.netcdf

import pyrrtm


class BaseTest(unittest.TestCase):

    def setUp(self):
        # read in the data needed for the test.
        nc = scipy.io.netcdf.netcdf_file('tests_data.nc', 'r')
        self.pavel = nc.variables['pavel'][:].copy()
        self.tavel = nc.variables['tavel'][:].copy()
        self.pz = nc.variables['pz'][:].copy()
        self.tz = nc.variables['tz'][:].copy()
        self.h2o = nc.variables['h2o'][:].copy()
        self.co2 = nc.variables['co2'][:].copy()
        self.o3 = nc.variables['o3'][:].copy()
        self.n2o = nc.variables['n2o'][:].copy()
        self.co = nc.variables['co'][:].copy()
        self.ch4 = nc.variables['ch4'][:].copy()
        self.o2 = nc.variables['o2'][:].copy()
        nc.close()

    def setup_profiles(self, model, auto_pz=False, auto_pavel=False):
        if not auto_pavel:
            model.tavel = self.tavel
            model.pavel = self.pavel
        if not auto_pz:
            model.pz = self.pz
            model.tz = self.tz
        model.set_species('h2o', self.h2o, 'molecules/cm2')
        model.set_species('co2', self.co2, 'molecules/cm2')
        model.set_species('o3', self.o3, 'molecules/cm2')
        model.set_species('n2o', self.n2o, 'molecules/cm2')
        model.set_species('co', self.co, 'molecules/cm2')
        model.set_species('ch4', self.ch4, 'molecules/cm2')
        model.set_species('o2', self.o2, 'molecules/cm2')


def rrtm_test_case(cls):
    ''' A decorator that implements the native and netcdf decorator
        by automatically adding a native and netcdf variant of each test.
    '''

    def add_test(func, native):
        @functools.wraps(func)
        def wrapped(self):
            pyrrtm.use_native(native)
            try:
                func(self)
            finally:
                pyrrtm.use_native(False)

        if native:
            decorator = unittest.skipIf(
                not pyrrtm.has_native,
                'pyrrtm not compiled with native library.')
            wrapped = decorator(wrapped)
            wrapped.__name__ = func.__name__ + '_native'
        else:
            wrapped.__name__ = func.__name__ + '_netcdf'

        setattr(cls, wrapped.__name__, wrapped)

    for name, func in list(cls.__dict__.items()):
        if hasattr(func, 'native'):
            add_test(func, True)
        if hasattr(func, 'netcdf'):
            add_test(func, False)
        if hasattr(func, 'native') or hasattr(func, 'netcdf'):
            delattr(cls, name)

    return cls


def test_native_and_netcdf(test_func):
    test_func.native = True
    test_func.netcdf = True
    return test_func


def test_netcdf(test_func):
    test_func.netcdf = True
    return test_func


@rrtm_test_case
class TestLW(BaseTest):

    def setup_lw_run(self, auto_pz=False, auto_pavel=False):
        lw = pyrrtm.LW(len(self.tavel))
        self.setup_profiles(lw, auto_pz=auto_pz, auto_pavel=auto_pavel)
        lw.iscat = 0
        lw.numangs = 2
        lw.tbound = 294.2
        lw.ireflect = 0
        lw.semis = 1
        return lw

    def check_result(self, key, output):
        nc = scipy.io.netcdf.netcdf_file('tests_data.nc', 'r')
        correct_htr = nc.variables['htr_' + key][:].copy()
        nc.close()
        return numpy.allclose(output.htr, correct_htr)

    @test_native_and_netcdf
    def test_basic(self):
        lw = self.setup_lw_run()
        output = lw.run()
        assert self.check_result('lw', output)

    @test_native_and_netcdf
    def test_disort(self):
        lw = self.setup_lw_run()
        lw.iscat = 1
        output = lw.run()
        assert self.check_result('lw_disort', output)

    @test_native_and_netcdf
    def test_auto_pz(self):
        lw = self.setup_lw_run(auto_pz=True)
        output = lw.run()
        assert self.check_result('lw_auto_pz', output)


@rrtm_test_case
class TestSW(BaseTest):

    def setup_sw_run(self, auto_pz=False, auto_pavel=False):
        sw = pyrrtm.SW(len(self.tavel))
        self.setup_profiles(sw, auto_pz=auto_pz, auto_pavel=auto_pavel)
        sw.ireflect = 0
        sw.semis = 1
        sw.sza = 0
        sw.juldat = 0
        sw.nstr = 2
        sw.solvar = 1
        return sw

    def check_result(self, key, output):
        nc = scipy.io.netcdf.netcdf_file('tests_data.nc', 'r')
        correct_htr = nc.variables['htr_' + key][:].copy()
        nc.close()
        return numpy.allclose(output.htr, correct_htr)

    @test_native_and_netcdf
    def test_basic(self):
        sw = self.setup_sw_run()
        output = sw.run()
        assert self.check_result('sw', output)

    # Although the test works in native mode, it causes nasty
    # errors to stdout so skip for now.
    @test_netcdf
    def test_failure(self):
        sw = self.setup_sw_run()
        # This setting causes RRTM to fail. We check it is handled.
        tavel = sw.tavel
        sw.tavel = numpy.ones(tavel.shape)
        with self.assertRaises(pyrrtm.RRTMError):
            sw.run()
        # check rrtm still functions after the error is raised.
        sw.tavel = tavel
        output = sw.run()
        assert self.check_result('sw', output)


if __name__ == '__main__':
    unittest.main()
