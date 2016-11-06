import pyrrtm
import unittest
import numpy
import scipy.io.netcdf

class BaseTest(object):

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


class TestLW(BaseTest, unittest.TestCase):

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

    def test_netcdf(self):
        pyrrtm.use_native(False)
        lw = self.setup_lw_run()
        output = lw.run()
        assert self.check_result('lw', output)

    @unittest.skipIf(not pyrrtm.has_native,
                     'pyrrtm not compiled with native library.')
    def test_native(self):
        pyrrtm.use_native(True)
        lw = self.setup_lw_run()
        output = lw.run()
        assert self.check_result('lw', output)

    def test_disort(self):
        pyrrtm.use_native(False)
        lw = self.setup_lw_run()
        lw.iscat = 1
        output = lw.run()
        assert self.check_result('lw_disort', output)

    def test_auto_pz(self):
        pyrrtm.use_native(False)
        lw = self.setup_lw_run(auto_pz=True)
        output = lw.run()
        assert self.check_result('lw_auto_pz', output)


class TestSW(BaseTest, unittest.TestCase):

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

    def test_netcdf(self):
        pyrrtm.use_native(False)
        sw = self.setup_sw_run()
        output = sw.run()
        assert self.check_result('sw', output)

    @unittest.skipIf(not pyrrtm.has_native,
                     'pyrrtm not compiled with native library.')
    def test_native(self):
        pyrrtm.use_native(True)
        sw = self.setup_sw_run()
        output = sw.run()
        assert self.check_result('sw', output)

if __name__ == '__main__':
    unittest.main()
