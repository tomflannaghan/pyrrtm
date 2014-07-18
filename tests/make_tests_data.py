import pyrrtm
import unittest
import scipy.io.netcdf

nc = scipy.io.netcdf.netcdf_file('input_mls.nc', 'r')
pavel = nc.variables['pavel'][:].copy()
tavel = nc.variables['tavel'][:].copy()
pz = nc.variables['pz'][:].copy()
tz = nc.variables['tz'][:].copy() 
wkl = nc.variables['wkl'][:].copy().T
wbrodl = nc.variables['wbrodl'][:].copy()
nc.close()        
h2o = wkl[0]
co2 = wkl[1]
o3 = wkl[2]
n2o = wkl[3]
co = wkl[4]
ch4 = wkl[5]
o2 = wkl[6]

output = {}

lw = pyrrtm.LW(len(tavel))
lw.tavel = tavel
lw.pavel = pavel
lw.pz = pz
lw.tz = tz
lw.set_species('h2o', h2o, 'molecules/cm2')
lw.set_species('co2', co2, 'molecules/cm2')
lw.set_species('o3', o3, 'molecules/cm2')
lw.set_species('n2o', n2o, 'molecules/cm2')
lw.set_species('co', co, 'molecules/cm2')
lw.set_species('ch4', ch4, 'molecules/cm2')
lw.set_species('o2', o2, 'molecules/cm2')
lw.iscat = 0
lw.numangs = 2
lw.ireflect = 0
lw.tbound = 294.2
lw.semis = 1
output['lw'] = lw.run()

lw = pyrrtm.LW(len(tavel))
lw.tavel = tavel
lw.pavel = pavel
lw.pz = pz
lw.tz = tz
lw.set_species('h2o', h2o, 'molecules/cm2')
lw.set_species('co2', co2, 'molecules/cm2')
lw.set_species('o3', o3, 'molecules/cm2')
lw.set_species('n2o', n2o, 'molecules/cm2')
lw.set_species('co', co, 'molecules/cm2')
lw.set_species('ch4', ch4, 'molecules/cm2')
lw.set_species('o2', o2, 'molecules/cm2')
lw.iscat = 1
lw.numangs = 2
lw.ireflect = 0
lw.tbound = 294.2
lw.semis = 1
output['lw_disort'] = lw.run()

lw = pyrrtm.LW(len(tavel))
lw.tavel = tavel
lw.pavel = pavel
lw.set_species('h2o', h2o, 'molecules/cm2')
lw.set_species('co2', co2, 'molecules/cm2')
lw.set_species('o3', o3, 'molecules/cm2')
lw.set_species('n2o', n2o, 'molecules/cm2')
lw.set_species('co', co, 'molecules/cm2')
lw.set_species('ch4', ch4, 'molecules/cm2')
lw.set_species('o2', o2, 'molecules/cm2')
lw.iscat = 0
lw.numangs = 2
lw.ireflect = 0
lw.tbound = 294.2
lw.semis = 1
output['lw_auto_pz'] = lw.run()

sw = pyrrtm.SW(len(tavel))
sw.tavel = tavel
sw.pavel = pavel
sw.tz = tz
sw.pz = pz
sw.set_species('h2o', h2o, 'molecules/cm2')
sw.set_species('co2', co2, 'molecules/cm2')
sw.set_species('o3', o3, 'molecules/cm2')
sw.set_species('n2o', n2o, 'molecules/cm2')
sw.set_species('co', co, 'molecules/cm2')
sw.set_species('ch4', ch4, 'molecules/cm2')
sw.set_species('o2', o2, 'molecules/cm2')
sw.ireflect = 0
sw.semis = 1
sw.sza = 0
sw.juldat = 0
sw.nstr = 2
sw.solvar = 1
output['sw'] = sw.run()

nc = scipy.io.netcdf.netcdf_file('tests_data.nc', 'w')
nc.createDimension('layer', len(tavel))
nc.createDimension('level', len(tz))
nc.createVariable('tavel', 'd', ('layer',))[:] = tavel
nc.createVariable('pavel', 'd', ('layer',))[:] = pavel
nc.createVariable('tz', 'd', ('level',))[:] = tz
nc.createVariable('pz', 'd', ('level',))[:] = pz
nc.createVariable('h2o', 'd', ('layer',))[:] = h2o
nc.createVariable('co2', 'd', ('layer',))[:] = co2
nc.createVariable('o3', 'd', ('layer',))[:] = o3
nc.createVariable('n2o', 'd', ('layer',))[:] = n2o
nc.createVariable('co', 'd', ('layer',))[:] = co
nc.createVariable('ch4', 'd', ('layer',))[:] = ch4
nc.createVariable('o2', 'd', ('layer',))[:] = o2
for key in output:
    nc.createVariable('htr_' + key, 'd', ('layer',))[:] = output[key].htr
nc.close()
