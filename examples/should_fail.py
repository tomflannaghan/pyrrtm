import pyrrtm
import numpy
import scipy.io.netcdf
pyrrtm.use_native(pyrrtm.has_native)

# read in the data
nc = scipy.io.netcdf.netcdf_file('tests_data.nc', 'r')
tavel = nc.variables['tavel'][:] # layer average temperature
pavel = nc.variables['pavel'][:] # layer average pressure
tz = nc.variables['tz'][:]       # level temperature
pz = nc.variables['pz'][:]       # level pressure
co2 = nc.variables['co2'][:]
o3 = nc.variables['o3'][:]
h2o = nc.variables['h2o'][:]
nc.close()

# set up the radiation code
nlayers = len(tavel)
lw = pyrrtm.LW(nlayers)
lw.iscat = 1 # iscat means that disort is used.
lw.tavel = tavel + 200 # hit the code hard to force it to fail.
lw.pavel = pavel
lw.tz = tz
lw.pz = pz
lw.set_species('co2', co2, 'molecules/cm2')
lw.set_species('h2o', h2o, 'molecules/cm2')
lw.set_species('o3', o3, 'molecules/cm2')

# run the radiation code
output = lw.run()

