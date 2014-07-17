import numpy

molar_mass = {"H2O": 18.0153, "CO2": 28.02, "O3": 48.0, "N2O": 44.013, 
              "CO": 28.01, "CH4": 16.04, "O2": 32.0, "dry air": 28.97}
molar_mass_air = 28.97
R = 8.3144621
R_air = 1000 * R / molar_mass_air
avogadro = 6.02214129e23
g = 9.8

def column_density_rrtm(tavel, pz, pavel):
    """Odd column density quantity for RRTM. Pressures are given in
    hPa, and temperature is given in Kelvin. Accepts 1d arrays."""
    ## density in kg / m^3
    rho = 100 * pavel / (R_air * tavel)
    ## moles / cm^3
    mole_density = 1000 * rho / molar_mass_air / 100**3
    ## molecules / cm3
    molecules_cm3 = mole_density * avogadro
    ## output in molecules / cm2
    dz = - (pz[1:] - pz[:-1]) * R_air * tavel \
        / (g * pavel)
    return 100 * molecules_cm3 * dz

def mmr_to_column_density(species, mmr, tavel, pz, pavel):
    """Returns the column density (suitable for use in RRTM)."""
    return column_density_rrtm(tavel, pz, pavel) *\
        mmr * molar_mass['dry air'] / molar_mass[species.upper()]

def column_density_to_mmr(species, col, tavel, pz, pavel):
    """Returns the mass mixing ratio."""
    return col / column_density_rrtm(tavel, pz, pavel) *\
        molar_mass[species.upper()] / molar_mass['dry air']

def mmr_to_vmr(gas, val):
    return val * molar_mass['dry air'] / molar_mass[gas.upper()]

def vmr_to_mmr(gas, val):
    return val * molar_mass[gas.upper()] / molar_mass['dry air']
