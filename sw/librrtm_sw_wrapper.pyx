cdef extern from "librrtm_sw.h":
    void initrrtm_(long * nstr, long * idelm, long * icos, double * juldat,
                   double * sza, long * isolvar, double * solvar)
    void initsurface_(long * iemiss, long * ireflect, double * semis) 
    void initprofile_(long * nlayers, double * tavel, double * pavel, 
                      double * tz, double * pz, 
                      long * nmol, double * wkl, double * wbrodl)
    void execrun_()
    void getoutput_(double * totuflux, double * totdflux, 
                    double * fnet, double * htr)

import numpy

def initrrtm(nstr, idelm, icos, juldat, sza, solvar):
    cdef long nstr_c = nstr
    cdef long idelm_c = idelm
    cdef long icos_c = icos
    cdef double juldat_c = juldat
    cdef double sza_c = sza
    cdef long isolvar_c = 1
    cdef double solvar_c = solvar
    initrrtm_(&nstr_c, &idelm_c, &icos_c, &juldat_c, 
              &sza_c, &isolvar_c, &solvar_c)

def initsurface(ireflect, semis):
    cdef long isemis_c = 1
    cdef long ireflect_c = ireflect
    cdef double semis_c = semis
    initsurface_(&isemis_c, &ireflect_c, &semis_c)

def initprofile(tavel, pavel, tz, pz, wkl, wbrodl):
    nlayers = len(tavel)
    assert wbrodl.shape == pavel.shape == (nlayers,)
    assert pz.shape == tz.shape == (nlayers + 1,)
    # c order [nlayers, nmol] = fortran order [nmol, nlayers]:
    assert wkl.shape[0] == nlayers
    nmol = wkl.shape[1]

    cdef long nlayers_c = nlayers
    cdef long nmol_c = nmol
    carr = lambda a: numpy.ascontiguousarray(a, dtype='double')
    cdef double [:] tavel_view = carr(tavel)
    cdef double [:] pavel_view = carr(pavel)
    cdef double [:] tz_view = carr(tz)
    cdef double [:] pz_view = carr(pz)
    cdef double [:, :] wkl_view = carr(wkl)
    cdef double [:] wbrodl_view = carr(wbrodl)
    initprofile_(&nlayers_c, &tavel_view[0], &pavel_view[0], 
                 &tz_view[0], &pz_view[0], 
                 &nmol_c, &wkl_view[0, 0], &wbrodl_view[0])
    
def execrun():
    execrun_()

def getoutput(nlayers):
    totuflux = numpy.zeros((nlayers + 1,), dtype='double')
    totdflux = numpy.zeros((nlayers + 1,), dtype='double')
    fnet = numpy.zeros((nlayers + 1,), dtype='double')
    htr = numpy.zeros((nlayers + 1,), dtype='double')
    cdef double [:] totuflux_v = totuflux
    cdef double [:] totdflux_v = totdflux
    cdef double [:] fnet_v = fnet
    cdef double [:] htr_v = htr
    getoutput_(&totuflux_v[0], &totdflux_v[0], &fnet_v[0], &htr_v[0])
    return totuflux, totdflux, fnet, htr[:-1]

