cdef extern from "librrtm.h":
    void initrrtm_(long * iscat, long * numangs)
    void initsurface_(long * iemiss, double * tbound, 
                      long * ireflect, double * semis) 
    void initprofile_(long * nlayers, double * tavel, double * pavel, 
                      double * tz, double * pz, 
                      long * nmol, double * wkl, double * wbrodl)
    void execrun_()
    void getoutput_(double * totuflux, double * totdflux, 
                    double * fnet, double * htr)

import numpy

def initrrtm(iscat, numangs):
    cdef long iscat_c = iscat 
    cdef long numangs_c = numangs
    initrrtm_(&iscat_c, &numangs_c)

def initsurface(tbound, ireflect, semis):
    cdef long isemis_c = 1
    cdef long ireflect_c = ireflect
    cdef double tbound_c = tbound
    cdef double semis_c = semis
    initsurface_(&isemis_c, &tbound_c, &ireflect_c, &semis_c)

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
    cdef double [:] totuflux = numpy.zeros((nlayers + 1,), dtype='double')
    cdef double [:] totdflux = numpy.zeros((nlayers + 1,), dtype='double')
    cdef double [:] fnet = numpy.zeros((nlayers + 1,), dtype='double')
    cdef double [:] htr = numpy.zeros((nlayers,), dtype='double')
    getoutput_(&totuflux[0], &totdflux[0], &fnet[0], &htr[0])
    return totuflux, totdflux, fnet, htr
