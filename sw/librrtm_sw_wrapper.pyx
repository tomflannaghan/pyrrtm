cdef extern from "librrtmsafe_sw.h":
    char rrtmerr_message[100]
    int rrtmsafe_sw_run(long nstr, long idelm, long icos, double juldat, double sza, long isolvar, double * solvar, long iemiss, long ireflect, double * semis, long nlayers, double * tavel, double * pavel, double * tz, double * pz, long nmol, double * wkl, double * wbrodl, double * totuflux, double * totdflux, double * fnet, double * htr)

import numpy

def run_rrtm(nstr, juldat, sza, solvar,
             ireflect, semis,
             tavel, pavel, tz, pz, wkl, wbrodl):
    # init
    cdef double solvar_c = solvar

    # surface
    cdef double semis_c = semis

    # profiles
    nlayers = len(tavel)
    assert wbrodl.shape == pavel.shape == (nlayers,)
    assert pz.shape == tz.shape == (nlayers + 1,)
    # c order [nlayers, nmol] = fortran order [nmol, nlayers]:
    assert wkl.shape[0] == nlayers
    nmol = wkl.shape[1]

    carr = lambda a: numpy.ascontiguousarray(a, dtype='double')
    cdef double [:] tavel_v = carr(tavel)
    cdef double [:] pavel_v = carr(pavel)
    cdef double [:] tz_v = carr(tz)
    cdef double [:] pz_v = carr(pz)
    cdef double [:, :] wkl_v = carr(wkl)
    cdef double [:] wbrodl_v = carr(wbrodl)

    # the output
    totuflux = numpy.zeros((nlayers + 1,), dtype='double')
    totdflux = numpy.zeros((nlayers + 1,), dtype='double')
    fnet = numpy.zeros((nlayers + 1,), dtype='double')
    htr = numpy.zeros((nlayers + 1,), dtype='double')
    cdef double [:] totuflux_v = totuflux
    cdef double [:] totdflux_v = totdflux
    cdef double [:] fnet_v = fnet
    cdef double [:] htr_v = htr

    # call the routine
    ret = rrtmsafe_sw_run(
        nstr, 0, 0, juldat, sza, 1, &solvar_c,
        1, ireflect, &semis_c,
        nlayers, &tavel_v[0], &pavel_v[0], &tz_v[0], &pz_v[0], 
        nmol, &wkl_v[0, 0], &wbrodl_v[0],
        &totuflux_v[0], &totdflux_v[0], &fnet_v[0], &htr_v[0])

    if ret != 0:
        # we had a problem. print the exception.
        raise Exception("Error in RRTM SW:" + <bytes> rrtmerr_message)

    return totuflux, totdflux, fnet, htr[:-1]
