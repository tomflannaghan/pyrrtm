#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include "librrtmsafe_sw.h"

/* Definitions of stuff in librrtm.f. */

void initrrtm_(long * nstr,     // Number of streams used by DISORT (0 -- 2)
               long * idelm,    // 0 = normal. 1 = delta-M approximation?
               long * icos,     // 0 = normal. Controls cosine response?
               double * juldat, // Julian Day
               double * sza,    // Solar zenith angle (0 = overhead).
               long * isolvar,  // 0 = no solar variability, 1,2 = use solvar.
               double * solvar);// Solar scaling function in each band [16]

void initsurface_(long * iemis,    // 0 = set semis to 1. 1,2 = use semis.
                  long * ireflect, // 0 = Lambertian reflection, 1 = specular.
                  double * semis); // Surface emissivity in each band [16]

void initprofile_(long * nlayers,  // the number of layers
                  double * tavel,  // average layer temp [nlayers]
                  double * pavel,  // average layer pres [nlayers]
                  double * tz,     // level temp [nlayers + 1]
                  double * pz,     // level pres [nlayers + 1]
                  long * nmol,     // the number of molecules used
                  double * wkl,    // conc. of molecules [nlayers,nmol]
                  double * wbrodl);// broadening conc. [nlayers]

void execrun_();

void getoutput_(double * totuflux, // upwelling flux [nlayers + 1]
                double * totdflux, // downwelling flux [nlayers + 1]
                double * fnet,     // net flux [nlayers + 1]
                double * htr);     // heating rate [nlayers + 1]

/*
 * Here, we write a safe wrapper that catches errors.
 */

jmp_buf rrtmerr_jump;

void rrtmerr_(char * message, long * length) {
  // this function will be called from fortran
  int n = (int) *length;
  int i;
  
  for (i=0; i<n; i++) {
    rrtmerr_message[i] = message[i];
  }
  rrtmerr_message[i] = '\0';
  // return control to run_rrtm.
  longjmp(rrtmerr_jump, 1);
}

int rrtmsafe_sw_run(long nstr, long idelm, long icos, 
                    double juldat, double sza, long isolvar, double * solvar,
                    long iemiss, long ireflect, double * semis,
                    long nlayers, double * tavel, double * pavel, 
                    double * tz, double * pz,
                    long nmol, double * wkl, double * wbrodl,
                    double * totuflux, double * totdflux, 
                    double * fnet, double * htr) {
  
  if (! setjmp(rrtmerr_jump) ) {
    initrrtm_(&nstr, &idelm, &icos, &juldat, &sza, &isolvar, solvar);
    initsurface_(&iemiss, &ireflect, semis);
    initprofile_(&nlayers, tavel, pavel, tz, pz, &nmol, wkl, wbrodl);
    execrun_();
    getoutput_(totuflux, totdflux, fnet, htr);
    return 0;
  }
  // if we get to here, there was an error.
  return 1;
}
