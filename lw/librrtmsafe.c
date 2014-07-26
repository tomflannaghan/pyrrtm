#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include "librrtmsafe.h"

/*
 * This defines everything in librrtm.f
 *
 * The shape of each argument is documented using C array
 * ordering. Everything is passed by pointer including
 * integers. "long" and "double" are used because of compiler flags
 * that specify double precision in the fortran.
 */

void initrrtm_(long * iscat,       // scattering option. 0 or 1.
               long * numangs);    // number of angles 0--3.

void initsurface_(long * iemiss,   // option for processing semis. Set to 2.
                  double * tbound, // surface temperature.
                  long * ireflect, // reflection options. Either 0 or 1.
                  double * semis); // surface emission for each band. [16]

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

int rrtmsafe_run(long iscat, long numangs, 
                 long iemiss, double tbound, long ireflect, double * semis,
                 long nlayers, double * tavel, double * pavel, 
                 double * tz, double * pz,
                 long nmol, double * wkl, double * wbrodl,
                 double * totuflux, double * totdflux, 
                 double * fnet, double * htr) {
  
  if (! setjmp(rrtmerr_jump) ) {
    initrrtm_(&iscat, &numangs);
    initsurface_(&iemiss, &tbound, &ireflect, semis);
    initprofile_(&nlayers, tavel, pavel, tz, pz, &nmol, wkl, wbrodl);
    execrun_();
    getoutput_(totuflux, totdflux, fnet, htr);
    return 0;
  }
  // if we get to here, there was an error.
  return 1;
}
