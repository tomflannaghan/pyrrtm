/*
 * This is a C header file that allows C-Fortran interop.
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
