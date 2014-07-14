/* C Header file for librrtm_sw.f, the external API to the RRTM SW code. */

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
