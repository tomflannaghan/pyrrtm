int rrtmsafe_sw_run(long nstr, long idelm, long icos, 
                    double juldat, double sza, long isolvar, double * solvar,
                    long iemiss, long ireflect, double * semis,
                    long nlayers, double * tavel, double * pavel, 
                    double * tz, double * pz,
                    long nmol, double * wkl, double * wbrodl,
                    double * totuflux, double * totdflux, 
                    double * fnet, double * htr);

char rrtmerr_message[100];
