int rrtmsafe_run(long iscat, long numangs, 
                 long iemiss, double tbound, long ireflect, double * semis,
                 long nlayers, double * tavel, double * pavel, 
                 double * tz, double * pz,
                 long nmol, double * wkl, double * wbrodl,
                 double * totuflux, double * totdflux, 
                 double * fnet, double * htr);

char rrtmerr_message[100];
