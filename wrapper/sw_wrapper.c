#include <netcdf.h>
#include <stdlib.h>
#include <stdio.h>
#include "common.h"
#include "../sw/librrtmsafe_sw.h"

// make sure these values are sufficiently large:
#define MAXMOL 7
#define MAXLAYERS 10000

// memory for the parameters read in:
double tavel[MAXLAYERS];
double pavel[MAXLAYERS];
double tz[MAXLAYERS];
double pz[MAXLAYERS];
double wkl[MAXLAYERS * MAXMOL];
double wbrodl[MAXLAYERS];
long nlayers, nmol, nstr, ireflect;
double juldat, sza, solvar, semis;

// memory for parameters read out:
double totuflux[MAXLAYERS];
double totdflux[MAXLAYERS];
double htr[MAXLAYERS];
double fnet[MAXLAYERS];


int sw_read_netcdf(const char *ifile) {
  int ret;
  int ncid;
  size_t nlayers_i, nlevels_i, nmol_i;
  int layer_dim, level_dim, mol_dim;
  
  // open the netcdf file for reading.
  if ( (ret = nc_open(ifile, NC_NOWRITE, &ncid)) != 0 ) 
    handle_error(ret);
  
  // get the dimensions.
  if ( ((ret = nc_inq_dimid(ncid, "layer", &layer_dim)) != 0) ||
       ((ret = nc_inq_dimid(ncid, "level", &level_dim)) != 0) ||
       ((ret = nc_inq_dimid(ncid, "mol", &mol_dim)) != 0)    ) {
    nc_close(ncid);
    handle_error(ret);
  }
  
  // get dim lengths.
  if ( ((ret = nc_inq_dimlen(ncid, layer_dim, &nlayers_i)) != 0) ||
       ((ret = nc_inq_dimlen(ncid, level_dim, &nlevels_i)) != 0) ||
       ((ret = nc_inq_dimlen(ncid, mol_dim, &nmol_i)) != 0)    ) {
    nc_close(ncid);
    handle_error(ret);
  }
  
  // convert to long and store in the global parameters
  nlayers = (long) nlayers_i;
  nmol = (long) nmol_i;
  
  // sanity checks.
  if (nlevels_i >= MAXLAYERS) error_and_exit("Number of levels too large.");
  if (nlevels_i != nlayers_i + 1)
    error_and_exit("Number of levels must be number of layers + 1.");
  if ((nmol_i > 7) || (nmol_i < 1)) 
    error_and_exit("Number of molecules must be between 1 and 7.");
  
  // now we are in the position to read the variables.
  if ( ((ret = read_var_double(ncid, "tavel", tavel)) != 0) ||
       ((ret = read_var_double(ncid, "pavel", pavel)) != 0) ||
       ((ret = read_var_double(ncid, "tz", tz)) != 0) ||
       ((ret = read_var_double(ncid, "pz", pz)) != 0) ||
       ((ret = read_var_double(ncid, "wkl", wkl)) != 0) ||
       ((ret = read_var_double(ncid, "wbrodl", wbrodl)) != 0)) {
    nc_close(ncid);
    handle_error(ret);
  }
  
  // we also need to read some attributes.
  if ( ((ret = nc_get_att_long(ncid, NC_GLOBAL, "sw_nstr", &nstr)) != 0) ||
       ((ret = nc_get_att_long(ncid, NC_GLOBAL, 
                               "sw_ireflect", &ireflect)) != 0) ||
       ((ret = nc_get_att_double(ncid, NC_GLOBAL, 
                                 "sw_juldat", &juldat)) != 0) ||
       ((ret = nc_get_att_double(ncid, NC_GLOBAL, "sw_sza", &sza)) != 0) ||
       ((ret = nc_get_att_double(ncid, NC_GLOBAL, "sw_semis", &semis)) != 0) ||
       ((ret = nc_get_att_double(ncid, NC_GLOBAL, "sw_solvar", &solvar)) != 0)
       ) {
    nc_close(ncid);
    handle_error(ret);    
  }
  
  // sanity checks on the attributes.
  if ((nstr < 0) || (nstr > 4)) error_and_exit("sw_nstr must be 0 -- 3.");
  if ((ireflect < 0) || (ireflect > 1)) 
    error_and_exit("sw_ireflect must be 0 or 1.");
  if ((solvar < 0) || (solvar > 3)) 
    error_and_exit("sw_solvar must be between 0 and 3.");
  if ((semis < 0) || (semis > 1)) 
    error_and_exit("semis must be between 0 and 1.");

  // close the netcdf file.
  nc_close(ncid);

return 0;
}


int sw_write_netcdf(const char *ofile) {
  int ret;
  int ncid;
  int level_dim;
  int layer_dim;
  int vid_totuflux, vid_totdflux, vid_htr, vid_fnet, vid_pz, vid_pavel;

  // open the netcdf file for writing.
  if ( (ret = nc_create(ofile, NC_CLOBBER, &ncid)) != 0 ) 
    handle_error(ret);

  // create the dimensions and variables, and exit define mode.
  if ( ((ret = nc_def_dim(ncid, "layer", nlayers, &layer_dim)) != 0) ||
       ((ret = nc_def_dim(ncid, "level", nlayers + 1, &level_dim)) != 0) ||
       ((ret = nc_def_var(ncid, "totuflux", NC_DOUBLE, 
                          1, &level_dim, &vid_totuflux)) != 0) ||
       ((ret = nc_def_var(ncid, "totdflux", NC_DOUBLE, 
                          1, &level_dim, &vid_totdflux)) != 0) ||
       ((ret = nc_def_var(ncid, "fnet", NC_DOUBLE, 
                          1, &level_dim, &vid_fnet)) != 0) ||
       ((ret = nc_def_var(ncid, "htr", NC_DOUBLE, 
                          1, &layer_dim, &vid_htr)) != 0) ||
       ((ret = nc_def_var(ncid, "pz", NC_DOUBLE, 
                          1, &level_dim, &vid_pz)) != 0) ||
       ((ret = nc_def_var(ncid, "pavel", NC_DOUBLE, 
                          1, &layer_dim, &vid_pavel)) != 0) ||
       ((ret = nc_enddef(ncid)) != 0) ) {
    nc_close(ncid);
    handle_error(ret);
  }

  // now fill up the variables.
  if ( ((ret = nc_put_var_double(ncid, vid_totuflux, totuflux)) != 0) ||
       ((ret = nc_put_var_double(ncid, vid_totdflux, totdflux)) != 0) ||
       ((ret = nc_put_var_double(ncid, vid_fnet, fnet)) != 0) ||
       ((ret = nc_put_var_double(ncid, vid_htr, htr)) != 0) ||
       ((ret = nc_put_var_double(ncid, vid_pz, pz)) != 0) ||
       ((ret = nc_put_var_double(ncid, vid_pavel, pavel)) != 0) ) {
    nc_close(ncid);
    handle_error(ret);
  }
  
  nc_close(ncid);
  return 0;
}


void sw_run(void) {
  if (rrtmsafe_sw_run(nstr, 0, 0, juldat, sza, 1, &solvar, 
                      1, ireflect, &semis,
                      nlayers, tavel, pavel, tz, pz, nmol, wkl, wbrodl,
                      totuflux, totdflux, fnet, htr) != 0) {
    printf("Error: %s\n", rrtmerr_message);
    exit(1);
  }
}


void sw_print_results(void) {
  int i;
  printf("\nResults:\n");
  printf("%10s %10s %10s %10s %10s\n", 
         "PZ", "TOTUFLUX", "TOTDFLUX", "FNET", "HTR");
  for (i=0; i<=nlayers; i++) {
    printf("%10.2f %10.2f %10.2f %10.2f %10.2f\n", 
           pz[i], totuflux[i], totdflux[i], fnet[i], htr[i]);
  }
}
