#include "librrtm.h"
#include <netcdf.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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
double semis[16];
long nlayers, nmol, iscat, numangs, ireflect;
double tbound;

// memory for parameters read out:
double totuflux[MAXLAYERS];
double totdflux[MAXLAYERS];
double htr[MAXLAYERS];
double fnet[MAXLAYERS];

/************************** Main block ************************/
int read_netcdf_file(const char *);
void run_rrtm(void);

void usage_and_exit() {
  const char *usage =
    "Usage: rrtm_lw [-v[v]] -i <input file> -o <output file> \n"
    "\n";
  printf("%s", usage);
  exit(-1);  
}

int main(int argc, char *argv[]) {
  int verbose = 0, vverbose = 0;

  char * ifile = NULL;
  char * ofile = NULL;
  char * rest;
  int i;
  int state = 0;

  // Command line handler.
  for (i=1; i<argc; i++) {
    if (argv[i][0] == '-') {
      rest = &(argv[i][1]);
      // handle flags that do not take an input.
      if (strcmp(rest, "v") == 0) verbose = 1;
      else if (strcmp(rest, "vv") == 0) {verbose = 1; vverbose = 1;}
      else if (strcmp(rest, "i") == 0) ;
      else if (strcmp(rest, "o") == 0) ;
      else usage_and_exit();
    }
    else {
      // handle flag arguments positional arguments.
      if (strcmp(rest, "i") == 0) ifile = argv[i];
      else if (strcmp(rest, "o") == 0) ofile = argv[i];
      else usage_and_exit();
    }
  }
  
  // Check everything needed was supplied.
  if (ifile == NULL) usage_and_exit();
  
  if (verbose) printf("Reading input file `%s`.\n", ifile);
  read_netcdf_file(ifile);
  if (verbose) printf("Data loaded.\n");
  run_rrtm();
  if (verbose) printf("The model ran.\n");

  if (vverbose) {
    printf("\nResults:\n");
    printf("%10s %10s %10s %10s %10s\n", 
           "PZ", "TOTUFLUX", "TOTDFLUX", "FNET", "HTR");
    for (i=0; i<=nlayers; i++) {
      printf("%10.2f %10.2f %10.2f %10.2f %10.2f\n", 
             pz[i], totuflux[i], totdflux[i], fnet[i], htr[i]);
    }
  }
  
  if (verbose) printf("Writing output file `%s`.\n", ofile);
  write_netcdf_file(ofile);
}

/************************** Error Handling *********************/

void error_and_exit(const char *message) {
  fprintf(stderr, "Error: %s\n", message);
  exit(-1);  
}

void handle_error(int status) {
  if (status != NC_NOERR) {
    error_and_exit(nc_strerror(status));
  }
}

/************************** Netcdf Input ***********************/

int read_netcdf_file(const char *ifile) {
  int ret;
  int ncid;
  size_t nlayers_i, nlevels_i, nmol_i, nbands_i;
  int layer_dim, level_dim, mol_dim, band_dim;
  
  // open the netcdf file for reading.
  if ( (ret = nc_open(ifile, NC_NOWRITE, &ncid)) != 0 ) 
    handle_error(ret);
  
  // get the dimensions.
  if ( ((ret = nc_inq_dimid(ncid, "layer", &layer_dim)) != 0) ||
       ((ret = nc_inq_dimid(ncid, "level", &level_dim)) != 0) ||
       ((ret = nc_inq_dimid(ncid, "band", &band_dim)) != 0) ||
       ((ret = nc_inq_dimid(ncid, "mol", &mol_dim)) != 0)    ) {
    nc_close(ncid);
    handle_error(ret);
  }
  
  // get dim lengths.
  if ( ((ret = nc_inq_dimlen(ncid, layer_dim, &nlayers_i)) != 0) ||
       ((ret = nc_inq_dimlen(ncid, level_dim, &nlevels_i)) != 0) ||
       ((ret = nc_inq_dimlen(ncid, band_dim, &nbands_i)) != 0) ||
       ((ret = nc_inq_dimlen(ncid, mol_dim, &nmol_i)) != 0)    ) {
    nc_close(ncid);
    handle_error(ret);
  }
  
  // convert to long and store in the global parameters
  nlayers = (long) nlayers_i;
  nmol = (long) nmol_i;
  
  // sanity checks.
  if (nlevels_i >= MAXLAYERS) error_and_exit("Number of levels too large.");
  if (nlevels_i != nlayers + 1)
    error_and_exit("Number of levels must be number of layers + 1.");
  if ((nmol > 7) || (nmol < 1)) 
    error_and_exit("Number of molecules must be between 1 and 7.");
  if (nbands_i != 16) error_and_exit("Number of bands must be 16.");
  

  // now we are in the position to read the variables.
  if ( ((ret = read_var_double(ncid, "tavel", tavel)) != 0) ||
       ((ret = read_var_double(ncid, "pavel", pavel)) != 0) ||
       ((ret = read_var_double(ncid, "tz", tz)) != 0) ||
       ((ret = read_var_double(ncid, "pz", pz)) != 0) ||
       ((ret = read_var_double(ncid, "wkl", wkl)) != 0) ||
       ((ret = read_var_double(ncid, "wbrodl", wbrodl)) != 0) ||
       ((ret = read_var_double(ncid, "semis", semis)) != 0)) {
    nc_close(ncid);
    handle_error(ret);
  }
  
  // we also need to read some attributes.
  if ( ((ret = nc_get_att_long(ncid, NC_GLOBAL, "iscat", &iscat)) != 0) ||
       ((ret = nc_get_att_long(ncid, NC_GLOBAL, "numangs", &numangs)) != 0) ||
       ((ret = nc_get_att_long(ncid, NC_GLOBAL, "ireflect", &ireflect)) != 0) ||
       ((ret = nc_get_att_double(ncid, NC_GLOBAL, "tbound", &tbound)) != 0) ) {
    nc_close(ncid);
    handle_error(ret);    
  }
  
  // sanity checks on the attributes.
  if ((iscat < 0) || (iscat > 2)) error_and_exit("iscat must be 0, 1 or 2.");
  if ((ireflect < 0) || (ireflect > 1)) 
    error_and_exit("ireflect must be 0 or 1.");
  if ((numangs < 0) || (numangs > 3)) 
    error_and_exit("numangs must be between 0 and 3.");

  // close the netcdf file.
  nc_close(ncid);

  return 0;
}

int read_var_double(int ncid, const char *name, double *data) {
  int ret;
  int varid;
  
  // var id is read here.
  if ( ((ret = nc_inq_varid(ncid, name, &varid)) != 0) ) {
    if (ret != NC_NOERR) return ret;
  }
  
  // now read the variable into memory.
  if ( ((ret = nc_get_var_double(ncid, varid, data)) != 0) ) {
    if (ret != NC_NOERR) return ret;
  }
  
  return 0;
}

/************************* Write Netcdf File **********************/

int write_netcdf_file(const char *ofile) {
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

/************************* Using the model ************************/
void run_rrtm(void) {
  long two = 2;
  initrrtm_(&iscat, &numangs);
  initsurface_(&two, &tbound, &ireflect, semis);
  initprofile_(&nlayers, tavel, pavel, tz, pz, &nmol, wkl, wbrodl);
  execrun_();
  getoutput_(totuflux, totdflux, fnet, htr);
}
