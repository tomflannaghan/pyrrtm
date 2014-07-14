#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "sw_wrapper.h"


void usage_and_exit() {
  const char *usage =
    "Usage: rrtm_sw [-v[v]] -i <input file> -o <output file> \n"
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
  if (ofile == NULL) usage_and_exit();
  
  if (verbose) printf("Reading input file `%s`.\n", ifile);
  sw_read_netcdf(ifile);
  if (verbose) printf("Data loaded.\n");
  sw_run();
  if (verbose) printf("The model ran.\n");

  //////// rest not implemented.
  /*
  if (vverbose) {
    lw_print_results();
  }
  
  if (verbose) printf("Writing output file `%s`.\n", ofile);
  lw_write_netcdf(ofile);*/
}
