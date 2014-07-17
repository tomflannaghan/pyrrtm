#include <netcdf.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/************************** Error Handling *********************/

void error_and_exit(const char *message) {
  fprintf(stdout, "Error: %s\n", message);
  exit(-1);  
}

void handle_error(int status) {
  if (status != NC_NOERR) {
    error_and_exit(nc_strerror(status));
  }
}

/************************** Netcdf convinience ****************/

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
