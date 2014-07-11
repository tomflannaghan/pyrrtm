# platform flags:
FC = gfortran
FCFLAG = -fdefault-integer-8 -fdefault-real-8 -fdefault-double-8 -frecord-marker=4 -fno-automatic
CFLAGS = 
LFLAGS = -lnetcdf -lm -lgfortran

UTIL_FILE = util_gfortran.f
COMMON_SRCS = rtreg.f rtr.f rrtatm.f setcoef.f taumol.f rtregcld.f \
	rtrcld.f extra.f rtrcldmr.f rtregcldmr.f k_g.f cldprop.f \
	rtrdis.f RDI1MACH.f  ErrPack.f LINPAK.f disort.f $(UTIL_FILE)

FSRCS = librrtm.f $(COMMON_SRCS)
CSRCS = rrtm_netcdf.c
HEADERS = librrtm.h

# directories used.
BPATH = build
OUTPUT = $(BPATH)/rrtm_lw_nc

# Object file names
OBPATH = ${FSRCS:%.f=$(BPATH)/%.o} ${CSRCS:%.c=$(BPATH)/%.o}

######################

.PHONY : all clean

all : | $(BPATH) $(OUTPUT)

clean :
	rm -f $(BPATH)/*

$(OUTPUT) : $(OBPATH)
	  gcc $(LFLAGS) -o $(OUTPUT) $^

$(BPATH) :
	mkdir -p $(BPATH)

$(BPATH)/%.o : %.f
	$(FC) -c $(FCFLAG)  $< -o $@

$(BPATH)/%.o : %.c $(HEADERS)
	gcc -c $(CFLAGS) $< -o $@
