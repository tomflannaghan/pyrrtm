
# directories used.
BPATH = build
OUTPUT = $(BPATH)/rrtm_nc

# platform flags:
FC = gfortran
FCFLAG = -fdefault-integer-8 -fdefault-real-8 -fdefault-double-8 -frecord-marker=4 -fno-automatic

UTIL_FILE = util_gfortran.f

#### long wave:

LW_COMMON_SRCS = rtreg.f rtr.f rrtatm.f setcoef.f taumol.f rtregcld.f \
	rtrcld.f extra.f rtrcldmr.f rtregcldmr.f k_g.f cldprop.f \
	rtrdis.f RDI1MACH.f  ErrPack.f LINPAK.f disort.f $(UTIL_FILE)

LW_FSRCS = librrtm.f $(LW_COMMON_SRCS)

LW_BPATH = $(BPATH)/lw
LW_SRC = lw

# Object file names
O_LW = ${LW_FSRCS:%.f=$(LW_BPATH)/%.o}

#### wrapper

WRAPPER_SRC = wrapper
WRAPPER_BPATH = $(BPATH)
WRAPPER_CSRCS = rrtm_netcdf.c
HEADERS = $(LW_SRC)/librrtm.h

CFLAGS = 
LFLAGS = -lnetcdf -lm -lgfortran

O_WRAPPER = ${WRAPPER_CSRCS:%.c=$(WRAPPER_BPATH)/%.o}

######################

.PHONY : all clean

all : | $(LW_BPATH) $(OUTPUT)

clean :
	rm -f $(LW_BPATH)/*.o $(WRAPPER_BPATH)/*.o

$(OUTPUT) : $(O_LW) $(O_WRAPPER)
	  gcc $(LFLAGS) -o $(OUTPUT) $^

$(LW_BPATH) :
	mkdir -p $(LW_BPATH)

$(LW_BPATH)/%.o : $(LW_SRC)/%.f
	$(FC) -c $(FCFLAG)  $< -o $@

$(WRAPPER_BPATH)/%.o : $(WRAPPER_SRC)/%.c $(HEADERS)
	gcc -c $(CFLAGS) $< -o $@
