
# directories used.
BPATH = build
LW_OUTPUT = $(BPATH)/rrtm_nc
SW_OUTPUT = $(BPATH)/rrtm_sw_nc

# platform flags:
FC = gfortran
FCFLAG = -fdefault-integer-8 -fdefault-real-8 -fdefault-double-8 -frecord-marker=4 -fno-automatic

UTIL_FILE = util_gfortran.f

#### long wave:

LW_FSRCS = librrtm.f rtreg.f rtr.f rrtatm.f setcoef.f taumol.f rtregcld.f \
	rtrcld.f extra.f rtrcldmr.f rtregcldmr.f k_g.f cldprop.f \
	rtrdis.f RDI1MACH.f  ErrPack.f LINPAK.f disort.f $(UTIL_FILE)

LW_BPATH = $(BPATH)/lw
LW_SRC = lw

O_LW = ${LW_FSRCS:%.f=$(LW_BPATH)/%.o}

#### short wave:

SW_FSRCS = librrtm_sw.f cldprop.f LINPAK.f setcoef.f disort.f taumoldis.f   \
		   ErrPack.f RDI1MACH.f  extra.f rrtatm.f k_g.f \
		   rtrdis.f $(UTIL_FILE)

SW_BPATH = $(BPATH)/sw
SW_SRC = sw

O_SW = ${SW_FSRCS:%.f=$(SW_BPATH)/%.o}

#### wrapper

WRAPPER_SRC = wrapper
WRAPPER_BPATH = $(BPATH)
LW_WRAPPER_CSRCS = rrtm_netcdf.c common.c lw_wrapper.c
SW_WRAPPER_CSRCS = rrtm_sw_netcdf.c common.c sw_wrapper.c
HEADERS = $(LW_SRC)/librrtm.h $(WRAPPER_SRC)/common.h \
	$(WRAPPER_SRC)/lw_wrapper.h $(WRAPPER_SRC)/sw_wrapper.h

CFLAGS = 
LFLAGS = -lnetcdf -lm -lgfortran

O_SW_WRAPPER = ${SW_WRAPPER_CSRCS:%.c=$(WRAPPER_BPATH)/%.o}
O_LW_WRAPPER = ${LW_WRAPPER_CSRCS:%.c=$(WRAPPER_BPATH)/%.o}

#### Python module

PYMOD_BPATH = $(BPATH)/pymodule

######################

.PHONY : all clean pymodule

all : | $(LW_BPATH) $(SW_BPATH) $(LW_OUTPUT) $(SW_OUTPUT)

clean :
	rm -rf $(LW_BPATH) $(WRAPPER_BPATH)

pymodule : all python/interface.py
	mkdir -p $(PYMOD_BPATH)
	cp python/interface.py $(PYMOD_BPATH)/__init__.py
	cp $(LW_OUTPUT) $(SW_OUTPUT) $(PYMOD_BPATH)/.

$(LW_OUTPUT) : $(O_LW) $(O_LW_WRAPPER)
	  gcc $(LFLAGS) -o $(LW_OUTPUT) $^

$(SW_OUTPUT) : $(O_SW) $(O_SW_WRAPPER)
	  gcc $(LFLAGS) -o $(SW_OUTPUT) $^

$(LW_BPATH) :
	mkdir -p $(LW_BPATH)

$(LW_BPATH)/%.o : $(LW_SRC)/%.f
	$(FC) -c $(FCFLAG)  $< -o $@

$(SW_BPATH) :
	mkdir -p $(SW_BPATH)

$(SW_BPATH)/%.o : $(SW_SRC)/%.f
	$(FC) -c $(FCFLAG)  $< -o $@

$(WRAPPER_BPATH)/%.o : $(WRAPPER_SRC)/%.c $(HEADERS)
	gcc -c $(CFLAGS) $< -o $@
