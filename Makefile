## directories used.

BPATH = build
LW_OUTPUT = $(BPATH)/rrtm_nc
SW_OUTPUT = $(BPATH)/rrtm_sw_nc

## platform flags:
FC = gfortran
FCFLAG = 	-fdefault-integer-8 -fdefault-real-8 -fdefault-double-8 \
			-frecord-marker=4 -fno-automatic

UTIL_FILE = util_gfortran.f

## long wave:

LW_FSRCS = 	librrtm.f rtreg.f rtr.f rrtatm.f setcoef.f taumol.f rtregcld.f \
			rtrcld.f extra.f rtrcldmr.f rtregcldmr.f k_g.f cldprop.f \
			rtrdis.f RDI1MACH.f  ErrPack.f LINPAK.f disort.f $(UTIL_FILE)

LW_BPATH = $(BPATH)/lw
LW_SRC = lw

O_LW = ${LW_FSRCS:%.f=$(LW_BPATH)/%.o}

## short wave:

SW_FSRCS = librrtm_sw.f cldprop.f LINPAK.f setcoef.f disort.f taumoldis.f   \
		   ErrPack.f RDI1MACH.f  extra.f rrtatm.f k_g.f \
		   rtrdis.f $(UTIL_FILE)

SW_BPATH = $(BPATH)/sw
SW_SRC = sw

O_SW = ${SW_FSRCS:%.f=$(SW_BPATH)/%.o}

## wrapper

WRAPPER_SRC = wrapper
WRAPPER_BPATH = $(BPATH)
LW_WRAPPER_CSRCS = rrtm_netcdf.c common.c lw_wrapper.c
SW_WRAPPER_CSRCS = rrtm_sw_netcdf.c common.c sw_wrapper.c
HEADERS = 	$(LW_SRC)/librrtm.h $(WRAPPER_SRC)/common.h \
			$(WRAPPER_SRC)/lw_wrapper.h $(WRAPPER_SRC)/sw_wrapper.h

CFLAGS = 
LFLAGS = -lnetcdf -lm -lgfortran

O_SW_WRAPPER = ${SW_WRAPPER_CSRCS:%.c=$(WRAPPER_BPATH)/%.o}
O_LW_WRAPPER = ${LW_WRAPPER_CSRCS:%.c=$(WRAPPER_BPATH)/%.o}

## Python module

PYMOD_BPATH = $(BPATH)/pymodule
PPYMOD_SRCS = 	netcdf_interface.py native_interface.py __init__.py \
				chem.py low_level.py
PYMOD_SRCS = ${PPYMOD_SRCS:%.py=python/%.py}

## Python .so libraries

LW_SO_BASE = librrtm_wrapper
SW_SO_BASE = librrtm_sw_wrapper
LW_SO = $(BPATH)/$(LW_SO_BASE).so
SW_SO = $(BPATH)/$(SW_SO_BASE).so
SO_FFLAGS = -fPIC
LW_SO_O = ${LW_FSRCS:%.f=$(LW_BPATH)/%_so.o}
SW_SO_O = ${SW_FSRCS:%.f=$(SW_BPATH)/%_so.o}
PYX_CFLAGS = -fPIC -pthread -fwrapv -fno-strict-aliasing -I/usr/include/python2.7

######################

.PHONY : all clean pymodule_netcdf pymodule_native

all : | $(LW_BPATH) $(SW_BPATH) $(LW_OUTPUT) $(SW_OUTPUT)

clean :
	rm -rf $(BPATH)

pymodule_netcdf : all $(PYMOD_SRCS)
	rm -rf $(PYMOD_BPATH)
	mkdir -p $(PYMOD_BPATH)
	cp $(LW_OUTPUT) $(SW_OUTPUT) $(PYMOD_SRCS) $(PYMOD_BPATH)/.

pymodule_native : all $(LW_SO) $(SW_SO) $(PYMOD_SRCS)
	rm -rf $(PYMOD_BPATH)
	mkdir -p $(PYMOD_BPATH)
	cp $(LW_OUTPUT) $(SW_OUTPUT) $(LW_SO) $(SW_SO) $(PYMOD_SRCS) $(PYMOD_BPATH)/.

## Netcdf interface:

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

## Pure python interface:

$(LW_SO) : $(LW_SO_O) $(LW_BPATH)/librrtm_wrapper.o
	gcc -shared $(LFLAGS) $(PYX_CFLAGS) $^ -o $@

$(SW_SO) : $(SW_SO_O) $(SW_BPATH)/librrtm_sw_wrapper.o
	gcc -shared $(LFLAGS) $(PYX_CFLAGS) $^ -o $@

$(LW_BPATH)/librrtm_wrapper.o : $(LW_SRC)/librrtm.h $(LW_SRC)/$(LW_SO_BASE).pyx
	cython $(LW_SRC)/$(LW_SO_BASE).pyx -o $(LW_BPATH)/$(LW_SO_BASE).c
	gcc -c -I$(LW_SRC) $(CFLAGS) $(PYX_CFLAGS) \
	    $(LW_BPATH)/$(LW_SO_BASE).c -o $@

$(SW_BPATH)/librrtm_sw_wrapper.o : $(SW_SRC)/librrtm_sw.h $(SW_SRC)/$(SW_SO_BASE).pyx
	cython $(SW_SRC)/$(SW_SO_BASE).pyx -o $(SW_BPATH)/$(SW_SO_BASE).c
	gcc -c -I$(SW_SRC) $(CFLAGS) $(PYX_CFLAGS) \
	     $(SW_BPATH)/$(SW_SO_BASE).c -o $@

$(LW_BPATH)/%_so.o : $(LW_SRC)/%.f
	$(FC) -c $(FCFLAG) $(SO_FFLAGS)  $< -o $@

$(SW_BPATH)/%_so.o : $(SW_SRC)/%.f
	$(FC) -c $(FCFLAG) $(SO_FFLAGS)  $< -o $@

