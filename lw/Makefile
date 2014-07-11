# platform flags:
FC = gfortran
FCFLAG = -fdefault-integer-8 -fdefault-real-8 -fdefault-double-8 -frecord-marker=4 -fPIC -fno-automatic
CFLAGS = -fPIC -pthread -fwrapv -fno-strict-aliasing -I/usr/include/python2.7 -I/home/splash/tflannag/extern/epd-7.3-2-rh5-x86_64/include/python2.7

UTIL_FILE = util_gfortran.f

# Files contained in src directory.  If additional .f files are added, add to this list.  
COMMON_SRCS = rtreg.f rtr.f rrtatm.f setcoef.f taumol.f rtregcld.f rtrcld.f extra.f rtrcldmr.f \
rtregcldmr.f k_g.f cldprop.f \
rtrdis.f RDI1MACH.f  ErrPack.f LINPAK.f disort.f $(UTIL_FILE)

SRCS = rrtm.f $(COMMON_SRCS)
LIB_SRCS = librrtm.f $(COMMON_SRCS)

# directories used.
CPATH = src
BPATH = build
OUTPUT = $(BPATH)/rrtm_lw
LIBPY_OUTPUT = $(BPATH)/rrtmflw.so

LIBPYX = $(BPATH)/rrtmflw_pyx
LIBPYX_OUTPUT = $(LIBPYX).so

# Object file names
OBPATH = ${SRCS:%.f=$(BPATH)/%.o}
LIB_OBPATH = ${LIB_SRCS:%.f=$(BPATH)/%.o}

######################

.PHONY : all lib clean

all : $(OUTPUT)

lib : $(LIBPY_OUTPUT) $(LIBPYX_OUTPUT)

clean :
	rm -f $(BPATH)/*

# Rule for compiling .o files
$(BPATH)/%.o : $(CPATH)/%.f
	$(FC) -c $(FCFLAG)  $< -o $@

$(OUTPUT) : $(OBPATH)
	  $(FC) $(FCFLAG) -o $(OUTPUT) $^

$(LIBPY_OUTPUT) : $(LIB_OBPATH) $(BPATH)/librrtm.pyf
	f2py --fcompiler=gfortran --f77flags="$(FCFLAG)" -c $^
	mv rrtmflw.so $(LIBPY_OUTPUT)

$(BPATH)/librrtm.pyf: $(CPATH)/librrtm.f
	f2py $(CPATH)/librrtm.f -m rrtmflw -h $(BPATH)/librrtm.pyf

$(LIBPYX_OUTPUT): $(LIB_OBPATH) $(BPATH)/librrtm.o $(LIBPYX).o
	gcc -shared $(CFLAGS) -lgfortran $^ -o $@	

$(BPATH)/%.o : $(CPATH)/%.c
	gcc -c $(CFLAGS) $< -o $@

$(CPATH)/%.c : $(CPATH)/%.pyx
	cython $< -o $@
