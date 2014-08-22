# -------------------------------------------------
# Main directory paths
# -------------------------------------------------
MAIN     = /scratch2/killedar/sourcecodes
#MAIN     = /scratch3/killedar/sourcecodes/public

MAINDIR   = /scratch3/killedar/sourcecodes
BUILDDIR  = $(MAINDIR)/mine
INCDIR    = $(MAINDIR)/include

#FFTW    =  -I$(MAIN)/lib/fftw/include -L$(MAIN)/lib/fftw/lib -lfftw3
FFTW    =  -I$(MAIN)/fftw-3.2.2/include -L$(MAIN)/fftw-3.2.2/lib -lfftw3


# -------------------------------------------------
# Libraries
# -------------------------------------------------

#LIBS    =  -L$(GCCDIR) -lg2c -L$(CFITSIODIR)/lib -lcfitsio -lnsl -lm -L$(PGPLOTDIR) -lpgplot -L$(X11DIR) -lX11 -L$(FFTWDIR)/lib -lfftw3 -J$(MODDIR) -lm  -I$(FFTWDIR)/include -I$(CFITSIODIR)/include
#LIBS	= $(FFTW)



# -------------------------------------------------
# Compiler
# -------------------------------------------------
FC = gfortran


# -------------------------------------------------
# Debugging Flags
# -------------------------------------------------
DBGFLAG  = -g             # generate debugger information (all compilers)

#DBGFLAG += -O0            # no optimisation (good for testing/debugging)
#DBGFLAG += -O1
DBGFLAG += -O2            # moderate optimisation
#DBGFLAG += -O3            # lots of optimisation

#DBGFLAG += -ffpe-trap=precision,zero,underflow,overflow,denormal,invalid
#DBGFLAG += -ffpe-trap=zero,underflow,overflow,denormal,invalid
DBGFLAG += -ffpe-trap=zero,underflow,overflow,invalid # (RECOMMENDED)

DBGFLAG += -frecord-marker=4    # (gfortran) read proper amount of wrapper 
DBGFLAG += -fbounds-check       # (gfortran) (RECOMMENDED)
DBGFLAG += -frange-check       	# (gfortran) (RECOMMENDED)

#DBGFLAG += -fconvert=little-endian	# (gfortran)
#DBGFLAG += -fconvert=big-endian	# (gfortran)
#DBGFLAG += -fconvert=native 	# (gfortran)
#DBGFLAG += -fconvert=swap	# (gfortran)

DBGFLAG += -Wall		# (gfortran) (RECOMMENDED)
DBGFLAG += -Wunderflow		# (gfortran) (RECOMMENDED)

#DBGFLAG += -fmem-report         # (gfortran) print stats about permanent memory allocation
#DBGFLAG += -dH                  # (gfortran) produce a core dump whenever an error occurs




# -------------------------------------------------
# Make targets
# -------------------------------------------------

.PHONY: clean all

all: *.o

%.o: %.f90
	$(FC) -c $(DBGFLAG) -J$(INCDIR) $+
	mv -i $@ $(INCDIR)/.

clean: 
	rm -f *.o *.mod
