# Compiler options

DEBUG = 0

PC = fpc
PCFLAGS = -Mdelphi

FC = gfortran
FCFLAGS = -fPIC -shared

ifeq ($(DEBUG), 1)
    # For debugging and development
    PCFLAGS += -gl
    FCFLAGS += -g -fPIC -Wall -Wextra -Wimplicit-interface -fmax-errors=1 \
               -fcheck=all -fbacktrace -std=f2008
else
    # For production
    FCFLAGS += -O2 -march=native -funroll-loops
endif


# Default target

EXECUTABLE = PlugInBareV70
.DEFAULT_GOAL = $(EXECUTABLE)


# Fortran-related targets

FLIB = libaquacrop.so

$(FLIB): global.o \
    interface_global.o
	$(FC) $(FCFLAGS) $(wildcard *.f90) -o $(FLIB)

fortranclean:
	$(RM) $(wildcard *.mod) $(wildcard *.o) $(FLIB)

global.o: global.f90 \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_global.o: interface_global.f90 \
    global.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

kinds.o: kinds.f90
	$(FC) $(FCFLAGS) -c $< -o $@


# Pascal-related targets

SRC = PlugInBareV70.dpr Global.pas Run.pas TempProcessing.pas \
      ClimProcessing.pas InitialSettings.pas StartUnit.pas \
      DefaultCropSoil.pas RootUnit.pas Simul.pas \
      interface_global.pas

$(EXECUTABLE): $(SRC) $(FLIB)
	$(PC) $(PCFLAGS) PlugInBareV70.dpr

clean: fortranclean
	$(RM) $(wildcard *.o) $(wildcard *.ppu) $(EXECUTABLE)
