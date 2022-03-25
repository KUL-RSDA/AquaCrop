# Compiler options

DEBUG = 0
FORTRAN_EXE = 0

PC = fpc
PCFLAGS = -Mdelphi

FC = gfortran
FCFLAGS = -fPIC -shared

ifeq ($(DEBUG), 1)
    # For debugging and development
    PCFLAGS += -gl -fPIC
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
    interface_global.o \
    interface_tempprocessing.o \
    interface_climprocessing.o \
    interface_initialsettings.o \
    interface_run.o \
    interface_startunit.o \
    interface_defaultcropsoil.o \
    interface_inforesults.o \
    interface_rootunit.o \
    interface_simul.o  
	$(FC) $(FCFLAGS) $(wildcard *.f90) -o $(FLIB)

fortranclean:
	$(RM) $(wildcard *.mod) $(wildcard *.o) $(FLIB)

kinds.o: kinds.f90
	$(FC) $(FCFLAGS) -c $< -o $@

global.o: global.f90 \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_global.o: interface_global.f90 \
    global.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

tempprocessing.o: tempprocessing.f90 \
    global.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_tempprocessing.o: interface_tempprocessing.f90 \
    tempprocessing.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

climprocessing.o: climprocessing.f90 \
    global.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_climprocessing.o: interface_climprocessing.f90 \
    climprocessing.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

initialsettings.o: initialsettings.f90 \
    global.o \
    defaultcropsoil.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_initialsettings.o: interface_initialsettings.f90 \
    initialsettings.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

run.o: run.f90 \
    global.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_run.o: interface_run.f90 \
    run.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

startunit.o: startunit.f90 \
    global.o \
    initialsettings.o \
    run.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_startunit.o: interface_startunit.f90 \
    startunit.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

defaultcropsoil.o: defaultcropsoil.f90 \
    global.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_defaultcropsoil.o: interface_defaultcropsoil.f90 \
    defaultcropsoil.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

inforesults.o: inforesults.f90 \
    global.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_inforesults.o: interface_inforesults.f90 \
    inforesults.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

rootunit.o: rootunit.f90 \
    global.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_rootunit.o: interface_rootunit.f90 \
    rootunit.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

simul.o: simul.f90 \
    global.o \
    tempprocessing.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_simul.o: interface_simul.f90 \
    simul.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

aquacrop_wrap.o: aquacrop_wrap.F90 \
    interface_global.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

pluginbarev70.o: pluginbarev70.F90 \
    aquacrop_wrap.o \
    global.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@


# Pascal-related targets

SRC = PlugInBareV70.dpr Global.pas Run.pas TempProcessing.pas \
      ClimProcessing.pas InitialSettings.pas StartUnit.pas \
      DefaultCropSoil.pas RootUnit.pas Simul.pas \
      interface_global.pas interface_tempprocessing.pas \
      interface_climprocessing.pas interface_initialsettings.pas \
      interface_run.pas interface_startunit.pas interface_defaultcropsoil.pas \
      interface_inforesults.pas interface_rootunit.pas interface_simul.pas

PLIB = libPlugInBareV70.so

$(PLIB): $(SRC) $(FLIB) PlugInBareV70.pas
	$(PC) $(PCFLAGS) PlugInBareV70.pas

pascalclean: fortranclean
	$(RM) $(wildcard *.o) $(wildcard *.ppu) $(PLIB)


# Finally, the actual 'executable' and 'clean' targets

ifeq ($(FORTRAN_EXE), 0)
# Pascal-based executable
$(EXECUTABLE): $(SRC) $(FLIB)
	$(PC) $(PCFLAGS) PlugInBareV70.dpr

else ifeq ($(FORTRAN_EXE), 1)
# Fortran-based executable
$(EXECUTABLE): pluginbarev70.o $(PLIB)
	$(FC) pluginbarev70.o aquacrop_wrap.o -o $(EXECUTABLE) -L$(PWD) -laquacrop -lPlugInBareV70
endif

clean: pascalclean fortranclean
	$(RM) $(EXECUTABLE)
