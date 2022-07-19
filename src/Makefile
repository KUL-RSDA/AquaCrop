# Compiler options

DEBUG = 0
FORTRAN_EXE = 1
STATIC = 0

PC = fpc
PCFLAGS = -Mdelphi

FC = gfortran
FCFLAGS =
LDFLAGS_EXE =
LDFLAGS_LIB =

ifeq ($(STATIC), 1)
    ifneq ($(DEBUG), 0)
        $(error DEBUG=0 is required for STATIC=1 build)
    endif
    ifneq ($(FORTRAN_EXE), 1)
        $(error FORTRAN_EXE=1 is required for STATIC=1 build)
    endif
endif

ifeq ($(FC), gfortran)
    ifeq ($(STATIC), 1)
        # For statically compiled (release) executables
        FCFLAGS += -O2 -mtune=generic -funroll-loops
        LDFLAGS_EXE += -static
        LDFLAGS_LIB += -static
    else
        FCFLAGS += -fPIC -fall-intrinsics
        LDFLAGS_LIB += -shared

        ifeq ($(DEBUG), 1)
            # For debugging and development
            PCFLAGS += -gl
            FCFLAGS += -g -Wall -Wextra -Wimplicit-interface -fmax-errors=1 \
                       -fcheck=all -fbacktrace -std=f2008
        else
            # For production
            FCFLAGS += -O2 -march=native -funroll-loops
        endif
    endif
else ifeq ($(FC), ifort)
    ifneq ($(FORTRAN_EXE), 1)
        $(error FORTRAN_EXE=1 is required for FC=ifort)
    endif
    ifneq ($(STATIC), 0)
        $(error STATIC=0 is required for FC=ifort)
    endif

    LDFLAGS_LIB += -shared
    FCFLAGS += -fPIC

    ifeq ($(DEBUG), 1)
        # For debugging and development
        FCFLAGS += -g -fpe0 -warn -traceback -debug extended -stand f08 \
                   -check all -check noarg_temp_created
    else
        # For production
        FCFLAGS += -O2 -fp-model strict -xHost
    endif
endif


# Main targets

EXECUTABLE = aquacrop
STATIC_LIBRARY = libaquacrop.a
SHARED_LIBRARY = libaquacrop.so
ifeq ($(STATIC), 1)
    LIBRARY = $(STATIC_LIBRARY)
else
    LIBRARY = $(SHARED_LIBRARY)
endif

bin: $(EXECUTABLE)
lib: $(LIBRARY)
all: bin lib
.DEFAULT_GOAL = all


# Fortran-related targets

OBJ = climprocessing.o \
    defaultcropsoil.o \
    global.o  \
    inforesults.o \
    initialsettings.o \
    kinds.o \
    project_input.o \
    rootunit.o \
    run.o \
    simul.o \
    startunit.o \
    tempprocessing.o \
    utils.o

OBJ_INTERFACE = interface_climprocessing.o \
    interface_defaultcropsoil.o \
    interface_global.o \
    interface_inforesults.o \
    interface_initialsettings.o \
    interface_project_input.o \
    interface_rootunit.o \
    interface_run.o \
    interface_simul.o \
    interface_startunit.o \
    interface_tempprocessing.o

$(STATIC_LIBRARY): $(OBJ) \
    $(OBJ_INTERFACE)
	$(AR) cr $(STATIC_LIBRARY) $(OBJ) $(OBJ_INTERFACE)

$(SHARED_LIBRARY): $(OBJ) \
    $(OBJ_INTERFACE)
	$(FC) $(LDFLAGS_LIB) $(FCFLAGS) $(wildcard *.f90) -o $(LIBRARY)

fortranclean:
	$(RM) $(wildcard *.mod) $(wildcard *.o) $(STATIC_LIBRARY) $(SHARED_LIBRARY)


aquacrop.o: aquacrop.F90 \
    startunit.o
	$(FC) $(FCFLAGS) -c $< -o $@

climprocessing.o: climprocessing.f90 \
    global.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

defaultcropsoil.o: defaultcropsoil.f90 \
    global.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

global.o: global.f90 \
    kinds.o \
    project_input.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

inforesults.o: inforesults.f90 \
    global.o \
    kinds.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

initialsettings.o: initialsettings.f90 \
    global.o \
    defaultcropsoil.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

kinds.o: kinds.f90
	$(FC) $(FCFLAGS) -c $< -o $@

project_input.o: project_input.f90 \
    kinds.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

rootunit.o: rootunit.f90 \
    global.o \
    kinds.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

run.o: run.f90 \
    global.o \
    inforesults.o \
    kinds.o \
    rootunit.o \
    simul.o \
    tempprocessing.o \
    climprocessing.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

simul.o: simul.f90 \
    global.o \
    tempprocessing.o \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@

startunit.o: startunit.f90 \
    global.o \
    initialsettings.o \
    kinds.o \
    run.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

tempprocessing.o: tempprocessing.f90 \
    global.o \
    kinds.o \
    project_input.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

utils.o: utils.f90 \
    kinds.o
	$(FC) $(FCFLAGS) -c $< -o $@


# Interface Fortran code: always adding the parent module and the kinds and
# utils modules as dependencies for convenience, even if not actually used.

interface_climprocessing.o: interface_climprocessing.f90 \
    climprocessing.o \
    kinds.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_defaultcropsoil.o: interface_defaultcropsoil.f90 \
    defaultcropsoil.o \
    kinds.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_global.o: interface_global.f90 \
    global.o \
    kinds.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_inforesults.o: interface_inforesults.f90 \
    inforesults.o \
    kinds.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_initialsettings.o: interface_initialsettings.f90 \
    initialsettings.o \
    kinds.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_project_input.o: interface_project_input.f90 \
    kinds.o \
    project_input.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_rootunit.o: interface_rootunit.f90 \
    kinds.o \
    rootunit.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_run.o: interface_run.f90 \
    kinds.o \
    run.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_simul.o: interface_simul.f90 \
    kinds.o \
    simul.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_startunit.o: interface_startunit.f90 \
    kinds.o \
    startunit.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@

interface_tempprocessing.o: interface_tempprocessing.f90 \
    global.o \
    kinds.o \
    tempprocessing.o \
    utils.o
	$(FC) $(FCFLAGS) -c $< -o $@


# Pascal-related targets

SRC = Global.pas \
      interface_climprocessing.pas \
      interface_defaultcropsoil.pas \
      interface_global.pas \
      interface_inforesults.pas \
      interface_initialsettings.pas \
      interface_project_input.pas \
      interface_rootunit.pas \
      interface_run.pas \
      interface_simul.pas \
      interface_startunit.pas \
      interface_tempprocessing.pas \
      aquacrop.dpr

pascalclean: fortranclean
	$(RM) $(wildcard *.o) $(wildcard *.ppu)


# Finally, the actual 'executable' and 'clean' targets

ifeq ($(FORTRAN_EXE), 0)
# Pascal-based executable
$(EXECUTABLE): $(SRC) $(LIBRARY)
	$(PC) $(PCFLAGS) aquacrop.dpr

else ifeq ($(FORTRAN_EXE), 1)
# Fortran-based executable
$(EXECUTABLE): aquacrop.o
	$(FC) $(LDFLAGS_EXE) aquacrop.o $(OBJ) -o $(EXECUTABLE)
endif

clean: pascalclean fortranclean
	$(RM) $(EXECUTABLE)
