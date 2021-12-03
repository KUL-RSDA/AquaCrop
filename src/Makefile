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


EXECUTABLE = PlugInBareV70
SRC = PlugInBareV70.dpr Global.pas Run.pas TempProcessing.pas \
      ClimProcessing.pas InitialSettings.pas StartUnit.pas \
      DefaultCropSoil.pas RootUnit.pas Simul.pas \
      interface_global.pas

$(EXECUTABLE): $(SRC) $(FORTRANLIB)
	$(PC) $(PCFLAGS) PlugInBareV70.dpr

clean:
	$(RM) $(wildcard *.o) $(wildcard *.ppu) $(EXECUTABLE)



FLIB = libaquacrop.so

flib:
	$(FC) $(FCFLAGS) $(wildcard *.f90) -o $(FLIB)

fclean:
	$(RM) $(wildcard *.mod) $(FLIB)
