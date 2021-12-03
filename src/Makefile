PC = fpc
PCFLAGS = -Mdelphi

EXECUTABLE = PlugInBareV70
SRC = PlugInBareV70.dpr Global.pas Run.pas TempProcessing.pas \
			ClimProcessing.pas InitialSettings.pas StartUnit.pas \
			DefaultCropSoil.pas RootUnit.pas Simul.pas InfoResults.pas

$(EXECUTABLE): $(SRC)
	$(PC) $(PCFLAGS) PlugInBareV70.dpr 

clean:
	$(RM) $(wildcard *.o) $(wildcard *.ppu) $(EXECUTABLE)
