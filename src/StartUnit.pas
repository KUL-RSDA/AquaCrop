unit StartUnit;

interface

USES Global, interface_global;

PROCEDURE GetProjectType(constref TheProjectFile : string;
                         VAR TheProjectType : repTypeProject);


implementation

USES SysUtils,InitialSettings,interface_initialsettings,Run,interface_run, interface_startunit;



PROCEDURE GetProjectType(constref TheProjectFile : string;
                         VAR TheProjectType : repTypeProject);
VAR i,lgth : INTEGER;
    TheExtension : string;

BEGIN
TheProjectType := TypeNone;
lgth := length(TheProjectFile);
IF (lgth > 0) THEN
   BEGIN
   i := 1;
   WHILE ((TheProjectFile[i] <> '.') AND (i < lgth)) DO i := i +1;
   IF (i = (lgth - 3)) THEN
      BEGIN
      TheExtension := COPY(TheProjectFile,(i+1),(i+3));
      FOR i := 1 to 3 do TheExtension[i] := UpCase(TheExtension[i]);
      IF (TheExtension = 'PRO')
         THEN TheProjectType := TypePRO
         ELSE BEGIN
              IF (TheExtension = 'PRM')
                 THEN TheProjectType := TypePRM
                 ELSE TheProjectType := TypeNone;
              END;
      END;
   END;
END; // GetProjectType


end.
