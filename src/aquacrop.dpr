program aquacrop;

uses
  interface_climprocessing in 'interface_climprocessing.pas',
  interface_defaultcropsoil in 'interface_defaultcropsoil.pas',
  interface_global in 'interface_global.pas',
  interface_inforesults in 'interface_inforesults.pas',
  interface_initialsettings in 'interface_initialsettings.pas',
  interface_rootunit in 'interface_rootunit.pas',
  interface_run in 'interface_run.pas',
  interface_simul in 'interface_simul.pas',
  interface_startunit in 'interface_startunit.pas',
  interface_tempprocessing in 'interface_tempprocessing.pas';


// NOTE: only "StartTheProgram" from "interface_startunit" is needed here,
//       but the other interfaces are imported as well to ensure that they
//       get compiled as well.


begin
  StartTheProgram;
end.
