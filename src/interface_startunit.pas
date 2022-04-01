unit interface_startunit;

interface

uses Global, interface_global, InitialSettings, interface_initialsettings, Run, interface_run;

procedure GetRequestDailyResults(VAR Out1Wabal : boolean; 
                                 VAR Out2Cro : boolean; 
                                 VAR Out3Prof : boolean;
                                 VAR Out4Salt : boolean;
                                 VAR Out5CompWC : boolean;
                                 VAR Out6CompEC : boolean;
                                 VAR Out7Clim : boolean;
                                 VAR OutDaily : boolean);
    external 'aquacrop' name '__ac_interface_startunit_MOD_getrequestdailyresults_wrap';

procedure GetRequestParticularResults(VAR Part1Mult : boolean;
                                      VAR Part2Eval : boolean);
    external 'aquacrop' name '__ac_interface_startunit_MOD_getrequestparticularresults_wrap';


implementation


initialization


end.

