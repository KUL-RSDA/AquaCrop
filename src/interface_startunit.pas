unit interface_startunit;

interface

uses Global, interface_global, InitialSettings, interface_initialsettings, Run, interface_run;

procedure GetTimeAggregationResults(VAR OutputAggregate : ShortInt);
    external 'aquacrop' name '__ac_startunit_MOD_gettimeaggregationresults';


implementation


initialization


end.

