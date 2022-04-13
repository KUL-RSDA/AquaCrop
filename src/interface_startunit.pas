unit interface_startunit;

interface

uses Global, interface_global, InitialSettings, interface_initialsettings, Run, interface_run;


procedure GetTimeAggregationResults();
    external 'aquacrop' name '__ac_startunit_MOD_gettimeaggregationresults';

procedure GetRequestDailyResults();
    external 'aquacrop' name '__ac_startunit_MOD_getrequestdailyresults';

procedure GetRequestParticularResults();
    external 'aquacrop' name '__ac_startunit_MOD_getrequestparticularresults';

procedure fProjects_open(constref filename : string; constref mode : string);

procedure fProjects_open_wrap(
            constref filename_ptr : PChar;
            constref filename_len : integer;
            constref mode_ptr : PChar;
            constref mode_len : integer);
        external 'aquacrop' name '__ac_interface_startunit_MOD_fprojects_open_wrap';

procedure fProjects_write(constref line : string; constref advance : boolean = True);

procedure fProjects_write_wrap(
            constref line_ptr : PChar;
            constref line_len : integer;
            constref advance : boolean);
        external 'aquacrop' name '__ac_interface_startunit_MOD_fprojects_write_wrap';

procedure fProjects_close();
        external 'aquacrop' name '__ac_startunit_MOD_fprojects_close';


implementation

procedure fProjects_open(constref filename : string; constref mode : string);
var
     filename_ptr, mode_ptr : PChar;
     filename_len, mode_len : integer;
begin;
     filename_ptr := PChar(filename);
     filename_len := Length(filename);
     mode_ptr := PChar(mode);
     mode_len := Length(mode);
     fProjects_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len);
end;

procedure fProjects_write(constref line : string; constref advance : boolean = True);
var
     line_ptr : PChar;
     line_len : integer;
begin;
     line_ptr := PChar(line);
     line_len := Length(line);
     fProjects_write_wrap(line_ptr, line_len, advance);
end;



initialization


end.

