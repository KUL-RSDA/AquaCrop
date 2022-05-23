unit interface_startunit;


interface


uses interface_global;


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

procedure PrepareReport();
    external 'aquacrop' name '__ac_startunit_MOD_preparereport';

procedure StartTheProgram();
    external 'aquacrop' name '__ac_startunit_MOD_starttheprogram';

procedure InitializeTheProgram();
    external 'aquacrop' name '__ac_startunit_MOD_initializetheprogram';

function GetListProjectsFile() : string;

function GetListProjectsFile_wrap() : PChar;
    external 'aquacrop' name '__ac_interface_startunit_MOD_getlistprojectsfile_wrap';

function GetNumberOfProjects() : integer;
    external 'aquacrop' name '__ac_startunit_MOD_getnumberofprojects';

function GetProjectFileName(constref iproject : integer) : string;

function GetProjectFileName_wrap(constref iproject : integer) : PChar;
    external 'aquacrop' name '__ac_interface_startunit_MOD_getprojectfilename_wrap';


procedure InitializeProject(constref iproject : integer;
                            constref TheProjectFile : string;
                            constref TheProjectType : repTypeProject);

procedure InitializeProject_wrap(constref iproject : integer;
                            constref TheProjectFile : PChar;
                            constref strlen : integer;
                            constref TheProjectType : integer);
        external 'aquacrop' name '__ac_interface_startunit_MOD_initializeproject_wrap';

procedure FinalizeTheProgram;
        external 'aquacrop' name '__ac_startunit_MOD_finalizetheprogram';

procedure WriteProjectsInfo(constref line : string);

procedure WriteProjectsInfo_wrap(constref p : PChar;
                            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_startunit_MOD_writeprojectsinfo_wrap';

procedure GetProjectType(constref TheProjectFile : string;
                         VAR TheProjectType : repTypeProject);

procedure GetProjectType_wrap(constref TheProjectFile : PChar;
                              constref strlen : integer;
                              VAR TheProjectType : integer);
        external 'aquacrop' name '__ac_interface_startunit_MOD_getprojecttype_wrap';

implementation


function GetListProjectsFile() : string;
var
    p : PChar;
begin
    p := GetListProjectsFile_wrap();
    GetListProjectsFile := Ansistring(p);
end;

function GetProjectFilename(constref iproject : integer) : string;
var
    p : PChar;
begin
    p := GetProjectFilename_wrap(iproject);
    GetProjectFileName := Ansistring(p);
end;


procedure WriteProjectsInfo(constref line : string);
var
    p : PChar;
    strlen : integer;
begin
    p := PChar(line);
    strlen := Length(line);
    WriteProjectsInfo_wrap(p, strlen);
end;

procedure GetProjectType(constref TheProjectFile: string;
                         VAR TheProjectType : repTypeProject);
var
    p : PChar;
    strlen : integer;
    int_projecttype : integer;

begin
    p := PChar(TheProjectFile);
    strlen := Length(TheProjectFile);
    int_projecttype := ord(TheProjectType);
    GetProjectType_wrap(p,strlen, int_projecttype);
    TheProjectType := repTypeProject(int_projecttype);
end;


procedure InitializeProject(constref iproject : integer;
                            constref TheProjectFile : string;
                            constref TheProjectType : repTypeProject);
var
    int_projecttype : integer;
    p : PChar;
    strlen : integer;
begin
    int_projecttype := ord(TheProjectType);
    p := PChar(TheProjectFile);
    strlen := Length(TheProjectFile);
    InitializeProject_wrap(iproject, p, strlen, int_projecttype);
end;


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
