unit interface_initialsettings;

interface

uses Global, interface_global, DefaultCropSoil, interface_defaultcropsoil;


procedure InitializeSettings;
    external 'aquacrop' name '__ac_initialsettings_MOD_initializesettings';

implementation


initialization


end.

