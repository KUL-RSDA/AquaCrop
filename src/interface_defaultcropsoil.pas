unit interface_defaultcropsoil;


interface


procedure ResetDefaultCrop;
    external 'aquacrop' name '__ac_defaultcropsoil_MOD_resetdefaultcrop';

procedure ResetDefaultSoil(constref use_default_soil_file : boolean);
    external 'aquacrop' name '__ac_interface_defaultcropsoil_MOD_resetdefaultsoil_wrap';


implementation


initialization


end.
