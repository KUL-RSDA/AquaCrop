unit interface_initialsettings;


interface


procedure InitializeSettings(constref initialize_soil : boolean);
    external 'aquacrop' name '__ac_interface_initialsettings_MOD_initializesettings_wrap';


implementation


initialization


end.
