# Testcase

This folder contains an AquaCrop testcase that you may use 
to verify the correct execution of the precompiled static AquaCrop 
executable on your hardware. Note that only x86_64 CPU architectures 
are supported. The reference output (./OUTP_REF) was generated with 
AquaCropV7.3.

## Running the testcase
To run the test, copy the executable to the present directory and
follow the instructions below for your operating system:

* **Linux**:

  Open a terminal, change to the present directory and execute
  "./aquacrop".

* **MacOS**:

  Open a terminal, change to the present directory and execute
  "./aquacrop". If you receive a security alert upon runtime, then locate
  the AquaCrop executable in your Finder, CTRL+click on the executable
  and click on "Open". Then, execute "./aquacrop".

* **Windows**:

  You can either:
  - Open a Windows Powershell or CMD shell, change to the present
    directory and execute ".\aquacrop.exe",
  - Or double-click on the executable in the present directory.
    An empty CMD window may appear while AquaCrop is running.

  Note 1: end of file (EoF) encoding is different for Windows compared
  to Linux/MacOS systems. The input files are configured for 
  Linux/MacOS and should therefore be converted when running the
  Windows executable. This can be done in e.g. Notepad++.

  Note 2: forward slashes in the Ottawa.PRM file should be changed
  to back slashes (\) to run AquaCrop in a Windows environment.


The calculation should finish after a few seconds.

## Verifying the output
Except for the first lines with the output creation dates, the resulting
output files in the "OUTP" directory should be identical to the reference
output files included in "OUTP_REF". You can check the differences by
opening a terminal and executing the following commands:

* **Linux and MacOS**:

        diff OUTP/AllDone.OUT OUTP_REF/AllDone.OUT
        diff OUTP/ListProjectsLoaded.OUT OUTP_REF/ListProjectsLoaded.OUT
        diff OUTP/xxx.OUT OUTP_REF/xxx.OUT
        ...

* **Windows**:

        fc.exe OUTP\AllDone.OUT OUTP_REF\AllDone.OUT
        fc.exe OUTP\ListProjectsLoaded.OUT OUTP_REF\ListProjectsLoaded.OUT
        fc.exe OUTP\xxx.OUT OUTP_REF\xxx.OUT
        ...

Where xxx represents any output (.OUT) file.
