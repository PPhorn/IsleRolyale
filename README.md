# IsleRolyale

To run the application through the console do the following:

1. Compile the application library by running the following command: fsharpc -a animalsSmall.fsi animalsSmall.fs
2. Next compile the .dll and .fsx file by running the following command: fsharpc -r animalsSmall.dll testIsleRoyale.fsx
3. Finally run the application by the command: mono testIsleRoyale.exe either with or without the self defined application parameters. If the user chooses to not apply any values when running the application, the default simulation parameters and filename is set to: mono testIsleRoyale.exe 40 test.txt 10 30 10 2 10 4
