# IsleRolyale

To run the application through the console do the following:

1. Compile the application library by running the following in the terminal/command prompt: fsharpc -a animalsSmall.fsi animalsSmall.fs
2. Next compile the .dll and .fsx file by running the following in the terminal/command prompt: fsharpc -r animalsSmall.dll simulateIsleRoyale.fsx
3. Finally run the application by issuing the following in the terminal/command prompt: mono simulateIsleRoyale.exe either with or without the self defined application parameters. If the user chooses to not apply any values when running the application, the default simulation parameters and filename is set to: mono simulateIsleRoyale.exe 40 test.txt 10 30 10 2 10 4

After running the application with the mono command, the user will find a new local .csv file in the application folder holding the population data of the most recent simulation of Isle Royale. If the user chose to not give any self defined input values, he/she can create a plot of the simulation data by compiling a .pdf from the file plot_isleRoyale.tex.

To create a data plot in .pdf through the console write “pdflatex plot_isleRoyale.tex” in the terminal/command prompt.
