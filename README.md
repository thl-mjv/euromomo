euromomo
========

Developement branch for the EuroMoMo hackathon

Installing
==========

If you just want to install the file, use

    library("devtools") # make sure you have this package installed first
    install_github("thl-mjv/euromomo")
    
Using & Testing
===============

1. Install the package 
2. Create defaults.txt using [this](https://github.com/thl-mjv/euromomo/blob/master/defaults-example.txt) as an example. Most important bits are
 1. Nomenclature (__Country__, __Counties__, __Institution__). These will affect the outputs
 2. __WorkDirectory__: The outputs will be created in subdirectories of this directory. It may be advisable to use absolute path. If ".", the directory where R is run is used.
 3. __InputFile__: The mortality data with variable __DoD__, __DoR__ and __age__ as well as whatever you may want to include. First line must have the variable names. Use ";" as separator. Date format must be "YYYY-MM-DD". Use either full path or path relatively to directory where R is run. 
 4. __HolidayFile__: The bank holidays file similar to __InputFile__ with variables __date__ and __closed__, latter being 1 if full bank holiday.
3. Create master momo file using [this](https://github.com/thl-mjv/euromomo/blob/master/dev/momomaster.R) as an example and run. Usually only thing you need to change is the path to "defaults.txt" in __parseDefaultsFile("defaults.txt")__. 
4. Send the outputs to
    
    
Developing
==========

See [developing guidelines](Documents/Developing.md)
