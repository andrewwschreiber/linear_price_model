# Overview

This code produces self-consistent state-level input output tables of the US economy from 2001-2022 using a modified version of code from the Wisconsin National Data Consortium ([WiNDC](https://windc.wisc.edu)). The construction process is segmented into two workstreams:

1. generate the "core" economic accounts based on a single representative agent by region and summary-sector aggregation from the Bureau of Economic Analysis, and

2. disaggregate the representative agent into household income quintiles based on several underlying data sources.

The code is designed to produce a flexible disaggregation of household income groups. The user selects the base year of their model and how many quantiles they would like to reflect in the household disaggregation build in "create_windc_dataset.r". The default is set to model pure quintiles based on CBO's definition of income (market income plus social insurance benefits -- this definition does not income means-tested transfers and tax payments).

To run the code, point command line to the root directory and run the following command:

'Rscript create_windc_dataset.r'

# Transfer to CGE directory:

Following the completion of "create_windc_dataset.r", copy the following files into CGE/build/data:
- calibrated household-level windc data for sage.year (household/datasets/*.gdx)
- household income thresholds (data/household/cps/household_income_bins_2016_2022.csv)

# References

Rutherford, T. F., & Schreiber, A. (2019). Tools for open source, subnational CGE modeling with an illustrative analysis of carbon leakage. Journal of Global Economic Analysis, 4(2), 10-21642.

## WiNDC Build Stream

The following links point to the documentation for each modules of
the build stream. Be sure to read the [Data](#data) and 
[Build Instructions](#build-instructions) sections to acquire
and build the data.

- [Data](#data)
- [Build Instructions](#build-instructions)
- [Core](core/README.md)
- [Household](household/README.md)


### Data

The data required to build each of piece of the repository is located at 
the following link [windc_2021.zip (277 MB)](https://windc.wisc.edu/downloads/version_4_0/windc_2021.zip). 
Download and extract `windc_2021.zip` into the `windc_build` directory. This will 
create a directory called `data`.

Data is included to run `core`, `household`, and `gtap9`. In order to run 
`gtap11` or `gtap11a` you need to obtain a license from GTAP. The data for 
GTAPWiNDC for the year 2017 is propriety to GTAP and requires a GTAP license. 
In order to enable gtap11/a you **must modify the file `GTAPWiNDC/gtapingams.gms`**
in the buildstream. Instructions are provided in that file.

The zip file should have the correct directory structure, but the following 
is the correct naming scheme:

```
|data
|-- core
|  |   windc_base.gdx
|-- household
|  |  |-- cex
|  |  |-- cps
|  |  |-- soi
```

### Build Instructions

The build stream must be run in a particular order as subsequent modules 
depend on previously built data. As convention, each module has a file 
`build.gms` that runs all essential methods and builds necessary data. 
Each modules may have other options, this is detailed on their documentation page. 

The correct order is: 

1. `core`
2. `household`


### General Instructions
Use the terminal/command line to navigate to the directory you wish to 
build, we'll use `core` as an example, and run the command:

    gams build.gms

Each of the modules have optional command line options. In `core` 
calibrating using the Huber methods is optional. To build using Huber
run the command:

    gams build.gms --huber=yes

Command line options for every file in each module are detailed on the
modules documentation page.
