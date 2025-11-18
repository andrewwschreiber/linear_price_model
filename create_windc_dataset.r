# create_windc_dataset.r
#
# produces the core benchmark dataset used as the basis for the sage model
#
# intended to be run from the top level directory of the model

setwd("~/git/windc_in_sage")

# ------------------------------------------------------------------------------
# options
# ------------------------------------------------------------------------------

# sage model base year
sage.year = 2022

# number of households based on quantiles (default to quintiles or 5 households 
# per region, binned by population and income)
sage.households = 5


# ------------------------------------------------------------------------------
# run the included windc distribution to produce household datasets
# ------------------------------------------------------------------------------

# construct windc database
cat("constructing windc accounts...\n")

# temporarily shift working directory to windc folders
save_wd = getwd()

# construct the core windc accounts [core accounts are built for all years from 
# 1997-2022]
setwd(paste0(save_wd,"/core"))
system('gams build.gms')

# download cps data and reconcile to the chosen level of disaggregation based on
# sage.households choice. code determines absolute income thresholds based on
# cbo definition of income
setwd(paste0(save_wd,"/data/household/cps"))
source("get_cps_quintiles.r")

# construct household accounts for sage.year
setwd(paste0(save_wd,"/household"))
system(paste0('gams build.gms --year=',sage.year,
              ' --hhdata=cps --invest=static --capital_ownership=all',
              ' --rmap=state --smap=windc'))



# ------------------------------------------------------------------------------
# end
# ------------------------------------------------------------------------------
