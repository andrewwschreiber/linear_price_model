# ------------------------------------------------------------------------------
# title: grab data on medicare and medicaid transfers
# ------------------------------------------------------------------------------

# set working directory
setwd("~/git/windc_build/data/household/health_care")

# set data directory
data.dir = getwd()

# install needed packages
list.of.packages <- 
  c("tidyverse","survey","tidycensus","foreach","dplyr")

new.packages <- 
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) 
  install.packages(new.packages, repos = "http://cran.rstudio.com/")

lapply(list.of.packages, library, character.only = TRUE)


# ------------------------------------------------------------------------------
# function for downloading files
# ------------------------------------------------------------------------------

download_file = function(url,file.name,dir=".",check.file=NA) {

  library(httr)

  # create the directory if needed
  if (!dir.exists(dir))
    dir.create(dir)

  # if no check file name is provided use the remote file name
  if (is.na(check.file))
    check.file = file.name

  # if the check file is present then do nothing further
  if (file.exists(file.path(dir,check.file)))
    return()

  # download the file
  cat(paste0("downloading ",file.name,"... \n"))
  GET(paste0(url,file.name),
      write_disk(file.path(dir,file.name),overwrite=TRUE),
      progress())

  # if the file is a zip archive extract the contents and delete the archive
  if (tools::file_ext(file.name)=="zip") {
    cat("extracting zip file...\n")
    unzip(file.path(dir,file.name),exdir=dir)
    file.remove(file.path(dir,file.name))
  }

  cat("\n")

}


# ------------------------------------------------------------------------------
# pull in health care expenditures from cms
# ------------------------------------------------------------------------------

url = "https://www.cms.gov/research-statistics-data-and-systems/statistics-trends-and-reports/nationalhealthexpenddata/downloads/"
file.name = "resident-state-estimates.zip"
download_file(url,paste0(file.name),dir=data.dir)

# use totals (Group = 1)
medicare_total = read.csv("MEDICARE_AGGREGATE20.CSV") %>%
  select(-Item, -Average_Annual_Percent_Growth) %>%
  filter(Code %in% 1) %>%
  pivot_longer(-c(Code,Group,Region_Number,Region_Name,State_Name), names_to="year", values_to="medicare") %>%
  filter(!(State_Name %in% "")) %>%
  select(State_Name,year,medicare)
  
medicare_numenr = read.csv("MEDICARE_ENROLLMENT20.CSV")
medicare_percap = read.csv("MEDICARE_PER_ENROLLEE20.CSV")

medicaid_total = read.csv("MEDICAID_AGGREGATE20.CSV") %>%
  select(-Item, -Average_Annual_Percent_Growth) %>%
  filter(Code %in% 1) %>%
  pivot_longer(-c(Code,Group,Region_Number,Region_Name,State_Name), names_to="year", values_to="medicaid") %>%
  filter(!(State_Name %in% "")) %>%
  select(State_Name,year,medicaid)

medicaid_numenr = read.csv("MEDICAID_ENROLLMENT20.CSV")
medicaid_percap = read.csv("MEDICAID_PER_ENROLLEE20.CSV")

# delete unzipped files
downloaded_files = list.files(pattern="\\.CSV$")
file.remove(downloaded_files)
file.remove("Documentation for the State Health Expenditure Data Files_Residence.docx")

# add medicaid and medicare together (keep needed data)
cms = left_join(medicare_total,medicaid_total) %>%
  mutate(year = as.numeric(str_replace(year,"Y",""))) %>%
  filter(year >= 2000)
  

# ------------------------------------------------------------------------------
# pull in acs data on medicare and mediaid enrollees
# ------------------------------------------------------------------------------

# census api key
# get one at: https://api.census.gov/data/key_signup.html
census_api_key("2e8d2ae236293d177891032824a43d16560b3bcf")

# list of state abbreviations plus DC
states = c(state.abb, "DC")

# define variables that exist for all years of data in years container
vars <- c(
    pub_ins_25 = "B27015_005",
    pub_ins_50 = "B27015_010",
    pub_ins_75 = "B27015_015",
    pub_ins_100 = "B27015_020",
    pub_ins_top = "B27015_025"
)

# see if available at the state level in single year files (beginning in 2009, 
# no data for 2020 due to low response rate)
geography <- "state"
years = c(2009:2019)
acs = data.frame()
for (y in years) {
  acs_loop =
    foreach (i=1:length(states), .combine=rbind,
             .packages=c("tidycensus", "tidyverse")) %do% {
               print(paste("starting state:", states[i]))
               get_acs(geography=geography,
                       state=states[i],
                       variables=vars,
                       year=y,
                       geometry=FALSE,
                       survey="acs1")
             }
  acs_loop$year = y
  acs = rbind(acs,acs_loop)
}

acs = acs %>% select(-moe)

# link states to abbreviations
state_fips = 
  c("01","02","04","05","06","08","09","10","11","12","13","15","16","17","18",
    "19","20","21","22","23","24","25","26","27","28","29","30","31","32","33",
    "34","35","36","37","38","39","40","41","42","44","45","46","47","48","49",
    "50","51","53","54","55","56","00")

state_abbr = 
  c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
    "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
    "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
    "VT","VA","WA","WV","WI","WY","US")

statemap = data.frame(fips = state_fips, state = state_abbr)

acs = acs %>% left_join(statemap, by=c("GEOID"="fips"))

# convert acs data to shares
acs = acs %>% select(NAME,state,year,variable,estimate) %>%
  pivot_wider(names_from=variable, values_from=estimate) %>%
  mutate(total = pub_ins_25 + pub_ins_50 + pub_ins_75 + pub_ins_100 + pub_ins_top,
         pub_ins_25 = pub_ins_25/total,
         pub_ins_50 = pub_ins_50/total,
         pub_ins_75 = pub_ins_75/total,
         pub_ins_100 = pub_ins_100/total,
         pub_ins_top = pub_ins_top/total) %>%
  select(-total)
         
# convert back to long format, linking medicare and medicaid data
acs = acs %>% 
  pivot_longer(-c(NAME,state,year), names_to="income", values_to="pub_ins_shares")

acs$income[acs$income=="pub_ins_25"] = "<25k"
acs$income[acs$income=="pub_ins_50"] = "25-50k"
acs$income[acs$income=="pub_ins_75"] = "50-75k"
acs$income[acs$income=="pub_ins_100"] = "75-100k"
acs$income[acs$income=="pub_ins_top"] = ">100k"

# assume benefits are equally distributed across households, convert to billions 
# of dollars
acs = acs %>%
  left_join(cms,by=c("NAME"="State_Name", "year"="year")) %>%
    mutate(medicare = 1e-3 * pub_ins_shares*medicare,
         medicaid = 1e-3 * pub_ins_shares*medicaid) %>%
  select(-NAME,-pub_ins_shares)

# check on totals by year
chktotals = acs %>%
  group_by(year) %>%
  summarize(medicare = sum(medicare),
            medicaid = sum(medicaid))


# ------------------------------------------------------------------------------
# save the data
# ------------------------------------------------------------------------------

write.csv(acs, paste0("public_health_benefits_",min(years),"_",max(years),".csv"), row.names=FALSE)


# ------------------------------------------------------------------------------
# end
# ------------------------------------------------------------------------------
