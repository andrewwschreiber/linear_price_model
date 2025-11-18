# ------------------------------------------------------------------------------
# title: grab time series of cps household data
# ------------------------------------------------------------------------------

# install needed packages
list.of.packages <- 
  c("tidyverse","cpsR","survey","bea.R","reldist","httr2","progress")

new.packages <- 
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) 
  install.packages(new.packages, repos = "http://cran.rstudio.com/")

lapply(list.of.packages, library, character.only = TRUE)

# cps and bea api keys
cps.key = "a69b1c9f95e30d420fcef94d191d649a340ca7f3"
bea.key = "12C8FE9E-DF75-4515-B170-296AB4E9AB93"



# ------------------------------------------------------------------------------
# pull in cps data
# ------------------------------------------------------------------------------

# years of needed data (asec does not exist prior to 2001. estimates from 
# datasets between 2001 and 2004 are estimated.)
years = c(2016,sage.year)

# income variables pre 2019 (retirement income variables redefined in 2019)
cps.vars.pre2019 = c("hwsval",  # "wages and salaries"
                     "hseval",  # "self-employment (nonfarm)"
                     "hfrval",  # "self-employment farm"
                     "hucval",  # "unemployment compensation"
                     "hwcval",  # "workers compensation"
                     "hssval",  # "social security"
                     "hssival", # "supplemental security"
                     "hpawval", # "public assistance or welfare"
                     "hvetval", # "veterans benefits"
                     "hsurval", # "survivors income"
                     "hdisval", # "disability"
                     "hretval", # "retirement income"
                     "hintval", # "interest"
                     "hdivval", # "dividends"
                     "hrntval", # "rents"
                     "hedval",  # "educational assistance"
                     "hcspval", # "child support"
                     "hfinval", # "financial assistance"
                     "hoival",  # "other income"
                     "htotval") # "total household income

#          "hcspval","hfinval","hoival"

# income variables post 2019 (retirement income variables redefined in 2019)
cps.vars.post2019 = c("hwsval",  # "wages and salaries"
                      "hseval",  # "self-employment (nonfarm)"
                      "hfrval",  # "self-employment farm"
                      "hucval",  # "unemployment compensation"
                      "hwcval",  # "workers compensation"
                      "hssval",  # "social security"
                      "hssival", # "supplemental security"
                      "hpawval", # "public assistance or welfare"
                      "hvetval", # "veterans benefits"
                      "hsurval", # "survivors income"
                      "hdisval", # "disability"
                      "hdstval", # "retirement distributions"
                      "hpenval", # "pension income"
                      "hannval", # "annuities"
                      "hintval", # "interest"
                      "hdivval", # "dividends"
                      "hrntval", # "rents"
                      "hedval",  # "educational assistance"
                      "hcspval", # "child support"
                      "hfinval", # "financial assistance"
                      "hoival",  # "other income"
                      "htotval") # "total household income
             
# added variables for weighting
cps.rw =   c("gestfips", # state fips
             "a_exprrp", # expanded relationship code
             "h_hhtype", # type of household interview
             "pppos",    # person identifier
             "marsupwt") # asec supplement final weight


# return the total reported income and share of total by income quantile for the
# specified year
get.shares = function(year) {

  # download and process the cps march supplement for the year after requested
  # since income questions on the supplement are for the previous year
  if (year < 2018) {
    cpsasec <- get_asec(year+1, vars = c(cps.vars.pre2019,cps.rw), key = cps.key, tibble=FALSE)
  } else {
    cpsasec <- get_asec(year+1, vars = c(cps.vars.post2019,cps.rw), key = cps.key, tibble=FALSE)
  }

  # add column for aggregate retirement distributions to align datasets
  # if (year >= 2018) {
  #  cpsasec$hretval = cpsasec$hdstval + cpsasec$hpenval + cpsasec$hannval
  #  cps.vars.post2019.add = c(cps.vars.post2019,"hretval")
  # }
  cps.vars.post2019.add = cps.vars.post2019

  # extract the household file with representative persons
  cpsasec = cpsasec[cpsasec$a_exprrp %in% c(1,2) & cpsasec$h_hhtype==1,]

  # extract the household file with representative persons
  cpsasec = cpsasec[cpsasec$pppos==41,]
  
  # generate variable based on cbo definition of income (size adjusted income 
  # before taxes and transfers), which is defined as:
  #   income before taxes and transfers
  #   = 
  #   market income:  labor income + business income + capital income (including capital gains)
  #                 + income received in retirement for past services
  #                 + income from other nongovernmental sources
  #   +
  #   social insurance benefits: 
  #                   social security + medicare + unemployment insurance
  #                 + workers compensation
  # 
  # N.B.: this definition of income does not include means-tested transfers 
  # (medicaid, snap, supplemental social insurance, housing assistance), and 
  # tax payments.
  # 
  # using just cps data, we will not capture some of these components (capital
  # gains, medicare)
  if (year < 2018) {
    cpsasec$cbo_income = 
      cpsasec$hwsval +  # wages and salaries
      cpsasec$hseval +  # self-employment (nonfarm)
      cpsasec$hfrval +  # self-employment farm
      cpsasec$hretval + # retirement income
      cpsasec$hintval + # interest
      cpsasec$hdivval + # dividends
      cpsasec$hrntval + # rents
      cpsasec$hoival +  # other income
      cpsasec$hssval +  # social security
      cpsasec$hucval +  # unemployment compensation
      cpsasec$hwcval    # workers compensation
  } else {
    cpsasec$cbo_income = 
      cpsasec$hwsval +  # wages and salaries
      cpsasec$hseval +  # self-employment (nonfarm)
      cpsasec$hfrval +  # self-employment farm
      cpsasec$hdstval + # retirement distributions
      cpsasec$hpenval + # pension income
      cpsasec$hannval + # annuities
      cpsasec$hintval + # interest
      cpsasec$hdivval + # dividends
      cpsasec$hrntval + # rents
      cpsasec$hoival +  # other income
      cpsasec$hssval +  # social security
      cpsasec$hucval +  # unemployment compensation
      cpsasec$hwcval    # workers compensation
  }
  
  # compute quantiles
  quantiles = seq(1/sage.households,(sage.households-1)/sage.households,
                  by=1/sage.households)
  bounds = wtd.quantile(cpsasec$cbo_income, q=quantiles, weight=cpsasec$marsupwt)
    
  # add household label to each entry
  upper_bounds = c(bounds,Inf)
  bounds <- c(-Inf,bounds,Inf)
  for (i in 1:length(bounds))
    cpsasec$hh[cpsasec$cbo_income>bounds[i] &
                 cpsasec$cbo_income<=bounds[i+1]] = paste0("hh",i)

  # save household label to upper bound mapping for tax rate calculations
  hh.inc = data.frame(value = upper_bounds)
  for (i in 1:length(upper_bounds))
    hh.inc$label[i] = paste0("hh",i)
  hh.inc$year = year
  
  # remove cbo income estimate
  cpsasec$cbo_income = NULL
  
  # scale income levels by the household weight
  if (year < 2018) {
    for (source in cps.vars.pre2019)
      cpsasec[,source] = cpsasec[,source]*cpsasec$marsupwt
  } else {
    for (source in cps.vars.post2019.add)
      cpsasec[,source] = cpsasec[,source]*cpsasec$marsupwt
  }

  # count observations in sample by quantile and state fips
  count <- cpsasec[,c("gestfips","hh")] %>% count(hh,gestfips)
  names(count)[2] <- "state"
  nat_count <- cpsasec[,c("gestfips","hh")] %>% count(hh)
  nat_count$state <- 0
  nat_count <- nat_count[,names(count)]
  count <- rbind(nat_count,count)

  # report number of households in millions by quantile and state fips
  numhh <- cpsasec[,c("gestfips","hh","marsupwt")] %>% group_by(gestfips,hh) %>%
    summarize(numhh = sum(marsupwt,na.rm=TRUE)*1e-6)
  names(numhh)[1] <- "state"

  # aggregate income by quantile and state fips
  if (year < 2018) {
  income = aggregate(
    cpsasec[,cps.vars.pre2019],by=list(hh=cpsasec$hh,state=cpsasec$gestfips),sum)
  nat_income = aggregate(
    cpsasec[,cps.vars.pre2019],by=list(hh=cpsasec$hh),sum)
  } else {
  income = aggregate(
    cpsasec[,cps.vars.post2019.add],by=list(hh=cpsasec$hh,state=cpsasec$gestfips),sum)
  nat_income = aggregate(
    cpsasec[,cps.vars.post2019.add],by=list(hh=cpsasec$hh),sum)
  }
  nat_income$state = 0
  nat_income = nat_income[,names(income)]
  income = rbind(nat_income,income)

  # convert income by quantile to share of total by state and quantile
  shares <- data.frame(income %>% group_by(state) %>% 
                       mutate(across(-c(hh), ~./sum(.))))

  # shift from wide to long format
  income = income %>% pivot_longer(!c(state,hh), names_to = "source", values_to = "value")
  shares = shares %>% pivot_longer(!c(state,hh), names_to = "source", values_to = "value")

  # add the data year
  income$year = year
  shares$year = year
  count$year = year
  numhh$year = year
  
  # function returns list of outputs
  return(list(income=income,shares=shares,count=count,numhh=numhh,hh.inc=hh.inc))

}

# mapping fips to state names
state_names = 
  c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
    "Connecticut","Delaware","District of Columbia","Florida","Georgia",
    "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
    "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
    "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
    "New Jersey","New Mexico","New York","North Carolina","North Dakota",
    "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
    "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
    "West Virginia","Wisconsin","Wyoming","United States")

state_fips = 
  c("1","2","4","5","6","8","9","10","11","12","13","15","16","17","18",
    "19","20","21","22","23","24","25","26","27","28","29","30","31","32","33",
    "34","35","36","37","38","39","40","41","42","44","45","46","47","48","49",
    "50","51","53","54","55","56","0")

state_abbr = 
  c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
    "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
    "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
    "VT","VA","WA","WV","WI","WY","US")

statemap = data.frame(name = state_names, state = state_fips, r = state_abbr)


# for each requested year get the income totals, shares, and counts by quantile
# and state
income = NULL
shares = NULL
count = NULL
numhh = NULL
hh.inc = NULL
for (year in years) {
  output = get.shares(year)
  income = rbind(income,output[["income"]])
  shares = rbind(shares,output[["shares"]])
  count = rbind(count,output[["count"]])
  numhh = rbind(numhh,output[["numhh"]])
  hh.inc = rbind(hh.inc,output[["hh.inc"]])
}

# merge state names into income and shares data.frames
shares$state <- as.character(shares$state)
income$state <- as.character(income$state)
count$state <- as.character(count$state)
numhh$state <- as.character(numhh$state)

income <- left_join(income, statemap, by="state")
shares <- left_join(shares, statemap, by="state")
count <- left_join(count, statemap, by="state")
numhh <- left_join(numhh, statemap, by="state")

# keep needed columns in data
income <- income %>% select(year,r,hh,source,value)
shares <- shares %>% select(year,r,hh,source,value)
count <- count %>% select(year,r,hh,n)
numhh <- numhh %>% select(year,r,hh,numhh)
numhh$state = NULL


# ------------------------------------------------------------------------------
# assess variable changes in retirement income
# ------------------------------------------------------------------------------

# combine variable ids with labels
cps.vars.plot = rbind(
                 c("hretval","retirement income"),
                 c("hdstval","retirement distributions"),
                 c("hpenval","pension income"),
                 c("hannval","annuities"))

cps.vars.plot = data.frame(source=cps.vars.plot[,1],
                           description=cps.vars.plot[,2],
                           stringsAsFactors=FALSE)

# add the income source descriptions
shares_plot = left_join(subset(shares,source %in% cps.vars.plot$source),
                        cps.vars.plot,by="source")
income_plot = left_join(subset(income,source %in% cps.vars.plot$source),
                        cps.vars.plot,by="source")

# set the ordering for the plot
shares_plot$description = factor(shares_plot$description,
                                 rev(cps.vars.plot$description))
shares_plot$year = factor(shares_plot$year)

income_plot$description = factor(income_plot$description,
                                 rev(cps.vars.plot$description))
income_plot$year = factor(income_plot$year)

categories <- factor(c("hh1","hh2","hh3","hh4","hh5"))
shares_plot$hh = factor(shares_plot$hh,rev(levels(categories)))
shares_plot$r = factor(shares_plot$r)

income_plot$hh = factor(income_plot$hh,rev(levels(categories)))
income_plot$r = factor(income_plot$r)

state_shares <- subset(shares_plot, r %in% "US")
state_income <- subset(income_plot, r %in% "US")

p = ggplot(state_shares)+
  geom_bar(aes(y=year,x=value,fill=hh),stat="identity")+
  scale_fill_brewer(palette="Set3") +
  facet_grid(r~description, scales="free") +
  labs(y="Year",x="Share of Total") +
#  xlim(0,1) +
  guides(fill=guide_legend(title="Household Categories",reverse=TRUE))+
  theme(axis.line.x      = element_line(colour="black"),
        axis.line.y      = element_line(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        legend.box.just  = "left",
        legend.key       = element_blank())
print(p)

p = ggplot(state_income)+
  geom_bar(aes(y=year,x=value/1e9,fill=hh),stat="identity")+
  scale_fill_brewer(palette="Set3") +
  facet_grid(r~description, scales="fixed") +
  labs(y="Year",x="billions $") +
#  xlim(0,1) +
  guides(fill=guide_legend(title="Household Categories",reverse=TRUE))+
  theme(axis.line.x      = element_line(colour="black"),
        axis.line.y      = element_line(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        legend.box.just  = "left",
        legend.key       = element_blank())
print(p)


# ------------------------------------------------------------------------------
# save cps results
# ------------------------------------------------------------------------------

# income totals
write.csv(income,
          paste0("cps_asec_income_totals_",min(years),"_",max(years),".csv"),
          row.names=F)

# income shares
write.csv(shares,
          paste0("cps_asec_income_shares_",min(years),"_",max(years),".csv"),
          row.names=F)

# count of observations
write.csv(count,
          paste0("cps_asec_income_counts_",min(years),"_",max(years),".csv"),
          row.names=F)

# number of households in u.s.
write.csv(numhh,
          paste0("cps_asec_numberhh_",min(years),"_",max(years),".csv"),
          row.names=F)

# report household income cutoffs for chosen quantiles
write.csv(hh.inc, 
          paste0("household_income_bins_",min(years),"_",max(years),".csv"), 
          row.names=FALSE)



# ------------------------------------------------------------------------------
# download/reconcile/save nipa income data
# ------------------------------------------------------------------------------

# [1] "Personal income"
# 	[2] "Compensation of employees"
# 		[3] "Wages and salaries"
# 			[4] "Private industries"
# 			[5] "Government"
# 		[6] "Supplements to wages and salaries"
# 			[7] "Employer contributions for employee pension and insurance funds"
# 			[8] "Employer contributions for government social insurance"
# 	[9] "Proprietors' income with inventory valuation and capital consumption adjustments"
# 		[10] "Farm"
# 		[11] "Nonfarm"
# 	[12] "Rental income of persons with capital consumption adjustment"
# 	[13] "Personal income receipts on assets"
# 		[14] "Personal interest income"
# 		[15] "Personal dividend income"
# 	[16] "Personal current transfer receipts"
# 		[17] "Government social benefits to persons"
# 	    [18] "Social security"
# 		  [19] "Medicare"
# 		  [20] "Medicaid"
# 		  [21] "Unemployment insurance"
# 		  [22] "Veterans' benefits"
# 		  [23] "Other"
# 	  [24] "Other current transfer receipts, from business (net)"
# 	[25] "Less: Contributions for government social insurance, domestic"
# [26] "Less: Personal current taxes"
# [27] "Equals: Disposable personal income"
# [28] "Less: Personal outlays"
# 	[29] "Personal consumption expenditures"
# 	[30] "Personal interest payments"
# 	[31] "Personal current transfer payments"
# 		[32] "To government"
# 		[33] "To the rest of the world (net)"
# [34] "Equals: Personal saving"
# 	[35] "Personal saving as a percentage of disposable personal income"

# download bea nipa data
specs <- list(
  'UserID' = bea.key,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T20100',
  'Frequency' = 'A',
  'Year' = '2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022',
  'ResultFormat' = 'json'
)
beadata <- data.frame(beaGet(specs))

# convert data to long format
beadata <- pivot_longer(
  beadata, 
  -c(TableName,SeriesCode,LineNumber,LineDescription,METRIC_NAME,CL_UNIT,UNIT_MULT),
  names_to = "year", values_to = "value")

beadata$year = as.numeric(str_replace(beadata$year,"DataValue_",""))

# keep all data, using gams to piece together needed components
write.csv(beadata, "nipa_income_outlays_2000_2022.csv", row.names=FALSE)

# define national average markup or fringe benefits relative to total employee 
# compensation
nipa_fringe = subset(beadata, LineNumber %in% c("2","3")) %>%
  rename("desc"="LineDescription") %>%
  select(year,desc,value)
nipa_fringe$desc[nipa_fringe$desc=="Compensation of employees"] = "compen"
nipa_fringe$desc[nipa_fringe$desc=="Wages and salaries"] = "wages"
nipa_fringe = nipa_fringe %>%
  pivot_wider(names_from=desc, values_from=value) %>% 
  mutate(markup = compen / wages) %>%
  select(year,markup)
write.csv(nipa_fringe, "nipa_fringe_benefit_markup.csv", row.names=FALSE)


# ------------------------------------------------------------------------------
# read in windc totals from core database
# ------------------------------------------------------------------------------

system('gdxdump ../../../core/WiNDCdatabase.gdx output=ld0_windc.csv symb=ld0_ format=csv')
system('gdxdump ../../../core/WiNDCdatabase.gdx output=kd0_windc.csv symb=kd0_ format=csv')

abbr <- data.frame(
  r = c("US","AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL",
        "IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",
        "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
        "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"),
  sname = c("united states","alabama","alaska","arizona","arkansas","california",
            "colorado","connecticut","delaware","district of columbia","florida",
            "georgia","hawaii","idaho","illinois","indiana","iowa","kansas",
            "kentucky","louisiana","maine","maryland","massachusetts","michigan",
            "minnesota","mississippi","missouri","montana","nebraska","nevada",
            "new hampshire","new jersey","new mexico","new york","north carolina",
            "north dakota","ohio","oklahoma","oregon","pennsylvania","rhode island",
            "south carolina","south dakota","tennessee","texas","utah","vermont",
            "virginia","washington","west virginia","wisconsin","wyoming"))

ld0 = read.csv(file=file.path("ld0_windc.csv")) %>%
  group_by(yr,r) %>%
  summarize(value = sum(Val,na.rm=TRUE)*1e9) %>%
  rename("year"="yr") %>%
  left_join(abbr)

kd0 = read.csv(file=file.path("kd0_windc.csv")) %>%
  group_by(yr,r) %>%
  summarize(value = sum(Val,na.rm=TRUE)*1e9) %>%
  rename("year"="yr") %>%
  left_join(abbr)

ld0_windc = ld0 %>% group_by(year) %>% summarize(windc = sum(value))
kd0_windc = kd0 %>% group_by(year) %>% summarize(windc = sum(value))


# ------------------------------------------------------------------------------
# compare cps and windc totals with nipa accounts
# ------------------------------------------------------------------------------

# plot for comparison with nipa and cps/windc

# aggregate cps data to national totals
cps_totals = subset(income, r %in% "US") %>%
  group_by(year,source) %>%
  summarize(cps = sum(value))

# link cps categories and nipa accounts

# [1] "Personal income"
#     "htotval"

nipa_cps_totinc = subset(beadata, LineNumber %in% "1") %>%
  rename("nipa"="value","desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  right_join(subset(cps_totals, source %in% "htotval")) %>%
  mutate(pct_diff = 100 * (cps / nipa - 1),
         category = "total income") %>%
  select(-source)

# 	[2] "Compensation of employees"
# 		[3] "Wages and salaries"
#         "hwsval"

nipa_cps_wages = subset(beadata, LineNumber %in% "3") %>%
  rename("nipa"="value","desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  right_join(subset(cps_totals, source %in% "hwsval")) %>%
  mutate(pct_diff = 100 * (cps / nipa - 1),
         category = "wages and salaries") %>%
  select(-source)

# 			[4] "Private industries"
# 			[5] "Government"
# 		[6] "Supplements to wages and salaries"
# 			[7] "Employer contributions for employee pension and insurance funds"
# 			[8] "Employer contributions for government social insurance"
# 	[9] "Proprietors' income with inventory valuation and capital consumption adjustments"
# 		[10] "Farm"
#          "hfrval"

nipa_cps_farm = subset(beadata, LineNumber %in% "10") %>%
  rename("nipa"="value","desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  right_join(subset(cps_totals, source %in% "hfrval")) %>%
  mutate(pct_diff = 100 * (cps / nipa - 1),
         category = "proprietor's income: farm") %>%
  select(-source)

# 		[11] "Nonfarm"
#          "hseval"

nipa_cps_nonfarm = subset(beadata, LineNumber %in% "11") %>%
  rename("nipa"="value","desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  right_join(subset(cps_totals, source %in% "hseval")) %>%
  mutate(pct_diff = 100 * (cps / nipa - 1),
         category = "proprietor's income: non-farm") %>%
  select(-source)

# 	[12] "Rental income of persons with capital consumption adjustment"
#        "hrntval"

nipa_cps_rent = subset(beadata, LineNumber %in% "12") %>%
  rename("nipa"="value","desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  right_join(subset(cps_totals, source %in% "hrntval")) %>%
  mutate(pct_diff = 100 * (cps / nipa - 1),
         category = "rental income") %>%
  select(-source)

# 	[13] "Personal income receipts on assets"
# 		[14] "Personal interest income"
#          "hintval"

nipa_cps_interest = subset(beadata, LineNumber %in% "14") %>%
  rename("nipa"="value","desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  right_join(subset(cps_totals, source %in% "hintval")) %>%
  mutate(pct_diff = 100 * (cps / nipa - 1),
         category = "personal interest income") %>%
  select(-source)

# 		[15] "Personal dividend income"
#          "hdivval"

nipa_cps_div = subset(beadata, LineNumber %in% "15") %>%
  rename("nipa"="value","desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  right_join(subset(cps_totals, source %in% "hdivval")) %>%
  mutate(pct_diff = 100 * (cps / nipa - 1),
         category = "personal divident income") %>%
  select(-source)

# 	[16] "Personal current transfer receipts"
# 		[17] "Government social benefits to persons"
# 	    [18] "Social security"
#            "hssval","hssival","hdisval"

cps_socsec = subset(cps_totals, source %in% c("hssval","hssival","hdisval")) %>%
  group_by(year) %>% summarize(cps = sum(cps))
nipa_cps_socsec = subset(beadata, LineNumber %in% "18") %>%
  rename("nipa"="value","desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  right_join(cps_socsec) %>%
  mutate(pct_diff = 100 * (cps / nipa - 1),
         category = "government benefits: social security")

# 		  [19] "Medicare"
# 		  [20] "Medicaid"
# 		  [21] "Unemployment insurance"
#            "hucval"

nipa_cps_uc = subset(beadata, LineNumber %in% "21") %>%
  rename("nipa"="value","desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  right_join(subset(cps_totals, source %in% "hucval")) %>%
  mutate(pct_diff = 100 * (cps / nipa - 1),
         category = "government benefits: unemployment insurance") %>%
  select(-source)

# 		  [22] "Veterans' benefits"
#            "hvetval"

nipa_cps_vet = subset(beadata, LineNumber %in% "22") %>%
  rename("nipa"="value","desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  right_join(subset(cps_totals, source %in% "hvetval")) %>%
  mutate(pct_diff = 100 * (cps / nipa - 1),
         category = "government benefits: veterans' benefits") %>%
  select(-source)

# 		  [23] "Other"
#            "hwcval","hpawval","hsurval","hedval",

cps_othtran = subset(cps_totals, source %in% c("hwcval","hpawval","hsurval","hedval")) %>%
  group_by(year) %>% summarize(cps = sum(cps))
nipa_cps_othtran = subset(beadata, LineNumber %in% "23") %>%
  rename("nipa"="value","desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  right_join(cps_othtran) %>%
  mutate(pct_diff = 100 * (cps / nipa - 1),
         category = "government benefits: other")

# 	  [24] "Other current transfer receipts, from business (net)"
#          "hcspval","hfinval","hoival"

# hoival: household income - other income: (such as foster child care, alimony, 
#         jury duty, armed forces reserves, severance pay, hobbies, or any other 
#         source
# hfinval:household income - financial assistance income
# hcspval:household income - child support

cps_nongovtran = subset(cps_totals, source %in% c("hcspval","hfinval","hoival")) %>%
  group_by(year) %>% summarize(cps = sum(cps))
nipa_cps_nongovtran = subset(beadata, LineNumber %in% "24") %>%
  rename("nipa"="value","desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  right_join(cps_nongovtran) %>%
  mutate(pct_diff = 100 * (cps / nipa - 1),
         category = "non-government transfer income")

# plot and report differences (for reference on what drives differences -- 
# https://www.census.gov/topics/income-poverty/income/guidance/data-sources/cps-vs-other.html)
nipa_cps = rbind(nipa_cps_totinc, nipa_cps_wages,nipa_cps_farm, nipa_cps_nonfarm,
                 nipa_cps_rent, nipa_cps_interest, nipa_cps_div,
                 nipa_cps_socsec, nipa_cps_uc, nipa_cps_vet,
                 nipa_cps_othtran, nipa_cps_nongovtran)
nipa_cps$category = factor(nipa_cps$category,
                           levels = unique(nipa_cps$category))
nipa_cps$year = factor(nipa_cps$year)
p = ggplot(nipa_cps)+
  geom_bar(aes(x=year,y=pct_diff,fill=category),position="dodge",stat="identity",color="black")+
  scale_fill_brewer(palette="Set3") +
  labs(title="CPS vs. NIPA: Aggregate Income Categories", 
       x="Year",y="% Difference from NIPA")+
  guides(fill=guide_legend(title="Income Category",reverse=FALSE))+
  facet_wrap(~category, ncol=4, scales="fixed")+
  theme(axis.line.x      = element_line(colour="black"),
        axis.line.y      = element_line(colour="black"),
        axis.text.x      = element_text(angle=90),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        legend.box.just  = "left",
        legend.key       = element_blank())
ggsave(file.path("cps_vs_nipa_income_categories.png"),plot=p,device="png",width=14,
      height=6, limitsize=FALSE)

write.csv(nipa_cps,"cps_vs_nipa_income_categories.csv",row.names=FALSE)


# link windc categories and nipa accounts
# ld0 = [2] "Compensation of employees"
# kd0 = [9] "Proprietors' income with inventory valuation and capital consumption adjustments"
#       [12] "Rental income of persons with capital consumption adjustment"
#       [13] "Personal income receipts on assets"

nipa_windc_labor = subset(beadata, LineNumber %in% "2") %>%
  rename("nipa"="value", "desc"="LineDescription") %>%
  select(year,nipa) %>%
  mutate(nipa = nipa * 1e6) %>%
  left_join(ld0_windc) %>%
  mutate(pct_diff = 100 * (windc / nipa - 1),
         category = "labor")

nipa_windc_capital = subset(beadata, LineNumber %in% c("9","12","13")) %>%
  group_by(year) %>%
  summarize(nipa = sum(value)*1e6) %>%
  select(year,nipa) %>%
  left_join(kd0_windc) %>%
  mutate(pct_diff = 100 * (windc / nipa - 1),
         category = "capital")

# plot and report difference between windc and nipa
nipa_windc = rbind(nipa_windc_labor, nipa_windc_capital)
p = ggplot(subset(nipa_windc, !(windc %in% NA)))+
  geom_bar(aes(x=year,y=pct_diff,fill=category),position="dodge",stat="identity",color="black")+
  scale_fill_brewer(palette="Set3") +
  labs(title="WiNDC vs. NIPA: Aggregate Income Categories", 
       x="Year",y="% Difference from NIPA",
       subtitle="Difference in capital income -- foreign capital ownership, capital gains.") +
  guides(fill=guide_legend(title="Income Category",reverse=TRUE))+
  theme(axis.line.x      = element_line(colour="black"),
        axis.line.y      = element_line(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        legend.box.just  = "left",
        legend.key       = element_blank())
print(p)
ggsave(file.path("windc_vs_nipa_income_categories.png"),device="png",width=10,
      height=6, limitsize=FALSE)

write.csv(nipa_windc,"windc_vs_nipa_income_categories.csv",row.names=FALSE)

# report capital ownership shares
cap_own = nipa_windc %>% 
  filter(category %in% "capital") %>%
  mutate(domestic_share = nipa / windc) %>%
  select(year,nipa,windc,domestic_share)

write.csv(cap_own, "windc_vs_nipa_domestic_capital.csv", row.names=FALSE)


# ------------------------------------------------------------------------------
# plot the time series of cps data (counts and shares)
# ------------------------------------------------------------------------------

# combine variable ids with labels
cps.vars.plot = rbind(
  c("hwsval","wages and salaries"),
  c("hseval","self-employment (nonfarm)"),
  c("hfrval","self-employment farm"),
  c("hucval","unemployment compensation"),
  c("hwcval","workers compensation"),
  c("hssval","social security"),
  c("hssival","supplemental security"),
  c("hpawval","public assistance or welfare"),
  c("hvetval","veterans benefits"),
  c("hsurval","survivors income"),
  c("hdisval","disability"),
  c("hretval","retirement income"),
  c("hdstval","retirement distributions"),
  c("hpenval","pension income"),
  c("hannval","annuities"),
  c("hintval","interest"),
  c("hdivval","dividends"),
  c("hrntval","rents"),
  c("hedval","educational assistance"),
  c("hcspval","child support"),
  c("hfinval","financial assistance"),
  c("hoival","other income"),
  c("htotval","total household income"))

cps.vars.plot = data.frame(
  source=cps.vars.plot[,1],
  description=cps.vars.plot[,2],
  stringsAsFactors=FALSE)

# add the income source descriptions
shares_plot = left_join(shares,cps.vars.plot,by="source")

# set the ordering for the plot
shares_plot$description = factor(shares_plot$description,
                                 rev(cps.vars.plot$description))

shares_plot$year = factor(shares_plot$year)

categories <- factor(c("hh1","hh2","hh3","hh4","hh5"))
shares_plot$hh = factor(shares_plot$hh,rev(levels(categories)))
shares_plot$r = factor(shares_plot$r)

# separate shares
state_shares <- subset(shares_plot, !(r %in% "US"))
nation_shares <- subset(shares_plot, (r %in% "US"))

# plot the shares by income source, decile and state
p = ggplot(state_shares)+
  geom_bar(aes(y=r,x=value,fill=hh),stat="identity")+
  scale_fill_brewer(palette="Set3") +
  facet_grid(year~description, scales="free") +
  labs(y="State",x="Share of Total") +
#  xlim(0,1) +
  guides(fill=guide_legend(title="Household Categories",reverse=TRUE))+
  theme(axis.line.x      = element_line(colour="black"),
        axis.line.y      = element_line(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        legend.box.just  = "left",
        legend.key       = element_blank())
print(p)
ggsave(file.path("cps_asec_income_shares_state.png"),device="png",width=50,
      height=60, limitsize=FALSE)

# plot the shares by income source and decile
p = ggplot(nation_shares)+
  geom_bar(aes(y=year,x=value,fill=hh),stat="identity")+
  facet_wrap(~description,nrow=4) +
  scale_fill_brewer(palette="Set3") +
  labs(y="years",x="Share of Total") +
#   xlim(0,1.1) +
  guides(fill=guide_legend(title="Household Categories",reverse=TRUE))+
  theme(axis.line.x      = element_line(colour="black"),
        axis.line.y      = element_line(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        legend.box.just  = "left",
        legend.key       = element_blank())
print(p)
ggsave(file.path("cps_asec_income_shares_nation.png"),device="png",width=20,
      height=15)

## define plotting parameter
count_plot <- count

## characterize factors
categories <- factor(c("hh1","hh2","hh3","hh4","hh5"))
count_plot$hh = factor(count_plot$hh,rev(levels(categories)))
count_plot$r = factor(count_plot$r)
count_plot$year = factor(count_plot$year)

## separate shares
state_count <- subset(count_plot, !(r %in% "US"))
nation_count <- subset(count_plot, (r %in% "US"))

# plot the counts by income source, decile and state
p = ggplot(state_count)+
  geom_bar(aes(y=r,x=n,fill=hh),stat="identity")+
  scale_fill_brewer(palette="Set3") +
  labs(y="State",x="Number of Observations") +
  facet_wrap(~year,nrow=1) +
  guides(fill=guide_legend(title="Quintile",reverse=TRUE))+
  theme(axis.line.x      = element_line(colour="black"),
        axis.line.y      = element_line(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        legend.box.just  = "left",
        legend.key       = element_blank())
print(p)
ggsave(file.path("cps_asec_countobs_state.png"),device="png",width=25,
       height=20)

# plot the counts by income source and decile
p = ggplot(nation_count)+
  geom_bar(aes(y=year,x=n,fill=hh),stat="identity")+
  scale_fill_brewer(palette="Set3") +
  labs(y="Year",x="N") +
  guides(fill=guide_legend(title="Quintiles",reverse=TRUE))+
  theme(axis.line.x      = element_line(colour="black"),
        axis.line.y      = element_line(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        legend.box.just  = "left",
        legend.key       = element_blank())
print(p)
ggsave(file.path("cps_asec_countobs_nation.png"),device="png",width=12,
       height=8)


# ------------------------------------------------------------------------------
# grab labor and capital income tax rates from taxsim v35
# ------------------------------------------------------------------------------

cat("constructing income tax rates...\n")

# taxes defined:
#  tl                   tax rate - labor income tax
#  tfica                tax rate - fica tax
#  tk                   tax rate - capital income tax
#  tp                   tax rate - payroll tax
#  tc                   tax rate - consumption tax
#
# personal income taxes:
#  uses data from the cps to drive nber's taxsim model to obtain earnings
#  weighted average effective marginal labor income tax rates by state and
#  implan household groupings. personal income taxes on capital income are
#  aggregated to national level effective marginal tax rates.
#
# corporate income tax:
#  hard coded from cbo study on effective tax rate.
#
# payroll tax:
#  the payroll tax is set to zero in the default version of the model. both the
#  employee and employer parts of the fica tax are defined using tfica to
#  capture the cap on social security taxes, but this tax rate is kept in the
#  model in case there is a future need to break out medicare taxes from fica.
#
# consumption tax:
#  the consumption tax rate is set to zero in the default version of the model.
#  sales and excise taxes are included in the implan defined production tax
#  rates.
#
# notes on using the cps with nber's taxsim:
#  1. requires the cpsR package to easily grab the cps data from census and 
#     import into r
#
#  2. uses the cps imputation of non-salt deductions for federal taxes when
#     possible
#
#  3. it is not clear if child cares expenses are included in the cps imputed
#     deductions so we include them by sharing them out to tax units in a
#     household based on share of dependents younger than 13
#
#  4. there have been concerns previously raised about the validity of the cps
#     variable linking dependents to tax filer (dep_stat) and the number of
#     non-filers/non-dependents with earnings. we have looked at both and in the
#     2021 asec version they appear reasonable but one should proceed with
#     caution.
#
#  5. we produce the rate disaggregated to the state level for use with our
#     application as they are aggregated up to a much more aggregated resolution
#     both in terms of space and households later in our workstream. care should
#     be taken using the rates at the state level, especially for the lowest
#     income household groups.
#



# ------------------------------------------------------------------------------
# set general parameters
# ------------------------------------------------------------------------------

# year of tax data calculated
year = 2016

# household income bins in 2016 data
hh.inc = subset(hh.inc, year==2016)
hh.inc$year = NULL

# national taxable capital income by type from irs soi 1040 line item totals
# https://www.irs.gov/pub/irs-soi/16inlinecount.pdf
# thousands of dollars
# hard coded as these are only available as a pdf
interest            = 96640233
ordinary.dividends  = 254065327
qualified.dividends = 202033967
prop.income         = 706486715
cap.gains.1040      = 614215455
business.income     = 328082028

# personal exemption for the tax year under consideration
pexemp = 4050

# standard deduction for head of household for the tax year under consideration
hh.std.deduct = 9300

# standard deduction for single or married filing separately
si.std.deduct = 6300

# standard deduction for married filing jointly for the tax year under
# consideration
mar.std.deduct = 12600

# url for nber's taxsim
taxsim_url = "https://taxsim.nber.org/taxsim35/redirect.cgi"  # version 35



# ------------------------------------------------------------------------------
# load the cps asec data
# ------------------------------------------------------------------------------

# list of cps march supplement variables needed to create the sample returns
# ptotval (total income), dbtn_val, ann_val, pnsn_val (pension/ira distributions),
# cap_val (used to impute capital gains tax) not available in 2017 asec
cps_vars = c("dep_stat", "filestat", "agi", "a_maritl", "ptotval", "ss_val",
             "ssi_val", "h_year", "gestfips", "a_age", "h_seq", "a_lineno",
             "a_spouse", "wsal_val", "semp_val", "frse_val", "hiemp", "phip_val",
             "rtm_val", "div_val", "int_val", "rnt_val", "oi_val", "ed_val",
             "srvs_val", "dsab_val", "uc_val", "paw_val", "wc_val", "vet_val",
             "csp_val", "fin_val", "tax_inc", "marsupwt", "statetax_a",
             "htotval", "prop_tax")

# download and process the cps march supplement for the year after the benchmark
# year since income questions on the supplement are for the previous year
cpsasec = get_asec(year+1, vars = cps_vars, key = cps.key, tibble=FALSE)

# remove entries that have missing values
h_ids = cpsasec %>%
        filter(semp_val == -9999 |
               frse_val == -9999 |
               rnt_val == -9999 |
               htotval == -9999) %>%
        select(h_seq)
print(paste0("Removing ", 
             signif(nrow(h_ids)/length(unique(cpsasec$h_seq))*100,2), 
             "% of households due to missing data"))
cpsasec = cpsasec %>%
          filter(!(h_seq %in% h_ids$h_seq))

# on rare occasion there can be households that have wages that are order of 
# magnitudes above the total household income, while theoretically possible
# the cps asec entries meeting this criteria appear to have inconsistencies that
# can't explain the relative magnitudes and are therefore, dropped
h_ids = cpsasec %>%
        filter(wsal_val > 0 & wsal_val > 100*htotval) %>%
        select(h_seq)
print(paste0("Removing ",
             signif(nrow(h_ids)/length(unique(cpsasec$h_seq))*100,2), 
             "% of households due to inconsistent income data"))
cpsasec = cpsasec %>%
          filter(!(h_seq %in% h_ids$h_seq))



# ------------------------------------------------------------------------------
# check for clear problems with filestat and dep_stat
# ------------------------------------------------------------------------------

# concerns have previously been raised about the number of non-filers in the cps
# here we check the percentage of non-filer that are not declared as dependents
# and also have income that would suggest that maybe they should be filing

# check the number of dependents also listed as filers - some may be legit, but
# worth checking
print(paste0("Percent of dependents listed as filers: ",
             signif(sum(cpsasec$dep_stat>0 & cpsasec$filestat<6)/
                      sum(cpsasec$dep_stat>0)*100,2),"%"))

# check the number of dependents also listed as filers but with income less
# than the standard deduction - not impossible but a concern if this is large
print(paste0("Percent of dependents listed as filers w/ income less than ",
             "standard deductino: ",
             signif(sum(cpsasec$dep_stat>0 & cpsasec$filestat<6 &
                          cpsasec$agi<hh.std.deduct)/
                      sum(cpsasec$dep_stat>0)*100,2),"%"))

# number of non filers that are also not a dependent
non.filer.non.dependent = sum(cpsasec$filestat==6 & cpsasec$dep_stat==0)

# number of single or head of households that are non filers and not a dependent
# and whose income obviously exceeds the threshold for filing - this doesn't
# distinguish between single and head of household so may falsely throw away
# issues for singles
single.issues = sum(cpsasec$a_maritl>3 & cpsasec$filestat==6 &
                      cpsasec$dep_stat==0 &
                      (cpsasec$ptotval-cpsasec$ss_val-cpsasec$ssi_val)>
                      (pexemp+hh.std.deduct))

# number of married persons that are non filers and not a dependent
# and whose income obviously exceeds the threshold for filing - this assumes
# all married couple file jointly
married.issues = sum(cpsasec$a_maritl<=3 & cpsasec$filestat==6 &
                       cpsasec$dep_stat==0 &
                       (cpsasec$ptotval-cpsasec$ss_val-cpsasec$ssi_val)>
                       (pexemp*2+mar.std.deduct))

print(paste0("Percent of non-filer non-dependent cases that may be problems: ",
             signif((married.issues+single.issues)/
                      non.filer.non.dependent,2)*100,"%"))



# ------------------------------------------------------------------------------
# state and year
# ------------------------------------------------------------------------------

# taxsimid  case id (arbitrary, but must be a non-negative numeric)
# just use the row in the cps asec data so we can map it back if needed
cpsasec$taxsimid = 1:nrow(cpsasec)

# yeartax   year
# asec data is for the preceeding year so set the tax year to the previous year
cpsasec$yeartax = cpsasec$h_year-1

# state     soi codes
# add the state soi code to the data using the fips codes
cpsasec$state = factor(cpsasec$gestfips,levels=c( 1L, 2L, 4L, 5L, 6L, 8L, 9L,
                                                  10L,11L,12L,13L,15L,16L,17L,
                                                  18L,19L,20L,21L,22L,23L,24L,
                                                  25L,26L,27L,28L,29L,30L,31L,
                                                  32L,33L,34L,35L,36L,37L,38L,
                                                  39L,40L,41L,42L,44L,45L,46L,
                                                  47L,48L,49L,50L,51L,53L,54L,
                                                  55L,56L),
                       labels=1:51)



# ------------------------------------------------------------------------------
# create marital status and ages
# ------------------------------------------------------------------------------

# mstat     marital status
#             1. single or head of household (unmarried)
#             2. joint (married)
#             6. separate (married)
#             8. dependent taxpayer (typically child with income)
#             head of household status is determined by taxsim
cpsasec$mstat = 1
cpsasec$mstat[cpsasec$filestat %in% 1:3] = 2
cpsasec$mstat[cpsasec$filestat==5 & cpsasec$dep_stat>0] = 8
cpsasec$mstat[cpsasec$filestat==6] = 0

# make sure that any dependent filing a tax return is not counted as a dependent
# on anyone else's tax return by removing them as a dependent
cpsasec$dep_stat[cpsasec$filestat<6] = 0

# page      age of primary taxpayer
cpsasec$page = cpsasec$a_age

# find the record associated with each spouse - doing this once allows for
# faster processing later on when we need reference spouses
cpsasec$id = cpsasec$h_seq*10000+cpsasec$a_lineno
temp = merge(data.frame(id=cpsasec$id),
             data.frame(id=cpsasec$h_seq*10000+cpsasec$a_spouse,
                        x=1:nrow(cpsasec)),
             sort=FALSE,all.x=TRUE)
names(temp)[names(temp)=="x"] = "spouse_row"
cpsasec = merge(cpsasec,temp,all.x=TRUE,sort=FALSE)

# sage      age of spouse (or zero)
cpsasec$sage = cpsasec$a_age[cpsasec$spouse_row]
cpsasec$sage[is.na(cpsasec$sage)] = 0

# make sure that spouse age is populated for anyone filing jointly
if (any(cpsasec$filestat<=3 & cpsasec$sage==0))
  warning("not all spouse ages populated")

# make sure that spouse age is not populated for anyone not filing jointly
if (any(cpsasec$filestat %in% c(4,5) & cpsasec$sage>0,na.rm=TRUE))
  warning("not all married couples filing jointly")

# make sure that spouse age of a spouse matches the primary's age
if (any(cpsasec$page!=cpsasec$sage[cpsasec$spouse_row],na.rm=TRUE))
  warning("mismatch in spouse ages")



# ------------------------------------------------------------------------------
# get number of dependents by age
# ------------------------------------------------------------------------------

# this is not the most straightforward way to code this process, but using a for
# loop here or sapply is painfully slow

# get the row of the person claiming each dependent and store that with the
# dependent record
temp = merge(data.frame(id=(cpsasec$h_seq*10000+
                              cpsasec$dep_stat)[cpsasec$dep_stat>0]),
             data.frame(id=cpsasec$h_seq*10000+cpsasec$a_lineno,
                        x=1:nrow(cpsasec)),
             sort=FALSE)$x
cpsasec$dep_row = NA
cpsasec$dep_row[cpsasec$dep_stat>0] = temp

# default to zero dependents
cpsasec$depx = 0
cpsasec$dep13 = 0
cpsasec$dep17 = 0
cpsasec$dep18 = 0

# depx      number of dependents (personal exemptions)
temp = aggregate(rep(1,nrow(cpsasec)),by=list(cpsasec$dep_row),sum)
cpsasec$depx[temp$Group.1] = temp$x

# dep13     number of children under 13 (dependent care credit)
dep_row = cpsasec$dep_row[cpsasec$a_age<13]
temp = aggregate(rep(1,length(dep_row)),by=list(dep_row),sum)
cpsasec$dep13[temp$Group.1] = temp$x

# dep17     number of children under 17 (child credit)
dep_row = cpsasec$dep_row[cpsasec$a_age<17]
temp = aggregate(rep(1,length(dep_row)),by=list(dep_row),sum)
cpsasec$dep17[temp$Group.1] = temp$x

# dep18     number of children under 18 (eitc)
dep_row = cpsasec$dep_row[cpsasec$a_age<18]
temp = aggregate(rep(1,length(dep_row)),by=list(dep_row),sum)
cpsasec$dep18[temp$Group.1] = temp$x

# populate dependents for spouses if married and not primary
cpsasec$depx = pmax(cpsasec$depx,cpsasec$depx[cpsasec$spouse_row],na.rm=TRUE)
cpsasec$dep13 = pmax(cpsasec$dep13,cpsasec$dep13[cpsasec$spouse_row],na.rm=TRUE)
cpsasec$dep17 = pmax(cpsasec$dep17,cpsasec$dep17[cpsasec$spouse_row],na.rm=TRUE)
cpsasec$dep18 = pmax(cpsasec$dep18,cpsasec$dep18[cpsasec$spouse_row],na.rm=TRUE)

# verify that dependent count is the same across spouses
vars = c("depx","dep13","dep17","dep18")
if (any(cpsasec[,vars]!=cpsasec[cpsasec$spouse_row,vars],na.rm=TRUE))
  warning("dependent counts not equal across spouses")



# ------------------------------------------------------------------------------
# income
# ------------------------------------------------------------------------------

# pwages    wage and salary income of primary taxpayer (include self-employment)
cpsasec$pwages = cpsasec$wsal_val+cpsasec$semp_val+cpsasec$frse_val-
                 cpsasec$hiemp*cpsasec$phip_val
#cpsasec$pwages = pmax(cpsasec$pwages, 0)

# swages    wage and salary income of spouse (include self-employment)
cpsasec$swages = cpsasec$pwages[cpsasec$spouse_row]
cpsasec$swages[is.na(cpsasec$swages)] = 0

# dividends dividend income (qualified dividends only for 2003 on)
# dividends are split between qualified dividends and other property income,
# which includes ordinary dividends, based on 1040 line item totals
cpsasec$dividends = cpsasec$div_val*qualified.dividends/
  (qualified.dividends+ordinary.dividends)

# intrec    interest received (+/-)
cpsasec$intrec = cpsasec$int_val

# cps stopped imputing capital gains in 2010
# stcg      short term capital gains or losses (+/-)
# ltcg      long term capital gains or losses (+/-)
cpsasec$stcg = 0
cpsasec$ltcg = 0

# otherprop other property income, including unearned partnership and s-corp
#           income, rent, non-qualified dividends, capital gains distributions
#           on form 1040 other income or loss not otherwise enumerated here
cpsasec$otherprop = cpsasec$rnt_val+cpsasec$div_val*ordinary.dividends/
  (qualified.dividends+ordinary.dividends)

# nonprop   other non-property income (not subject to medicare niit) such as
#           alimony, fellowships, state income tax refunds (itemizers only),
#           adjustments and items such as alimony paid, keogh and ira
#           contributions foreign income exclusion nols can be entered here as
#           negative income (+/-)
cpsasec$nonprop = cpsasec$oi_val+cpsasec$ed_val

# pensions  taxable pensions and ira distributions
# cpsasec$pensions = cpsasec$ann_val+cpsasec$dbtn_val+cpsasec$pnsn_val
cpsasec$pensions = cpsasec$rtm_val

# gssi      gross social security benefits
# include social security (ss), supplemental social security (ssi), survivors
# benefits (srvs), and disability (dsab)
cpsasec$gssi = cpsasec$ss_val+cpsasec$ssi_val+cpsasec$srvs_val+cpsasec$dsab_val

# ui        unemployment compensation received
cpsasec$ui = cpsasec$uc_val

# transfers other non-taxable transfer income such as welfare, workers comp,
#           veterans benefits, child support that would affect eligibility for
#           state property tax rebates but would not be taxable at the federal
#           level
# include public assistance and welfare (paw), workers compensation (wc),
# vetrans benefits (vet), child support (csp), and financial assistance (fin)
cpsasec$transfers = cpsasec$paw_val+cpsasec$wc_val+cpsasec$vet_val+
  cpsasec$csp_val+cpsasec$fin_val

# add spouses non-earnings income together
vars = c("dividends","intrec","otherprop","nonprop","pensions","gssi","ui",
         "transfers")
for (v in vars) {
  spouse = cpsasec[cpsasec$spouse_row,v]
  spouse[is.na(spouse)] = 0
  cpsasec[,v] = cpsasec[,v]+spouse
}

# make sure that spouse wage of a spouse matches the primary's wage
if (any(cpsasec$pwage!=cpsasec$swage[cpsasec$spouse_row],na.rm=TRUE))
  warning("mismatch in spouse wages")

# verify that spouses have the same non-wage income
if (any(cpsasec[,vars]!=cpsasec[cpsasec$spouse_row,vars],na.rm=TRUE))
  warning("non-wage income not equal across spouses")



# ------------------------------------------------------------------------------
# get the number of tax filers per household
# ------------------------------------------------------------------------------

# property tax is only available for households so we distribute evenly over tax
# filing units per household - therefore, we need to get the number of distinct
# tax filing unit per household

# aggregate the number of filers per household by filing type
hh_num = aggregate(rep(1,nrow(cpsasec)),
                   by=list(cpsasec$h_seq,cpsasec$filestat),sum)
names(hh_num) = c("h_seq","filestat","count")

# aggregate all joint filers into a single group with code 1
hh_num$filestat[hh_num$filestat<=3] = 1
hh_num = aggregate(hh_num$count,
                   by=list(hh_num$h_seq,hh_num$filestat),sum)
names(hh_num) = c("h_seq","filestat","count")

# divide joing filer count by 2 to get to filing units
hh_num$count[hh_num$filestat==1] = hh_num$count[hh_num$filestat==1]/2

# remove non-filers from final count by zeroing them out
hh_num$count[hh_num$filestat==6] = 0

# final count of filing units per household
hh_num = aggregate(hh_num$count,by=list(hh_num$h_seq),sum)
names(hh_num) = c("h_seq","h_filer_count")
cpsasec = merge(cpsasec,hh_num,all.x=TRUE,sort=FALSE)



# ------------------------------------------------------------------------------
# deductions
# ------------------------------------------------------------------------------

# rentpaid  rent paid (used only for calculating state property tax rebates)
# not available in the cps
cpsasec$rentpaid = 0

# proptax   real estate taxes paid. this is a preference for the amt and is is
#           also used to calculate state property tax rebates
# household value is split evenly between tax filing units
cpsasec$proptax = cpsasec$prop_tax/cpsasec$h_filer_count

# otheritem other itemized deductions that are a preference for the alternative
#           minimum tax. these would include other state and local taxes
#           (line 8 of schedule a) plus local income tax and preference share of
#           medical expenses
cpsasec$otheritem = 0

# childcare child care expenses
# the cps includes this information at the household level but assume 0 and use
# the amount implicitly included in the census imputed deductions instead
cpsasec$childcare = 0

# applicability amounts for the personal exemption phase out
# *** the personal exemption is currently 0 under the TCJA so this doesn't 
# *** matter but would need to be updated if that changes
cpsasec$pexemp_cap = 250000*(cpsasec$mstat==8)+
                     275000*(cpsasec$mstat==1)+
                     300000*(cpsasec$mstat==2)

# define the personal exemption amount for each filer including the phase out
cpsasec$pexemp = (cpsasec$depx+1)*pexemp*
                 pmax((1-pmax(cpsasec$agi-cpsasec$pexemp_cap,0)/2500*.02),0)

# mortgage  deductions not included in otheritem and not a preference for the
#           amt, including (on schedule a for 2009), such as deductible medical
#           expenses, motor vehicle taxes paid, home mortgage interest,
#           charitable contributions, casualty or theft losses
# use the census imputation when possible
cpsasec$mortgage = cpsasec$agi-cpsasec$tax_inc-cpsasec$pexemp

# add the standard deductions to the dataset
cpsasec$std_deduct = si.std.deduct*(cpsasec$mstat==8)+
                     hh.std.deduct*(cpsasec$mstat==1)+
                     mar.std.deduct*(cpsasec$mstat==2)

# check if there are any cases where the "imputed deductions" are negative
# (i.e., zero taxable income) but that shouldn't be the case based on personal
# exemptions and standard deduction alone - not impossible but a high value
# here would give concern about the imputed deductions
print(paste0("Percent of zero taxable income filers not based on exemptions ",
             "and standard deductions alone: ",
             signif(sum((cpsasec$agi>cpsasec$std_deduct+cpsasec$pexemp &
                           cpsasec$mortgage<0)[cpsasec$mstat>0])/
                      sum((cpsasec$mortgage<0)[cpsasec$mstat>0]),2)*100,"%"))

# remove salt deductions from potential itemized deductions so they are not
# overestimated
cpsasec$mortgage = cpsasec$mortgage-cpsasec$prop_tax-cpsasec$statetax_a

# assume that filers with no apparent taxable income have no itemized deductions
cpsasec$mortgage[cpsasec$mortgage<0] = 0

# populate deductions for spouses if married and not primary
cpsasec$mortgage = pmax(cpsasec$mortgage,cpsasec$mortgage[cpsasec$spouse_row],
                        na.rm=TRUE)



# ------------------------------------------------------------------------------
# function for runing taxsim
# ------------------------------------------------------------------------------

get_results = function(data, taxsim_file) {
  
  # create unique file name for upload to taxsim
  y = regexpr('[0-9][0-9][:punct:][0-9][0-9][:punct:][0-9][0-9]',Sys.time(),TRUE)
  z = y+attr(y,"match.length")-1
  time = unlist(strsplit(substr(Sys.time(),y,z),":"))
  time2 = paste(time,collapse="")
  taxsim_file = paste0(time2,Sys.getpid())
  
  # write the file to be uploaded to taxsim
  write_csv(data, taxsim_file, progress = FALSE)
  
  # upload the taxsim input file and retrieve the results
  req = request(taxsim_url) %>%
        req_body_file(taxsim_file) %>%
        req_perform()
  
  # convert the csv output of taxsim into a tibble
  tax_data = req %>%
             resp_body_raw() %>%
             read_csv(show_col_types = FALSE)
  
  # remove the taxsim input file
  file.remove(taxsim_file)
  
  return(tax_data)
  
}



run_taxsim = function(cpsasec, marginal_type = 11, detail = 0) {
  
  # variables used by taxsim
  taxsim_vars = c("taxsimid","year","state","mstat","page","sage","depx",
                  "dep13","dep17","dep18","pwages","swages","dividends",
                  "intrec","stcg","ltcg","otherprop","nonprop","pensions",
                  "gssi","ui","transfers","rentpaid","proptax","otheritem",
                  "childcare","mortgage")
  
  # create the data set for taxsim
  data = cpsasec %>%
         rename(year = yeartax) %>%
         filter(filestat<6) %>%
         select(all_of(taxsim_vars)) %>%
         mutate(mtr = marginal_type,
                idtl = detail,
                dividends = round(dividends),
                otherprop = round(otherprop))
  
  # using taxsim over ftp runs into both problems with the epa firewall and the
  # newer versions of Rcurl implement retries in a way that doesn't work well 
  # with taxsim where the file doesn't exist until the download is requested.
  # using http through a post of the data file works and doesn't get stopped by
  # the firewall, but the client fails if the file has many more than 1900 rows.
  # so we chunk the operation to get around that, which is slower than it needs 
  # to be but increases the reliability
  chunks = split(seq(nrow(data)), seq(nrow(data)) %/% 1000)
  pb = progress_bar$new(total = length(chunks))
  tax_data = NULL
  for (i in seq_along(chunks)) {
    pb$tick()
    tax_data = rbind(tax_data, get_results(data[chunks[[i]], ], taxsim_file))
  }
  
  return(tax_data)
  
}




# ------------------------------------------------------------------------------
# earnings weighted effective marginal tl tax rates by state and household type
# ------------------------------------------------------------------------------

# run taxsim for marginal taxes on wages and report version
tax_data = run_taxsim(cpsasec, marginal_type = 11, detail = 2)

# total wages tax rate
tax_data$rate = tax_data$frate+tax_data$srate

# merge additional cps data into the taxsim results
tax_data = merge(tax_data,
                 cpsasec[,c("taxsimid","pwages","marsupwt","htotval",
                            "gestfips")],
                 by.x="taxsimid",by.y="taxsimid",sort=FALSE)

# average tax rate
tax_data = tax_data %>%
           rename(agi = v10) %>%
           mutate(tl_avg = case_when(pwages == 0 ~ 0,
                                     pwages != 0 ~ (fiitax+siitax)/htotval))

# adjust the tax rates to account for the fact that taxes are not paid on the
# employers share of fica taxes since they will be multiplied by the full
# compensation in the model, including the employers share
tax_data$rate = tax_data$rate*(1/(1+tax_data$ficar/2/100))
tax_data$tl_avg = tax_data$tl_avg*(1/(1+tax_data$ficar/2/100))
tax_data$ficar = tax_data$ficar*(1/(1+tax_data$ficar/2/100))

# calculate the earnings weighted effective marginal tax rates
# filers with no earnings are given a fica rate of 0 by taxsim even though their
# first dollar of income would be taxed at the statutory rate. this doesn't
# matter for the earnings weighted average but for other averaging methods this
# would need to be taken into consideration
tax_data$tl = tax_data$rate*tax_data$pwages*tax_data$marsupwt
tax_data$tp = tax_data$ficar*tax_data$pwages*tax_data$marsupwt

# calculate the earnings weighted average tax rates. the average tax rate for
# each return is based on total income but the weighting is done via primary
# wages since each joint return show up twice switching the primary and
# secondary wage earners
tax_data$tl_avg = tax_data$tl_avg*tax_data$pwages*tax_data$marsupwt

# add implan household label to each tax unit
bins = c(-Inf,hh.inc$value)
for (i in 1:nrow(hh.inc))
  tax_data$hh[tax_data$htotval>bins[i] &
                tax_data$htotval<=bins[i+1]] = hh.inc$label[i]

# get state average earnings weighted effective marginal tax rates
state.tax = aggregate(tax_data[,c("tl","tp","tl_avg")],
                      by=list(tax_data$gestfips,tax_data$hh),sum)
names(state.tax)[1:2] = c("fips","hh")
state.tax$wgt = aggregate(tax_data$pwages*tax_data$marsupwt,
                          by=list(tax_data$gestfips,tax_data$hh),sum)$x
state.tax$tl = state.tax$tl/state.tax$wgt
state.tax$tp = state.tax$tp/state.tax$wgt
state.tax$tl_avg = state.tax$tl_avg/state.tax$wgt

# add the state abbreviations to the data using the fips codes
state.tax$state = factor(state.tax$fips,levels = c(1L,  2L,  4L,  5L,  6L,
                                                   8L,  9L, 10L, 11L, 12L,
                                                   13L, 15L, 16L, 17L, 18L,
                                                   19L, 20L, 21L, 22L, 23L,
                                                   24L, 25L, 26L, 27L, 28L,
                                                   29L, 30L, 31L, 32L, 33L,
                                                   34L, 35L, 36L, 37L, 38L,
                                                   39L, 40L, 41L, 42L, 44L,
                                                   45L, 46L, 47L, 48L, 49L,
                                                   50L, 51L, 53L, 54L, 55L,
                                                   56L),
                         labels = c("al","ak","az","ar","ca",
                                    "co","ct","de","dc","fl",
                                    "ga","hi","id","il","in",
                                    "ia","ks","ky","la","me",
                                    "md","ma","mi","mn","ms",
                                    "mo","mt","ne","nv","nh",
                                    "nj","nm","ny","nc","nd",
                                    "oh","ok","or","pa","ri",
                                    "sc","sd","tn","tx","ut",
                                    "vt","va","wa","wv","wi",
                                    "wy"))

# limit the fica tax rate to the statutory rate to avoid rounding errors
fica.rate = 15.3
state.tax$tp = pmin(state.tax$tp,fica.rate)

# convert to fractions from percentages
state.tax$tl = state.tax$tl/100
state.tax$tp = state.tax$tp/100

# add the payroll taxes in the the wage income taxes
state.tax$tlp = state.tax$tl+state.tax$tp

# reorder
state.tax$fips = NULL
state.tax = state.tax[,c("state","hh","tl","tp","tl_avg","tlp")]

# dump state labor tax information into a csv file
write.csv(state.tax, file="labor_tax_rates.csv", row.names=FALSE)



# ------------------------------------------------------------------------------
# plot the state by household tax rates
# ------------------------------------------------------------------------------

# set the order of the household labels to be increasing in income
state.tax$hh = factor(state.tax$hh,hh.inc$label)

# convert the tax rate to a percentage
state.tax$tl = state.tax$tl*100
state.tax$tlp = state.tax$tlp*100
state.tax$tl_avg = state.tax$tl_avg*100
state.tax$tp = state.tax$tp*100

# plot the variation in wage tax rates by household types across states
p = ggplot(state.tax)+
  geom_boxplot(aes(x=hh,y=tl))+
  scale_y_continuous(breaks=round(seq(-40,60,by=5),1))+
  labs(y="Effective Marignal Income Tax Rate on Wage Earnings",x="Household")+
  theme(axis.line.x      = element_line(colour="black"),
        axis.line.y      = element_line(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        legend.box.just  = "left",
        legend.key       = element_blank())
p
ggsave(file.path("taxsim_marginal_labor_tax_tl.png"),device="png",width=8,
      height=6, limitsize=FALSE)


# plot the variation in payroll taxes rates by household types across states
p = ggplot(state.tax)+
  geom_boxplot(aes(x=hh,y=tp))+
  scale_y_continuous(breaks=round(seq(-40,60,by=2),1))+
  labs(y="Effective Payroll Tax Rate (FICA) on Wages",
       x="Household")+
  theme(axis.line.x      = element_line(colour="black"),
        axis.line.y      = element_line(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        legend.box.just  = "left",
        legend.key       = element_blank())
p
ggsave(file.path("taxsim_marginal_fica_tax_tfica.png"),device="png",width=8,
      height=6, limitsize=FALSE)

# plot the variation in average tax rates by household types across states
p = ggplot(state.tax)+
  geom_boxplot(aes(x=hh,y=tl_avg))+
  scale_y_continuous(breaks=round(seq(-40,60,by=5),1))+
  labs(y="Average Personal Income Tax Rate",x="Household")+
  theme(axis.line.x      = element_line(colour="black"),
        axis.line.y      = element_line(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        legend.box.just  = "left",
        legend.key       = element_blank())
p
ggsave(file.path("taxsim_average_labor_tax_tl_avg.png"),device="png",width=8,
      height=6, limitsize=FALSE)



# ------------------------------------------------------------------------------
# earnings weighted effective marginal personal income tax rate on capital
# ------------------------------------------------------------------------------

# run taxsim for marginal taxes on interest income
tax_data_k = run_taxsim(cpsasec, marginal_type = 14)
tax_data_k$int = tax_data_k$frate+tax_data_k$srate

# run taxsim for marginal taxes on qualified dividends
# taxsim doesn't appear to have a margin type for dividends (which are defined
# as qualified in taxsim) so use the long term capital gains margin instead
temp = run_taxsim(cpsasec, marginal_type=70)
tax_data_k$div = temp$frate+temp$srate

# run taxsim for marginal taxes on other property income (including ordinary
# dividends)
temp = run_taxsim(cpsasec, marginal_type=17)
tax_data_k$prop = temp$frate+temp$srate

# merge additional cps data into the taxsim results
tax_data_k = merge(tax_data_k,
                   cpsasec[,c("taxsimid","dividends","intrec","marsupwt",
                              "htotval","gestfips","otherprop")],
                   by.x="taxsimid",by.y="taxsimid",sort=FALSE)

# earnings weighted national interest income tax rate
tk.int = sum(tax_data_k$int*tax_data_k$intrec*tax_data_k$marsupwt)/
  sum(tax_data_k$intrec*tax_data_k$marsupwt)

# earnings weighted national qualified dividend income tax rate
tk.div = sum(tax_data_k$div*tax_data_k$dividends*tax_data_k$marsupwt)/
  sum(tax_data_k$dividends*tax_data_k$marsupwt)

# earnings weighted national property income tax rate
tk.prop = sum(tax_data_k$prop*tax_data_k$otherprop*tax_data_k$marsupwt)/
  sum(tax_data_k$otherprop*tax_data_k$marsupwt)



# ------------------------------------------------------------------------------
# corporate income tax rate
# ------------------------------------------------------------------------------

# tax rate - corporate income tax
# from cbo (2017) international comparison of corporate income tax rates
# https://www.cbo.gov/system/files/115th-congress-2017-2018/reports/52419-internationaltaxratecomp.pdf
tk.corp = 18.6



# ------------------------------------------------------------------------------
# effective marginal tax rate of capital income
# ------------------------------------------------------------------------------

# weighted average for the effective marginal tax rate on capital assuming that
# interest and dividend income is taxed both as corporate income and personal
# income and that sole proprietorship and s corporation income is only tax as
# personal income where weights are based on 1040 line item totals
tk = data.frame(r=unique(state.tax$state),
                value=((tk.corp+tk.int)*interest+
                         (tk.corp+tk.div)*qualified.dividends+
                         (tk.corp+tk.prop)*ordinary.dividends+
                         tk.prop*(prop.income+cap.gains.1040+business.income))/
                  (interest+ordinary.dividends+qualified.dividends+
                     prop.income+cap.gains.1040+business.income)/100)

# dump state capital tax information into a csv file
write.csv(tk, file="capital_tax_rates.csv", row.names=FALSE)



# ------------------------------------------------------------------------------
# end
# ------------------------------------------------------------------------------
