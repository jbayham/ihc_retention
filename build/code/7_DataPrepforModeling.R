################################################################################
################################################################################
# (7) Data Prep for models #####################################################
################################################################################
################################################################################
# 
#library(dplyr)

sm_data <- read.csv(file = "build/cache/dataPRD_wIncomeBin.csv") %>%
  janitor::clean_names()

# for survival model -From Jude (start year/end year)
sm_data$year_end <- (sm_data$year-sm_data$first_year)+1
sm_data$year_start <- (sm_data$year-sm_data$first_year)
dataset <- sm_data

###
##
# this is to compute the surv for multiple exits and entries 
#dataset <- dataset %>%
#  group_by(res_id) %>%
#  arrange(year, .by_group = TRUE) %>%
#  mutate(yearminusyear = lead(year,k=1, default = first(year)) - year)

#dataset$residDIFF <- ave(dataset$res_id, FUN = function(x) c(diff(x),0))

#dataset$skipyear <- ifelse(dataset$yearminusyear>1 & dataset$residDIFF==0,1,0)

#dataset$surv2 <- dataset$skipyear+dataset$surv
#
##
###

# remove 2019 year, SAC, EACC, Alaska State, and Utah State
dataset <- dataset[dataset$year!=2019,]
dataset <- dataset[dataset$gacc_x!="GA-SAC",]
dataset <- dataset[dataset$gacc_x!="WI-EACC",]
dataset <- dataset[dataset$agency!="Alaska State",]
dataset <- dataset[dataset$agency!="Utah State",]

# average days assigned = cumulative days/cumulative years
dataset$AverageDaysAssigned <- dataset$cumusum_da/dataset$cumusum_year
dataset$AverageDaysAssignedROUND <- plyr::round_any(dataset$AverageDaysAssigned, 10, f = ceiling)
dataset <- dataset[dataset$AverageDaysAssigned<159,]
dataset$AverageDaysAssignedROUND <- base::factor(dataset$AverageDaysAssignedROUND)
dataset$days_assigned <- plyr::round_any(dataset$days_assigned, 10, f = ceiling)
dataset <- dataset[dataset$days_assigned<160,]

dataset$med_wageCOPY <- dataset$med_wage

# round cumulative days to nearest 100 and remove <1100 cumulative days
# it is one sample and causes the model to fail
dataset$cumulativedaysROUND <- plyr::round_any(dataset$cumusum_da, 100, f = floor)  
dataset <- dataset[dataset$cumulativedaysROUND<800,]
dataset$cumulativedaysROUND <- factor(dataset$cumulativedaysROUND)
dataset$year <- base::factor(dataset$year)
dataset$med_wage <- plyr::round_any(dataset$med_wage, 10000, f = ceiling) 

# set factors and organize levels
dataset$Wage <- dataset$senior_f_fx_loc
dataset$Wage <- as.factor(dataset$Wage)
dataset$Year <- as.factor(dataset$year)
dataset$AverageDaysAssigned <- as.factor(dataset$AverageDaysAssignedROUND)
dataset$GACC <- dataset$gacc_x
dataset$Agency <- dataset$agency
dataset$Agency <- factor(dataset$agency, levels=c('USFS', 'NPS', 'BLM', 'BIA'))
dataset$CumulativeDays <- dataset$cumulativedaysROUND
dataset$DaysAssigned <- factor(dataset$days_assigned)
dataset$CompetingWage <- factor(dataset$med_wage)
dataset$cumusum_year <- factor(dataset$cumusum_year, levels=c('11','10','9','8','7','6','5','4','3','2','1'))
dataset$total_years <- factor(dataset$total_years)


dataset$WageDifference <- dataset$med_wageCOPY-dataset$senior_f_fx_loc_copy
dataset$WageDifference2 <- plyr::round_any(dataset$WageDifference, 5000, f = ceiling) 

#dataset$WageDifference <- factor(dataset$WageDifference, levels=c("0","-30000","-20000","-10000","10000","20000","30000","40000","50000","60000"))

dataset$WageDifference2 <- factor(dataset$WageDifference2, levels=c("0","-30000","-25000","-20000","-15000","-10000","-5000","5000","10000","15000","20000","25000","30000","35000","40000","45000","50000","55000"))

#dataset$cumusum_year <- as.numeric(dataset$cumusum_year)

#dataset<- dataset[dataset$cumusum_year<11,]

#dataset$CumulativeYears <- factor(dataset$cumusum_year, levels = c("1","2","3","4","5","6","7","8","9","10","11"))

dataset$CumulativeExperience <- dataset$CumulativeDays

dataset$CompetingWageDifference <- dataset$WageDifference2
#dataset$CompetingWageDifference <- dataset$WageDifference


dataset$CompetingWage <- plyr::round_any(dataset$med_wageCOPY, 10000, f = ceiling)

dataset$CompetingWage <- as.factor(dataset$CompetingWage)

#dataset$Wage <- plyr::round_any(dataset$senior_f_fx_loc_copy, 5000, f = ceiling) 
#dataset$Wage <- as.factor(dataset$Wage)
#table(dataset$GACC)
#table(dataset$Agency)



saveRDS(dataset, file = "analysis/inputs/dataPRD_dataforModeling.rds")

remove <- c("sm_data","remove","dataset")
rm(list = remove)

################################################################################
################################################################################
################################################################################