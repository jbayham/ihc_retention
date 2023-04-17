################################################################################
################################################################################
# (1) filepath to Personnel retention data #####################################
################################################################################
################################################################################
# filter PRD data and calculate basic statistics 

# Filepath to personnel retention data (only interested in IHC for this project)
filepathPRD <- "./build/inputs/Personnel Retentaion data 2008 to 2019 IHCs only.csv"
#filepathPRD <- "C:\\Users\\magst\\Desktop\\git\\ihc_retention\\build\\inputs\\Personnel Retentaion data 2008 to 2019 IHCs only.csv"
# read and filter personal retention data
# select only "IHC" and res and crew ideas greater than 0
# select IHC where days assigned >28
dataPRD <- read.csv(filepathPRD)
dataPRD <- dataPRD[dataPRD$personnel_type == "IHC", ]
dataPRD <- dataPRD[dataPRD$res_id > 0, ]
dataPRD <- dataPRD[dataPRD$IHC_res_ID > 0, ]
dataPRD <- dataPRD[dataPRD$days_assigned > 28, ]


# a column for first year and last year 
# and total lifetime days assigned 
# and total years (year max - year min) doesn't account for skip years
# and average days assigned (total days/total years)
minyear <- aggregate(dataPRD$year, 
                     list(dataPRD$res_id), 
                     FUN=min)
colnames(minyear)[1] <- "res_id"

maxyear <- aggregate(dataPRD$year, 
                     list(dataPRD$res_id), 
                     FUN=max)
colnames(maxyear)[1] <- "res_id"

TOTALDAYS <- aggregate(dataPRD$days_assigned, 
                       list(dataPRD$res_id), 
                       FUN=sum)
colnames(TOTALDAYS)[1] <- "res_id"

TOTALYEARS <- aggregate(dataPRD$year, 
                        list(dataPRD$res_id), 
                        FUN=length)
colnames(TOTALYEARS)[1] <- "res_id"

AveDAYPerYEARS <- aggregate(dataPRD$days_assigned, 
                            list(dataPRD$res_id), 
                            FUN=mean)
colnames(AveDAYPerYEARS)[1] <- "res_id"

dataPRD <- merge(x=dataPRD, 
                 y=minyear, 
                 by="res_id")
dataPRD <- merge(x=dataPRD, 
                 y=maxyear, 
                 by="res_id")
dataPRD <- merge(x=dataPRD, 
                 y=TOTALDAYS, 
                 by="res_id")
dataPRD <- merge(x=dataPRD, 
                 y=TOTALYEARS, 
                 by="res_id")
dataPRD <- merge(x=dataPRD, 
                 y=AveDAYPerYEARS, 
                 by="res_id")

colnames(dataPRD)[6] <- "firstYear"
colnames(dataPRD)[7] <- "lastYear"
colnames(dataPRD)[8] <- "TOTALDAYS"
colnames(dataPRD)[9] <- "TotalYEARS"
colnames(dataPRD)[10] <- "AveDAYPerYEARS"

# quick data check on the cohort we're looking at
# how many people started in 2008 and were still there in 2012

tmp = dataPRD %>% select(res_id, firstYear, lastYear) %>% group_by(res_id, firstYear, lastYear) %>% summarise(count = n()) %>% ungroup()
length(tmp$res_id[tmp$firstYear == 2008 & tmp$lastYear>2011])

# cumulative year (corrects for skip years)
dataPRD$cumusumYEARMarker <-  1
dataPRD <- dataPRD%>%dplyr::arrange(res_id,year)%>%dplyr::group_by(res_id)%>%dplyr::mutate(cumusumYEAR=cumsum(cumusumYEARMarker))
dataPRD <- dataPRD%>%dplyr::arrange(res_id,year)%>%dplyr::group_by(res_id)%>%dplyr::mutate(cumusumDA=cumsum(days_assigned))


### 9/20/2022 ###
# final exit 
dataPRD["surv"] <- ifelse(dataPRD$lastYear==dataPRD$year,"1","0")

## 3/23/2023 - right censor fatalities instead of removing
dataPRD$surv = case_when( (dataPRD$res_id == 574054 |
                           dataPRD$res_id == 451135 |
                           dataPRD$res_id == 863525 |
                           dataPRD$res_id == 354944 |
                           dataPRD$res_id == 1721363 |
                           dataPRD$res_id == 61950) & 
                            dataPRD$surv == "1" ~ "0",
                          TRUE ~ dataPRD$surv)


dataPRD <- dataPRD %>%
  group_by(res_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(yearminusyear = lead(year,k=1, default = first(year)) - year,
         yearminusyear_lag = lead(year,k=1, default = first(year)) - year)

dataPRD$residDIFF <- ave(dataPRD$res_id, FUN = function(x) c(diff(x),0))

dataPRD$skipyear <- ifelse(dataPRD$yearminusyear>1 & dataPRD$residDIFF==0,1,0)

dataPRD$skipyear <- as.numeric(dataPRD$skipyear)

dataPRD$skipyear_lag = dplyr::lag(dataPRD$skipyear)

dataPRD$surv <- as.numeric(dataPRD$surv)

dataPRD$surv2 <- dataPRD$skipyear+dataPRD$surv

## Need to update start year and end year for reentry.
# There are two possible ways to treat reenter.
# First, reset their "clock" so that a re-entry is associated with t0=0 (i.e., change first year to year of reentry).
# Second, have them reenter in the cohort of people who have the same years of experience as they do.

#Shayne's original year_start and year_end
dataPRD = dataPRD %>% mutate(year_end = (year-firstYear)+1,
       year_start = (year-firstYear)) %>%
  # for those who reentered, set t0 to cohort with same years of experience
  mutate(year_start_reentry_older_cohort = cumusumYEAR - 1,
         year_end_reentry_older_cohort = cumusumYEAR) %>%
  # for those who reentered, to restart their clock, set their first_year_reentry_clock equal to the year of reentry
  # first year needs to be set to reentry year for all years after each skip
  mutate(first_year_reentry_clock = case_when(skipyear_lag == 1 ~ year, 
                                              firstYear == year ~ firstYear,
                                              TRUE ~ NA_integer_)) %>%
  arrange(res_id, year) %>%
  fill(first_year_reentry_clock) %>% # fills down by default
  mutate(year_start_reentry_clock = year - first_year_reentry_clock,
         year_end_reentry_clock = year - first_year_reentry_clock + 1)

#double check that reentry calculations went correctly
#tmp = dataPRD %>%
#  select(res_id, year,skipyear_lag, firstYear, first_year_reentry_clock,
#         year_start_reentry_clock,year_end_reentry_clock,
#         year_start_reentry_older_cohort,year_end_reentry_older_cohort)



# clean global environments
remove <- c("maxyear","minyear","TOTALDAYS","TOTALYEARS","AveDAYPerYEARS", "tmp")
rm(list = remove)

saveRDS(dataPRD, file = "build/cache/PRD_IHC.rds")

remove <- c("dataPRD","filepathPRD","remove")
rm(list = remove)

################################################################################
################################################################################
################################################################################

