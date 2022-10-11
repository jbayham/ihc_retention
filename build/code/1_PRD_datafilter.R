################################################################################
################################################################################
# (1) filepath to Personnel retention data #####################################
################################################################################
################################################################################
# filter PRD data and calculate basic statistics 

# Filepath to personnel retention data (only interested in IHC for this project)
filepathPRD <- "./build/inputs/Personnel Retentaion data 2008 to 2019 IHCs only.csv"

# read and filter personal retention data
# select only "IHC" and res and crew ideas greater than 0
# select IHC where days assigned >28
dataPRD <- read.csv(filepathPRD)
dataPRD <- dataPRD[dataPRD$personnel_type == "IHC", ]
dataPRD <- dataPRD[dataPRD$res_id > 0, ]
dataPRD <- dataPRD[dataPRD$IHC_res_ID > 0, ]
dataPRD <- dataPRD[dataPRD$days_assigned > 28, ]

# remove fatalities 
dataPRD <- dataPRD[dataPRD$res_id != 574054, ]
dataPRD <- dataPRD[dataPRD$res_id != 451135, ]
dataPRD <- dataPRD[dataPRD$res_id != 863525, ]
dataPRD <- dataPRD[dataPRD$res_id != 354944, ]
dataPRD <- dataPRD[dataPRD$res_id != 1721363, ]
dataPRD <- dataPRD[dataPRD$res_id != 61950, ]

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


# cumulative year (corrects for skip years)
dataPRD$cumusumYEARMarker <-  1
dataPRD <- dataPRD%>%dplyr::arrange(res_id,year)%>%dplyr::group_by(res_id)%>%dplyr::mutate(cumusumYEAR=cumsum(cumusumYEARMarker))
dataPRD <- dataPRD%>%dplyr::arrange(res_id,year)%>%dplyr::group_by(res_id)%>%dplyr::mutate(cumusumDA=cumsum(days_assigned))
#dataPRD <- dataPRD%>%dplyr::arrange(res_id,year)%>%dplyr::group_by(res_id)%>%dplyr::mutate(cumusumSFF=cumsum(SeniorFFxLoc))
#dataPRD <- dataPRD%>%dplyr::arrange(res_id,year)%>%dplyr::group_by(res_id)%>%dplyr::mutate(cumusumTH=cumsum(total_hours))

# clean global environments
remove <- c("maxyear","minyear","TOTALDAYS","TOTALYEARS","AveDAYPerYEARS")
rm(list = remove)

saveRDS(dataPRD, file = "build/cache/PRD_IHC.rds")

remove <- c("dataPRD","filepathPRD","remove")
rm(list = remove)

################################################################################
################################################################################
################################################################################

