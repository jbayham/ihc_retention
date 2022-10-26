################################################################################
################################################################################
# (3) Join data ~ Left join salary~fips/area~locality adjustment to dataPRD ####
################################################################################
################################################################################
#library(dplyr)
dataPRD <- readRDS(file = "build/cache/PRD_IHC.rds")

# Contains salary data by crew by year (res_id_primary and year are the joining columns)
filepathSalary <- "build/inputs/IHC_salary_estimates.csv"

# Contains fips codes (code) and area name (area1) PER year (year)
fileFIPSandCode <- "build/inputs/FIPScodeandNames.txt"

# Contains fips and crew level res_id
filepathFIPS <- "build/inputs/IHC_home_base_locations_wFIPS.txt"

# Contains competing wages (from Jude)
filepathCompetingWages <- "build/cache/hb_occ.csv"

# salary data (crew level)
dataSalary <- read.csv(filepathSalary)

LocPayALL <- readRDS("build/cache/LocPayALL.rds")

# ALL FIPS codes for each area name
dataFIPSandCodes <- read.csv(fileFIPSandCode,sep="\t",header=TRUE)
for(i in 2:nrow(dataFIPSandCodes)) if(dataFIPSandCodes$area1[i]=="") dataFIPSandCodes$area1[i] <- dataFIPSandCodes$area1[i-1]  
dataFIPSandCodes <- dataFIPSandCodes[complete.cases(dataFIPSandCodes),]

# crew level fips codes 
dataFIPS <- read.csv(filepathFIPS, sep="\t")

# read in competing wages data and subset the necessary columns
dataCompetingWages <- read.csv(filepathCompetingWages)
colnames(dataCompetingWages)[3] <- "IHC_res_ID"
dataCompetingWages <- subset(dataCompetingWages, select = c("IHC_res_ID","year","med_wage"))

# Join homebase/FIPS 
# with personal retention data by crew id
# change column to IHC_res_ID to match column name in dataPRD
colnames(dataFIPS)[3] <- "IHC_res_ID"
dataPRD <- left_join(dataPRD, 
                     dataFIPS, 
                     by = c("IHC_res_ID"="IHC_res_ID"))

# for Alaska FIPS numbers (change from 2000s to 2)
dataPRD$FIPS[dataPRD$FIPS == 2090] <- 2
dataPRD$FIPS[dataPRD$FIPS == 2170] <- 2
# new FOR KERN (Kern included in LA so change Kern fips to LA fips)
dataPRD$FIPS[dataPRD$FIPS == 6029] <- 6037
# make fips an integer
dataPRD$FIPS <- as.integer(dataPRD$FIPS)

#
##
# merge Salary data (crew level)
# with PRD data using "IHC_res_ID" crew id
colnames(dataSalary)[3] <- "IHC_res_ID"
dataPRD <- left_join(x=dataPRD, 
                     y=dataSalary, 
                     by = c("IHC_res_ID","year"))

#
##
# merge FIPS/area code 
# with PRD data
colnames(dataFIPSandCodes)[1] <- "FIPS"
colnames(dataFIPSandCodes)[2] <- "area1"
dataFIPSandCodes$FIPS <- as.integer(dataFIPSandCodes$FIPS)
# change name to similar naming convention 
dataFIPSandCodes$area1[dataFIPSandCodes$area1 == "Alaska"] <- "ALASKA"  
LocPayALL$area1[LocPayALL$area1 == "STATE OF ALASKA"] <- "ALASKA"  

# join fips code which contains area name to PRD dataset
dataPRD <- left_join(x=dataPRD, 
                     y=dataFIPSandCodes, 
                     by = c("FIPS","year"))#by=c("FIPS"="FIPS"))

# change area1 names to all upper case for similarity 
dataPRD$area1 <- toupper(dataPRD$area1)

###Correct name mismatch###
# change names to match LocPayAll area name ~ mismatch naming convention 
dataPRD$area1[dataPRD$area1 == "SACRAMENTO--ARDEN-ARCADE--YUBA CITY, CA-NV"] <- "SACRAMENTO-ROSEVILLE, CA-NV"  
dataPRD$area1[dataPRD$area1 == "WASHINGTON-BALTIMORE-NORTHERN VIRGINIA, DC-MD-VA-WV-PA"] <- "WASHINGTON-BALTIMORE-ARLINGTON, DC-MD-VA-WV-PA"    
dataPRD$area1[dataPRD$area1 == "DENVER-AURORA-BOULDER, CO"] <- "DENVER-AURORA, CO"    
dataPRD$area1[dataPRD$area1 == "SEATTLE-TACOMA-OLYMPIA, WA"] <- "SEATTLE-TACOMA, WA"    
dataPRD$area1[dataPRD$area1 == "LOS ANGELES-LONG BEACH-RIVERSIDE, CA"] <- "LOS ANGELES-LONG BEACH, CA"    
dataPRD$area1[dataPRD$area1 == "PORTLAND-VANCOUVER-BEAVERTON, OR-WA"] <- "PORTLAND-VANCOUVER-SALEM, OR-WA"    
dataPRD$area1[dataPRD$area1 == "PORTLAND-VANCOUVER-HILLSBORO, OR-WA"] <- "PORTLAND-VANCOUVER-SALEM, OR-WA"    
dataPRD$area1[dataPRD$area1 == "CHICAGO-NAPERVILLE-MICHIGAN CITY, IL-IN-WI"] <- "CHICAGO-NAPERVILLE, IL-IN-WI"    
dataPRD$area1[dataPRD$area1 == "SAN DIEGO-CARLSBAD-SAN MARCOS, CA"] <- "SAN DIEGO-CARLSBAD, CA"    
###end Correct name mismatch###

dataPRD <- left_join(x=dataPRD, 
                     y=LocPayALL, by=c("area1","year"))

dataPRD$area1[is.na(dataPRD$area1)]<-"REST OF U.S."

dataPRD <- left_join(x=dataPRD, 
                     y=LocPayALL, by=c("area1","year"))

dataPRD <- left_join(x=dataPRD, 
                     y=dataCompetingWages, by=c("IHC_res_ID","year"))

# check for NAs and summary of dataset
any(is.na(dataPRD$med_wage))
sum(is.na(dataPRD$med_wage))
length(unique(dataPRD$res_id))

saveRDS(dataPRD, file = "build/cache/dataPRD_join.rds")

remove <- c("dataCompetingWages","dataFIPS","dataFIPSandCodes","dataPRD","dataSalary","LocPayALL",
            "fileFIPSandCode","filepathCompetingWages","filepathFIPS","filepathSalary","i","remove")
rm(list = remove)

################################################################################
################################################################################
################################################################################