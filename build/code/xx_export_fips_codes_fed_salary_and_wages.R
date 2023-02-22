

# Could add weighted competing wages, but don't have time now
# see script 02 for code to do that.

# Contains fips codes (code) and area name (area1) PER year (year)
#only has codes for the non-rest of us areas
fileFIPSandCode_special_areas <- "build/inputs/FIPScodeandNames.txt"

# contains all FIPS codes (including Rest of US)
fileFIPSCodes = "build/inputs/FIPS state and county codes.xlsx"

# Run for each year 
# FIPS code info for COLA areas only goes back to 2011 and there's weirdness in 2011
#LocPay2008 <- read.csv("build/inputs/LocalityPay2008.txt", sep="\t", header = FALSE)
#LocPay2009 <- read.csv("build/inputs/LocalityPay2009.txt", sep="\t", header = FALSE)
#LocPay2010 <- read.csv("build/inputs/LocalityPay2010.txt", sep="\t", header = FALSE)
#LocPay2011 <- read.csv("build/inputs/LocalityPay2011.txt", sep="\t", header = FALSE)
LocPay2012 <- read.csv("build/inputs/LocalityPay2012.txt", sep="\t", header = FALSE)
LocPay2013 <- read.csv("build/inputs/LocalityPay2013.txt", sep="\t", header = FALSE)
LocPay2014 <- read.csv("build/inputs/LocalityPay2014.txt", sep="\t", header = FALSE)
LocPay2015 <- read.csv("build/inputs/LocalityPay2015.txt", sep="\t", header = FALSE)
LocPay2016 <- read.csv("build/inputs/LocalityPay2016.txt", sep="\t", header = FALSE)
LocPay2017 <- read.csv("build/inputs/LocalityPay2017.txt", sep="\t", header = FALSE)
LocPay2018 <- read.csv("build/inputs/LocalityPay2018.txt", sep="\t", header = FALSE)
LocPay2019 <- read.csv("build/inputs/LocalityPay2019.txt", sep="\t", header = FALSE)
LocPay2020 <- read.csv("build/inputs/LocalityPay2020.txt", sep="\t", header = FALSE)
LocPay2021 <- read.csv("build/inputs/LocalityPay2021.txt", sep="\t", header = FALSE)
LocPay2022 <- read.csv("build/inputs/LocalityPay2022.txt", sep="\t", header = FALSE)

# this is to make a table to join
LocPayALL <- rbind(#LocPay2008,
                   #LocPay2009,
                   #LocPay2010,
                   #LocPay2011,
                   LocPay2012,
                   LocPay2013,
                   LocPay2014,
                   LocPay2015,
                   LocPay2016,
                   LocPay2017,
                   LocPay2018,
                   LocPay2019,
                   LocPay2020,
                   LocPay2021,
                   LocPay2022)

# remove individual year variables from global environment 
remove <- c(#"LocPay2008", "LocPay2009","LocPay2010", "LocPay2011",
            "LocPay2012", "LocPay2013","LocPay2014", "LocPay2015",
            "LocPay2016", "LocPay2017","LocPay2018", "LocPay2019", 
            "LocPay2020","LocPay2021", "LocPay2022")
rm(list = remove)

# change column names
# the locality payment table is structured as such:
# each row contains an area name, a locality payment, and a year
# ~ a locality payment per area per year
colnames(LocPayALL) <- c('area1', 'area2', 'code','LocoAdj','year')
# remove % and convert to numeric 
LocPayALL$LocoAdj<-gsub("/$|%","",LocPayALL$LocoAdj)
LocPayALL$LocoAdj <- as.numeric(LocPayALL$LocoAdj)



# ALL FIPS codes for each special area name (no rest of us)
dataFIPSandCodes_wo_rus <- read.csv(fileFIPSandCode_special_areas,sep="\t",header=TRUE)
for(i in 2:nrow(dataFIPSandCodes_wo_rus)) if(dataFIPSandCodes_wo_rus$area1[i]=="") dataFIPSandCodes_wo_rus$area1[i] <- dataFIPSandCodes_wo_rus$area1[i-1]  
dataFIPSandCodes_wo_rus <- dataFIPSandCodes_wo_rus[complete.cases(dataFIPSandCodes_wo_rus),]
colnames(dataFIPSandCodes_wo_rus)[1] <- "FIPS"
colnames(dataFIPSandCodes_wo_rus)[2] <- "area1"
dataFIPSandCodes_wo_rus$FIPS <- as.integer(dataFIPSandCodes_wo_rus$FIPS)


# all FIPS codes (for Rest of US)
allCountyFIPS = read_excel(fileFIPSCodes, sheet = "County codes") %>% clean_names("snake")
allStateFIPS = read_excel(fileFIPSCodes, sheet = "State codes") %>% clean_names("snake")

allYears = c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)

allYearsAllFIPS = crossing(allYears, allCountyFIPS$county_level_fips_code) %>%
  rename("county_level_fips_code"="allCountyFIPS$county_level_fips_code",
         "year" = "allYears") %>%
  left_join(allCountyFIPS, by =c("county_level_fips_code" = "county_level_fips_code")) %>%
  # add state to county for ease of use later
  # plus expand so there's one entry per year
  mutate(state_level_fips_code = ifelse(nchar(as.character(county_level_fips_code)) == 5, 
                                        as.numeric(substr(as.character(county_level_fips_code), start = 1, stop = 2)),
                                        as.numeric(substr(as.character(county_level_fips_code), start = 1, stop = 1)))) %>%
  left_join(allStateFIPS, by = c("state_level_fips_code" = "state_level_fips_code")) %>%
  rename("county_name" = "place_name.x",
         "state_name" = "place_name.y") %>%
  # merge all FIPS with special FIPS
  left_join(dataFIPSandCodes_wo_rus %>% select(FIPS, area1, year),
            by = c("county_level_fips_code" = "FIPS",
                   "year" = "year")) 
  


# change name to same naming convention 
# also fixes the Alaska area issue in dataFIPSandCodes_wo_rus
allYearsAllFIPS$area1[allYearsAllFIPS$state_name == "ALASKA"] <- "ALASKA"  
LocPayALL$area1[LocPayALL$area1 == "STATE OF ALASKA"] <- "ALASKA"  

# change area1 names to all upper case for similarity 
allYearsAllFIPS$area1 <- toupper(allYearsAllFIPS$area1)

###Correct name mismatch###
# change names to match LocPayAll area name ~ mismatch naming convention (actually, new names later in the data set, but this works)
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "SACRAMENTO--ARDEN-ARCADE--YUBA CITY, CA-NV"] <- "SACRAMENTO-ROSEVILLE, CA-NV"  
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "WASHINGTON-BALTIMORE-NORTHERN VIRGINIA, DC-MD-VA-WV-PA"] <- "WASHINGTON-BALTIMORE-ARLINGTON, DC-MD-VA-WV-PA"    
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "DENVER-AURORA-BOULDER, CO"] <- "DENVER-AURORA, CO"    
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "SEATTLE-TACOMA-OLYMPIA, WA"] <- "SEATTLE-TACOMA, WA"    
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "LOS ANGELES-LONG BEACH-RIVERSIDE, CA"] <- "LOS ANGELES-LONG BEACH, CA"    
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "PORTLAND-VANCOUVER-BEAVERTON, OR-WA"] <- "PORTLAND-VANCOUVER-SALEM, OR-WA"    
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "PORTLAND-VANCOUVER-HILLSBORO, OR-WA"] <- "PORTLAND-VANCOUVER-SALEM, OR-WA"    
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "CHICAGO-NAPERVILLE-MICHIGAN CITY, IL-IN-WI"] <- "CHICAGO-NAPERVILLE, IL-IN-WI"    
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "SAN DIEGO-CARLSBAD-SAN MARCOS, CA"] <- "SAN DIEGO-CARLSBAD, CA"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "ALBANY-SCHENECTADY, NY"] <- "ALBANY-SCHENECTADY, NY-MA"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "ATLANTA-SANDY SPRINGS-GAINESVILLE, GA-AL"] <- "ATLANTA--ATHENS-CLARKE COUNTY--SANDY SPRINGS, GA-AL"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "HUNTSVILLE-DECATUR, AL"] <- "HUNTSVILLE-DECATUR-ALBERTVILLE, AL"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "NEW YORK-NEWARK-BRIDGEPORT, NY-NJ-CT-PA"] <- "NEW YORK-NEWARK, NY-NJ-CT-PA"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "HARTFORD-WEST HARTFORD-WILLIMANTIC, CT-MA"] <- "HARTFORD-WEST HARTFORD, CT-MA"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "PHILADELPHIA-CAMDEN-VINELAND, PA-NJ-DE-MD"] <- "PHILADELPHIA-READING-CAMDEN, PA-NJ-DE-MD"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "MIAMI-FORT LAUDERDALE-POMPANO BEACH, FL"] <- "MIAMI-FORT LAUDERDALE-PORT ST. LUCIE, FL"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "INDIANAPOLIS-ANDERSON-COLUMBUS, IN"] <- "INDIANAPOLIS-CARMEL-MUNCIE, IN"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "CINCINNATI-MIDDLETOWN-WILMINGTON, OH-KY-IN"] <- "CINCINNATI-WILMINGTON-MAYSVILLE, OH-KY-IN"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "BOSTON-WORCESTER-MANCHESTER, MA-NH-RI-ME"] <- "BOSTON-WORCESTER-PROVIDENCE, MA-RI-NH-ME"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "DETROIT-WARREN-FLINT, MI"] <- "DETROIT-WARREN-ANN ARBOR, MI"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "MINNEAPOLIS-ST. PAUL-ST. CLOUD, MN-WI"] <- "MINNEAPOLIS-ST. PAUL, MN-WI"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "BUFFALO-NIAGARA-CATTARAUGUS, NY"] <- "BUFFALO-CHEEKTOWAGA, NY"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "RALEIGH-DURHAM-CARY, NC"] <- "RALEIGH-DURHAM-CHAPEL HILL, NC"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "CLEVELAND-AKRON-ELYRIA, OH"] <- "CLEVELAND-AKRON-CANTON, OH"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "DAYTON-SPRINGFIELD-GREENVILLE, OH"] <- "DAYTON-SPRINGFIELD-SIDNEY, OH"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "COLUMBUS-MARION-CHILLICOTHE, OH"] <- "COLUMBUS-MARION-ZANESVILLE, OH"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "PITTSBURGH-NEW CASTLE, PA"] <- "PITTSBURGH-NEW CASTLE-WEIRTON, PA-OH-WV"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "HOUSTON-BAYTOWN-HUNTSVILLE, TX"] <- "HOUSTON-THE WOODLANDS, TX"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "DALLAS-FORT WORTH, TX"] <- "DALLAS-FORT WORTH, TX-OK"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "BOSTON-WORCESTER-PROVIDENCE, MA-RI-NH-CT-ME"] <- "BOSTON-WORCESTER-PROVIDENCE, MA-RI-NH-ME"
allYearsAllFIPS$area1[allYearsAllFIPS$area1 == "ALBANY-SCHENECTADY, NY"] <- "ALBANY-SCHENECTADY, NY-MA"



allYearsAllFIPS$area1[is.na(allYearsAllFIPS$area1)]<-"REST OF U.S."

allYearsAllFIPS <- left_join(x=allYearsAllFIPS, 
                     y=LocPayALL, by=c("area1","year"))


write_csv(x=allYearsAllFIPS %>% 
            select(year, county_level_fips_code, state_level_fips_code, county_name, state_name, LocoAdj, code,area1) %>%
            rename("locality_adjustment_percent" = "LocoAdj",
                   "locality_area_code" = "code",
                   "locality_area_name" = "area1"),
          file = "build/cache/locality_adjustments_for_UW.csv")





