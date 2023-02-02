################################################################################
################################################################################
# (2) locality payment #########################################################
################################################################################
################################################################################
# not the cleanest way but 
# an easy work around to the missing xmls on the internet
# Manually copied data from this website:
# https://www.federalpay.org/gs/locality

# each year file contains an area (the joining column) 
# and locality adjustment percent for each year

# Run for each year 
LocPay2008 <- read.csv("build/inputs/LocalityPay2008.txt", sep="\t", header = FALSE)
LocPay2009 <- read.csv("build/inputs/LocalityPay2009.txt", sep="\t", header = FALSE)
LocPay2010 <- read.csv("build/inputs/LocalityPay2010.txt", sep="\t", header = FALSE)
LocPay2011 <- read.csv("build/inputs/LocalityPay2011.txt", sep="\t", header = FALSE)
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
LocPayALL <- rbind(LocPay2008,
                   LocPay2009,
                   LocPay2010,
                   LocPay2011,
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
remove <- c("LocPay2008", "LocPay2009","LocPay2010", "LocPay2011",
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

saveRDS(LocPayALL, file = "build/cache/LocPayALL.rds")

remove <- c("LocPayALL","remove")
rm(list = remove)

################################################################################
################################################################################
################################################################################

