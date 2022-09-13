################################################################################
################################################################################
# (6) To compute income bin for each year ######################################
################################################################################
################################################################################
library(cartography)

dataPRD3 <- readRDS(file = "./build/cache/dataPRD_wSURV.rds")

# for each year
dataPRD2012 <- dataPRD3[dataPRD3$year==2012,]
breaks2012 <- getBreaks(dataPRD2012$SeniorFFxLoc, nclass=10, method = "quantile")
#plot(breaks2012)
dataPRD2012$SeniorFFxLoc[dataPRD2012$SeniorFFxLoc >= breaks2012[1] & dataPRD2012$SeniorFFxLoc < breaks2012[2]] <- 0
dataPRD2012$SeniorFFxLoc[dataPRD2012$SeniorFFxLoc >= breaks2012[2] & dataPRD2012$SeniorFFxLoc < breaks2012[3]] <- 1
dataPRD2012$SeniorFFxLoc[dataPRD2012$SeniorFFxLoc >= breaks2012[3] & dataPRD2012$SeniorFFxLoc < breaks2012[4]] <- 2
dataPRD2012$SeniorFFxLoc[dataPRD2012$SeniorFFxLoc >= breaks2012[4] & dataPRD2012$SeniorFFxLoc < breaks2012[5]] <- 3
dataPRD2012$SeniorFFxLoc[dataPRD2012$SeniorFFxLoc >= breaks2012[5] & dataPRD2012$SeniorFFxLoc < breaks2012[6]] <- 4
dataPRD2012$SeniorFFxLoc[dataPRD2012$SeniorFFxLoc >= breaks2012[6] & dataPRD2012$SeniorFFxLoc < breaks2012[7]] <- 5
dataPRD2012$SeniorFFxLoc[dataPRD2012$SeniorFFxLoc >= breaks2012[7] & dataPRD2012$SeniorFFxLoc < breaks2012[8]] <- 6
dataPRD2012$SeniorFFxLoc[dataPRD2012$SeniorFFxLoc >= breaks2012[8] & dataPRD2012$SeniorFFxLoc < breaks2012[9]] <- 7
dataPRD2012$SeniorFFxLoc[dataPRD2012$SeniorFFxLoc >= breaks2012[9] & dataPRD2012$SeniorFFxLoc <= breaks2012[10]] <- 8
dataPRD2012$SeniorFFxLoc[dataPRD2012$SeniorFFxLoc >= breaks2012[10] & dataPRD2012$SeniorFFxLoc <= breaks2012[11]] <- 9
unique(dataPRD2012$SeniorFFxLoc)

dataPRD2013 <- dataPRD3[dataPRD3$year==2013,]
breaks2013 <- getBreaks(dataPRD2013$SeniorFFxLoc, nclass=10, method = "quantile")
#plot(breaks2013)
dataPRD2013$SeniorFFxLoc[dataPRD2013$SeniorFFxLoc >= breaks2013[1] & dataPRD2013$SeniorFFxLoc < breaks2013[2]] <- 0
dataPRD2013$SeniorFFxLoc[dataPRD2013$SeniorFFxLoc >= breaks2013[2] & dataPRD2013$SeniorFFxLoc < breaks2013[3]] <- 1
dataPRD2013$SeniorFFxLoc[dataPRD2013$SeniorFFxLoc >= breaks2013[3] & dataPRD2013$SeniorFFxLoc < breaks2013[4]] <- 2
dataPRD2013$SeniorFFxLoc[dataPRD2013$SeniorFFxLoc >= breaks2013[4] & dataPRD2013$SeniorFFxLoc < breaks2013[5]] <- 3
dataPRD2013$SeniorFFxLoc[dataPRD2013$SeniorFFxLoc >= breaks2013[5] & dataPRD2013$SeniorFFxLoc < breaks2013[6]] <- 4
dataPRD2013$SeniorFFxLoc[dataPRD2013$SeniorFFxLoc >= breaks2013[6] & dataPRD2013$SeniorFFxLoc < breaks2013[7]] <- 5
dataPRD2013$SeniorFFxLoc[dataPRD2013$SeniorFFxLoc >= breaks2013[7] & dataPRD2013$SeniorFFxLoc < breaks2013[8]] <- 6
dataPRD2013$SeniorFFxLoc[dataPRD2013$SeniorFFxLoc >= breaks2013[8] & dataPRD2013$SeniorFFxLoc < breaks2013[9]] <- 7
dataPRD2013$SeniorFFxLoc[dataPRD2013$SeniorFFxLoc >= breaks2013[9] & dataPRD2013$SeniorFFxLoc <= breaks2013[10]] <- 8
dataPRD2013$SeniorFFxLoc[dataPRD2013$SeniorFFxLoc >= breaks2013[10] & dataPRD2013$SeniorFFxLoc <= breaks2013[11]] <- 9
unique(dataPRD2013$SeniorFFxLoc)

dataPRD2014 <- dataPRD3[dataPRD3$year==2014,]
breaks2014 <- getBreaks(dataPRD2014$SeniorFFxLoc, nclass=10, method = "quantile")
#plot(breaks2014)
dataPRD2014$SeniorFFxLoc[dataPRD2014$SeniorFFxLoc >= breaks2014[1] & dataPRD2014$SeniorFFxLoc < breaks2014[2]] <- 0
dataPRD2014$SeniorFFxLoc[dataPRD2014$SeniorFFxLoc >= breaks2014[2] & dataPRD2014$SeniorFFxLoc < breaks2014[3]] <- 1
dataPRD2014$SeniorFFxLoc[dataPRD2014$SeniorFFxLoc >= breaks2014[3] & dataPRD2014$SeniorFFxLoc < breaks2014[4]] <- 2
dataPRD2014$SeniorFFxLoc[dataPRD2014$SeniorFFxLoc >= breaks2014[4] & dataPRD2014$SeniorFFxLoc < breaks2014[5]] <- 3
dataPRD2014$SeniorFFxLoc[dataPRD2014$SeniorFFxLoc >= breaks2014[5] & dataPRD2014$SeniorFFxLoc < breaks2014[6]] <- 4
dataPRD2014$SeniorFFxLoc[dataPRD2014$SeniorFFxLoc >= breaks2014[6] & dataPRD2014$SeniorFFxLoc < breaks2014[7]] <- 5
dataPRD2014$SeniorFFxLoc[dataPRD2014$SeniorFFxLoc >= breaks2014[7] & dataPRD2014$SeniorFFxLoc < breaks2014[8]] <- 6
dataPRD2014$SeniorFFxLoc[dataPRD2014$SeniorFFxLoc >= breaks2014[8] & dataPRD2014$SeniorFFxLoc < breaks2014[9]] <- 7
dataPRD2014$SeniorFFxLoc[dataPRD2014$SeniorFFxLoc >= breaks2014[9] & dataPRD2014$SeniorFFxLoc <= breaks2014[10]] <- 8
dataPRD2014$SeniorFFxLoc[dataPRD2014$SeniorFFxLoc >= breaks2014[10] & dataPRD2014$SeniorFFxLoc <= breaks2014[11]] <- 9
unique(dataPRD2014$SeniorFFxLoc)

dataPRD2015 <- dataPRD3[dataPRD3$year==2015,]
breaks2015 <- getBreaks(dataPRD2015$SeniorFFxLoc, nclass=10, method = "quantile")
#plot(breaks2015)
dataPRD2015$SeniorFFxLoc[dataPRD2015$SeniorFFxLoc >= breaks2015[1] & dataPRD2015$SeniorFFxLoc < breaks2015[2]] <- 0
dataPRD2015$SeniorFFxLoc[dataPRD2015$SeniorFFxLoc >= breaks2015[2] & dataPRD2015$SeniorFFxLoc < breaks2015[3]] <- 1
dataPRD2015$SeniorFFxLoc[dataPRD2015$SeniorFFxLoc >= breaks2015[3] & dataPRD2015$SeniorFFxLoc < breaks2015[4]] <- 2
dataPRD2015$SeniorFFxLoc[dataPRD2015$SeniorFFxLoc >= breaks2015[4] & dataPRD2015$SeniorFFxLoc < breaks2015[5]] <- 3
dataPRD2015$SeniorFFxLoc[dataPRD2015$SeniorFFxLoc >= breaks2015[5] & dataPRD2015$SeniorFFxLoc < breaks2015[6]] <- 4
dataPRD2015$SeniorFFxLoc[dataPRD2015$SeniorFFxLoc >= breaks2015[6] & dataPRD2015$SeniorFFxLoc < breaks2015[7]] <- 5
dataPRD2015$SeniorFFxLoc[dataPRD2015$SeniorFFxLoc >= breaks2015[7] & dataPRD2015$SeniorFFxLoc < breaks2015[8]] <- 6
dataPRD2015$SeniorFFxLoc[dataPRD2015$SeniorFFxLoc >= breaks2015[8] & dataPRD2015$SeniorFFxLoc < breaks2015[9]] <- 7
dataPRD2015$SeniorFFxLoc[dataPRD2015$SeniorFFxLoc >= breaks2015[9] & dataPRD2015$SeniorFFxLoc <= breaks2015[10]] <- 8
dataPRD2015$SeniorFFxLoc[dataPRD2015$SeniorFFxLoc >= breaks2015[10] & dataPRD2015$SeniorFFxLoc <= breaks2015[11]] <- 9
unique(dataPRD2015$SeniorFFxLoc)

dataPRD2016 <- dataPRD3[dataPRD3$year==2016,]
breaks2016 <- getBreaks(dataPRD2016$SeniorFFxLoc, nclass=10, method = "quantile")
#plot(breaks2016)
dataPRD2016$SeniorFFxLoc[dataPRD2016$SeniorFFxLoc >= breaks2016[1] & dataPRD2016$SeniorFFxLoc < breaks2016[2]] <- 0
dataPRD2016$SeniorFFxLoc[dataPRD2016$SeniorFFxLoc >= breaks2016[2] & dataPRD2016$SeniorFFxLoc < breaks2016[3]] <- 1
dataPRD2016$SeniorFFxLoc[dataPRD2016$SeniorFFxLoc >= breaks2016[3] & dataPRD2016$SeniorFFxLoc < breaks2016[4]] <- 2
dataPRD2016$SeniorFFxLoc[dataPRD2016$SeniorFFxLoc >= breaks2016[4] & dataPRD2016$SeniorFFxLoc < breaks2016[5]] <- 3
dataPRD2016$SeniorFFxLoc[dataPRD2016$SeniorFFxLoc >= breaks2016[5] & dataPRD2016$SeniorFFxLoc < breaks2016[6]] <- 4
dataPRD2016$SeniorFFxLoc[dataPRD2016$SeniorFFxLoc >= breaks2016[6] & dataPRD2016$SeniorFFxLoc < breaks2016[7]] <- 5
dataPRD2016$SeniorFFxLoc[dataPRD2016$SeniorFFxLoc >= breaks2016[7] & dataPRD2016$SeniorFFxLoc < breaks2016[8]] <- 6
dataPRD2016$SeniorFFxLoc[dataPRD2016$SeniorFFxLoc >= breaks2016[8] & dataPRD2016$SeniorFFxLoc < breaks2016[9]] <- 7
dataPRD2016$SeniorFFxLoc[dataPRD2016$SeniorFFxLoc >= breaks2016[9] & dataPRD2016$SeniorFFxLoc <= breaks2016[10]] <- 8
dataPRD2016$SeniorFFxLoc[dataPRD2016$SeniorFFxLoc >= breaks2016[10] & dataPRD2016$SeniorFFxLoc <= breaks2016[11]] <- 9
unique(dataPRD2016$SeniorFFxLoc)

dataPRD2017 <- dataPRD3[dataPRD3$year==2017,]
breaks2017 <- getBreaks(dataPRD2017$SeniorFFxLoc, nclass=10, method = "quantile")
#plot(breaks2017)
dataPRD2017$SeniorFFxLoc[dataPRD2017$SeniorFFxLoc >= breaks2017[1] & dataPRD2017$SeniorFFxLoc < breaks2017[2]] <- 0
dataPRD2017$SeniorFFxLoc[dataPRD2017$SeniorFFxLoc >= breaks2017[2] & dataPRD2017$SeniorFFxLoc < breaks2017[3]] <- 1
dataPRD2017$SeniorFFxLoc[dataPRD2017$SeniorFFxLoc >= breaks2017[3] & dataPRD2017$SeniorFFxLoc < breaks2017[4]] <- 2
dataPRD2017$SeniorFFxLoc[dataPRD2017$SeniorFFxLoc >= breaks2017[4] & dataPRD2017$SeniorFFxLoc < breaks2017[5]] <- 3
dataPRD2017$SeniorFFxLoc[dataPRD2017$SeniorFFxLoc >= breaks2017[5] & dataPRD2017$SeniorFFxLoc < breaks2017[6]] <- 4
dataPRD2017$SeniorFFxLoc[dataPRD2017$SeniorFFxLoc >= breaks2017[6] & dataPRD2017$SeniorFFxLoc < breaks2017[7]] <- 5
dataPRD2017$SeniorFFxLoc[dataPRD2017$SeniorFFxLoc >= breaks2017[7] & dataPRD2017$SeniorFFxLoc < breaks2017[8]] <- 6
dataPRD2017$SeniorFFxLoc[dataPRD2017$SeniorFFxLoc >= breaks2017[8] & dataPRD2017$SeniorFFxLoc < breaks2017[9]] <- 7
dataPRD2017$SeniorFFxLoc[dataPRD2017$SeniorFFxLoc >= breaks2017[9] & dataPRD2017$SeniorFFxLoc <= breaks2017[10]] <- 8
dataPRD2017$SeniorFFxLoc[dataPRD2017$SeniorFFxLoc >= breaks2017[10] & dataPRD2017$SeniorFFxLoc <= breaks2017[11]] <- 9
unique(dataPRD2017$SeniorFFxLoc)

dataPRD2018 <- dataPRD3[dataPRD3$year==2018,]
breaks2018 <- getBreaks(dataPRD2018$SeniorFFxLoc, nclass=10, method = "quantile")
#plot(breaks2018)
dataPRD2018$SeniorFFxLoc[dataPRD2018$SeniorFFxLoc >= breaks2018[1] & dataPRD2018$SeniorFFxLoc < breaks2018[2]] <- 0
dataPRD2018$SeniorFFxLoc[dataPRD2018$SeniorFFxLoc >= breaks2018[2] & dataPRD2018$SeniorFFxLoc < breaks2018[3]] <- 1
dataPRD2018$SeniorFFxLoc[dataPRD2018$SeniorFFxLoc >= breaks2018[3] & dataPRD2018$SeniorFFxLoc < breaks2018[4]] <- 2
dataPRD2018$SeniorFFxLoc[dataPRD2018$SeniorFFxLoc >= breaks2018[4] & dataPRD2018$SeniorFFxLoc < breaks2018[5]] <- 3
dataPRD2018$SeniorFFxLoc[dataPRD2018$SeniorFFxLoc >= breaks2018[5] & dataPRD2018$SeniorFFxLoc < breaks2018[6]] <- 4
dataPRD2018$SeniorFFxLoc[dataPRD2018$SeniorFFxLoc >= breaks2018[6] & dataPRD2018$SeniorFFxLoc < breaks2018[7]] <- 5
dataPRD2018$SeniorFFxLoc[dataPRD2018$SeniorFFxLoc >= breaks2018[7] & dataPRD2018$SeniorFFxLoc < breaks2018[8]] <- 6
dataPRD2018$SeniorFFxLoc[dataPRD2018$SeniorFFxLoc >= breaks2018[8] & dataPRD2018$SeniorFFxLoc < breaks2018[9]] <- 7
dataPRD2018$SeniorFFxLoc[dataPRD2018$SeniorFFxLoc >= breaks2018[9] & dataPRD2018$SeniorFFxLoc <= breaks2018[10]] <- 8
dataPRD2018$SeniorFFxLoc[dataPRD2018$SeniorFFxLoc >= breaks2018[10] & dataPRD2018$SeniorFFxLoc <= breaks2018[11]] <- 9
unique(dataPRD2018$SeniorFFxLoc)

dataPRD2019 <- dataPRD3[dataPRD3$year==2019,]
breaks2019 <- getBreaks(dataPRD2019$SeniorFFxLoc, nclass=10, method = "quantile")
#plot(breaks2019)
dataPRD2019$SeniorFFxLoc[dataPRD2019$SeniorFFxLoc >= breaks2019[1] & dataPRD2019$SeniorFFxLoc < breaks2019[2]] <- 0
dataPRD2019$SeniorFFxLoc[dataPRD2019$SeniorFFxLoc >= breaks2019[2] & dataPRD2019$SeniorFFxLoc < breaks2019[3]] <- 1
dataPRD2019$SeniorFFxLoc[dataPRD2019$SeniorFFxLoc >= breaks2019[3] & dataPRD2019$SeniorFFxLoc < breaks2019[4]] <- 2
dataPRD2019$SeniorFFxLoc[dataPRD2019$SeniorFFxLoc >= breaks2019[4] & dataPRD2019$SeniorFFxLoc < breaks2019[5]] <- 3
dataPRD2019$SeniorFFxLoc[dataPRD2019$SeniorFFxLoc >= breaks2019[5] & dataPRD2019$SeniorFFxLoc < breaks2019[6]] <- 4
dataPRD2019$SeniorFFxLoc[dataPRD2019$SeniorFFxLoc >= breaks2019[6] & dataPRD2019$SeniorFFxLoc < breaks2019[7]] <- 5
dataPRD2019$SeniorFFxLoc[dataPRD2019$SeniorFFxLoc >= breaks2019[7] & dataPRD2019$SeniorFFxLoc < breaks2019[8]] <- 6
dataPRD2019$SeniorFFxLoc[dataPRD2019$SeniorFFxLoc >= breaks2019[8] & dataPRD2019$SeniorFFxLoc < breaks2019[9]] <- 7
dataPRD2019$SeniorFFxLoc[dataPRD2019$SeniorFFxLoc >= breaks2019[9] & dataPRD2019$SeniorFFxLoc <= breaks2019[10]] <- 8
dataPRD2019$SeniorFFxLoc[dataPRD2019$SeniorFFxLoc >= breaks2019[10] & dataPRD2019$SeniorFFxLoc <= breaks2019[11]] <- 9
unique(dataPRD2019$SeniorFFxLoc)

#put all data frames into list
df_list <- list(dataPRD2012,dataPRD2013,dataPRD2014,dataPRD2015,dataPRD2016,dataPRD2017,dataPRD2018,dataPRD2019)

#merge all data frames in list
dataPRD4 <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

# clean the global environments
remove <- c("dataPRD2012","dataPRD2013","dataPRD2014","dataPRD2015","dataPRD2016","dataPRD2017","dataPRD2018","dataPRD2019")
rm(list = remove)
remove <- c("breaks2012","breaks2013","breaks2014","breaks2015","breaks2016","breaks2017","breaks2018","breaks2019")
rm(list = remove)

write.csv(dataPRD4, file = "./build/cache/dataPRD_wIncomeBin.csv")

remove <- c("dataPRD4","remove", "dataPRD3", "df_list")
rm(list = remove)

################################################################################
################################################################################
################################################################################
