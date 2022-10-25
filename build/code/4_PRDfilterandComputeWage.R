################################################################################
################################################################################
# (4) Filter and compute wage after locality adjustment ########################
################################################################################
################################################################################
#####

dataPRD <- readRDS(file = "build/cache/dataPRD_join.rds")

# remove nas from est_annual_pay_OPM_base_rate_senior_FF column and med_wage
dataPRD2 <- dataPRD[!is.na(dataPRD$med_wage)&!is.na(dataPRD$est_annual_pay_OPM_base_rate_senior_FF), ]

sum(is.na(dataPRD2$med_wage))

# check for NA and min/max etc...
any(is.na(dataPRD2$LocoAdj.y))
any(is.na(dataPRD2$area1))
any(is.na(dataPRD2$days_assigned))
any(is.na(dataPRD2$base_pay_hourly_GS8_step5))
any(is.na(dataPRD2$base_pay_hourly_GS5_step2))
any(is.na(dataPRD2$est_annual_pay_OPM_base_rate_senior_FF))
sum(is.na(dataPRD2$med_wage))
min(dataPRD2$year)
max(dataPRD2$year)

# change from percent to proportion 
dataPRD2$LocoAdj.y <- dataPRD2$LocoAdj.y/100
# calculated adjusted wage = wage*locality adjustment+wage
dataPRD2$GSBASExLoc <- dataPRD2$base_pay_hourly_GS8_step5*dataPRD2$LocoAdj.y+dataPRD2$base_pay_hourly_GS8_step5
dataPRD2$GSBASE5xLoc <- dataPRD2$base_pay_hourly_GS5_step2*dataPRD2$LocoAdj.y+dataPRD2$base_pay_hourly_GS5_step2
dataPRD2$SeniorFFxLoc <- dataPRD2$est_annual_pay_OPM_base_rate_senior_FF*dataPRD2$LocoAdj.y+dataPRD2$est_annual_pay_OPM_base_rate_senior_FF

# unique pay
unique(dataPRD2$GSBASExLoc)
unique(dataPRD2$GSBASE5xLoc)
unique(dataPRD2$SeniorFFxLoc)
unique(dataPRD2$est_annual_pay_OPM_base_rate_senior_FF)


# cumulative year (corrects for skip years) ~ moved to 1_PRD_datafilter.R
#dataPRD2$cumusumYEARMarker <-  1
#dataPRD2 <- dataPRD2%>%dplyr::arrange(res_id,year)%>%dplyr::group_by(res_id)%>%dplyr::mutate(cumusumYEAR=cumsum(cumusumYEARMarker))
#dataPRD2 <- dataPRD2%>%dplyr::arrange(res_id,year)%>%dplyr::group_by(res_id)%>%dplyr::mutate(cumusumDA=cumsum(days_assigned))
#dataPRD2 <- dataPRD2%>%dplyr::arrange(res_id,year)%>%dplyr::group_by(res_id)%>%dplyr::mutate(cumusumSFF=cumsum(SeniorFFxLoc))
#dataPRD2 <- dataPRD2%>%dplyr::arrange(res_id,year)%>%dplyr::group_by(res_id)%>%dplyr::mutate(cumusumTH=cumsum(total_hours))

# make a copy of Senior FF column
dataPRD2$SeniorFFxLocCOPY <- dataPRD2$SeniorFFxLoc

saveRDS(dataPRD2, file = "build/cache/dataPRD_wSalary.rds")

remove <- c("dataPRD","dataPRD2","remove")
rm(list = remove)

################################################################################
################################################################################
################################################################################