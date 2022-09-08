################################################################################
################################################################################
# (5) Survival event data ######################################################
################################################################################
################################################################################

# https://cran.r-project.org/web/packages/survival/vignettes/timedep.pdf
# data for time dependent covariates
dataPRD3 <- readRDS(file = "C:\\Users\\magst\\Desktop\\git\\ihc_retention\\build\\cache\\dataPRD_wSalary.rds")

dataPRD3["surv"] <- ifelse(dataPRD3$lastYear==dataPRD3$year,"1","0")

saveRDS(dataPRD3, file = "C:\\Users\\magst\\Desktop\\git\\ihc_retention\\build\\cache\\dataPRD_wSURV.rds")

remove <- c("dataPRD3","remove")
rm(list = remove)

################################################################################
################################################################################
################################################################################
