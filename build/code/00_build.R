#Builds dataset

#Build retention data
source("build/code/01_personnel_retention_data.R")

#Build locality adjustment
source("build/code/02_locality_adjustment.R")

#Build competing wages database
source("build/code/03_prep_competing_wages.R")