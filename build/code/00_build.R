#Builds dataset

#Build retention data
source("build/code/01_personnel_retention_data.R")

#Build locality adjustment
source("build/code/02_locality_adjustment.R")

#Build competing wages database
#source("build/code/03_prep_competing_wages.R")

#Finish building locality adjustment
source("build/code/04_join_salary_locality_retention.R")

#Build own wages
source("build/code/05_retention_compute_wage.R")

#Deflate wages
source("build/code/06_deflated_wages.R")

#Finalizing data - rounding, factor conversion
source("build/code/07_DataPrepforModeling.R")
