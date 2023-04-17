#This script initializes the project and should be run at the beginning of each
#session

#########################
#Load init functions
source("functions/init_functions.R")

#Loading and installing packages
library(pacman)
p_load(tidyverse,lubridate,conflicted,readxl,sf,mapview,janitor,missForest,cartography,gtools,
       survival,survminer,ggforestplot,ggsurvfit,viridis,quantmod,modelsummary,install = F)


#Setting package::function priority with conflicted package
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("scale", "base")

#########################
#Loading project helper functions (all scripts within folder)
run.script("functions")


##########################################
##########################################
#Function to download the project data (on first run, google should prompt you to login with credentials)
#if data folder doesn't exist, build data
#get_data("url")
#if(!dir.exists("build/inputs"))  system("ln -s /RSTOR/bayham/projects/ihc_retention/inputs build/inputs")  #link to folder on jude's server

folder.setup()


#dlgMessage("Do you need to pull the repo?")

################################
#Build dataset
#source("build/code/00_build.R")