# Init functions:
# These functions help to initialize the project without
# leaving artifacts in the workspace

#####################################################
#Loading and installing packages
init.pacs <- function(package.list){
  check <- unlist(lapply(package.list, require, character.only = TRUE))
  #Install if not in default library
  if(any(!check)){
    for(pac in package.list[!check]){
      install.packages(pac)
    }
    lapply(package.list, require, character.only = TRUE)
  }
}
#unit test
#init.pacs(c("scales"))


#####################################################
#Run all scripts in a directory
run.script <- function(dir.name){
  #check whether directory exists
  if(dir.exists(dir.name)){
    if(!is.null(dir(dir.name,pattern = ".R"))){
      invisible(lapply(dir(dir.name,pattern = ".R",full.names = T),source))
    }
  } else {
    stop("Invalid directory name")
  }
}
#unit test
#run.script("functions")

####################################################
#Load data from cache or build from munge script
#note that both inputs are strings that call files by name
load.or.build <- function(dataset,munge.script){
  if(file.exists(str_c("build/cache/",dataset))){
    message(str_c("Loading ",dataset," from cache"))
    load(str_c("build/cache/",dataset),envir = .GlobalEnv)
  } else {
    message(str_c("running ",munge.script," to build ",dataset))
    source(str_c("build/code/",munge.script))
  }
}

####################################################
get_data <- function(folder_url){
  #Function to read project data from google drive
  
  #input: url to google folder where data is hosted
  
  #output: none.  data is downloaded to build/inputs
  
  require(googledrive)
  
  if(!dir.exists("build/inputs")){
    message("The folder: build/inputs does not exist.  Run folder.setup().")
    return(NULL)
  } 
  
  #Locate files in folder
  files.exist <- dir(path = "build/inputs")
  
  #Locate zip files on google drive
  files <- drive_get(as_id(folder_url)) %>%
    drive_ls() %>%
    filter(!(name %in% files.exist))
  
  if(nrow(files)==0){
    message("All files up to date.")
    return(NULL)
  }
  
  #Checking for folders that can't be downloaded from googledrive
  files.folders <- files %>%
    filter(!str_detect(name,"[\\.]"))
  
  if(nrow(files.folders)>0){
    stop(str_c("The googledrive package cannot recursively download files in a folder.  Zip them and reupload them.  Folders: /n ",
               str_c(files.folders$name,collapse = ", ")))
  }
  
  
  #Loop over list of zip files; write to data folder; unzip; and delete zipped folder
  #message("The zipped file with raw data is large.  It may take a little while - but it only happens once.")
  map2(.x = files$id,
       .y = files$name,
       function(x, y) {
         #Download files from googledrive folder
         
         drive_download(as_id(x),
                        path = str_c("build/inputs/", y),
                        overwrite = T)
         
         if(str_detect(y,".zip")){
           #Unzip files
           unzip(
             zipfile = str_c("build/inputs/", y),
             exdir = "build/inputs",
             overwrite = T
           )
           
           #Remove the zipped file
           #file.remove(str_c("build/inputs/", y))
         }
       })
  
}


#This function builds out the folder structure
folder.setup <- function(){
  require(purrr)
  folder.list <- c("build/code",
                   "build/cache",
                   "build/inputs",
                   "analysis/inputs",
                   "analysis/code",
                   "analysis/cache",
                   "analysis/outputs")
  
  map(folder.list,
      function(x){
        if(!dir.exists(x)){
          dir.create(x,recursive = T)
          message(str_c("The ",x," folder has been created."))
        } else {
          message(str_c("The ",x," folder already exists."))
        }
      })
  
  return(NULL)
}


