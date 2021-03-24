# initialize_run.R

#Before you begin, set data_path and model_path to those on your local machine
data_path   <- "~/Data/IEP_Model/data_iep_model/"
data_dir    <- dir(data_path)

script_path <- "~/Data/IEP_Model/R/"
script_dir  <- dir(script_path)

model_path  <- "~/Projects/iep_model/"
model_dir   <- dir(model_path)

output_path <- "~/Data/IEP_Model/Output/"
output_dir  <- dir(output_path)

# load libraries
source(paste0(data_path, "R/libraries_list.R"))

#data
source("~/Projects/IEP_2/Model/dev/R_files/data/policy_data.R") 
source("~/Projects/IEP_2/Model/dev/R_files/data/water_footprint_data.R")

#state classes
source("~/Projects/IEP_2/Model/dev/R_files/state_classes.R")

#state manager classes
source("~/Projects/IEP_2/Model/dev/R_files/state_managers.R")
