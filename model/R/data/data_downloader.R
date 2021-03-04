# data_downloader.R
# run the following as a script in the console

# set URL from which FAOSTAT data is downloaded
fao_data_download_url <- "http://fenixservices.fao.org/faostat/static/bulkdownloads/"

# set labels for the required data
# note that most of these data sets are not yet used in the Model version here, but will be as the deprecated version is converted
data_file_labels <- c(
  "Trade_Crops_Livestock_E_All_Data_(Normalized)", 
  "Production_Crops_E_All_Data_(Normalized)", 
  "Production_Livestock_E_All_Data_(Normalized)", 
  "Production_LivestockPrimary_E_All_Data_(Normalized)", 
  "Environment_LandCover_E_All_Data_(Normalized)", 
  "FoodSupply_Crops_E_All_Data_(Normalized)", 
  "FoodSupply_LivestockFish_E_All_Data_(Normalized)", 
  "Trade_Crops_Livestock_E_All_Data_(Normalized)", 
  "Production_Crops_E_All_Data_(Normalized)", 
  "Production_Livestock_E_All_Data_(Normalized)", 
  "Production_LivestockPrimary_E_All_Data_(Normalized)", 
  "Environment_LandCover_E_All_Data_(Normalized)", 
  "FoodSupply_Crops_E_All_Data_(Normalized)", 
  "FoodSupply_LivestockFish_E_All_Data_(Normalized)", 
  "FoodBalanceSheetsHistoric_E_All_Data_(Normalized)", 
  "Environment_LandCover_E_All_Data_(Normalized)", 
  "FoodSupply_LivestockFish_E_All_Data_(Normalized)", 
  "Emissions_Agriculture_Enteric_Fermentation_E_All_Data_(Normalized)", 
  "Emissions_Agriculture_Manure_Management_E_All_Data_(Normalized)", 
  "Forestry_E_All_Data_(Normalized)"
)

# downloads files into /data_iep_model
# this could take some time, depending on your internet connection
for(label in data_file_labels) {
  download.file(url      = paste0(fao_data_download_url, label, ".zip"), 
                destfile = paste0(data_path, label, ".zip"))
}
# unzip files, each into its own subdirectory of /data_iep_model
# subdirectories will maintain their awkward FAOSTAT names for now
for(label in data_file_labels) {
  unzip(zipfile = data_dir[grep(label, data_dir)], 
        exdir   = label)
}

# once these files are successfully extracted, you can do some clean-up and remove the compressed versions
# system("rm ~/data_iep_model/*.zip")

