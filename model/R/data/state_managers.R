#water_footprint_data.R

# References:
# Mekonnen, M.M. and Hoekstra, A.Y. (2010) The green, blue and grey water footprint of crops and derived crop products, 										
# Value of Water Research Report Series No. 47, UNESCO-IHE, Delft, the Netherlands.									
# http://www.waterfootprint.org/Reports/Report47-WaterFootprintCrops-Vol1.pdf	
#Crops water footprint
#' Water footprint
#' 
if(!exists("crops_wf")) {
  dest_path <- "~/Projects/iep_food/data/water_footprint/"
  
  # download.file(url = "https://www.waterfootprint.org/media/downloads/Report47-Appendix-II.xlsx", 
  #               destfile = paste0(dest_path, "crops_wf.xlsx"), 
  #               method = "wget")
  
  crops_df <- readxl::read_xlsx(paste0(dest_path, "crops_wf.xlsx"), 
                                sheet = 2)
  # The spreadsheet is organised with product/crop codes and identifiers in columns 1 to 8, 
  # and water footprint types (green, blue, grey) in column 9, 
  # a global average WF for each product/crop in column 10, 
  # and country-level data in columns 11+.
  # Country-level data are broken down by province, with a last column for every country being "CNTRY-average".
  # Data shaping below:
  label_cols    <- seq(1, 8, by = 1)
  wf_col        <- grep("WF type", crops_df[5,])
  global_av_col <- grep("Global average", crops_df[4,]) %>% 
    min()
  country_cols  <- grep("CNTRY-average", crops_df[4,])
  # Get headers for the new data frames:
  country_names <- crops_df[3, country_cols] %>% 
    t() %>% 
    as.vector() 
  country_codes <- crops_df[1, country_cols] %>% 
    t() %>% 
    as.vector() 
  global_av_code<- "WRLD" #world average 
  label_codes   <- crops_df[4, label_cols] %>% 
    t() %>% 
    as.vector()
  label_codes_simplified <- c("Product_code_FAOSTAT", 
                              "Product_code_HS", 
                              "Product_code_SITC", 
                              "Product_description_HS", 
                              "Product_description_FAOSTAT", 
                              "Root_product_HS", 
                              "Product_fraction", 
                              "Value_fraction")
  wf_code       <- "wf_type"
  # Get values in the data frames and assign headers:
  label_values          <- crops_df[-c(1:4), label_cols] %>% 
    as_tibble() %>% 
    slice(-1) %>% #remove the first row (empty but for 'WF type' label)
    fill(label_cols, .direction = "down") 
  names(label_values)   <- label_codes_simplified
  glob_av_values        <- crops_df[-c(1:4), global_av_col] %>% 
    as_tibble() %>% 
    mutate(across(where(is.character), as.numeric)) %>% 
    slice(-1) #remove the first row (empty but for 'WF type' label)
  names(glob_av_values) <- global_av_code
  wf_types              <- crops_df[-c(1:5), wf_col] %>% 
    as_tibble()
  names(wf_types)       <- wf_code
  wf_types <- wf_types %>% #convert wf_types to lower cases
    mutate(wf_type = case_when(
      wf_type == "Green" ~ "green", 
      wf_type == "Blue"  ~ "blue", 
      wf_type == "Grey"  ~ "grey"))
  wf_values             <- crops_df[-c(1:4), country_cols] %>% 
    mutate(across(where(is.character), as.numeric))%>% 
    slice(-1) #remove the first row (empty but for 'WF type' label)
  names(wf_values)      <- country_names
  
  values_df <- cbind(label_values, 
                     wf_types, 
                     glob_av_values, 
                     wf_values)
  
  # Overwrite crops_wf with clean, shaped data
  crops_wf <- values_df %>% 
    as_tibble() %>% 
    tidyr::pivot_longer(., 
                        cols = WRLD:Zimbabwe, 
                        names_to = "country_name", 
                        values_to = "water_m3_per_ton")
  rm(crops_df, 
     values_df, 
     label_cols, 
     wf_col, 
     global_av_col, 
     country_cols, 
     country_codes, 
     label_codes, 
     wf_code, 
     label_values, 
     glob_av_values, 
     wf_types, 
     wf_values)
}

# NB: Indexing in the Hoekstra data set uses ISO alpha 2 country codes but we're using ISO alpha 3 codes.
# I need a mapping function to convert the country names and/or alpha 2 codes to ISO Alpha 3 codes.
# Directly converting alpha 2 to alpha 3 codes produces ambiguity, meaning that lots of manual fixes are necessary.
# Instead, I use the codes from the livestock water footprint data set and replace country codes here.

country_codes_mapper <- read_csv(file = paste0(dest_path, "country_codes_mapper.csv"))


left_behinds <- data.frame(
  country_name = c("American Samoa", "Azerbaijan", "Cayman Islands", "Congo", "Congo, Democratic Republic of", "Cote d'Ivoire", "Fiji", "Guam", "Iran, Islamic Republic of", "Korea, Democratic People's Republic of", "Lao People's Democratic Republic", "Macedonia , The Former Yugoslav Republic of", "Maldives", "Marshall Islands", "Micronesia, Federated States of", "Moldova", "Occupied Palestinian Territory", "Puerto Rico", "Reunion", "Saint Pierre and Miquelon", "Saint Vincent and Grenadines", "Tanzania, United Republic of", "Venezuela, Bolivarian Republic of", "Wallis and Futuna Islands", "Western Sahara"), 
  iso_code     = c("ASM", "AZE", "CYM", "CRG", "DRC", "CIV", "FJI", "GUM", "IRN", "PRK", "LAO", "MKD", "MDV", "MHL", "FSM", "MDA", "PSE", "PRI", "REU", "SPM", "VCT", "TZA", "VEN", "WLF", "ESH") 
)

country_names <- crops_wf %>% 
  dplyr::distinct(country_name) %>% 
  dplyr::filter(country_name != "WRLD")

iso_codes_mapper <- country_names %>% 
  full_join(country_codes_mapper, 
            by = "country_name", 
            keep.all = TRUE) %>% 
  rbind(data.frame(
    country_name = "WRLD", 
    iso_code     = "WRLD"
  )) %>% 
  drop_na() %>% 
  rbind(left_behinds)

crops_wf <- crops_wf %>% 
  dplyr::full_join(iso_codes_mapper, 
                   by = "country_name")

crops_model_group_mapper <- read_csv(file = paste0(dest_path, "crops_product_mapper.csv"))

waterfootprint_data_crops <- crops_wf %>% 
  dplyr::left_join(crops_model_group_mapper, 
                   by = "Product_description_FAOSTAT") %>% 
  dplyr::select(Product_description_FAOSTAT, model_group, Product_fraction, Value_fraction, wf_type, country_name, iso_code,water_m3_per_ton)

mean_waterfootprint_crops <- waterfootprint_data_crops %>% 
  dplyr::group_by(country_name, 
                  iso_code, 
                  wf_type, 
                  model_group) %>% 
  dplyr::summarize(wf_mean = mean(water_m3_per_ton, 
                                  na.rm = TRUE))

# get_mean_crop_wf = function(x1, x2) {
#   x <- waterfootprint_data_crops %>% 
#     dplyr::filter(iso_code == x1, model_group == x2) %>% 
#     dplyr::group_by(model_group) %>% 
#     dplyr::summarize(wf_mean = mean(water_m3_per_ton, 
#                                         na.rm = TRUE)) %>% 
#     dplyr::pull(wf_mean)
#   
#   if(length(x) != 0) {
#     return(x)
#   } else {
#     y <- waterfootprint_data_crops %>% 
#       dplyr::filter(iso_code == "WRLD", model_group == x2) %>% 
#       dplyr::group_by(model_group) %>% 
#       dplyr::summarize(wf_mean = mean(water_m3_per_ton, 
#                                           na.rm = TRUE)) %>% 
#       dplyr::pull(wf_mean)
#     
#     if(length(y) != 0) {
#       return(y)
#     } else {
#       return(0)
#     }
#   }
# }


# Animals and animal products wf:
# Mekonnen, M.M. and Hoekstra, A.Y. (2010) The green, blue and grey water footprint of farm animals and animal products, 										
# Value of Water Research Report Series No. 48, UNESCO-IHE, Delft, the Netherlands.									
if(!exists("livestock_wf")) {
  download.file(url = "https://www.waterfootprint.org/media/downloads/Report48-Appendix-V.zip",  
                destfile = paste0(dest_path, "livestock_wf.zip"), 
                method = "wget")
  
  unzip(zipfile = paste0(dest_path, "livestock_wf.zip"), 
        exdir   = dest_path, 
        overwrite = TRUE)
  
  livestock_df <- readxl::read_xlsx(paste0(dest_path, "livestock_wf.xlsx"), 
                                     sheet = 2)
  # The spreadsheet is organised with animal/animal product codes and identifiers in columns 1 to 8, 
  # and water footprint types (green, blue, grey) in column 9. Unlike the crop_wf file, the 
  # global average WF for each product/crop is given for grazing, mixed, industrial, and weighted average in columns 10-13, 
  # and this pattern repeats for country-level data in columns 14+.
  # Country-level data are broken down by livestock feed/use type, with a 4th and final column for each country being "Weighted average".
  # Here only the weighted averages for each country will be used for simplicity.
  # Data shaping below:
  label_cols    <- seq(1, 8, by = 1)
  wf_col        <- 9 #no label for wf type here, it is called Production system
  global_av_col <- 13
  country_cols  <- grep("Weighted average", livestock_df[3,])[-c(1)] #drop column 13 which is already assigned for world average
  # Get headers for the new data frames:
  country_names <- livestock_df %>% 
    slice(2) %>% 
    select(country_cols) %>% 
    t() %>% 
    replace_na("Zimbabwe") %>% # NB: Zimbabwe does not get scraped from the Excel file for some reason
    as.vector()
  country_names <- as_tibble(country_names) %>% 
    dplyr::rename("country_name" = "value")
  # Need to map country names onto country ISO alpha3 codes
  # I have done the separately:
  country_code_mapper <- read_csv("~/Projects/iep_food/data/water_footprint/region_mapper - country_code_mapper.csv") %>% 
    rename(country_name = Country) %>% 
    distinct(iso_code, country_name)
  country_codes_tmp <- country_names %>% 
    dplyr::left_join(country_code_mapper, 
                     by = "country_name")
  # Several of the countries were missed and do not have adequately mapped ISO3 codes. Need to do this manually.
  left_behinds <- country_codes_tmp %>% 
    dplyr::filter(is.na(iso_code)) %>% 
    dplyr::full_join(
      data.frame(
        country_name = c("Azerbaijan, Republic of", "British Virgin Islands", "Congo, Republic of", "Congo, Dem Republic of", "Faeroe Islands", "Falkland Isl", "Fiji Islands", "Greenland", "Iran, Islamic Rep of", "Korea, Dem People's Rep", "Macedonia,The Fmr Yug Rp", "Micronesia,Fed States of", "Moldova, Republic of", "Netherlands Antiles", "Palau", "Saint Vincent/Grenadines", "Tanzania, United Rep of", "Venezuela, Boliv Rep of", "Wallis and Futuna Is", "WRLD"), 
        iso_code_rep = c("AZE", "VGB", "COG", "DRC", "FRO", "FLK", "FJI", "GRL", "IRN", "PRK", "MKD", "FSM", "MDA", "ANT", "PLW", "VCT", "TZA", "VEN", "WLF", "WRLD")
      ), 
      by = "country_name"
    )
  country_codes_mapper <- country_codes_tmp %>% 
    dplyr::left_join(left_behinds, 
                     by = c("country_name", "iso_code")) %>% 
    dplyr::mutate(iso_code = ifelse(is.na(iso_code), 
                                    iso_code_rep, 
                                    iso_code)) %>% 
    dplyr::select(-iso_code_rep)
  
  # write_csv(country_codes_mapper, path = paste0(dest_path, "country_codes_mapper.csv"))
  # country_codes <- country_code_mapper %>% 
  #   dplyr::pull(iso_code)

  global_av_code<- "WRLD" #world average 
  label_codes   <- livestock_df %>% 
    dplyr::slice(2) %>%
    dplyr::select(label_cols) %>% 
    t() %>% 
    as.vector() #this is so not tidy but as_vector() isn't working for me
  label_codes_simplified <- c("HS_PC_TAS_code", 
                              "SITC_Rev_3_SITA_code", 
                              "Product_description_HS", 
                              "Product_description_SITC", 
                              "Rootproduct_HS", 
                              "Rootproduct_SITC", 
                              "Product_fraction", 
                              "Value_fraction")
  wf_code       <- "wf_type"
  # Get values in the data frames and assign headers:
  label_values          <- livestock_df %>% 
    dplyr::slice(-c(1:3)) %>% 
    dplyr::select(label_cols) %>% 
    fill(label_cols, .direction = "down")
  names(label_values)   <- label_codes_simplified
  glob_av_values        <- livestock_df %>% 
    slice(-c(1:3)) %>% 
    dplyr::select(global_av_col) %>%   
    mutate(across(where(is.character), as.numeric))
  names(glob_av_values) <- global_av_code
  wf_types              <- livestock_df %>% 
    slice(-c(1:3)) %>% 
    dplyr::select(wf_col)
  names(wf_types)       <- wf_code
  wf_types <- wf_types %>% #convert wf_types to lower cases
    mutate(wf_type = case_when(
      wf_type == "Green" ~ "green", 
      wf_type == "Blue"  ~ "blue", 
      wf_type == "Grey"  ~ "grey"))
  wf_values             <- livestock_df %>% 
    slice(-c(1:3)) %>% 
    dplyr::select(all_of(country_cols)) %>%  
    mutate(across(where(is.character), as.numeric))
  names(wf_values)      <- country_names %>% 
    pull(country_name)
  
  wf_values <- wf_values[, !duplicated(colnames(wf_values))] # NB: Serbia and Montenegro is duplicated in the dataset for some reason
  
  values_df <- cbind(label_values, 
                     wf_types, 
                     glob_av_values, 
                     wf_values) %>% 
    as_tibble()
  
  
  # Overwrite crops_wf with clean, shaped data
  livestock_wf <- values_df %>% 
    tidyr::pivot_longer(., 
                        cols = WRLD:Zimbabwe, 
                        names_to = "country_name", 
                        values_to = "water_m3_per_ton") %>% 
    dplyr::left_join(country_codes_mapper, 
                     by = "country_name")
  rm(values_df, 
     label_cols, 
     wf_col, 
     global_av_col, 
     country_cols, 
     country_codes_tmp, 
     label_codes, 
     wf_code, 
     label_values, 
     glob_av_values, 
     wf_types, 
     wf_values)
  
}


# Biofuels wf:
# Mekonnen, M.M. and Hoekstra, A.Y. (2010) The green, blue and grey water footprint of crops and derived crop products, 										
# Value of Water Research Report Series No. 47, UNESCO-IHE, Delft, the Netherlands.									
# http://www.waterfootprint.org/Reports/Report47-WaterFootprintCrops-Vol1.pdf	
# download.file(url = "https://www.waterfootprint.org/media/downloads/Report47-Appendix-III.zip",  
#               destfile = paste0(dest_path, "biofuels_wf.zip"), 
#               method = "wget")


# Industrial products wf:
# download.file(url = "https://www.waterfootprint.org/media/downloads/WF-industrial-products.xls",  
#               destfile = paste0(dest_path, "industrial_products_wf.xls"), 
#               method = "wget")

# Map products to model_groups

# what about a null model? randomly sample (monte carlo, autosample) of 41 years as compared to my distribution

#Map wf values to products
#animal products
livestock_wf_mapper <- read_csv(file = paste0(dest_path, "livestock_product_mapper.csv"))

livestock_model_group_mapper <- livestock_wf_mapper %>% 
  dplyr::select(Product_description_HS, animal_group1, use_class) %>%
  tidyr::unite("model_group", c(animal_group1, use_class), remove = TRUE) %>% 
  dplyr::bind_rows(
    livestock_wf_mapper %>% 
      dplyr::select(Product_description_HS, animal_group2, use_class) %>% 
      tidyr::drop_na() %>% 
      tidyr::unite("model_group", c(animal_group2, use_class), remove = TRUE)
  ) %>% 
  dplyr::bind_rows(
    livestock_wf_mapper %>% 
      dplyr::select(Product_description_HS, animal_group3, use_class) %>% 
      tidyr::drop_na() %>% 
      tidyr::unite("model_group", c(animal_group3, use_class), remove = TRUE)
  ) %>% 
  dplyr::bind_rows(
    livestock_wf_mapper %>% 
      dplyr::select(Product_description_HS, animal_group4, use_class) %>% 
      tidyr::drop_na() %>% 
      tidyr::unite("model_group", c(animal_group4, use_class), remove = TRUE)
  ) %>% 
  dplyr::select(model_group, Product_description_HS)




#WRLD is still not getting its iso code

waterfootprint_data_livestock <- livestock_wf %>% 
  dplyr::left_join(livestock_model_group_mapper, 
                   by = "Product_description_HS") %>% 
  dplyr::select(Product_description_HS, model_group, Product_fraction, Value_fraction, wf_type, country_name, iso_code,water_m3_per_ton)

mean_waterfootprint_livestock <- waterfootprint_data_livestock %>% 
  dplyr::group_by(country_name, 
                  iso_code, 
                  wf_type, 
                  model_group) %>% 
  dplyr::summarize(wf_mean = mean(water_m3_per_ton, 
                                  na.rm = TRUE))


# get_mean_livestock_wf = function(x1, x2) {
#   x <- waterfootprint_data_livestock %>% 
#     dplyr::filter(iso_code == x1, model_group == x2) %>% 
#     dplyr::group_by(model_group) %>% 
#     dplyr::summarize(wf_mean = mean(water_m3_per_ton, 
#                                         na.rm = TRUE)) %>% 
#     dplyr::pull(wf_mean)
#   
#   if(length(x) != 0) {
#     return(x)
#   } else {
#     y <- waterfootprint_data_livestock %>% 
#       dplyr::filter(iso_code == "WRLD", model_group == x2) %>% 
#       dplyr::group_by(model_group) %>% 
#       dplyr::summarize(wf_mean = mean(water_m3_per_ton, 
#                                      na.rm = TRUE)) %>% 
#       dplyr::pull(wf_mean)
#     
#     if(length(y) != 0) {
#       return(y)
#     } else {
#       return(0)
#     }
#   }
# }

