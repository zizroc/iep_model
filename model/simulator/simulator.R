# simulator module



time_frame     <- c(2000:2005)
land_use_types <- c("cropland", "permanent_cropland", "arable_land", "pasture", "forest", "otherland")
crop_types     <- c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
live_types     <- c(  "aves", "bovine", "camelid",   "caprine",      "equine",  "rodentia",   "sus",   "fish")
live_use_group <- c( "dairy",   "meat",   "other")
model_groups   <- c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop", "aves_dairy", "bovine_dairy", "camelid_dairy", "caprine_dairy", "aves_meat", "bovine_meat", "camelid_meat", "caprine_meat", "equine_meat", "rodentia_meat", "sus_meat", "fish_meat", "aves_other", "bovine_other", "camelid_other", "caprine_other", "equine_other", "rodentia_other", "sus_other", "fish_other")
# iso_codes      <- c("ARG", "CAN", "CHN", "MEX")
iso_codes      <- c("CHN")


for(year_index in time_frame) {
  for(iso_code in iso_codes) {
    #population module
    
    #land use module 1
    if(year_index == 2000) {
      lu     <- land_use$new()
      lu_mng <- land_use_manager$new()
    }
    lu$set_year(year_index)
    lu$set_iso_alpha3(iso_code)
    for(lu_type in land_use_types) {
      lu$set_land_use_type(lu_type)
      lu$set_land_use_area(lu_mng$land_use_area(lu$fao_countrycode, lu$year, lu$land_use_type, df = lu$get_land_use_data()))
      lu$set_dry_matter_production(lu_mng$dry_matter_productivity(lu$land_use_type))
      lu$set_land_use_data()
    }
    
    #crop module 1
    if(year_index == 2000) {
      crp     <- crop$new()
      crp_mng <- crop_manager$new()
      trd_crp <- trade_crop_manager$new()
    }
    crp$set_year(year_index)
    crp$set_iso_alpha3(iso_code)
    for(crop_type in crop_types){
      crp$set_model_group(crop_type)
      crp$set_land_alloted(crp_mng$manage_cropland_allotment(crp$fao_countrycode, crp$year, crp$model_group))
      crp$set_harvest_area(crp_mng$manage_harvest_area(crp$fao_countrycode, crp$year, crp$model_group, df1 = lu$get_land_use_data(), df2 = crp$ratio_of_land_alloted))
      crp$set_harvest_yield(crp_mng$manage_harvest_yield(crp$fao_countrycode, crp$year, crp$model_group, df = crp$get_crop_data()))
      crp$set_production(crp_mng$manage_production(crp$harvest_area, crp$harvest_yield))
      crp$set_food_stock(crp$production*crp_mng$manage_crop_allocation(crp$fao_countrycode, crp$year, crp$model_group, "food"))
      crp$set_feed_stock(crp$production*crp_mng$manage_crop_allocation(crp$fao_countrycode, crp$year, crp$model_group, "feed"))
      crp$set_seed_stock(crp$production*crp_mng$manage_crop_allocation(crp$fao_countrycode, crp$year, crp$model_group, "seed"))
      crp$set_losses_stock(crp$production*crp_mng$manage_crop_allocation(crp$fao_countrycode, crp$year, crp$model_group, "loss")) 
      crp$set_processing_stock(crp$production*crp_mng$manage_crop_allocation(crp$fao_countrycode, crp$year, crp$model_group, "proc"))
      crp$set_other_uses_stock(crp$production*crp_mng$manage_crop_allocation(crp$fao_countrycode, crp$year, crp$model_group, "othe"))
      crp$set_country_data()
      crp$set_crop_data()
    }
    
    
    #livestock module 1
    if(year_index == 2000) {
      liv     <- livestock$new()
      liv_mng <- livestock_manager$new()
    }
    liv$set_year(year_index)
    liv$set_iso_alpha3(iso_code)
    for(live_type in live_types) {
      liv$set_model_group(live_type)
      liv$set_herd_tlu(liv_mng$herd_tlu(liv$model_group))
      liv$set_stock_growth_rate(liv_mng$growth_rate(liv$fao_countrycode, liv$year, liv$model_group, df = liv$get_livestock_data()))
      liv$set_dairy_stock_quantity(liv_mng$product_usage(liv$model_group, "dairy")*liv_mng$quantity(liv$fao_countrycode, liv$year, liv$model_group, df = liv$get_livestock_data()))
      liv$set_meat_stock_quantity(liv_mng$product_usage(liv$model_group,   "meat")*liv_mng$quantity(liv$fao_countrycode, liv$year, liv$model_group, df = liv$get_livestock_data()))
      liv$set_other_stock_quantity(liv_mng$product_usage(liv$model_group, "other")*liv_mng$quantity(liv$fao_countrycode, liv$year, liv$model_group, df = liv$get_livestock_data()))
      liv$set_feed_drymatter_demand(liv_mng$feed_demand(liv$model_group, "DM"))
      liv$set_feed_drymatter_demand(liv$feed_drymatter_demand*liv_mng$feed_demand(liv$model_group, "CP"))
      liv$set_livestock_data()
    }
    
    #water demand (footprint) module 1 
    if(year_index == 2000) {
      wf     <- water_footprint$new() 
      wf_mng <- water_footprint_manager$new()
    }
    wf$set_year(year_index)
    wf$set_iso_alpha3(iso_code)
    for(crop_type in crop_types) {
      wf$set_model_group(crop_type)
      
      #find crop production
      crop_prod <- crp$get_crop_data() %>% 
        dplyr::filter(iso_alpha3_code == wf$iso_alpha3 & year == wf$year & model_group == wf$model_group) %>% 
        dplyr::pull(production)
      
      wf$set_green_wf(wf_mng$get_crop_wf(wf$iso_alpha3, wf$model_group, "green")*crop_prod)
      wf$set_blue_wf(wf_mng$get_crop_wf(wf$iso_alpha3, wf$model_group, "blue")*crop_prod)
      wf$set_grey_wf(wf_mng$get_crop_wf(wf$iso_alpha3, wf$model_group, "grey")*crop_prod)
      wf$set_wf_data()
    }
    
    #' TODO animal products water footprint module
    
    #trade: crop module 2
    for(crop_type in crop_types){ 
      crp$set_model_group(crop_type)
      crp$set_food_imports(trd_crp$imports(crp$fao_countrycode, crp$year, crp$model_group, "food", df = crp$get_trade_crop_data()))
      crp$set_feed_imports(trd_crp$imports(crp$fao_countrycode, crp$year, crp$model_group, "feed", df = crp$get_trade_crop_data()))
      crp$set_food_exports(trd_crp$exports(crp$fao_countrycode, crp$year, crp$model_group, "food", df = crp$get_trade_crop_data()))
      crp$set_feed_exports(trd_crp$exports(crp$fao_countrycode, crp$year, crp$model_group, "feed", df = crp$get_trade_crop_data()))
      crp$set_trade_crop_data()
    }
    
    #livestock module 2
    
    #' TODO fix this to update with traded crops as well
    # for(crop_type in crop_types) {
    #   wf$set_model_group(crop_type)
    #   wf$set_green_wf(wf_mng$get_crop_wf(wf$iso_alpha3, wf$model_group, "green"))
    #   wf$set_blue_wf(wf_mng$get_crop_wf(wf$iso_alpha3, wf$model_group, "blue"))
    #   wf$set_grey_wf(wf_mng$get_crop_wf(wf$iso_alpha3, wf$model_group, "grey"))
    #   wf$update_wf_data(wf$iso_alpha3, wf$year, wf$model_group)
    # }
    
    for(crop_type in crop_types) {
      wf$set_model_group(crop_type)
      
      net_trade <- crp$get_trade_crop_data() %>% 
        dplyr::filter(iso_alpha3_code == wf$iso_alpha3 & year == wf$year & model_group == wf$model_group) %>% 
        dplyr::mutate(net_trade = food_imports + feed_imports - food_exports - feed_exports) %>% 
        dplyr::pull(net_trade)
      
      wf$set_traded_blue_wf(wf_mng$get_crop_wf(wf$iso_alpha3, wf$model_group, "blue")*net_trade)
      wf$set_traded_green_wf(wf_mng$get_crop_wf(wf$iso_alpha3, wf$model_group, "green")*net_trade)
      wf$set_traded_grey_wf(wf_mng$get_crop_wf(wf$iso_alpha3, wf$model_group, "grey")*net_trade)
      wf$set_traded_wf_data()
      rm(net_trade)
    }
    
    #reporting module
    #' TODO figure out why this is double-reporting the last country
    # if(iso_code == max(iso_codes) & i == length(time_frame)) {
    #   print(lu$get_land_use_data())
    #   # print(lu$get_total_land_area())
    #   
    #   print(crp$get_crop_data())
    #   print(crp$get_trade_crop_data()) 
    #   print(liv$get_livestock_data())
    #   print(wf$get_wf_data())
    #   print(wf$get_traded_wf_data())
    # }
  }
}

wf_df <- wf$get_wf_data() %>% 
  dplyr::full_join(wf$get_traded_wf_data()) 

wf_df_agg <- wf$get_wf_data() %>% 
  dplyr::full_join(wf$get_traded_wf_data()) %>% 
  dplyr::group_by(iso_alpha3_code, fao_countrycode, year) %>% 
  dplyr::summarize(blue_wf = sum(blue_wf, 
                                  na.rm = TRUE), 
                   green_wf = sum(green_wf, 
                                  na.rm = TRUE),
                   grey_wf = sum(grey_wf, 
                                  na.rm = TRUE)) %>% 
  tidyr::pivot_longer(., 
                      cols = blue_wf:grey_wf, 
                      names_to = "wf_type", 
                      values_to = "water_m3_per_yr")

write_csv(wf_df,     path = paste0(output_path, "/wf_data.csv"))
write_csv(wf_df_agg, path = paste0(output_path, "/wf_data_aggregated.csv"))

