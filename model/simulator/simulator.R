# simulator module



time_frame     <- c(2000:2008)
land_use_types <- c("cropland", "fallow_cropland", "pasture", "forest", "otherland")
crop_types     <- c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
live_types     <- c(  "aves", "bovine", "camelid",   "caprine",      "equine",  "rodentia",   "sus",   "fish")
live_use_group <- c( "dairy",   "meat",   "other")
model_groups   <- c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop", "aves_dairy", "bovine_dairy", "camelid_dairy", "caprine_dairy", "aves_meat", "bovine_meat", "camelid_meat", "caprine_meat", "equine_meat", "rodentia_meat", "sus_meat", "fish_meat", "aves_other", "bovine_other", "camelid_other", "caprine_other", "equine_other", "rodentia_other", "sus_other", "fish_other")
fuel_types     <- c("biofuel", "biomass", "coal", "ethanol", "gas", "geothermal", "hydro", "nuclear", "solar", "wind")
# iso_codes      <- c("ARG", "CAN", "CHN", "MEX")
iso_codes      <- c("ARG", "CHN")
 
#this class is under construction
simulator = R6::R6Class(
  "simulator", 
  list(
    initialize_run = function() {
      #' initialize_run
      #' @description Sets path dependencies and initial conditions for any simulation run
      source(paste0(model_path, "model/R/initialize_run.R"))
    }, 
    construct_model_objects = function(year) {
      #' construct_model_objects 
      #' 
      #' @description Runs the class constructors.
      #' 
      #' @details This function instantiates Model objects for the year 2000, the initial run year. Each class_object is associated with one or more class 
      #' manager objects. All Model objects are defined in the state_classes.R and state_managers.R files. These are sourced in the initialize_run method 
      #' of this class, and must be done prior to using this method.
      #' 
      #' @returns Set of Model objects, e.g., for land use, water footprint, trade, etc.
      if(year == 2000) {
        pop     <- population$new()
        lu      <- land_use$new()
        crp     <- crop$new()
        liv     <- livestock$new()
        wf      <- water_footprint$new() 
  
        pop_mng <- population_manager$new()
        lu_mng  <- land_use_manager$new()
        crp_mng <- crop_manager$new()
        trd_crp <- trade_crop_manager$new()
        liv_mng <- livestock_manager$new()
        wf_mng  <- water_footprint_manager$new()
        frst_mng<- forest_manager$new()
      }
    }, 
    population_module = function(iso3, year, population_policy_df) {
      pop$set_year(year)
      pop$set_iso_alpha3(iso3)
      pop$set_population(
        pop_mng$population(
          iso3 = pop$iso_alpha3, 
          year_index = pop$year, 
          population_data_df = pop$get_population_data(), 
          policy_df = population_policy_df
        )
      )
      pop$set_population_data()
      return(pop)
    }, 
    land_use_module = function(iso3, year, lu_types, land_use_policy_df) {
      lu$set_year(year)
      lu$set_iso_alpha3(iso3)
      for(lu_type in land_use_types) {
        lu$set_land_use_type(lu_type)
        lu$set_land_use_area(lu_mng$land_use_area(lu$iso_alpha3, lu$year, lu$land_use_type, land_use_data_df = lu$get_land_use_data(), policy_df = land_use_policy_df))
        lu$set_dry_matter_production(lu_mng$dry_matter_productivity(lu$land_use_type))
        lu$set_land_use_data()
      }
      return(lu)
    }, 
    crop_module = function(iso3, year, crop_types, crop_policy_df) {
      crp$set_year(year_index)
      crp$set_iso_alpha3(iso_code)
      for(crop_type in crop_types){
        crp$set_model_group(crop_type)
        
        crp$set_adjusted_cropland_area(crp_mng$adjusted_cropland_area(crp$iso_alpha3, crp$year, crp$model_group, land_use_area_df = lu$get_land_use_data(), policy_df = crop_policy_df))
        crp$set_cropland_allotment(crp_mng$cropland_allotment(crp_mng$adjusted_cropland_area(crp$iso_alpha3, crp$year, crp$model_group, crop_data_df = crp$get_crop_data(), policy_df = crop_policy_df)))
        crp$set_harvest_area(crp_mng$harvest_area(crp$adjusted_cropland_area, crp$ratio_of_land_allotted))
                                   
        # crp$set_harvest_area(crp_mng$harvest_area(crp$iso_alpha3, crp$year, crp$model_group, land_use_area_df = lu$get_land_use_data(), policy_df = crop_policy_df))
        crp$set_harvest_yield(crp_mng$harvest_yield(crp$iso_alpha3, crp$year, crp$model_group, crop_data_df = crp$get_crop_data(), policy_df = crop_policy_df))
        crp$set_production(crp_mng$crop_production(crp$harvest_area, crp$harvest_yield))
        crp$set_food_stock(crp$production*crp_mng$crop_allocation(crp$iso_alpha3, crp$year, crp$model_group, "food"))
        crp$set_feed_stock(crp$production*crp_mng$crop_allocation(crp$iso_alpha3, crp$year, crp$model_group, "feed"))
        crp$set_seed_stock(crp$production*crp_mng$crop_allocation(crp$iso_alpha3, crp$year, crp$model_group, "seed"))
        crp$set_losses_stock(crp$production*crp_mng$crop_allocation(crp$iso_alpha3, crp$year, crp$model_group, "loss")) 
        crp$set_processing_stock(crp$production*crp_mng$crop_allocation(crp$iso_alpha3, crp$year, crp$model_group, "proc"))
        crp$set_other_uses_stock(crp$production*crp_mng$crop_allocation(crp$iso_alpha3, crp$year, crp$model_group, "othe"))
        crp$set_country_data()
        crp$set_crop_data()
      }
      return(crp)
    }, 
    livestock_module = function(iso3, year, live_types, livestock_data_df) {
      liv$set_year(year)
      liv$set_iso_alpha3(iso3)
      for(live_type in live_types) {
        liv$set_model_group(live_type)
        liv$set_herd_tlu(liv_mng$herd_tlu(liv$model_group))
        # liv$set_stock_growth_rate(liv_mng$growth_rate(liv$iso_alpha3, liv$year, liv$model_group, df = liv$get_livestock_data()))
        liv$set_dairy_stock_quantity(liv_mng$product_usage(liv$model_group, "dairy")*liv_mng$quantity(liv$iso_alpha3, liv$year, liv$model_group, livestock_data_df = liv$get_livestock_data(), policy_df = livestock_policy_df))
        liv$set_meat_stock_quantity(liv_mng$product_usage(liv$model_group,   "meat")*liv_mng$quantity(liv$iso_alpha3, liv$year, liv$model_group, livestock_data_df = liv$get_livestock_data(), policy_df = livestock_policy_df))
        liv$set_other_stock_quantity(liv_mng$product_usage(liv$model_group, "other")*liv_mng$quantity(liv$iso_alpha3, liv$year, liv$model_group, livestock_data_df = liv$get_livestock_data(), policy_df = livestock_policy_df))
        liv$set_feed_drymatter_demand(liv_mng$feed_demand(liv$model_group, "FCR"))
        liv$set_feed_drymatter_demand(liv$feed_drymatter_demand*liv_mng$feed_demand(liv$model_group, "CP"))
        liv$set_livestock_data()
      }
      return(liv)
    }, 
    water_footprint_module = function(iso3, year, crop_types) {
      wf$set_year(year)
      wf$set_iso_alpha3(iso3)
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
      return(wf)
    }, 
    trade_module = function(crop_types) {
      for(crop_type in crop_types){ 
        crp$set_model_group(crop_type)
        crp$set_food_imports(trd_crp$imports(crp$iso_alpha3, crp$year, crp$model_group, "food", trade_crop_data_df = crp$get_trade_crop_data()))
        crp$set_feed_imports(trd_crp$imports(crp$iso_alpha3, crp$year, crp$model_group, "feed", trade_crop_data_df = crp$get_trade_crop_data()))
        crp$set_food_exports(trd_crp$exports(crp$iso_alpha3, crp$year, crp$model_group, "food", trade_crop_data_df = crp$get_trade_crop_data()))
        crp$set_feed_exports(trd_crp$exports(crp$iso_alpha3, crp$year, crp$model_group, "feed", trade_crop_data_df = crp$get_trade_crop_data()))
        crp$set_trade_crop_data()
      }
      return(crp)
    }, 
    run_test_simulation = function(time_frame, iso_codes) {
      #' run_test_simulation
      #' @description Runs simulation for global conditions, from year 2000 on
      #' @param list Vector or list of numerical years, from 2000
      #' @param list Vector or list of character ISO Alpha-3 codes for countries, e.g., "AFG"
      for(year_index in time_frame) {
        for(iso_code in iso_codes) { 
          self$construct_model_objects(year = year_index)
          self$population_module(iso3 = iso_code, year = year_index)
          self$land_use_module(iso3, year, lu_types, land_use_policy_df)
          self$crop_module(iso3, year, crop_types, crop_policy_df)
          self$livestock_module(iso3, year, live_types, livestock_data_df)
          self$water_footprint_module(iso3, year, crop_types)
          self$trade_module(crop_types)
          
          
        }
      }
    }, 
    run_global_simulation = function() {
      #' run_global_simulation
      #' @description Runs simulation for global conditions, from year 2000 on
      
    }
  )
)


# simulator
for(year_index in time_frame) {
  for(iso_code in iso_codes) {
    
    # construct_model_objects()
    if(year_index == 2000) {
      pop     <- population$new()
      lu      <- land_use$new()
      crp     <- crop$new()
      liv     <- livestock$new()
      wf      <- water_footprint$new() 
      ener    <- energy$new()
      
      pop_mng <- population_manager$new()
      lu_mng  <- land_use_manager$new()
      crp_mng <- crop_manager$new()
      trd_crp <- trade_crop_manager$new()
      liv_mng <- livestock_manager$new()
      wf_mng  <- water_footprint_manager$new()
      frst_mng<- forest_manager$new()
      ener_mng<- energy_manager$new()
    }
    
    #population module 1
    pop$set_year(year_index)
    pop$set_iso_alpha3(iso_code)
    pop$set_population(
      pop_mng$population(
        pop$iso_alpha3, 
        pop$year, 
        population_data_df = pop$get_population_data(), 
        policy_df = population_policy_df
        )
      )
    pop$set_population_data()
    
    #land use module 1
    lu$set_year(year_index)
    lu$set_iso_alpha3(iso_code)
    for(lu_type in land_use_types) {
      lu$set_land_use_type(lu_type)
      lu$set_land_use_area(lu_mng$land_use_area(lu$iso_alpha3, lu$year, lu$land_use_type, land_use_data_df = lu$get_land_use_data(), policy_df = land_use_policy_df))
      lu$set_dry_matter_production(lu_mng$dry_matter_productivity(lu$land_use_type))
      lu$set_land_use_data()
    }
    
    #crop module 1
    crp$set_year(year_index)
    crp$set_iso_alpha3(iso_code)
    for(crop_type in crop_types){
      crp$set_model_group(crop_type)
      
      crp$set_adjusted_cropland_area(crp_mng$adjusted_cropland_area(crp$iso_alpha3, crp$year, crp$model_group, land_use_area_df = lu$get_land_use_data(), policy_df = crop_policy_df))
      crp$set_land_allotted(crp_mng$cropland_allotment(crp$iso_alpha3, crp$year, crp$model_group, crop_data_df = crp$get_crop_data(), policy_df = crop_policy_df))
      crp$set_harvest_area(crp_mng$harvest_area(crp$adjusted_cropland_area, crp$ratio_of_land_allotted))
      
      # crp$set_land_allotted(crp_mng$crop_allotment(crp$iso_alpha3, crp$year, crp$model_group, crop_data_df = crp$get_crop_data(), policy_df = crop_policy_df))
      crp$set_harvest_yield(crp_mng$harvest_yield(crp$iso_alpha3, crp$year, crp$model_group, crop_data_df = crp$get_crop_data(), policy_df = crop_policy_df))
      crp$set_production(crp_mng$crop_production(crp$harvest_area, crp$harvest_yield))
      crp$set_food_stock(crp$production*crp_mng$crop_allocation(crp$iso_alpha3, crp$year, crp$model_group, "food", crop_data_df = crp$get_crop_data(), policy_df = cropland_allocation_policy_df))
      crp$set_feed_stock(crp$production*crp_mng$crop_allocation(crp$iso_alpha3, crp$year, crp$model_group, "feed", crop_data_df = crp$get_crop_data(), policy_df = cropland_allocation_policy_df))
      crp$set_seed_stock(crp$production*crp_mng$crop_allocation(crp$iso_alpha3, crp$year, crp$model_group, "seed", crop_data_df = crp$get_crop_data(), policy_df = cropland_allocation_policy_df))
      crp$set_loss_stock(crp$production*crp_mng$crop_allocation(crp$iso_alpha3, crp$year, crp$model_group, "loss", crop_data_df = crp$get_crop_data(), policy_df = cropland_allocation_policy_df)) 
      crp$set_proc_stock(crp$production*crp_mng$crop_allocation(crp$iso_alpha3, crp$year, crp$model_group, "proc", crop_data_df = crp$get_crop_data(), policy_df = cropland_allocation_policy_df))
      crp$set_othe_stock(crp$production*crp_mng$crop_allocation(crp$iso_alpha3, crp$year, crp$model_group, "othe", crop_data_df = crp$get_crop_data(), policy_df = cropland_allocation_policy_df))
      crp$set_country_data()
      crp$set_crop_data()
    }
    
    
    #livestock module 1
    liv$set_year(year_index)
    liv$set_iso_alpha3(iso_code)
    for(live_type in live_types) {
      liv$set_model_group(live_type)
      liv$set_herd_tlu(liv_mng$herd_tlu(liv$model_group))
      # liv$set_stock_growth_rate(liv_mng$growth_rate(liv$iso_alpha3, liv$year, liv$model_group, df = liv$get_livestock_data()))
      liv$set_dairy_stock_quantity(liv_mng$product_usage(liv$model_group, "dairy")*liv_mng$quantity(liv$iso_alpha3, liv$year, liv$model_group, livestock_data_df = liv$get_livestock_data(), policy_df = livestock_policy_df))
      liv$set_meat_stock_quantity(liv_mng$product_usage(liv$model_group,   "meat")*liv_mng$quantity(liv$iso_alpha3, liv$year, liv$model_group, livestock_data_df = liv$get_livestock_data(), policy_df = livestock_policy_df))
      liv$set_other_stock_quantity(liv_mng$product_usage(liv$model_group, "other")*liv_mng$quantity(liv$iso_alpha3, liv$year, liv$model_group, livestock_data_df = liv$get_livestock_data(), policy_df = livestock_policy_df))
      liv$set_feed_drymatter_demand(liv_mng$feed_demand(liv$model_group, "FCR"))
      liv$set_feed_drymatter_demand(liv$feed_drymatter_demand*liv_mng$feed_demand(liv$model_group, "CP"))
      liv$set_livestock_data()
    }
    
    #water demand (footprint) module 1 
    wf$set_year(year_index)
    wf$set_iso_alpha3(iso_code)
    for(crop_type in crop_types) {
      wf$set_model_group(crop_type)
      
      #find crop production
      crop_prod <- crp$crop_data %>% 
        dplyr::filter(iso_alpha3_code == wf$iso_alpha3 & year == wf$year & model_group == wf$model_group) %>% 
        dplyr::pull(production)
      
      wf$set_green_wf(wf_mng$get_crop_wf(wf$iso_alpha3, wf$model_group, "green")*crop_prod)
      wf$set_blue_wf(wf_mng$get_crop_wf(wf$iso_alpha3, wf$model_group, "blue")*crop_prod)
      wf$set_grey_wf(wf_mng$get_crop_wf(wf$iso_alpha3, wf$model_group, "grey")*crop_prod)
      wf$set_wf_data()
    }
    
    #energy: energy module 1
    ener$set_year(year_index)
    ener$set_iso_alpha3(iso_code)
    for(fuel_type in fuel_types) {
      ener$set_fuel_group(fuel_type)
      if(fuel_type %in% c("ethanol", "biofuel")) {
        fuel_sum = 0
        for(crop_type in crop_types) {
          fuel <- ener_mng$get_biofuel_feedstock(ener$iso_alpha3, ener$year, crop_type, crop_data_df = crp$crop_data)*ener_mng$get_biofuel_from_crops(crop_type, fuel_type)
          fuel_sum   <- fuel_sum + fuel
        }
        ener$set_quantity(fuel_sum)
        ener$set_kWh(0.277778*ener_mng$get_specific_energy(fuel_type)*ener$quantity)
        ener$set_t_CO2(ener_mng$get_emission_intensity(fuel_type)*ener$quantity)
      } else if(fuel_type == "biomass") {
        fuelwood <-  frst_mng$forest_harvest_area(ener$iso_alpha3, ener$year, lu$land_use_data)*frst_mng$harvest_wood()
        ener$set_quantity(frst_mng$get_charcoal(fuelwood, production_method = "industrial"))
        ener$set_kWh(0.277778*ener_mng$get_specific_energy(fuel_type)*ener$quantity)
        ener$set_t_CO2(ener_mng$get_emission_intensity(fuel_type)*ener$quantity)
      } else if(fuel_type %in% c("coal", "gas", "oil", "peat")) {
        ener$set_quantity(ener_mng$extract_fossil_fuels(ener$iso_alpha3, fuel_type))
        ener$set_kWh(0.277778*ener_mng$get_specific_energy(fuel_type)*ener$quantity)
        ener$set_t_CO2(ener_mng$get_emission_intensity(fuel_type)*ener$quantity)
      } else if(fuel_type %in% c("geothermal", "hydro", "solar", "wind")) {
        ener$set_quantity(NA)
        ener$set_kWh(0.277778*ener_mng$produce_renewables(ener$iso_alpha3, fuel_type))
        ener$set_t_CO2(0)
      } else {
        ener$set_quantity(NA)
        ener$set_kWh(0.277778*ener_mng$produce_nuclear(ener$iso_alpha3, fuel_type))
        ener$set_t_CO2(0)
      }
      ener$set_energy_data()
    }
    
    
    #' TODO animal products water footprint module
    
    #trade: crop module 2
    for(crop_type in crop_types){ 
      crp$set_model_group(crop_type)
      crp$set_food_imports(trd_crp$imports(crp$iso_alpha3, crp$year, crp$model_group, "food", trade_crop_data_df = crp$get_trade_crop_data()))
      crp$set_feed_imports(trd_crp$imports(crp$iso_alpha3, crp$year, crp$model_group, "feed", trade_crop_data_df = crp$get_trade_crop_data()))
      crp$set_food_exports(trd_crp$exports(crp$iso_alpha3, crp$year, crp$model_group, "food", trade_crop_data_df = crp$get_trade_crop_data()))
      crp$set_feed_exports(trd_crp$exports(crp$iso_alpha3, crp$year, crp$model_group, "feed", trade_crop_data_df = crp$get_trade_crop_data()))
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
    
    #population module 2
    #' TODO add net migration to population module
    # pop$set_migration(
    #   pop_mng$net_migration(
    #     pop$iso_alpha3, 
    #     pop$year, 
    #     population_data_df = pop$get_migration_data(), 
    #     policy_df = migration_policy_df
    #   )
    # )
    # pop$set_migration_data()
    
    #reporting module
    if(iso_code == max(iso_codes) & year_index == max(time_frame)) {
      readr::write_csv(pop$population_data, file = paste0(output_path, "population_data", ".csv"))
      readr::write_csv(lu$land_use_data, file = paste0(output_path, "land_use_data", ".csv"))
      readr::write_csv(crp$crop_data, file = paste0(output_path, "crop_data", ".csv"))
      readr::write_csv(crp$trade_crop_data, file = paste0(output_path, "trade_crop_data", ".csv"))
      readr::write_csv(liv$livestock_data, file = paste0(output_path, "livestock_data", ".csv"))
      readr::write_csv(wf$wf_data, file = paste0(output_path, "wf_data", ".csv"))
      readr::write_csv(wf$traded_wf_data, file = paste0(output_path, "traded_wf_data", ".csv"))
      readr::write_csv(ener$energy_data, file = paste0(output_path, "energy_data", ".csv"))
    }
    
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

# wf_df <- wf$get_wf_data() %>% 
#   dplyr::full_join(wf$get_traded_wf_data()) 
# 
# wf_df_agg <- wf$get_wf_data() %>% 
#   dplyr::full_join(wf$get_traded_wf_data()) %>% 
#   dplyr::group_by(iso_alpha3_code, fao_countrycode, year) %>% 
#   dplyr::summarize(blue_wf = sum(blue_wf, 
#                                   na.rm = TRUE), 
#                    green_wf = sum(green_wf, 
#                                   na.rm = TRUE),
#                    grey_wf = sum(grey_wf, 
#                                   na.rm = TRUE)) %>% 
#   tidyr::pivot_longer(., 
#                       cols = blue_wf:grey_wf, 
#                       names_to = "wf_type", 
#                       values_to = "water_m3_per_yr")
# 
# write_csv(wf_df,     path = paste0(output_path, "/wf_data.csv"))
# write_csv(wf_df_agg, path = paste0(output_path, "/wf_data_aggregated.csv"))

