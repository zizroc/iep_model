# manages state classes
library(roxygen2)
library(docstring)

source(paste0(model_path, "/model/R/data/policy_data.R"))

population_manager <- R6::R6Class(
  "population_manager", 
  list( 
    population = function(iso3, year_index, population_data_df = NULL, policy_df = NULL) {
      #' population 
      #' 
      #' @description Calculates number of humans in the specified country
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical year
      #' @param population_data_df Data frame containing historical population data passed from population class.
      #' @return numerical Crude growth rate (persons added per 1e5 people per year)
      if(year_index == 2000) {
        
        pop <- population_un %>% 
          dplyr::filter(iso_alpha3 == iso3 & year == year_index) %>% 
          dplyr::pull(population_total)
        
        if(length(pop) != 0) {
          return(pop)
        } else {
          return(0)
        }
        
      } else {
        pop <- population_data_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index-1) %>% 
          dplyr::pull(population)
        
        cgr <- policy_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index) %>% 
          dplyr::pull(cgr)
        
        if(length(pop) != 0 & length(cgr) != 0) {
          pop <- pop + pop*cgr
          return(pop)
        } else {
          return(0)
        }
      } 
    }, 
    net_migration = function(iso3, year_index, migration_data_df = NULL, policy_df = NULL) {
      #' net_migration 
      #' 
      #' @description Calculates net number of humans that immigrate to the specified country
      #' 
      #' @details Resource constraints in the Model do not change the crude growth rate; but they do change 
      #' the crude net migration rate (CNMR).
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical year
      #' @param migration_data_df Data frame containing historical migration data passed from population class.
      #' @return numerical Net number of human immigrants
      if(year_index == 2000) {
        
        ntmg <- population_un %>% 
          dplyr::filter(iso_alpha3 == iso3 & year == year_index) %>% 
          dplyr::pull(net_migrants)
        
        if(length(ntmg) != 0) {
          return(ntmg)
        } else {
          return(0)
        }
        
      } else {
        ntmg <- migration_data_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index-1) %>% 
          dplyr::pull(net_migrants)
        
        cnmr <- policy_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index) %>% 
          dplyr::pull(cnmr)
        
        if(length(ntmg) != 0 & length(cnmr) != 0) {
          ntmg <- ntmg + ntmg*cnmr
          return(ntmg)
        } else {
          return(0)
        }
      } 
    }
))

land_use_manager <- R6::R6Class(
  "land_use_manager", 
  list(
    land_use_area  = function(iso3, year_index, use_group, land_use_data_df = NULL, policy_df = NULL) { 
      #' land_use_area 
      #' 
      #' @description Calculates area (hectares) under management of specified land use
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical year
      #' @param use_group character string land use type c("cropland", "fallow_cropland", "pasture", "forest", "otherland") 
      #' @param land_use_data_df Data frame passed from land_use class
      #' @param policy_df Data frame containing land use change values (must be exogenously set)
      if(year_index == 2000) { 
        if(use_group != "fallow_cropland") {
          lua <- landuse_area %>% 
            dplyr::filter(iso_alpha3 == iso3 & year == 2000 & land_use_type == use_group) %>% 
            dplyr::pull(area_ha)
        } else {
          arable_area  <- landuse_area %>% 
            dplyr::filter(iso_alpha3 == iso3 & year == 2000 & land_use_type == "cropland") %>% 
            dplyr::pull(area_ha)

          harvest_area <- total_area_harvested %>% 
            filter(iso_alpha3 == iso3 & Year == 2000) %>% 
            pull(sum_value)
          
          lua <- arable_area - harvest_area
        }
      } else {
        lua <- land_use_data_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index-1 & land_use_type == use_group) %>% 
          dplyr::pull(land_use_area)
        
        delta_lua <- policy_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index & land_use_type == use_group) %>% 
          dplyr::pull(delta_land_use_area)
        
        lua <- lua + lua*delta_lua
      }
      
      if(length(lua) != 0) {
        return(lua)
      } else {
        return(0)
      }
    }, 
    #' fallow_area = function(iso3, year_index, land_use_data_df = NULL, crop_data_df = NULL) { 
    #'   #' fallow_area 
    #'   #' 
    #'   #' @description Area of cropland uncropped/unharvested (ha). NB: Deprecated and handled as fallow_cropland by land_use_area function.
    #'   #' 
    #'   #' @details Not all available cropland is put into production. Some fraction is held fallow or rested. The relationship to cropland and 
    #'   #' harvested_area is: cropland = harvested_area + fallow_area. NB: A negative value for fallow_area indicates bad data.
    #'   #' 
    #'   #' @param iso3 character ISO Alpha-3 code
    #'   #' @param year_index numerical year
    #'   #' @param land_use_data_df Data frame passed from land_use class
    #'   #' @param 
    #'   #' @return numerical Area of fallow land (ha) 
    #'   if(year_index == 2000) {
    #'     arable_area <- landuse_area %>% 
    #'       dplyr::filter(iso_alpha3 == iso3 & year == 2000 & land_use_type == "cropland") %>% 
    #'       dplyr::pull(area_ha)
    #'     
    #'     harvested_area <- total_area_harvested %>% 
    #'       filter(iso_alpha3 == "AFG" & Year == 2000) %>% 
    #'       pull(sum_value)
    #'     
    #'   } else {
    #'     arable_area <- land_use_data_df %>% 
    #'       dplyr::filter(iso_alpha3_code == iso3 & year == year_ind & land_use_type == "cropland") %>% 
    #'       dplyr::pull(land_use_area)
    #'     
    #'     harvested_area <- crop_data_df %>% 
    #'       dplyr::filter(iso_alpha3_code == iso3 & year == year_ind) %>% 
    #'       dplyr::group_by(iso_alpha3_code, year) %>% 
    #'       dplyr::summarize(sum_harv_area = sum(harvest_area, 
    #'                                            na.rm = TRUE), 
    #'                        .groups = "drop") %>% 
    #'       dplyr::pull(sum_harv_area)
    #'   }
    #'   
    #'   if(length(arable_area) != 0 & length(harvested_area) != 0) {
    #'     fallow_area <- arable_area - harvested_area
    #'     return(fallow_area) 
    #'   } else {
    #'     fallow_area <- 0
    #'   }
    #' }, 
    dry_matter_productivity = function(use_group) {
      #' dry_matter_productivity 
      #' 
      #' @description Calculates the dry matter production density (tonnes per hectare) under management of specified land use type. Presently for pasture only.
      #' 
      #' TODO Make this less janky: there should be different DM productivity for different countries/climate zones; should be 
      #' impacted by climate change.
      #' 
      #' @param use_group character land use type c("cropland", "permanent_cropland", "arable_land", "pasture", "forest", "otherland")
      pasture_DM_productivity = data.frame(
        land_use_type    = c("cropland", "permanent_cropland", "arable_land", "pasture", "forest", "otherland"), 
        tonnes_DM_per_ha = c(0, 0, 0, 10, 0, 0)
      )
      x <- pasture_DM_productivity %>% 
        dplyr::filter(land_use_type == use_group) %>% 
        dplyr::pull(tonnes_DM_per_ha)
      
      if(length(x) != 0) {
        return(x)
      } else {
        return(0)
      }
    }
  )
)

crop_manager <- R6::R6Class(
  "crop_manager", 
  # inherit = crop, 
  list(
    adjusted_cropland_area  = function(iso3, year_index, crop_group, land_use_area_df = NULL, policy_df = NULL) {
      #' adjusted_cropland_area
      #' 
      #' @description Merely passes cropland area from land_area/land_area_manager classes to this class.
      #'       #' 
      #' @details Harvest area initial values are set based on historic FAO data. For years > 2000, "cropland area" is passed from the land_use_manager and 
      #' "cropland allotted" is passed from the crop_manager for each crop type. Not all available cropland is planted/harvested, and adjusted_cropland_area 
      #' is the subset of cropland area that was planted with crops minus fallow/rested land;  i.e., adjusted_cropland_area is the difference between cropland 
      #' and fallow_area (alternatively, cropland is the sum of adjusted_cropland_area and fallow_area).
      #' 
      #' NB: This function requires that the base_data.R module is run first, to fill the crops_area_harvested data frame for year 2000.
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical Year
      #' @param crop_group character Crop type, e.g., c("cereal", "pulse", "oilcrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' @param land_use_area_df numeric Data frame passed from land_use module. Value is cropland area harvested, minus fallow land. Used for year > 2000.
      #' @param policy_df Data frame containing crop yield change values (must be exogenously set)
      #' @return hectares harvested for given parameters
      
      if(year_index == 2000) { 
        harv_area <- landuse_area %>% 
          dplyr::filter(iso_alpha3 == iso3 & year == 2000 & land_use_type == "cropland") %>% 
          dplyr::pull(area_ha)
        
        if(length(harv_area) != 0) {
          return(harv_area)
        } else {
          return(0)
        }
        
      } else {
        harv_area <- land_use_area_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index-1 & land_use_type == "cropland") %>% 
          dplyr::pull(land_use_area)
        
        if(length(harv_area) != 0) {
          return(harv_area)
        } else {
          return(0)
        }
      }
    }, 
    cropland_allotment = function(iso3, year_index, crop_group, crop_data_df = NULL, policy_df = NULL) {
      #' cropland_allotment
      #'
      #' @description Manages the proportion of cropland used for crop production.
      #'
      #' @details Not all cropland is put into production. Once fallow land area is subtracted from available cropland, crop production values
      #' are determined by their proportion to the whole, by a ratio_of_land_allotted value, passed from the policy_df.
      #'
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical year
      #' @param crop_group character string crop type
      #' @param crop_data_df Data frame containing historical crop values
      #' @param policy_df Data frame containing values for the ratio of available cropland allotted for the production of each crop type.
      #' @return proportion between 0 and 1
      if(year_index == 2000) {
        ratio_allotted <- crops_area_allotted %>% 
          dplyr::filter(iso_alpha3 == iso3 & year == year_index & crop_type == crop_group) %>% 
          dplyr::pull(relative_area_allotted)
        
        if(length(ratio_allotted) != 0) { 
          return(ratio_allotted)
        } else {
          return(0)
        }
        
      } else {
        ratio_allotted <- crop_data_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index & model_group == crop_group) %>% 
          dplyr::pull(ratio_of_land_allotted)
        
        delta_ratio_allotted <- policy_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index & model_group == crop_group) %>% 
          dplyr::pull(delta_allocation)
        
        if(length(ratio_allotted) != 0 & length(delta_ratio_allotted) != 0) {
          ratio_allotted <- ratio_allotted + ratio_allotted*delta_ratio_allotted
        } else {
          ratio_allotted <- 0
        }
        return(ratio_allotted)
      }
    }, 
    harvest_area  = function(adjusted_cropland_area, crop_allotment) {
      #' harvest_area
      #' 
      #' @description Product of adjusted_cropland_area and crop_allotment
      #' 
      #' @details Multiplies adjusted_cropland_area by crop_allotment to determine the area for each crop type to be harvested. The sum of harvest_area values 
      #' for all crop types gives the adjusted_cropland_area.
      #' 
      #' NB: This function requires that the base_data.R module is run first, to fill the crops_area_harvested data frame for year 2000.
      #' 
      #' @param adjusted_cropland_area numerical Passed from this class.
      #' @param crop_allotment numerical Passed from crop class (ratio between 0 and 1)
      #' @return hectares harvested for given parameters
      
      if(length(adjusted_cropland_area) != 0 & length(crop_allotment) != 0) {
        harv_area <- adjusted_cropland_area*crop_allotment
      } else {
        harv_area <- 0
      }
      return(harv_area) 
    }, 
    harvest_yield = function(iso3, year_index, crop_group, crop_data_df = NULL, policy_df = NULL) { 
      #' harvest_yield 
      #' 
      #' @description Calculates the harvest yield (t/ha) under management of specified land use type. Cropland only.
      #' 
      #' @details Harvest yield initial values are set based on historic FAO data. For years > 2000, harvested yields are calculated as the sum of 
      #' the prior year's yield, passed from the crop_data container in the crop class, and the change in yield for the intervening period, i.e., 
      #' evaluated for the current year. The change in yield is computed automatically by the function as the product of the prior year's yield and 
      #' an annual percentage growth term (delta_harvest_yield) passed from a crop_policy_df data frame. The values in this crop policy data frame 
      #' are set extrinsically, not intrinsically derived in the Model. NB: This function requires that the base_data.R module is run first, to fill 
      #' the crops_yield data frame for year 2000.
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical year
      #' @param crop_group character string crop type
      #' @param crop_data_df Data frame containing historical data passed from crop class
      #' @param policy_df Data frame containing crop yield change values (must be extrinsically set) 
      #' @return numeric Harvest yield (tonnes/ha) 
      if(year_index == 2000) {
        yield <- crops_yield %>% 
          dplyr::filter(iso_alpha3 == iso3 & Year == 2000 & crop_type == crop_group) %>% 
          dplyr::pull(value)
        
        if(length(yield) != 0) {
          return(yield)
        } else {
          return(0)
        }
        
      } else {
        yield       <- crop_data_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index-1 & model_group == crop_group) %>% 
          dplyr::pull(harvest_yield)
        
        delta_yield <- policy_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index & model_group == crop_group) %>% 
          dplyr::pull(delta_harvest_yield)
        
        if(length(yield) != 0) {
          return(yield + yield*delta_yield)
        } else {
          return(0)
        }
      }
    }, 
    crop_production = function(harvest_area, harvest_yield) {
      #' crop_production 
      #' 
      #' @description Calculates crop production (tonnes) as the product of harvest yield and harvest area
      #' 
      #' @details Crop production is the product of harvest_area and harvest_yield, which are passed from other functions in this class. 
      #' If either term is non-numerical, crop production will return zero without throwing an error. Users should investigate results to 
      #' confirm that production was computed properly. Crop_production is computed as the product of harvest_area, cropland_allotment, and 
      #' harvest_yield for each crop type.
      #' 
      #' @param harvest_area numerical Harvest area output of harvest_area (ha)
      #' @param harvest_yield numerical Harvest yield output of harvest_yield (t/ha)
      #' @return tonnes of crop
      if(length(harvest_area) != 0 & length(harvest_yield) != 0) {
        return(harvest_area*harvest_yield)
      } else {
        return(0)
      }
    },
    crop_allocation = function(iso3, year_index, crop_group, crop_use, crop_data_df = NULL, policy_df = NULL) {
      #' crop_allocation 
      #' 
      #' @description Calculates the ratio of crop production allocated to one of six uses, i.e., food, feed, seed, processing, losses, and other uses
      #' 
      #' @details After crops are harvested, the Model allocates them to primary uses based on FAO categories, i.e., food for human consumption, feed for 
      #' livestock consumption, seed for crop reproduction, processing ("proc") for non-food industrial uses such as in textile manufacture, and other uses 
      #' ("othe") such as for biofuels. Following FAO, the Model treats harvest and value-chain waste and/or losses ("loss") as an allocation. Crops for 
      #' import/export are not yet explicitly considered. For each crop_group, crop allocation values for all six crop_use categories must sum to 1.
      #' 
      #' The data frame domestic_production_relative is generated by a diet_policy_data.R script and must be run to set the initial crop allocation values.
      #' 
      #' Changes in crop allocation are presently fixed by the crop_policy_df. We intend for these to eventually dynamically respond to ranked demands, e.g., 
      #' food caloric and nutritional requirement, followed by the same for livestock, followed by seed replacement requirement, etc.
      #' 
      #' TODO Roll the domestic_production_relative generator into the base_data.R script
      #' TODO Add logic to deal with export/import of domestic over/under-production
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical Year
      #' @param crop_group character Crop type, e.g., c("cereal", "pulse", "oilcrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' @param crop_use character Main allocation for crop, e.g., c("food", "feed", "seed", "loss", "proc", "othe")
      #' @param crop_data_df Data frame containing historical data passed from crop class
      #' @param policy_df Data frame containing any changes in crop allocation based on policy.
      #' @return proportion between 0 and 1 
      #' 
      if(year_index == 2000) {
        rel_dom_prod <- domestic_production_relative %>% 
          dplyr::filter(iso_alpha3 == iso3 & year == 2000 & model_group == crop_group) %>% 
          dplyr::pull(paste0(crop_use,"_ratio"))
        
        if(length(rel_dom_prod) != 0) {
          return(rel_dom_prod)
        } else {
          return(0)
        }
      } else {
        
        #this is where feedback logic will go
        
        #crop_data_df is not for relative data
        
        rel_dom_prod      <- crop_data_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index-1 & model_group == crop_group) %>% 
          dplyr::select(paste0(crop_use, "_stock"), production) %>% 
          dplyr::mutate(allocated_ratio = .[1]/production) %>% 
          dplyr::pull(allocated_ratio) %>% 
          as.numeric()
        
        delta_rel_dom_prod <- policy_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index & model_group == crop_group) %>% 
          dplyr::pull(paste0("delta_", crop_use, "_ratio")) %>% 
          as.numeric()
        
        rel_dom_prod  <- rel_dom_prod + delta_rel_dom_prod
        
        if(length(rel_dom_prod) != 0) {
          return(rel_dom_prod)
        } else {
          return(0)
        }
      }
    }
  )
)

trade_crop_manager <-  R6::R6Class(
  "trade_crop_manager", 
  list(
    imports = function(iso3, year_index, crop_group, crop_use, trade_crop_data_df = NULL) {
      #' imports 
      #' 
      #' @description Manages imports allocated to food and feed uses. 
      #' 
      #' @details Trade occurs during the final phase of each time step in the Model. The Model checks to confirm that domestic crop production satisfies domestic 
      #' demands for consumption. If domestic production is insufficient, the deficit is imported; if domestic production is excessive, the surplus is exported. Imports 
      #' go to, and exports come from, a clearinghouse that is presently infinite in abundance and capacity. NB: Because of this treatment, there is no direct policy 
      #' adjustment from, say a trade_crop_policy_df, and even though the year argument filters on the present year, it is pulling historic data that occurred earlier 
      #' in the time step.
      #' 
      #' Limitations: This function only handles food and feed crop trades, which are the majority uses globally. Eventually it will be extended to seed, processing, 
      #' losses, and other uses. In addition, the clearinghouse will be parametrized to model global trade territorial and political dependencies.
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical Year
      #' @param crop_group character Crop type, e.g., c("cereal", "pulse", "oilcrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' @param crop_use character Crop allocation, e.g., c("food", "feed")
      #' @param trade_crop_data_df Data frame containing historical trade data passed from crop class
      #' @return numerical imported crops (tonnes) 
      if(year_index == 2000) {
        #fill in some historic data here
        return(0)
      } else {
        imports <- trade_crop_data_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index & model_group == crop_group) %>% 
          dplyr::pull(paste0(crop_use, "_imports"))
        
        if(length(imports) != 0) {
          return(imports)
        } else {
          return(0)
        }
      }
    }, 
    exports = function(iso3, year_index, crop_group, crop_use, trade_crop_data_df = NULL) {
      #' exports 
      #' 
      #' @description Manages exports allocated to food and feed uses. 
      #' 
      #' @details Trade occurs during the final phase of each time step in the Model. The Model checks to confirm that domestic crop production satisfies domestic 
      #' demands for consumption. If domestic production is insufficient, the deficit is imported; if domestic production is excessive, the surplus is exported. Imports 
      #' go to, and exports come from, a clearinghouse that is presently infinite in abundance and capacity. NB: Because of this treatment, there is no direct policy 
      #' adjustment from, say a trade_crop_policy_df, and even though the year argument filters on the present year, it is pulling historic data that occurred earlier 
      #' in the time step.
      #' 
      #' Limitations: This function only handles food and feed crop trades, which are the majority uses globally. Eventually it will be extended to seed, processing, 
      #' losses, and other uses. In addition, the clearinghouse will be parametrized to model global trade territorial and political dependencies.
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical Year
      #' @param crop_group character Crop type, e.g., c("cereal", "pulse", "oilcrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' @param crop_use character Crop allocation, e.g., c("food", "feed")
      #' @param trade_crop_data_df Data frame containing historical trade data passed from crop class
      #' @return numerical exported crops (tonnes) 
      if(year_index == 2000) {
        #fill in some historic data here
        return(0)
      } else {
        exports <- trade_crop_data_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index & model_group == crop_group) %>% 
          dplyr::pull(paste0(crop_use, "_exports"))
        
        if(length(exports) != 0) {
          return(exports)
        } else {
          return(0)
        }
      }
    }
  )
)

livestock_manager <- R6::R6Class(
  "livestock_manager", 
  list(
    quantity = function(iso3, year_index, live_group, livestock_data_df = NULL, policy_df = NULL) { 
      #' quantity 
      #' 
      #' @description Manages quantity of livestock. 
      #' 
      #' @details Livestock are counted by head (individuals), except for fish, which are counted by tonne, and bees, which are counted by hive. The Model 
      #' quantifies all terrestrial livestock by head (and number of hives), unlike FAO which classifies small animals, e.g., fowl and rodents, by 1000 head. 
      #' Livestock quantity changes as a combined result of management practices (slaughter, breeding, imports/exports). This is represented by a crude annual 
      #' growth rate (CGR), computed by the function automatically. The same types of livestock grow at equivalent rates regardless of their use category, i.e., 
      #' bovine_dairy and bovine_meat have the same CGR in the Model.
      #' 
      #' The function pulls initial data from livestock_filtered, based on FAO numbers, for year 2000. Run the base_data.R script to load these data. NB: If either 
      #' historical livestock data or livestock_policy_df pull non-numeric values, the function will return zero without throwing an error, so you should 
      #' inspect initial results.
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical Year
      #' @param live_group character Livestock group, e.g., c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
      #' @param livestock_data_df Data frame containing historical livestock numbers passed from livestock class
      #' @param policy_df Data frame with growth rates for animal stocks, set by growth_rate function
      #' @return numerical Individual head of livestock
      if(year_index == 2000) {
        quant <- livestock_filtered %>% 
          dplyr::filter(iso_alpha3  == iso3 & Year == 2000 & livestock_group == live_group) %>% 
          dplyr::pull(stock) %>% 
          sum(., 
              na.rm = TRUE)
        
        return(quant)
      } else {
        quant <- livestock_data_df %>% 
          dplyr::filter(iso_alpha3_code  == iso3 & year == year_index-1 & model_group == live_group) %>% 
          dplyr::pull("total_stock_quantity")
        
        cgr   <- policy_df %>% 
          dplyr::filter(iso_alpha3_code  == iso3 & year == year_index & model_group == live_group) %>% 
          dplyr::pull("crude_growth_rate")
        
        if(length(quant) != 0 & length(cgr) != 0) {
          return(quant + quant*cgr)
        } else {
          return(0)
        }
      }
    }, 
    #' growth_rate = function(x1, x2, x3, df = NULL) { 
    #'   #' growth_rate 
    #'   #' 
    #'   #' Livestock quantity changes as a combined result of management practices (slaughter, breeding, imports/exports). This is represented by a crude annual growth rate.
    #'   #' 
    #'   #' @param x1 numerical country code used by FAO
    #'   #' @param x2 numerical year
    #'   #' @param x3 character string livestock type, e.g., c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
    #'   #' @param df data frame with previous time step growth rate
    #'   #' @return numerical crude growth rate 
    #'   if(x2 == 2000) {
    #'     return(0)
    #'   } else {
    #'     x <- df %>% 
    #'       dplyr::filter(fao_countrycode == x1 & year == x2-1 & model_group == x3) %>% 
    #'       dplyr::pull(crude_growth_rate)
    #'     
    #'     y <- livestock_growth_policy_df %>% 
    #'       dplyr::filter(livestock_group == x3) %>% 
    #'       dplyr::pull(growth)
    #'     
    #'     if(length(x) != 0 & length(y) != 0) {
    #'       return(x + y)
    #'     } else {
    #'       return(0)
    #'     }
    #'   }
    #' }, 
    product_usage = function(live_group, use_type) { 
      #' product_usage 
      #' 
      #' 
      #' @description Manages the proportion of livestock designated for primary usage category. 
      #' 
      #' @details Livestock are raised for different purposes (e.g., dairy and beef cattle). The model does not presently allow for mixed use of livestock. 
      #' It depends on primary uses of animals by type, and presently does not depend on geography or time.
      #' 
      #' The function pulls initial data from livestock_filtered, based on FAO numbers, for year 2000. Run the base_data.R script to load these data. NB: If 
      #' either historical livestock data or livestock_policy_df pull non-numeric values, the function will return zero without throwing an error, so you should 
      #' inspect initial results.
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical Year
      #' @param live_group character Livestock group, e.g., c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
      #' @param use_type character Main usage for the livestock group, e.g., c("broiler", "layer", "honey", "dairy", "meat", "other")
      #' 
      #' TODO add country and time dependence
      #' 
      #' @return numerical Dimensionless proportion between 0 and 1
      use_ratio <- stock_usage_proportion_df %>% 
        dplyr::filter(livestock_group == live_group, stock_usage == use_type) %>% 
        dplyr::pull(proportion)
      if(length(use_ratio) != 0) {
        return(use_ratio)
      } else {
        return(0)
      }
    }, 
    feed_demand = function(live_group, feed_type) {
      #' feed_demand 
      #' 
      #' @description Manages the feed dry matter (DM) and crude protein (CP) demands of livestock based on feed conversion rate (FCR)
      #' 
      #' @details The numbers in livestock_feed_df are amalgamated from several sources, given in references. Aves: NRC (1994), Miller (2004). Bovines: Kassam et al. (1991), Miller (2004); for  
      #' 
      #' TODO make this less janky
      #' 
      #' @param live_group character Livestock group, e.g., livestock_group = c("aves", "bovine", "camelid", "caprine", "equine", "fish", "insecta", "rodentia", "sus")
      #' @param feed_matter character Feed conversion ratio (FCR) or crude protein (CP) e.g., feed_type = c("FCR", "CP")
      #' @return numerical (for FCR, multiplier of DM in tonnes per livestock tonne or, for CP, proportion between 0 and 1)
      #' @references Alexander, P., Brown, C., Arneth, A., Dias, C., Finnigan, J., Moran, D. and Rounsevell, M.D. (2017). Could consumption of insects, cultured meat or imitation meat reduce global agricultural land use?. Global Food Security, 15, pp.22-32. 
      #' Alexander, P., Brown, C., Arneth, A., Finnigan, J., Moran, D., & Rounsevell, M. D. (2017). Losses, inefficiencies and waste in the global food system. Agricultural systems, 153, 190-200.https://doi.org/10.1016/j.agsy.2017.01.014
      #' Anderson, T., Hoffman, P. (2006). Focus On Forage, 8(1). 
      #' Halls, A. E. (2010). Nutritional Requirements for Rabbits. Shur-Gain, Nutreco Canada Inc.
      #' Miller, E.L. (2004). Protein nutrition requirements of farmed livestock and dietary supply. In Protein sources for the animal feed industry, expert consultation and workshop, Bangkok (pp. 29-76). Rome: Food and Agriculture Organization of the United Nations.
      #' New, M. (1987). Feed and feeding of fish and shrimp: A manual on the preparation and presentation of compound feeds for shrimp and fish in aquaculture. (No. F009. 102). FAO.
      #' Novak, S., Shoveller, A. K., Warren, L. K. (2008). Nutrition and Feeding Management for Horse Owners. Alberta Agriculture and Rural Development, Edmonton.
      #' National Research Council, (1994). Nutrient Requirements of Poultry Ninth Revised Edition National Academy Press. Washington DC.
      #' Rashid, M. (2008). Goats and Their Nutrition. Manitoba Goat Association, Manitoba Agriculture, Food and Rural Initiatives. March.
      #' Sheep Nutrition Fact Sheet (n.d.). Saskatchewan Sheep Development Board & Saskatchewan Ministry of Agriculture, Saskatoon.
      #' Van Huis, A., Van Itterbeeck, J., Klunder, H., Mertens, E., Halloran, A., Muir, G. and Vantomme, P. (2013). Edible insects: future prospects for food and feed security (No. 171). Food and Agriculture Organization of the United Nations.
      #' Vasal, S.K. (2004). The role of high lysine cereals in animal and human nutrition in Asia. Protein sources for the animal feed industry. Rome: FAO, pp.167-184.
      #' Wilson, J.G. (2002). Productivity, fisheries and aquaculture in temperate estuaries. Estuarine, Coastal and Shelf Science, 55(6), pp.953-967.
      #' McGregor, B. (2007). Meat and Offal Yields of Goats, Agriculture Victoria. http://agriculture.vic.gov.au/agriculture/livestock/goats/production/meat-and-offal-yields-of-goats#:~:text=The%20mean%20live%20weight%20of,and%20sampled%20for%20chemical%20analyses.
      #' Agriculture Victoria. (2017). Feedlotting Lambs, Agriculture Victoria. http://agriculture.vic.gov.au/agriculture/livestock/sheep/feeding-and-nutrition/feedlotting-lambs
      #' Hashi, A.M., Kamoun, M. and Cianci, D. (1995). Feed requirements of the camel. Elevage et alimentation du dromadaire. Zaragoza: CIHEAM, pp.71-80.
      livestock_feed_df <- tibble(
        livestock_group = c("aves", "bovine", "camelid", "caprine", "equine", "fish", "insecta", "rodentia", "sus"), 
        CP  = c(0.2, 0.11, 0.11, 0.112, 0.113, 0.62, 0.1, 0.15, 0.2), 
        FCR = c(2, 6, 6.5, 5, 6, 1.5, 1.68, 3, 3)
      )
      x <- livestock_feed_df %>% 
        dplyr::filter(livestock_group == live_group)
      if(feed_type == "CP") {
        x <- x %>% 
          dplyr::pull(CP)
        return(x)
      } else if(feed_type == "FCR") {
        x <- x %>% 
          dplyr::pull(FCR)
        if(length(x) != 0) {
          return(x)
        } else {
          return("Error: check feed_type. Must be CP or FCR.") 
        }
      } else {
        return("Error: check live_type.") 
      }
    }, 
    herd_tlu = function(live_group, tlu_def_df = NULL) { 
      #' herd_tlu 
      #' 
      #' @description Average TLU per herd animal
      #' 
      #' @details One TLU (tropical livestock unit) is defined as equivalent to 250 kg. Herd TLU values vary by context, and may be defined separately at country-level. Users 
      #' may opt to do this by entering a tlu_def_df with appropriate herd_tlu and ISO3 code entries, as shown in the Example. These values must sum to 1.
      #' 
      #' TLU for aves, bovine, and sus were taken from, or inferred from data found in, FAOSTAT (2000) which cites Jahnke et al. (1988); for caprine, camelid, equine, and rodentia 
      #' from Kassam et al (1991), FAO & IIASA.
      #' 
      #' @param live_group character Livestock group, e.g., livestock_group = c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
      #' @param tlu_def_df Optional data frame that may be entered by the user. If omitted, default values are entered.
      #' 
      #' @return numerical TLU per herd animal
      #' 
      #' @example 
      #' tlu_def_df = data.frame(
      #' iso_alpha3_code = rep("CHN", 7), 
      #' live_type = c("aves", "bovine", "caprine", "camelid", "equine", "rodentia", "sus"), 
      #' herd_tlu  = c(  1e-5,        1,      0.09,      1.13,     0.75,       2e-5,   0.2)
      #' )
      #' 
      #' @references Jahnke, H.E., Tacher, G., Kiel, P. & Rojat, D. 1988. Livestock production in tropical Africa, with special reference to the tsetse-affected zone. Livestock production in tsetse-affected areas of Africa. Proceedings of a meeting held in Nairobi, 23-27 November 1987. Nairobi, ILCA/International Laboratory for Research on Animal Diseases (ILRAD). pp 3-21. 
      #' FAOSTAT. 2000. Rome: Food and Agriculture Organization. 
      #' Kassam, A. H. 1991. Agro-ecological Land Resources Assessment for Agricultural Development Planning, A Case Study of Kenya : Resources Data Base and Land Productivity. Rome and Laxenburg: Food and Agriculture Organization and International Institute for Applied Systems Analysis.
      #' 
      # tlu <- tlu_def_df %>% 
      #   dplyr::filter(iso_alpha3_code == iso3 & year == year_index & live_type == live_group) %>% 
      #   dplyr::pull(herd_tlu)
      
      tlu <- data.frame(
        live_type = c("aves", "bovine", "caprine", "camelid", "equine", "rodentia", "sus"),
        herd_tlu  = c(  1e-5,        1,      0.09,      1.13,     0.75,       2e-5,   0.2)
      ) %>% 
        dplyr::filter(live_type == live_group) %>% 
        dplyr::pull(herd_tlu)
      
      if(length(tlu) != 0) {
        return(tlu)
      } else {
        return("Error: check live_type")
      }
    }
  )
)

water_footprint_manager <- R6::R6Class(
  "water_footprint_manager", 
  list(
    get_crop_wf = function(iso3, crop_group, wf_group) { 
      #' get_crop_wf 
      #' 
      #' @description Water footprint (cubic meters per crop tonne) of crops and derived crop products (1996-2005) from Mekonnen and Hoekstra (2011).
      #' @details
      #' Blue Water Footprint: The amount of surface water and groundwater required (evaporated or used directly) to produce an item.
      #' Green Water Footprint: The amount of rainwater required (evaporated or used directly) to make an item. 
      #' Grey Water Footprint: The amount of freshwater required to dilute the wastewater generated in manufacturing, in order to maintain water quality , as determined by state and local standards. 
      #' @param iso3 character Country-level ISO Alpha 3 code, e.g., iso3 = "CAN"
      #' @param crop_group character Type of crop, e.g., crop_group = c("cereal", "pulse", "rootstubers", "fibercrop", "citrus", "fruit", "vegetable", "oilcrop", "treenut", "sugarcrop")
      #' @param wf_group character Water footprint type, e.g., wf = c("green", "blue", "grey")
      #' 
      #' @return numerical water footrpint (cubic meters per crop tonne)
      #' @references Mekonnen, M.M. & Hoekstra, A.Y. (2011) The green, blue and grey water footprint of crops and derived crop products, Hydrology and Earth System Sciences, 15(5): 1577-1600.
        x <- mean_waterfootprint_crops %>% 
          dplyr::filter(iso_code == iso3 & model_group == crop_group & wf_type == wf_group) %>% 
          dplyr::pull(wf_mean)
      if(length(x) != 0) {
        return(x)
      } else {
        return(0)
      }
    }, 
    get_livestock_wf = function(iso3, livestock_group, use_group, wf) {
      #' get_livestock_wf 
      #' 
      #' @description Water footprint (cubic meters per animal product tonne) of farm animals and animal products (1996-2005) from Mekonnen & Hoekstra (2012).
      #' @details
      #' Blue Water Footprint: The amount of surface water and groundwater required (evaporated or used directly) to produce an item.
      #' Green Water Footprint: The amount of rainwater required (evaporated or used directly) to make an item. 
      #' Grey Water Footprint: The amount of freshwater required to dilute the wastewater generated in manufacturing, in order to maintain water quality , as determined by state and local standards. 
      #' 
      #' @param iso3 character Country level ISO Alpha 3 code, e.g., iso3 = "CAN"
      #' @param livestock_group character Livestock group, e.g., livestock_group = c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
      #' @param use_group character Animal product usage group, e.g., use_group = c("dairy", "meat", "other"). NB: eggs are coded as aves_dairy.
      #' @param wf character Water footprint type, e.g., wf = c("green", "blue", "grey")
      #' 
      #' @return numerical water footprint (cubic meters per animal product tonne)
      #' @references Mekonnen, M.M. & Hoekstra, A.Y. (2012) A global assessment of the water footprint of farm animal products, Ecosystems, 15(3): 401–415.
      
      x <- mean_waterfootprint_livestock %>% 
        dplyr::filter(iso_code == iso3 & model_group == paste0(livestock_group, "_", use_group) & wf_type == wf) %>% 
        dplyr::pull(wf_mean)
      if(length(x) != 0) {
        return(x)
      } else {
        return(0)
      }
    }
  )
)

diet_manager <- R6::R6Class(
  "diet_manager", 
  list(
    get_crop_kcal = function(iso3, crop_group, crop_data_df = NULL) { 
      #' get_crop_kcal 
      #' 
      #' @description Metabolic energy (kcal) of crops and derived crop products.
      #' @details Function converts crop groups to average kcal values.
      #' 
      #' @param iso3 character Country-level ISO Alpha 3 code, e.g., iso3 = "CAN"
      #' @param crop_group character Type of crop, e.g., crop_group = c("cereal", "pulse", "rootstubers", "fibercrop", "citrus", "fruit", "vegetable", "oilcrop", "treenut", "sugarcrop")
      #' @param crop_data_df Crop data container passed from crop class.
      #' 
      #' @return numerical Total metabolic energy of crops produced (kcal)
      x <- mean_waterfootprint_crops %>% 
        dplyr::filter(iso_code == iso3 & crop_type == crop_group & wf_type == wf) %>% 
        dplyr::pull(wf_mean)
      if(length(x) != 0) {
        return(x)
      } else {
        return(0)
      }
    }
  )
)

energy_manager <- R6::R6Class(
  "energy_manager", 
  list(
    get_specific_energy = function(fuel_group) { 
      #' get_specific_energy 
      #' 
      #' @description Specific energy (megajoules per tonnne) for each fuel_group.
      #' 
      #' @details Fuel types (e.g., gas, coal) produce energy and greenhouse gases at different rates, depending on how they are used. The Model 
      #' does not yet account for this; it merely makes a direct conversion based on a table in this function. We also do not consider nuclear fuel, 
      #' hydroelectric, geothermal, solar, or wind energy in terms of direct fuel use.
      #' 
      #' @param fuel_group character Type of fuel that produces the energy, e.g., fuel_group = c("biofuel", "biomass", "coal", "ethanol", 
      #' "gas", "geothermal", "hydro", "nuclear", "solar", "wind")
      #' 
      #' @return numerical MJ/t of fuel converted to energy
      #' 
      #' @references Hore-Lacy, I., 2010. Nuclear Energy in the 21st Century: World Nuclear University Press. Elsevier.
      df <- data.frame(
        fuel_type = c("biofuel", "biomass", "coal", "ethanol", "gas", "geothermal", "hydro", "nuclear", "oil", "solar", "wind"), 
        MJ_per_kg = c(38, 16, 24, 26.8, 55, NA, NA, NA, 44, NA, NA), 
        MJ_per_t = c(38e3, 16e3, 24e3, 26.8e3, 55e3, NA, NA, NA, 44e3, NA, NA)
      )
      spec_eng <- df %>% 
        dplyr::filter(fuel_type == fuel_group) %>% 
        dplyr::pull(MJ_per_t)
      
      if(length(spec_eng) != 0) {
        return(spec_eng)
      } else {
        return(0)
      }
    }, 
    get_emission_intensity = function(fuel_group) { 
      #' get_emission_intensity 
      #' 
      #' @description Emission intensity (tonnes CO2e per megajoule) for each fuel_group.
      #' 
      #' @details Counted as net CO2e for the moment, so biofuel has net zero emission intensity, gas has positive, etc.
      #' 
      #' @param fuel_group character Type of fuel that produces the energy, e.g., fuel_group = c("biofuel", "biomass", "coal", "ethanol", 
      #' "gas", "geothermal", "hydro", "nuclear", "solar", "wind")
      #' 
      #' @return numerical t CO2e / MJ of fuel energy
      df <- data.frame(
        fuel_type = c("biofuel", "biomass", "coal", "ethanol", "gas", "geothermal", "hydro", "nuclear", "oil", "solar", "wind"),
        tCO2e_per_MJ = c(0, 0, 9.04e-05, 0, 5.03e-05, 0, 0, 0, 6.93e-05, 0, 0)
      )
      ener_dens <- df %>% 
        dplyr::filter(fuel_type == fuel_group) %>% 
        dplyr::pull(tCO2e_per_MJ)
      
      if(length(ener_dens) != 0) {
        return(ener_dens)
      } else {
        return(0)
      }
    }, 
    get_biofuel_feedstock = function(iso3, year_index, crop_group, crop_data_df) {
      #' get_biofuel_feedstock
      #' 
      #' @description Pulls othe_stock from crop_data passed from crop class
      #' 
      feedstk <- crop_data_df %>% 
        dplyr::filter(iso_alpha3_code == iso3 & year == year_index & model_group == crop_group) %>% 
        dplyr::pull(othe_stock)
      
      if(length(feedstk) != 0) {
        return(feedstk)
      } else {
        return(0)
      }
    }, 
    get_biofuel_from_crops = function(crop_group, fuel_group) { 
      #' get_biofuel_from_crops
      #' 
      #' @description Function to convert tonnes of crop harvested to tonnes of biofuel or ethanol fuel produced.
      #' 
      #' @details Sugar/starchy crops go to bioethanol production; oilcrops go to biodiesel production. Ethanol conversion for cereal (also used for vegetable, fruit, 
      #' citrus) based on maize; biodiesel (here, biofuel) conversion for oilcrop based on soybeans (i.e., 1.5 US gallons of fuel per bushel of soybeans, or 60 lb at 
      #' 13% moisture). Treenut also assumed to be the same as oilcrop.
      #' 
      #' @param crop_group character Crop type, e.g., c("cereal", "pulse", "oilcrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' @param fuel_group character Fuel type, e.g., c("ethanol", "biodiesel")
      #' 
      #' @return t fuel/t crop
      #' 
      #' @references 
      #' cereal ethanol: US Department of Agriculture, 2006. The economic feasibility of ethanol production from sugar in the United States.
      #' pulse ethanol: Upendra, R.S., Pratima, K., Priyanka, S., Jagadish, M.L. and Nandhini, N.J., 2013. Production of bioethanol from hitherto underutilized agro waste 
      #' (Field beans/Green pea pods waste) incorporating zero waste generation technique. International Journal of Innovative Research in Science, Engineering 
      #' and Technology, 2(10), pp.5574-5579.
      df = data.frame(
        crop_type = c("cereal", "pulse", "oilcrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop"), 
        fuel_type = c("ethanol", "ethanol", "biofuel", "ethanol", "ethanol", "ethanol", "ethanol", "biofuel", "ethanol"), 
        L_fuel_per_kg_crop = c(0.320, 0.250, 0.209, 0.250, 0.320, 0.320, 0.320, 0.209, 0.0924), 
        t_fuel_per_t_crop = c(0.252, 0.197, 0.192, 0.197, 0.252, 0.252, 0.252, 0.192, 0.0729)
      )
      if(fuel_group == "ethanol") {
        biof <- df %>% 
          dplyr::filter(crop_type == crop_group & fuel_type == "ethanol") %>% 
          dplyr::pull(t_fuel_per_t_crop)
      } else if(fuel_group == "biofuel") {
        biof <- df %>% 
          dplyr::filter(crop_type == crop_group & fuel_type == "biofuel") %>% 
          dplyr::pull(t_fuel_per_t_crop)
      } else {
        print("Error in get_biofuel_from_crops: check that fuel is biofuel or ethanol.")
      }
      if(length(biof) != 0) {
        return(biof)
      } else {
        return(0) 
      }
    }, 
    extract_fossil_fuels = function(iso3, year_index, fuel_group, policy_df = NULL) {
      #' extract_fossil_fuels
      #' 
      #' @description Energy from fossil fuel sources, including peat
      #' 
      #' @details We are presently using principle energy consumption (PEC) numbers provided by [Our World In Data](https://ourworldindata.org/grapher/primary-energy-consumption-by-region) 
      #' from various sources for fossil fuel production. While this is clearly incorrect, it makes more sense for the time being on a per country basis, because 
      #' we cannot practically test for the entire world, and then redistribute energy by trade. This is something that will need to be worked on later.
      #' 
      #' @param iso3 character ISO Alpha-3 code for country
      #' @param fuel_group character Fuel type must be fossil or peat, e.g., c("coal", "gas", "oil", "peat")
      #' 
      #' @return numerical Energy (megajoules per year)
      #' 
      #' @references Statistical Review of World Energy. https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy.html
      if(fuel_group %in% c("coal", "gas", "oil", "peat")) {
        energy <- pec_MJ %>% 
          dplyr::filter(iso3_code == iso3 & year == year_index) %>% 
          dplyr::pull(fuel_group)
        
        if(length(energy) != 0) {
          return(energy)
        } else {
          return(0)
        }
      } else {
        print("Error in extract_fossil_fuels: fuel group must be among coal, gas, peat, or oil")
      }
    }, 
    produce_renewables = function(iso3, year_index, fuel_group, policy_df = NULL) {
      #' produce_renewables
      #' 
      #' @description Energy from renewable sources, excluding nuclear
      #' 
      #' @details We are presently using principle energy consumption (PEC) numbers provided by [Our World In Data](https://ourworldindata.org/grapher/primary-energy-consumption-by-region) 
      #' from various sources for fossil fuel production. While this is clearly incorrect, it makes more sense for the time being on a per country basis, because 
      #' we cannot practically test for the entire world, and then redistribute energy by trade. This is something that will need to be worked on later.
      #' 
      #' TODO Integrate code developed for estimating national level solar and wind energy production here. These data are independent of the OWID PEC numbers and based 
      #' on total available energy by source.
      #' 
      #' @param iso3 character ISO Alpha-3 code for country
      #' @param fuel_group character Fuel type must renewable, e.g., c("geothermal", "hydro", "solar", "wind")
      #' 
      #' @return numerical Energy (megajoules per year)
      #' 
      #' @references Statistical Review of World Energy. https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy.html
      if(fuel_group %in% c("geothermal", "hydro", "solar", "wind")) {
        energy <- pec_MJ %>% 
          dplyr::filter(iso3_code == iso3 & year == year_index) %>% 
          dplyr::pull(fuel_group)
        
        if(length(energy) != 0) {
          return(energy)
        } else {
          return(0)
        }
      } else {
        print("Error in produce_renewables: fuel group must be among geothermal, hydro, solar, or wind")
      }
    }, 
    produce_nuclear = function(iso3, year_index, fuel_group, policy_df = NULL) {
      #' produce_nuclear
      #' 
      #' @description Energy from nuclear sources
      #' 
      #' @details We are presently using principle energy consumption (PEC) numbers provided by [Our World In Data](https://ourworldindata.org/grapher/primary-energy-consumption-by-region) 
      #' from various sources for fossil fuel production. While this is clearly incorrect, it makes more sense for the time being on a per country basis, because 
      #' we cannot practically test for the entire world, and then redistribute energy by trade. This is something that will need to be worked on later.
      #' 
      #' TODO Integrate code developed for estimating national level solar and wind energy production here. These data are independent of the OWID PEC numbers and based 
      #' on total available energy by source.
      #' 
      #' @param iso3 character ISO Alpha-3 code for country
      #' @param fuel_group character Fuel type must be nuclear
      #' 
      #' @return numerical Energy (megajoules per year)
      #' 
      #' @references Statistical Review of World Energy. https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy.html
      if(fuel_group == "nuclear") {
        energy <- pec_MJ %>% 
          dplyr::filter(iso3_code == iso3 & year == year_index) %>% 
          dplyr::pull(fuel_group)
        
        if(length(energy) != 0) {
          return(energy)
        } else {
          return(0)
        }
      } else {
        print("Error in produce_renewables: fuel group must nuclear")
      }
    }
  )
)

forest_manager <- R6::R6Class(
  "forest_manager", 
  list(
    forest_harvest_area = function(iso3, year, land_use_data_df = NULL) {
      #' forest_harvest_area
      #' 
      #' @description Just passes all forest area (hectares) from land_use class. Eventually this should pass forest area 
      #' designated for harvesting wood products.
      #' 
      #' @return Forest area (ha)
      lua <- land_use_data_df %>% 
        dplyr::filter(iso_alpha3_code == iso3 & year == year_index & land_use_type == "forest") %>% 
        dplyr::pull(land_use_area)
      
      if(length(lua) != 0) {
        return(lua)
      } else {
        return(0)
      }
    }, 
    new_forest = function(iso3, year, land_use_data_df = NULL, forest_data_df = NULL, policy_df = NULL) {
      #' new_forest
      #' 
      #' @description Determines area of new forest planted annually (hectares)
      #' 
      #' @param iso3 character
      #' @param year numerical
      #' @param land_use_data_df Data frame of historical land use passed from land_use class
      #' @param forest_data_df Data frame of historical forest passed from forest class
      #' @param policy_df Data frame of forest management policy
      #' 
      #' @return Afforested area (ha) 
      
    }, 
    harvest_wood = function() {
      #' harvest_wood
      #' 
      #' @description Determines the density of wood to be harvested from forest land
      #' 
      #' @details This will need to be filled in with specific values for forest types/countries/regions/etc. I have used a value of 86 US tons per acre 
      #' from Thomas (2018).
      #' 
      #' @return harvest wood area density (t/ha)
      #' 
      #' @reference Thomas, N. 2018. How Many Tons of Wood are on an Acre of Land? Forest2Market.com. 
      #' https://www.forest2market.com/blog/how-many-tons-of-wood-are-on-an-acre-of-land (Accessed 21-04-2021)
      return(193)
    }, 
    get_charcoal = function(harvested_wood, production_method = NULL) {
      #' get_charcoal
      #' 
      #' @description Determines the density of charcoal from harvested wood.
      #' 
      #' @details Charcoal is produced by heating dry wood to a high temperature in low-oxygen/anoxic conditions, resulting in the elimination of excess water 
      #' and volatile compounds from the product. The production methods used here are averages of those specified in Stassen (2002) for each method. If the user 
      #' does not add a production method argument, the improved_traditional method is assumed.
      #' 
      #' NB: Presently all harvested wood is processed for charcoal. This is highly unrealistic as charcoal is a comparatively low-value wood product.
      #' 
      #' TODO Factor by some fraction of harvested_wood actually used for charcoal production
      #' 
      #' @param numerical harvested_wood Tonnes of wood to be converted to charcoal.
      #' @param character production_method Method used to produce charcoal, e.g., c("traditional", "improved_traditional", "industrial", "high_yield")
      #' 
      #' @return Charcoal mass (tonnes)
      #' 
      #' @references Stassen, H. E. 2002. "Developments in charcoal production technology". In Unasilva: An international journal of forestry and forest industries, Vol. 53 2002/4
      #' 
      df <- data.frame(
        method = c("traditional", "improved_traditional", "industrial", "high_yield"), 
        ratio = c(0.1, 0.143, 0.167, 0.286)
      )
      if(is.null(production_method)) {
        production_method = "improved_traditional"
      }
      ratio <- df %>% 
        dplyr::filter(method == production_method) %>% 
        dplyr::pull(ratio)
      
      char <- ratio*harvested_wood
      if(length(char) != 0) {
        return(char)
      } else {
        "Error in get_charcoal: check that you have entered an appropriate method and numerical wood harvest value."
      }
    }
  ))
