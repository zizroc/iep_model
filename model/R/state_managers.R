# manages state classes
library(roxygen2)
library(docstring)
# Using docstring with R6 classes
# e.g., you have to assign objects:
# watr <- water_footprint_manager$new()
# but docstring(watr$get_livestock_wf) will not work
# get_livestock_wf <- watr$get_livestock_wf
# docstring(get_livestock_wf)

crop_policy_df <- data.frame (
  iso_alpha3_code        = "CHN", 
  year                   = 2001, 
  crop_type              = c("cereal", "pulse", "oilcrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop"), 
  ratio_of_land_allotted = c(    0.35,     0.1,      0.12,          0.06,        0.12,     0.1,      0.1,     0.025,       0.025), 
  delta_harvest_yield    = c(), 
  delta_allocation       = c()
)

livestock_policy_df <- data.frame (
  iso_alpha3_code        = "CHN", 
  year                   = 2001, 
  live_type              = c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish"), 
  crude_growth_rate      = c()
)

land_use_manager <- R6::R6Class(
  "land_use_manager", 
  list(
    land_use_area  = function(iso3, year_index, use_group, land_use_policy_df = NULL) { 
      #' land_use_area 
      #' 
      #' @description Calculates area (hectares) under management of specified land use
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical year
      #' @param use_group character string land use type c("cropland", "permanent_cropland", "arable_land", "pasture", "forest", "otherland")
      #' @param land_use_policy_df Data frame containing land use change values (must be exogenously set)
      if(year_index == 2000) {
        luc <- historical_land_use_data %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == 2000 & land_use_type == use_group) %>% 
          dplyr::pull(land_use_area)
        
        if(length(luc) != 0) {
          return(luc)
        } else {
          return(0)
        }
      } else {
        # df <- land_use_data
        luc  <- land_use_policy_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index-1 & land_use_type == use_group) %>% 
          dplyr::pull(land_use_area) 
        
        # dx <- land_use_area_change_df
        delta_luc <- 0 #temporarily set to zero LUC
        
        if(length(x) != 0) {
          return(x + x*dx)
        } else {
          return(0)
        }
      }
    }, 
    dry_matter_productivity = function(use_group) {
      #' dry_matter_productivity 
      #' 
      #' @description Calculates the dry matter production density (tonnes per hectare) under management of specified land use type. Presently for pasture only.
      #' 
      #' @param use_group character land use type c("cropland", "permanent_cropland", "arable_land", "pasture", "forest", "otherland")
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
    harvest_area  = function(iso3, year_index, crop_group, crop_policy_df = NULL, land_use_area_df = NULL) {
      #' harvest_area 
      #' 
      #' @description Calculates the area (hectares) harvested under management of specified land use type. Cropland only.
      #' 
      #' @details Harvest area initial values are set based on historic FAO data. For years > 2000, cropland area harvested and cropland allotted to each 
      #' crop type are passed from the land-use module. NB: This function requires that the base_data.R module is run first, to fill the crops_area_harvested 
      #' data frame for year 2000.
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical Year
      #' @param crop_group character Crop type, e.g., c("cereal", "pulse", "oilcrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' @param land_use_area_df numeric Data frame passed from land_use module. Value is cropland area harvested, minus fallow land. Used for year > 2000.
      #' @param crop_policy_df Data frame containing crop yield change values (must be exogenously set)
      #' @return hectares harvested for given parameters
      area_used <- land_use_area_df %>% 
        dplyr::filter(iso_alpha3_code == iso3 & year == year_index & land_use_type == "cropland") %>% 
        dplyr::pull(land_use_area)
      
      ratio_allotted <- crop_policy_df %>% 
        dplyr::filter(iso_alpha3_code == iso3 & year == year_index & crop_type == crop_group) %>% 
        dplyr::pull(ratio_of_land_allotted)
      
      if(length(area_used) != 0 & length(ratio_allotted) != 0) {
        return(area_used*ratio_allotted)
      } else {
        return(0)
      }
    }, 
    harvest_yield = function(iso3, year_index, crop_group, crop_policy_df = NULL, crop_data_df = NULL) { 
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
      #' @param crop_policy_df Data frame containing crop yield change values (must be extrinsically set) 
      #' @param crop_data_df Data frame containing historical data passed from crop class
      #' @return numeric Harvest yield (tonnes/ha) 
      if(year_index == 2000) {
        yield <- crops_yield %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == 2000 & crop_type == crop_group) %>% 
          dplyr::pull(value)
        
        if(length(yield) != 0) {
          return(yield)
        } else {
          return(0)
        }
        
      } else {
        yield       <- crop_data_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index-1 & crop_type == crop_group) %>% 
          dplyr::pull(harvest_yield)
        
        delta_yield <- crop_policy_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index & crop_type == crop_group) %>% 
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
      #' confirm that production was computed properly.
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
    #' cropland_allotment = function(iso3, year_index, crop_group, crop_policy_df = NULL) {
    #'   #' cropland_allotment 
    #'   #' 
    #'   #' @description Manages the proportion of cropland used for crop production.
    #'   #' 
    #'   #' @details Not all cropland is put into production. Once fallow land area is subtracted from available cropland, crop production values 
    #'   #' are determined by their proportion to the whole, by a ratio_of_land_allotted value, passed from the crop_policy_df.
    #'   #' 
    #'   #' @param iso3 character ISO Alpha-3 code
    #'   #' @param year_index numerical year
    #'   #' @param crop_group character string crop type 
    #'   #' @param crop_policy_df Data frame containing values for the ratio of available cropland allotted for the production of each crop type.
    #'   #' @return proportion between 0 and 1 
    #'   cropland_allotted <- crop_policy_df %>% 
    #'     dplyr::filter(iso_alpha3_code == iso3 & year == year_index & model_group == crop_group) %>% 
    #'     dplyr::pull(ratio_of_land_allotted)
    #'   if(length(cropland_allotted) != 0) {
    #'     return(cropland_allotted)
    #'   } else {
    #'     return(0) 
    #'   }
    #' }, 
    crop_allocation = function(fao_countrycode, year_index, crop_group, crop_use, crop_data_df = NULL, crop_policy_df = NULL) {
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
      #' TODO Swap out fao_countrycode argument for an iso3 argument
      #' TODO Add logic to deal with export/import of domestic over/under-production
      #' 
      #' @param iso3 character ISO Alpha-3 code
      #' @param year_index numerical Year
      #' @param crop_group character Crop type, e.g., c("cereal", "pulse", "oilcrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' @param crop_use character Main allocation for crop, e.g., c("food", "feed", "seed", "loss", "proc", "othe")
      #' @param crop_data_df Data frame containing historical data passed from crop class
      #' @param crop_policy_df Data frame containing any changes in crop allocation based on policy.
      #' @return proportion between 0 and 1 
      #' 
      if(year_index == 2000) {
        rel_dom_prod <- domestic_production_relative %>% 
          dplyr::filter(Year == 2000) %>% 
          dplyr::select(Area.Code, Area, Year, model_group, contains("ratio")) %>% 
          dplyr::filter(Area.Code == fao_countrycode & model_group == crop_group) %>% 
          dplyr::pull(paste0(crop_use,"_ratio"))
        
        if(length(rel_dom_prod) != 0) {
          return(rel_dom_prod)
        } else {
          return(0)
        }
      } else {
        
        #this is where feedback logic will go
        
        rel_dom_prod      <- crop_data_df %>% 
          dplyr::filter(fao_countrycode == fao_countrycode & year == year_index-1 & model_group == crop_group) %>% 
          dplyr::select(paste0(crop_use, "_stock"), production) %>% 
          dplyr::mutate(allocated_ratio = .[1]/production) %>% 
          dplyr::pull(allocated_ratio) %>% 
          as.numeric()
        
        delta_rel_dom_prod <- crop_policy_df %>% 
          dplyr::filter(iso_alpha3_code == iso3 & year == year_index & model_group == crop_group) %>% 
          dplyr::pull(paste0("delta_", crop_allocation, "_ratio")) %>% 
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
    #manage_stock_quantity
    #x1 = fao_countrycode
    #x2 = year
    #x3 = model_group for livestock
    #df = livestock$get_livestock_quantity()
    quantity = function(iso3, year_index, live_group, livestock_data_df = NULL, livestock_policy_df = NULL) { 
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
      #' @param livestock_policy_df Data frame with growth rates for animal stocks, set by growth_rate function
      #' @return numerical Individual head of livestock
      if(year_index == 2000) {
        quant <- livestock_filtered %>% 
          dplyr::filter(iso_alpha3_code  == iso3 & year == 2000 & livestock_group == live_group) %>% 
          dplyr::pull(stock) %>% 
          sum(., 
              na.rm = TRUE)
        
        return(quant)
      } else {
        quant <- livestock_data_df %>% 
          dplyr::filter(iso_alpha3_code  == iso3 & year == year_index-1 & model_group == live_group) %>% 
          dplyr::pull("total_stock_quantity")
        
        cgr   <- livestock_policy_df %>% 
          dplyr::filter(iso_alpha3_code  == iso3 & year == year_index & live_type == live_group) %>% 
          dplyr::pull("stock_growth_rate")
        
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
    #'       dplyr::pull(stock_growth_rate)
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
      #' TODO make this less janky
      #' 
      #' @param live_group character Livestock group, e.g., livestock_group = c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
      #' @param feed_matter character Feed conversion ratio (FCR) or crude protein (CP) e.g., feed_type = c("FCR", "CP")
      #' @return numerical (for FCR, tonnes of DM per livestock tonne or, for CP, proportion between 0 and 1)
      x <- livestock_feed_summary_ave %>% 
        dplyr::filter(model_group == live_group)
      if(feed_matter == "CP") {
        return(x %>% 
                 dplyr::pull(CP_ratio_of_DM))
      } else if(feed_matter == "FCR") {
        x <- x %>% 
          dplyr::pull(FCR)
        if(length(x) != 0) {
          return(x)
        } else {
          return(0)
        }
      } else {
        return(0)
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
      tlu <- tlu_def_df %>% 
        dplyr::filter(iso_alpha3_code == iso3 & year == year_index & live_type == live_group) %>% 
        dplyr::pull(herd_tlu)
      
      if(length(tlu) == 0) {
        tlu <- data.frame(
          live_type = c("aves", "bovine", "caprine", "camelid", "equine", "rodentia", "sus"),
          herd_tlu  = c(  1e-5,        1,      0.09,      1.13,     0.75,       2e-5,   0.2)
        ) %>% 
          dplyr::filter(live_type == live_group) %>% 
          dplyr::pull(herd_tlu)
      } 
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
    get_crop_wf = function(iso3, crop_group, wf) { 
      #' get_crop_wf 
      #' 
      #' @description Water footprint (cubic meters per crop tonne) of crops and derived crop products (1996-2005) from Mekonnen and Hoekstra (2011).
      #' @details
      #' Blue Water Footprint: The amount of surface water and groundwater required (evaporated or used directly) to produce an item.
      #' Green Water Footprint: The amount of rainwater required (evaporated or used directly) to make an item. 
      #' Grey Water Footprint: The amount of freshwater required to dilute the wastewater generated in manufacturing, in order to maintain water quality , as determined by state and local standards. 
      #' @param iso3 character Country-level ISO Alpha 3 code, e.g., iso3 = "CAN"
      #' @param crop_group character Type of crop, e.g., crop_group = c("cereal", "pulse", "rootstubers", "fibercrop", "citrus", "fruit", "vegetable", "oilcrop", "treenut", "sugarcrop")
      #' @param wf character Water footprint type, e.g., wf = c("green", "blue", "grey")
      #' 
      #' @return numerical water footrpint (cubic meters per crop tonne)
      #' @references Mekonnen, M.M. & Hoekstra, A.Y. (2011) The green, blue and grey water footprint of crops and derived crop products, Hydrology and Earth System Sciences, 15(5): 1577-1600.
        x <- mean_waterfootprint_crops %>% 
          dplyr::filter(iso_code == iso3 & crop_type == crop_group & wf_type == wf) %>% 
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
