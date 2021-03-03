# manages state classes
library(roxygen2)
library(docstring)
# Using docstring with R6 classes
# e.g., you have to assign objects:
# watr <- water_footprint_manager$new()
# but docstring(watr$get_livestock_wf) will not work
# get_livestock_wf <- watr$get_livestock_wf
# docstring(get_livestock_wf)


land_use_manager <- R6::R6Class(
  "land_use_manager", 
  list(
    land_use_area  = function(x1, x2, x3, df = NULL) { 
      #' land_use_area 
      #' 
      #' Calculates area (hectares) under management of specified land use
      #' 
      #' @param x1 numerical FAO country code
      #' @param x2 numerical year
      #' @param x3 character string land use type c("cropland", "permanent_cropland", "arable_land", "pasture", "forest", "otherland")
      if(x2 == 2000) {
        x <- historical_land_use_data %>% 
          dplyr::filter(fao_countrycode == x1 & year == 2000 & land_use_type == x3) %>% 
          dplyr::pull(land_use_area)
        
        if(length(x) != 0) {
          return(x)
        } else {
          return(0)
        }
      } else {
        # df <- land_use_data
        x  <- df %>% 
          dplyr::filter(fao_countrycode == x1 & year == x2-1 & land_use_type == x3) %>% 
          dplyr::pull(land_use_area) 
        
        # dx <- land_use_area_change_df
        dx <- 0 #temporarily set to zero LUC
        
        if(length(x) != 0) {
          return(x + x*dx)
        } else {
          return(0)
        }
      }
    }, 
    dry_matter_productivity = function(x1) {
      #' dry_matter_productivity 
      #' 
      #' Calculates the dry matter production density (tonnes per hectare) under management of specified land use type. Presently for pasture only.
      #' 
      #' @param x1 character string land use type c("cropland", "permanent_cropland", "arable_land", "pasture", "forest", "otherland")
      x <- pasture_DM_productivity %>% 
        dplyr::filter(land_use_type == x1) %>% 
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
    manage_harvest_area  = function(x1, x2, x3, df1 = NULL, df2 = NULL) {
      #' manage_harvest_area 
      #' 
      #' Calculates the area (hectares) harvested under management of specified land use type. Cropland only.
      #' 
      #' @param x1 numerical FAO country code
      #' @param x2 numerical year
      #' @param x3 character string crop type
      #' @param df1 data frame land use area (hectares) by country, year, land-use type
      #' @param df2 data frame ratio of cropland allotted to selected crop type
      #' @return hectares harvested for given parameters
      if(x2 == 2000) {
        # x <- historical_crop_data %>% 
        #   dplyr::filter(fao_countrycode == x1 & year == 2000 & model_group == x3) %>% 
        #   dplyr::pull(harvest_area)
        
        x   <- df1 %>% 
          dplyr::filter(fao_countrycode == x1 & year == 2000 & land_use_type == "cropland") %>% 
          dplyr::pull(land_use_area)
        
        y   <- df2 
        
        if(length(x) != 0 & length(y) != 0) {
          return(x*y)
        } else {
          return(0)
        }
      } else {
        x   <- df1 %>% 
          dplyr::filter(fao_countrycode == x1 & year == x2 & land_use_type == "cropland") %>% 
          dplyr::pull(land_use_area)
        
        y   <- df2 
        
        if(length(x) != 0 & length(y) != 0) {
          return(x*y)
        } else {
          return(0)
        }
      }
    }, 
    manage_harvest_yield = function(x1, x2, x3, df = NULL) { 
      #' manage_harvest_yield 
      #' 
      #' Calculates the harvest yield (tonnes per hectare) under management of specified land use type. Cropland only.
      #' 
      #' @param x1 numerical FAO country code
      #' @param x2 numerical year
      #' @param x3 character string crop type
      #' @param df data frame crop management yield values
      #' @return hectares harvested for given parameters
      if(x2 == 2000) {
        x <- historical_crop_data %>% 
          dplyr::filter(fao_countrycode == x1 & year == 2000 & model_group == x3) %>% 
          dplyr::pull(harvest_yield)
        
        if(length(x) != 0) {
          return(x)
        } else {
          return(0)
        }
        
      } else {
        x  <- df %>% 
          dplyr::filter(fao_countrycode == x1 & year == x2-1 & model_group == x3) %>% 
          dplyr::pull(harvest_yield)
        
        dx <- df %>% #crop_management_df
          dplyr::filter(fao_countrycode == x1 & model_group == x3) %>% 
          dplyr::pull(delta_harvest_yield)
        
        if(length(x) != 0) {
          return(x + x*dx)
        } else {
          return(0)
        }
      }
    }, 
    manage_production = function(x1, x2) {
      #' manage_production 
      #' 
      #' Calculates crop production (tonnes) as the product of harvest yield and harvest area
      #' 
      #' @param x1 numerical harvest area (output of manage_harvest_area)
      #' @param x2 numerical harvest yield (output of manage_harvest_yield)
      #' @return tonnes produced 
      if(length(x1) != 0 & length(x2) != 0) {
        return(x1*x2)
      } else {
        return(0)
      }
    }, 
    manage_cropland_allotment = function(x1, x2, x3) {
      #' manage_cropland_allotment 
      #' 
      #' Not all cropland capable of growing crops is put into production. This function manages the proportion of cropland 
      #' used for crop production.
      #' 
      #' @param x1 numerical country code used by FAO
      #' @param x2 numerical year
      #' @param x3 character string crop type
      #' @return proportion between 0 and 1 
      x <- cropland_allotment_by_type %>% 
        dplyr::filter(fao_countrycode == x1 & year == x2 & model_group == x3) %>% 
        dplyr::pull(ratio_of_land_alloted)
      if(length(x) != 0) {
        return(x)
      } else {
        return(0) 
      }
    }, 
    manage_crop_allocation = function(x1, x2, x3, x4) {
      #' manage_crop_allocation 
      #' 
      #' Crops are allocated to food, feed, seed, processing and other uses; and losses are treated as an allocation.
      #' 
      #' @param x1 numerical country code used by FAO
      #' @param x2 numerical year
      #' @param x3 character string crop type
      #' @param x4 character string, e.g., c("food", "feed", "seed", "loss", "proc", "othe")
      #' @return proportion between 0 and 1 
      x <- domestic_production_relative %>% 
        dplyr::filter(Year == 2000) %>% 
        dplyr::select(Area.Code, Area, Year, model_group, contains("ratio"))
      
      if(x2 == 2000) {
        x <- x %>% 
          dplyr::filter(Area.Code == x1 & model_group == x3) %>% 
          dplyr::pull(paste0(x4,"_ratio"))
        
        if(length(x) != 0) {
          return(x)
        } else {
          return(0)
        }
        
      } else {
        
        #this is where feedback logic will go -- for now it is the same as for year 2000
        
        x  <- x %>% 
          dplyr::filter(Area.Code == x1 & model_group == x3) %>% 
          dplyr::pull(paste0(x4,"_ratio"))
        
        dx <- product_use_ratio_change_df %>% 
          dplyr::filter(year == x2 & model_group == x3) %>% 
          dplyr::pull(paste0("delta_", x4, "_ratio"))
        
        x  <- x + x*dx
        
        if(length(x) != 0) {
          return(x)
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
    # x4 is "food_imports"/"feed_imports"/etc.
    # df is data from the end of the time-step
    imports = function(x1, x2, x3, x4, df = NULL) {
      #' imports 
      #' 
      #' Manages imports allocated to food and feed uses. NB: Eventually this will be extended to seed, processing, losses, and other uses.
      #' 
      #' @param x1 numerical country code used by FAO
      #' @param x2 numerical year
      #' @param x3 character string crop type
      #' @param x4 character string, e.g., c("food", "feed")
      #' @return numerical tonnes 
      if(x2 == 2000) {
        #fill in some historic data here
        return(0)
      } else {
        x <- df %>% 
          dplyr::filter(fao_countrycode == x1 & year == x2 & model_group == x3) %>% 
          dplyr::pull(paste0(x4, "_imports"))
        
        if(length(x) != 0) {
          return(x)
        } else {
          return(0)
        }
      }
    }, 
    exports = function(x1, x2, x3, x4, df = NULL) { 
      #' exports 
      #' 
      #' Manages exports allocated to food and feed uses. NB: Eventually this will be extended to seed, processing, losses, and other uses.
      #' 
      #' @param x1 numerical country code used by FAO
      #' @param x2 numerical year
      #' @param x3 character string crop type
      #' @param x4 character string, e.g., c("food", "feed")
      #' @return numerical tonnes 
      if(x2 == 2000) {
        #fill in some historic data here
        return(0)
      } else {
        x <- df %>% 
          dplyr::filter(fao_countrycode == x1 & year == x2 & model_group == x3) %>% 
          dplyr::pull(paste0(x4, "_exports"))
        
        if(length(x) != 0) {
          return(x)
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
    quantity = function(x1, x2, x3, df = NULL) { 
      #' quantity 
      #' 
      #' Manages quantity of livestock. Livestock are counted by head (individuals), except for fish, which are counted by tonne.
      #' 
      #' @param x1 numerical country code used by FAO
      #' @param x2 numerical year
      #' @param x3 character string livestock type, e.g., c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
      #' @param df tibble with growth rates for animal stocks, set by growth_rate function
      #' @return numerical tonnes 
      if(x2 == 2000) {
        x <- livestock_filtered %>% 
          dplyr::filter(Area.Code == x1 & Year.Code == x2 & livestock_group == x3) %>% 
          dplyr::pull(stock) %>% 
          sum(., 
              na.rm = TRUE)
        
        return(x)
      } else {
        x  <- df %>% 
          dplyr::filter(fao_countrycode  == x1 & year == x2-1 & model_group == x3) %>% 
          dplyr::pull("total_stock_quantity")
        
        dx <- df %>% 
          dplyr::filter(fao_countrycode  == x1 & year == x2-1 & model_group == x3) %>% 
          dplyr::pull("stock_growth_rate")
        
        if(length(x) != 0 & length(dx) != 0) {
          return(x + x*dx)
        } else {
          return(0)
        }
      }
    }, 
    growth_rate = function(x1, x2, x3, df = NULL) { 
      #' growth_rate 
      #' 
      #' Livestock quantity changes as a combined result of management practices (slaughter, breeding, imports/exports). This is represented by a crude annual growth rate.
      #' 
      #' @param x1 numerical country code used by FAO
      #' @param x2 numerical year
      #' @param x3 character string livestock type, e.g., c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
      #' @param df data frame with previous time step growth rate
      #' @return numerical crude growth rate 
      if(x2 == 2000) {
        return(0)
      } else {
        x <- df %>% 
          dplyr::filter(fao_countrycode == x1 & year == x2-1 & model_group == x3) %>% 
          dplyr::pull(stock_growth_rate)
        
        y <- livestock_growth_policy_df %>% 
          dplyr::filter(livestock_group == x3) %>% 
          dplyr::pull(growth)
        
        if(length(x) != 0 & length(y) != 0) {
          return(x + y)
        } else {
          return(0)
        }
      }
    }, 
    product_usage = function(x1, x2) { 
      #' product_usage 
      #' 
      #' Livestock are raised for different purposes (e.g., dairy and beef cattle). The model does not presently allow for mixed use of livestock. 
      #' It depends on primary uses of animals by type, and presently does not depend on geography or time.
      #' 
      #' TODO add country and time dependence
      #' 
      #' @param x1 character string livestock group, e.g., c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
      #' @param x2 character string, e.g., c("broiler", "layer", "honey", "dairy", "meat", "other")
      #' @return numerical proportion between 0 and 1
      x <- stock_usage_proportion_df %>% 
        dplyr::filter(livestock_group == x1, stock_usage == x2) %>% 
        dplyr::pull(proportion)
      if(length(x) != 0) {
        return(x)
      } else {
        return(0)
      }
    }, 
    feed_demand = function(x1, x2) {
      #' feed_demand 
      #' 
      #' Manages the feed dry matter (DM) and crude protein (CP) demands of livestock based on feed conversion rate (FCR)
      #' 
      #' TODO make this less janky
      #' 
      #' @param x1 character string livestock group, e.g., c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
      #' @param x2 character string, e.g., c("FCR", "CP")
      #' @return numerical (for FCR, tonnes of DM per livestock tonne or, for CP, proportion between 0 and 1)
      x <- livestock_feed_summary_ave %>% 
        dplyr::filter(model_group == x1)
      if(x2 == "CP") {
        return(x %>% 
                 dplyr::pull(CP_ratio_of_DM))
      } else if(x2 == "FCR") {
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
    herd_tlu = function(x1) { 
      #' herd_tlu 
      #' 
      #' Average tropical livestock units (TLU) per herd animal
      #' 
      #' @param x1 character string livestock group, e.g., c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
      #' @return numerical TLU per herd animal
      #' @references Jahnke, H.E., Tacher, G., Kiel, P. & Rojat, D. 1988. Livestock production in tropical Africa, with special reference to the tsetse-affected zone. Livestock production in tsetse-affected areas of Africa. Proceedings of a meeting held in Nairobi, 23-27 November 1987. Nairobi, ILCA/International Laboratory for Research on Animal Diseases (ILRAD). pp 3-21. 
      #' FAOSTAT. 2000. Rome: Food and Agriculture Organization. 
      #' Kassam, A. H. 1991. Agro-ecological Land Resources Assessment for Agricultural Development Planning, A Case Study of Kenya : Resources Data Base and Land Productivity. Rome and Laxenburg: Food and Agriculture Organization and International Institute for Applied Systems Analysis.
      x <- data.frame(
        model_group    = c("aves", "bovine", "caprine", "camelid", "equine", "rodentia", "sus"),
        TLU_per_animal = c(  1e-5,        1,      0.09,      1.13,     0.75,       2e-5,   0.2),
        citation       = c("Jahnke et al 1988. Numbers from FAOSTAT (2000)", "Jahnke et al 1988. Numbers from FAOSTAT (2000)", "Kassam et al 1991, FAO & IIASA", "Kassam et al 1991, FAO & IIASA", "Kassam et al 1991, FAO & IIASA", "Kassam et al 1991, FAO & IIASA", "Jahnke et al 1988. Numbers from FAOSTAT (2000)")
      ) %>% 
        dplyr::filter(model_group == x1) %>% 
        dplyr::pull(TLU_per_animal)
      
      if(length(x) != 0) {
        return(x)
      } else {
        return(0)
      }
    }
  )
)

water_footprint_manager <- R6::R6Class(
  "water_footprint_manager", 
  list(
    #' Uses data from Mikkonen and Hoekstra, 2005, for water footprint. Presently just for crops and livestock.
    #' 
    #' Blue Water Footprint: The amount of surface water and groundwater required (evaporated or used directly) to produce an item.
    #' Green Water Footprint: The amount of rainwater required (evaporated or used directly) to make an item. 
    #' Grey Water Footprint: The amount of freshwater required to dilute the wastewater generated in manufacturing, in order to maintain water quality , as determined by state and local standards.
    #' \code{get_crop_wf} uses data from Mikkonnen and Hoekstra wrangled and cleaned to compute water footprint 
    #' of crop products.
    #' @param x1 character string of country level ISO Alpha 3 code
    #' @param x2 model_group for crops model_group = c("cereal", "pulse", "rootstubers", "fibercrop", "citrus", "fruit", "vegetable", "oilcrop", "treenut", "sugarcrop") or livestock model_group = c("aves_dairy", "aves_meat", 
    #' "aves_other", "bovine_dairy", "bovine_meat", "bovine_other", "camelid_dairy", "camelid_meat", "camelid_other", "caprine_dairy" , "caprine_meat", "caprine_other", "equine_meat", "equine_other", 
    #' "rodentia_dairy", "rodentia_meat", "rodentia_other", "sus_meat", "sus_other") 
    #' @param x3 character string of water footprint type wf_type = c("green", "blue", "grey")
    #' @return a double for water footprint in m^3 per tonne of product
    #' @export
    get_crop_wf = function(x1, x2, x3) { 
      #' get_crop_wf 
      #' 
      #' Water footprint (cubic meters per crop tonne) of crops and derived crop products (1996-2005).
      #' Blue Water Footprint: The amount of surface water and groundwater required (evaporated or used directly) to produce an item.
      #' Green Water Footprint: The amount of rainwater required (evaporated or used directly) to make an item. 
      #' Grey Water Footprint: The amount of freshwater required to dilute the wastewater generated in manufacturing, in order to maintain water quality , as determined by state and local standards.
      #' 
      #' @param x1 character string of country level ISO Alpha 3 code
      #' @param x2 character string, e.g., c("cereal", "pulse", "rootstubers", "fibercrop", "citrus", "fruit", "vegetable", "oilcrop", "treenut", "sugarcrop")
      #' @param x3 character string of water footprint type, e.g., c("green", "blue", "grey")
      #' 
      #' @param x1 character string livestock group, e.g., c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
      #' @return numerical (cubic meters per crop tonne)
      #' @references Mekonnen, M.M. & Hoekstra, A.Y. (2011) The green, blue and grey water footprint of crops and derived crop products, Hydrology and Earth System Sciences, 15(5): 1577-1600.
        x <- mean_waterfootprint_crops %>% 
          dplyr::filter(iso_code == x1 & model_group == x2 & wf_type == x3) %>% 
          dplyr::pull(wf_mean)
      if(length(x) != 0) {
        return(x)
      } else {
        return(0)
      }
    }, 
    #' \code{get_crop_wf} uses data from Mikkonnen and Hoekstra wrangled and cleaned to compute water footprint 
    #' of crop products.
    #' @param x1 character string of country level ISO Alpha 3 code
    #' @param x2 model_group for livestock model_group = c("aves", "bovine", "camelid", "caprine", "equine", "rodentia", "sus") 
    #' @param x3 character string from c("dairy", "meat", "other")
    #' @param x4 character string of water footprint type wf_type = c("green", "blue", "grey")
    #' @return a double for water footprint in m^3 per tonne of product
    #' TODO add fish (if wf for fish makes sense)
    #' @export
    get_livestock_wf = function(x1, x2, x3, x4) {
      #' get_livestock_wf 
      #' 
      #' Water footprint (cubic meters per animal product tonne) of farm animals and animal products (1996-2005).
      #' Blue Water Footprint: The amount of surface water and groundwater required (evaporated or used directly) to produce an item.
      #' Green Water Footprint: The amount of rainwater required (evaporated or used directly) to make an item. 
      #' Grey Water Footprint: The amount of freshwater required to dilute the wastewater generated in manufacturing, in order to maintain water quality , as determined by state and local standards.
      #' 
      #' @param x1 character string of country level ISO Alpha 3 code
      #' @param x2 character string of livestock group, e.g., c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish")
      #' @param x3 character string, e.g., c("dairy", "meat", "other"). NB: eggs are coded as aves_dairy.
      #' @param x4 character string of water footprint type, e.g., c("green", "blue", "grey")
      #' 
      #' @return numerical (cubic meters per animal product tonne)
      #' @references Mekonnen, M.M. & Hoekstra, A.Y. (2012) A global assessment of the water footprint of farm animal products, Ecosystems, 15(3): 401–415.
      
      x <- mean_waterfootprint_livestock %>% 
        dplyr::filter(iso_code == x1 & model_group == paste(x2, x3, sep = "_") & wf_type == x4) %>% 
        dplyr::pull(wf_mean)
      if(length(x) != 0) {
        return(x)
      } else {
        return(0)
      }
    }
  )
)
