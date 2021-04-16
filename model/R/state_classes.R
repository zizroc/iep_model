#instantiates at country level
library(roxygen2)
library(R6)
library(tidyverse)

#data
source(paste0(script_path, "map_data.R"))

country <- R6::R6Class(
  "country", 
  list(
  iso_alpha3      = NULL, 
  countryname     = NULL, 
  fao_countrycode = NULL, 
  un_countrycode  = NULL, 
  subregioncode   = NULL, 
  subregionname   = NULL, 
  year            = NULL, 
  country_data    = NULL, 
  initialize = function(
    year         = 2000, 
    country_data = tibble(.rows = 50)
    ) {
    self$year         <- year
    self$country_data <- country_data
  },
  set_year         = function(year) {
    #' set_year 
    #' 
    #' @description Sets the year for evaluation.
    #' 
    #' @param year numeric Common year
    
    self$year <- year
  }, 
  set_iso_alpha3  = function(iso3) { 
    #' set_iso_alpha3 
    #' 
    #' @description Used after constructor to instantiate at country-level.
    #' 
    #' @param iso3 character ISO Alpha-3 alphanumeric country code
    #' 
    #' @details Set the country object's ISO alpha3 code first to fill in other variables automatically.
    #' @examples 
    #' obj <- country$new()
    #' obj$set_iso_alpha3("CAN")
    
    self$iso_alpha3 <- iso3
    
    self$countryname <- loc_map %>% 
      dplyr::filter(ISO3_Code == iso3) %>% 
      dplyr::pull(Location)
    
    self$fao_countrycode <- loc_map %>% 
      dplyr::filter(ISO3_Code == iso3) %>% 
      dplyr::pull(Area.Code)
    
    self$un_countrycode <- loc_map %>% 
      dplyr::filter(ISO3_Code == iso3) %>% 
      dplyr::pull(LocID)
    
    self$subregioncode <- loc_map %>% 
      dplyr::filter(ISO3_Code == iso3) %>% 
      dplyr::pull(SubRegID)
    
    self$subregionname <- loc_map %>% 
      dplyr::filter(ISO3_Code == iso3) %>% 
      dplyr::pull(SubRegName)
  }, 
  set_country_data = function() {
    #' set_country_data 
    #' 
    #' @description Appends annual data from class variables to data frame of prior variable values.
    #' 
    #' @details Set the country object's ISO alpha3 code first to fill in other variables automatically.
    #' @examples 
    #' obj <- country$new()
    #' obj$set_iso_alpha3("CAN")
    #' obj$set_year(2001)
    #' obj$set_country_data()
    #' 
    df = tibble(
      country_name         = self$countryname, 
      iso_alpha3_code      = self$iso_alpha3, 
      fao_countrycode      = self$fao_countrycode, 
      un_countrycode       = self$un_countrycode, 
      subregion_name       = self$subregionname, 
      subregion_id_code_un = self$subregioncode, 
      year                 = self$year
    )
    self$country_data <- self$country_data %>% 
                                 bind_rows(df)
  }, 
  get_country_data = function() {
    #' get_country_data 
    #' 
    #' @description Data frame container for country class variables.
    #' 
    #' @return Tibble (data frame) of all country class data set so far.
    #' 
    return(as_tibble(self$country_data))
  }
))

#population module
population <- R6::R6Class(
  "population", 
  inherit = country, 
  list(
    population = NA, 
    cgr = NA, 
    net_migration = NA, 
    cnmr = NA, 
    population_data = tibble(), 
    migration_data = tibble(), 
    set_population = function(value) {
      #' set_population 
      #' 
      #' @description Sets the population for specified country.
      self$population <- value
      invisible(self)
    }, 
    set_cgr = function(value) {
      #' set_cgr 
      #' 
      #' @description Sets the crude growth rate of the population (birth minus death rate).
      #' 
      #' @details Defined as CGR = (number of live births - number of deaths)/(mid-year population * 1e4).
      self$cgr <- value
      invisible(self)
    }, 
    set_migration = function(value) {
      #' set_migration 
      #' 
      #' @description Sets the net migration (number of immigrants minus number of emigrants) for specified country.
      self$net_migration <- value
      invisible(self)
    },
    set_cnmr = function(value) {
      #' set_cnmr 
      #' 
      #' @description Sets the crude net migration rate of the population.
      #' 
      #' @details Defined as CNMR = (number of immigrants - number of emigrants)/(mid-year population * 1e4).
      self$cnmr <- value
      invisible(self)
    }, 
    set_population_data = function() {
      #' set_population_data 
      #' 
      #' @description Data frame container for population variables.
      #' 
      #' @examples 
      #' obj <- population$new()
      #' obj$set_iso_alpha3("CAN")
      #' obj$set_year(2001)
      #' obj$set_population_data()
      #' 
      df <- tibble(
        iso_alpha3_code = self$iso_alpha3, 
        fao_countrycode = self$fao_countrycode, 
        year            = self$year,
        population      = self$population, 
        cgr             = self$cgr
      )
      self$population_data = self$population_data %>% 
        bind_rows(df)
      invisible(self)
    }, 
    get_population_data = function() {
      #' get_population_data 
      #' 
      #' @returns Tibble of historical population variables.
      return(as_tibble(self$population_data))
      invisible(self)
    }, 
    set_migration_data = function() {
      #' set_migration_data 
      #' 
      #' @description Data frame container for migration variables.
      #' 
      df <- tibble(
        iso_alpha3_code = self$iso_alpha3, 
        fao_countrycode = self$fao_countrycode, 
        year            = self$year,
        net_migration   = self$net_migration, 
        cnmr            = self$cnmr
      )
      self$migration_data = self$migration_data %>% 
        bind_rows(df)
      invisible(self)
    }, 
    get_migration_data = function() {
      #' get_migration_data 
      #' 
      #' @returns Tibble of historical migration variables.
      return(as_tibble(self$migration_data))
      invisible(self)
    }
  ))

#land use module
land_use <- R6::R6Class(
  "land_use", 
  inherit = country, 
  list(
    land_use_type         = NA, 
    land_use_area         = NA, 
    dry_matter_production = NA,
    total_land_area       = tibble(.rows = 50), 
    land_use_data         = tibble(.rows = 50), 
    # land_use_change_data = tibble(), 
    set_land_use_type = function(value) { 
      #' set_land_use_type 
      #' 
      #' @description Sets the land use area type according to FAO land use classification at country-level.
      #' 
      #' @param value character land use by type c("cropland", "fallow_cropland", "pasture", "otherland", "forest")
      #' 
      self$land_use_type <- value
      invisible(self)
    }, 
    set_land_use_area = function(value) { 
      #' set_land_use_area 
      #' 
      #' @description Sets the value of land area (hectares) for specified land use type at country-level.
      #' 
      #' @param value numeric value of land area (hectares)
      #' 
      self$land_use_area <- value
      invisible(self)
    }, 
    set_dry_matter_production = function(value) {
      #' set_dry_matter_production 
      #' 
      #' @description Sets the value of dry matter production (tonnes per year) at country-level.
      #' 
      #' @param value numeric total tonnes of dry matter per year
      #' 
      self$dry_matter_production <- value
      invisible(self)
    }, 
    set_land_use_data = function() {
      #' set_land_use_data 
      #' 
      #' @description Data frame container for land use class variables.
      #' 
      #' @examples 
      #' obj <- land_use$new()
      #' obj$set_iso_alpha3("CAN")
      #' obj$set_year(2001)
      #' obj$set_land_use_data()
      #' 
      x <- tibble(
        iso_alpha3_code       = self$iso_alpha3, 
        fao_countrycode       = self$fao_countrycode, 
        year                  = self$year,
        land_use_type         = self$land_use_type, 
        land_use_area         = self$land_use_area, 
        dry_matter_production = self$dry_matter_production
      )
      self$land_use_data = rbind(self$land_use_data, x)
      invisible(self)
    }, 
    get_land_use_data = function(value) {
      return(as_tibble(self$land_use_data))
    }
    # , 
    # get_total_land_area = function() {
    #   self$total_land_area = self$get_land_use_data %>% 
    #     dplyr::group_by(iso_alpha3_code, 
    #                     fao_countrycode, 
    #                     year, 
    #                     .groups = "drop") %>% 
    #     dplyr::summarize(total_land_area = sum(land_use_area, 
    #                                            na.rm = TRUE))
    #   return(as_tibble(self$total_land_area))
    #   invisible(self)
    # }
  )
)

# crop module
crop <- R6::R6Class(
  "crop", 
  inherit = country, 
  list(
    model_group              = NA, 
    ratio_of_land_allotted = NA, 
    harvest_area           = NA, 
    harvest_yield          = NA, 
    production             = NA, #NB: production is domestic (it does not include imports or exports)
    food_stock             = NA,
    feed_stock             = NA,
    seed_stock             = NA,
    loss_stock             = NA,
    proc_stock             = NA,
    othe_stock             = NA, 
    food_imports           = NA,
    feed_imports           = NA,
    seed_imports           = NA,
    loss_imports           = NA,
    proc_imports           = NA,
    othe_imports           = NA, 
    food_exports           = NA,
    feed_exports           = NA,
    seed_exports           = NA,
    loss_exports           = NA,
    proc_exports           = NA,
    othe_exports           = NA, 
    crop_data              = tibble(.rows = 50), 
    trade_crop_data        = tibble(.rows = 50), 
    set_model_group = function(value) { 
      #' set_model_group 
      #' 
      #' @description Sets the value of model_group at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      self$model_group <- value
    }, 
    set_land_allotted = function(value) { 
      #' set_land_allotted 
      #' 
      #' @description Sets the value of land area allotted (hectares) to crop at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$ratio_of_land_allotted <- value
    }, 
    set_harvest_area = function(value) { 
      #' set_harvest_area 
      #' 
      #' @description Sets the value of land area harvested (hectares) for crop at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$harvest_area <- value
    }, 
    set_harvest_yield = function(value) { 
      #' set_harvest_yield 
      #' 
      #' @description Sets the value of harvested crop yield (tonnes per hectare) at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$harvest_yield <- value
    }, 
    set_production = function(value) { 
      #' set_harvest_yield 
      #' 
      #' @description Sets the value of crop production (tonnes) at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$production <- value
    }, 
    set_food_stock = function(value) { 
      #' set_food_stock 
      #' 
      #' @description Sets the value of food intended for human consumption production (tonnes) at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$food_stock <- value
    }, 
    set_feed_stock = function(value) { 
      #' set_feed_stock 
      #' 
      #' @description Sets the value of feed intended for animal consumption production (tonnes) at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$feed_stock <- value
    }, 
    set_seed_stock = function(value) { 
      #' set_seed_stock 
      #' 
      #' @description Sets the value of seed intended for crop reproduction (tonnes) at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$seed_stock <- value
    }, 
    set_loss_stock = function(value) { 
      #' set_loss_stock 
      #' 
      #' @description Sets the value of crop lost or wasted (tonnes) at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$loss_stock <- value
    }, 
    set_proc_stock = function(value) { 
      #' set_proc_stock 
      #' 
      #' @description Sets the value of crop allotted for non-food processing (tonnes) purposes at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$proc_stock <- value
    }, 
    set_othe_stock = function(value) { 
      #' set_othe_stock 
      #' 
      #' @description Sets the value of crop allotted for non-food other use (tonnes) purposes at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$othe_stock <- value
    }, 
    set_food_imports = function(value) { 
      #' set_food_imports 
      #' 
      #' @description Sets the value of food crops imported (tonnes) at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$food_imports <- value
    }, 
    set_food_exports = function(value) { 
      #' set_food_exports 
      #' 
      #' @description Sets the value of food crops exported (tonnes) at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$food_exports <- value
    }, 
    set_feed_imports = function(value) { 
      #' set_feed_imports 
      #' 
      #' @description Sets the value of feed crops imported (tonnes) at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$feed_imports <- value
    }, 
    set_feed_exports = function(value) { 
      #' set_feed_exports 
      #' 
      #' @description Sets the value of feed crops exported (tonnes) at country-level.
      #' 
      #' @param character model group, i.e., model_group = c("cereal",  "pulse", "oilcrop", "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop")
      #' 
      self$feed_exports <- value
    }, 
    set_crop_data = function() { 
      #' set_crop_data 
      #' 
      #' @description Appends data frame containing crop data with latest variable values at country-level.
      #' 
      #' 
      x = tibble(
        iso_alpha3_code        = self$iso_alpha3, 
        fao_countrycode        = self$fao_countrycode, 
        year                   = self$year,
        model_group              = self$model_group, 
        ratio_of_land_allotted = self$ratio_of_land_allotted, 
        harvest_area           = self$harvest_area, 
        harvest_yield          = self$harvest_yield, 
        production             = self$production, 
        food_stock             = self$food_stock,
        feed_stock             = self$feed_stock,
        seed_stock             = self$seed_stock,
        loss_stock             = self$loss_stock,
        proc_stock             = self$proc_stock,
        othe_stock             = self$othe_stock 
      )
      self$crop_data = rbind(self$crop_data, x)
    }, 
    get_crop_data = function(value) { 
      #' get_crop_data 
      #' 
      #' @description Data frame containing crop data with latest variable values at country-level.
      #' @return Tibble (data frame) containing crop data at country-level.
      #' 
      return(as_tibble(self$crop_data))
    }, 
    set_trade_crop_data = function() { 
      #' set_trade_crop_data 
      #' 
      #' @description Appends data frame containing crop trade data with latest variable values at country-level.
      #' 
      x = tibble(
        iso_alpha3_code  = self$iso_alpha3, 
        fao_countrycode  = self$fao_countrycode, 
        year             = self$year,
        model_group        = self$model_group, 
        food_imports     = self$food_imports, 
        feed_imports     = self$feed_imports, 
        food_exports     = self$food_exports, 
        feed_exports     = self$feed_exports
      )
      self$trade_crop_data = rbind(self$trade_crop_data, x)
    }, 
    get_trade_crop_data = function() { 
      #' set_trade_crop_data 
      #' 
      #' @description Data frame containing crop trade data with latest variable values at country-level.
      #' @return Tibble (data frame) containing crop trade data at country-level.
      return(as_tibble(self$trade_crop_data)) 
    }
  )
)

livestock <- R6::R6Class(
  "livestock", 
  inherit = country, 
  list(
    model_group               = NA, 
    dairy_stock_quantity      = NA, 
    meat_stock_quantity       = NA, 
    other_stock_quantity      = NA, 
    stock_growth_rate         = NA, 
    herd_tlu                  = NA, 
    feed_drymatter_demand     = NA, 
    feed_crude_protein_demand = NA, 
    livestock_data            = tibble(.rows = 50), 
    set_model_group = function(value) {
      self$model_group = value
    }, 
    set_dairy_stock_quantity = function(value) {
      self$dairy_stock_quantity = value
    }, 
    set_meat_stock_quantity = function(value) {
      self$meat_stock_quantity = value
    }, 
    set_other_stock_quantity = function(value) {
      self$other_stock_quantity = value
    }, 
    set_stock_growth_rate = function(value) {
      self$stock_growth_rate = value
    }, 
    set_herd_tlu = function(value) {
      self$herd_tlu <- value
    }, 
    set_feed_drymatter_demand = function(value) {
      self$feed_drymatter_demand = value
    }, 
    set_feed_crude_protein_demand = function(value) {
      self$feed_crude_protein_demand = value
    }, 
    set_livestock_data = function() {
      x <- tibble(
        iso_alpha3_code           = self$iso_alpha3, 
        fao_countrycode           = self$fao_countrycode, 
        year                      = self$year,
        model_group               = self$model_group, 
        herd_tlu                  = self$herd_tlu, 
        dairy_stock_quantity      = self$dairy_stock_quantity, 
        meat_stock_quantity       = self$meat_stock_quantity, 
        other_stock_quantity      = self$other_stock_quantity, 
        stock_growth_rate         = self$stock_growth_rate, 
        total_stock_quantity      = self$dairy_stock_quantity +
          self$meat_stock_quantity +
          self$other_stock_quantity,
        feed_drymatter_demand     = self$feed_drymatter_demand, 
        feed_crude_protein_demand = self$feed_crude_protein_demand
      )
      self$livestock_data = rbind(self$livestock_data, x)
    }, 
    get_livestock_data = function() {
      return(as_tibble(self$livestock_data))
    }
  )
)

#' Uses data from Mikkonen and Hoekstra, 2005, for water footprint. Presently just for crops and livestock.
#' 
#' \code{water_footprint} Stands for water footprint of AFOLU products.
#' @inherit country
#' @param green_wf double (units m^3) for Green Water Footprint: The amount of rainwater required (evaporated or used directly) to make an item. 
#' @param blue_wf double (units m^3) for Blue Water Footprint: The amount of surface water and groundwater required (evaporated or used directly) to produce an item.
#' @param grey_wf double (units m^3) for Grey Water Footprint: The amount of freshwater required to dilute the wastewater generated in manufacturing, in order to maintain water quality , as determined by state and local standards.
#' @param wf_data data frame containing water footprint data, country and year information
#' @return a double for water footprint in m^3 per tonne of product
#' @export
water_footprint <- R6::R6Class(
  "water_footprint", 
  inherit = country, 
  list(
    model_group    = NA, 
    green_wf       = NA, 
    blue_wf        = NA, 
    grey_wf        = NA, 
    traded_green_wf= NA, 
    traded_blue_wf = NA, 
    traded_grey_wf = NA, 
    wf_data        = tibble(), 
    traded_wf_data = tibble(), 
    set_model_group = function(value) {
      self$model_group <- value
    }, 
    set_green_wf = function(value) {
      self$green_wf <- value
    }, 
    set_blue_wf = function(value) {
      self$blue_wf <- value
    }, 
    set_grey_wf = function(value) {
      self$grey_wf <- value
    }, 
    set_traded_green_wf = function(value) {
      self$traded_green_wf <- value
    }, 
    set_traded_blue_wf = function(value) {
      self$traded_blue_wf <- value
    }, 
    set_traded_grey_wf = function(value) {
      self$traded_grey_wf <- value
    }, 
    set_wf_data = function() { 
      x <- tibble(
        iso_alpha3_code = self$iso_alpha3, 
        fao_countrycode = self$fao_countrycode, 
        year            = self$year, 
        model_group     = self$model_group, 
        green_wf        = self$green_wf, 
        blue_wf         = self$blue_wf, 
        grey_wf         = self$grey_wf
      )
      self$wf_data <- rbind(self$wf_data, x)
    }, 
    #' #' TODO This function will update the wf_data tibble with new crops and animal products data after trade
    #' #' Instead, I am treating traded crops separately for the moment.
    #' update_wf_data = function(value1, value2, value3) {
    #'   x <- self$wf_data %>% 
    #'     dplyr::filter(iso_alpha3_code == value1 & year == value2 & model_group == value3) %>% 
    #'     dplyr::rows_update(
    #'       green_wf = self$green_wf, 
    #'       blue_wf  = self$blue_wf, 
    #'       grey_wf  = self$grey_wf
    #'       )
    #'   self$wf_data <- x
    #' }, 
    get_wf_data = function() {
      return(as_tibble(self$wf_data))
    }, 
    set_traded_wf_data = function() { 
      x <- tibble(
        iso_alpha3_code = self$iso_alpha3, 
        fao_countrycode = self$fao_countrycode, 
        year            = self$year, 
        model_group     = self$model_group, 
        traded_green_wf = self$traded_green_wf, 
        traded_blue_wf  = self$traded_blue_wf, 
        traded_grey_wf  = self$traded_grey_wf
      )
      self$traded_wf_data <- rbind(self$traded_wf_data, x)
    }, 
    get_traded_wf_data = function() {
      return(as_tibble(self$traded_wf_data))
    }
  )
)




diet <- R6::R6Class(
  "diet", 
  inherit = country, 
  list(
    model_group = NA, 
    kcal        = NA, 
    fats        = NA, 
    protein     = NA, 
    diet_data   = tibble(.rows = 50), 
    set_model_group = function(value) {
      #' set_model_group
      #' 
      #' @description Sets the model group, e.g., model_group = c("cereal", "pulse", "oilcrop", 
      #' "fibercrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop", "aves_eggs", 
      #' "aves_meat", "bovine_dairy", "bovine_meat", "camelid_dairy", "camelid_meat", "caprine_dairy", 
      #' "caprine_meat", "equine_meat", "rodentia_meat", "sus_meat", "fish", "insecta")
      #' 
      self$model_group <- value
      invisible(self)
    }, 
    set_kcal = function(value) {
      self$kcal <- value
      invisible(self)
    }, 
    set_fats = function(value) {
      self$fats <- value
      invisible(self)
    }, 
    set_protein = function(value) {
      self$protein <- value
      invisible(self)
    }, 
    set_diet_data = function() {
      df <- tibble(
        iso_alpha3_code = self$iso_alpha3, 
        fao_countrycode = self$fao_countrycode, 
        year            = self$year,
        model_group     = self$model_group, 
        kcal            = self$kcal, 
        fats            = self$fats, 
        protein         = self$protein 
      )
      self$diet_data = self$diet_data %>% 
        bind_rows(df)
      invisible(self)
    }, 
    get_diet_data = function() {
      return(as_tibble(self$diet_data))
      invisible(self)
    }
  )
)

