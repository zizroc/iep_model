#instantiates at country level
library(roxygen2)
library(R6)
library(tidyverse)

#data
loc_map <- read.csv("~/Projects/IEP_2/Model/R_files/Data_files/location_code_map_data.csv", 
                    header = TRUE, 
                    stringsAsFactors = FALSE) %>% 
  dplyr::select(-X) %>% 
  na.omit()

# countrycodes <- loc_map %>% 
#   filter(ISO3_Code %in% c("ARG", "CHN")) %>% 
#   # filter(ISO3_Code %in% c("ARG", "CAN", "CHN", "MEX", "USA")) %>% 
#   # filter(SubRegName %in% c("Eastern Asia")) %>%
#   dplyr::select(ISO3_Code, Area, Location, Area.Code, LocID)
# 
# countrycode_iso3alpha   <- countrycodes %>% 
#   pull(ISO3_Code)
# countrycode_un_numeric  <- countrycodes %>% 
#   pull(LocID)
# countrycode_fao_numeric <- countrycodes %>% 
#   pull(Area.Code)

#' Define an R6Class for country
#' 
#' @param iso_alpha3      The country's unique 3-character string ID.
#' @param countryname     Official long name of the country.
#' @param fao_countrycode The country's unique numerical ID as used by Food and Agriculture Organisation.
#' @param un_countrycode  The country's unique numerical ID as used by the United Nations.
#' @param subregioncode   The containing subregion's unique numerical ID as used by UN.
#' @param subregionname   The containing subregion's short name as used by UN.
#' @param year            Numerical year, starting at 2000.
#' @param country_data    Tibble containing country level identifiers.
#' @return                Instance of country level data.
#' @examples
#' country$set_iso_alpha3("CAN")
country <- R6::R6Class("country", list(
  iso_alpha3      = NA, 
  countryname     = NA, 
  fao_countrycode = NA, 
  un_countrycode  = NA, 
  subregioncode   = NA, 
  subregionname   = NA, 
  year            = NA, 
  country_data    = tibble(.rows = 50), 
  # initialize = function(year = 2000) {
  #   self$year <- year
  # }, 
  set_year         = function(x) {
    self$year <- x
  }, 
  set_iso_alpha3  = function(x) {
    self$iso_alpha3 <- x
    
    self$countryname <- loc_map %>% 
      dplyr::filter(ISO3_Code == x) %>% 
      dplyr::pull(Location)
    
    self$fao_countrycode <- loc_map %>% 
      dplyr::filter(ISO3_Code == x) %>% 
      dplyr::pull(Area.Code)
    
    self$un_countrycode <- loc_map %>% 
      dplyr::filter(ISO3_Code == x) %>% 
      dplyr::pull(LocID)
    
    self$subregioncode <- loc_map %>% 
      dplyr::filter(ISO3_Code == x) %>% 
      dplyr::pull(SubRegID)
    
    self$subregionname <- loc_map %>% 
      dplyr::filter(ISO3_Code == x) %>% 
      dplyr::pull(SubRegName)
  }, 
  set_country_data = function() {
    x = tibble(
      country_name         = self$countryname, 
      iso_alpha3_code      = self$iso_alpha3, 
      fao_countrycode      = self$fao_countrycode, 
      un_countrycode       = self$un_countrycode, 
      subregion_name       = self$subregionname, 
      subregion_id_code_un = self$subregioncode, 
      year                 = self$year
    )
    self$country_data <- rbind(self$country_data, x)
  }, 
  get_country_data = function() {
    return(as_tibble(self$country_data))
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
      self$land_use_type <- value
      invisible(self)
    }, 
    set_land_use_area = function(value) {
      self$land_use_area <- value
      invisible(self)
    }, 
    set_dry_matter_production = function(value) {
      self$dry_matter_production <- value
      invisible(self)
    }, 
    set_land_use_data = function() {
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
    model_group            = NA, 
    ratio_of_land_allotted = NA, 
    harvest_area           = NA, 
    harvest_yield          = NA, 
    production             = NA, #NB: production is domestic (it does not include imports or exports)
    food_stock             = NA,
    feed_stock             = NA,
    seed_stock             = NA,
    losses_stock           = NA,
    processing_stock       = NA,
    other_uses_stock       = NA, 
    food_imports           = NA,
    feed_imports           = NA,
    seed_imports           = NA,
    losses_imports         = NA,
    processing_imports     = NA,
    other_uses_imports     = NA, 
    food_exports           = NA,
    feed_exports           = NA,
    seed_exports           = NA,
    losses_exports         = NA,
    processing_exports     = NA,
    other_uses_exports     = NA, 
    crop_data              = tibble(.rows = 50), 
    trade_crop_data        = tibble(.rows = 50), 
    set_model_group = function(value) {
      self$model_group <- value
    }, 
    set_land_allotted = function(value) {
      self$ratio_of_land_allotted <- value
    }, 
    set_harvest_area = function(value) {
      self$harvest_area <- value
    }, 
    set_harvest_yield = function(value) {
      self$harvest_yield <- value
    }, 
    set_production = function(value) {
      self$production <- value
    }, 
    set_food_stock = function(value) {
      self$food_stock <- value
    }, 
    set_feed_stock = function(value) {
      self$feed_stock <- value
    }, 
    set_seed_stock = function(value) {
      self$seed_stock <- value
    }, 
    set_losses_stock = function(value) {
      self$losses_stock <- value
    }, 
    set_processing_stock = function(value) {
      self$processing_stock <- value
    }, 
    set_other_uses_stock = function(value) {
      self$other_uses_stock <- value
    }, 
    set_food_imports = function(value) {
      self$food_imports <- value
    }, 
    set_food_exports = function(value) {
      self$food_exports <- value
    }, 
    set_feed_imports = function(value) {
      self$feed_imports <- value
    }, 
    set_feed_exports = function(value) {
      self$feed_exports <- value
    }, 
    set_crop_data = function() {
      x = tibble(
        iso_alpha3_code        = self$iso_alpha3, 
        fao_countrycode        = self$fao_countrycode, 
        year                   = self$year,
        model_group            = self$model_group, 
        ratio_of_land_allotted = self$ratio_of_land_allotted, 
        harvest_area           = self$harvest_area, 
        harvest_yield          = self$harvest_yield, 
        production             = self$production, 
        food_stock             = self$food_stock,
        feed_stock             = self$feed_stock,
        seed_stock             = self$seed_stock,
        losses_stock           = self$losses_stock,
        processing_stock       = self$processing_stock,
        other_uses_stock       = self$other_uses_stock 
      )
      self$crop_data = rbind(self$crop_data, x)
    }, 
    get_crop_data = function(value) {
      return(as_tibble(self$crop_data))
    }, 
    set_trade_crop_data = function() {
      x = tibble(
        iso_alpha3_code  = self$iso_alpha3, 
        fao_countrycode  = self$fao_countrycode, 
        year             = self$year,
        model_group      = self$model_group, 
        food_imports     = self$food_imports, 
        feed_imports     = self$feed_imports, 
        food_exports     = self$food_exports, 
        feed_exports     = self$feed_exports
      )
      self$trade_crop_data = rbind(self$trade_crop_data, x)
    }, 
    get_trade_crop_data = function() {
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
