# policy data

historical_land_use_data = data.frame(
  iso_alpha3_code = "CHN", 
  fao_countrycode = 351, 
  year            = 2000, 
  land_use_type   = c("cropland", "permanent_cropland", "arable_land", "pasture", "forest", "otherland"), 
  land_use_area   = c(     6.2e6,               5.58e6,         6.2e5,       1e6,      1e7,         1e5)
)

#the following are just placeholders
#to do this properly, some animal mix groups can contribute to improving land management
population_policy_df <- data.frame (
  iso_alpha3_code = c("CHN", "CHN"),
  year            = c(2001, 2002),  
  cgr             = c(0.01, 0.01)
)

migration_policy_df <- data.frame (
  iso_alpha3_code = c("CHN", "CHN"),
  year            = c(2001, 2002), 
  net_migration   = c(0.01, 0.01)
)

land_use_policy_df <- data.frame (
  iso_alpha3_code        = rep("CHN", 10),
  year                   = c(rep(2001, 5), rep(2002, 5)), 
  land_use_type          = rep(c("cropland", "fallow_cropland", "pasture", "forest", "otherland"), 2), 
  delta_land_use_area    = 0
)

crop_policy_df <- data.frame (
  iso_alpha3_code        = "CHN",
  year                   = c(rep(2001, 9), rep(2002, 9)), 
  model_group            = rep(c("cereal", "pulse", "oilcrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop"), 2), 
  delta_harvest_yield    = 0, 
  delta_allocation       = 0
)

cropland_allocation_policy_df <- data.frame(
  iso_alpha3_code  = "CHN",
  year             = c(rep(2001, 9), rep(2002, 9)), 
  model_group      = rep(c("cereal", "pulse", "oilcrop", "rootstubers", "vegetable", "fruit", "citrus", "treenut", "sugarcrop"), 2), 
  delta_food_ratio = 0, 
  delta_feed_ratio = 0, 
  delta_seed_ratio = 0, 
  delta_loss_ratio = 0, 
  delta_proc_ratio = 0, 
  delta_othe_ratio = 0
)

livestock_policy_df <- data.frame (
  iso_alpha3_code        = "CHN",
  year                   = c(rep(2001, 8), rep(2002, 8)), 
  model_group            = rep(c("aves", "beehive", "bovine", "camelid", "caprine", "rodentia", "sus", "fish"), 2), 
  crude_growth_rate      = 0
)


# 
# pasture_DM_productivity = data.frame(
#   iso_alpha3_code  = "CHN", 
#   fao_countrycode  = 351, 
#   year             = c(2000, 2001, 2002, 2003, 2004, 2005), 
#   land_use_type    = c("pasture"), 
#   tonnes_DM_per_ha = c(      2.2)
# )
# 
# cropland_allotment_by_type = data.frame(
#   iso_alpha3_code         = "CHN", 
#   fao_countrycode         = 351, 
#   year                    = c(rep(2000, 10), rep(2001, 10), rep(2002, 10), rep(2003, 10), rep(2004, 10), rep(2005, 10)), 
#   model_group             = c("cereal", "pulse", "oilcrop", "rootstubers", "fibercrop", "citrus", "vegetable", "fruit", "treenut", "sugarcrop"),  
#   ratio_of_land_allotted  = c(     0.4,       0,       0.1,           0.1,           0,     0.05,         0.1,     0.1,       0.1,        0.05) 
# )
# 
# historical_crop_data = data.frame(
#   fao_countrycode         = 351, 
#   model_group             = c("cereal", "pulse", "oilcrop", "rootstubers", "fibercrop", "citrus", "vegetable", "fruit", "treenut", "sugarcrop"), 
#   year                    = 2000, 
#   ratio_of_land_allotted  = 0.6, 
#   harvest_area            = 1e6, 
#   harvest_yield           = 2.4, 
#   production              = 2.4e6
# )
# crop_management_df = data.frame(
#   fao_countrycode     = 351, 
#   model_group         = c("cereal", "pulse", "oilcrop", "rootstubers", "fibercrop", "citrus", "vegetable", "fruit", "treenut", "sugarcrop"), 
#   year                = c(rep(2000, 10), rep(2001, 10), rep(2002, 10), rep(2003, 10), rep(2004, 10), rep(2005, 10)), 
#   delta_harvest_area  = 0.01, 
#   delta_harvest_yield = 0.01
# )
# trade_data = data.frame(
#   fao_countrycode = 351, 
#   model_group     = "cereal", 
#   year            = c(2000, 2001, 2002, 2003, 2004, 2005), 
#   food_imports    = runif(6, 1e4, 1e5), 
#   food_exports    = runif(6, 1e4, 1e5), 
#   feed_imports    = runif(6, 1e4, 1e5), 
#   feed_exports    = runif(6, 1e4, 1e5)
# )