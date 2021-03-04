<!-- README.md is generated from README.Rmd. Please edit that file -->

# iep_model

<!-- badges: start -->
<!-- badges: end -->

iep_model is an integrated system model under development for the Iterative Eden Project.

## Structure

The Model is organized into 3 classes of files: (1) module files (e.g., 'crops.R' and 'crop_manager.R') that define model class structures and logic; (2) data files (e.g., 'policy_data.R') that shape raw data sources into data frames that may be passed to the module files; and (3) a simulator ('simulator.R') that calls and sequences the modules. The module files live in the /R directory; and the data files live in the /data directory, nested within /R. The simulator file lives in a separate /simulator directory.

There are 2 main types of module files: (i) state files and (ii) state manager files. State files (e.g., 'crops.R') contain R6 classes of abstract system states, and they must be given an instance (i.e., have their initially empty parameters mapped to a country, a year, a state of this country's crops, etc.). State files also contain data frames that work like a memory of the different parameter values the state files have had through time, for different countries, and so on. State files are instantiated by the simulator file. State manager files contain functions that cause, and logic that constrains, changes in state files' parameters as the simulator evolves states time-step by time-step. State manager functions may also contain logic that constrains spatial changes (e.g., cropland area must be smaller than a country's total land area).

The code structure of state and state manager files differs slightly because we do not need to use the full object oriented character for state manager files that R6 classes provide. Specifically, we require that state files produce mutable objects, but state manager file objects need not be mutable. We are defining state managers in terms of R6 objects because this keeps state manager functions from entering the global workspace environment, since they are defined only within their own R6 class environments. Hence, to call a function (and obtain a value), we instantiate a manager file, viz.

``` r
crp_mng <- crop_manager$new()
value   <- crp_mng$manage_harvest_area(...)
```

State files' parameters are either single-valued variables (that may be of numerical or character string type) or data frames that contain vectors of numerical or character string type. The logic that sets variable parameters in state classes allows only for those values to be replaced, so they can never be other than single-valued. The logic that sets data frame parameters in state classes usually appends updated values to a data frame of previously set values, creating a memory of prior system states. Data frame parameters contain "data" in their labels.

Data files generally convert raw data files to structured data frames that may be passed to the state and state manager files. Therefore, data files have the least standard format. To make these as legible as possible, we follow tidy data shaping conventions and make plenty of comments. In the interests of reproducibility, we have tried to use open access data sources and download raw sources to shape within the code itself; however, this is not always possible.

The simulator file sequences modules in order because of dependencies shared between the modules. Running modules out of order, or omitting models unless expressly allowed, may cause fatal errors in the Model or errors in its results. Presently, "modules" are loosely defined by in-line code comments in the simulator file. These modules group code that instantiates state and state manager files, and then uses state manager functions to set parameters in the state files. Each state file contains one or more parameters whose labels include the word "data". Once the state's single-valued variable parameters are set, these are passed to a data parameter, which appends the new values to prior values.

In the simulator file, modules are split into 2 parts, which may be thought of as "supply" and "demand" parts of the module. The supply part of a module instantiates a state object and its associated state manager object (e.g., 'crop' and 'crop_manager') and fills the 'data' parameter for each instance (i.e., country, year, etc.). Because humans and livestock compete for crops (food and feed, respectively), crops and livestock compete for land, and so on, the other modules' supply parts must be loaded before demands may be considered. The demand part of a module revisits the total demand for some product (e.g., cereal crops) from humans and animals, and imports or exports to zero deficits or surpluses respectively. In the Model, this is done via trade. Presently, the Model trades only food and feed from crops, and trade products are in effect sourced from, and sent to, a cornucopia of infinite size and riches. This will become a clearinghouse with logic to constrain trade in terms of geographic range (i.e., regionalization, high transport cost, tarrifs, etc.) and strategic (water) or environmental considerations (biodiversity protection, carbon cost, etc.).

## Installation

Right now the Model must be run from a local machine. The Model itself is small, but you must have space for a few GB of data.

Start by cloning this repo to your machine, or clicking 'Code' > 'Download ZIP' on GitHub. (The RStudio IDE makes this straightforward, with lots of good documentation. If you are new to RStudio, download/update to the most recent version from [here](https://rstudio.com/products/rstudio/download/#download).) Extract the 'iep_model-main.zip' and rename the new folder '/iep_model'. Find 'iep_model-main.Rproj' in '/iep_model' and click on it. This should begin an RStudio session.

From RStudio, use 'Terminal' to set up a new data folder ('/data_iep_model') somewhere else on your machine; i.e., not within the '/iep_model' folder. (It is not necessary to access a terminal from within RStudio. If you are not yet familiar with shell programming, it is convenient to keep everything in one place.) Create a new R script file and paste the following (replacing the tilde as required) to make a path to this folder, viz.

``` r
data_path <- "~/data_iep_model"
data_dir  <- dir(data_path)

model_path <- "~/iep_model"
model_dir  <- dir(model_path)

```

You must now download and extract data for the Model. Run the 'data_downloader.R' script in your console.
```{r}
# open this file and run as a script
["~/iep_model/model/R/data/data_downloader.R"]("~/iep_model/model/R/data/data_downloader.R")
```

You should now have a set of large data files in their own folders nested within your 'data_path' directory. (You will see these listed as .csv files. Unfortunately, the FAO uses inconsistent conventions when structuring their data; so we will treat these .csv files as raw data that will need to be reshaped to be used by the Model. Reshaping will generally be handled automatically by the '_manager.R' files. While you confirm this you can also take the opportunity to delete the .zip versions of the data files.)



## Example

Instantiating a country-level object.

``` r
library(IEPModel)

# chn <- country$new()
# chn$set_iso_alpha3_code("CHN")
# chn
```

The R6Class object ‘chn’ is now instantiated as model country China, containing identification codes from the FAO and UN. Extract other features from the ‘chn’ object by calling variable names, such as ‘countryname’ which returns “China”.

``` r
# chn$countryname
```

In the Model, many functions live inside of R6Class objects, and not in the global workspace. You will need to instantiate one of the manager classes, e.g., ‘crops\_manager’, to make available any function to do with crop management.

``` r
# crp     <- crops$new()
# crp$set_iso_alpha3_code("CHN") # this is allowed because the crops class inherits the country class functions
# crp$set_year(2000) # arbitrarily set year (2000 is the lowest model year considered)
# 
# country <- crp$fao_countrycode # instantiating 'crp' as China above automatically generates ID codes
# year    <- crp$year
# crop    <- crp$model_group #where model_group = c("cereal", "pulse", etc.)
# # df is a data frame that allows crop production from previous model years to be passed to the function
# 
# crp_mng         <- crop_manager$new()
# crp$set_production(crp_mng$production(country, year, crop, df = NULL))
# crp$set_crop_data()
```

The ‘crp’ object now contains the crop production information for the selected model_group in China for the year 2000. Inspect it thus:

``` r
# crp$get_crop_data()
```

This prints a tibble containing the instance of country level data.