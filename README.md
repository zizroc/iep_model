<!-- README.md is generated from README.Rmd. Please edit that file -->

# iep_model

<!-- badges: start -->
<!-- badges: end -->

The iep_model is an integrated system model framework under development for the Iterative Eden Project.

## Structure

The Model is organized into 3 kinds of files: (1) module files (e.g., 'crops.R' and 'crop_manager.R') that define model class structures and logic; (2) data files (e.g., 'policy_data.R') that shape raw data sources into data frames that may be passed to the module files; and (3) a simulator ('simulator.R') that calls and sequences the modules. The module files live in the /R directory; and the data files live in the /data directory, nested within /R. The simulator file lives in a separate /simulator directory.

There are 2 main types of module files: (i) state files and (ii) state manager files. State files (e.g., 'crops.R') contain R6 classes of abstract system states, and they must be given an instance (i.e., have their initially empty parameters mapped to a country, a year, a state of this country's crops, etc.). State files also contain data frames that work like a memory of the different parameter values the state files have had through time, for different countries, and so on. State files are instantiated at country-level by the simulator file. State manager files contain functions that cause, and logic that constrains, changes in state files' parameters as the simulator evolves states time-step by time-step. State manager functions may also contain logic that constrains spatial changes (e.g., cropland area must be smaller than a country's total land area).

The code structure of state and state manager files differs slightly because we do not need to use the full object oriented character for state manager files that R6 classes provide. Specifically, we require that state files produce mutable objects, but state manager file objects need not be mutable. We are defining state managers in terms of R6 objects because this keeps state manager functions from entering the global workspace environment, since they are defined only within their own R6 class environments. Hence, to call a function (and obtain a value), we instantiate a manager file, viz.

``` r
crp_mng <- crop_manager$new()
value   <- crp_mng$manage_harvest_area(...)
```

State files' parameters are either single-valued variables (that may be of numerical or character string type) or data frames that contain vectors of numerical or character string type. The logic that sets variable parameters in state classes allows only for those values to be replaced, so they can never be other than single-valued. The logic that sets data frame parameters in state classes usually appends updated values to a data frame of previously set values, creating a memory of prior system states. Data frame parameters contain "data" in their labels.

Data files generally convert raw data files to structured data frames that may be passed to the state and state manager files. Therefore, data files have the least standard format. To make these as legible as possible, we follow tidy data shaping conventions and make plenty of comments. In the interests of reproducibility, we have tried to use open access data sources and download raw sources to shape within the code itself; however, this is not always possible.

The simulator file sequences modules in order because of dependencies shared between the modules. Running modules out of order, or omitting models unless expressly allowed, may cause fatal errors in the Model or errors in its results. Presently, "modules" are loosely defined (see in-line comments) in the simulator file. These modules start with an object constructor (i.e., instantiates some state and its associated state_manager file), and then uses state manager functions to set parameters in the state files. Each state file contains one or more parameters whose labels include the word "data". Once the state's single-valued variable parameters are set, these are passed to a data parameter, which appends the new values to prior values.

In the simulator file, modules are split into 2 parts, which may be thought of as "supply" and "demand" parts of the module. The supply part of a module constructs a state object and its associated state manager object (e.g., 'crop' and 'crop_manager') and fills the 'data' parameter for each instance (i.e., country, year, etc.). Because humans and livestock compete for crops (food and feed, respectively), crops and livestock compete for land, and so on, the other modules' supply parts must be loaded before demands may be considered. The demand part of a module revisits the total demand for some product (e.g., cereal crops) from humans and animals, and imports or exports to zero deficits or surpluses respectively. In the Model, this is done via trade. Presently, the Model trades only food and feed from crops, and trade products are in effect sourced from, and sent to, a cornucopia of infinite size and riches. This will become a clearinghouse with logic to constrain trade in terms of geographic range (i.e., regionalization, high transport cost, tarrifs, etc.) and strategic (water) or environmental considerations (biodiversity protection, carbon cost, etc.).

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

You must now download and extract data for the Model. Run the 'data_downloader.R' script in your console, i.e., ["~/iep_model/model/R/data/data_downloader.R"]("~/iep_model/model/R/data/data_downloader.R").

You should now have a set of large data files in their own folders nested within your 'data_path' directory. (You will see these listed as .csv files. Unfortunately, the FAO uses inconsistent conventions when structuring their data; so we will treat these .csv files as raw data that will need to be reshaped to be used by the Model. Reshaping will generally be handled automatically by the '_manager.R' files. Manually reshaped files will need to be added separately, in the next step. When you confirm that the files have downloaded and been extracted properly, you can also take the opportunity to delete the .zip versions of the data files.)

## Run the model

Start a new R session. 

## Example

Constructing a country-level object.

``` r
# chn <- country$new()
# chn$set_iso_alpha3_code("CHN")
# chn
```

The R6Class object ‘chn’ is now instantiated as model country China, containing identification codes from the FAO and UN. Extract other features from the ‘chn’ object by calling variable names, such as ‘countryname’ which returns “China”.

``` r
# chn$countryname
```

In the Model, many functions live inside of R6Class objects, and not in the global workspace. You will need to instantiate one of the manager classes, e.g., ‘crops_manager’, to make available any function to do with crop management.

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

## Documentation

The Model presently uses the 'docstring' library for documentation. This makes determining the purpose of the Model's various functions easy.

To find documentation associated with a function inside a class, you must (unfortunately) first instantiate an object with the class, then assign the function to some temporary variable, and then use the '?' operator on the temporary variable. An example follows:

``` r
#the '?' operator from docstring appears to get confused by the '$' pointer operator so an extra step is required
cntry <- country$new()
cntry$set_iso_alpha3("CAN")
tmp_var <- cntry$set_iso_alpha3
?tmp_var
```

## Details

### Data

Delimited data files (e.g., .csv) should be contained within the /data_path directory defined above. R scripts used to load these data into the working environment should be contained within a separate /R directory. (Neither of these directories should be nested within the /iep_model_path directory defined above. You may have noticed that this creates two distinct /R directories, here and nested within the /iep_model_path directory. This is intentional so the nested /R directory stays package-compliant and will become more important in future.)

#### libraries_list.R

Loads the required R libraries into memory for the session.

#### map_data.R

Loads label information for countries and their geographic sub-regions, as defined by the UN/FAO into the working environment from a .csv file ('location_code_map_data.csv'). The .csv file is a manually cleaned and merged data set that maps ordinary country names, alphanumeric ISO Alpha-3, numeric FAO, and numeric UN codes at country-level, and the associated names and numeric codes for the sub-regions that contain them.

### R6 classes

Under package development criteria, each R6 class should have its own separate .R file. This will happen eventually. For the moment, all classes are nested within two .R files: (1) state_classes.R and (2) state_managers.R.

#### state_classes.R

* R6class::country. Base class and unit of analysis from which all others are sub-classed. Contains geographic location and year information.

* R6class::land_use. Sub-class of country. Contains information related to land use type and areal coverage.

* R6class::crop. Sub-class of country. Contains information related to crop productivity, harvest yield, the ratio of arable land allotted to crop production (the remainder being fallowed), as well as how crops are allocated, i.e., as food (for human consumption), feed (for animal consumption), seed, loss/wastage (harvest loss and value-chain losses), industrial processing (e.g., textiles fabrication), and other uses (e.g., bioethanol). Harvest yield values are in tonnes per hectare; land allotment ratios are given in dimensionless values between 0 and 1; allocation values are given in tonnes per year.

* R6class::livestock. Sub-class of country. Contains information related to low-level livestock values, e.g., stock whose primary usage is for dairy or meat products, or "other" for neither, e.g., for hides. Stock values are given in individual animal head count (even for rodents and fowl, which FAO denominates by 1000 head), except for fish, whose stock is given in tonnes; livestock weights are given in terms of herd tropical livestock units (TLU); livestock (crude) growth rate (head per year); and the class contains variables for feed demand in terms of dry matter (tonnes) and crude protein (tonnes). NB: Because livestock serve multiple uses, the Model makes a bit of a fudge here: it uses the R6class::livestock_manager$product_usage function to pass a dimensionless ratio between 0 and 1 to stock_quantity values, based on total national-level stock usages from FAO.


#### state_managers.R