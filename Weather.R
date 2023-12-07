# 0) Loading libraries ---------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  raster, # raster data handling
  terra, # raster data handling
  sf, # vector data handling
  tidyr,# reshape
  dplyr, #wrangling
  ggplot2, # making maps
  readxl,# handling excel files
  lmtest, #for coeftest
  sandwich, #for vcovHC
  rgeos, #geometric operations
  fixest, #regressions with fixed effects
  gridExtra, #combining plots
  lubridate, #for dates
  broom, #for the augmennt function
  quantreg, #quantile regressions
  geosphere, #distance between cities
  zoo, # fot the na approx function
  stagg
  )

# 1) Loading data --------------------------------------------------------------

#SPEI
r <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei12.nc")

#Population
population_weights <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/gpw_v4_population_density_rev11_2010_30_min.tif")


#MY boundary
MY_sf <-
  st_read("c:/Users/samue/Desktop/Dissertation/Migration/Data/gadm41_MYS_shp/gadm41_MYS_1.shp") %>%
  st_as_sf() %>%
  st_transform(terra::crs(r))

#Formatting name of Terengganu state
MY_sf$NAME_1 <- ifelse(MY_sf$NAME_1 == "Trengganu", "Terengganu", MY_sf$NAME_1)
MY_sf$NAME_1[MY_sf$NAME_1 == "\t\nTerengganu"] <- "Terengganu"

#Loading internal migration flow data
migration <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/Migration.xlsx")

#Year of the report in the excel files -> documented year is the preceding year
migration$year = migration$year - 1

#Removing migration to the same state. Removing years 2006 and 2007 because Putrajaya was part of Selangor in the data.
migration <- migration %>%
  filter(origin != destination,
         year >= 2010 & year < 2020)


#Loading cities data
cities <- read.csv("c:/Users/samue/Desktop/Dissertation/Migration/Data/MY_cities.csv")

#Population 
#opening excel file
pop <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/MY - pop.xlsx")

#Merging with migration dataset
migration <- migration %>%
  left_join(pop, by = c("origin" = "state", "year"))


# 2) Weather data --------------------------------------------------------------

#SPEI - 24 ---------------------------------------------------------------------

r <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei24.nc")

#Caclulating State averages
#Cropping raster layers
MY_r <- terra::crop(r, MY_sf)

#Converting to Spatvector
MY_sv <- vect(MY_sf)

#Extracting values from the raster for each state
SPEI_by_state <- terra::extract(MY_r, MY_sv, fun = "mean", na.rm = TRUE) 

# SPEI_by_state2 <- terra::extract(MY_r, MY_sv, fun = "mean", weights = TRUE, na.rm = TRUE)
# rownames(SPEI_by_state2) <- MY_sf$NAME_1


#Deleting ID column
SPEI_by_state <- select(SPEI_by_state, -ID)

#Changing row names to states.
rownames(SPEI_by_state) <- MY_sf$NAME_1

#Inverting columns and rows
SPEI_by_state <- SPEI_by_state %>%
  t() %>%
  as.data.frame() 

#Adding date column
start_date <- as.Date("1901-1-1")
end_date <- as.Date("2022-12-1")
dates <- seq(start_date, end_date, by = "month")
dates <- format(dates, "%Y/%m")

SPEI_by_state <- SPEI_by_state %>%
  mutate(date = dates) %>%
  relocate(date, .before = 1) 

row.names(SPEI_by_state) <- NULL # resetting row names to initial values.

#Splitting years and months
SPEI_by_state <- SPEI_by_state %>%
  separate(date, into = c("year", "month"), sep = "/")

#Lagging the years by 6 months. In the migration survey, migration flows are from
#July to June next year.
SPEI_by_state <- SPEI_by_state %>%
  mutate(year = as.integer(year), month = as.integer(month)) %>%
  mutate(year = ifelse(month <= 6, year - 1, year))

#Yearly SPEI calculation ---- Keeping the 12 month June SPEI
SPEI_by_state <- SPEI_by_state %>%
  filter(month == 6,
         year >= 2000 & year < 2020) %>%
  select(- month)

#Adding SPEI at origin state to migration dataset 
#Transformation to long format
SPEI_by_state <- SPEI_by_state %>%
  gather(key = "state", value = "SPEI24", -year)


# Convert 'year' in spi_long to numeric
SPEI_by_state <- SPEI_by_state %>%
  mutate(year = as.numeric(year))

#Merge datasets
migration <- migration %>%
  left_join(SPEI_by_state, by = c("origin" = "state", "year"))

#Population weighted -----

#Cropping the raster
MY_weights <- terra::crop(population_weights, MY_sf)
grids_by_state <- terra::extract(MY_r, MY_sv, na.rm = TRUE)

#Extracting density values
weights_by_state <- terra::extract(MY_weights, MY_sv, na.rm = TRUE, cells = TRUE) 

#Calculating weights
weights_by_state <- weights_by_state %>%
  group_by(ID) %>%
  mutate(gpw_v4_population_density_rev11_2010_30_min = ifelse(cell == 36 & ID == 13, 0, gpw_v4_population_density_rev11_2010_30_min),
         weights = gpw_v4_population_density_rev11_2010_30_min / sum(gpw_v4_population_density_rev11_2010_30_min)) %>%
  ungroup() %>%
  select(-gpw_v4_population_density_rev11_2010_30_min, -ID, -cell)


grids_by_state$weights <- weights_by_state$weights

#Calculating weighted average
grids_by_state <- grids_by_state %>%
  mutate_at(vars(starts_with("spei")), funs(. * weights)) %>%
  group_by(ID) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  select(-ID, -weights)

#Changing row names to states.
rownames(grids_by_state) <- MY_sf$NAME_1

#Inverting columns and rows
grids_by_state <- grids_by_state %>%
  t() %>%
  as.data.frame() 

#Adding date column
start_date <- as.Date("1901-1-1")
end_date <- as.Date("2022-12-1")
dates <- seq(start_date, end_date, by = "month")
dates <- format(dates, "%Y/%m")

grids_by_state <- grids_by_state %>%
  mutate(date = dates) %>%
  relocate(date, .before = 1) 

row.names(grids_by_state) <- NULL # resetting row names to initial values.

#Splitting years and months
grids_by_state <- grids_by_state %>%
  separate(date, into = c("year", "month"), sep = "/")

#Lagging the years by 6 months. In the migration survey, migration flows are from
#July to June next year.
grids_by_state <- grids_by_state %>%
  mutate(year = as.integer(year), month = as.integer(month)) %>%
  mutate(year = ifelse(month <= 6, year - 1, year))

#Yearly SPEI calculation ---- Keeping the 12 month June SPEI
grids_by_state <- grids_by_state %>%
  filter(month == 6,
         year >= 2000 & year < 2020) %>%
  select(- month)

#Adding SPEI at origin state to migration dataset 
#Transformation to long format
grids_by_state <- grids_by_state %>%
  gather(key = "state", value = "SPEI24pop", -year)


# Convert 'year' in spi_long to numeric
grids_by_state <- grids_by_state %>%
  mutate(year = as.numeric(year),
         SPEI24pop = as.numeric(SPEI24pop))

#Merge datasets
migration <- migration %>%
  left_join(grids_by_state, by = c("origin" = "state", "year"))



#SPEI - 04 ---------------------------------------------------------------------

r <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei04.nc")

#Caclulating State averages
#Cropping raster layers
MY_r <- terra::crop(r, MY_sf)

#Converting to Spatvector
MY_sv <- vect(MY_sf)

#Extracting values from the raster for each state
SPEI_by_state <- terra::extract(MY_r, MY_sv, fun = "mean", na.rm = TRUE) 

# SPEI_by_state2 <- terra::extract(MY_r, MY_sv, fun = "mean", weights = TRUE, na.rm = TRUE)
# rownames(SPEI_by_state2) <- MY_sf$NAME_1


#Deleting ID column
SPEI_by_state <- select(SPEI_by_state, -ID)

#Changing row names to states.
rownames(SPEI_by_state) <- MY_sf$NAME_1

#Inverting columns and rows
SPEI_by_state <- SPEI_by_state %>%
  t() %>%
  as.data.frame() 

#Adding date column
start_date <- as.Date("1901-1-1")
end_date <- as.Date("2022-12-1")
dates <- seq(start_date, end_date, by = "month")
dates <- format(dates, "%Y/%m")

SPEI_by_state <- SPEI_by_state %>%
  mutate(date = dates) %>%
  relocate(date, .before = 1) 

row.names(SPEI_by_state) <- NULL # resetting row names to initial values.

#Splitting years and months
SPEI_by_state <- SPEI_by_state %>%
  separate(date, into = c("year", "month"), sep = "/")

#Lagging the years by 6 months. In the migration survey, migration flows are from
#July to June next year.
SPEI_by_state <- SPEI_by_state %>%
  mutate(year = as.integer(year), month = as.integer(month)) %>%
  mutate(year = ifelse(month <= 6, year - 1, year))

#Yearly SPEI calculation ---- Keeping the 12 month June SPEI
SPEI_by_state <- SPEI_by_state %>%
  filter(month == 1,
         year >= 2000 & year < 2020) %>%
  select(- month)

#Adding SPEI at origin state to migration dataset 
#Transformation to long format
SPEI_by_state <- SPEI_by_state %>%
  gather(key = "state", value = "SPEI04", -year)


# Convert 'year' in spi_long to numeric
SPEI_by_state <- SPEI_by_state %>%
  mutate(year = as.numeric(year))

#Merge datasets
migration <- migration %>%
  left_join(SPEI_by_state, by = c("origin" = "state", "year"))

#Population weighted -----

#Cropping the raster
MY_weights <- terra::crop(population_weights, MY_sf)
grids_by_state <- terra::extract(MY_r, MY_sv, na.rm = TRUE)

#Extracting density values
weights_by_state <- terra::extract(MY_weights, MY_sv, na.rm = TRUE, cells = TRUE) 

#Calculating weights
weights_by_state <- weights_by_state %>%
  group_by(ID) %>%
  mutate(gpw_v4_population_density_rev11_2010_30_min = ifelse(cell == 36 & ID == 13, 0, gpw_v4_population_density_rev11_2010_30_min),
         weights = gpw_v4_population_density_rev11_2010_30_min / sum(gpw_v4_population_density_rev11_2010_30_min)) %>%
  ungroup() %>%
  select(-gpw_v4_population_density_rev11_2010_30_min, -ID, -cell)


grids_by_state$weights <- weights_by_state$weights

#Calculating weighted average
grids_by_state <- grids_by_state %>%
  mutate_at(vars(starts_with("spei")), funs(. * weights)) %>%
  group_by(ID) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  select(-ID, -weights)

#Changing row names to states.
rownames(grids_by_state) <- MY_sf$NAME_1

#Inverting columns and rows
grids_by_state <- grids_by_state %>%
  t() %>%
  as.data.frame() 

#Adding date column
start_date <- as.Date("1901-1-1")
end_date <- as.Date("2022-12-1")
dates <- seq(start_date, end_date, by = "month")
dates <- format(dates, "%Y/%m")

grids_by_state <- grids_by_state %>%
  mutate(date = dates) %>%
  relocate(date, .before = 1) 

row.names(grids_by_state) <- NULL # resetting row names to initial values.

#Splitting years and months
grids_by_state <- grids_by_state %>%
  separate(date, into = c("year", "month"), sep = "/")

#Lagging the years by 6 months. In the migration survey, migration flows are from
#July to June next year.
grids_by_state <- grids_by_state %>%
  mutate(year = as.integer(year), month = as.integer(month)) %>%
  mutate(year = ifelse(month <= 6, year - 1, year))

#Yearly SPEI calculation ---- Keeping the 12 month June SPEI
grids_by_state <- grids_by_state %>%
  filter(month == 1,
         year >= 2000 & year < 2020) %>%
  select(- month)

#Adding SPEI at origin state to migration dataset 
#Transformation to long format
grids_by_state <- grids_by_state %>%
  gather(key = "state", value = "SPEI04pop", -year)


# Convert 'year' in spi_long to numeric
grids_by_state <- grids_by_state %>%
  mutate(year = as.numeric(year),
         SPEI04pop = as.numeric(SPEI04pop))


#Merge datasets
migration <- migration %>%
  left_join(grids_by_state, by = c("origin" = "state", "year"))

#Lags --------------------------------------------------------------------------
# First, create two versions of SPEI_by_state with the lags
SPEI_by_state_lag1 <- SPEI_by_state %>%
  mutate(year = year + 1) %>%  # Shift the year by 1 to get the previous year's SPEI
  rename(SPEI04_lag1 = SPEI04)


grids_by_state_lag1 <- grids_by_state %>%
  mutate(year = year + 1) %>%  # Shift the year by 1 to get the previous year's SPEI
  rename(SPEI04pop_lag1 = SPEI04pop)

# Now, left join the migration_data with SPEI_by_state_lag1 and SPEI_by_state_lag2
migration <- migration %>%
  left_join(SPEI_by_state_lag1, by = c("year", "origin" = "state")) %>%
  left_join(grids_by_state_lag1, by = c("year", "origin" = "state"))


#SPEI - 04 ---------------------------------------------------------------------

r <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei04.nc")

#Caclulating State averages
#Cropping raster layers
MY_r <- terra::crop(r, MY_sf)

#Converting to Spatvector
MY_sv <- vect(MY_sf)

#Extracting values from the raster for each state
SPEI_by_state <- terra::extract(MY_r, MY_sv, fun = "mean", na.rm = TRUE) 

# SPEI_by_state2 <- terra::extract(MY_r, MY_sv, fun = "mean", weights = TRUE, na.rm = TRUE)
# rownames(SPEI_by_state2) <- MY_sf$NAME_1


#Deleting ID column
SPEI_by_state <- select(SPEI_by_state, -ID)

#Changing row names to states.
rownames(SPEI_by_state) <- MY_sf$NAME_1

#Inverting columns and rows
SPEI_by_state <- SPEI_by_state %>%
  t() %>%
  as.data.frame() 

#Adding date column
start_date <- as.Date("1901-1-1")
end_date <- as.Date("2022-12-1")
dates <- seq(start_date, end_date, by = "month")
dates <- format(dates, "%Y/%m")

SPEI_by_state <- SPEI_by_state %>%
  mutate(date = dates) %>%
  relocate(date, .before = 1) 

row.names(SPEI_by_state) <- NULL # resetting row names to initial values.

#Splitting years and months
SPEI_by_state <- SPEI_by_state %>%
  separate(date, into = c("year", "month"), sep = "/")

#Lagging the years by 6 months. In the migration survey, migration flows are from
#July to June next year.
SPEI_by_state <- SPEI_by_state %>%
  mutate(year = as.integer(year), month = as.integer(month)) %>%
  mutate(year = ifelse(month <= 6, year - 1, year))

#Yearly SPEI calculation ---- Keeping the 12 month June SPEI
SPEI_by_state <- SPEI_by_state %>%
  filter(month == 6,
         year >= 2000 & year < 2020) %>%
  select(- month)

#Adding SPEI at origin state to migration dataset 
#Transformation to long format
SPEI_by_state <- SPEI_by_state %>%
  gather(key = "state", value = "SPEI04v2", -year)


# Convert 'year' in spi_long to numeric
SPEI_by_state <- SPEI_by_state %>%
  mutate(year = as.numeric(year))

#Merge datasets
migration <- migration %>%
  left_join(SPEI_by_state, by = c("origin" = "state", "year"))

#Population weighted -----

#Cropping the raster
MY_weights <- terra::crop(population_weights, MY_sf)
grids_by_state <- terra::extract(MY_r, MY_sv, na.rm = TRUE)

#Extracting density values
weights_by_state <- terra::extract(MY_weights, MY_sv, na.rm = TRUE, cells = TRUE) 

#Calculating weights
weights_by_state <- weights_by_state %>%
  group_by(ID) %>%
  mutate(gpw_v4_population_density_rev11_2010_30_min = ifelse(cell == 36 & ID == 13, 0, gpw_v4_population_density_rev11_2010_30_min),
         weights = gpw_v4_population_density_rev11_2010_30_min / sum(gpw_v4_population_density_rev11_2010_30_min)) %>%
  ungroup() %>%
  select(-gpw_v4_population_density_rev11_2010_30_min, -ID, -cell)


grids_by_state$weights <- weights_by_state$weights

#Calculating weighted average
grids_by_state <- grids_by_state %>%
  mutate_at(vars(starts_with("spei")), funs(. * weights)) %>%
  group_by(ID) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  select(-ID, -weights)

#Changing row names to states.
rownames(grids_by_state) <- MY_sf$NAME_1

#Inverting columns and rows
grids_by_state <- grids_by_state %>%
  t() %>%
  as.data.frame() 

#Adding date column
start_date <- as.Date("1901-1-1")
end_date <- as.Date("2022-12-1")
dates <- seq(start_date, end_date, by = "month")
dates <- format(dates, "%Y/%m")

grids_by_state <- grids_by_state %>%
  mutate(date = dates) %>%
  relocate(date, .before = 1) 

row.names(grids_by_state) <- NULL # resetting row names to initial values.

#Splitting years and months
grids_by_state <- grids_by_state %>%
  separate(date, into = c("year", "month"), sep = "/")

#Lagging the years by 6 months. In the migration survey, migration flows are from
#July to June next year.
grids_by_state <- grids_by_state %>%
  mutate(year = as.integer(year), month = as.integer(month)) %>%
  mutate(year = ifelse(month <= 6, year - 1, year))

#Yearly SPEI calculation ---- Keeping the 12 month June SPEI
grids_by_state <- grids_by_state %>%
  filter(month == 6,
         year >= 2000 & year < 2020) %>%
  select(- month)

#Adding SPEI at origin state to migration dataset 
#Transformation to long format
grids_by_state <- grids_by_state %>%
  gather(key = "state", value = "SPEI04v2pop", -year)


# Convert 'year' in spi_long to numeric
grids_by_state <- grids_by_state %>%
  mutate(year = as.numeric(year),
         SPEI04v2pop = as.numeric(SPEI04v2pop))


#Merge datasets
migration <- migration %>%
  left_join(grids_by_state, by = c("origin" = "state", "year"))

#Lags --------------------------------------------------------------------------
# First, create two versions of SPEI_by_state with the lags
SPEI_by_state_lag1 <- SPEI_by_state %>%
  mutate(year = year + 1) %>%  # Shift the year by 1 to get the previous year's SPEI
  rename(SPEI04v2_lag1 = SPEI04v2)


grids_by_state_lag1 <- grids_by_state %>%
  mutate(year = year + 1) %>%  # Shift the year by 1 to get the previous year's SPEI
  rename(SPEI04v2pop_lag1 = SPEI04v2pop)

# Now, left join the migration_data with SPEI_by_state_lag1 and SPEI_by_state_lag2
migration <- migration %>%
  left_join(SPEI_by_state_lag1, by = c("year", "origin" = "state")) %>%
  left_join(grids_by_state_lag1, by = c("year", "origin" = "state"))


# SPEI 12 by state -------------------------------------------------------------

r <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei12.nc")

#Caclulating State averages
#Cropping raster layers
MY_r <- terra::crop(r, MY_sf)

#Converting to Spatvector
MY_sv <- vect(MY_sf)

#Extracting values from the raster for each state
SPEI_by_state <- terra::extract(MY_r, MY_sv, fun = "mean", na.rm = TRUE) 

# SPEI_by_state2 <- terra::extract(MY_r, MY_sv, fun = "mean", weights = TRUE, na.rm = TRUE)
# rownames(SPEI_by_state2) <- MY_sf$NAME_1


#Deleting ID column
SPEI_by_state <- select(SPEI_by_state, -ID)

#Changing row names to states.
rownames(SPEI_by_state) <- MY_sf$NAME_1

#Inverting columns and rows
SPEI_by_state <- SPEI_by_state %>%
  t() %>%
  as.data.frame() 

#Adding date column
start_date <- as.Date("1901-1-1")
end_date <- as.Date("2022-12-1")
dates <- seq(start_date, end_date, by = "month")
dates <- format(dates, "%Y/%m")

SPEI_by_state <- SPEI_by_state %>%
  mutate(date = dates) %>%
  relocate(date, .before = 1) 

row.names(SPEI_by_state) <- NULL # resetting row names to initial values.

#Splitting years and months
SPEI_by_state <- SPEI_by_state %>%
  separate(date, into = c("year", "month"), sep = "/")

#Lagging the years by 6 months. In the migration survey, migration flows are from
#July to June next year.
SPEI_by_state <- SPEI_by_state %>%
  mutate(year = as.integer(year), month = as.integer(month)) %>%
  mutate(year = ifelse(month <= 6, year - 1, year))

#Yearly SPEI calculation ---- Keeping the 12 month June SPEI
SPEI_by_state <- SPEI_by_state %>%
  filter(month == 3,
         year >= 2000 & year < 2020) %>%
  select(- month)

#Adding SPEI at origin state to migration dataset 
#Transformation to long format
SPEI_by_state <- SPEI_by_state %>%
  gather(key = "state", value = "SPEI", -year)


# Convert 'year' in spi_long to numeric
SPEI_by_state <- SPEI_by_state %>%
  mutate(year = as.numeric(year))

#Merge datasets
migration <- migration %>%
  left_join(SPEI_by_state, by = c("origin" = "state", "year"))

#Population weighted -----

#Cropping the raster
MY_weights <- terra::crop(population_weights, MY_sf)
grids_by_state <- terra::extract(MY_r, MY_sv, na.rm = TRUE)

#Extracting density values
weights_by_state <- terra::extract(MY_weights, MY_sv, na.rm = TRUE, cells = TRUE) 

#Calculating weights
weights_by_state <- weights_by_state %>%
  group_by(ID) %>%
  mutate(gpw_v4_population_density_rev11_2010_30_min = ifelse(cell == 36 & ID == 13, 0, gpw_v4_population_density_rev11_2010_30_min),
         weights = gpw_v4_population_density_rev11_2010_30_min / sum(gpw_v4_population_density_rev11_2010_30_min)) %>%
  ungroup() %>%
  select(-gpw_v4_population_density_rev11_2010_30_min, -ID, -cell)


grids_by_state$weights <- weights_by_state$weights

#Calculating weighted average
grids_by_state <- grids_by_state %>%
  mutate_at(vars(starts_with("spei")), funs(. * weights)) %>%
  group_by(ID) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  select(-ID, -weights)

#Changing row names to states.
rownames(grids_by_state) <- MY_sf$NAME_1

#Inverting columns and rows
grids_by_state <- grids_by_state %>%
  t() %>%
  as.data.frame() 

#Adding date column
start_date <- as.Date("1901-1-1")
end_date <- as.Date("2022-12-1")
dates <- seq(start_date, end_date, by = "month")
dates <- format(dates, "%Y/%m")

grids_by_state <- grids_by_state %>%
  mutate(date = dates) %>%
  relocate(date, .before = 1) 

row.names(grids_by_state) <- NULL # resetting row names to initial values.

#Splitting years and months
grids_by_state <- grids_by_state %>%
  separate(date, into = c("year", "month"), sep = "/")

#Lagging the years by 6 months. In the migration survey, migration flows are from
#July to June next year.
grids_by_state <- grids_by_state %>%
  mutate(year = as.integer(year), month = as.integer(month)) %>%
  mutate(year = ifelse(month <= 6, year - 1, year))

#Yearly SPEI calculation ---- Keeping the 12 month June SPEI
grids_by_state <- grids_by_state %>%
  filter(month == 6,
         year >= 2000 & year < 2020) %>%
  select(- month)

#Adding SPEI at origin state to migration dataset 
#Transformation to long format
grids_by_state <- grids_by_state %>%
  gather(key = "state", value = "SPEIpop", -year)


# Convert 'year' in spi_long to numeric
grids_by_state <- grids_by_state %>%
  mutate(year = as.numeric(year),
         SPEIpop = as.numeric(SPEIpop))

#Merge datasets
migration <- migration %>%
  left_join(grids_by_state, by = c("origin" = "state", "year"))

#Lags --------------------------------------------------------------------------
# First, create two versions of SPEI_by_state with the lags
SPEI_by_state_lag1 <- SPEI_by_state %>%
  mutate(year = year + 1) %>%  # Shift the year by 1 to get the previous year's SPEI
  rename(SPEI_lag1 = SPEI)


grids_by_state_lag1 <- grids_by_state %>%
  mutate(year = year + 1) %>%  # Shift the year by 1 to get the previous year's SPEI
  rename(SPEIpop_lag1 = SPEIpop)

# Now, left join the migration_data with SPEI_by_state_lag1 and SPEI_by_state_lag2
migration <- migration %>%
  left_join(SPEI_by_state_lag1, by = c("year", "origin" = "state")) %>%
  left_join(grids_by_state_lag1, by = c("year", "origin" = "state"))
            

# 2.2) Weather variables -------------------------------------------------------

#Piecewise linear regressions ----
migration <- migration %>%
  mutate(SPEI = as.numeric(SPEI),
         SPEIpop = as.numeric(SPEIpop),
         SPEI24pop = as.numeric(SPEI24pop),
         SPEI04pop = as.numeric(SPEI04pop),
         SPEI04v2pop = as.numeric(SPEI04v2pop),
         SPEI_lag1 = as.numeric(SPEI_lag1),
         SPEIpop_lag1 = as.numeric(SPEIpop_lag1),
         SPEI04pop_lag1 = as.numeric(SPEI04pop_lag1),
         SPEI04v2pop_lag1 = as.numeric(SPEI04v2pop_lag1),
         SPEI_droughts_0.5 = ifelse(SPEI <= -0.5, SPEI + 0.5, 0),
         SPEI_floods_0.5 = ifelse(SPEI >= 0.5, SPEI - 0.5, 0),
         SPEI_droughts_1 = ifelse(SPEI <= -1, SPEI + 1, 0),
         SPEI_floods_1 = ifelse(SPEI >= 1, SPEI - 1, 0),
         SPEI_droughts_1.5 = ifelse(SPEI <= -1.5, SPEI + 1.5, 0),
         SPEI_floods_1.5 = ifelse(SPEI >= 1.5, SPEI - 1.5, 0),
         SPEI_droughts_2 = ifelse(SPEI <= -2, SPEI + 2, 0),
         SPEI_floods_2 = ifelse(SPEI >= 2, SPEI - 2, 0),
         SPEI_droughts_0.5_lag1 = ifelse(SPEI_lag1 <= -0.5, SPEI_lag1 + 0.5, 0),
         SPEI_floods_0.5_lag1 = ifelse(SPEI_lag1 >= 0.5, SPEI_lag1 - 0.5, 0),
         SPEI_droughts_1_lag1 = ifelse(SPEI_lag1 <= -1, SPEI_lag1 + 1, 0),
         SPEI_floods_1_lag1 = ifelse(SPEI_lag1 >= 1, SPEI_lag1 - 1, 0),
         SPEI_droughts_1.5_lag1 = ifelse(SPEI_lag1 <= -1.5, SPEI_lag1 + 1.5, 0),
         SPEI_floods_1.5_lag1 = ifelse(SPEI_lag1 >= 1.5, SPEI_lag1 - 1.5, 0),
         SPEI_droughts_2_lag1 = ifelse(SPEI_lag1 <= -2, SPEI_lag1 + 2, 0),
         SPEI_floods_2_lag1 = ifelse(SPEI_lag1 >= 2, SPEI_lag1 - 2, 0),
         SPEIpop_droughts_0.5 = ifelse(SPEIpop <= -0.5, SPEIpop + 0.5, 0),
         SPEIpop_floods_0.5 = ifelse(SPEIpop >= 0.5, SPEIpop - 0.5, 0),
         SPEIpop_droughts_1 = ifelse(SPEIpop <= -1, SPEIpop + 1, 0),
         SPEIpop_floods_1 = ifelse(SPEIpop >= 1, SPEIpop - 1, 0),
         SPEIpop_droughts_1.5 = ifelse(SPEIpop <= -1.5, SPEIpop + 1.5, 0),
         SPEIpop_floods_1.5 = ifelse(SPEIpop >= 1.5, SPEIpop - 1.5, 0),
         SPEIpop_droughts_2 = ifelse(SPEIpop <= -2, SPEIpop + 2, 0),
         SPEIpop_floods_2 = ifelse(SPEIpop >= 2, SPEIpop - 2, 0),
         SPEIpop_droughts_0.5_lag1 = ifelse(SPEIpop_lag1 <= -0.5, SPEIpop_lag1 + 0.5, 0),
         SPEIpop_floods_0.5_lag1 = ifelse(SPEIpop_lag1 >= 0.5, SPEIpop_lag1 - 0.5, 0),
         SPEIpop_droughts_1_lag1 = ifelse(SPEIpop_lag1 <= -1, SPEIpop_lag1 + 1, 0),
         SPEIpop_floods_1_lag1 = ifelse(SPEIpop_lag1 >= 1, SPEIpop_lag1 - 1, 0),
         SPEIpop_droughts_1.5_lag1 = ifelse(SPEIpop_lag1 <= -1.5, SPEIpop_lag1 + 1.5, 0),
         SPEIpop_floods_1.5_lag1 = ifelse(SPEIpop_lag1 >= 1.5, SPEIpop_lag1 - 1.5, 0),
         SPEIpop_droughts_2_lag1 = ifelse(SPEIpop_lag1 <= -2, SPEIpop_lag1 + 2, 0),
         SPEIpop_floods_2_lag1 = ifelse(SPEIpop_lag1 >= 2, SPEIpop_lag1 - 2, 0),
         SPEI24_droughts_0.5 = ifelse(SPEI24 <= -0.5, SPEI24 + 0.5, 0),
         SPEI24_floods_0.5 = ifelse(SPEI24 >= 0.5, SPEI24 - 0.5, 0),
         SPEI24_droughts_1 = ifelse(SPEI24 <= -1, SPEI24 + 1, 0),
         SPEI24_floods_1 = ifelse(SPEI24 >= 1, SPEI24 - 1, 0),
         SPEI24_droughts_1.5 = ifelse(SPEI24 <= -1.5, SPEI24 + 1.5, 0),
         SPEI24_floods_1.5 = ifelse(SPEI24 >= 1.5, SPEI24 - 1.5, 0),
         SPEI24_droughts_2 = ifelse(SPEI24 <= -2, SPEI24 + 2, 0),
         SPEI24_floods_2 = ifelse(SPEI24 >= 2, SPEI24 - 2, 0),
         SPEI24pop_droughts_0.5 = ifelse(SPEI24pop <= -0.5, SPEI24pop + 0.5, 0),
         SPEI24pop_floods_0.5 = ifelse(SPEI24pop >= 0.5, SPEI24pop - 0.5, 0),
         SPEI24pop_droughts_1 = ifelse(SPEI24pop <= -1, SPEI24pop + 1, 0),
         SPEI24pop_floods_1 = ifelse(SPEI24pop >= 1, SPEI24pop - 1, 0),
         SPEI24pop_droughts_1.5 = ifelse(SPEI24pop <= -1.5, SPEI24pop + 1.5, 0),
         SPEI24pop_floods_1.5 = ifelse(SPEI24pop >= 1.5, SPEI24pop - 1.5, 0),
         SPEI24pop_droughts_2 = ifelse(SPEI24pop <= -2, SPEI24pop + 2, 0),
         SPEI24pop_floods_2 = ifelse(SPEI24pop >= 2, SPEI24pop - 2, 0),
         SPEI04_droughts_0.5 = ifelse(SPEI04 <= -0.5, SPEI04 + 0.5, 0),
         SPEI04_floods_0.5 = ifelse(SPEI04 >= 0.5, SPEI04 - 0.5, 0),
         SPEI04_droughts_1 = ifelse(SPEI04 <= -1, SPEI04 + 1, 0),
         SPEI04_floods_1 = ifelse(SPEI04 >= 1, SPEI04 - 1, 0),
         SPEI04_droughts_1.5 = ifelse(SPEI04 <= -1.5, SPEI04 + 1.5, 0),
         SPEI04_floods_1.5 = ifelse(SPEI04 >= 1.5, SPEI04 - 1.5, 0),
         SPEI04_droughts_2 = ifelse(SPEI04 <= -2, SPEI04 + 2, 0),
         SPEI04_floods_2 = ifelse(SPEI04 >= 2, SPEI04 - 2, 0),
         SPEI04pop_droughts_0.5 = ifelse(SPEI04pop <= -0.5, SPEI04pop + 0.5, 0),
         SPEI04pop_floods_0.5 = ifelse(SPEI04pop >= 0.5, SPEI04pop - 0.5, 0),
         SPEI04pop_droughts_1 = ifelse(SPEI04pop <= -1, SPEI04pop + 1, 0),
         SPEI04pop_floods_1 = ifelse(SPEI04pop >= 1, SPEI04pop - 1, 0),
         SPEI04pop_droughts_1.5 = ifelse(SPEI04pop <= -1.5, SPEI04pop + 1.5, 0),
         SPEI04pop_floods_1.5 = ifelse(SPEI04pop >= 1.5, SPEI04pop - 1.5, 0),
         SPEI04pop_droughts_2 = ifelse(SPEI04pop <= -2, SPEI04pop + 2, 0),
         SPEI04pop_floods_2 = ifelse(SPEI04pop >= 2, SPEI04pop - 2, 0),
         SPEI04v2_droughts_0.5 = ifelse(SPEI04v2 <= -0.5, SPEI04v2 + 0.5, 0),
         SPEI04v2_floods_0.5 = ifelse(SPEI04v2 >= 0.5, SPEI04v2 - 0.5, 0),
         SPEI04v2_droughts_1 = ifelse(SPEI04v2 <= -1, SPEI04v2 + 1, 0),
         SPEI04v2_floods_1 = ifelse(SPEI04v2 >= 1, SPEI04v2 - 1, 0),
         SPEI04v2_droughts_1.5 = ifelse(SPEI04v2 <= -1.5, SPEI04v2 + 1.5, 0),
         SPEI04v2_floods_1.5 = ifelse(SPEI04v2 >= 1.5, SPEI04v2 - 1.5, 0),
         SPEI04v2_droughts_2 = ifelse(SPEI04v2 <= -2, SPEI04v2 + 2, 0),
         SPEI04v2_floods_2 = ifelse(SPEI04v2 >= 2, SPEI04v2 - 2, 0),
         SPEI04v2pop_droughts_0.5 = ifelse(SPEI04v2pop <= -0.5, SPEI04v2pop + 0.5, 0),
         SPEI04v2pop_floods_0.5 = ifelse(SPEI04v2pop >= 0.5, SPEI04v2pop - 0.5, 0),
         SPEI04v2pop_droughts_1 = ifelse(SPEI04v2pop <= -1, SPEI04v2pop + 1, 0),
         SPEI04v2pop_floods_1 = ifelse(SPEI04v2pop >= 1, SPEI04v2pop - 1, 0),
         SPEI04v2pop_droughts_1.5 = ifelse(SPEI04v2pop <= -1.5, SPEI04v2pop + 1.5, 0),
         SPEI04v2pop_floods_1.5 = ifelse(SPEI04v2pop >= 1.5, SPEI04v2pop - 1.5, 0),
         SPEI04v2pop_droughts_2 = ifelse(SPEI04v2pop <= -2, SPEI04v2pop + 2, 0),
         SPEI04v2pop_floods_2 = ifelse(SPEI04v2pop >= 2, SPEI04v2pop - 2, 0),
         SPEI04pop_droughts_0.5_lag1 = ifelse(SPEI04pop_lag1 <= -0.5, SPEI04pop_lag1 + 0.5, 0),
         SPEI04pop_floods_0.5_lag1 = ifelse(SPEI04pop_lag1 >= 0.5, SPEI04pop_lag1 - 0.5, 0),
         SPEI04pop_droughts_1_lag1 = ifelse(SPEI04pop_lag1 <= -1, SPEI04pop_lag1 + 1, 0),
         SPEI04pop_floods_1_lag1 = ifelse(SPEI04pop_lag1 >= 1, SPEI04pop_lag1 - 1, 0),
         SPEI04pop_droughts_1.5_lag1 = ifelse(SPEI04pop_lag1 <= -1.5, SPEI04pop_lag1 + 1.5, 0),
         SPEI04pop_floods_1.5_lag1 = ifelse(SPEI04pop_lag1 >= 1.5, SPEI04pop_lag1 - 1.5, 0),
         SPEI04pop_droughts_2_lag1 = ifelse(SPEI04pop_lag1 <= -2, SPEI04pop_lag1 + 2, 0),
         SPEI04pop_floods_2_lag1 = ifelse(SPEI04pop_lag1 >= 2, SPEI04pop_lag1 - 2, 0),
         SPEI04v2pop_droughts_0.5_lag1 = ifelse(SPEI04v2pop_lag1 <= -0.5, SPEI04v2pop_lag1 + 0.5, 0),
         SPEI04v2pop_floods_0.5_lag1 = ifelse(SPEI04v2pop_lag1 >= 0.5, SPEI04v2pop_lag1 - 0.5, 0),
         SPEI04v2pop_droughts_1_lag1 = ifelse(SPEI04v2pop_lag1 <= -1, SPEI04v2pop_lag1 + 1, 0),
         SPEI04v2pop_floods_1_lag1 = ifelse(SPEI04v2pop_lag1 >= 1, SPEI04v2pop_lag1 - 1, 0),
         SPEI04v2pop_droughts_1.5_lag1 = ifelse(SPEI04v2pop_lag1 <= -1.5, SPEI04v2pop_lag1 + 1.5, 0),
         SPEI04v2pop_floods_1.5_lag1 = ifelse(SPEI04v2pop_lag1 >= 1.5, SPEI04v2pop_lag1 - 1.5, 0),
         SPEI04v2pop_droughts_2_lag1 = ifelse(SPEI04v2pop_lag1 <= -2, SPEI04v2pop_lag1 + 2, 0),
         SPEI04v2pop_floods_2_lag1 = ifelse(SPEI04v2pop_lag1 >= 2, SPEI04v2pop_lag1 - 2, 0))

# 3) Geographical data --------------------------------------------

#Distance between states--------------------------------------------------------
#Converting geometries of the states into point geometries
points_MY <- st_centroid(MY_sf)

# Transform the data frame to an appropriate CRS for geodesic calculations
points_MY <- st_transform(points_MY, "+proj=longlat +datum=WGS84")

#Calculating distance matrix between the points
dist_matrix <- st_distance(points_MY)

dist_matrix <- dist_matrix %>%
  as.data.frame()

rownames(dist_matrix) <- MY_sf$NAME_1
colnames(dist_matrix) <- MY_sf$NAME_1

#Wide to long transformation
dist_matrix$state <- row.names(dist_matrix)

dist_matrix_long <- dist_matrix %>%
  tidyr::pivot_longer(cols = -state, names_to = "destination", values_to = "distance") %>%
  filter(state != destination)

#Adding distance to migration data
migration <- merge(migration, dist_matrix_long, by.x = c("origin", "destination"), by.y = c("state", "destination"), all.x = TRUE)

#Replace NAs with 0s and converting meters to kilometers
migration <- migration %>%
  mutate(distance = ifelse(is.na(distance), 0, distance)) %>%
  arrange(year) %>%
  select(year, everything()) %>%
  mutate(distance = round(distance / 1000, 2))

#Distance between cities -------------------------------------------------------

#Most populous cities per state
cities <- cities %>%
  group_by(state) %>%
  slice_max(order_by = population) %>%
  ungroup()

cities_sf <- st_as_sf(cities, coords = c("long", "lat"), crs = 4326)

# Transform the data frame to an appropriate CRS for geodesic calculations
cities_sf <- st_transform(cities_sf, "+proj=longlat +datum=WGS84")

#Calculating distance matrix between the points
dist_matrix <- st_distance(cities_sf)

dist_matrix <- dist_matrix %>%
  as.data.frame()

rownames(dist_matrix) <- cities_sf$state
colnames(dist_matrix) <- cities_sf$state

#Wide to long transformation
dist_matrix$state <- row.names(dist_matrix)

dist_matrix_long <- dist_matrix %>%
  tidyr::pivot_longer(cols = -state, names_to = "destination", values_to = "distance_cities") %>%
  filter(state != destination)

#Adding distance to migration data
migration <- merge(migration, dist_matrix_long, by.x = c("origin", "destination"), by.y = c("state", "destination"), all.x = TRUE)

#Replace NAs with 0s and converting meters to kilometers
migration <- migration %>%
  mutate(distance_cities = ifelse(is.na(distance_cities), 0, distance_cities)) %>%
  arrange(year) %>%
  select(year, everything()) %>%
  mutate(distance_cities = round(distance_cities / 1000, 2))

#Distance far and close
migration <- migration %>%
  mutate(far = ifelse(distance > 1000, 1, 0),
         close = ifelse(distance < 1000, 1, 0))

#Borders------------------------------------------------------------------------
#Check spatial relationships and create adjacency matrix
adj_matrix <- st_touches(MY_sf)

adj_matrix <- adj_matrix %>%
  as.data.frame()

#changing numbers to state names
state_names <- c(MY_sf$NAME_1)

adj_matrix$row.id <- state_names[adj_matrix$row.id]
adj_matrix$col.id <- state_names[adj_matrix$col.id]

#Renaming columns
adj_matrix <- adj_matrix %>%
  rename("state" = row.id,
         "bstate" = col.id)

# Create an empty vector to store the border values
migration$border <- numeric(nrow(migration))

# Iterate over each row of migration data and assign the border value
for (i in 1:nrow(migration)) {
  origin <- migration$origin[i]
  destination <- migration$destination[i]

  # Check if origin and destination share a border in adj_matrix
  if (destination %in% adj_matrix[adj_matrix[,1] == origin, 2]) {
    migration$border[i] <- 1  # Set border value to 1 if they share a border
  } else {
    migration$border[i] <- 0  # Set border value to 0 if they do not share a border
  }
}


# 4) Additional data for interactions ------------------------------------------

#Value added of agriculture as a share of GDP ----------------------------------
GDP <- read.csv("c:/Users/samue/Desktop/Dissertation/Migration/Data/GDP.csv")

putrajaya_gdp <- tibble(
  Year = 2010:2019,
  GDP_agri_per = 0,
  State = "Putrajaya"
)

#share of value added of agri to GDP
GDP <- GDP %>%
  filter(Base.Year >= 2010 & Year < 2015 | Base.Year >= 2015) %>%
  select(Year, State, Kind.of.Economic.Activity, Value.RM.Million) %>%
  group_by(State, Year) %>%
  mutate(Value.RM.Million = ifelse(Value.RM.Million == "-", 0, Value.RM.Million),
         GDP_agri = ifelse(Kind.of.Economic.Activity == "Agriculture", as.numeric(Value.RM.Million), 0),
         GDP = sum(as.numeric(Value.RM.Million)),
         GDP_agri_per = GDP_agri / GDP) %>%
  filter(Kind.of.Economic.Activity == "Agriculture") %>%
  arrange(State) %>%
  select(Year, State, GDP_agri_per)

GDP <- bind_rows(GDP, putrajaya_gdp)

#Creating quartiles and tertiles
GDP <- GDP %>%
  group_by(Year) %>%
  mutate(GDP_agri_quintile = as.factor(ntile(GDP_agri_per, 5)),
         GDP_agri_quartile = as.factor(ntile(GDP_agri_per, 4)),
         agri_5 = ifelse(GDP_agri_quintile == 5, 1, 0),
         agri_4 = ifelse(GDP_agri_quartile == 4, 1, 0),
         non_agri_5 = ifelse(agri_5 == 1, 0, 1),
         non_agri_4 = ifelse(agri_4 == 1, 0, 1)) %>%
         arrange(Year, GDP_agri_quartile)
  

#Merging with migration dataset
migration <- migration %>%
  mutate(year = as.numeric(year)) %>%
  left_join(GDP, by = c("origin" = "State", "year" = "Year")) 

# Income -----------------------------------------------------------------------
income <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/income.xlsx")

income <- income %>%
  filter(year >= 2009)

# Create a tibble with all possible combinations of state and year
all_combinations <- expand.grid(state = unique(income$state), year = 2009:2019)

# Left join with the original income data
income <- left_join(all_combinations, income, by = c("state", "year")) %>%
  arrange(state, year)

#Lintear interpolation
income <- income %>%
  group_by(state) %>%
  mutate(income = zoo::na.approx(income)) %>%
  ungroup()

#Quintiles and dummies
income <- income %>%
  mutate(income = as.numeric(income)) %>%
  group_by(year) %>%
  mutate(income_quintile = as.factor(ntile(income, 5)),
         income_quartile = as.factor(ntile(income, 4)),
         income_tertile = as.factor(ntile(income, 3)),
         poor_5 = ifelse(income_quintile == 1, 1, 0),
         poor_4 = ifelse(income_quartile == 1, 1, 0),
         poor_3 = ifelse(income_tertile == 1, 1, 0)) %>%
  arrange(state, year)


#Merge datasets
migration <- migration %>%
  left_join(income, by = c("origin" = "state", "year"))


#Malaria -----------------------------------------------------------------------

malaria <- read.csv("c:/Users/samue/Desktop/Dissertation/Migration/Data/malaria.csv")

malaria <- malaria %>%
  filter(year >= 2010) %>%
  group_by(year) %>%
  mutate(malaria_quintile = as.factor(ntile(incidence, 5)),
         malaria_quartile = as.factor(ntile(incidence, 4)),
         malaria_tertile = as.factor(ntile(incidence, 3)),
         malaria_high_5 = ifelse(malaria_quintile == 5, 1, 0),
         malaria_high_4 = ifelse(malaria_quartile == 4, 1, 0),
         malaria_high_3 = ifelse(malaria_tertile == 3, 1, 0))

migration <- migration %>%
  left_join(malaria, by = c("origin" = "state", "year")) 

#Urbanization rate -------------------------------------------------------------
#opening excel file
urban <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/Urbanisation2010.xlsx")

urban <- urban %>%
  filter(year == 2010) 

urban <- urban %>%
  group_by(year) %>%
  mutate(urban_quintile = as.factor(ntile(urban, 5)),
         urban_quartile = as.factor(ntile(urban, 4)),
         urban_tertile = as.factor(ntile(urban, 3)),
         urban_state = ifelse(urban > 65, 1, 0),
         rural_state = ifelse(urban_state == 1, 0, 1),
         urban_state_60 = ifelse(urban > 60, 1, 0),
         rural_state_60 = ifelse(urban_state_60 == 1, 0, 1),
         urban_state_70 = ifelse(urban > 70, 1, 0),
         rural_state_70 = ifelse(urban_state_70 == 1, 0, 1)) %>%
  arrange(state, year) %>%
  ungroup() %>%
  select(-year)

#Merging with migration dataset
migration <- migration %>%
  left_join(urban, by = c("origin" = "state"))


#Irrigation --------------------------------------------------------------------

CP <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/cropland2005AD.asc")
IR <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/AEI_EARTHSTAT_IR_2005.asc")


#Cropping the rasters
MY_CP <- terra::crop(CP, MY_sf)
MY_IR <- terra::crop(IR, MY_sf)

#Extracting values
CP_by_state <- terra::extract(MY_CP, MY_sv, na.rm = TRUE)
IR_by_state <- terra::extract(MY_IR, MY_sv, na.rm = TRUE)

#Percentage of irrigated croplands
CP_by_state$AEI_EARTHSTAT_IR_2005 <- IR_by_state$AEI_EARTHSTAT_IR_2005

irrigation <- CP_by_state %>%
  filter(!is.na(cropland2005AD)) %>%
  group_by(ID) %>%
  mutate(irrigated = ifelse(cropland2005AD != 0 & AEI_EARTHSTAT_IR_2005 != 0, 1, 0),
         total_irr = sum(irrigated, na.rm = TRUE),
         count = n(),
         irr = total_irr / count) %>%
  summarise(irr = mean(irr)) %>%
  mutate(irr = as.numeric(irr))

irrigation$ID <- MY_sf$NAME_1

migration <- migration %>%
  mutate(year = as.numeric(year)) %>%
  left_join(irrigation, by = c("origin" = "ID")) %>%
  mutate(irr_50 = ifelse(irr >= 0.5, 1, 0),
         irr_65 = ifelse(irr >= 0.65, 1, 0),
         irr_35 = ifelse(irr >= 0.35, 1, 0))


#Agri share of employment ------------------------------------------------------
agri <- read.csv("c:/Users/samue/Desktop/Dissertation/Migration/Data/Sector_final.csv")

putrajaya2010 <- tibble(
  year = 2010,
  state = "Putrajaya",
  agri = 0.1
)

agri <- agri %>%
  filter(year >= 2010,
         state != "MALAYSIA") %>%
  group_by(state, year) %>%
  mutate(agriculture = ifelse(industry == "Agriculture, forestry and fishing", employed, 0),
         total = sum(as.numeric(employed)),
         agri = round(as.numeric(agriculture) * 100 / as.numeric(total), 2),
         agri = ifelse(origin == "Putrajaya" & year == "2010", 0.1, agri)) %>%
  filter(industry == "Agriculture, forestry and fishing") %>%
  select(year, state, agri)

agri <- bind_rows(agri, putrajaya2010)


#Creating quartiles and tertiles
agri <- agri %>%
  group_by(year) %>%
  mutate(agri_sh_quintile = as.factor(ntile(agri, 5)),
         agri_sh_quartile = as.factor(ntile(agri, 4)),
         agri_sh_tertile = as.factor(ntile(agri, 3)),
         agri_sh_5 = ifelse(agri_sh_quintile == 5, 1, 0),
         agri_sh_4 = ifelse(agri_sh_quartile == 4, 1, 0),
         agri_sh_3 = ifelse(agri_sh_tertile == 3, 1, 0))

#Merging with migration dataset
migration <- migration %>%
  left_join(agri, by = c("origin" = "state", "year")) 


# Vulnerability ----------------------------------------------------------------
#opening excel file
vulnerable <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/vulnerable.xlsx")

x <- vulnerable %>%
  filter(type == "Plank", strata == "Total", state != "Malaysia", year == 2012) %>%
  mutate(plank_quintile = as.factor(ntile(percentage, 5)),
         plank_quartile = as.factor(ntile(percentage, 4)),
         plank_tertile = as.factor(ntile(percentage, 3)),
         plank_5 = ifelse(plank_quintile == 5, 1, 0),
         plank_4 = ifelse(plank_quartile == 4, 1, 0),
         plank_3 = ifelse(plank_tertile == 3, 1, 0)) %>%
  select(-year)

migration <- migration %>%
  left_join(x, by = c("origin" = "state")) 

# 5) Preparation for regressions -----------------------------------------------


#Correcting the years as character issue
migration <- migration %>%
  mutate(year = as.numeric(year))

# 5.1) Dependent variable transformations ---------------------------------------
# IHS with migration rates ----
#Creation of Niit
migration <- migration %>%
  group_by(origin, year) %>%
  mutate(Niit = pop - sum(flow, na.rm = TRUE))

#Dividing flow by population floe * 1'000'000 for IHS
migration <- migration %>%
  mutate(migrates = flow * 1000000 / Niit)

#IHS
migration <- migration %>%
  mutate(IHS_flow_rates = log(migrates + (migrates^2 + 1)^0.5))

#Ln + 1 of dependent variable
migration <- migration %>%
  mutate(LN1_flow_rates = log(migrates + 1))

# 5.2) Restricted datasets -----------------------------------------------------
#Without high values ----
migration_wo_high <- migration %>%
  filter(!(origin == "Putrajaya" & destination == "Selangor"),
         !(origin == "Kuala Lumpur" & destination == "Selangor"))

x <- migration %>%
  select(year, origin, destination, migrates)

#Outliers using IQR ----

#By pairs
migration_outliers <- migration %>%
  group_by(origin, destination) %>%
  mutate(Q1 = quantile(migrates, 0.25),
         Q3 = quantile(migrates, 0.75),
         IQR = Q3 - Q1,
         lower_bound = Q1 - 1.5 * IQR,
         upper_bound = Q3 + 1.5 * IQR,
         outliers = ifelse(migrates < lower_bound | migrates > upper_bound, migrates, NA)) %>%
  filter(is.na(outliers))

# Removal of zero values ----

#Removing pairs with only zero values

#Strictly zero flow pairs
migration_wo_zeros <- migration %>%
  group_by(origin, destination) %>%
  mutate(total_flow = sum(flow)) %>%
  arrange(origin, destination) %>%
  filter(total_flow != 0)

#Mostly zeros >= 6/8 zeros -> 75%
migration_wo_zeros_75 <- migration %>%
  group_by(origin, destination) %>%
  mutate(zeros = ifelse(flow == 0, 1, 0),
         total_zeros = sum(zeros)) %>%
  filter(total_zeros < 6) %>%
  arrange(origin, destination) 




# 6) Descriptive statistics ----------------------------------------------------
# Maps -------------------------------------------------------------------------

#Making a map showing SPEI in 2008
#Merge SPEI values with shapefile
merged_data_spei <- merge(MY_sf, SPEI_by_state, by.x = "NAME_1", by.y = "state")

# Create the map for 2008
spei_2010 <- merged_data_spei %>%
  filter(year == 2010)

plot_spei_2010 <- ggplot() +
  geom_sf(data = spei_2010, aes(fill = SPEI), color = "black") +
  labs(title = "SPEI by State", subtitle = "2010") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "gray") +
  labs(fill = "SPEI Value") +
  theme_bw()

# Create map for 2019
spei_2019 <- merged_data_spei %>%
  filter(year == 2019)

plot_spei_2019 <- ggplot() +
  geom_sf(data = spei_2019, aes(fill = SPEI), color = "black") +
  labs(title = "SPEI by State", subtitle = "2019") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "gray") +
  labs(fill = "SPEI Value") +
  theme_bw()

plot_spei_2010
plot_spei_2019


#SPEI plot over the years ----
plot_spei <- SPEI_by_state %>%
  group_by(year) %>%
  filter(year > 2006) %>%
  summarise(SPEI = mean(SPEI))

# Create a new variable 'color' to distinguish positive and negative values
plot_spei <- plot_spei %>%
  mutate(color = ifelse(SPEI >= 0, "Positive", "Negative"))

# Define the color palette for positive and negative values
color_palette <- c("Positive" = "blue", "Negative" = "red")

# Create bar plot
plot_spei <- ggplot(data = plot_spei, aes(x = year, y = SPEI, fill = color)) +
  geom_col() +
  labs(title = "SPEI",
       x = "Year") +
  theme_minimal() +
  guides(fill = "none")  # Remove the legend

plot_spei

#Droughts and floods by state ----

count <- SPEI_by_state %>%
  mutate(droughts_count = ifelse(SPEI <= -1.5, 1, 0),
         floods_count = ifelse(SPEI >= 1.5, 1, 0))

count <- count %>%
  group_by(state) %>%
  summarise(droughts_count  = sum(droughts_count),
            floods_count = sum(floods_count),
            extremes_count = ifelse(droughts_count == 0 | floods_count == 0, 0, droughts_count + floods_count)) 

#Occurence of droughts by state
count$state <- reorder(count$state, count$droughts_count)

plot1 <- ggplot(count, aes(x = droughts_count, y = state)) +
  geom_col()

#Occurence of floods by state
count$state <- reorder(count$state, count$floods_count)

plot2 <- ggplot(count, aes(x = floods_count, y = state)) +
  geom_col()

#States experiencing both
count$state <- reorder(count$state, count$extremes_count)

plot3 <- ggplot(count, aes(x = extremes_count, y = state)) +
  geom_col()

grid.arrange(plot1, plot2, plot3, ncol = 3)


# 7) Final results ------------------------------------------------------------

#Centering SPEI
migration <- migration %>%
  group_by(year) %>%
  mutate(across(starts_with("SPEI"), ~ . - mean(., na.rm = TRUE)))

# Fixed effects ----------------------------------------------------------------
#Poisson
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + log(distance) + border | sw0(year + origin + destination, origin + destination^year, origin + destination^year + origin^destination), fixef.rm = "none", migration)

etable(g, cluster = "origin")

#Response function
#Saving the estimate in a table
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 | origin + destination^year + origin^destination, fixef.rm = "none", migration)

table <- coeftable(g, cluster = "origin")

# Create a data frame with a range of SPEI values
spei_values <- data.frame(SPEIpop = seq(-3, 3, by = 0.01))

# Define the estimated coefficients
slope_droughts <- table[2, 1] + table[1, 1]
slope_floods <- table[3, 1] + table[1, 1]
slope_normal <- table[1, 1]

#Calculate predicted values for each segment
spei_values <- spei_values %>%
  mutate(marginal = ifelse(spei_values$SPEIpop <= -1.5, (-1.5 * slope_normal) + slope_droughts * (SPEIpop + 1.5),
                           ifelse(spei_values$SPEIpop >= 1.5, (1.5 * slope_normal) + slope_floods * (SPEIpop - 1.5),
                                  slope_normal * SPEIpop)))

#Response function plot
ggplot(spei_values, aes(x = SPEIpop, y = marginal)) +
  geom_line() +
  theme_minimal() +
  labs(x = "SPEI", y = "Migration rates") +
  geom_hline(yintercept = 0, linetype = 2, colour = "red") +
  geom_segment(aes(x = -1.5 , xend = 1.5,y = slope_normal, yend = slope_normal), color = "grey") +
  geom_segment(aes(x = 1.5 , xend = 3,y = slope_floods, yend = slope_floods), color = "grey") +
  geom_segment(aes(x = -3 , xend = -1.5,y = slope_droughts, yend = slope_droughts), color = "grey") +
  scale_x_continuous(breaks = seq(-3, 3, by = 0.5)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.5))

#SPEI variations ---------------------------------------------------------------

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

#Lag specs
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop_lag1 + SPEIpop_droughts_1.5_lag1 + SPEIpop_floods_1.5_lag1 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 +  SPEI04pop_lag1 + SPEI04pop_droughts_0.5_lag1 + SPEI04pop_floods_0.5_lag1 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop_lag1 + SPEI04v2pop_droughts_1_lag1 + SPEI04v2pop_floods_0.5_lag1 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

#tests -------------------------------------------------------------------------

#different SD 
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_0.5 + SPEIpop_floods_0.5 + mvsw(SPEIpop_droughts_1.5, SPEIpop_floods_1.5) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_0.5 + SPEIpop_floods_1 + mvsw(SPEIpop_droughts_1.5, SPEIpop_floods_2) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_0.5 + SPEIpop_floods_1.5 + mvsw(SPEIpop_droughts_1.5) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")


g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1 + SPEIpop_floods_0.5 + mvsw(SPEIpop_droughts_2, SPEIpop_floods_1.5) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1 + SPEIpop_floods_1 + mvsw(SPEIpop_droughts_2, SPEIpop_floods_2) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1 + SPEIpop_floods_1.5 + mvsw(SPEIpop_droughts_2) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")


g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_0.5 + mvsw(SPEIpop_floods_1.5) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1 + mvsw(SPEIpop_floods_2) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")



#Lags
g <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_0.5 + SPEI04v2pop_floods_0.5 +  SPEI04v2pop_lag1 + SPEI04v2pop_droughts_0.5_lag1 + SPEI04v2pop_floods_0.5_lag1 + mvsw(SPEI04v2pop_droughts_1.5 + SPEI04v2pop_droughts_1.5_lag1, SPEI04v2pop_floods_1.5 + SPEI04v2pop_floods_1.5_lag1) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_0.5 + SPEI04v2pop_floods_1 + SPEI04v2pop_lag1 + SPEI04v2pop_droughts_0.5_lag1 + SPEI04v2pop_floods_1_lag1 + mvsw(SPEI04v2pop_droughts_1.5 + SPEI04v2pop_droughts_1.5_lag1, SPEI04v2pop_floods_2 + SPEI04v2pop_floods_2_lag1) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_0.5 + SPEI04v2pop_floods_1.5 + SPEI04v2pop_lag1 + SPEI04v2pop_droughts_0.5_lag1 + SPEI04v2pop_floods_1.5_lag1 + mvsw(SPEI04v2pop_droughts_1.5 + SPEI04v2pop_droughts_1.5_lag1) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")


g <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop_lag1 + SPEI04v2pop_droughts_1_lag1 + SPEI04v2pop_floods_0.5_lag1 + mvsw(SPEI04v2pop_droughts_2 + SPEI04v2pop_droughts_2_lag1, SPEI04v2pop_floods_1.5 + SPEI04v2pop_floods_1.5_lag1) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_1 + SPEI04v2pop_lag1 + SPEI04v2pop_droughts_1_lag1 + SPEI04v2pop_floods_1_lag1 + mvsw(SPEI04v2pop_droughts_2 + SPEI04v2pop_droughts_2_lag1, SPEI04v2pop_floods_2 + SPEI04v2pop_floods_2_lag1) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_1.5 + SPEI04v2pop_lag1 + SPEI04v2pop_droughts_1_lag1 + SPEI04v2pop_floods_1.5_lag1 + mvsw(SPEI04v2pop_droughts_2 + SPEI04v2pop_droughts_2_lag1) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")


g <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1.5 + SPEI04v2pop_floods_0.5 +  SPEI04v2pop_lag1 + SPEI04v2pop_droughts_1.5_lag1 + SPEI04v2pop_floods_0.5_lag1 + mvsw(SPEI04v2pop_floods_1.5 + SPEI04v2pop_floods_1.5_lag1) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1.5 + SPEI04v2pop_floods_1 + SPEI04v2pop_lag1 + SPEI04v2pop_droughts_1.5_lag1 + SPEI04v2pop_floods_1_lag1 + mvsw(SPEI04v2pop_floods_2 + SPEI04v2pop_floods_2_lag1) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1.5 + SPEI04v2pop_floods_1.5 + SPEI04v2pop_lag1 + SPEI04v2pop_droughts_1.5_lag1 + SPEI04v2pop_floods_1.5_lag1 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")


#7.2.1) Heterogeneous effects --------------------------------------------------

#Agri share --------------------------------------------------------------------
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:agri_sh_5 + SPEIpop_droughts_1.5:agri_sh_5 + SPEIpop_floods_1.5:agri_sh_5|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:agri_sh_5 + SPEI04pop_droughts_0.5:agri_sh_5 + SPEI04pop_floods_0.5:agri_sh_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:agri_sh_5 + SPEI04v2pop_droughts_1:agri_sh_5 + SPEI04v2pop_floods_0.5:agri_sh_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:agri_sh_4 + SPEIpop_droughts_1.5:agri_sh_4 + SPEIpop_floods_1.5:agri_sh_4|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:agri_sh_4 + SPEI04pop_droughts_0.5:agri_sh_4 + SPEI04pop_floods_0.5:agri_sh_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:agri_sh_4 + SPEI04v2pop_droughts_1:agri_sh_4 + SPEI04v2pop_floods_0.5:agri_sh_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:agri_sh_3 + SPEIpop_droughts_1.5:agri_sh_3 + SPEIpop_floods_1.5:agri_sh_3|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:agri_sh_3 + SPEI04pop_droughts_0.5:agri_sh_3 + SPEI04pop_floods_0.5:agri_sh_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:agri_sh_3 + SPEI04v2pop_droughts_1:agri_sh_3 + SPEI04v2pop_floods_0.5:agri_sh_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

#Lags
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop_lag1 + SPEIpop_droughts_1.5_lag1 + SPEIpop_floods_1.5_lag1 + SPEIpop:agri_sh_5 + SPEIpop_droughts_1.5:agri_sh_5 + SPEIpop_floods_1.5:agri_sh_5 + SPEIpop_lag1:agri_sh_5 + SPEIpop_droughts_1.5_lag1:agri_sh_5 + SPEIpop_floods_1.5_lag1:agri_sh_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 +  SPEI04pop_lag1 + SPEI04pop_droughts_0.5_lag1 + SPEI04pop_floods_0.5_lag1 + SPEI04pop:agri_sh_5 + SPEI04pop_droughts_0.5:agri_sh_5 + SPEI04pop_floods_0.5:agri_sh_5 +  SPEI04pop_lag1:agri_sh_5 + SPEI04pop_droughts_0.5_lag1:agri_sh_5 + SPEI04pop_floods_0.5_lag1:agri_sh_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop_lag1 + SPEI04v2pop_droughts_1_lag1 + SPEI04v2pop_floods_0.5_lag1 + SPEI04v2pop:agri_sh_5 + SPEI04v2pop_droughts_1:agri_sh_5 + SPEI04v2pop_floods_0.5:agri_sh_5 + SPEI04v2pop_lag1:agri_sh_5 + SPEI04v2pop_droughts_1_lag1:agri_sh_5 + SPEI04v2pop_floods_0.5_lag1:agri_sh_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop_lag1 + SPEIpop_droughts_1.5_lag1 + SPEIpop_floods_1.5_lag1 + SPEIpop:agri_sh_4 + SPEIpop_droughts_1.5:agri_sh_4 + SPEIpop_floods_1.5:agri_sh_4 + SPEIpop_lag1:agri_sh_4 + SPEIpop_droughts_1.5_lag1:agri_sh_4 + SPEIpop_floods_1.5_lag1:agri_sh_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 +  SPEI04pop_lag1 + SPEI04pop_droughts_0.5_lag1 + SPEI04pop_floods_0.5_lag1 + SPEI04pop:agri_sh_4 + SPEI04pop_droughts_0.5:agri_sh_4 + SPEI04pop_floods_0.5:agri_sh_4 +  SPEI04pop_lag1:agri_sh_4 + SPEI04pop_droughts_0.5_lag1:agri_sh_4 + SPEI04pop_floods_0.5_lag1:agri_sh_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop_lag1 + SPEI04v2pop_droughts_1_lag1 + SPEI04v2pop_floods_0.5_lag1 + SPEI04v2pop:agri_sh_4 + SPEI04v2pop_droughts_1:agri_sh_4 + SPEI04v2pop_floods_0.5:agri_sh_4 + SPEI04v2pop_lag1:agri_sh_4 + SPEI04v2pop_droughts_1_lag1:agri_sh_4 + SPEI04v2pop_floods_0.5_lag1:agri_sh_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop_lag1 + SPEIpop_droughts_1.5_lag1 + SPEIpop_floods_1.5_lag1 + SPEIpop:agri_sh_3 + SPEIpop_droughts_1.5:agri_sh_3 + SPEIpop_floods_1.5:agri_sh_3 + SPEIpop_lag1:agri_sh_3 + SPEIpop_droughts_1.5_lag1:agri_sh_3 + SPEIpop_floods_1.5_lag1:agri_sh_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 +  SPEI04pop_lag1 + SPEI04pop_droughts_0.5_lag1 + SPEI04pop_floods_0.5_lag1 + SPEI04pop:agri_sh_3 + SPEI04pop_droughts_0.5:agri_sh_3 + SPEI04pop_floods_0.5:agri_sh_3 +  SPEI04pop_lag1:agri_sh_3 + SPEI04pop_droughts_0.5_lag1:agri_sh_3 + SPEI04pop_floods_0.5_lag1:agri_sh_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop_lag1 + SPEI04v2pop_droughts_1_lag1 + SPEI04v2pop_floods_0.5_lag1 + SPEI04v2pop:agri_sh_3 + SPEI04v2pop_droughts_1:agri_sh_3 + SPEI04v2pop_floods_0.5:agri_sh_3 + SPEI04v2pop_lag1:agri_sh_3 + SPEI04v2pop_droughts_1_lag1:agri_sh_3 + SPEI04v2pop_floods_0.5_lag1:agri_sh_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

#%
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop_lag1 + SPEIpop_droughts_1.5_lag1 + SPEIpop_floods_1.5_lag1 + SPEIpop:agri + SPEIpop_droughts_1.5:agri + SPEIpop_floods_1.5:agri + SPEIpop_lag1:agri + SPEIpop_droughts_1.5_lag1:agri + SPEIpop_floods_1.5_lag1:agri |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 +  SPEI04pop_lag1 + SPEI04pop_droughts_0.5_lag1 + SPEI04pop_floods_0.5_lag1 + SPEI04pop:agri + SPEI04pop_droughts_0.5:agri + SPEI04pop_floods_0.5:agri +  SPEI04pop_lag1:agri + SPEI04pop_droughts_0.5_lag1:agri + SPEI04pop_floods_0.5_lag1:agri |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop_lag1 + SPEI04v2pop_droughts_1_lag1 + SPEI04v2pop_floods_0.5_lag1 + SPEI04v2pop:agri + SPEI04v2pop_droughts_1:agri + SPEI04v2pop_floods_0.5:agri + SPEI04v2pop_lag1:agri + SPEI04v2pop_droughts_1_lag1:agri + SPEI04v2pop_floods_0.5_lag1:agri |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

#Poor --------------------------------------------------------------------------
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:poor_5 + SPEIpop_droughts_1.5:poor_5 + SPEIpop_floods_1.5:poor_5|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:poor_5 + SPEI04pop_droughts_0.5:poor_5 + SPEI04pop_floods_0.5:poor_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:poor_5 + SPEI04v2pop_droughts_1:poor_5 + SPEI04v2pop_floods_0.5:poor_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:poor_4 + SPEIpop_droughts_1.5:poor_4 + SPEIpop_floods_1.5:poor_4|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:poor_4 + SPEI04pop_droughts_0.5:poor_4 + SPEI04pop_floods_0.5:poor_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:poor_4 + SPEI04v2pop_droughts_1:poor_4 + SPEI04v2pop_floods_0.5:poor_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:poor_3 + SPEIpop_droughts_1.5:poor_3 + SPEIpop_floods_1.5:poor_3|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:poor_3 + SPEI04pop_droughts_0.5:poor_3 + SPEI04pop_floods_0.5:poor_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:poor_3 + SPEI04v2pop_droughts_1:poor_3 + SPEI04v2pop_floods_0.5:poor_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

#Malaria ----
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:malaria_high_5 + SPEIpop_droughts_1.5:malaria_high_5 + SPEIpop_floods_1.5:malaria_high_5|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:malaria_high_5 + SPEI04pop_droughts_0.5:malaria_high_5 + SPEI04pop_floods_0.5:malaria_high_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:malaria_high_5 + SPEI04v2pop_droughts_1:malaria_high_5 + SPEI04v2pop_floods_0.5:malaria_high_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:malaria_high_4 + SPEIpop_droughts_1.5:malaria_high_4 + SPEIpop_floods_1.5:malaria_high_4|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:malaria_high_4 + SPEI04pop_droughts_0.5:malaria_high_4 + SPEI04pop_floods_0.5:malaria_high_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:malaria_high_4 + SPEI04v2pop_droughts_1:malaria_high_4 + SPEI04v2pop_floods_0.5:malaria_high_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:malaria_high_5 + SPEIpop_droughts_1.5:malaria_high_3 + SPEIpop_floods_1.5:malaria_high_3|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:malaria_high_3 + SPEI04pop_droughts_0.5:malaria_high_3 + SPEI04pop_floods_0.5:malaria_high_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:malaria_high_3 + SPEI04v2pop_droughts_1:malaria_high_3 + SPEI04v2pop_floods_0.5:malaria_high_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")


# #urban state ----

x <- migration %>%
  mutate(urban_4 = ifelse(urban_quartile == 4, 1, 0),
         urban_5 = ifelse(urban_quintile == 5, 1, 0),
         urban_3 = ifelse(urban_tertile == 3, 1, 0))

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:urban_5 + SPEIpop_droughts_1.5:urban_5 + SPEIpop_floods_1.5:urban_5|  origin + destination^year + origin^destination, fixef.rm = "none", x)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:urban_5 + SPEI04pop_droughts_0.5:urban_5 + SPEI04pop_floods_0.5:urban_5 |  origin + destination^year + origin^destination, fixef.rm = "none", x)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:urban_5 + SPEI04v2pop_droughts_1:urban_5 + SPEI04v2pop_floods_0.5:urban_5 |  origin + destination^year + origin^destination, fixef.rm = "none", x)

etable(g, g2, g3, digits = "r3", cluster = "origin")


g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:urban_4 + SPEIpop_droughts_1.5:urban_4 + SPEIpop_floods_1.5:urban_4|  origin + destination^year + origin^destination, fixef.rm = "none", x)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:urban_4 + SPEI04pop_droughts_0.5:urban_4 + SPEI04pop_floods_0.5:urban_4 |  origin + destination^year + origin^destination, fixef.rm = "none", x)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:urban_4 + SPEI04v2pop_droughts_1:urban_4 + SPEI04v2pop_floods_0.5:urban_4 |  origin + destination^year + origin^destination, fixef.rm = "none", x)

etable(g, g2, g3, digits = "r3", cluster = "origin")


g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:urban_3 + SPEIpop_droughts_1.5:urban_3 + SPEIpop_floods_1.5:urban_3|  origin + destination^year + origin^destination, fixef.rm = "none", x)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:urban_3 + SPEI04pop_droughts_0.5:urban_3 + SPEI04pop_floods_0.5:urban_3 |  origin + destination^year + origin^destination, fixef.rm = "none", x)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:urban_3 + SPEI04v2pop_droughts_1:urban_3 + SPEI04v2pop_floods_0.5:urban_3 |  origin + destination^year + origin^destination, fixef.rm = "none", x)

etable(g, g2, g3, digits = "r3", cluster = "origin")



# #Plank
# g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:plank_5 + SPEIpop_droughts_1.5:plank_5 + SPEIpop_floods_1.5:plank_5|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:plank_5 + SPEI04pop_droughts_0.5:plank_5 + SPEI04pop_floods_0.5:plank_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:plank_5 + SPEI04v2pop_droughts_1:plank_5 + SPEI04v2pop_floods_0.5:plank_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# 
# etable(g, g2, g3, digits = "r3", cluster = "origin")
# 
# 
# g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:plank_4 + SPEIpop_droughts_1.5:plank_4 + SPEIpop_floods_1.5:plank_4|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:plank_4 + SPEI04pop_droughts_0.5:plank_4 + SPEI04pop_floods_0.5:plank_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:plank_4 + SPEI04v2pop_droughts_1:plank_4 + SPEI04v2pop_floods_0.5:plank_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# 
# etable(g, g2, g3, digits = "r3", cluster = "origin")
# 
# 
# g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:plank_3 + SPEIpop_droughts_1.5:plank_3 + SPEIpop_floods_1.5:plank_3|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:plank_3 + SPEI04pop_droughts_0.5:plank_3 + SPEI04pop_floods_0.5:plank_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:plank_3 + SPEI04v2pop_droughts_1:plank_3 + SPEI04v2pop_floods_0.5:plank_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# 
# etable(g, g2, g3, digits = "r3", cluster = "origin")

#Agri --------------------------------------------------------------------------
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:agri_5 + SPEIpop_droughts_1.5:agri_5 + SPEIpop_floods_1.5:agri_5|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:agri_5 + SPEI04pop_droughts_0.5:agri_5 + SPEI04pop_floods_0.5:agri_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:agri_5 + SPEI04v2pop_droughts_1:agri_5 + SPEI04v2pop_floods_0.5:agri_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:agri_4 + SPEIpop_droughts_1.5:agri_4 + SPEIpop_floods_1.5:agri_4|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:agri_4 + SPEI04pop_droughts_0.5:agri_4 + SPEI04pop_floods_0.5:agri_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:agri_4 + SPEI04v2pop_droughts_1:agri_4 + SPEI04v2pop_floods_0.5:agri_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + SPEIpop:agri_3 + SPEIpop_droughts_1.5:agri_3 + SPEIpop_floods_1.5:agri_3|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_0.5 + SPEI04pop:agri_3 + SPEI04pop_droughts_0.5:agri_3 + SPEI04pop_floods_0.5:agri_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_0.5 + SPEI04v2pop:agri_3 + SPEI04v2pop_droughts_1:agri_3 + SPEI04v2pop_floods_0.5:agri_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

#1 SD
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1 + SPEIpop_floods_1 + SPEIpop:agri_5 + SPEIpop_droughts_1:agri_5 + SPEIpop_floods_1:agri_5|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_1 + SPEI04pop_floods_1 + SPEI04pop:agri_5 + SPEI04pop_droughts_1:agri_5 + SPEI04pop_floods_1:agri_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_1 + SPEI04v2pop:agri_5 + SPEI04v2pop_droughts_1:agri_5 + SPEI04v2pop_floods_1:agri_5 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1 + SPEIpop_floods_1 + SPEIpop:agri_4 + SPEIpop_droughts_1:agri_4 + SPEIpop_floods_1:agri_4|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_1 + SPEI04pop_floods_1 + SPEI04pop:agri_4 + SPEI04pop_droughts_1:agri_4 + SPEI04pop_floods_1:agri_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_1 + SPEI04v2pop:agri_4 + SPEI04v2pop_droughts_1:agri_4 + SPEI04v2pop_floods_1:agri_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, digits = "r3", cluster = "origin")

# g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1 + SPEIpop_floods_1 + SPEIpop:agri_3 + SPEIpop_droughts_1:agri_3 + SPEIpop_floods_1:agri_3|  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_1 + SPEI04pop_floods_1 + SPEI04pop:agri_3 + SPEI04pop_droughts_1:agri_3 + SPEI04pop_floods_1:agri_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1 + SPEI04v2pop_floods_1 + SPEI04v2pop:agri_3 + SPEI04v2pop_droughts_1:agri_3 + SPEI04v2pop_floods_1:agri_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# 
# etable(g, g2, g3, digits = "r3", cluster = "origin")




#test---------------------------------------------------------------------------


#Irrigation --------------------------------------------------------------------
#Value added agri
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + 
              SPEIpop:irr + SPEIpop_droughts_1.5:irr + SPEIpop_floods_1.5:irr + 
              SPEIpop:agri_5 + SPEIpop_droughts_1.5:agri_5 + SPEIpop_floods_1.5:agri_5 + 
              SPEIpop:agri_5:irr + SPEIpop_droughts_1.5:agri_5:irr + SPEIpop_floods_1.5:agri_5:irr |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_0.5 + SPEI04pop_floods_1.5 + 
              SPEI04pop:irr + SPEI04pop_droughts_1.5:irr + SPEI04pop_floods_1.5:irr + 
              SPEI04pop:agri_5 + SPEI04pop_droughts_1.5:agri_5 + SPEI04pop_floods_1.5:agri_5 + 
              SPEI04pop:agri_5:irr + SPEI04pop_droughts_1.5:agri_5:irr + SPEI04pop_floods_1.5:agri_5:irr |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1.5 + SPEI04v2pop_floods_1.5 + 
              SPEI04v2pop:irr + SPEI04v2pop_droughts_1.5:irr + SPEI04v2pop_floods_1.5:irr + 
              SPEI04v2pop:agri_5 + SPEI04v2pop_droughts_1.5:agri_5 + SPEI04v2pop_floods_1.5:agri_5 + 
              SPEI04v2pop:agri_5:irr + SPEI04v2pop_droughts_1.5:agri_5:irr + SPEI04v2pop_floods_1.5:agri_5:irr |  origin + destination^year + origin^destination, fixef.rm = "none", migration)


etable(g, g2, g3, digits = "r3", cluster = "origin")

#Agri share
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1.5 + 
              SPEIpop:irr + SPEIpop_droughts_1.5:irr + SPEIpop_floods_1.5:irr + 
              SPEIpop:agri_sh_5 + SPEIpop_droughts_1.5:agri_sh_5 + SPEIpop_floods_1.5:agri_sh_5 + 
              SPEIpop:agri_sh_5:irr + SPEIpop_droughts_1.5:agri_sh_5:irr + SPEIpop_floods_1.5:agri_sh_5:irr |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

g2 <- fepois(migrates ~ SPEI04pop + SPEI04pop_droughts_1.5 + SPEI04pop_floods_1.5 + 
               SPEI04pop:irr + SPEI04pop_droughts_1.5:irr + SPEI04pop_floods_1.5:irr + 
               SPEI04pop:agri_sh_5 + SPEI04pop_droughts_1.5:agri_sh_5 + SPEI04pop_floods_1.5:agri_sh_5 + 
               SPEI04pop:agri_sh_5:irr + SPEI04pop_droughts_1.5:agri_sh_5:irr + SPEI04pop_floods_1.5:agri_sh_5:irr |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

g3 <- fepois(migrates ~ SPEI04v2pop + SPEI04v2pop_droughts_1.5 + SPEI04v2pop_floods_1.5 + 
               SPEI04v2pop:irr + SPEI04v2pop_droughts_1.5:irr + SPEI04v2pop_floods_1.5:irr + 
               SPEI04v2pop:agri_sh_5 + SPEI04v2pop_droughts_1.5:agri_sh_5 + SPEI04v2pop_floods_1.5:agri_sh_5 + 
               SPEI04v2pop:agri_sh_5:irr + SPEI04v2pop_droughts_1.5:agri_sh_5:irr + SPEI04v2pop_floods_1.5:agri_sh_5:irr |  origin + destination^year + origin^destination, fixef.rm = "none", migration)


etable(g, g2, g3, digits = "r3", cluster = "origin")


#Robustness tests --------------------------------------------------------------


#8) Appendix ----------------------------------------------------------------
 
#78.0) Fixed effects -----------------------------------------------------------

#OLS
lm <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEI_floods_1.5 + log(distance) + border | sw0(year + origin + destination, origin + destination^year, origin + destination^year + origin^destination), migration)
etable(lm, cluster = "origin")

#8.1) OLS vs PPML --------------------------------------------------------------

#Droughts-floods 1.5
lm <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEI_floods_1.5 | origin + destination^year + origin^destination, migration)
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEI_floods_1.5 | origin + destination^year + origin^destination, fixef.rm = "none", migration)

lm2 <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEI_floods_1.5 | origin + destination^year + origin^destination, migration_wo_zeros)
g2 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEI_floods_1.5 | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros)

lm3 <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEI_floods_1.5 | origin + destination^year + origin^destination, migration_wo_zeros_75)
g3 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEI_floods_1.5 | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros_75)

lm4 <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEI_floods_1.5 | origin + destination^year + origin^destination, migration_wo_high)
g4 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEI_floods_1.5 | origin + destination^year + origin^destination, fixef.rm = "none" ,migration_wo_high)

etable(lm, lm2, lm3, lm4, g, g2, g3, g4, digits = "r3", cluster = "origin")


#Droughts 1.5 - floods 1
lm <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1 | origin + destination^year + origin^destination, migration)
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1 | origin + destination^year + origin^destination, fixef.rm = "none", migration)

lm2 <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1 | origin + destination^year + origin^destination, migration_wo_zeros)
g2 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1 | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros)

lm3 <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1 | origin + destination^year + origin^destination, migration_wo_zeros_75)
g3 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1 | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros_75)

lm4 <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1 | origin + destination^year + origin^destination, migration_wo_high)
g4 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_1.5 + SPEIpop_floods_1 | origin + destination^year + origin^destination, fixef.rm = "none" ,migration_wo_high)

etable(lm, lm2, lm3, lm4, g, g2, g3, g4, digits = "r3", cluster = "origin")


#Droughts 0.5 1.5 floods 0.5 1.5
lm <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_0.5 + SPEIpop_droughts_1.5 + SPEIpop_floods_0.5 + SPEIpop_floods_1.5 | origin + destination^year + origin^destination, migration)
g <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_0.5 + SPEIpop_droughts_1.5 + SPEIpop_floods_0.5 + SPEIpop_floods_1.5 | origin + destination^year + origin^destination, fixef.rm = "none", migration)

lm2 <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_0.5 + SPEIpop_droughts_1.5 + SPEIpop_floods_0.5 + SPEIpop_floods_1.5 | origin + destination^year + origin^destination, migration_wo_zeros)
g2 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_0.5 + SPEIpop_droughts_1.5 + SPEIpop_floods_0.5 + SPEIpop_floods_1.5 | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros)

lm3 <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_0.5 + SPEIpop_droughts_1.5 + SPEIpop_floods_0.5 + SPEIpop_floods_1.5 | origin + destination^year + origin^destination, migration_wo_zeros_75)
g3 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_0.5 + SPEIpop_droughts_1.5 + SPEIpop_floods_0.5 + SPEIpop_floods_1.5 | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros_75)

lm4 <- feols(IHS_flow_rates ~ SPEIpop + SPEIpop_droughts_0.5 + SPEIpop_droughts_1.5 + SPEIpop_floods_0.5 + SPEIpop_floods_1.5 | origin + destination^year + origin^destination, migration_wo_high)
g4 <- fepois(migrates ~ SPEIpop + SPEIpop_droughts_0.5 + SPEIpop_droughts_1.5 + SPEIpop_floods_0.5 + SPEIpop_floods_1.5 | origin + destination^year + origin^destination, fixef.rm = "none" ,migration_wo_high)

etable(lm, lm2, lm3, lm4, g, g2, g3, g4, digits = "r3", cluster = "origin")

#Tests -------------------------------------------------------------------------
#Correlation matrices

 
# selected_data <- migration[c("malaria_low_3", "GDP_high_3", "GDP_middle_3", "GDP_low_3")]
# correlation_matrix <- round(cor(selected_data), 2)
# print(correlation_matrix)
# 
# selected_data <- migration[c("malaria_low_3", "agri_high_3", "agri_middle_3", "agri_low_3")]
# correlation_matrix <- round(cor(selected_data), 2)
# print(correlation_matrix)
# 
# selected_data <- migration[c("malaria_low_3", "rich_3", "middle_3", "poor_3")]
# correlation_matrix <- round(cor(selected_data), 2)
# print(correlation_matrix)
# 
# selected_data <- migration[c("malaria_high_4", "agri_high_4", "GDP_high_4", "poor_4")]
# correlation_matrix <- round(cor(selected_data), 2)
# print(correlation_matrix)
