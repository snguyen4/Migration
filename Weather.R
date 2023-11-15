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
  geosphere #distance between cities
  )

# 1) Loading data --------------------------------------------------------------

# #Loading NETCDF.
# nc_pre <- nc_open("c:/Users/samue/Desktop/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc")
# nc_spei <- nc_open("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei12.nc")

#Loading CRU TS precipitation dataset into R
# r2 <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc", subds = "pre")
r <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei12.nc")
r1 <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei04.nc")
r2 <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei06.nc")


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

#Loading disaster dataset - Note: I deleted the Peninsular malaysia row
disasters <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/disasters.xlsx")

#Loading cities data
cities <- read.csv("c:/Users/samue/Desktop/Dissertation/Migration/Data/MY_cities.csv")


# 2) Weather data --------------------------------------------------------------

# SPEI 12 by state -------------------------------------------------------------
#Caclulating State averages
#Cropping raster layers
MY_r <- terra::crop(r, MY_sf)

#Converting to Spatvector
MY_sv <- vect(MY_sf)

#Extracting values from the raster for each state
SPEI_by_state <- terra::extract(MY_r, MY_sv, fun = "mean", na.rm = TRUE)

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
  gather(key = "state", value = "SPEI", -year)


# Convert 'year' in spi_long to numeric
SPEI_by_state <- SPEI_by_state %>%
  mutate(year = as.numeric(year))

#Merge datasets
migration <- migration %>%
  left_join(SPEI_by_state, by = c("origin" = "state", "year"))

#SPEI_4 ------------------------------------------------------------------------

#Calculating State averages
#Cropping raster layers
MY_r1 <- terra::crop(r1, MY_sf)

#Converting to Spatvector
MY_sv1 <- vect(MY_sf)

#Extracting values from the raster for each state
SPEI_by_state_1 <- terra::extract(MY_r1, MY_sv1, fun = "mean", na.rm = TRUE)

#Deleting ID column
SPEI_by_state_1 <- select(SPEI_by_state_1, -ID)

#Changing row names to states.
rownames(SPEI_by_state_1) <- MY_sf$NAME_1

#Inverting columns and rows
SPEI_by_state_1 <- SPEI_by_state_1 %>%
  t() %>%
  as.data.frame() 

SPEI_by_state_1 <- SPEI_by_state_1 %>%
  mutate(date = dates) %>%
  relocate(date, .before = 1) 

row.names(SPEI_by_state_1) <- NULL # resetting row names to initial values.

#Splitting years and months
SPEI_by_state_1 <- SPEI_by_state_1 %>%
  separate(date, into = c("year", "month"), sep = "/")

#Lagging the years by 6 months. In the migration survey, migration flows are from
#July to June next year.
SPEI_by_state_1 <- SPEI_by_state_1 %>%
  mutate(year = as.integer(year), month = as.integer(month)) %>%
  mutate(year = ifelse(month <= 6, year - 1, year))

#Adding SPEI at origin state to migration dataset
#Transformation to long format
SPEI_by_state_1 <- SPEI_by_state_1 %>%
  gather(key = "state", value = "SPEI_4", -year, -month)

# Convert 'year' in spi_long to numeric
SPEI_by_state_1 <- SPEI_by_state_1 %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month))

#Yearly SPEI calculation - Keeping 4 month SPEI during main and off growing seasons
SPEI_by_state_1 <- SPEI_by_state_1 %>%
  filter(month == 10 |
         month == 4,
         year >= 2000 & year < 2020)

# Create 'SPEI_growing_main' and 'SPEI_growing_off' variables
SPEI_by_state_1 <- SPEI_by_state_1 %>%
  mutate(
    SPEI_growing_main = ifelse(month == 10, SPEI_4, 0),
    SPEI_growing_off = ifelse(month == 04, SPEI_4, 0)
  )

SPEI_by_state_1 <- SPEI_by_state_1 %>%
  group_by(year, state) %>%
  select(-SPEI_4) %>%
  summarise(
    SPEI_growing_main = sum(SPEI_growing_main),
    SPEI_growing_off = sum(SPEI_growing_off)
  ) %>%
  ungroup() 

# Convert year to numeric
SPEI_by_state_1 <- SPEI_by_state_1 %>%
  mutate(year = as.numeric(year))

#Merge datasets
migration <- migration %>%
  left_join(SPEI_by_state_1, by = c("origin" = "state", "year"))

#SPEI_06 -----------------------------------------------------------------------
#Caclulating State averages
#Cropping raster layers
MY_r2 <- terra::crop(r2, MY_sf)

#Converting to Spatvector
MY_sv <- vect(MY_sf)

#Extracting values from the raster for each state
SPEI_by_state_2 <- terra::extract(MY_r2, MY_sv, fun = "mean", na.rm = TRUE)

#Deleting ID column
SPEI_by_state_2 <- select(SPEI_by_state_2, -ID)

#Changing row names to states.
rownames(SPEI_by_state_2) <- MY_sf$NAME_1

#Inverting columns and rows
SPEI_by_state_2 <- SPEI_by_state_2 %>%
  t() %>%
  as.data.frame() 

SPEI_by_state_2 <- SPEI_by_state_2 %>%
  mutate(date = dates) %>%
  relocate(date, .before = 1) 

row.names(SPEI_by_state_2) <- NULL # resetting row names to initial values.

#Splitting years and months
SPEI_by_state_2 <- SPEI_by_state_2 %>%
  separate(date, into = c("year", "month"), sep = "/")

#Lagging the years by 6 months. In the migration survey, migration flows are from
#July to June next year.
SPEI_by_state_2 <- SPEI_by_state_2 %>%
  mutate(year = as.integer(year), month = as.integer(month)) %>%
  mutate(year = ifelse(month <= 6, year - 1, year))

#Yearly SPEI calculation ---- Keeping the 12 month June SPEI
SPEI_by_state_2 <- SPEI_by_state_2 %>%
  filter(month == 6,
         year >= 2000 & year < 2020) %>%
  select(- month)

#Adding SPEI at origin state to migration dataset 
#Transformation to long format
SPEI_by_state_2 <- SPEI_by_state_2 %>%
  gather(key = "state", value = "SPEI_06", -year)


# Convert 'year' in spi_long to numeric
SPEI_by_state_2 <- SPEI_by_state_2 %>%
  mutate(year = as.numeric(year))

#Merge datasets
migration <- migration %>%
  left_join(SPEI_by_state_2, by = c("origin" = "state", "year"))

# 2.1) Lags --------------------------------------------------------------------

# # First, create two versions of SPEI_by_state with the lags
# SPEI_by_state_lag1 <- SPEI_by_state %>%
#   mutate(year = year + 1) %>%  # Shift the year by 1 to get the previous year's SPEI
#   rename(SPEI_lag1 = SPEI)
# 
# SPEI_by_state_lag2 <- SPEI_by_state %>%
#   mutate(year = year + 2) %>%  # Shift the year by 2 to get the SPEI of two years ago
#   rename(SPEI_lag2 = SPEI)
# 
# SPEI_by_state_lag3 <- SPEI_by_state %>%
#   mutate(year = year + 3) %>%  # Shift the year by 2 to get the SPEI of two years ago
#   rename(SPEI_lag3 = SPEI)
# 
# SPEI_by_state_lag4 <- SPEI_by_state %>%
#   mutate(year = year + 4) %>%  # Shift the year by 2 to get the SPEI of two years ago
#   rename(SPEI_lag4 = SPEI)
# 
# SPEI_by_state_lag5 <- SPEI_by_state %>%
#   mutate(year = year + 5) %>%  # Shift the year by 2 to get the SPEI of two years ago
#   rename(SPEI_lag5 = SPEI)
# 
# # Now, left join the migration_data with SPEI_by_state_lag1 and SPEI_by_state_lag2
# migration <- migration %>%
#   left_join(SPEI_by_state_lag1, by = c("year", "origin" = "state")) %>%
#   left_join(SPEI_by_state_lag2, by = c("year", "origin" = "state")) %>%
#   left_join(SPEI_by_state_lag3, by = c("year", "origin" = "state")) %>%
#   left_join(SPEI_by_state_lag4, by = c("year", "origin" = "state")) %>%
#   left_join(SPEI_by_state_lag5, by = c("year", "origin" = "state"))
# 
# #Growing seasons
# # First, create two versions of SPEI_by_state with the lags
# SPEI_by_state_lag1 <- SPEI_by_state_1 %>%
#   mutate(year = as.numeric(year + 1)) %>%  # Shift the year by 1 to get the previous year's SPEI
#   rename(SPEI_growing_main_lag1 = SPEI_growing_main,
#          SPEI_growing_off_lag1 = SPEI_growing_off)
# 
# SPEI_by_state_lag2 <- SPEI_by_state_1 %>%
#   mutate(year = as.numeric(year + 2)) %>%  # Shift the year by 2 to get the SPEI of two years ago
#   rename(SPEI_growing_main_lag2 = SPEI_growing_main,
#          SPEI_growing_off_lag2 = SPEI_growing_off)
# 
# SPEI_by_state_lag3 <- SPEI_by_state_1 %>%
#   mutate(year = as.numeric(year + 3)) %>%  # Shift the year by 2 to get the SPEI of two years ago
#   rename(SPEI_growing_main_lag3 = SPEI_growing_main,
#          SPEI_growing_off_lag3 = SPEI_growing_off)
# 
# SPEI_by_state_lag4 <- SPEI_by_state_1 %>%
#   mutate(year = as.numeric(year + 4)) %>%  # Shift the year by 2 to get the SPEI of two years ago
#   rename(SPEI_growing_main_lag4 = SPEI_growing_main,
#          SPEI_growing_off_lag4 = SPEI_growing_off)
# 
# SPEI_by_state_lag5 <- SPEI_by_state_1 %>%
#   mutate(year = as.numeric(year + 5)) %>%  # Shift the year by 2 to get the SPEI of two years ago
#   rename(SPEI_growing_main_lag5 = SPEI_growing_main,
#          SPEI_growing_off_lag5 = SPEI_growing_off)
# 
# # Now, left join the migration_data with SPEI_by_state_lag1 and SPEI_by_state_lag2
# migration <- migration %>%
#   mutate(year = as.numeric(year)) %>%
#   left_join(SPEI_by_state_lag1, by = c("year", "origin" = "state")) %>%
#   left_join(SPEI_by_state_lag2, by = c("year", "origin" = "state")) %>%
#   left_join(SPEI_by_state_lag3, by = c("year", "origin" = "state")) %>%
#   left_join(SPEI_by_state_lag4, by = c("year", "origin" = "state")) %>%
#   left_join(SPEI_by_state_lag5, by = c("year", "origin" = "state"))


# 2.2) Weather variables -------------------------------------------------------


#Piecewise linear regressions ----
migration <- migration %>%
  mutate(SPEI_droughts_intense = ifelse(SPEI <= -1.5, SPEI + 1.5, 0),
         SPEI_floods_intense = ifelse(SPEI >= 1.5, SPEI - 1.5, 0),
         SPEI_droughts = ifelse(SPEI <= - 1, SPEI + 1, 0),
         SPEI_floods = ifelse(SPEI >= 1, SPEI - 1, 0),
         SPEI_droughts_main = ifelse(SPEI_growing_main <= - 1, SPEI_growing_main + 1, 0),
         SPEI_floods_main = ifelse(SPEI_growing_main >= 1, SPEI_growing_main - 1, 0),
         SPEI_droughts_main_intense = ifelse(SPEI_growing_main <= - 1.5, SPEI_growing_main + 1.5, 0),
         SPEI_floods_main_intense = ifelse(SPEI_growing_main >= 1.5, SPEI_growing_main - 1.5, 0),
         SPEI_droughts_off = ifelse(SPEI_growing_off <= - 1, SPEI_growing_off + 1, 0),
         SPEI_floods_off = ifelse(SPEI_growing_off >= 1, SPEI_growing_off - 1, 0),
         SPEI_droughts_off_intense = ifelse(SPEI_growing_off <= - 1.5, SPEI_growing_off + 1.5, 0),
         SPEI_floods_off_intense = ifelse(SPEI_growing_off >= 1.5, SPEI_growing_off - 1.5, 0),
         SPEI_06_droughts = ifelse(SPEI_06 <= - 1, SPEI_06 + 1, 0),
         SPEI_06_floods = ifelse(SPEI_06 >= 1, SPEI_06 - 1, 0),
         SPEI_06_droughts_intense = ifelse(SPEI_06 <= - 1.5, SPEI_06 + 1.5, 0),
         SPEI_06_floods_intense = ifelse(SPEI_06 >= 1.5, SPEI_06 - 1.5, 0))

# #lags
# migration <- migration %>%
#   mutate(SPEI_droughts_intense_lag1 = ifelse(SPEI_lag1 <= -1.5, SPEI_lag1 + 1.5, 0),
#          SPEI_floods_intense_lag1 = ifelse(SPEI_lag1 >= 1.5, SPEI_lag1 - 1.5, 0),
#          SPEI_droughts_lag1 = ifelse(SPEI_lag1 <= - 1, SPEI_lag1 + 1, 0),
#          SPEI_floods_lag1 = ifelse(SPEI_lag1 >= 1, SPEI_lag1 - 1, 0))
# 
# migration <- migration %>%
#   mutate(SPEI_droughts_intense_lag2 = ifelse(SPEI_lag2 <= -1.5, SPEI_lag2 + 1.5, 0),
#          SPEI_floods_intense_lag2 = ifelse(SPEI_lag2 >= 1.5, SPEI_lag2 - 1.5, 0),
#          SPEI_droughts_lag2 = ifelse(SPEI_lag2 <= - 1, SPEI_lag2 + 1, 0),
#          SPEI_floods_lag2 = ifelse(SPEI_lag2 >= 1, SPEI_lag2 - 1, 0))


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


# 4) Sociodemographic data ----------------------------------------

#Wages
#In the dataset, Putrajaya's GDP is included in Kula Lumpur's
GDP <- read.csv("c:/Users/samue/Desktop/Dissertation/Migration/Data/GDP.csv")

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

#Merging with migration dataset
migration <- migration %>%
  mutate(year = as.numeric(year)) %>%
  left_join(GDP, by = c("origin" = "State", "year" = "Year")) %>%
  mutate(GDP_agri_per = ifelse(is.na(GDP_agri_per), 0, GDP_agri_per))

#Population 
#opening excel file
pop <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/MY - pop.xlsx")

#Merging with migration dataset
migration <- migration %>%
  left_join(pop, by = c("origin" = "state", "year"))

#Urbanisation rate 
#opening excel file
urban <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/Urbanisation2010.xlsx")

#Merging with migration dataset
migration <- migration %>%
  left_join(urban, by = c("origin" = "state"))

#Paddy fields
#vector of states producing rice
paddy <- c("Kedah", "Perlis", "Perak", "Pulau Pinang", "Kelantan", "Terengganu", "Selangor")

migration <- migration %>%
  mutate(rice = as.numeric(origin %in% paddy))

#Agricultural employment
agri <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/Sector.xlsx")

agri <- agri %>%
  filter(year == 2010,
         Sector == "Agriculture") %>%
  mutate(agri = Rate) %>%
  select(state, agri)

agri2 <- read.csv("c:/Users/samue/Desktop/Dissertation/Migration/Data/Sector_final.csv")
  
#Merging with migration dataset
migration <- migration %>%
  left_join(agri, by = c("origin" = "state"))


# Creating quartiles -----

#Rural
migration <- migration %>%
  mutate(rural_rate = 100 - urban)

# Create custom breaks for quartiles
breaks <- quantile(migration$rural_rate, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Use the cut() function with custom breaks to create GDP quartiles
migration <- migration %>%
  mutate(rural_quartile = cut(rural_rate, breaks = breaks, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE),
         rural = ifelse(rural_quartile == "Q4", 1, 0))

#Agricultural rates
migration <- migration %>%
  mutate(agri_rate = agri)

# Create custom breaks for quartiles
breaks <- quantile(migration$agri_rate, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Use the cut() function with custom breaks to create GDP quartiles
migration <- migration %>%
  mutate(agri_quartile = cut(agri_rate, breaks = breaks, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE),
         agri = ifelse(agri_quartile == "Q4", 1, 0))

#GDP_agr_per
# Create custom breaks for quartiles
breaks <- quantile(migration$GDP_agri_per, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Use the cut() function with custom breaks to create GDP quartiles
migration <- migration %>%
  mutate(GDP_agri_per_quartile = cut(GDP_agri_per, breaks = breaks, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE),
         GDP_agri_per_dummy = ifelse(GDP_agri_per_quartile == "Q4", 1, 0))

# Creating tertiles-----

#Rural

# Create custom breaks for tertiles
breaks <- quantile(migration$rural_rate, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

# Use the cut() function with custom breaks to create GDP tertiles
migration <- migration %>%
  mutate(rural_tertile = cut(rural_rate, breaks = breaks, labels = c("T1", "T2", "T3"), include.lowest = TRUE),
         rural_t = ifelse(rural_tertile == "T3", 1, 0))

#Agricultural rates

# Create custom breaks for tertiles
breaks <- quantile(migration$agri_rate, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

# Use the cut() function with custom breaks to create GDP tertiles
migration <- migration %>%
  mutate(agri_tertile = cut(agri_rate, breaks = breaks, labels = c("T1", "T2", "T3"), include.lowest = TRUE),
         agri_t = ifelse(agri_tertile == "T3", 1, 0))

#GDP_agr_per
# Create custom breaks for tertiles
breaks <- quantile(migration$GDP_agri_per, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

# Use the cut() function with custom breaks to create GDP tertiles
migration <- migration %>%
  mutate(GDP_agri_per_tertile = cut(GDP_agri_per, breaks = breaks, labels = c("T1", "T2", "T3"), include.lowest = TRUE),
         GDP_agri_per_dummy_t = ifelse(GDP_agri_per_tertile == "T3", 1, 0))


# 7) Preparation for regressions -----------------------------------------------

#Correcting the years as character issue
migration <- migration %>%
  mutate(year = as.numeric(year))

# 7.1) Dependent variable transformations ---------------------------------------
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

# 7.2) Restricted datasets -----------------------------------------------------
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

# Create bins for SPEI values ----
migration <- migration %>%
  mutate(SPEI_category = cut(SPEI, breaks = seq(-2.5, 3, by = 0.5)))


# 8) Descriptive statistics ----------------------------------------------------

# Migration flows -----
#Creating histogram of migration flows ----
ggplot(migration, aes(x = flow)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Migration Flows", x = "Flows", y = "Frequency") +
  theme_minimal()

#Percentage of 0s
mean(migration$flow == 0)

#SPEI vs migration
ggplot(migration, aes(x = SPEI, y = flow)) +
  geom_point() +
  labs(x = "SPEI", y = "Migration") +  # Add axis labels
  ggtitle("SPEI vs. Migration") +  # Add a title
  theme_minimal()

# Migration rates -----
ggplot(migration, aes(x = migrates)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Migration Rates", x = "Flows", y = "Frequency") +
  theme_minimal()

#logs
ggplot(migration, aes(x = IHS_flow_rates)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Migration Rates", x = "Flows", y = "Frequency") +
  theme_minimal()

#SPEI vs migration rates
ggplot(migration, aes(x = SPEI, y = migrates)) +
  geom_point() +
  labs(x = "SPEI", y = "Migration") +  # Add axis labels
  ggtitle("SPEI vs. Migration Rates") +  # Add a title
  theme_minimal()

#Without zeros ----
#logs
ggplot(migration_wo_zeros, aes(x = IHS_flow_rates)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Migration Rates", x = "Flows", y = "Frequency") +
  theme_minimal()

#levels
ggplot(migration_wo_zeros, aes(x = migrates)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Migration Rates", x = "Flows", y = "Frequency") +
  theme_minimal()

#SPEI vs migration rates
ggplot(migration_wo_zeros, aes(x = SPEI, y = migrates)) +
  geom_point() +
  labs(x = "SPEI", y = "Migration") +  # Add axis labels
  ggtitle("SPEI vs. Migration Rates") +  # Add a title
  theme_minimal()

ggplot(migration_wo_zeros, aes(x = SPEI, y = IHS_flow_rates)) +
  geom_point() +
  labs(x = "SPEI", y = "Migration") +  # Add axis labels
  ggtitle("SPEI vs. Migration") +  # Add a title
  theme_minimal()


#Zeros only ----
#bar plot of zero values

migration %>%
  filter(flow == 0) %>%
  group_by(SPEI_category) %>%
  summarize(Count = n()) %>%
  ggplot(aes(x = SPEI_category, y = Count)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(
    title = "Number of Zero Flow Observations by SPEI Category",
    x = "SPEI Category",
    y = "Number of Observations"
  )


# Maps -------------------------------------------------------------------------

#Grid + SF in 2019-01 ----

# Plot the cropped raster with the shapefile
plot(MY_r[[1400]])
plot(MY_sv, add = TRUE)


#Making a map showing SPEI in 2008----
# Merge SPEI values with shapefile
merged_data_spei <- merge(MY_sf, SPEI_by_state, by.x = "NAME_1", by.y = "state")

# Create the map for 2008
spei_2008 <- merged_data_spei %>%
  filter(year == 2008)

plot_spei_2008 <- ggplot() +
  geom_sf(data = spei_2008, aes(fill = SPEI), color = "black") +
  labs(title = "SPEI by State", subtitle = "2008") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "gray") +
  labs(fill = "SPEI Value") +
  theme_bw()

# Create map for 2019----
spei_2019 <- merged_data_spei %>%
  filter(year == 2019)

plot_spei_2019 <- ggplot() +
  geom_sf(data = spei_2019, aes(fill = SPEI), color = "black") +
  labs(title = "SPEI by State", subtitle = "2019") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "gray") +
  labs(fill = "SPEI Value") +
  theme_bw()


# Weather variables ------------------------------------------------------------

#SPEI plot
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


# 9) Regressions ---------------------------------------------------------------


# 9.1) Gradually adding fixed effects ----
#OLS
lm <- feols(IHS_flow_rates ~ SPEI + log(distance) + border | mvsw(year, origin, destination, destination^year, origin^destination), migration)
etable(lm, cluster = "origin")

#Poisson
g <- fepois(migrates ~ SPEI + log(distance) + border | mvsw(year, origin, destination, destination^year, origin^destination), fixef.rm = "none", migration)
etable(g, cluster = "origin")


# 9.2) Main specs --------------------------------------------------------------

#OLS ----
#Beta is positive unless you include destination year fixed effects
#It is positive with levels and negative with IHS when there are only
#destination-year fixed effects. Both are negative when either
#origin or bilateral FE are added -> why?
lm <- feols(c(IHS_flow_rates, migrates) ~ SPEI + log(distance) + border | mvsw(origin, destination^year, origin^destination), migration)
etable(lm, cluster = "origin")

#PPML ----
#Very similar with poisson. Inclusion of destination-year fixed effects make
#the coefficient negative.
g <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + border + log(distance)| mvsw(origin, destination^year, origin^destination), fixef.rm = "none", migration)
etable(g, cluster = "origin")

# 9.3) Tests with restricted datasets ------------------------------------------

#Checking for best estimation method. Sensitivity analysis. PPML vs OLS ----
lm <- feols(c(IHS_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration)
g <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), fixef.rm = "none", migration)

lm2 <- feols(c(IHS_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_wo_zeros)
g2 <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), fixef.rm = "none", migration_wo_zeros)

lm3 <- feols(c(IHS_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_wo_zeros_75)
g3 <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), fixef.rm = "none", migration_wo_zeros_75)

lm4 <- feols(c(IHS_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_wo_high)
g4 <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), fixef.rm = "none", migration_wo_high)

lm5 <- feols(c(IHS_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_outliers)
g5 <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), fixef.rm = "none", migration_outliers)

#It is consistent unless you remove the pairs with over 75% zero flows
#which leads to a sign change in the SPEI variable -> It becomes positive. 
#Migrates is very sensitive to high outliers
#Overall the SPEI coefficient stays the same even with bilateral fixed effects
etable(lm, lm2, lm3, lm4, lm5, cluster = "origin")


#SPEI coefficient in this case seems to be negative even with the removal of the
#zeros. Overall the coefficients are stable and the signs don't change.
etable(g, g2, g3, g4, g5, cluster = "origin")


#Overall the poisson with distance and border as controls has a slight advantage
#as the signs don't change even with the removal of zeros.


#Piecewise tests ----
lm <- feols(IHS_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, migration)
g <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | csw(origin + destination^year, origin^destination), fixef.rm = "none", migration)

lm2 <- feols(IHS_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, migration_wo_zeros)
g2 <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, fixef.rm = "none", migration_wo_zeros)

lm3 <- feols(IHS_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, migration_wo_zeros_75)
g3 <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, fixef.rm = "none", migration_wo_zeros_75)

lm4 <- feols(IHS_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, migration_wo_high)
g4 <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, fixef.rm = "none" ,migration_wo_high)

lm5 <- feols(IHS_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, migration_outliers)
g5 <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, fixef.rm = "none", migration_outliers)

#Regular droughts and floods are never significant.

# The coefficients change signs when zeros are removed.
# Coefficients change a lot depending on the restriction
etable(lm, lm2, lm3, lm4, lm5, cluster = "origin")


#Overall the coefficients have the same sign. Significance changes a bit.
#Overall more stable
etable(g, g2, g3, g4, g5, cluster = "origin")



# 9.3.1) Zero testing ----------------------------------------------------------

x <- migration %>%
  mutate(zero_high = ifelse((SPEI > -2 & SPEI <= 1.5 & flow == 0) | (SPEI > 2 & SPEI <= 3 & flow == 0), 1, 0)) %>%
  filter(zero_high != 1)

x <- migration %>%
  mutate(zero_high = ifelse(SPEI > -0.7 & SPEI <= 3 & flow == 0, 1, 0)) %>%
  filter(zero_high != 1)

migration_wo_zeros_test <- migration %>%
  filter(flow != 0)


#normal
g <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)

#without zeros
g2 <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, fixef.rm = "none", migration_wo_zeros_test)

#without zeros in some SPEI bins
g3 <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, fixef.rm = "none", x)


#Removing zeros between -1.5 to -0.5 decreases significance of all variables.
#Removing zeros between 0 and 0.5 decreases the significance somewhat.
#Removing those two in combination changes the significance + changes sign of floods_intense.
#All 3 have similar signs

#1333, 1301 observations lefts SPEI still negative
#1287 from -1.5 to 2 -> SPEI positive
#1285 everything but 1.5 to 2 SPEI still negative

#Removing positive SPEI zeros makes SPEI more positive.
#1425 observations left. from >-1 to 3 SPEI makes the coefficient positive
#1499 obs from 0.7 to 3 SPEI
etable(g, g2, g3, cluster = "origin")

# 9.3.2) White's test ----------------------------------------------------------

#Manual fixed effects addition
migration$origin_fe <- factor(migration$origin)

migration <- migration %>%
  unite(temp, destination, year, sep = "_") %>%
  mutate(destination_year_fe = factor(temp)) %>%
  separate(temp, into = c("destination", "year"), sep = "_")

#Linear regrression
test <- lm(IHS_flow_rates ~ SPEI + origin_fe + destination_year_fe + log(distance) + border , migration)

#White test. Rejection of the null hypothesis of homoscedasticity.
#The error are heteroscedastic.
lmtest::bptest(test)

# Plug the original data into the model and find fitted values and
# residuals/errors
fitted_data <- augment(test, data = migration)

# Look at relationship between fitted values and residuals
ggplot(fitted_data, aes(x = .fitted, y = .resid)) + 
  geom_point() +
  geom_smooth(method = "lm")


# 9.4) Significant specs -------------------------------------------------------

#Regular regressions ----------------------------------------------------

#SPEI_growing_off significant at the 10% level
g <- fepois(migrates ~ sw(SPEI, SPEI_growing_main, SPEI_growing_off) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g, cluster = "origin")

#Piecewise regressions -------------------------------------------------
#Cutoffs at 1.5 for intense and 1 for normal: SPEI is always significant.
#Droughts and floods not significant. Intense version significant.

#Without controls still significant, but coefficient on floods_intense changes
g <- fepois(migrates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g, cluster = "origin")


# 9.5) Lags --------------------------------------------------------------------

# #12 month SPEI ----
# 
# #Without controls SPEI_lag3 significant at the 10% level
# g <- fepois(migrates ~ sw(SPEI, SPEI_lag1, SPEI_lag2, SPEI_lag3, SPEI_lag4, SPEI_lag5) + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# #Significant at the 10% level with 1 and 2 lags
# #Without controls SPEI significant at the 5% level with the addition if kag 1 and 2
# #Significant at the 10% level with 5 lags
# g <- fepois(migrates ~ csw(SPEI, SPEI_lag1, SPEI_lag2, SPEI_lag3, SPEI_lag4, SPEI_lag5) + log(distance) + border| origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# #Joint significance test
# g <- fepois(migrates ~ SPEI + SPEI_lag1 + SPEI_lag2 + SPEI_lag3 + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# wald(g, "SPEI", cluster = "origin")
# 
# 
# 
# #SPEI main growing ----
# 
# #Lags are not significant
# g <- fepois(migrates ~ sw(SPEI_growing_main, SPEI_growing_main_lag1, SPEI_growing_main_lag2, SPEI_growing_main_lag3, SPEI_growing_main_lag4, SPEI_growing_main_lag5) + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# #Lags are not significant
# g <- fepois(migrates ~ csw(SPEI_growing_main, SPEI_growing_main_lag1, SPEI_growing_main_lag2, SPEI_growing_main_lag3, SPEI_growing_main_lag4, SPEI_growing_main_lag5) + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# #Not significant at all jointly
# g <- fepois(migrates ~ SPEI_growing_main + SPEI_growing_main_lag1 + SPEI_growing_main_lag2 + SPEI_growing_main_lag3 + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# wald(g, "SPEI", cluster = "origin")
# 
# 
# #SPEI off growing ----
# 
# #lag 2 significant at the 5% level
# #Without controls lag 2 and 4 significant at the 10% level
# g <- fepois(migrates ~ sw(SPEI_growing_off, SPEI_growing_off_lag1, SPEI_growing_off_lag2, SPEI_growing_off_lag3, SPEI_growing_off_lag4, SPEI_growing_off_lag5)  + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# #SPEI_growing_off significant at the 10% level and robust. Drops downs when lag 2 is included. Lag 2 significant at the 10% level with the inclusion of lag 3.
# 
# #Similar without controls. The inclusion of lags might weaken standard errors and change significance.
# g <- fepois(migrates ~ csw(SPEI_growing_off, SPEI_growing_off_lag1, SPEI_growing_off_lag2, SPEI_growing_off_lag3, SPEI_growing_off_lag4, SPEI_growing_off_lag5)  + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# #Not significant at all jointly
# g <- fepois(migrates ~ SPEI_growing_off + SPEI_growing_off_lag1 + SPEI_growing_off_lag2 + SPEI_growing_off_lag3 + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# wald(g, "SPEI", cluster = "origin")
# 
# 
# #Piecewise ----
# 
# #Overall similar results to the one without lags
# g <- fepois(migrates ~ SPEI + SPEI_lag1 + sw(SPEI_droughts_intense + SPEI_droughts_intense_lag1 + SPEI_floods_intense + SPEI_floods_intense_lag1, SPEI_droughts + SPEI_droughts_lag1 + SPEI_floods + SPEI_floods_lag1) + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# #floods_intense becomes insignificant with 2 lags
# 
# #Without controls, it is still significant. Moreover SPEI_floods is significant
# #at the 10% level
# g <- fepois(migrates ~ SPEI + SPEI_lag1 + SPEI_lag2 + sw(SPEI_droughts_intense + SPEI_droughts_intense_lag1 + SPEI_droughts_intense_lag2 + SPEI_floods_intense + SPEI_floods_intense_lag1 + SPEI_floods_intense_lag2, SPEI_droughts + SPEI_droughts_lag1 + SPEI_droughts_lag2 + SPEI_floods + SPEI_floods_lag1 + SPEI_floods_lag2) | origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# # 9.7) Heterogeneous effects ---------------------------------------------------
# 
# 
# #12-month SPEI
# g <- fepois(migrates ~ SPEI + sw(SPEI:rice, SPEI:urban, SPEI:border, SPEI:log(distance)) | origin + destination^year + origin^destination, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# #Main growing season
# g <- fepois(migrates ~ SPEI_growing_main + sw(SPEI_growing_main:rice, SPEI_growing_main:urban, SPEI_growing_main:border, SPEI_growing_main:log(distance)) + log(distance) + border| origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# #Off growing season
# g <- fepois(migrates ~ SPEI_growing_main + sw(SPEI_growing_main:rice, SPEI_growing_main:urban, SPEI_growing_main:border, SPEI_growing_main:log(distance)) + log(distance) + border| origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# #Piecewise
# g <- fepois(migrates ~ SPEI + sw(SPEI:rice + SPEI_droughts_intense + SPEI_droughts_intense:rice + SPEI_floods_intense + SPEI_floods_intense:rice, SPEI:urban + SPEI_droughts_intense + SPEI_droughts_intense:urban + SPEI_floods_intense + SPEI_floods_intense:urban, SPEI:border + SPEI_droughts_intense + SPEI_droughts_intense:border + SPEI_floods_intense + SPEI_floods_intense:border, SPEI:log(distance) + SPEI_droughts_intense + SPEI_droughts_intense:log(distance) + SPEI_floods_intense + SPEI_floods_intense:log(distance)) + log(distance) + border| origin + destination^year, fixef.rm = "none", migration)
# etable(g, cluster = "origin")



#9.7.1) Quartiles and dummies --------------------------------------------------

#Rural ----
g <- fepois(migrates ~ sw(SPEI:rural_quartile, SPEI_growing_main:rural_quartile, SPEI_growing_off:rural_quartile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g, cluster = "origin")

g <- fepois(migrates ~ SPEI:rural_quartile + SPEI_droughts_intense:rural_quartile + SPEI_floods_intense:rural_quartile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI_growing_main:rural_quartile + SPEI_droughts_main:rural_quartile + SPEI_floods_main:rural_quartile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI_growing_off:rural_quartile + SPEI_droughts_off:rural_quartile + SPEI_floods_off:rural_quartile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, cluster = "origin")

#Dummies

g <- fepois(migrates ~ SPEI + SPEI:rural + sw0(SPEI_droughts_intense + SPEI_droughts_intense:rural + SPEI_floods_intense + SPEI_floods_intense:rural) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI_growing_main + SPEI_growing_main:rural + sw0(SPEI_droughts_main + SPEI_droughts_main:rural) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI_growing_off + SPEI_growing_off:rural + sw0(SPEI_droughts_off + SPEI_droughts_off:rural + SPEI_floods_off + SPEI_floods_off:rural) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, cluster = "origin")

#Agricultural dependence ----
g <- fepois(migrates ~ sw(SPEI:agri_quartile, SPEI_growing_main:agri_quartile, SPEI_growing_off:agri_quartile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g, cluster = "origin")

g <- fepois(migrates ~ SPEI:agri_quartile + SPEI_droughts_intense:agri_quartile + SPEI_floods_intense:agri_quartile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI_growing_main:agri_quartile + SPEI_droughts_main:agri_quartile + SPEI_floods_main:agri_quartile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI_growing_off:agri_quartile + SPEI_droughts_off:agri_quartile + SPEI_floods_off:agri_quartile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, cluster = "origin")

#Dummies

g <- fepois(migrates ~ SPEI + SPEI:agri + sw0(SPEI_droughts_intense + SPEI_droughts_intense:agri + SPEI_floods_intense + SPEI_floods_intense:agri) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI_growing_main + SPEI_growing_main:agri + sw0(SPEI_droughts_main + SPEI_droughts_main:agri) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI_growing_off + SPEI_growing_off:agri + sw0(SPEI_droughts_off + SPEI_droughts_off:agri + SPEI_floods_off + SPEI_floods_off:agri) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, cluster = "origin")

# 10) Final results ------------------------------------------------------------

#10.0) Fixed effects -----------------------------------------------------------

#OLS
lm <- feols(IHS_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | sw0(year + origin + destination, origin + destination^year, origin + destination^year + origin^destination), migration)
etable(lm, cluster = "origin")

#Poisson
g <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | sw0(year + origin + destination, origin + destination^year, origin + destination^year + origin^destination), fixef.rm = "none", migration)
etable(g, cluster = "origin")

#Report
etable(lm, g, cluster = "origin", digits = "r3", tex = TRUE, style.tex = style.tex("aer"))

#10.1) OLS vs PPML -------------------------------------------------------------

lm <- feols(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration)
g <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration)

lm2 <- feols(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_wo_zeros)
g2 <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros)

lm3 <- feols(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_wo_zeros_75)
g3 <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros_75)

lm4 <- feols(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_wo_high)
g4 <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none" ,migration_wo_high)

lm5 <- feols(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_outliers)
g5 <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration_outliers)


# The coefficients change signs when zeros are removed.
# Coefficients change a lot depending on the restriction
etable(lm, lm2, lm3, lm4, lm5, cluster = "origin")


#Overall the coefficients have the same sign. changes a bit.
#Overall more stable
etable(g, g2, g3, g4, g5, cluster = "origin")

#report
lm <- feols(LN1_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration)
g <- fepois(flow ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration)

lm2 <- feols(LN1_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_wo_zeros)
g2 <- fepois(flow ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros)

lm3 <- feols(LN1_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_wo_zeros_75)
g3 <- fepois(flow ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros_75)

lm4 <- feols(LN1_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_wo_high)
g4 <- fepois(flow ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none" ,migration_wo_high)

lm5 <- feols(LN1_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_outliers)
g5 <- fepois(flow ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration_outliers)

etable(lm, lm2, lm3, lm4, lm5, g, g2, g3, g5, digits = "r3", cluster = "origin")

#10.2) Main spec ---------------------------------------------------------------

#Piecewise + normal ----
#Intense one is the only significant one
g <- fepois(migrates ~ SPEI + mvsw(SPEI_droughts, SPEI_floods) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g, cluster = "origin")

g2 <- fepois(migrates ~ SPEI + mvsw(SPEI_droughts_intense, SPEI_floods_intense) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g2, cluster = "origin")

#Report
etable(g, g2, digits = "r3", cluster = "origin", tex = TRUE, style.tex = style.tex("aer"))

#Response function
#Saving the estimate in a table
table <- coeftable(g, cluster = "origin")

# Create a data frame with a range of SPEI values
spei_values <- data.frame(SPEI = seq(-3, 3, by = 0.01))

# Define the estimated coefficients
slope_droughts <- table[2, 1] + table[1, 1]
slope_floods <- table[3, 1] + table[1, 1]
slope_normal <- table[1, 1]

#Calculate predicted values for each segment
spei_values <- spei_values %>%
  mutate(marginal = ifelse(spei_values$SPEI <= -1.5, (-1.5 * slope_normal) + slope_droughts * (SPEI + 1.5),
                           ifelse(spei_values$SPEI >= 1.5, (1.5 * slope_normal) + slope_floods * (SPEI - 1.5),
                                  slope_normal * SPEI)))

#Response function plot
ggplot(spei_values, aes(x = SPEI, y = marginal)) +
  geom_line() +
  theme_minimal() +
  labs(x = "SPEI", y = "Migrates") +
  geom_hline(yintercept = 0, linetype = 2, colour = "red") +
  geom_segment(aes(x = -1.5 , xend = 1.5,y = slope_normal, yend = slope_normal), color = "grey") +
  geom_segment(aes(x = 1.5 , xend = 3,y = slope_floods, yend = slope_floods), color = "grey") +
  geom_segment(aes(x = -3 , xend = -1.5,y = slope_droughts, yend = slope_droughts), color = "grey") +
  scale_x_continuous(breaks = seq(-3, 3, by = 0.5)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.5))


#10.2.1) Heterogeneous effects -------------------------------------------------

#Proximity ----
g <- fepois(migrates ~ SPEI + SPEI:log(distance) + sw0(SPEI_droughts_intense + SPEI_droughts_intense:log(distance) + SPEI_floods_intense + SPEI_floods_intense:log(distance)) | origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI + SPEI:border + sw0(SPEI_droughts_intense + SPEI_droughts_intense:border + SPEI_floods_intense + SPEI_floods_intense:border) | origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, cluster = "origin")

#Report
etable(g, g2, digits = "r3", cluster = "origin", tex = TRUE, style.tex = style.tex("aer"))


#Rural ----
#Tertiles
g <- fepois(migrates ~ SPEI:rural_tertile + sw0(SPEI_droughts_intense:rural_tertile + SPEI_floods_intense:rural_tertile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

#Report
etable(g, digits = "r3", cluster = "origin", tex = TRUE, style.tex = style.tex("aer"))

#Tertile dummy
g <- fepois(migrates ~ SPEI + SPEI:rural_t + sw0(SPEI_droughts_intense + SPEI_droughts_intense:rural_t + SPEI_floods_intense + SPEI_floods_intense:rural_t) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

#Report
etable(g, digits = "r3", cluster = "origin", tex = TRUE, style.tex = style.tex("aer"))


#Agricultural dependence ----

#Tertiles
g <- fepois(migrates ~ SPEI:agri_tertile + sw0(SPEI_droughts_intense:agri_tertile + SPEI_floods_intense:agri_tertile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

#Report
etable(g, digits = "r3", cluster = "origin", tex = TRUE, style.tex = style.tex("aer"))

#Dummy tertile
g <- fepois(migrates ~ SPEI + SPEI:agri_t + sw0(SPEI_droughts_intense + SPEI_droughts_intense:agri_t + SPEI_floods_intense + SPEI_floods_intense:agri_t) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")


#10.3) Growing season: dry spec ------------------------------------------------

#SPEI_growing_off significant at the 10% level
g <- fepois(migrates ~ SPEI_growing_off + sw0(SPEI_droughts_off + SPEI_floods_off, SPEI_droughts_off_intense + SPEI_floods_off_intense)|  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

#Report
etable(g, digits = "r3", cluster = "origin", tex = TRUE, style.tex = style.tex("aer"))

# 10.3.1) heterogeneous effects ------------------------------------------------

#Proximity ----
g <- fepois(migrates ~ SPEI_growing_off + SPEI_growing_off:log(distance) + sw0(SPEI_droughts_off + SPEI_droughts_off:log(distance) + SPEI_floods_off + SPEI_floods_off:log(distance), SPEI_droughts_off_intense + SPEI_droughts_off_intense:log(distance) + SPEI_floods_off_intense + SPEI_floods_off_intense:log(distance)) | origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI_growing_off + SPEI_growing_off:border + sw0(SPEI_droughts_off + SPEI_droughts_off:border + SPEI_floods_off + SPEI_floods_off:border, SPEI_droughts_off_intense + SPEI_droughts_off_intense:border + SPEI_floods_off_intense + SPEI_floods_off_intense:border) | origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, cluster = "origin")

#Report
etable(g, g2, digits = "r3", cluster = "origin", tex = TRUE, style.tex = style.tex("aer"))

#Rural -----
#Tertile
g <- fepois(migrates ~ SPEI_growing_off:rural_tertile + sw0(SPEI_droughts_off:rural_tertile + SPEI_floods_off:rural_tertile, SPEI_droughts_off_intense:rural_tertile + SPEI_floods_off_intense:rural_tertile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

#Report
etable(g, digits = "r3", cluster = "origin", tex = TRUE, style.tex = style.tex("aer"))


#Dummy
g <- fepois(migrates ~ SPEI_growing_off + SPEI_growing_off:rural_t + sw0(SPEI_droughts_off + SPEI_droughts_off:rural_t + SPEI_floods_off + SPEI_floods_off:rural_t, SPEI_droughts_off_intense + SPEI_droughts_off_intense:rural_t + SPEI_floods_off_intense + SPEI_floods_off_intense:rural_t) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

#Agricultural dependence ----
#Tertile
g <- fepois(migrates ~ SPEI_growing_off:agri_tertile + sw0(SPEI_droughts_off:agri_tertile + SPEI_floods_off:agri_tertile, SPEI_droughts_off_intense:agri_tertile + SPEI_floods_off_intense:agri_tertile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

#Report
etable(g, digits = "r3", cluster = "origin", tex = TRUE, style.tex = style.tex("aer"))


#Dummy
g <- fepois(migrates ~ SPEI_growing_off + SPEI_growing_off:agri_t + sw0(SPEI_droughts_off + SPEI_droughts_off:agri_t + SPEI_floods_off + SPEI_floods_off:agri_t, SPEI_droughts_off_intense + SPEI_droughts_off_intense:agri_t + SPEI_floods_off_intense + SPEI_floods_off_intense:agri_t) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

#10.4) Lags --------------------------------------------------------------------





#10.5) Appendix ----------------------------------------------------------------

#10.5.1) Growing season; main --------------------------------------------------
g <- fepois(migrates ~ SPEI_growing_main + mvsw(SPEI_droughts_main, SPEI_floods_main)|  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

#Heterogeneity -----------------------------------------------------------------

#Proximity ----
g <- fepois(migrates ~ SPEI_growing_main + SPEI_growing_main:log(distance) + sw0(SPEI_droughts_main_intense + SPEI_droughts_main_intense:log(distance) + SPEI_floods_main_intense + SPEI_floods_main_intense:log(distance)) | origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI_growing_main + SPEI_growing_main:border + sw0(SPEI_droughts_main_intense + SPEI_droughts_main_intense:border + SPEI_floods_main_intense + SPEI_floods_main_intense:border) | origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, cluster = "origin")

#Rural -----
#tertiles
g <- fepois(migrates ~ SPEI_growing_main:rural_tertile + sw0(SPEI_droughts_main_intense:rural_tertile + SPEI_floods_main_intense:rural_tertile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

#Dummy
g <- fepois(migrates ~ SPEI_growing_main + SPEI_growing_main:rural_t + sw0(SPEI_droughts_main_intense + SPEI_droughts_main_intense:rural_t + SPEI_floods_main_intense + SPEI_floods_main_intense:rural_t) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

#Agricultural dependence ----
#tertiles
g <- fepois(migrates ~ SPEI_growing_main:agri_tertile + sw0(SPEI_droughts_main_intense:agri_tertile + SPEI_floods_main_intense:agri_tertile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

#Dummy
g <- fepois(migrates ~ SPEI_growing_main + SPEI_growing_main:agri_t + sw0(SPEI_droughts_main_intense + SPEI_droughts_main_intense:agri_t + SPEI_floods_main + SPEI_floods_main:agri_t) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")


#Tests -------------------------------------------------------------------------
#Correlation matrices
selected_data <- migration[c("SPEI_droughts", "SPEI_floods", "SPEI_droughts_intense", "SPEI_floods_intense", "SPEI")]
correlation_matrix <- round(cor(selected_data), 2)
print(correlation_matrix)


selected_data <- migration[c("SPEI_droughts_main", "SPEI_floods_main", "SPEI_growing_main")]
correlation_matrix <- round(cor(selected_data), 2)
print(correlation_matrix)


selected_data <- migration[c("SPEI_droughts_off", "SPEI_floods_off", "SPEI_growing_off")]
correlation_matrix <- round(cor(selected_data), 2)
print(correlation_matrix)


g <- fepois(migrates ~ sw0(SPEI_06:GDP_agri_per_tertile, SPEI:GDP_agri_per_tertile, SPEI_growing_main:GDP_agri_per_tertile, SPEI_growing_off:GDP_agri_per_tertile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

g <- fepois(migrates ~ SPEI_growing_off:GDP_agri_per_tertile + SPEI_droughts_off:GDP_agri_per_tertile + SPEI_floods_off:GDP_agri_per_tertile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, cluster = "origin")

