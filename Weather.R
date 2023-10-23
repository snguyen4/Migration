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
  lubridate
  )

# 1) Loading data --------------------------------------------------------------

# #Loading NETCDF.
# nc_pre <- nc_open("c:/Users/samue/Desktop/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc")
# nc_spei <- nc_open("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei12.nc")

#Loading CRU TS precipitation dataset into R
# r2 <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc", subds = "pre")
r <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei12.nc")
r1 <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei01.nc")

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

#Monthly SPEI for frequency calculations ----
SPEI_by_state_month <- SPEI_by_state

#Creating subset of the data for the years 2000-2019
SPEI_by_state_month <- SPEI_by_state_month %>%
  filter(year >= 2000 & year < 2020)

#Adding SPEI at origin state to migration dataset
#Transformation to long format
SPEI_by_state_month <- SPEI_by_state_month %>%
  gather(key = "state", value = "SPEI", -year, -month)

# Convert 'year' in spi_long to numeric
SPEI_by_state_month <- SPEI_by_state_month %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month))

#Yearly SPEI calculation ---- Keeping the 12 month June SPEI
SPEI_by_state <- SPEI_by_state %>%
  filter(month == 6,
         year >= 2000 & year < 2020) %>%
  select(- month)

#Adding SPEI at origin state to migration dataset ----
#Transformation to long format
SPEI_by_state <- SPEI_by_state %>%
  gather(key = "state", value = "SPEI", -year)

#Lags #Problem! Gap years!

# SPEI_by_state <- SPEI_by_state %>%
#   group_by(state) %>%      # Group the data by State
#   mutate(SPEI_lag1 = lag(SPEI, order_by = year),
#          SPEI_lag2 = lag(SPEI, 2, order_by = year),
#          SPEI_lag3 = lag(SPEI, 3, order_by = year),
#          SPEI_lag4 = lag(SPEI, 4, order_by = year),
#          SPEI_lag5 = lag(SPEI, 5, order_by = year)) %>%
#   ungroup()    

# Convert 'year' in spi_long to numeric
SPEI_by_state <- SPEI_by_state %>%
  mutate(year = as.numeric(year))

#Merge datasets
migration <- migration %>%
  left_join(SPEI_by_state, by = c("origin" = "state", "year"))

#SPEI_1 ------------------------------------------------------------------------
# SPEI 12 by state ----
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

#Monthly SPEI for frequency calculations ----
SPEI_by_state_1_month <- SPEI_by_state_1

#Creating subset of the data for the years 2000-2019
SPEI_by_state_1_month <- SPEI_by_state_1_month %>%
  filter(year >= 2000 & year < 2020)

#Adding SPEI at origin state to migration dataset
#Transformation to long format
SPEI_by_state_1_month <- SPEI_by_state_1_month %>%
  gather(key = "state", value = "SPEI_1", -year, -month)

# Convert 'year' in spi_long to numeric
SPEI_by_state_1_month <- SPEI_by_state_1_month %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month))

#Yearly SPEI calculation ---- Keeping the 12 month June SPEI
SPEI_by_state_1 <- SPEI_by_state_1 %>%
  select(-month) %>%
  group_by(year) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

#Adding SPEI at origin state to migration dataset ----
#Transformation to long format
SPEI_by_state_1 <- SPEI_by_state_1 %>%
  gather(key = "state", value = "SPEI_1", -year)

# Convert 'year' in spi_long to numeric
SPEI_by_state_1 <- SPEI_by_state_1 %>%
  mutate(year = as.numeric(year))

#Merge datasets
migration <- migration %>%
  left_join(SPEI_by_state_1, by = c("origin" = "state", "year"))

# 2.1) Weather variables -------------------------------------------------------

#Dummies ----
#Droughts/floods dummies
migration <- migration %>%
  mutate(droughts_intense = ifelse(SPEI <= -1.5, 1, 0),
         floods_intense = ifelse(SPEI >= 1.5, 1, 0),
         droughts_between = ifelse(SPEI <= - 1 & SPEI > -1.5, 1, 0),
         floods_between = ifelse(SPEI >= 1 & SPEI < 1.5, 1, 0),
         droughts = ifelse(SPEI <= -1, 1, 0),
         floods = ifelse(SPEI >= 1, 1, 0),
         normal = ifelse(SPEI < 1 & SPEI > -1, 1, 0),
         normal_intense = ifelse(SPEI < 1.5 & SPEI > -1.5, 1, 0))


#Piecewise linear regression ----
migration <- migration %>%
  mutate(SPEI_droughts_intense = ifelse(SPEI <= -1.5, SPEI + 1.5, 0),
         SPEI_floods_intense = ifelse(SPEI >= 1.5, SPEI - 1.5, 0),
         SPEI_droughts = ifelse(SPEI <= - 1, SPEI + 1, 0),
         SPEI_floods = ifelse(SPEI >= 1, SPEI - 1, 0))


# # Disasters ------------------------------------------------------------------
# #Wrangling
# disasters <- disasters %>%
#   mutate(end_year = ifelse(`Start Year` != `End Year`, `End Year`, 0)) %>%
#   bind_rows(disasters %>%
#               filter(`Start Year` != `End Year`) %>%
#               mutate(`Start Year` = `End Year`, `End Year` = 0))
# 
# disasters <- disasters %>%
#   select(Origin, `Start Year`) %>%
#   rename(origin = Origin, 
#          year = `Start Year`)
# 
# # Number of disasters per year per origin
# disasters <- disasters %>%
#   group_by(origin, year) %>%
#   mutate(disasters = n()) %>%
#   summarize_all(mean) %>%
#   ungroup()
#   
# disasters <- disasters %>%
#   mutate(year = as.numeric(year))
#   
# # Left join the migration dataset with the disasters dataset
# migration <- migration %>%
#   left_join(disasters, by = c("origin", "year"))
# 
# #Replacing Nas by 0
# migration <- migration %>%
#   mutate(disasters = ifelse(is.na(disasters), 0, disasters))

# 3) Geographical data --------------------------------------------

#Distance between states--------------------------------------------------------
#Converting geometries of the states into point geometries
points_MY <- st_centroid(MY_sf)

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

#Is a federal state or not?
wp = c("Kuala Lumpur", "Putrajaya", "Labuan")

migration <- migration %>%
  mutate(federal = ifelse(origin %in% wp, 1, 0))

# 4) Sociodemographic data ----------------------------------------

# #Wages
# #In the dataset, Putrajaya's GDP is included in Kula Lumpur's
# GDP <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/MY - GDPpC.xlsx")

#Population ----
#opening excel file
pop <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/MY - pop.xlsx")

#Merging with migration dataset
migration <- migration %>%
  left_join(pop, by = c("origin" = "state", "year"))

#Urbanisation rate ----
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

# 5) Alternative datasets ------------------------------------------------------




# 6) Preparation for regressions -----------------------------------------------

# #Using inverse hyperbolic sine (IHS) to account for zeros- OLS------------------
# migration <- migration %>%
#   mutate(IHS_flow = log(flow + (flow^2 + 1)^0.5))

#Correcting the years as character issue
migration <- migration %>%
  mutate(year = as.numeric(year))

# 6.1) Dependent variable transformations ---------------------------------------
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

# 6.2) Restricted datasets -----------------------------------------------------
#Without federal states in Selangor ----
migration_wo_wp <- migration %>%
  filter(!(origin == "Putrajaya" & destination == "Selangor"))
         # !(origin == "Kuala Lumpur" & destination == "Selangor"))
         #!(year == 2010 & origin == "Putrajaya" & destination == "Sabah"))

# # < 5000 migrates
# migration_5000 <- migration %>%
#   filter(migrates < 5000)
# 
# # < 4000 migrates
# migration_4000 <- migration %>%
#   filter(migrates < 4000)
# 
# # < 3000 migrates
# migration_3000 <- migration %>%
#   filter(migrates < 3000)



#Outliers using IQR ----

# x <- migration %>%
#   filter(origin == "Perlis" & destination == "Kedah")

#By pairs 
migration_outliers <- migration %>%
  group_by(origin, destination) %>%
  mutate(outliers = ifelse(length(boxplot(migrates)$out) >= 0, boxplot(migrates)$out, NA))

# #Keeping outliers with high SPEI
# migration_outliers_SPEI <- migration_outliers %>%
#   filter(!(flow %in% outliers & !(abs(SPEI) >= 1)))

#Not keeping them
migration_outliers <- migration_outliers %>%
  filter(!(migrates %in% outliers))

#without federal states in Selangor
migration_outliers_wo_wp <- migration_outliers %>%
  filter(!(origin == "Putrajaya" & destination == "Selangor"),
         !(origin == "Kuala Lumpur" & destination == "Selangor"))

# migration_outliers_SPEI_wo_wp <- migration_outliers_SPEI %>%
#   filter(!(origin == "Putrajaya" & destination == "Selangor"),
#          !(origin == "Kuala Lumpur" & destination == "Selangor"))

# Removal of zero values ----
migration_wo_zeros <- migration %>%
  filter(flow != 0)

migration_wo_zeros_wp <- migration %>%
  filter(flow != 0,
         !(origin == "Putrajaya" & destination == "Selangor"),
         !(origin == "Kuala Lumpur" & destination == "Selangor"))

migration_outliers_wo_zeros <- migration_outliers %>%
  filter(flow != 0)

migration_outliers_wo_zeros_wp <- migration_outliers_wo_wp %>%
  filter(flow != 0)

# 6.3) Lags --------------------------------------------------------------------

# First, create two versions of SPEI_by_state with the lags
SPEI_by_state_lag1 <- SPEI_by_state %>%
  mutate(year = year + 1) %>%  # Shift the year by 1 to get the previous year's SPEI
  rename(SPEI_lag1 = SPEI)

SPEI_by_state_lag2 <- SPEI_by_state %>%
  mutate(year = year + 2) %>%  # Shift the year by 2 to get the SPEI of two years ago
  rename(SPEI_lag2 = SPEI)

# Now, left join the migration_data with SPEI_by_state_lag1 and SPEI_by_state_lag2
migration <- migration %>%
  left_join(SPEI_by_state_lag1, by = c("year", "origin" = "state")) %>%
  left_join(SPEI_by_state_lag2, by = c("year", "origin" = "state"))

# 7) Descriptive statistics ----------------------------------------------------

# 7.1) Migration flows ---------------------------------------------------------
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

# Migration rates --------
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

ggplot(migration, aes(x = SPEI, y = IHS_flow_rates)) +
  geom_point() +
  labs(x = "SPEI", y = "Migration") +  # Add axis labels
  ggtitle("SPEI vs. Migration") +  # Add a title
  theme_minimal()


# 7.2) Maps --------------------------------------------------------------------

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


# 7.3) Weather variables -------------------------------------------------------

#SPEI plot ----
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


# 8) Regressions ---------------------------------------------------------------


# 8.1) Gradually adding fixed effects ----
#OLS
lm <- feols(IHS_flow_rates ~ SPEI + log(distance) + border | mvsw(year, origin, destination, destination^year, origin^destination), migration)
etable(lm, cluster = "origin")

#Poisson
g <- fepois(migrates ~ SPEI + log(distance) + border | mvsw(year, origin, destination, destination^year, origin^destination), fixef.rm = "none" ,migration)
etable(g, cluster = "origin")


# 8.2) Main specs --------------------------------------------------------------

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
g <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + border + log(distance)| mvsw(origin, destination^year, origin^destination), fixef.rm = "none" ,migration)
etable(g, cluster = "origin")


#Checking for best estimation method. Sensitivity analysis. PPML vs OLS ----
lm <- feols(c(IHS_flow_rates, LN1_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration)
g <- fepois(c(IHS_flow_rates, LN1_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), fixef.rm = "none" ,migration)
n <- fenegbin(c(IHS_flow_rates, LN1_flow_rates, migrates) ~ SPEI + log(distance) + border | origin + destination^year, migration)

lm2 <- feols(c(IHS_flow_rates, LN1_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_wo_zeros)
g2 <- fepois(c(IHS_flow_rates, LN1_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), fixef.rm = "none" ,migration_wo_zeros)
n2 <- fenegbin(c(IHS_flow_rates, LN1_flow_rates, migrates) ~ SPEI + log(distance) + border | origin + destination^year ,migration_wo_zeros)

lm3 <- feols(c(IHS_flow_rates, LN1_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_wo_wp)
g3 <- fepois(c(IHS_flow_rates, LN1_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), fixef.rm = "none" ,migration_wo_wp)
n3 <- fenegbin(c(IHS_flow_rates, LN1_flow_rates, migrates) ~ SPEI + log(distance) + border | origin + destination^year, migration_wo_wp)

lm4 <- feols(c(IHS_flow_rates, LN1_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_outliers)
g4 <- fepois(c(IHS_flow_rates, LN1_flow_rates, migrates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), fixef.rm = "none" ,migration_outliers)
n4 <- fenegbin(c(IHS_flow_rates, LN1_flow_rates, migrates) ~ SPEI + log(distance) + border | origin + destination^year ,migration_outliers)

# lm5 <- feols(IHS_flow_rates ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_outliers_wo_zeros)
# g5 <- fepois(migrates ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), fixef.rm = "none" ,migration_outliers_wo_zeros)
# n5 <- fenegbin(migrates ~ SPEI + log(distance) + border | origin + destination^year ,migration_outliers_wo_zeros)


etable(lm, lm2, lm3, lm4, cluster = "origin")
etable(g, g2, g3, g4, cluster = "origin")
etable(n, n2, n3, n4, cluster = "origin")

#Piecewise tests
lm <- feols(IHS_flow_rates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) + log(distance) + border | origin + destination^year, migration)
g <- fepois(migrates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) + log(distance) + border | origin + destination^year, fixef.rm = "none" ,migration)
n <- fenegbin(migrates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) + log(distance) + border | origin + destination^year ,migration)

lm2 <- feols(IHS_flow_rates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) + log(distance) + border | origin + destination^year, migration_wo_zeros)
g2 <- fepois(migrates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) + log(distance) + border | origin + destination^year, fixef.rm = "none" ,migration_wo_zeros)
n2 <- fenegbin(migrates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) + log(distance) + border | origin + destination^year ,migration_wo_zeros)

lm3 <- feols(IHS_flow_rates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) + log(distance) + border | origin + destination^year, migration_wo_wp)
g3 <- fepois(migrates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) + log(distance) + border | origin + destination^year, fixef.rm = "none" ,migration_wo_wp)
n3 <- fenegbin(migrates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) + log(distance) + border | origin + destination^year ,migration_wo_wp)

lm4 <- feols(IHS_flow_rates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) + log(distance) + border | origin + destination^year, migration_outliers)
g4 <- fepois(migrates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) + log(distance) + border | origin + destination^year, fixef.rm = "none" ,migration_outliers)
n4 <- fenegbin(migrates ~ SPEI + sw(SPEI_droughts_intense + SPEI_floods_intense, SPEI_droughts + SPEI_floods) + log(distance) + border | origin + destination^year ,migration_outliers)

# lm5 <- feols(IHS_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, migration_outliers_wo_zeros)
# g5 <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, fixef.rm = "none", migration_outliers_wo_zeros)
# n5 <- fenegbin(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, migration_outliers_wo_zeros)

etable(lm, lm2, lm3, lm4, cluster = "origin")
etable(g, g2, g3, g4, cluster = "origin")
etable(n, n2, n3, n4, cluster = "origin")

#Dummies test
lm <- feols(IHS_flow_rates ~ mvsw(droughts_between, floods_between, droughts_intense, floods_intense) + log(distance) + border | origin + destination^year, migration)
g <- fepois(migrates ~ mvsw(droughts_between, floods_between, droughts_intense, floods_intense) + log(distance) + border | origin + destination^year, fixef.rm = "none" ,migration)
n <- fenegbin(migrates ~ mvsw(droughts_between, floods_between, droughts_intense, floods_intense) + log(distance) + border | origin + destination^year ,migration)

lm2 <- feols(IHS_flow_rates ~ mvsw(droughts_between, floods_between, droughts_intense, floods_intense) + log(distance) + border | origin + destination^year, migration_wo_zeros)
g2 <- fepois(migrates ~ mvsw(droughts_between, floods_between, droughts_intense, floods_intense) + log(distance) + border | origin + destination^year, fixef.rm = "none" ,migration_wo_zeros)
n2 <- fenegbin(migrates ~ mvsw(droughts_between, floods_between, droughts_intense, floods_intense) + log(distance) + border | origin + destination^year ,migration_wo_zeros)

lm3 <- feols(IHS_flow_rates ~ mvsw(droughts_between, floods_between, droughts_intense, floods_intense) + log(distance) + border | origin + destination^year, migration_wo_wp)
g3 <- fepois(migrates ~ mvsw(droughts_between, floods_between, droughts_intense, floods_intense) + log(distance) + border | origin + destination^year, fixef.rm = "none" ,migration_wo_wp)
n3 <- fenegbin(migrates ~ mvsw(droughts_between, floods_between, droughts_intense, floods_intense) + log(distance) + border | origin + destination^year ,migration_wo_wp)

lm4 <- feols(IHS_flow_rates ~ mvsw(droughts_between, floods_between, droughts_intense, floods_intense) + log(distance) + border | origin + destination^year, migration_outliers)
g4 <- fepois(migrates ~ mvsw(droughts_between, floods_between, droughts_intense, floods_intense) + log(distance) + border | origin + destination^year, fixef.rm = "none" ,migration_outliers)
n4 <- fenegbin(migrates ~ mvsw(droughts_between, floods_between, droughts_intense, floods_intense) + log(distance) + border | origin + destination^year ,migration_outliers)

# lm5 <- feols(IHS_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, migration_outliers_wo_zeros)
# g5 <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, fixef.rm = "none", migration_outliers_wo_zeros)
# n5 <- fenegbin(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, migration_outliers_wo_zeros)

etable(lm, lm2, lm3, lm4, cluster = "origin")
etable(g, g2, g3, g4, cluster = "origin")
etable(n, n2, n3, n4, cluster = "origin")

# 8.3 Lags ---------------------------------------------------------------------


# 8.4) Heterogeneous effects ---------------------------------------------------
# #Levels
# g1 <- fepois(migrates ~ SPEI + sw(SPEI:rice,
#                                  SPEI:urban,
#                                  SPEI:border)| origin + destination^year, migration)
# etable(g1, vcov = "hetero", tex = TRUE)
# 
# #FLoods
# g1 <- fepois(migrates ~ magnitude_floods_intense + sw(magnitude_floods_intense:rice, 
#                                                       magnitude_floods_intense:urban,
#                                                       magnitude_floods_intense:border)| origin + destination^year, migration)
# etable(g1, vcov = "hetero", tex = TRUE)
# 
# g2 <- fepois(migrates ~ frequency_floods_intense + sw(frequency_floods_intense:rice, 
#                                                       frequency_floods_intense:urban,
#                                                       frequency_floods_intense:border)| origin + destination^year, migration)
# etable(g2, vcov = "hetero", tex = TRUE)
# 
# g3 <- fepois(migrates ~ max_duration_floods_intense + sw(max_duration_floods_intense:rice, 
#                                                   max_duration_floods_intense:urban,
#                                                   max_duration_floods_intense:border)| origin + destination^year, migration)
# etable(g3, vcov = "hetero", tex = TRUE)
# 
# #Floods
# g1 <- fepois(migrates ~ magnitude_droughts_intense + sw(magnitude_droughts_intense:rice, 
#                                                       magnitude_droughts_intense:urban,
#                                                       magnitude_droughts_intense:border)| origin + destination^year, migration)
# etable(g1, vcov = "hetero", tex = TRUE)
# 
# g2 <- fepois(migrates ~ frequency_droughts_intense + sw(frequency_droughts_intense:rice, 
#                                                       frequency_droughts_intense:urban,
#                                                       frequency_droughts_intense:border)| origin + destination^year, migration)
# etable(g2, vcov = "hetero", tex = TRUE)
# 
# g3 <- fepois(migrates ~ max_duration_droughts_intense + sw(max_duration_droughts_intense:rice, 
#                                                          max_duration_droughts_intense:urban,
#                                                          max_duration_droughts_intense:border)| origin + destination^year, migration)
# etable(g3, vcov = "hetero", tex = TRUE)


# 9) Tests ---------------------------------------------------------------------

# x <- migration %>%
#   mutate(test = ifelse(SPEI >= 0.5 & SPEI < 1, 1, 0),
#          test2 = ifelse(SPEI <= -0.5 & SPEI > -1, 1, 0),
#          test3 = ifelse(SPEI >= 1 & SPEI < 1.5, 1, 0),
#          test4 = ifelse(SPEI <= -1 & SPEI > -1.5, 1, 0))

lm <- feols(IHS_flow_rates ~ SPEI + SPEI_droughts + SPEI_floods + log(distance) + border | origin + destination^year, migration)
etable(lm, cluster = "origin")

g <- fepois(migrates ~ droughts_between + droughts_intense + floods_between + floods_intense + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)
etable(g, cluster = "origin")

g <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)
etable(g, cluster = "origin")

g_lag = fepois(migrates ~ SPEI + SPEI_lag1 + SPEI_lag2 + log(distance) + border | origin + destination^year, fixef.rm = "none", migration)
etable(g_lag, cluster = "origin")





