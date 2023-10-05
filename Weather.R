# 0) Loading libraries ---------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  raster, # raster data handling
  terra, # raster data handling
  sf, # vector data handling
  tidyr,# reshape
  dplyr, #wrangling
  ggplot2, # make maps
  readxl,# handle excel files
  lmtest, #for coeftest
  sandwich, #for vcovHC
  rgeos, #geometric operations
  gravity #PPML estimator
  )

# 1) Loading data --------------------------------------------------------------

# #Loading NETCDF.
# nc_pre <- nc_open("c:/Users/samue/Desktop/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc")
# nc_spei <- nc_open("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei12.nc")

#Loading CRU TS precipitation dataset into R
# r2 <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc", subds = "pre")
r <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei12.nc")

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
  filter(origin != destination & year != "2006" & year != "2007")

#Loading disaster dataset - Note: I deleted the Peninsular malaysia row
disasters <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/disasters.xlsx")


# 2) Working with weather data -------------------------------------------------

# SPEI by state ----
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

# #Calculating 12 month SPI for all 16 states
# 
# #Wide to long transformation
# long_data <- SPEI_by_state %>%
#   gather(key = "state", value = "precipitation", -date)
# 
# spi12 <- spi(long_data$precipitation, 12)
# 
# spi_my <- data.frame(matrix(spi12$fitted, ncol = 16))
# 
# colnames(spi_my) <- MY_sf$NAME_1
# 
# spi_my <- spi_my %>%
#   mutate(date = dates) %>%
#   relocate(date, .before = 1) 
 

#Splitting years and months
SPEI_by_state <- SPEI_by_state %>%
  separate(date, into = c("year", "month"), sep = "/")

#Alternative SPEI table with months instead of years.
SPEI_by_state_2009 <- SPEI_by_state %>%
  filter(year == 2019)

#Monthly SPEI for frequency calculations ----
SPEI_by_state_month <- SPEI_by_state

#Creating subset of the data for the years 2006-2019
SPEI_by_state_month <- SPEI_by_state_month %>%
  filter(year >= 2003 & year < 2020)


#Adding SPEI at origin state to migration dataset
#Transformation to long format
SPEI_by_state_month <- SPEI_by_state_month %>%
  gather(key = "state", value = "SPEI", -year, -month)

# Convert 'year' in spi_long to numeric
SPEI_by_state_month <- SPEI_by_state_month %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month))

#Yearly SPEI calculation ----
SPEI_by_state <- SPEI_by_state %>%
  select(-month) %>%
  group_by(year) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

#Creating subset of the data for the years 2006-2019
SPEI_by_state <- SPEI_by_state %>%
  filter(year >= 2003 & year < 2020)


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

# 2.1) Weather variables -------------------------------------------------------
# Disasters --------------------------------------------------------------------
#Wrangling
disasters <- disasters %>%
  mutate(end_year = ifelse(`Start Year` != `End Year`, `End Year`, 0)) %>%
  bind_rows(disasters %>%
              filter(`Start Year` != `End Year`) %>%
              mutate(`Start Year` = `End Year`, `End Year` = 0))

disasters <- disasters %>%
  select(Origin, `Start Year`) %>%
  rename(origin = Origin, 
         year = `Start Year`)

# Number of disasters per year per origin
disasters <- disasters %>%
  group_by(origin, year) %>%
  mutate(disasters = n()) %>%
  summarize_all(mean) %>%
  ungroup()
  
disasters <- disasters %>%
  mutate(year = as.numeric(year))
  
# Left join the migration dataset with the disasters dataset
migration <- migration %>%
  left_join(disasters, by = c("origin", "year"))

#Replacing Nas by 0
migration <- migration %>%
  mutate(disasters = ifelse(is.na(disasters), 0, disasters))


#Frequency ---------------------------------------------------------------------

#value of 1 if SPEI > +-1 in a month and zero otherwise
frequency <- SPEI_by_state_month %>%
  mutate(count = ifelse(abs(SPEI) >= 1, 1, 0))


frequency <- frequency %>%
  group_by(state, year) %>%
  summarize(count = sum(count))

# Create a new variable 'frequency' with the sum of counts for the 5 preceding years
#If I only want the 5 years preceding the current year, modify the 5 to 6 and substract it by count
frequency <- frequency %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(frequency = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  ungroup()

# Removing count
frequency <- frequency %>%
  select(-count) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(frequency, by = c("origin" = "state", "year"))

#Drought frequency ----
#value of 1 if SPEI < -1 in a month and zero otherwise
frequency_droughts <- SPEI_by_state_month %>%
  mutate(count = ifelse(SPEI <= -1, 1, 0))


frequency_droughts <- frequency_droughts %>%
  group_by(state, year) %>%
  summarize(count = sum(count))

# Create a new variable 'frequency_droughts' with the sum of counts for the 5 preceding years
#If I only want the 5 years preceding the current year, modify the 5 to 6 and subtract it by count
frequency_droughts <- frequency_droughts %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(frequency_droughts = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  ungroup()

# Removing count
frequency_droughts <- frequency_droughts %>%
  select(-count) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(frequency_droughts, by = c("origin" = "state", "year"))


#Flood frequency ----
#value of 1 if SPEI  >= 1 in a month and zero otherwise
frequency_flood <- SPEI_by_state_month %>%
  mutate(count = ifelse(SPEI >= 1, 1, 0))


frequency_flood <- frequency_flood %>%
  group_by(state, year) %>%
  summarize(count = sum(count))

# Create a new variable 'frequency_flood' with the sum of counts for the 5 preceding years
#If I only want the 5 years preceding the current year, modify the 5 to 6 and subtract it by count
frequency_flood <- frequency_flood %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(frequency_flood = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  ungroup()

# Removing count
frequency_flood <- frequency_flood %>%
  select(-count) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(frequency_flood, by = c("origin" = "state", "year"))

#Maximal duration --------------------------------------------------------------
#max duration in nb of months of a drought or flood in the 5 years preceding migration
duration <- SPEI_by_state_month %>%
  mutate(count = ifelse(abs(SPEI) >= 1, 1, 0))

#Counting the number of consecutive 1s
duration <- duration %>%
  group_by(state) %>%
  mutate(duration = sequence(rle(count)$lengths) * count) %>%
  ungroup()

#Keeping the highest value per year
duration <- duration %>%
  group_by(state, year) %>%
  slice(which.max(duration)) %>%
  select(year, state, duration)

# Calculate the rolling maximum duration over the past 5 years
duration <- duration %>%
  arrange(state, year) %>%
  select(state, year, duration) %>%
  group_by(state) %>%
  mutate(
    max_duration = zoo::rollapply(duration, width = 5, FUN = max, fill = NA, align = "right")
  ) %>%
  ungroup()

# Removing duration
duration <- duration %>%
  select(-duration) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(duration, by = c("origin" = "state", "year"))

#Max duration droughts ----
duration_droughts <- SPEI_by_state_month %>%
  mutate(count = ifelse(SPEI <= -1, 1, 0))

#Counting the number of consecutive 1s
duration_droughts <- duration_droughts %>%
  group_by(state) %>%
  mutate(duration_droughts = sequence(rle(count)$lengths) * count) %>%
  ungroup()

#Keeping the highest value per year
duration_droughts <- duration_droughts %>%
  group_by(state, year) %>%
  slice(which.max(duration_droughts)) %>%
  select(year, state, duration_droughts)

# Calculate the rolling maximum duration_droughts over the past 5 years
duration_droughts <- duration_droughts %>%
  arrange(state, year) %>%
  select(state, year, duration_droughts) %>%
  group_by(state) %>%
  mutate(
    max_duration_droughts = zoo::rollapply(duration_droughts, width = 5, FUN = max, fill = NA, align = "right")
  ) %>%
  ungroup()

# Removing duration_droughts
duration_droughts <- duration_droughts %>%
  select(-duration_droughts) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(duration_droughts, by = c("origin" = "state", "year"))

# Max duration floods ----
duration_floods <- SPEI_by_state_month %>%
  mutate(count = ifelse(SPEI >= 1, 1, 0))

#Counting the number of consecutive 1s
duration_floods <- duration_floods %>%
  group_by(state) %>%
  mutate(duration_floods = sequence(rle(count)$lengths) * count) %>%
  ungroup()

#Keeping the highest value per year
duration_floods <- duration_floods %>%
  group_by(state, year) %>%
  slice(which.max(duration_floods)) %>%
  select(year, state, duration_floods)

# Calculate the rolling maximum duration_floods over the past 5 years
duration_floods <- duration_floods %>%
  arrange(state, year) %>%
  select(state, year, duration_floods) %>%
  group_by(state) %>%
  mutate(
    max_duration_floods = zoo::rollapply(duration_floods, width = 5, FUN = max, fill = NA, align = "right")
  ) %>%
  ungroup()

# Removing duration_floods
duration_floods <- duration_floods %>%
  select(-duration_floods) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(duration_floods, by = c("origin" = "state", "year"))

# Magnitude --------------------------------------------------------------------
#sum per year of SPEI
magnitude <- SPEI_by_state_month %>%
  group_by(state, year) %>%
  summarise(sum_spei = sum(ifelse(abs(SPEI) >= 1, abs(SPEI), 0)))

#sum of the preceding 5 years
magnitude <- magnitude %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(magnitude = rollsum(sum_spei, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  select(-sum_spei) %>%
  ungroup()

#Adding it to the migration dataset
migration <- migration %>%
  left_join(magnitude, by = c("origin" = "state", "year"))

# Magnitude droughts ----
magnitude_droughts <- SPEI_by_state_month %>%
  group_by(state, year) %>%
  summarise(sum_spei = sum(ifelse(SPEI <= -1, abs(SPEI), 0)))

#sum of the preceding 5 years
magnitude_droughts <- magnitude_droughts %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(magnitude_droughts = rollsum(sum_spei, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  select(-sum_spei) %>%
  ungroup()

#Adding it to the migration dataset
migration <- migration %>%
  left_join(magnitude_droughts, by = c("origin" = "state", "year"))


# Magnitude floods ----
magnitude_floods <- SPEI_by_state_month %>%
  group_by(state, year) %>%
  summarise(sum_spei = sum(ifelse(SPEI >= 1, abs(SPEI), 0)))

#sum of the preceding 5 years
magnitude_floods <- magnitude_floods %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(magnitude_floods = rollsum(sum_spei, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  select(-sum_spei) %>%
  ungroup()

#Adding it to the migration dataset
migration <- migration %>%
  left_join(magnitude_floods, by = c("origin" = "state", "year"))

# 3) Working with geographical data --------------------------------------------

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


# 4) Working with sociodemographic data ----------------------------------------

# #Wages
# #In the dataset, Putrajaya's GDP is included in Kula Lumpur's
# GDP <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/MY - GDPpC.xlsx")

#Population
#opening excel file
pop <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/MY - pop.xlsx")

#Merging with migration dataset
migration <- migration %>%
  left_join(pop, by = c("origin" = "state", "year"))


# 5) Alternative datasets ------------------------------------------------------




# 6) Preparation of the database for regressions -------------------------------

#Using inverse hyperbolic sine (IHS) to account for zeros- OLS------------------
migration <- migration %>%
  mutate(IHS_flow = log(flow + (flow^2 + 1)^0.5))

# IHS with migration rates ----
#Creation of Niit
migration <- migration %>%
  group_by(origin, year) %>%
  mutate(Niit = pop - sum(flow, na.rm = TRUE))

#Dividing flow by population
migration <- migration %>%
  mutate(migrates = flow / Niit)

#IHS
migration <- migration %>%
  mutate(IHS_flow_rates = log(migrates + (migrates^2 + 1)^0.5))


#Fixed effects dummy creation---------------------------------------------------
#origin fixed effects
migration$origin_fe <- factor(migration$origin)

#destination fixed effects
migration$destination_fe <- factor(migration$destination)

#time fixed effects
migration$year_fe <- factor(migration$year)

# origin year FE
migration <- migration %>%
  unite(temp, origin, year, sep = "_") %>%
  mutate(origin_year_fe = factor(temp)) %>%
  separate(temp, into = c("origin", "year"), sep = "_")

# destination year FE
migration <- migration %>%
  unite(temp, destination, year, sep = "_") %>%
  mutate(destination_year_fe = factor(temp)) %>%
  separate(temp, into = c("destination", "year"), sep = "_")

#Country pair FE
migration <- migration %>%
  rowwise() %>%
  mutate(
    country_pair = ifelse(origin < destination, paste0(origin, "_", destination), paste0(destination, "_", origin))
  ) %>%
  ungroup()

migration <- migration %>%
  mutate(bilateral_fe = factor(country_pair))

# #Spread to check dummies
# migration <- migration %>%
#   mutate(dummy_variable = 1) %>%
#   spread(bilateral_fe, dummy_variable, fill = 0)


#Correcting the yeas as character issue
migration <- migration %>%
  mutate(year = as.numeric(year))


# 7) Descriptive statistics ----------------------------------------------------

#Creating histogram of migration flows
ggplot(migration, aes(x = flow)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Migration Flows", x = "Flows", y = "Frequency") +
  theme_minimal()

#Percentage of 0s
mean(migration$flow == 0)

# #Creating an out-migration map.
# origin_flows <- aggregate(flow ~ origin + year, data = migration[migration$origin != migration$destination, ], FUN = sum)
# 
# merged_data_out <- merge(MY_sf, origin_flows, by.x = "NAME_1", by.y = "origin", all.x = TRUE)
# 
# 
# #2006 out-migration map
# out_2006 <- merged_data_out %>%
#   filter(year == 2006)
# 
# plot_out_2006 <- ggplot() +
#   geom_sf(data = out_2006, aes(fill = flow)) +
#   labs(title = "Out-Migration by State",
#        subtitle = "2006") +
#   scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = NA) +
#   theme_void()
# 
# #2019 out-migration map
# out_2019 <- merged_data_out %>%
#   filter(year == 2019)
# 
# plot_out_2019 <- ggplot() +
#   geom_sf(data = out_2019, aes(fill = flow)) +
#   labs(title = "Out-Migration by State",
#        subtitle = "2019") +
#   scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = NA) +
#   theme_void()



# #Making a map showing the SPI in 2006
# # Merge SPI values with shapefile
# merged_data_spi <- merge(MY_sf, spi_long, by.x = "NAME_1", by.y = "state")
# 
# # Create the map for 2006
# spi_2006 <- merged_data_spi %>%
#   filter(year == 2006)
# 
# plot_spi_2006 <- ggplot() +
#   geom_sf(data = spi_2006, aes(fill = SPI), color = "black") +
#   labs(title = "SPI by State", subtitle = "2006") +
#   scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "gray") +
#   labs(fill = "SPI Value") +
#   theme_bw()
# 
# # Create the map for 2019
# spi_2019 <- merged_data_spi %>%
#   filter(year == 2019)
# 
# plot_spi_2019 <- ggplot() +
#   geom_sf(data = spi_2019, aes(fill = SPI), color = "black") +
#   labs(title = "SPI by State", subtitle = "2019") +
#   scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "gray") +
#   labs(fill = "SPI Value") +
#   theme_bw()


# #Combining plots SPI + out-migration 2006
# combined_plot_out <- grid.arrange(plot_out_2006, plot_out_2019, ncol = 1)
# combined_plot_out
# 
# #2019
# combined_plot_spi <- grid.arrange(plot_spi_2006, plot_spi_2019, ncol = 1)
# combined_plot_spi




# 8) Regressions ---------------------------------------------------------------

# 8.1) Independent variable - Migration numbers - origin time fixed effects-----
#OLS----------------------------------------------------------------------------
#Base specification - Time FE ----
lm1 <- lm(IHS_flow ~ SPEI + year_fe, 
          data = migration)
# Compute heteroscedastic-robust standard errors
robust_se_lm1 <- vcovHC(lm1, type = "HC1")
#Regression using robust errors, clustered at the destination level
lm1_robust <- coeftest(lm1, vcov. = robust_se_lm1, cluster = migration$destination)
head(lm1_robust)

#Base specification - time + destination FE ----
lm2 <- lm(IHS_flow ~ SPEI + year_fe + destination_fe, 
          data = migration)
# Compute heteroscedastic-robust standard errors
robust_se_lm2 <- vcovHC(lm2, type = "HC1")
#Regression using robust errors, clustered at the destination level
lm2_robust <- coeftest(lm2, vcov. = robust_se_lm2, cluster = migration$destination)
head(lm2_robust)

#Base specification - time + destination + origin FE ----
lm3 <- lm(IHS_flow ~ SPEI + year_fe + destination_fe + origin_fe, 
          data = migration)
# Compute heteroscedastic-robust standard errors
robust_se_lm3 <- vcovHC(lm3, type = "HC1")
#Regression using robust errors, clustered at the destination level
lm3_robust <- coeftest(lm3, vcov. = robust_se_lm3, cluster = migration$destination)
head(lm3_robust)

#Base specification - time + destination + origin + origin-year FE ----
lm4 <- lm(IHS_flow ~ SPEI + year_fe + destination_fe + origin_fe + origin_year_fe, 
          data = migration)
# Compute heteroscedastic-robust standard errors
robust_se_lm4 <- vcovHC(lm4, type = "HC1")
#Regression using robust errors, clustered at the destination level
lm4_robust <- coeftest(lm4, vcov. = robust_se_lm4, cluster = migration$destination)
head(lm4_robust)

#Base specification - time + destination + origin + origin-year + origin-destination FE ----
lm5 <- lm(IHS_flow ~ SPEI + year_fe + destination_fe + origin_fe + origin_year_fe + bilateral_fe, 
          data = migration)
# Compute heteroscedastic-robust standard errors
robust_se_lm5 <- vcovHC(lm5, type = "HC1")
#Regression using robust errors, clustered at the destination level
lm5_robust <- coeftest(lm5, vcov. = robust_se_lm5, cluster = migration$destination)
head(lm5_robust)

#PPML --------------------------------------------------------------------------
#Base specification - using glm ----
ppml1 <- glm(flow ~ SPEI + origin_year_fe + bilateral_fe, 
             data = migration,
             family = quasipoisson(link = "log"),
             control = glm.control(epsilon = 1e-5, maxit = 100))

# Compute heteroscedastic-robust standard errors
robust_se_ppml1 <- vcovHC(ppml1, type = "HC1")
#Regression using robust errors, clustered at the destination level
ppml1_robust <- coeftest(ppml1, vcov. = robust_se_ppml1, cluster = migration$destination)
head(ppml1_robust)


# #Using the ppml function -> similar results to using glm 
# ppml2 <- ppml(dependent_variable = "flow",
#               distance = "distance",
#               additional_regressors = c("SPEI", "origin_year_fe", "bilateral_fe"),
#               data = migration)
# # Compute heteroscedastic-robust standard errors
# robust_se_ppml2 <- vcovHC(ppml2, type = "HC1")
# #Regression using robust errors, clustered at the destination level
# ppml2_robust <- coeftest(ppml2, vcov. = robust_se_ppml2, cluster = migration$destination)
# head(ppml2_robust)


# 8.2) Migration rates ---------------------------------------------------------
#Data wrangling. Putrajaya in 2008 has no population information
migration_rates <- migration %>%
  filter(year != 2008)


#OLS ---------------------------------------------------------------------------
#Base specification - Time FE ----
lm1_rates <- lm(IHS_flow_rates ~ SPEI + origin_fe + destination_year_fe + bilateral_fe, 
          data = migration_rates)
# Compute heteroscedastic-robust standard errors
robust_se_lm1_rates <- vcovHC(lm1_rates, type = "HC1")
#Regression using robust errors, clustered at the destination level
lm1_robust_rates <- coeftest(lm1_rates, vcov. = robust_se_lm1_rates, cluster = migration$destination)
head(lm1_robust_rates)

#PPML --------------------------------------------------------------------------




#Tests -------------------------------------------------------------------------
lm <- lm(IHS_flow_rates ~ frequency_flood + log(distance) + border + origin_fe + destination_year_fe,
         data = migration_rates)
# Compute heteroscedastic-robust standard errors
robust_se_lm <- vcovHC(lm, type = "HC1")
#Regression using robust errors, clustered at the destination level
lm_robust <- coeftest(lm, vcov. = robust_se_lm, cluster = migration$destination)
head(lm_robust)

ppml <- glm(migrates ~ frequency_flood + log(distance) + border + origin_fe + destination_year_fe,
            data = migration_rates,
            family = quasipoisson(link = "log"),
            control = glm.control(epsilon = 1e-5, maxit = 100))
# Compute heteroscedastic-robust standard errors
robust_se_ppml <- vcovHC(ppml, type = "HC1")
#Regression using robust errors, clustered at the destination level
ppml_robust <- coeftest(ppml, vcov. = robust_se_ppml, cluster = migration$destination)
head(ppml_robust)
