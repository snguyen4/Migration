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

#Lagging the years by 6 months. In the migration survey, migration flows are from
#July to June next year.
SPEI_by_state <- SPEI_by_state %>%
  mutate(year = as.integer(year), month = as.integer(month)) %>%
  mutate(year = ifelse(month <= 6, year - 1, year))


#Alternative SPEI table with months instead of years.
SPEI_by_state_2019 <- SPEI_by_state %>%
  filter(year == 2019)

#Monthly SPEI for frequency calculations ----
SPEI_by_state_month <- SPEI_by_state

#Creating subset of the data for the years 2006-2019
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
         year >= 2010 & year < 2020) %>%
  select(- month)



#Adding SPEI at origin state to migration dataset ----
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
# # Disasters --------------------------------------------------------------------
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
# 
# 
# # 2.1.1) Variation: SPEI > +- 1 ------------------------------------------------
# 
# #Frequency ---------------------------------------------------------------------
# 
# #value of 1 if SPEI > +-1 in a month and zero otherwise
# frequency <- SPEI_by_state_month %>%
#   mutate(count = ifelse(abs(SPEI) >= 1, 1, 0))
# 
# 
# frequency <- frequency %>%
#   group_by(state, year) %>%
#   summarize(count = sum(count))
# 
# # Create a new variable 'frequency' with the sum of counts for the 5 preceding years
# #If I only want the 5 years preceding the current year, modify the 5 to 6 and substract it by count
# frequency <- frequency %>%
#   arrange(state, year) %>%  # Sort the data by 'state' and 'year'
#   group_by(state) %>%      # Group by 'state'
#   mutate(frequency = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   ungroup()
# 
# # Removing count
# frequency <- frequency %>%
#   select(-count) %>%
#   mutate(year = as.numeric(year))
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(frequency, by = c("origin" = "state", "year"))
# 
# #Drought frequency ----
# #value of 1 if SPEI < -1 in a month and zero otherwise
# frequency_droughts <- SPEI_by_state_month %>%
#   mutate(count = ifelse(SPEI <= -1, 1, 0))
# 
# 
# frequency_droughts <- frequency_droughts %>%
#   group_by(state, year) %>%
#   summarize(count = sum(count))
# 
# # Create a new variable 'frequency_droughts' with the sum of counts for the 5 preceding years
# #If I only want the 5 years preceding the current year, modify the 5 to 6 and subtract it by count
# frequency_droughts <- frequency_droughts %>%
#   arrange(state, year) %>%  # Sort the data by 'state' and 'year'
#   group_by(state) %>%      # Group by 'state'
#   mutate(frequency_droughts = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   ungroup()
# 
# # Removing count
# frequency_droughts <- frequency_droughts %>%
#   select(-count) %>%
#   mutate(year = as.numeric(year))
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(frequency_droughts, by = c("origin" = "state", "year"))
# 
# 
# #Flood frequency ----
# #value of 1 if SPEI  >= 1 in a month and zero otherwise
# frequency_floods <- SPEI_by_state_month %>%
#   mutate(count = ifelse(SPEI >= 1, 1, 0))
# 
# 
# frequency_floods <- frequency_floods %>%
#   group_by(state, year) %>%
#   summarize(count = sum(count))
# 
# # Create a new variable 'frequency_flood' with the sum of counts for the 5 preceding years
# #If I only want the 5 years preceding the current year, modify the 5 to 6 and subtract it by count
# frequency_floods <- frequency_floods %>%
#   arrange(state, year) %>%  # Sort the data by 'state' and 'year'
#   group_by(state) %>%      # Group by 'state'
#   mutate(frequency_floods = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   ungroup()
# 
# # Removing count
# frequency_floods <- frequency_floods %>%
#   select(-count) %>%
#   mutate(year = as.numeric(year))
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(frequency_floods, by = c("origin" = "state", "year"))
# 
# #Maximal duration --------------------------------------------------------------
# #max duration in nb of months of a drought or flood in the 5 years preceding migration
# duration <- SPEI_by_state_month %>%
#   mutate(count = ifelse(abs(SPEI) >= 1, 1, 0))
# 
# #Counting the number of consecutive 1s
# duration <- duration %>%
#   group_by(state) %>%
#   mutate(duration = sequence(rle(count)$lengths) * count) %>%
#   ungroup()
# 
# #Keeping the highest value per year
# duration <- duration %>%
#   group_by(state, year) %>%
#   slice(which.max(duration)) %>%
#   select(year, state, duration)
# 
# # Calculate the rolling maximum duration over the past 5 years
# duration <- duration %>%
#   arrange(state, year) %>%
#   select(state, year, duration) %>%
#   group_by(state) %>%
#   mutate(
#     max_duration = zoo::rollapply(duration, width = 5, FUN = max, fill = NA, align = "right")
#   ) %>%
#   ungroup()
# 
# # Removing duration
# duration <- duration %>%
#   select(-duration) %>%
#   mutate(year = as.numeric(year))
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(duration, by = c("origin" = "state", "year"))
# 
# #Max duration droughts ----
# duration_droughts <- SPEI_by_state_month %>%
#   mutate(count = ifelse(SPEI <= -1, 1, 0))
# 
# #Counting the number of consecutive 1s
# duration_droughts <- duration_droughts %>%
#   group_by(state) %>%
#   mutate(duration_droughts = sequence(rle(count)$lengths) * count) %>%
#   ungroup()
# 
# #Keeping the highest value per year
# duration_droughts <- duration_droughts %>%
#   group_by(state, year) %>%
#   slice(which.max(duration_droughts)) %>%
#   select(year, state, duration_droughts)
# 
# # Calculate the rolling maximum duration_droughts over the past 5 years
# duration_droughts <- duration_droughts %>%
#   arrange(state, year) %>%
#   select(state, year, duration_droughts) %>%
#   group_by(state) %>%
#   mutate(
#     max_duration_droughts = zoo::rollapply(duration_droughts, width = 5, FUN = max, fill = NA, align = "right")
#   ) %>%
#   ungroup()
# 
# # Removing duration_droughts
# duration_droughts <- duration_droughts %>%
#   select(-duration_droughts) %>%
#   mutate(year = as.numeric(year))
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(duration_droughts, by = c("origin" = "state", "year"))
# 
# # Max duration floods ----
# duration_floods <- SPEI_by_state_month %>%
#   mutate(count = ifelse(SPEI >= 1, 1, 0))
# 
# #Counting the number of consecutive 1s
# duration_floods <- duration_floods %>%
#   group_by(state) %>%
#   mutate(duration_floods = sequence(rle(count)$lengths) * count) %>%
#   ungroup()
# 
# #Keeping the highest value per year
# duration_floods <- duration_floods %>%
#   group_by(state, year) %>%
#   slice(which.max(duration_floods)) %>%
#   select(year, state, duration_floods)
# 
# # Calculate the rolling maximum duration_floods over the past 5 years
# duration_floods <- duration_floods %>%
#   arrange(state, year) %>%
#   select(state, year, duration_floods) %>%
#   group_by(state) %>%
#   mutate(
#     max_duration_floods = zoo::rollapply(duration_floods, width = 5, FUN = max, fill = NA, align = "right")
#   ) %>%
#   ungroup()
# 
# # Removing duration_floods
# duration_floods <- duration_floods %>%
#   select(-duration_floods) %>%
#   mutate(year = as.numeric(year))
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(duration_floods, by = c("origin" = "state", "year"))
# 
# # Magnitude --------------------------------------------------------------------
# #sum per year of SPEI
# magnitude <- SPEI_by_state_month %>%
#   group_by(state, year) %>%
#   summarise(sum_spei = sum(ifelse(abs(SPEI) >= 1, abs(SPEI), 0)))
# 
# #sum of the preceding 5 years
# magnitude <- magnitude %>%
#   arrange(state, year) %>%  # Sort the data by 'state' and 'year'
#   group_by(state) %>%      # Group by 'state'
#   mutate(magnitude = rollsum(sum_spei, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   select(-sum_spei) %>%
#   ungroup()
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(magnitude, by = c("origin" = "state", "year"))
# 
# # Magnitude droughts ----
# magnitude_droughts <- SPEI_by_state_month %>%
#   group_by(state, year) %>%
#   summarise(sum_spei = sum(ifelse(SPEI <= -1, abs(SPEI), 0)))
# 
# #sum of the preceding 5 years
# magnitude_droughts <- magnitude_droughts %>%
#   arrange(state, year) %>%  # Sort the data by 'state' and 'year'
#   group_by(state) %>%      # Group by 'state'
#   mutate(magnitude_droughts = rollsum(sum_spei, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   select(-sum_spei) %>%
#   ungroup()
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(magnitude_droughts, by = c("origin" = "state", "year"))
# 
# 
# # Magnitude floods ----
# magnitude_floods <- SPEI_by_state_month %>%
#   group_by(state, year) %>%
#   summarise(sum_spei = sum(ifelse(SPEI >= 1, abs(SPEI), 0)))
# 
# #sum of the preceding 5 years
# magnitude_floods <- magnitude_floods %>%
#   arrange(state, year) %>%  # Sort the data by 'state' and 'year'
#   group_by(state) %>%      # Group by 'state'
#   mutate(magnitude_floods = rollsum(sum_spei, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   select(-sum_spei) %>%
#   ungroup()
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(magnitude_floods, by = c("origin" = "state", "year"))
# 
# # 2.1.2) Variation: SPEI > +-1.5 -------------------------------------------------------
# 
# #value of 1 if SPEI > +-1.5 in a month and zero otherwise
# frequency_intense <- SPEI_by_state_month %>%
#   mutate(count = ifelse(abs(SPEI) >= 1.5, 1, 0))
# 
# 
# frequency_intense <- frequency_intense %>%
#   group_by(state, year) %>%
#   summarize(count = sum(count))
# 
# # Create a new variable 'frequency_intense' with the sum of counts for the 5 preceding years
# #If I only want the 5 years preceding the current year, modify the 5 to 6 and substract it by count
# frequency_intense <- frequency_intense %>%
#   arrange(state, year) %>%  # Sort the data by 'state' and 'year'
#   group_by(state) %>%      # Group by 'state'
#   mutate(frequency_intense = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   ungroup()
# 
# # Removing count
# frequency_intense <- frequency_intense %>%
#   select(-count) %>%
#   mutate(year = as.numeric(year))
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(frequency_intense, by = c("origin" = "state", "year"))
# 
# #Drought frequency ----
# #value of 1 if SPEI < -1 in a month and zero otherwise
# frequency_droughts_intense <- SPEI_by_state_month %>%
#   mutate(count = ifelse(SPEI <= -1.5, 1, 0))
# 
# 
# frequency_droughts_intense <- frequency_droughts_intense %>%
#   group_by(state, year) %>%
#   summarize(count = sum(count))
# 
# # Create a new variable 'frequency_droughts_intense' with the sum of counts for the 5 preceding years
# #If I only want the 5 years preceding the current year, modify the 5 to 6 and subtract it by count
# frequency_droughts_intense <- frequency_droughts_intense %>%
#   arrange(state, year) %>%  # Sort the data by 'state' and 'year'
#   group_by(state) %>%      # Group by 'state'
#   mutate(frequency_droughts_intense = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   ungroup()
# 
# # Removing count
# frequency_droughts_intense <- frequency_droughts_intense %>%
#   select(-count) %>%
#   mutate(year = as.numeric(year))
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(frequency_droughts_intense, by = c("origin" = "state", "year"))
# 
# 
# #Flood frequency ----
# #value of 1 if SPEI  >= 1 in a month and zero otherwise
# frequency_floods_intense <- SPEI_by_state_month %>%
#   mutate(count = ifelse(SPEI >= 1.5, 1, 0))
# 
# 
# frequency_floods_intense <- frequency_floods_intense %>%
#   group_by(state, year) %>%
#   summarize(count = sum(count))
# 
# # Create a new variable 'frequency_flood' with the sum of counts for the 5 preceding years
# #If I only want the 5 years preceding the current year, modify the 5 to 6 and subtract it by count
# frequency_floods_intense <- frequency_floods_intense %>%
#   arrange(state, year) %>%  # Sort the data by 'state' and 'year'
#   group_by(state) %>%      # Group by 'state'
#   mutate(frequency_floods_intense = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   ungroup()
# 
# # Removing count
# frequency_floods_intense <- frequency_floods_intense %>%
#   select(-count) %>%
#   mutate(year = as.numeric(year))
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(frequency_floods_intense, by = c("origin" = "state", "year"))
# 
# #Maximal duration --------------------------------------------------------------
# #max duration in nb of months of a drought or flood in the 5 years preceding migration
# duration_intense <- SPEI_by_state_month %>%
#   mutate(count = ifelse(abs(SPEI) >= 1.5, 1, 0))
# 
# #Counting the number of consecutive 1s
# duration_intense <- duration_intense %>%
#   group_by(state) %>%
#   mutate(duration_intense = sequence(rle(count)$lengths) * count) %>%
#   ungroup()
# 
# #Keeping the highest value per year
# duration_intense <- duration_intense %>%
#   group_by(state, year) %>%
#   slice(which.max(duration_intense)) %>%
#   select(year, state, duration_intense)
# 
# # Calculate the rolling maximum duration over the past 5 years
# duration_intense <- duration_intense %>%
#   arrange(state, year) %>%
#   select(state, year, duration_intense) %>%
#   group_by(state) %>%
#   mutate(
#     max_duration_intense = zoo::rollapply(duration_intense, width = 5, FUN = max, fill = NA, align = "right")
#   ) %>%
#   ungroup()
# 
# # Removing duration
# duration_intense <- duration_intense %>%
#   select(-duration_intense) %>%
#   mutate(year = as.numeric(year))
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(duration_intense, by = c("origin" = "state", "year"))
# 
# #Max duration droughts ----
# duration_droughts_intense <- SPEI_by_state_month %>%
#   mutate(count = ifelse(SPEI <= -1.5, 1, 0))
# 
# #Counting the number of consecutive 1s
# duration_droughts_intense <- duration_droughts_intense %>%
#   group_by(state) %>%
#   mutate(duration_droughts_intense = sequence(rle(count)$lengths) * count) %>%
#   ungroup()
# 
# #Keeping the highest value per year
# duration_droughts_intense <- duration_droughts_intense %>%
#   group_by(state, year) %>%
#   slice(which.max(duration_droughts_intense)) %>%
#   select(year, state, duration_droughts_intense)
# 
# # Calculate the rolling maximum duration_droughts_intense over the past 5 years
# duration_droughts_intense <- duration_droughts_intense %>%
#   arrange(state, year) %>%
#   select(state, year, duration_droughts_intense) %>%
#   group_by(state) %>%
#   mutate(
#     max_duration_droughts_intense = zoo::rollapply(duration_droughts_intense, width = 5, FUN = max, fill = NA, align = "right")
#   ) %>%
#   ungroup()
# 
# # Removing duration_droughts_intense
# duration_droughts_intense <- duration_droughts_intense %>%
#   select(-duration_droughts_intense) %>%
#   mutate(year = as.numeric(year))
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(duration_droughts_intense, by = c("origin" = "state", "year"))
# 
# # Max duration floods ----
# duration_floods_intense <- SPEI_by_state_month %>%
#   mutate(count = ifelse(SPEI >= 1.5, 1, 0))
# 
# #Counting the number of consecutive 1s
# duration_floods_intense <- duration_floods_intense %>%
#   group_by(state) %>%
#   mutate(duration_floods_intense = sequence(rle(count)$lengths) * count) %>%
#   ungroup()
# 
# #Keeping the highest value per year
# duration_floods_intense <- duration_floods_intense %>%
#   group_by(state, year) %>%
#   slice(which.max(duration_floods_intense)) %>%
#   select(year, state, duration_floods_intense)
# 
# # Calculate the rolling maximum duration_floods_intense over the past 5 years
# duration_floods_intense <- duration_floods_intense %>%
#   arrange(state, year) %>%
#   select(state, year, duration_floods_intense) %>%
#   group_by(state) %>%
#   mutate(
#     max_duration_floods_intense = zoo::rollapply(duration_floods_intense, width = 5, FUN = max, fill = NA, align = "right")
#   ) %>%
#   ungroup()
# 
# # Removing duration_floods_intense
# duration_floods_intense <- duration_floods_intense %>%
#   select(-duration_floods_intense) %>%
#   mutate(year = as.numeric(year))
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(duration_floods_intense, by = c("origin" = "state", "year"))
# 
# # Magnitude --------------------------------------------------------------------
# #sum per year of SPEI
# magnitude_intense <- SPEI_by_state_month %>%
#   group_by(state, year) %>%
#   summarise(sum_spei = sum(ifelse(abs(SPEI) >= 1.5, abs(SPEI), 0)))
# 
# #sum of the preceding 5 years
# magnitude_intense <- magnitude_intense %>%
#   arrange(state, year) %>%  # Sort the data by 'state' and 'year'
#   group_by(state) %>%      # Group by 'state'
#   mutate(magnitude_intense = rollsum(sum_spei, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   select(-sum_spei) %>%
#   ungroup()
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(magnitude_intense, by = c("origin" = "state", "year"))
# 
# # Magnitude droughts ----
# magnitude_droughts_intense <- SPEI_by_state_month %>%
#   group_by(state, year) %>%
#   summarise(sum_spei = sum(ifelse(SPEI <= -1.5, abs(SPEI), 0)))
# 
# #sum of the preceding 5 years
# magnitude_droughts_intense <- magnitude_droughts_intense %>%
#   arrange(state, year) %>%  # Sort the data by 'state' and 'year'
#   group_by(state) %>%      # Group by 'state'
#   mutate(magnitude_droughts_intense = rollsum(sum_spei, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   select(-sum_spei) %>%
#   ungroup()
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(magnitude_droughts_intense, by = c("origin" = "state", "year"))
# 
# 
# # Magnitude floods ----
# magnitude_floods_intense <- SPEI_by_state_month %>%
#   group_by(state, year) %>%
#   summarise(sum_spei = sum(ifelse(SPEI >= 1.5, abs(SPEI), 0)))
# 
# #sum of the preceding 5 years
# magnitude_floods_intense <- magnitude_floods_intense %>%
#   arrange(state, year) %>%  # Sort the data by 'state' and 'year'
#   group_by(state) %>%      # Group by 'state'
#   mutate(magnitude_floods_intense = rollsum(sum_spei, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   select(-sum_spei) %>%
#   ungroup()
# 
# #Adding it to the migration dataset
# migration <- migration %>%
#   left_join(magnitude_floods_intense, by = c("origin" = "state", "year"))

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
  mutate(outliers = ifelse(length(boxplot(flow)$out) >= 0, boxplot(flow)$out, NA))

# #Keeping outliers with high SPEI
# migration_outliers_SPEI <- migration_outliers %>%
#   filter(!(flow %in% outliers & !(abs(SPEI) >= 1)))

#Not keeping them
migration_outliers <- migration_outliers %>%
  filter(!(flow %in% outliers))

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
  geom_point(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Migration Rates", x = "Flows", y = "Frequency") +
  theme_minimal()

#Percentage of 0s
mean(migration$flow == 0)

#SPEI vs migration rates
ggplot(migration, aes(x = SPEI, y = migrates)) +
  geom_point() +
  labs(x = "SPEI", y = "Migration") +  # Add axis labels
  ggtitle("SPEI vs. Migration Rates") +  # Add a title
  theme_minimal()


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


# #Combining plots SPI + out-migration 2006
# combined_plot_out <- grid.arrange(plot_out_2006, plot_out_2019, ncol = 1)
# combined_plot_out
# 
# #2019
# combined_plot_spi <- grid.arrange(plot_spi_2006, plot_spi_2019, ncol = 1)
# combined_plot_spi

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

# Create the bar plot
plot_spei <- ggplot(data = plot_spei, aes(x = year, y = SPEI, fill = color)) +
  geom_col() +
  labs(title = "SPEI",
       x = "Year") +
  theme_minimal() +
  guides(fill = "none")  # Remove the legend

plot_spei

#Plots of frequencies ----
#Droughts
plot_frequency_droughts <- frequency_droughts %>%
  group_by(year) %>%
  filter(year > 2006) %>%
  summarise(frequency_droughts = sum(frequency_droughts))

plot_frequency_droughts <- ggplot(data = plot_frequency_droughts, aes(x = year, y = frequency_droughts)) +
  geom_line(color = "blue") +
  labs(title = "Drought Frequency",
       x = "Year") +
  theme_minimal()

#Floods
plot_frequency_floods <- frequency_floods %>%
  group_by(year) %>%
  filter(year > 2006) %>%
  summarise(frequency_floods = sum(frequency_floods))

plot_frequency_floods <- ggplot(data = plot_frequency_floods, aes(x = year, y = frequency_floods)) +
  geom_line(color = "blue") +
  labs(title = "Flood Frequency",
       x = "Year") +
  theme_minimal()

#Plots of max durations ---

#Droughts
plot_duration_droughts <- duration_droughts %>%
  group_by(year) %>%
  filter(year > 2006) %>%
  summarise(max_duration_droughts = sum(max_duration_droughts))

plot_duration_droughts <- ggplot(data = plot_duration_droughts, aes(x = year, y = max_duration_droughts)) +
  geom_line(color = "blue") +
  labs(title = "Maximum Duration of Droughts",
       x = "Year") +
  theme_minimal()

#Floods
plot_duration_floods <- duration_floods %>%
  group_by(year) %>%
  filter(year > 2006) %>%
  summarise(max_duration_floods = sum(max_duration_floods))

plot_duration_floods <- ggplot(data = plot_duration_floods, aes(x = year, y = max_duration_floods)) +
  geom_line(color = "blue") +
  labs(title = "Maximum Duration of Floods",
       x = "Year") +
  theme_minimal()

#Plots of magnitude ----

#Droughts
plot_magnitude_droughts <- magnitude_droughts %>%
  group_by(year) %>%
  filter(year > 2006) %>%
  summarise(magnitude_droughts = sum(magnitude_droughts))

plot_magnitude_droughts <- ggplot(data = plot_magnitude_droughts, aes(x = year, y = magnitude_droughts)) +
  geom_line(color = "blue") +
  labs(title = "Magnitude of Droughts",
       x = "Year") +
  theme_minimal()

#Floods
plot_magnitude_floods <- magnitude_floods %>%
  group_by(year) %>%
  filter(year > 2006) %>%
  summarise(magnitude_floods = sum(magnitude_floods))

plot_magnitude_floods <- ggplot(data = plot_magnitude_floods, aes(x = year, y = magnitude_floods)) +
  geom_line(color = "blue") +
  labs(title = "Magnitude of Floods",
       x = "Year") +
  theme_minimal()

#Combined plots of weather variables ----
combined <- grid.arrange(plot_frequency_droughts, plot_frequency_floods, 
                      plot_duration_droughts, plot_duration_floods,
                      plot_magnitude_droughts, plot_magnitude_floods,
                      ncol = 2)

# 8) Regressions ---------------------------------------------------------------


# 8.1) Gradually adding fixed effects ----
#OLS
lm <- feols(IHS_flow_rates ~ SPEI + log(distance) + border | mvsw(year, origin, destination, destination^year, origin^destination), migration)
etable(lm, cluster = "origin")

#Poisson
g <- fepois(migrates ~ SPEI + log(distance) + border | mvsw(year, origin, destination, destination^year, origin^destination), migration)
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
g <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + border + log(distance)| mvsw(origin, destination^year, origin^destination), migration)
etable(g, vcluster = "origin")


#Checking for best estimation method. Sensitivity analysis. PPML vs OLS.
lm <- feols(c(IHS_flow_rates, LN1_flow_rates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration)
g <- fepois(migrates ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration)

lm2 <- feols(c(IHS_flow_rates, LN1_flow_rates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_wo_zeros)
g2 <- fepois(migrates ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_wo_zeros)

lm3 <- feols(c(IHS_flow_rates, LN1_flow_rates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_wo_wp)
g3 <- fepois(migrates ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_wo_wp)

lm4 <- feols(c(IHS_flow_rates, LN1_flow_rates) ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_outliers)
g4 <- fepois(migrates ~ SPEI + log(distance) + border | csw(origin + destination^year, origin^destination), migration_outliers)


etable(lm, lm2, lm3, lm4, cluster = "origin")
etable(g, g2, g3, g4, cluster = "origin")


# 8.3) Heterogeneous effects ---------------------------------------------------
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

#fixest test ----
lm <- feols(c(IHS_flow_rates, migrates) ~ SPEI + federal | origin + destination^year + origin^destination, migration)
etable(lm, vcov = "hetero")

g <- fepois(migrates ~ SPEI | origin + destination^year + origin^destination, migration)
etable(g, vcov = "hetero")


