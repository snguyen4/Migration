
#Dallmann 2017 weather variables -----------------------------------------------
# 2.1.1) Variation: SPEI > +- 1 ------------------------------------------------

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
frequency_floods <- SPEI_by_state_month %>%
  mutate(count = ifelse(SPEI >= 1, 1, 0))


frequency_floods <- frequency_floods %>%
  group_by(state, year) %>%
  summarize(count = sum(count))

# Create a new variable 'frequency_flood' with the sum of counts for the 5 preceding years
#If I only want the 5 years preceding the current year, modify the 5 to 6 and subtract it by count
frequency_floods <- frequency_floods %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(frequency_floods = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  ungroup()

# Removing count
frequency_floods <- frequency_floods %>%
  select(-count) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(frequency_floods, by = c("origin" = "state", "year"))

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

# 2.1.2) Variation: SPEI > +-1.5 -------------------------------------------------------

#value of 1 if SPEI > +-1.5 in a month and zero otherwise
frequency_intense <- SPEI_by_state_month %>%
  mutate(count = ifelse(abs(SPEI) >= 1.5, 1, 0))


frequency_intense <- frequency_intense %>%
  group_by(state, year) %>%
  summarize(count = sum(count))

# Create a new variable 'frequency_intense' with the sum of counts for the 5 preceding years
#If I only want the 5 years preceding the current year, modify the 5 to 6 and substract it by count
frequency_intense <- frequency_intense %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(frequency_intense = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  ungroup()

# Removing count
frequency_intense <- frequency_intense %>%
  select(-count) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(frequency_intense, by = c("origin" = "state", "year"))

#Drought frequency ----
#value of 1 if SPEI < -1 in a month and zero otherwise
frequency_droughts_intense <- SPEI_by_state_month %>%
  mutate(count = ifelse(SPEI <= -1.5, 1, 0))


frequency_droughts_intense <- frequency_droughts_intense %>%
  group_by(state, year) %>%
  summarize(count = sum(count))

# Create a new variable 'frequency_droughts_intense' with the sum of counts for the 5 preceding years
#If I only want the 5 years preceding the current year, modify the 5 to 6 and subtract it by count
frequency_droughts_intense <- frequency_droughts_intense %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(frequency_droughts_intense = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  ungroup()

# Removing count
frequency_droughts_intense <- frequency_droughts_intense %>%
  select(-count) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(frequency_droughts_intense, by = c("origin" = "state", "year"))


#Flood frequency ----
#value of 1 if SPEI  >= 1 in a month and zero otherwise
frequency_floods_intense <- SPEI_by_state_month %>%
  mutate(count = ifelse(SPEI >= 1.5, 1, 0))


frequency_floods_intense <- frequency_floods_intense %>%
  group_by(state, year) %>%
  summarize(count = sum(count))

# Create a new variable 'frequency_flood' with the sum of counts for the 5 preceding years
#If I only want the 5 years preceding the current year, modify the 5 to 6 and subtract it by count
frequency_floods_intense <- frequency_floods_intense %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(frequency_floods_intense = rollsum(count, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  ungroup()

# Removing count
frequency_floods_intense <- frequency_floods_intense %>%
  select(-count) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(frequency_floods_intense, by = c("origin" = "state", "year"))

#Maximal duration --------------------------------------------------------------
#max duration in nb of months of a drought or flood in the 5 years preceding migration
duration_intense <- SPEI_by_state_month %>%
  mutate(count = ifelse(abs(SPEI) >= 1.5, 1, 0))

#Counting the number of consecutive 1s
duration_intense <- duration_intense %>%
  group_by(state) %>%
  mutate(duration_intense = sequence(rle(count)$lengths) * count) %>%
  ungroup()

#Keeping the highest value per year
duration_intense <- duration_intense %>%
  group_by(state, year) %>%
  slice(which.max(duration_intense)) %>%
  select(year, state, duration_intense)

# Calculate the rolling maximum duration over the past 5 years
duration_intense <- duration_intense %>%
  arrange(state, year) %>%
  select(state, year, duration_intense) %>%
  group_by(state) %>%
  mutate(
    max_duration_intense = zoo::rollapply(duration_intense, width = 5, FUN = max, fill = NA, align = "right")
  ) %>%
  ungroup()

# Removing duration
duration_intense <- duration_intense %>%
  select(-duration_intense) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(duration_intense, by = c("origin" = "state", "year"))

#Max duration droughts ----
duration_droughts_intense <- SPEI_by_state_month %>%
  mutate(count = ifelse(SPEI <= -1.5, 1, 0))

#Counting the number of consecutive 1s
duration_droughts_intense <- duration_droughts_intense %>%
  group_by(state) %>%
  mutate(duration_droughts_intense = sequence(rle(count)$lengths) * count) %>%
  ungroup()

#Keeping the highest value per year
duration_droughts_intense <- duration_droughts_intense %>%
  group_by(state, year) %>%
  slice(which.max(duration_droughts_intense)) %>%
  select(year, state, duration_droughts_intense)

# Calculate the rolling maximum duration_droughts_intense over the past 5 years
duration_droughts_intense <- duration_droughts_intense %>%
  arrange(state, year) %>%
  select(state, year, duration_droughts_intense) %>%
  group_by(state) %>%
  mutate(
    max_duration_droughts_intense = zoo::rollapply(duration_droughts_intense, width = 5, FUN = max, fill = NA, align = "right")
  ) %>%
  ungroup()

# Removing duration_droughts_intense
duration_droughts_intense <- duration_droughts_intense %>%
  select(-duration_droughts_intense) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(duration_droughts_intense, by = c("origin" = "state", "year"))

# Max duration floods ----
duration_floods_intense <- SPEI_by_state_month %>%
  mutate(count = ifelse(SPEI >= 1.5, 1, 0))

#Counting the number of consecutive 1s
duration_floods_intense <- duration_floods_intense %>%
  group_by(state) %>%
  mutate(duration_floods_intense = sequence(rle(count)$lengths) * count) %>%
  ungroup()

#Keeping the highest value per year
duration_floods_intense <- duration_floods_intense %>%
  group_by(state, year) %>%
  slice(which.max(duration_floods_intense)) %>%
  select(year, state, duration_floods_intense)

# Calculate the rolling maximum duration_floods_intense over the past 5 years
duration_floods_intense <- duration_floods_intense %>%
  arrange(state, year) %>%
  select(state, year, duration_floods_intense) %>%
  group_by(state) %>%
  mutate(
    max_duration_floods_intense = zoo::rollapply(duration_floods_intense, width = 5, FUN = max, fill = NA, align = "right")
  ) %>%
  ungroup()

# Removing duration_floods_intense
duration_floods_intense <- duration_floods_intense %>%
  select(-duration_floods_intense) %>%
  mutate(year = as.numeric(year))

#Adding it to the migration dataset
migration <- migration %>%
  left_join(duration_floods_intense, by = c("origin" = "state", "year"))

# Magnitude --------------------------------------------------------------------
#sum per year of SPEI
magnitude_intense <- SPEI_by_state_month %>%
  group_by(state, year) %>%
  summarise(sum_spei = sum(ifelse(abs(SPEI) >= 1.5, abs(SPEI), 0)))

#sum of the preceding 5 years
magnitude_intense <- magnitude_intense %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(magnitude_intense = rollsum(sum_spei, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  select(-sum_spei) %>%
  ungroup()

#Adding it to the migration dataset
migration <- migration %>%
  left_join(magnitude_intense, by = c("origin" = "state", "year"))

# Magnitude droughts ----
magnitude_droughts_intense <- SPEI_by_state_month %>%
  group_by(state, year) %>%
  summarise(sum_spei = sum(ifelse(SPEI <= -1.5, abs(SPEI), 0)))

#sum of the preceding 5 years
magnitude_droughts_intense <- magnitude_droughts_intense %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(magnitude_droughts_intense = rollsum(sum_spei, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  select(-sum_spei) %>%
  ungroup()

#Adding it to the migration dataset
migration <- migration %>%
  left_join(magnitude_droughts_intense, by = c("origin" = "state", "year"))


# Magnitude floods ----
magnitude_floods_intense <- SPEI_by_state_month %>%
  group_by(state, year) %>%
  summarise(sum_spei = sum(ifelse(SPEI >= 1.5, abs(SPEI), 0)))

#sum of the preceding 5 years
magnitude_floods_intense <- magnitude_floods_intense %>%
  arrange(state, year) %>%  # Sort the data by 'state' and 'year'
  group_by(state) %>%      # Group by 'state'
  mutate(magnitude_floods_intense = rollsum(sum_spei, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
  select(-sum_spei) %>%
  ungroup()

#Adding it to the migration dataset
migration <- migration %>%
  left_join(magnitude_floods_intense, by = c("origin" = "state", "year"))

# #Plots of frequencies ----
# #Droughts
# plot_frequency_droughts <- frequency_droughts %>%
#   group_by(year) %>%
#   filter(year > 2006) %>%
#   summarise(frequency_droughts = sum(frequency_droughts))
# 
# plot_frequency_droughts <- ggplot(data = plot_frequency_droughts, aes(x = year, y = frequency_droughts)) +
#   geom_line(color = "blue") +
#   labs(title = "Drought Frequency",
#        x = "Year") +
#   theme_minimal()
# 
# #Floods
# plot_frequency_floods <- frequency_floods %>%
#   group_by(year) %>%
#   filter(year > 2006) %>%
#   summarise(frequency_floods = sum(frequency_floods))
# 
# plot_frequency_floods <- ggplot(data = plot_frequency_floods, aes(x = year, y = frequency_floods)) +
#   geom_line(color = "blue") +
#   labs(title = "Flood Frequency",
#        x = "Year") +
#   theme_minimal()

# 
# #Plots of max durations ---
# 
# #Droughts
# plot_duration_droughts <- duration_droughts %>%
#   group_by(year) %>%
#   filter(year > 2006) %>%
#   summarise(max_duration_droughts = sum(max_duration_droughts))
# 
# plot_duration_droughts <- ggplot(data = plot_duration_droughts, aes(x = year, y = max_duration_droughts)) +
#   geom_line(color = "blue") +
#   labs(title = "Maximum Duration of Droughts",
#        x = "Year") +
#   theme_minimal()
# 
# #Floods
# plot_duration_floods <- duration_floods %>%
#   group_by(year) %>%
#   filter(year > 2006) %>%
#   summarise(max_duration_floods = sum(max_duration_floods))
# 
# plot_duration_floods <- ggplot(data = plot_duration_floods, aes(x = year, y = max_duration_floods)) +
#   geom_line(color = "blue") +
#   labs(title = "Maximum Duration of Floods",
#        x = "Year") +
#   theme_minimal()
# 
# #Plots of magnitude ----
# 
# #Droughts
# plot_magnitude_droughts <- magnitude_droughts %>%
#   group_by(year) %>%
#   filter(year > 2006) %>%
#   summarise(magnitude_droughts = sum(magnitude_droughts))
# 
# plot_magnitude_droughts <- ggplot(data = plot_magnitude_droughts, aes(x = year, y = magnitude_droughts)) +
#   geom_line(color = "blue") +
#   labs(title = "Magnitude of Droughts",
#        x = "Year") +
#   theme_minimal()
# 
# #Floods
# plot_magnitude_floods <- magnitude_floods %>%
#   group_by(year) %>%
#   filter(year > 2006) %>%
#   summarise(magnitude_floods = sum(magnitude_floods))
# 
# plot_magnitude_floods <- ggplot(data = plot_magnitude_floods, aes(x = year, y = magnitude_floods)) +
#   geom_line(color = "blue") +
#   labs(title = "Magnitude of Floods",
#        x = "Year") +
#   theme_minimal()
# 
# #Combined plots of weather variables ----
# combined <- grid.arrange(plot_frequency_droughts, plot_frequency_floods, 
#                       plot_duration_droughts, plot_duration_floods,
#                       plot_magnitude_droughts, plot_magnitude_floods,
#                       ncol = 2)


#SPEI --------------------------------------------------------------------------
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


#Interaction alternatives
#Value added of agriculture as a share of GDP ----------------------------------
GDP <- read.csv("c:/Users/samue/Desktop/Dissertation/Migration/Data/GDP.csv")

putrajaya_gdp <- tibble(
  Year = 2010:2019,
  GDP_agri_per = 0,
  State = "Putrajaya"
)

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
  mutate(GDP_agri_quartile = as.factor(ntile(GDP_agri_per, 4)),
         GDP_agri_tertile = as.factor(ntile(GDP_agri_per, 3)),
         threshold_35 = quantile(GDP_agri_per, probs = 0.65),
         threshold_30 = quantile(GDP_agri_per, probs = 0.7),
         threshold_25 = quantile(GDP_agri_per, probs = 0.75),
         threshold_20 = quantile(GDP_agri_per, probs = 0.8),
         threshold_15 = quantile(GDP_agri_per, probs = 0.85),
         threshold_10 = quantile(GDP_agri_per, probs = 0.9),
         threshold_05 = quantile(GDP_agri_per, probs = 0.95),
         GDP_agri_35 = ifelse(GDP_agri_per >= threshold_35, 1, 0),
         GDP_agri_30 = ifelse(GDP_agri_per >= threshold_30, 1, 0),
         GDP_agri_25 = ifelse(GDP_agri_per >= threshold_25, 1, 0),
         GDP_agri_20 = ifelse(GDP_agri_per >= threshold_20, 1, 0),
         GDP_agri_15 = ifelse(GDP_agri_per >= threshold_15, 1, 0),
         GDP_agri_10 = ifelse(GDP_agri_per >= threshold_10, 1, 0),
         GDP_agri_05 = ifelse(GDP_agri_per >= threshold_05, 1, 0),
         middle_poor_dummy = ifelse(GDP_agri_quartile != 4, 1, 0)) %>%
  arrange(Year, GDP_agri_quartile) %>%
  select(-threshold_35, -threshold_30, -threshold_25, -threshold_20, -threshold_15, -threshold_10, -threshold_05)


#Merging with migration dataset
migration <- migration %>%
  mutate(year = as.numeric(year)) %>%
  left_join(GDP, by = c("origin" = "State", "year" = "Year")) 

#Destination
GDP_destination <- GDP %>%
  select(Year, State, GDP_agri_per) %>%
  mutate(GDP_agri_destination_quartile = as.factor(ntile(GDP_agri_per, 4)),
         GDP_agri_rich_destination = ifelse(GDP_agri_destination_quartile == 1, 1, 0)) %>%
  select(-GDP_agri_per)

#Merging
migration <- migration %>%
  mutate(year = as.numeric(year)) %>%
  left_join(GDP_destination, by = c("destination" = "State", "year" = "Year")) 


#Urbanization rate -------------------------------------------------------------
#opening excel file
urban <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/Urbanisation2010.xlsx")

urban <- urban %>%
  filter(year == 2010) %>%
  mutate(rural = 100 - urban)

#Thresholds
threshold_35 <- quantile(urban$rural, probs = 0.65)
threshold_30 <- quantile(urban$rural, probs = 0.7)
threshold_25 <- quantile(urban$rural, probs = 0.75)
threshold_20 <- quantile(urban$rural, probs = 0.8)
threshold_15 <- quantile(urban$rural, probs = 0.85)
threshold_10 <- quantile(urban$rural, probs = 0.9)
threshold_05 <- quantile(urban$rural, probs = 0.95)


urban <- urban %>%
  group_by(year) %>%
  mutate(rural_quartile = as.factor(ntile(rural, 4)),
         rural_tertile = as.factor(ntile(rural, 3)),
         rural_35 = ifelse(rural >= threshold_35, 1, 0),
         rural_30 = ifelse(rural >= threshold_30, 1, 0),
         rural_25 = ifelse(rural >= threshold_25, 1, 0),
         rural_20 = ifelse(rural >= threshold_20, 1, 0),
         rural_15 = ifelse(rural >= threshold_15, 1, 0),
         rural_10 = ifelse(rural >= threshold_10, 1, 0),
         rural_05 = ifelse(rural >= threshold_05, 1, 0)) %>%
  arrange(state, year) %>%
  ungroup() %>%
  select(-year, -urban)

#Merging with migration dataset
migration <- migration %>%
  left_join(urban, by = c("origin" = "state"))


#destination
urban_destination <- urban %>%
  select(state, rural_quartile) %>%
  mutate(urban_destination = ifelse(rural_quartile == 1, 1, 0)) %>%
  select(-rural_quartile)

migration <- migration %>%
  left_join(urban_destination, by = c("destination" = "state"))

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
  mutate(agri_employment_quartile = as.factor(ntile(agri, 4)),
         agri_employment_tertile = as.factor(ntile(agri, 3)),
         threshold_35 = quantile(agri, probs = 0.65),
         threshold_30 = quantile(agri, probs = 0.7),
         threshold_25 = quantile(agri, probs = 0.75),
         threshold_20 = quantile(agri, probs = 0.8),
         threshold_15 = quantile(agri, probs = 0.85),
         threshold_10 = quantile(agri, probs = 0.9),
         threshold_05 = quantile(agri, probs = 0.95),
         agri_35 = ifelse(agri >= threshold_35, 1, 0),
         agri_30 = ifelse(agri >= threshold_30, 1, 0),
         agri_25 = ifelse(agri >= threshold_25, 1, 0),
         agri_20 = ifelse(agri >= threshold_20, 1, 0),
         agri_15 = ifelse(agri >= threshold_15, 1, 0),
         agri_10 = ifelse(agri >= threshold_10, 1, 0),
         agri_05 = ifelse(agri >= threshold_05, 1, 0)) %>%
  select(-threshold_35, -threshold_30, -threshold_25, -threshold_20, -threshold_15, -threshold_10, -threshold_05)


#Merging with migration dataset
migration <- migration %>%
  left_join(agri, by = c("origin" = "state", "year")) 


#Income ------------------------------------------------------------------------
income <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/income.xlsx")

income <- income %>%
  filter(year == 2012) %>%
  mutate(income = as.numeric(income))

#Thresholds
threshold_35 <- quantile(income$income, probs = 0.35)
threshold_30 <- quantile(income$income, probs = 0.30)
threshold_25 <- quantile(income$income, probs = 0.25)
threshold_20 <- quantile(income$income, probs = 0.20)
threshold_15 <- quantile(income$income, probs = 0.15)
threshold_10 <- quantile(income$income, probs = 0.10)
threshold_05 <- quantile(income$income, probs = 0.05)


income <- income %>%
  mutate(income = as.numeric(income)) %>%
  group_by(year) %>%
  mutate(income_quartile = as.factor(ntile(income, 4)),
         income_tertile = as.factor(ntile(income, 3)),
         income_35 = ifelse(income <= threshold_35, 1, 0),
         income_30 = ifelse(income <= threshold_30, 1, 0),
         income_25 = ifelse(income <= threshold_25, 1, 0),
         income_20 = ifelse(income <= threshold_20, 1, 0),
         income_15 = ifelse(income <= threshold_15, 1, 0),
         income_10 = ifelse(income <= threshold_10, 1, 0),
         income_05 = ifelse(income <= threshold_05, 1, 0)) %>%
  arrange(state, year) %>%
  ungroup() %>%
  select(-year)

migration <- migration %>%
  left_join(income, by = c("origin" = "state"))

#Destination
income_destination <- income %>%
  select(state, income_quartile) %>%
  mutate(income_destination_rich = ifelse(income_quartile == 4, 1, 0)) %>%
  select(-income_quartile)

migration <- migration %>%
  left_join(income_destination, by = c("destination" = "state"))

#Hospitals ---------------------------------------------------------------------

hospitals <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/hospitals.xlsx")


hospitals <- hospitals %>%
  filter(year >= 2010 & year <= 2019) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(pop, by = c("state", "year")) %>%
  filter(!is.na(pop)) %>%
  group_by(year) %>%
  mutate(beds_p = beds / pop,
         beds_quartile = as.factor(ntile(beds_p, 4)),
         beds_tertile = as.factor(ntile(beds_p, 3)),
         beds_4 = ifelse(beds_quartile == 1, 1, 0),
         beds_3 = ifelse(beds_tertile == 1, 1, 0)) %>%
  select(-pop)

migration <- migration %>%
  mutate(year = as.numeric(year)) %>%
  left_join(hospitals, by = c("origin" = "state", "year"))

#Descriptiptive stats ----------------------------------------------------------

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

# Create bins for SPEI values ----
migration <- migration %>%
  mutate(SPEI_category = cut(SPEI, breaks = seq(-2.5, 3, by = 0.5)))


# 9) Regressions tests ---------------------------------------------------------


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

# #Rural ----
# g <- fepois(migrates ~ sw(SPEI:rural_quartile, SPEI_growing_main:rural_quartile, SPEI_growing_off:rural_quartile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# g <- fepois(migrates ~ SPEI:rural_quartile + SPEI_droughts_intense:rural_quartile + SPEI_floods_intense:rural_quartile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g2 <- fepois(migrates ~ SPEI_growing_main:rural_quartile + SPEI_droughts_main:rural_quartile + SPEI_floods_main:rural_quartile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g3 <- fepois(migrates ~ SPEI_growing_off:rural_quartile + SPEI_droughts_off:rural_quartile + SPEI_floods_off:rural_quartile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# 
# etable(g, g2, g3, cluster = "origin")
# 
# #Dummies
# 
# g <- fepois(migrates ~ SPEI + SPEI:rural + sw0(SPEI_droughts_intense + SPEI_droughts_intense:rural + SPEI_floods_intense + SPEI_floods_intense:rural) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g2 <- fepois(migrates ~ SPEI_growing_main + SPEI_growing_main:rural + sw0(SPEI_droughts_main + SPEI_droughts_main:rural) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g3 <- fepois(migrates ~ SPEI_growing_off + SPEI_growing_off:rural + sw0(SPEI_droughts_off + SPEI_droughts_off:rural + SPEI_floods_off + SPEI_floods_off:rural) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# 
# etable(g, g2, g3, cluster = "origin")

# #Agricultural dependence ----
# g <- fepois(migrates ~ sw(SPEI:agri_quartile, SPEI_growing_main:agri_quartile, SPEI_growing_off:agri_quartile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# etable(g, cluster = "origin")
# 
# g <- fepois(migrates ~ SPEI:agri_quartile + SPEI_droughts_intense:agri_quartile + SPEI_floods_intense:agri_quartile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g2 <- fepois(migrates ~ SPEI_growing_main:agri_quartile + SPEI_droughts_main:agri_quartile + SPEI_floods_main:agri_quartile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g3 <- fepois(migrates ~ SPEI_growing_off:agri_quartile + SPEI_droughts_off:agri_quartile + SPEI_floods_off:agri_quartile |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# 
# etable(g, g2, g3, cluster = "origin")
# 
# #Dummies
# 
# g <- fepois(migrates ~ SPEI + SPEI:agri + sw0(SPEI_droughts_intense + SPEI_droughts_intense:agri + SPEI_floods_intense + SPEI_floods_intense:agri) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g2 <- fepois(migrates ~ SPEI_growing_main + SPEI_growing_main:agri + sw0(SPEI_droughts_main + SPEI_droughts_main:agri) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# g3 <- fepois(migrates ~ SPEI_growing_off + SPEI_growing_off:agri + sw0(SPEI_droughts_off + SPEI_droughts_off:agri + SPEI_floods_off + SPEI_floods_off:agri) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
# 
# etable(g, g2, g3, cluster = "origin")

#Thresholds regressions --------------------------------------------------------
#Value added of agri as share of GDP
g <- fepois(migrates ~ SPEI + SPEI:GDP_agri_30 + SPEI_droughts_intense + SPEI_droughts_intense:GDP_agri_30 + SPEI_floods_intense + SPEI_floods_intense:GDP_agri_30 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI + SPEI:GDP_agri_20 + SPEI_droughts_intense + SPEI_droughts_intense:GDP_agri_20 + SPEI_floods_intense + SPEI_floods_intense:GDP_agri_20 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI + SPEI:GDP_agri_10 + SPEI_droughts_intense + SPEI_droughts_intense:GDP_agri_10 + SPEI_floods_intense + SPEI_floods_intense:GDP_agri_10 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g4 <- fepois(migrates ~ SPEI + SPEI:GDP_agri_05 + SPEI_droughts_intense + SPEI_droughts_intense:GDP_agri_05 + SPEI_floods_intense + SPEI_floods_intense:GDP_agri_05 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, g4, cluster = "origin")
etable(g, g2, g3, g4, cluster = ~origin + destination)


#urbanisation
g <- fepois(migrates ~ SPEI + SPEI:rural_30 + SPEI_droughts_intense + SPEI_droughts_intense:rural_30 + SPEI_floods_intense + SPEI_floods_intense:rural_30 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI + SPEI:rural_20 + SPEI_droughts_intense + SPEI_droughts_intense:rural_20 + SPEI_floods_intense + SPEI_floods_intense:rural_20 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI + SPEI:rural_10 + SPEI_droughts_intense + SPEI_droughts_intense:rural_10 + SPEI_floods_intense + SPEI_floods_intense:rural_10 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g4 <- fepois(migrates ~ SPEI + SPEI:rural_05 + SPEI_droughts_intense + SPEI_droughts_intense:rural_05 + SPEI_floods_intense + SPEI_floods_intense:rural_05 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, g4, cluster = "origin")

#employment in agri
g <- fepois(migrates ~ SPEI + SPEI:agri_30 + SPEI_droughts_intense + SPEI_droughts_intense:agri_30 + SPEI_floods_intense + SPEI_floods_intense:agri_30 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI + SPEI:agri_20 + SPEI_droughts_intense + SPEI_droughts_intense:agri_20 + SPEI_floods_intense + SPEI_floods_intense:agri_20 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI + SPEI:agri_10 + SPEI_droughts_intense + SPEI_droughts_intense:agri_10 + SPEI_floods_intense + SPEI_floods_intense:agri_10 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g4 <- fepois(migrates ~ SPEI + SPEI:agri_05 + SPEI_droughts_intense + SPEI_droughts_intense:agri_05 + SPEI_floods_intense + SPEI_floods_intense:agri_05 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, g4, cluster = "origin")
etable(g, g2, g3, g4, cluster = ~origin + destination)


#income
g <- fepois(migrates ~ SPEI + SPEI:income_20 + SPEI_droughts_intense + SPEI_droughts_intense:income_20 + SPEI_floods_intense + SPEI_floods_intense:income_20 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI + SPEI:income_15 + SPEI_droughts_intense + SPEI_droughts_intense:income_15 + SPEI_floods_intense + SPEI_floods_intense:income_15 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g3 <- fepois(migrates ~ SPEI + SPEI:income_10 + SPEI_droughts_intense + SPEI_droughts_intense:income_10 + SPEI_floods_intense + SPEI_floods_intense:income_10 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g4 <- fepois(migrates ~ SPEI + SPEI:income_05 + SPEI_droughts_intense + SPEI_droughts_intense:income_05 + SPEI_floods_intense + SPEI_floods_intense:income_05 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)

etable(g, g2, g3, g4, cluster = "origin")
etable(g, g2, g3, g4, cluster = ~origin + destination)

#hospitals
g <- fepois(migrates ~ SPEI + SPEI:beds_4 + SPEI_droughts_intense + SPEI_droughts_intense:beds_4 + SPEI_floods_intense + SPEI_floods_intense:beds_4 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI + SPEI:beds_3 + SPEI_droughts + SPEI_droughts:beds_3 + SPEI_floods + SPEI_floods:beds_3 |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g, g2, cluster = "origin")
etable(g, g2, cluster = ~origin + destination)



#Destination states characteristics --------------------------------------------
g <- fepois(migrates ~ SPEI + SPEI:distance + SPEI_droughts_intense + SPEI_droughts_intense:distance + SPEI_floods_intense + SPEI_floods_intense:distance |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g, cluster = ~origin + destination)

g <- fepois(migrates ~ SPEI + SPEI:border + SPEI_droughts_intense + SPEI_droughts_intense:border + SPEI_floods_intense + SPEI_floods_intense:border |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g, cluster = ~origin + destination)


g <- fepois(migrates ~ SPEI + SPEI:GDP_agri_rich_destination + SPEI_droughts_intense + SPEI_droughts_intense:GDP_agri_rich_destination + SPEI_floods_intense + SPEI_floods_intense:GDP_agri_rich_destination |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g, cluster = ~origin + destination)

g <- fepois(migrates ~ SPEI + SPEI:income_destination_rich + SPEI_droughts_intense + SPEI_droughts_intense:income_destination_rich + SPEI_floods_intense + SPEI_floods_intense:income_destination_rich |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g, cluster = ~origin + destination)


g <- fepois(migrates ~ SPEI + SPEI:urban_destination + SPEI_droughts_intense + SPEI_droughts_intense:urban_destination + SPEI_floods_intense + SPEI_floods_intense:urban_destination |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g, cluster = ~origin + destination)

