
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
