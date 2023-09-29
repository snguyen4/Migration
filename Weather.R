# 0) Loading libraries ---------------------------------------------------------
#-------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  stars, # spatiotemporal data handling
  raster, # raster data handling
  terra, # raster data handling
  sf, # vector data handling
  dplyr, # data wrangling
  stringr, # string manipulation
  lubridate, # dates handling
  data.table, # data wrangling
  patchwork, # arranging figures
  tigris, # county border
  colorspace, # color scale
  viridis, # arranging figures
  tidyr, # reshape
  ggspatial, # north arrow and scale bar
  ggplot2, # make maps
  ncdf4, # handle netcdf files
  readxl,# handle excel files
  openxlsx, # write excel files
  ggthemes, # themes
  circlize, #circular plot
  SPEI, # SPI
  gridExtra, # grids for plots
  lmtest, #for coeftest
  sandwich, #for vcovHC
  rgeos, #geometric operations
  gravity #PPML estimator

  )

# 1) Loading data --------------------------------------------------------------
#-------------------------------------------------------------------------------

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
  

# 2) Working with weather data -------------------------------------------------
#-------------------------------------------------------------------------------

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

#rounding to 2 decimals
for (col in names(SPEI_by_state)) {
  if (col != "date" && is.numeric(SPEI_by_state[[col]])) {
    SPEI_by_state[, col] <- round(SPEI_by_state[, col], 2)
  }
}

#-------------------------------------------------------------------------------

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
# 
#Splitting years and months
SPEI_by_state <- SPEI_by_state %>%
  separate(date, into = c("year", "month"), sep = "/")

#Yearly SPI calculation
SPEI_by_state <- SPEI_by_state %>%
  select(-month) %>%
  group_by(year) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

#Creating subset of the data for the years 2006-2019
SPEI_by_state <- SPEI_by_state %>%
  filter(year >= 2008 & year < 2020)


#Adding SPI at origin state to migration dataset
#Transformation to long format
SPEI_by_state <- SPEI_by_state %>%
  gather(key = "state", value = "SPEI", -year)

# Convert 'year' in spi_long to numeric
SPEI_by_state <- SPEI_by_state %>%
  mutate(year = as.numeric(year))

#Merge datasets
migration <- migration %>%
  left_join(SPEI_by_state, by = c("origin" = "state", "year"))


# 3) Working with geographical data --------------------------------------------
#-------------------------------------------------------------------------------

#Distance between states
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

#-------------------------------------------------------------------------------

# #Borders
# #Check spatial relationships and create adjacency matrix
# adj_matrix <- st_touches(MY_sf)
# 
# adj_matrix <- adj_matrix %>%
#   as.data.frame()
# 
# #changing numbers to state names
# state_names <- c(MY_sf$NAME_1)
# 
# adj_matrix$row.id <- state_names[adj_matrix$row.id]
# adj_matrix$col.id <- state_names[adj_matrix$col.id]
# 
# #Renaming columns
# adj_matrix <- adj_matrix %>%
#   rename("state" = row.id,
#          "bstate" = col.id)
# 
# # Create an empty vector to store the border values
# migration$border <- numeric(nrow(migration))
# 
# # Iterate over each row of migration data and assign the border value
# for (i in 1:nrow(migration)) {
#   origin <- migration$origin[i]
#   destination <- migration$destination[i]
#   
#   # Check if origin and destination share a border in adj_matrix
#   if (destination %in% adj_matrix[adj_matrix[,1] == origin, 2]) {
#     migration$border[i] <- 1  # Set border value to 1 if they share a border
#   } else {
#     migration$border[i] <- 0  # Set border value to 0 if they do not share a border
#   }
# }


# 4) Working with sociodemographic data ----------------------------------------
#-------------------------------------------------------------------------------

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
#-------------------------------------------------------------------------------

# #Kuala Lumpur + Putrajaya merged 
# #Quickfix of renaming Putrajaya Kuala Lumpur
# 
# migration_wo_PJY <- migration %>%
#   filter(origin != "Putrajaya" & destination != "Putrajaya")
# 
# #Adding GDP to the dataset
# migration_wo_PJY <- migration_wo_PJY %>%
#   left_join(GDP, by = c("origin" = "state", "year"))
# 
# migration_wo_PJY <- migration_wo_PJY %>%
#   rename("wi" = "GDPpC")
# 
# migration_wo_PJY <- migration_wo_PJY %>%
#   left_join(GDP, by = c("destination" = "state", "year"))
# 
# migration_wo_PJY <- migration_wo_PJY %>%
#   rename("wj" = "GDPpC")
# 
# #Creating wage differential variables
# migration_wo_PJY <- migration_wo_PJY %>%
#   mutate(wdiff = log(wi / wj))
# 
# # Creating Niit
# migration_wo_PJY <- migration_wo_PJY %>%
#   group_by(origin, year) %>%
#   mutate(Niit = pop - sum(flow))
# 
# #Creating lnmig
# 
# migration_wo_PJY <- migration_wo_PJY %>%
#   mutate(lnmig = log(flow / Niit))



# 6) Descriptive statistics ----------------------------------------------------
#------------------------------------------------------------------------------- 
 
# #Creating histogram of migration flows
# ggplot(migration, aes(x = flow)) +
#   geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
#   labs(title = "Histogram of Migration Flows", x = "Flows", y = "Frequency") +
#   theme_minimal()
# 
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


#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
  
# #Combining plots SPI + out-migration 2006
# combined_plot_out <- grid.arrange(plot_out_2006, plot_out_2019, ncol = 1)
# combined_plot_out
# 
# #2019
# combined_plot_spi <- grid.arrange(plot_spi_2006, plot_spi_2019, ncol = 1)
# combined_plot_spi

#-------------------------------------------------------------------------------
  
# #circular migration flow plot
# # Aggregate flows by origin and destination
# migration_flows <- aggregate(flow ~ origin + destination, data = migration, sum)
# 
# migration_flows <- migration_flows %>%
#   filter(flow >= 20)
# 
# # Create migration matrix
# migration_matrix <- with(migration_flows, table(origin, destination, flow))
# 
# # Set up the plot
# circos.par(cell.padding = c(0.02, 0.02))
# circos.initialize(factors = unique(c(rownames(migration_matrix), colnames(migration_matrix))), xlim = c(0, 1))
# 
# # Add the chord diagram
# chordDiagram(migration_matrix)
# 
# 
# #Checking flows from Selangor
# Selangor_flows <- migration[migration$origin != migration$destination, ] %>%
#   filter(origin == "Selangor")

# 7) Preparation of the database for regressions -------------------------------
#-------------------------------------------------------------------------------
#Fixed effects dummy creation---------------------------------------------------
#origin fixed effects
migration$origin_fe <- factor(migration$origin)

#destination fixed effects
migration$destination_fe <- factor(migration$destination)

#time fixed effects
migration$year_fe <- factor(migration$year)

#Using inverse hyperbolic sine (IHS) to account for zeros- OLS------------------
migration <- migration %>%
  mutate(IHS_flow = log(flow + (flow^2 + 1)^0.5))


# 8) Regressions ---------------------------------------------------------------
#-------------------------------------------------------------------------------

#OLS----------------------------------------------------------------------------
#Base specification
lm1 <- lm(IHS_flow ~ SPEI + origin_fe:year_fe + origin_fe:destination_fe, 
          data = migration)
# Compute heteroscedastic-robust standard errors
robust_se <- vcovHC(lm1, type = "HC1")
#Regression using robust errors, clustered at the destination level
lm1_robust <- coeftest(lm1, vcov. = robust_se, cluster = migration$destination)
lm1_robust


#PPML --------------------------------------------------------------------------
#Base specification
ppml1 <- glm(IHS_flow ~ SPEI + origin_fe:year_fe + origin_fe:destination_fe, 
             data = migration,
             family = "quasipoisson")
# Compute heteroscedastic-robust standard errors
robust_se <- vcovHC(ppml1, type = "HC1")
#Regression using robust errors, clustered at the destination level
ppml1_robust <- coeftest(ppml1, vcov. = robust_se, cluster = migration$destination)
ppml1_robust
