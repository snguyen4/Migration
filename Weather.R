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
  rgeos #geometric operations
  )

# 1) Loading data --------------------------------------------------------------
#-------------------------------------------------------------------------------

#Loading NETCDF. Pre -> Precipitation (mm/month) 
nc_pre <- nc_open("c:/Users/samue/Desktop/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc")

#Loading CRU TS precipitation dataset into R
r <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc", subds = "pre")

#MY boundary
MY_sf <-
  st_read("c:/Users/samue/Desktop/Dissertation/Migration/Data/gadm41_MYS_shp/gadm41_MYS_1.shp") %>%
  st_as_sf() %>%
  st_transform(terra::crs(r))

MY_sf$NAME_1 <- ifelse(MY_sf$NAME_1 == "Trengganu", "Terengganu", MY_sf$NAME_1)
MY_sf$NAME_1[MY_sf$NAME_1 == "\t\nTerengganu"] <- "Terengganu"


#Loading internal migration flow data
migration <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/Migration.xlsx")

#Year of the report in the excel files -> documented year is the preceding year
migration$year = migration$year - 1


# 2) Working with weather data -------------------------------------------------
#-------------------------------------------------------------------------------

#Caclulating State averages
#Cropping raster layers
MY_r <- terra::crop(r, MY_sf)

#Converting to Spatvector
MY_sv <- vect(MY_sf)

#Extracting values from the raster for each state
pre_by_state <- terra::extract(MY_r, MY_sv, fun = "mean", na.rm = TRUE)

#Deleting ID column
pre_by_state <- select(pre_by_state, -ID)

#Changing row names to states.
rownames(pre_by_state) <- MY_sf$NAME_1

#Inverting columns and rows
pre_by_state <- pre_by_state %>%
  t() %>%
  as.data.frame() 

#Adding date column
start_date <- as.Date("1901-1-1")
end_date <- as.Date("2022-12-1")
dates <- seq(start_date, end_date, by = "month")
dates <- format(dates, "%Y/%m")
pre_by_state <- pre_by_state %>%
  mutate(date = dates) %>%
  relocate(date, .before = 1) 

row.names(pre_by_state) <- NULL # resetting row names to initial values.

#rounding to 2 decimals
for (col in names(pre_by_state)) {
  if (col != "date" && is.numeric(pre_by_state[[col]])) {
    pre_by_state[, col] <- round(pre_by_state[, col], 2)
  }
}

#Writing to xlsx
write.xlsx(pre_by_state, "Data/pre_by_state.xlsx")

#-------------------------------------------------------------------------------

#Calculating 12 month SPI for all 16 states

#Wide to long transformation
long_data <- pre_by_state %>%
  gather(key = "state", value = "precipitation", -date)

spi12 <- spi(long_data$precipitation, 12)

spi_my <- data.frame(matrix(spi12$fitted, ncol = 16))

colnames(spi_my) <- MY_sf$NAME_1

spi_my <- spi_my %>%
  mutate(date = dates) %>%
  relocate(date, .before = 1) 

#Splitting years and months
spi_my <- spi_my %>%
  separate(date, into = c("year", "month"), sep = "/")

#Yearly SPI calculation
spi_year <- spi_my %>%
  select(-month) %>%
  group_by(year) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

#Creating subset of the data for the years 2006-2019
spi_year <- spi_year %>%
  filter(year >= 2006 & year < 2020)

# #Plotting weather + shp
# plot(MY_r$pre_1)
# plot(MY_sv, add = TRUE)

# 3) Working with geographical data --------------------------------------------
#-------------------------------------------------------------------------------

#Distance between states
#Converting geometries of the states into point gemoetries
points_MY <- st_centroid(MY_sf)

#Calculating distance matrix between the points
dist_matrix <- st_distance(points_MY)

dist_matrix <- dist_matrix %>%
  as.data.frame()

rownames(dist_matrix) <- MY_sf$NAME_1
colnames(dist_matrix) <- MY_sf$NAME_1

#-------------------------------------------------------------------------------

#Borders
#Check spatial relationships and create adjency matrix
adj_matrix <- st_touches(MY_sf)

adj_matrix <- adj_matrix %>%
  as.data.frame()

rownames(adj_matrix) <- MY_sf$NAME_1
colnames(adj_matrix) <- MY_sf$NAME_1


# 4) Working with sociodemographic data ----------------------------------------
#-------------------------------------------------------------------------------

# 5) Descriptive statistics ----------------------------------------------------
#------------------------------------------------------------------------------- 
 
#Creating histogram of migration flows
ggplot(migration, aes(x = flow)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Migration Flows", x = "Flows", y = "Frequency") +
  theme_minimal()

#Creating an out-migration map.
origin_flows <- aggregate(flow ~ origin + year, data = migration[migration$origin != migration$destination, ], FUN = sum)

merged_data_out <- merge(MY_sf, origin_flows, by.x = "NAME_1", by.y = "origin", all.x = TRUE)


#2006 out-migration map
out_2006 <- merged_data_out %>%
  filter(year == 2006)

plot_out_2006 <- ggplot() +
  geom_sf(data = out_2006, aes(fill = flow)) +
  labs(title = "Out-Migration by State",
       subtitle = "2006") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = NA) +
  theme_void()

#2019 out-migration map
out_2019 <- merged_data_out %>%
  filter(year == 2019)

plot_out_2019 <- ggplot() +
  geom_sf(data = out_2019, aes(fill = flow)) +
  labs(title = "Out-Migration by State",
       subtitle = "2019") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = NA) +
  theme_void()


#-------------------------------------------------------------------------------

#Making a map showing the SPI in 2006
# Merge SPI values with shapefile
spi_long <- spi_year %>%
  gather(key = "state", value = "SPI", -year)

merged_data_spi <- merge(MY_sf, spi_long, by.x = "NAME_1", by.y = "state")

# Create the map for 2006
spi_2006 <- merged_data_spi %>%
  filter(year == 2006)

plot_spi_2006 <- ggplot() +
  geom_sf(data = spi_2006, aes(fill = SPI), color = "black") +
  labs(title = "SPI by State", subtitle = "2006") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "gray") +
  labs(fill = "SPI Value") +
  theme_bw()

# Create the map for 2019
spi_2019 <- merged_data_spi %>%
  filter(year == 2019)

plot_spi_2019 <- ggplot() +
  geom_sf(data = spi_2019, aes(fill = SPI), color = "black") +
  labs(title = "SPI by State", subtitle = "2019") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "gray") +
  labs(fill = "SPI Value") +
  theme_bw()

#-------------------------------------------------------------------------------
  
#Combining plots SPI + out-migration 2006
combined_plot_out <- grid.arrange(plot_out_2006, plot_out_2019, ncol = 1)
combined_plot_out

#2019
combined_plot_spi <- grid.arrange(plot_spi_2006, plot_spi_2019, ncol = 1)
combined_plot_spi

#-------------------------------------------------------------------------------
  
#circular migration flow plot
# Aggregate flows by origin and destination
migration_flows <- aggregate(flow ~ origin + destination, data = migration, sum)

migration_flows <- migration_flows %>%
  filter(flow >= 20)

# Create migration matrix
migration_matrix <- with(migration_flows, table(origin, destination, flow))

# Set up the plot
circos.par(cell.padding = c(0.02, 0.02))
circos.initialize(factors = unique(c(rownames(migration_matrix), colnames(migration_matrix))), xlim = c(0, 1))

# Add the chord diagram
chordDiagram(migration_matrix)


#Checking flows from Selangor
Selangor_flows <- migration[migration$origin != migration$destination, ] %>%
  filter(origin == "Selangor")

# 4) Regressions ---------------------------------------------------------------
#-------------------------------------------------------------------------------

# Simple regression out-migration ~ SPI
#Merge out-migration and spi_long
origin_flows_lm <- origin_flows %>%
  rename(state = origin)

merged_out_spi <- merge(origin_flows_lm, spi_long, by = c("year", "state"))

#Simple regression
lm_out_spi <- lm(flow ~ SPI, data = merged_out_spi)
summary(lm_out_spi)

#With heteroscedasticity
coeftest(lm_out_spi, vcov = vcovHC(lm_out_spi))
