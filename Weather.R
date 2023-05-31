#Loading libraries
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
  readxl # handle excel files
)

# 1) Loading data --------------------------------------------------------------

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


# 2) Working with weather data -------------------------------------------------

#Cropping raster layers
MY_r <- terra::crop(r, MY_sf)

#Converting to Spatvector
MY_sv <- vect(MY_sf)

#Extracting values from the raster for each state
pre_by_state <- terra::extract(MY_r, MY_sv, fun = "mean", na.rm = TRUE)

#Deleting ID column
pre_by_state <- select(pre_by_state, -ID)

#Changing column names to date
a <- 1901:2022
b <- 1:12

name <- c()

for(k in 1:length(a)){
  name <- append(name,
                 paste0(a[k],"-",b))
}

colnames(pre_by_state) <- name

#Changing row names to states.
rownames(pre_by_state) <- MY_sf$NAME_1

#Inverting columns and rows
pre_by_state <- pre_by_state %>%
  t() %>%
  as.data.frame()

#Creation of a time series
# Define the start and end dates
start_date <- as.Date("1901-01-01")
end_date <- as.Date("2019-12-01")

# Create a sequence of dates
dates <- seq(start_date, end_date, by = "month")

# Create an empty list to store time series objects
ts_list <- list()

# Loop through each column
for (i in 1:ncol(pre_by_state)) {
  # Create time series for each column
  ts_data <- ts(pre_by_state[, i], start = c(1901, 1), frequency = 12)
  
  # Set the time series name as the column name
  names(ts_data) <- colnames(pre_by_state)[i]
  
  # Store the time series in the list
  ts_list[[i]] <- ts_data
}

# Check the created time series
print(ts_list)


#Plotting weather
plot(MY_r$pre_1)
plot(MY_sv, add = TRUE)

# 3) Descriptive statistics ---------------------------------------------------
  
#Creating histogram of migration flows
ggplot(migration, aes(x = flow)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Migration Flows", x = "Flows", y = "Frequency") +
  theme_minimal()

#Creating an out-migration map.
origin_flows <- aggregate(flow ~ origin, data = migration[migration$origin != migration$destination, ], FUN = sum)

merged_data <- merge(MY_sf, origin_flows, by.x = "NAME_1", by.y = "origin", all.x = TRUE)

ggplot() +
  geom_sf(data = merged_data, aes(fill = flow)) +
  labs(title = "Out-Migration by State",
       subtitle = "2006-2019") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = NA) +
  theme_void()

#Checking flows from Selangor
Selangor_flows <- migration[migration$origin != migration$destination, ] %>%
  filter(origin == "Selangor")

# #Plotting precipitatin in Johor
# johor_ts <- ts_list[[1]]  # Assuming Johor is the first element in the list
# 
# johor_df <- data.frame(time = time(johor_ts), value = as.vector(johor_ts))
# 
# ggplot(johor_df, aes(x = time, y = value)) +
#   geom_line() +
#   labs(x = "Year", y = "Value", title = "Time Series for Johor")
