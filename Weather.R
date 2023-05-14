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
nc_pre <- nc_open("~/Msc Economics/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc")

#Loading CRU TS precipitation dataset into R
r <- rast("~/Msc Economics/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc", subds = "pre")

#MY boundary
MY_sf <-
  st_read("~/Msc Economics/Dissertation/Migration/Data/gadm41_MYS_shp/gadm41_MYS_1.shp") %>%
  st_as_sf() %>%
  st_transform(terra::crs(r))

#Loading internal migration flow data
migration <- read_excel("~/Msc Economics/Dissertation/Migration/Data/Migration.xlsx")


# 2) Working with weather data -------------------------------------------------

#Cropping raster layers
MY_r <- terra::crop(r, MY_sf)

#Converting to Spatvector
MY_sv <- vect(MY_sf)

#Extracting values from the raster for each state
pre_by_state <- terra::extract(MY_r, MY_sv, fun = "mean", na.rm = TRUE)

#Changing column names to date
a <- 1901:2022
b <- 1:12

name <- c()

for(k in 1:length(a)){
  name <- append(name,
                 paste0(a[k],"-",b))
}

colnames(pre_by_state) <- name

#Changing row names to states' names
pre_by_state$ID <- MY_sf$NAME_1
  