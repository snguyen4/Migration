#Loading libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  terra, # handle raster data
  raster, # handle raster data
  exactextractr, # fast extractions
  sf, # vector data operations
  dplyr, # data wrangling
  tidyr, # data wrangling
  data.table, # data wrangling
  prism, # download PRISM data
  tictoc, # timing codes
  tigris, # to get county sf
  tmap, # for mapping
  ncdf4
  
)

#Loading NETCDF. Pre -> Precipitation (mm/month) 
nc_pre <- nc_open("~/Msc Economics/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc")

nc_pre


#Loading CRU TS precipitation dataset into R
r <- rast("~/Msc Economics/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc", subds = "pre")

r

#MY boundary
MY_sf <-
  st_read("~/Msc Economics/Dissertation/Migration/Data/gadm41_MYS_shp/gadm41_MYS_1.shp") %>%
  st_as_sf() %>%
  st_transform(terra::crs(r))

#Cropping raster layers
MY_r <- terra::crop(r, MY_sf)

#Converting to Spatvector
MY_sv <- vect(MY_sf)

#Extracting values from the raster for each state
pre_by_state <- terra::extract(MY_r, MY_sv, fun = "mean", na.rm = TRUE)

#Changing column names to date
a <- 1901:2022
b <- 1:12

name <- c("ID")

for(k in 1:length(a)){
  name <- append(name,
                 paste0(a[k],"-",b))
}

colnames(pre_by_state) <- name

#Changing row names to states' names
pre_by_state$ID <- MY_sf$NAME_1
