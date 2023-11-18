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
  lubridate, #for dates
  broom, #for the augmennt function
  quantreg, #quantile regressions
  geosphere #distance between cities
  )

# 1) Loading data --------------------------------------------------------------

# #Loading NETCDF.
# nc_pre <- nc_open("c:/Users/samue/Desktop/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc")
# nc_spei <- nc_open("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei12.nc")

#Loading CRU TS precipitation dataset into R
# r2 <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/cru_ts4.07.1901.2022.pre.dat.nc", subds = "pre")
r <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei12.nc")
r1 <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei04.nc")
r2 <- rast("c:/Users/samue/Desktop/Dissertation/Migration/Data/spei06.nc")

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


#Loading cities data
cities <- read.csv("c:/Users/samue/Desktop/Dissertation/Migration/Data/MY_cities.csv")

#Population 
#opening excel file
pop <- read_excel("c:/Users/samue/Desktop/Dissertation/Migration/Data/MY - pop.xlsx")

#Merging with migration dataset
migration <- migration %>%
  left_join(pop, by = c("origin" = "state", "year"))


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

#Yearly SPEI calculation ---- Keeping the 12 month June SPEI
SPEI_by_state <- SPEI_by_state %>%
  filter(month == 6,
         year >= 2000 & year < 2020) %>%
  select(- month)

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


# 2.2) Weather variables -------------------------------------------------------


#Piecewise linear regressions ----
migration <- migration %>%
  mutate(SPEI_droughts_intense = ifelse(SPEI <= -1.5, SPEI + 1.5, 0),
         SPEI_floods_intense = ifelse(SPEI >= 1.5, SPEI - 1.5, 0),
         SPEI_droughts = ifelse(SPEI <= - 1, SPEI + 1, 0),
         SPEI_floods = ifelse(SPEI >= 1, SPEI - 1, 0))

# 3) Geographical data --------------------------------------------

#Distance between states--------------------------------------------------------
#Converting geometries of the states into point geometries
points_MY <- st_centroid(MY_sf)

# Transform the data frame to an appropriate CRS for geodesic calculations
points_MY <- st_transform(points_MY, "+proj=longlat +datum=WGS84")

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

#Distance between cities -------------------------------------------------------

#Most populous cities per state
cities <- cities %>%
  group_by(state) %>%
  slice_max(order_by = population) %>%
  ungroup()

cities_sf <- st_as_sf(cities, coords = c("long", "lat"), crs = 4326)

# Transform the data frame to an appropriate CRS for geodesic calculations
cities_sf <- st_transform(cities_sf, "+proj=longlat +datum=WGS84")

#Calculating distance matrix between the points
dist_matrix <- st_distance(cities_sf)

dist_matrix <- dist_matrix %>%
  as.data.frame()

rownames(dist_matrix) <- cities_sf$state
colnames(dist_matrix) <- cities_sf$state

#Wide to long transformation
dist_matrix$state <- row.names(dist_matrix)

dist_matrix_long <- dist_matrix %>%
  tidyr::pivot_longer(cols = -state, names_to = "destination", values_to = "distance_cities") %>%
  filter(state != destination)

#Adding distance to migration data
migration <- merge(migration, dist_matrix_long, by.x = c("origin", "destination"), by.y = c("state", "destination"), all.x = TRUE)

#Replace NAs with 0s and converting meters to kilometers
migration <- migration %>%
  mutate(distance_cities = ifelse(is.na(distance_cities), 0, distance_cities)) %>%
  arrange(year) %>%
  select(year, everything()) %>%
  mutate(distance_cities = round(distance_cities / 1000, 2))

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


# 4) Additional data for interactions ------------------------------------------

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
         GDP_agri_tertile = as.factor(ntile(GDP_agri_per, 3))) %>%
         arrange(Year, GDP_agri_quartile)
  

#Merging with migration dataset
migration <- migration %>%
  mutate(year = as.numeric(year)) %>%
  left_join(GDP, by = c("origin" = "State", "year" = "Year")) 


# 5) Preparation for regressions -----------------------------------------------

#Correcting the years as character issue
migration <- migration %>%
  mutate(year = as.numeric(year))

# 5.1) Dependent variable transformations ---------------------------------------
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

# 5.2) Restricted datasets -----------------------------------------------------
#Without high values ----
migration_wo_high <- migration %>%
  filter(!(origin == "Putrajaya" & destination == "Selangor"),
         !(origin == "Kuala Lumpur" & destination == "Selangor"))

x <- migration %>%
  select(year, origin, destination, migrates)

#Outliers using IQR ----

#By pairs
migration_outliers <- migration %>%
  group_by(origin, destination) %>%
  mutate(Q1 = quantile(migrates, 0.25),
         Q3 = quantile(migrates, 0.75),
         IQR = Q3 - Q1,
         lower_bound = Q1 - 1.5 * IQR,
         upper_bound = Q3 + 1.5 * IQR,
         outliers = ifelse(migrates < lower_bound | migrates > upper_bound, migrates, NA)) %>%
  filter(is.na(outliers))

# Removal of zero values ----

#Removing pairs with only zero values

#Strictly zero flow pairs
migration_wo_zeros <- migration %>%
  group_by(origin, destination) %>%
  mutate(total_flow = sum(flow)) %>%
  arrange(origin, destination) %>%
  filter(total_flow != 0)

#Mostly zeros >= 6/8 zeros -> 75%
migration_wo_zeros_75 <- migration %>%
  group_by(origin, destination) %>%
  mutate(zeros = ifelse(flow == 0, 1, 0),
         total_zeros = sum(zeros)) %>%
  filter(total_zeros < 6) %>%
  arrange(origin, destination) 




# 6) Descriptive statistics ----------------------------------------------------

# Maps -------------------------------------------------------------------------



#Making a map showing SPEI in 2008
#Merge SPEI values with shapefile
merged_data_spei <- merge(MY_sf, SPEI_by_state, by.x = "NAME_1", by.y = "state")

# Create the map for 2008
spei_2010 <- merged_data_spei %>%
  filter(year == 2010)

plot_spei_2010 <- ggplot() +
  geom_sf(data = spei_2010, aes(fill = SPEI), color = "black") +
  labs(title = "SPEI by State", subtitle = "2010") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "gray") +
  labs(fill = "SPEI Value") +
  theme_bw()

# Create map for 2019
spei_2019 <- merged_data_spei %>%
  filter(year == 2019)

plot_spei_2019 <- ggplot() +
  geom_sf(data = spei_2019, aes(fill = SPEI), color = "black") +
  labs(title = "SPEI by State", subtitle = "2019") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "gray") +
  labs(fill = "SPEI Value") +
  theme_bw()

plot_spei_2010
plot_spei_2019


#SPEI plot over the years ----
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

# 7) Final results ------------------------------------------------------------

#7.0) Fixed effects -----------------------------------------------------------

#OLS
lm <- feols(IHS_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | sw0(year + origin + destination, origin + destination^year, origin + destination^year + origin^destination), migration)
etable(lm, cluster = "origin")

#Poisson
g <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense + log(distance) + border | sw0(year + origin + destination, origin + destination^year, origin + destination^year + origin^destination), fixef.rm = "none", migration)
etable(g, cluster = "origin")

#Report
etable(lm, g, cluster = "origin", digits = "r3", tex = TRUE, style.tex = style.tex("aer"))

#7.1) OLS vs PPML -------------------------------------------------------------

lm <- feols(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration)
g <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration)

lm2 <- feols(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_wo_zeros)
g2 <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros)

lm3 <- feols(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_wo_zeros_75)
g3 <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros_75)

lm4 <- feols(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_wo_high)
g4 <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none" ,migration_wo_high)

lm5 <- feols(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_outliers)
g5 <- fepois(c(IHS_flow_rates, migrates) ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration_outliers)


# The coefficients change signs when zeros are removed.
# Coefficients change a lot depending on the restriction
etable(lm, lm2, lm3, lm4, lm5, cluster = "origin")


#Overall the coefficients have the same sign. changes a bit.
#Overall more stable
etable(g, g2, g3, g4, g5, cluster = "origin")

#report
lm <- feols(LN1_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration)
g <- fepois(flow ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration)

lm2 <- feols(LN1_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_wo_zeros)
g2 <- fepois(flow ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros)

lm3 <- feols(LN1_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_wo_zeros_75)
g3 <- fepois(flow ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration_wo_zeros_75)

lm4 <- feols(LN1_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_wo_high)
g4 <- fepois(flow ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none" ,migration_wo_high)

lm5 <- feols(LN1_flow_rates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, migration_outliers)
g5 <- fepois(flow ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense | origin + destination^year + origin^destination, fixef.rm = "none", migration_outliers)

etable(lm, lm2, lm3, lm4, lm5, g, g2, g3, g5, digits = "r3", cluster = "origin")

#7.2) Main spec ---------------------------------------------------------------

#Piecewise + normal ----
#Intense one is the only significant one
g <- fepois(migrates ~ SPEI + mvsw(SPEI_droughts, SPEI_floods) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g, cluster = "origin")

g2 <- fepois(migrates ~ SPEI + mvsw(SPEI_droughts_intense, SPEI_floods_intense) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
etable(g2, cluster = "origin")

#Report
etable(g, g2, digits = "r3", cluster = "origin", tex = TRUE, style.tex = style.tex("aer"))

#Response function
#Saving the estimate in a table
g <- fepois(migrates ~ SPEI + SPEI_droughts_intense + SPEI_floods_intense |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
table <- coeftable(g, cluster = "origin")

# Create a data frame with a range of SPEI values
spei_values <- data.frame(SPEI = seq(-3, 3, by = 0.01))

# Define the estimated coefficients
slope_droughts <- table[2, 1] + table[1, 1]
slope_floods <- table[3, 1] + table[1, 1]
slope_normal <- table[1, 1]

#Calculate predicted values for each segment
spei_values <- spei_values %>%
  mutate(marginal = ifelse(spei_values$SPEI <= -1.5, (-1.5 * slope_normal) + slope_droughts * (SPEI + 1.5),
                           ifelse(spei_values$SPEI >= 1.5, (1.5 * slope_normal) + slope_floods * (SPEI - 1.5),
                                  slope_normal * SPEI)))

#Response function plot
ggplot(spei_values, aes(x = SPEI, y = marginal)) +
  geom_line() +
  theme_minimal() +
  labs(x = "SPEI", y = "Migrates") +
  geom_hline(yintercept = 0, linetype = 2, colour = "red") +
  geom_segment(aes(x = -1.5 , xend = 1.5,y = slope_normal, yend = slope_normal), color = "grey") +
  geom_segment(aes(x = 1.5 , xend = 3,y = slope_floods, yend = slope_floods), color = "grey") +
  geom_segment(aes(x = -3 , xend = -1.5,y = slope_droughts, yend = slope_droughts), color = "grey") +
  scale_x_continuous(breaks = seq(-3, 3, by = 0.5)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.5))


#7.2.1) Heterogeneous effects -------------------------------------------------

#Origin states characteristics -------------------------------------------------
#Quartiles and tertiles
g <- fepois(migrates ~ SPEI:GDP_agri_quartile + sw0(SPEI_droughts_intense:GDP_agri_quartile + SPEI_floods_intense:GDP_agri_quartile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)
g2 <- fepois(migrates ~ SPEI:GDP_agri_tertile + sw0(SPEI_droughts_intense:GDP_agri_tertile + SPEI_floods_intense:GDP_agri_tertile) |  origin + destination^year + origin^destination, fixef.rm = "none", migration)


etable(g, g2, cluster = "origin")




#8) Appendix ----------------------------------------------------------------



#Tests -------------------------------------------------------------------------
#Correlation matrices
selected_data <- migration[c("SPEI_droughts", "SPEI_floods", "SPEI_droughts_intense", "SPEI_floods_intense", "SPEI")]
correlation_matrix <- round(cor(selected_data), 2)
print(correlation_matrix)


