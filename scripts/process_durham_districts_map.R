library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sf)

# GEOGRAPHY
# Get Durham police beats and districts
download.file("https://webgis.durhamnc.gov/server/rest/services/PublicServices/Public_Safety/MapServer/8/query?outFields=*&where=1%3D1&f=geojson",
              "data/source/geo/durham_police_beats.geojson")

# Read in geojson and then transform to sf format
beats_geo <- st_read("data/source/geo/durham_police_beats.geojson") %>% st_transform(3857)

# Get demographic data for Census block groups to aggregate/apportion to precinct geography
# Also transforming to match the planar projection of NYPD's beats spatial file
# This also reduces us down to just the numeric population est and geometry
blocks <- get_decennial(geography = "block", 
                        year = 2020,
                        output = 'wide',
                        variables = "P1_001N", 
                        state = "NC",
                        county = c("Durham"),
                        geometry = TRUE) %>%
  rename("population"="P1_001N") %>% 
  select(3) %>%
  janitor::clean_names() %>%
  st_transform(3857)

districts_geo <- beats_geo %>% 
  group_by(LAWDIST) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# Calculate the estimated population of police districts geographies/interpolate with tidycensus bgs
# Reminder: ext=true SUMS the population during interpolation
districts_withpop <- st_interpolate_aw(blocks, districts_geo, ext = TRUE)
# Drops geometry so it's not duplicated in the merge
districts_withpop <- st_drop_geometry(districts_withpop)
# Binds that new population column to the table
districts_geo <- cbind(districts_geo,districts_withpop)
# Cleans up unneeded calculation file
# rm(districts_withpop, blocks)

# Check total population assigned/estimated across all precincts
sum(districts_geo$population) # tally is 453321 

# Round the population figure; rounded to nearest thousand
districts_geo$population <- round(districts_geo$population,-3)
# Prep for tracker use
districts_geo <- districts_geo %>% st_transform(4326)
districts_geo <- st_make_valid(districts_geo)

# beats_geo <- beats_geo %>% select(7,6,11)
districts_geo <- districts_geo %>% rename("district"="LAWDIST")




# saving a clean geojson and separate RDS for use in tracker
file.remove("data/output/geo/durham_districts.geojson")
st_write(districts_geo,"data/output/geo/durham_districts.geojson")
saveRDS(districts_geo,"scripts/rds/durham_districts.rds")

# BEAT MAP JUST FOR TESTING PURPOSES
# CAN COMMENT OUT ONCE FINALIZED
# Set bins for districts pop map
popbins <- c(0,2500,5000,7500,10000,50000, Inf)
poppal <- colorBin("YlOrRd", districts_geo$population, bins = popbins)
poplabel <- paste(sep = "<br>", districts_geo$district,prettyNum(districts_geo$population, big.mark = ","))

durham_districts_map <- leaflet(districts_geo) %>%
  setView(-78.89, 35.99, zoom = 11.5) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "Stamen.TonerLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`))
durham_districts_map