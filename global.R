library(httr)
library(jsonlite)
library(tidyjson)
library(tidyverse)
library(lubridate)


#Voordat Shiny app wordt opgestart, wordt eerst de data binnengehaald
#In eerste instantie is dat de data van de laatste 5 minuten

# API Luftdaten.--------------------------------------------------

#URL
#v2 gives the average over last five minutes
Linkluftdaten <- "http://api.luftdaten.info/static/v2/data.json"
#Filter werkt nog niet
#Linkluftdaten <- "http://api.luftdaten.info/static/v2/filter/country=NL/data.json"

#Get the data
dataluftd <- GET(Linkluftdaten)
luftdaten_text <- content(dataluftd, as = "text")

#Correcte manier van unnesten.
API_Luftdaten <- fromJSON(luftdaten_text, flatten = TRUE) %>%
  filter(location.country == "NL") %>% 
  rename(kit_id = id) %>% 
  select(-c(sampling_rate,location.exact_location, location.altitude, location.indoor, sensor.pin, sensor.sensor_type.id)) %>% 
  unnest(sensordatavalues) %>% 
  filter(value_type %in% c("P1", "P2")) %>% 
  mutate(value = as.numeric(value)) %>% 
  pivot_wider(names_from = value_type, values_from = value) %>%  
  filter(!is.na(P1)) %>% 
  mutate(location.longitude = as.numeric(location.longitude)) %>% 
  mutate(location.latitude  = as.numeric(location.latitude)) %>% 
  rename(lat = location.latitude,
         lon=  location.longitude,
         Var = sensor.sensor_type.name,
         date = timestamp)



# Samen meten ---------------------------------------------------------

getlink_value <- function(link, name) {
  df <- fromJSON(content(GET(link), as  = "text"), flatten  = TRUE)
  df$value$kit_id <- name
  return(df$value)
}

getobs <- function(link, name, meting) {
  link <- paste0(link, "?$top=80")
  df <- fromJSON(content(GET(link), as  = "text"), flatten  = TRUE)
  df$value$kit_id <- name
  df$value$meting <- meting
  return(df$value)
}


#Get thinglist
thinglist <- read_rds("thinglist.rds")


#Get location of things
locations <- bind_rows(mapply(getlink_value,
                              thinglist$`Locations@iot.navigationLink`, 
                              thinglist$name)) %>% 
  filter(!is.na(name))

x <- vector()
for( i in 1:length(locations$location.coordinates)) {
  x[i] <- locations$location.coordinates[[i]][1]
}
locations$lon <- x

y <- vector()
for( i in 1:length(locations$location.coordinates)) {
  y[i] <- locations$location.coordinates[[i]][2]
}
locations$lat <- y


#Get information individual sensors
df <- bind_rows(mapply(getlink_value, 
                       thinglist$`Datastreams@iot.navigationLink`, 
                       thinglist$name,
                       SIMPLIFY = FALSE)) %>% 
  separate(description, c("d1", "d2", "meting"), sep = "-")


#Observations
obs <- bind_rows(mapply(getobs,
                        df$`Observations@iot.navigationLink`,
                        df$kit_id,
                        df$meting,
                        SIMPLIFY = FALSE))

#Change variables and link to coordinates
input_df <- obs %>% 
  mutate(phenomenonTime = ymd_hms(phenomenonTime)) %>% 
  rename(date = phenomenonTime) %>%
  group_by(kit_id, date, meting) %>% 
  summarise(result_m = mean(result)) %>% 
  pivot_wider(names_from = meting,
              values_from = result_m) %>% 
  inner_join(locations, by = "kit_id")

library(DT)
#library(geoshaper)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(markdown)
library(openair)
library(plotly)
library(purrr)
library(purrrlyr)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(sp)

projectnaam <- "Burger Sensor Data platform"


# Functies voor het genereren van de input opties voor openair call.
source("selectReactiveComponent.R", local = TRUE) 
# Functies voor het genereren van de inhoud van de tabpanels.
source("tabPanels.R", local = TRUE) 

## Gedeelte 4 ----

choices <- c( "PM10 - gekalibreerd", "PM2.5 - gekalibreerd","PM10", "PM2.5") #set up choices for shiny app
kleur_cat <- list('#42145f','#ffb612','#a90061','#777c00','#007bc7','#673327','#e17000','#39870c', '#94710a','#01689b','#f9e11e','#76d2b6','#d52b1e','#8fcae7','#ca005d','#275937','#f092cd')
kleur_sensor <- "leeg"
kleur_marker_sensor <- "#525252" # default kleur sensor
geen_groep <- "" # default waarde als de sensor niet in een groep zit

icons_stations <- iconList(
  knmi = makeIcon("ionicons_compass.svg", 18, 18),
  lml = makeIcon("ionicons_analytics.svg", 15, 15))

# Default locatie, kleur en label opzetten 
input_df$kit_id <- gsub('HLL_hl_', '', input_df$kit_id) #remove HLL-string from input_df for shorter label

# Voor de sensormarkers: locatie, label en kleur etc. Per sensor één unieke locatie
sensor_unique <- aggregate(input_df[,c('lat','lon')], list(input_df$kit_id), FUN = mean) # gemiddelde om per sensor een latlon te krijgen
names(sensor_unique)[names(sensor_unique)=='Group.1'] <-'kit_id'
sensor_unique$selected <-FALSE
sensor_unique$groep <- geen_groep
sensor_unique$kleur <- kleur_marker_sensor
sensor_labels <- as.list(sensor_unique$kit_id) # labels to use for hoover info

# Voor de multiselect tool: omzetten lat/lon naar spatialpoints
ms_coordinates <- SpatialPointsDataFrame(sensor_unique[,c('lon','lat')],sensor_unique)

# Voor de knmimarkers: locatie en labels opzetten
knmi_stations <- data.frame("code" = c("knmi_06225", "knmi_06240", "knmi_06260"), "lat" =c(52.4622,52.3156,52.0989), "lon" =c(4.555,4.79028,5.17972))
knmi_stations$naam <- c("IJmuiden", "Schiphol", "De Bilt")
knmi_labels <- as.list(paste("KNMI", knmi_stations$naam, sep = ": "))

# Voor de lmlmarkers: locatie en labels opzetten
lml_stations <- data.frame("code" = c("NL49014","NL49551","NL49572","NL49561","NL10636","NL49573","NL49570","NL49553","NL49012"))
lml_stations$lat <- c(52.3597,52.463,52.4744,52.334,52.105,52.4789,52.4893,52.494,52.39)
lml_stations$lon <- c(4.86621,4.60184,4.6288,4.77401,5.12446,4.57934,4.64053,4.60199,4.88781)

# Maak in de labelling onderscheid tussen de LML en GGD stations
lml_labels <- vector("list", length(lml_stations$code))
lml_labels[grep('NL49', lml_stations$code)] <- "GGD"
lml_labels[grep('NL10', lml_stations$code)] <- "LML"
lml_labels <- as.list(paste(lml_labels, lml_stations$code, sep = ": "))

#Test


# From https://github.com/RedOakStrategic/geoshaper/blob/master/R/findLocations.R
#' Find locations inside a polygon, square, or circle drawn with leaflet.extras drawing tools on a Shiny Leaflet map.
#'
#' @param shape Shiny input (input$MAPID_draw_new_feature), representing shape drawn on the map by the user.
#' @param location_coordinates A SpatialPointsDataFrame containing coordinates and ids for all map locations.
#' @param location_id_colname Column name from location_coordinates containing desired names or ids for set of locations returned.
#' @return A vector of location ids.
#' @examples
#' mock_input.map_feature <- list(type = "Feature"
#'                          , properties = list(`_leaflet_id`= 13477, feature_type = "rectangle")
#'                          , geometry = list(type = "Polygon"
#'                          , coordinates = list(list(list(-76.15723, 39.51252)
#'                          , list(-76.15723,  40.30467), list(-74.73999, 40.30467)
#'                          , list(-74.73999, 39.51252), list(-76.15723, 39.51252)))))
#' airports <- data.frame('locationID' = c('PHL', 'DTW')
#'                       , 'Longitude' = c(-75.2408, -83.3533)
#'                       , 'Latitude' = c(39.8722, 42.2125))
#' coords = sp::SpatialPointsDataFrame(airports[,c('Longitude', 'Latitude')], airports)
#' findLocations(shape = mock_input.map_feature
#'                      , location_coordinates = coords
#'                      , location_id_colname = "locationID")


findLocations <- function(shape, location_coordinates, location_id_colname) {
  
  # derive polygon coordinates and feature_type from shape input
  polygon_coordinates <- shape$geometry$coordinates
  feature_type <- shape$properties$feature_type
  
  if(feature_type %in% c("rectangle","polygon")) {
    
    # transform into a spatial polygon
    drawn_polygon <- sp::Polygon(do.call(rbind,lapply(polygon_coordinates[[1]],function(x){c(x[[1]][1],x[[2]][1])})))
    
    # identify selected locations
    selected_locs <- sp::over(location_coordinates, sp::SpatialPolygons(list(sp::Polygons(list(drawn_polygon),"drawn_polygon"))))
    
    # get location ids
    x = (location_coordinates[which(!is.na(selected_locs)), location_id_colname])
    
    selected_loc_id = as.character(x[[location_id_colname]])
    
    return(selected_loc_id)
    
  } else if (feature_type == "circle") {
    
    center_coords <- matrix(c(polygon_coordinates[[1]], polygon_coordinates[[2]])
                            , ncol = 2)
    
    # get distances to center of drawn circle for all locations in location_coordinates
    # distance is in kilometers
    dist_to_center <- sp::spDistsN1(location_coordinates, center_coords, longlat = TRUE)
    
    # get location ids
    # radius is in meters
    x <- location_coordinates[dist_to_center < shape$properties$radius/1000, location_id_colname]
    
    selected_loc_id = as.character(x[[location_id_colname]])
    
    return(selected_loc_id)
  }
}




