library(httr)
library(jsonlite)
library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)

getlink <- function(link) {
  df <- fromJSON(content(GET(link), as  = "text"), flatten = TRUE)
  return(df)
}

getlink_value <- function(link, name) {
  df <- fromJSON(content(GET(link), as  = "text"), flatten  = TRUE)
  df$value$kit_id <- name
  return(df$value)
}

getobs <- function(link, name, meting) {
  df <- fromJSON(content(GET(link), as  = "text"), flatten  = TRUE)
  df$value$kit_id <- name
  df$value$meting <- meting
  return(df$value)
}


thingcodes <- read_excel("../codethings.xlsx")
thinglist <- read_excel("../things.xlsx")
#List of 
thinglist <-left_join(thingcodes, thinglist)
View(thinglist)
write_rds(thinglist, "thinglist.rds")

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
  print(i)
  y[i] <- locations$location.coordinates[[i]][2]
}
locations$lat <- y




#Get information individual sensors
df <- bind_rows(mapply(getlink_value, 
                    thinglist$`Datastreams@iot.navigationLink`, 
                    thinglist$name,
                    SIMPLIFY = FALSE)) %>% 
  separate(description, c("d1", "d2", "meting"), sep = "-")
View(df)

#Observations
obs <- bind_rows(mapply(getobs,
                        df$`Observations@iot.navigationLink`,
                        df$kit_id,
                        df$meting,
                        SIMPLIFY = FALSE))
#Change variables
obs_wide <- obs %>% 
  mutate(phenomenonTime = ymd_hms(phenomenonTime)) %>% 
  rename(date = phenomenonTime) %>%
  select(kit_id, date, meting, result) %>%
  group_by(kit_id, date, meting) %>% 
  summarise(result_m = mean(result)) %>% 
  pivot_wider(names_from = meting,
              values_from = result_m) %>% 
  left_join(locations)
  





#data en naam per sensor verzamelen
getdata <- function(datalink) {
  df <- getlink(datalink)
  return(df$value %>% select(phenomenonTime, result))
}


View(c)
sensor <- str_split(df$value$name[1], "-")
View(test)



dflist <- lapply(df$value$`Observations@iot.navigationLink`, getlink)
View(dflist)
sensor1 <- dflist[[1]]$value
View(sensor1)

Infor3 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105717)/Observations"

View(getlink(Infor3))

#Nieuwe methode
#Lijst van links naar sensoren

overzicht <- "https://api-samenmeten.rivm.nl/v1.0/Things"
t126 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(126)"



SamenMeten_text <- content(GET(overzicht), as = "text")
SamenMeten <- fromJSON(SamenMeten_text, flatten = TRUE)

thingslijst <- SamenMeten$value$`Datastreams@iot.navigationLink`
locatielijst <- SamenMeten$value$`HistoricalLocations@iot.navigationLink`
test <- GET(thingslijst[1])
str(test)
Datastream1 <- fromJSON(test$content, flatten = TRUE)
View(test$content)
View(thingslijst)

tes <- content(GET("https://api-samenmeten.rivm.nl/v1.0/Datastreams(614)/Observations", as="test"))


#Thing 126
t126 <- "https://api-samenmeten.rivm.nl/v1.0/Things(126)"
t126_text <- content(GET(t126), as = "text")
SamenMeten <- fromJSON(SamenMeten_text, flatten = TRUE)

Datastreams <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams"

sensors <- "https://api-samenmeten.rivm.nl/v1.0/Sensors"
test <- content(GET(sensors), as = "text")
Sensors <- fromJSON(test, flatten = TRUE)

View(Sensors$value)


################################oud######################
  Infor1 <- "https://api-samenmeten.rivm.nl/v1.0/Things(3665)"
  Infor2 <- "https://api-samenmeten.rivm.nl/v1.0/Things(3665)/Locations"
  
  # pm25 data 
  Infor3 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105717)/Observations"
  Infor4 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105717)/Observations?$top=20&$skip=20"
  Infor5 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105717)/Observations?$top=20&$skip=40"
  Infor6 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105717)/Observations?$top=20&$skip=60"
  Infor7 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105717)/ObservedProperty"
  
  # pm10 data 
  Infor8 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105719)/Observations"
  Infor9 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105719)/Observations?$top=20&$skip=20"
  Infor10 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105719)/Observations?$top=20&$skip=40"
  
  Infor11 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105719)/Observations?$top=20&$skip=60"
  Infor12 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105719)/ObservedProperty"
  
  # pm25 kal data
  Infor13 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105718)/Observations"
  Infor14 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105718)/Observations?$top=20&$skip=20"
  Infor15 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105718)/Observations?$top=20&$skip=40"
  Infor16 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105718)/Observations?$top=20&$skip=60"
  Infor17 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105718)/ObservedProperty"
  
  # pm10 kal data
  Infor18 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105720)/Observations"
  Infor19 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105720)/Observations?$top=20&$skip=20"
  Infor20 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105720)/Observations?$top=20&$skip=40"
  Infor21 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105720)/Observations?$top=20&$skip=60"
  Infor22 <- "https://api-samenmeten.rivm.nl/v1.0/Datastreams(105720)/ObservedProperty"
  
  ## URL's in verschillende workspaces plakken ----
  # Vervolgens is het de bedoeling dat alle API URL links worden verplaatst naar verschillende workspaces. 
  # In dit geval wordt 'pastelink' gebruikt als een toepasselijke benaming voor het plakken van de diverse API URL calls.
  pastelink1 <- paste(Infor1)
  pastelink2 <- paste(Infor2)
  
  # pm25 data
  pastelink3 <- paste(Infor3)
  pastelink4 <- paste(Infor4)
  pastelink5 <- paste(Infor5)
  pastelink6 <- paste(Infor6)
  pastelink7 <- paste(Infor7)
  
  # pm10 data
  pastelink8 <- paste(Infor8)
  pastelink9 <- paste(Infor9)
  pastelink10 <- paste(Infor10)
  pastelink11 <- paste(Infor11)
  pastelink12 <- paste(Infor12)
  
  # pm25 kal data
  pastelink13 <- paste(Infor13)
  pastelink14 <- paste(Infor14)
  pastelink15 <- paste(Infor15)
  pastelink16 <- paste(Infor16)
  pastelink17 <- paste(Infor17)
  
  # pm10 kal data
  pastelink18 <- paste(Infor18)
  pastelink19 <- paste(Infor19)
  pastelink20 <- paste(Infor20)
  pastelink21 <- paste(Infor21)
  pastelink22 <- paste(Infor22)
  
  ## Data uit de API URL's binnenhalen ----
  # Vervolgens is het de bedoeling dat de benodigde data uit de API URL's wordt gehaald.
  # Om dit uit te voeren wordt de functie 'GET' gebruikt. 
  # Met de functie GET geef je aan wele dat je uit de API call1 (link) wilt binnenhalen.
  # Het content type dient application/json te zijn, anders kan er niet mee worden gewerkt.
  # Bij status: '200' is de data correct ingeladen.
  thing <- GET(pastelink1)
  location <- GET(pastelink2)
  
  # pm25 data
  obser_pm25 <- GET(pastelink3)
  obser2_pm25 <- GET(pastelink4)
  obser3_pm25 <- GET(pastelink5)
  obser4_pm25 <- GET(pastelink6)
  observ_prop_pm25 <- GET(pastelink7)
  
  # pm10 data
  obser_pm10 <- GET(pastelink8)
  obser2_pm10 <- GET(pastelink9)
  obser3_pm10 <- GET(pastelink10)
  obser4_pm10 <- GET(pastelink11)
  observ_prop_pm10 <- GET(pastelink12)
  
  # pm25 kal data
  obser_pm25_kal <- GET(pastelink13)
  obser2_pm25_kal <- GET(pastelink14)
  obser3_pm25_kal <- GET(pastelink15)
  obser4_pm25_kal <- GET(pastelink16)
  observ_prop_pm25_kal <- GET(pastelink17)
  
  # pm10 kal data
  obser_pm10_kal <- GET(pastelink18)
  obser2_pm10_kal <- GET(pastelink19)
  obser3_pm10_kal <- GET(pastelink20)
  obser4_pm10_kal <- GET(pastelink21)
  observ_prop_pm10_kal <- GET(pastelink22)
  
  
  ## De databestanden omzetten naar tekstbestanden ---- 
  # Met de functie 'content(dataset, "text")' geef je aan dat de data omgezet moet worden naar 
  # een tekstbestand.
  thing_sensor <- content(thing,"text")
  location_sensor <- content(location,"text")
  
  # pm25 data
  obser_pm25_sensor <- content(obser_pm25, "text")
  obser2_pm25_sensor <- content(obser2_pm25, "text")
  obser3_pm25_sensor <- content(obser3_pm25, "text")
  obser4_pm25_sensor <- content(obser4_pm25, "text")
  observprop_pm25_sensor <- content(observ_prop_pm25, "text")
  
  # pm10 data
  obser_pm10_sensor <- content(obser_pm10, "text")
  obser2_pm10_sensor <- content(obser2_pm10, "text")
  obser3_pm10_sensor <- content(obser3_pm10, "text")
  obser4_pm10_sensor <- content(obser4_pm10, "text")
  observprop_pm10_sensor <- content(observ_prop_pm10, "text")
  
  # pm25 kal data
  obser_pm25_kal_sensor <- content(obser_pm25_kal, "text")
  obser2_pm25_kal_sensor <- content(obser2_pm25_kal, "text")
  obser3_pm25_kal_sensor <- content(obser3_pm25_kal, "text")
  obser4_pm25_kal_sensor <- content(obser4_pm25_kal, "text")
  observprop_pm25_kal_sensor <- content(observ_prop_pm25_kal, "text")
  
  # pm10 kal data
  obser_pm10_kal_sensor <- content(obser_pm10_kal, "text")
  obser2_pm10_kal_sensor <- content(obser2_pm10_kal, "text")
  obser3_pm10_kal_sensor <- content(obser3_pm10_kal, "text")
  obser4_pm10_kal_sensor <- content(obser4_pm10_kal, "text")
  observprop_pm10_kal_sensor <- content(observ_prop_pm10_kal, "text")
  
  ## De tekstbestanden omzetten naar RStudio bestanden ---- 
  # Om verder te kunnen werken met de data is het van belang dat de JSON bestanden worden omgezet naar R bestanden.
  # Met de functie 'fromJSON' wordt dit gewaarborgd. 
  thing_json <- fromJSON(thing_sensor, flatten = TRUE)
  location_json <- fromJSON(location_sensor, flatten = TRUE)
  
  # pm25 data
  obser_pm25_json <- fromJSON(obser_pm25_sensor, flatten = TRUE)
  obser2_pm25_json <- fromJSON(obser2_pm25_sensor, flatten = TRUE)
  obser3_pm25_json <- fromJSON(obser3_pm25_sensor, flatten = TRUE)
  obser4_pm25_json <- fromJSON(obser4_pm25_sensor, flatten = TRUE)
  observprop_pm25_json <- fromJSON(observprop_pm25_sensor, flatten = TRUE)
  
  # pm10 data
  obser_pm10_json <- fromJSON(obser_pm10_sensor, flatten = TRUE)
  obser2_pm10_json <- fromJSON(obser2_pm10_sensor, flatten = TRUE)
  obser3_pm10_json <- fromJSON(obser3_pm10_sensor, flatten = TRUE)
  obser4_pm10_json <- fromJSON(obser4_pm10_sensor, flatten = TRUE)
  observprop_pm10_json <- fromJSON(observprop_pm10_sensor, flatten = TRUE)
  
  # pm25 kal data
  obser_pm25_kal_json <- fromJSON(obser_pm25_kal_sensor, flatten = TRUE)
  obser2_pm25_kal_json <- fromJSON(obser2_pm25_kal_sensor, flatten = TRUE)
  obser3_pm25_kal_json <- fromJSON(obser3_pm25_kal_sensor, flatten = TRUE)
  obser4_pm25_kal_json <- fromJSON(obser4_pm25_kal_sensor, flatten = TRUE)
  observprop_pm25_kal_json <- fromJSON(observprop_pm25_kal_sensor, flatten = TRUE)
  
  # pm10 kal data
  obser_pm10_kal_json <- fromJSON(obser_pm10_kal_sensor, flatten = TRUE)
  obser2_pm10_kal_json <- fromJSON(obser2_pm10_kal_sensor, flatten = TRUE)
  obser3_pm10_kal_json <- fromJSON(obser3_pm10_kal_sensor, flatten = TRUE)
  obser4_pm10_kal_json <- fromJSON(obser4_pm10_kal_sensor, flatten = TRUE)
  observprop_pm10_kal_json <- fromJSON(observprop_pm10_kal_sensor, flatten = TRUE)
  
  ## De RStudio bestanden omzetten naar bewerkbare bestanden ----
  # Door de omzetting kan er een tabel worden gemaakt van de data. 
  # De tabel wordt gemaakt met behulp van de functie 'as.data.frame'.
  # Er moet eerst een dataframe worden gemaakt voordat de data verder bewerkt (gefilterd kan worden).
  thing_data <- as.data.frame(thing_json)
  location_data <- as.data.frame(location_json)
  
  # pm25 data
  obser_pm25_data <- as.data.frame(obser_pm25_json)
  obser2_pm25_data <- as.data.frame(obser2_pm25_json)
  obser3_pm25_data <- as.data.frame(obser3_pm25_json)
  obser4_pm25_data <- as.data.frame(obser4_pm25_json)
  observprop_pm25_data <- as.data.frame(observprop_pm25_json)
  
  # pm10 data
  obser_pm10_data <- as.data.frame(obser_pm10_json)
  obser2_pm10_data <- as.data.frame(obser2_pm10_json)
  obser3_pm10_data <- as.data.frame(obser3_pm10_json)
  obser4_pm10_data <- as.data.frame(obser4_pm10_json)
  observprop_pm10_data <- as.data.frame(observprop_pm10_json)
  
  # pm25 kal data
  obser_pm25_kal_data <- as.data.frame(obser_pm25_kal_json)
  obser2_pm25_kal_data <- as.data.frame(obser2_pm25_kal_json)
  obser3_pm25_kal_data <- as.data.frame(obser3_pm25_kal_json)
  obser4_pm25_kal_data <- as.data.frame(obser4_pm25_kal_json)
  observprop_pm25_kal_data <- as.data.frame(observprop_pm25_kal_json)
  
  # pm10 kal data
  obser_pm10_kal_data <- as.data.frame(obser_pm10_kal_json)
  obser2_pm10_kal_data <- as.data.frame(obser2_pm10_kal_json)
  obser3_pm10_kal_data <- as.data.frame(obser3_pm10_kal_json)
  obser4_pm10_kal_data <- as.data.frame(obser4_pm10_kal_json)
  observprop_pm10_kal_data <- as.data.frame(observprop_pm10_kal_json)
  
  ## De desbetreffende tabellen bijwerken ----
  # Om vervolgens onnodige informatie weg te laten en alleen de noodzakelijke elementen te behouden,
  # wordt het makkelijker om met de data te werken. 
  # Het werkt als volgt:
  # df = subset(mydata, select = -c('.' , '.'))
  # binnen '-c()' wordt aangegeven welke kolommen weggelaten moeten worden.
  df  = subset(thing_data, select = -c(description, properties.project, X.iot.id, X.iot.selfLink, Locations.iot.navigationLink, Datastreams.iot.navigationLink, HistoricalLocations.iot.navigationLink))
  df2 = subset(location_data, select = -c(value..iot.id, value.name, value.location.type, value..iot.selfLink, value.description, value.Things.iot.navigationLink, value.HistoricalLocations.iot.navigationLink))
  
  # pm25 data
  df3 = subset(obser_pm25_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df4 = subset(obser2_pm25_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df5 = subset(obser3_pm25_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df6 = subset(obser4_pm25_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df7 = subset(observprop_pm25_data, select = -c(X.iot.id, X.iot.selfLink, name, definition, Datastreams.iot.navigationLink))
  
  # pm10 data
  df8 = subset(obser_pm10_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df9 = subset(obser2_pm10_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df10 = subset(obser3_pm10_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df11 = subset(obser4_pm10_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df12 = subset(observprop_pm10_data, select = -c(X.iot.id, X.iot.selfLink, name, definition, Datastreams.iot.navigationLink))
  
  # pm25 kal data
  df13 = subset(obser_pm25_kal_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df14 = subset(obser2_pm25_kal_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df15 = subset(obser3_pm25_kal_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df16 = subset(obser4_pm25_kal_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df17 = subset(observprop_pm25_kal_data, select = -c(X.iot.id, X.iot.selfLink, name, definition, Datastreams.iot.navigationLink))
  
  # pm10 kal data
  df18 = subset(obser_pm10_kal_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df19 = subset(obser2_pm10_kal_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df20 = subset(obser3_pm10_kal_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df21 = subset(obser4_pm10_kal_data, select = -c(X.iot.nextLink, value..iot.id, value..iot.selfLink, value.Datastream.iot.navigationLink, value.FeatureOfInterest.iot.navigationLink, value.resultTime))
  df22 = subset(observprop_pm10_kal_data, select = -c(X.iot.id, X.iot.selfLink, name, definition, Datastreams.iot.navigationLink))
  
  
  # Vervolgens moeten er in de observatie bestanden de kolomnamen van de waardes aangepast worden.
  # Dit zal in een overzichtelijke dataframe resulteren wanneer alles is samengevoegd.
  
  names(df3)[2] <- "value.pm25"
  names(df4)[2] <- "value.pm25"
  names(df5)[2] <- "value.pm25"
  names(df6)[2] <- "value.pm25"
  names(df7)[1] <- "description.pm25"
  
  names(df8)[2] <- "value.pm10"
  names(df9)[2] <- "value.pm10"
  names(df10)[2] <- "value.pm10"
  names(df11)[2] <- "value.pm10"
  names(df12)[1] <- "description.pm10"
  
  names(df13)[2] <- "value.pm25.kal"
  names(df14)[2] <- "value.pm25.kal"
  names(df15)[2] <- "value.pm25.kal"
  names(df16)[2] <- "value.pm25.kal"
  names(df17)[1] <- "description.pm25.kal"
  
  names(df18)[2] <- "value.pm10.kal"
  names(df19)[2] <- "value.pm10.kal"
  names(df20)[2] <- "value.pm10.kal"
  names(df21)[2] <- "value.pm10.kal"
  names(df22)[1] <- "description.pm10.kal"
  
  # Om in de volgende stappen alle dataframes samen te voegen, 
  # wordt er in elke dataframe de kolom 'naam' toegevoegd.
  # Hierdoor kunnen alle losse dataframes uiteindelijk door een overeenkomend kolom samengevoegd worden.
  
  df2$name <- df$name
  
  # pm25 data
  df3$name <- df$name
  df4$name <- df$name
  df5$name <- df$name 
  df6$name <- df$name 
  df7$name <- df$name 
  
  # pm10 data
  df8$name <- df$name
  df9$name <- df$name
  df10$name <- df$name 
  df11$name <- df$name 
  df12$name <- df$name 
  
  # pm25 kal data
  df13$name <- df$name
  df14$name <- df$name
  df15$name <- df$name 
  df16$name <- df$name 
  df17$name <- df$name 
  
  # pm10 kal data
  df18$name <- df$name
  df19$name <- df$name
  df20$name <- df$name 
  df21$name <- df$name 
  df22$name <- df$name 
  
  ## Samenvoegen van de tabellen ----
  # er zijn twee mogelijkheden om datasets samen te voegen: horizontaal en verticaal
  # horizontaal: toevoegen columns door de functie 'merge' te gebruiken: total <- merge(data frameA, data frameB,by"ID")
  # verticaal: toevoegen rows door de functie 'rbind' te gebruiken: total <- rbind(data frameA, data frameB)
  
  total <- merge(df, df2,by="name")
  
  # pm25 data
  data_pm25 <- rbind(df3, df4)
  data2_pm25 <- rbind(df5, df6)
  totaldata_pm25 <- rbind.fill(data_pm25,data2_pm25)
  total_pm25 <- merge(totaldata_pm25, df7, by="name")
  table_pm25 <- merge(total, total_pm25,by="name")
  
  # pm10 data
  data_pm10 <- rbind(df8, df9)
  data2_pm10 <- rbind(df10, df11)
  totaldata_pm10 <- rbind.fill(data_pm10,data2_pm10)
  total_pm10 <- merge(totaldata_pm10, df12,by="name")
  table_pm10 <- merge(total, total_pm10,by="name")
  
  # pm25 kal data
  data_pm25_kal <- rbind(df13, df14)
  data2_pm25_kal <- rbind(df15, df16)
  totaldata_pm25_kal <- rbind.fill(data_pm25_kal,data2_pm25_kal)
  table_pm25_kal <- merge(totaldata_pm25_kal, df17, by="name")
  
  # pm10 kal data
  data_pm10_kal <- rbind(df18, df19)
  data2_pm10_kal <- rbind(df20, df21)
  totaldata_pm10_kal <- rbind.fill(data_pm10_kal,data2_pm10_kal)
  table_pm10_kal <- merge(totaldata_pm10_kal, df22,by="name")
  
  # tabel met alle data
  table_total <- merge(table_pm25, total_pm10,by="value.phenomenonTime")
  table_total2 <- merge(table_pm25_kal, table_pm10_kal,by="value.phenomenonTime")
  table_total3 <- merge(table_total, table_total2,by="value.phenomenonTime")
  SamenMeten_final = subset(table_total3, select = -c(name.y.x, name.y.y, name.x.y))
  
  ## Laatste bewerkingen kolommen / bijhorende waarden in de tabellen ----
  # Om in een latere fase de tabel te kunnen koppelen aan de Hollandse Luchten applicatie van het RIVM,
  # is het noodzakelijk dat de nieuwe dataset overeenkomt met de oude dataset.
  # Dit betekent dat de kolommen en de desbetreffende waarden in de kolommen van de nieuwe dataset,
  # overeenkomen met die van de oude dataset. 
  
  # Als eerste zijn noodzakelijke kolommen verwerkt en aangepast.
  # Dit is het geval bij de locatiegegevens, de lat en lon, die in dit geval in aparte kolommen gezet moeten worden.
  # Vervolgens is de waarde aanduiding in de kolom met de tijdgegevens aangepast.
  
  SamenMeten_final <- separate(SamenMeten_final, col=value.location.coordinates, into = c("lat","lon"), sep = ",")
  SamenMeten_final <- separate(SamenMeten_final, col=value.phenomenonTime, into = c("date","time"), sep = "T")
  View(SamenMeten_final)
  
  # Bij de onderstaande code worden onnodige tekens, in dit geval '(', ')' en '.000Z', uit de kolommen verwijderd.
  # Dit wordt gerealiseerd door een gedeelte binnen de kolomnaam te splitsen en verwijderen van de desbetreffende waardes.
  
  SamenMeten_final$lat <- sapply(strsplit(SamenMeten_final$lat, split='(', fixed=TRUE), function(x) (x[2]))
  SamenMeten_final$lon <- sapply(strsplit(SamenMeten_final$lon, split=')', fixed=TRUE), function(x) (x[1]))
  SamenMeten_final$time <- sapply(strsplit(SamenMeten_final$time, split='.000Z', fixed=TRUE), function(x) (x[1]))
  
  SamenMeten_final$date <- as.POSIXct(paste(SamenMeten_final$date, SamenMeten_final$time), format="%Y-%m-%d %H:%M:%S")
  SamenMeten_final$time <- NULL
  
  # Vervolgens is het van belang om te kijken hoe de class van de kolom wordt gedefiniëerd. 
  # Om de data uiteindelijk te kunnen verwerken in de Hollandse Luchten applicatie.
  # De functie 'class' wordt gebruikt om erachter te komen wat voor soort class specifieke kolommen in de tabel hebben.
  
  class(SamenMeten_final$date)
  class(SamenMeten_final$pm25)
  class(SamenMeten_final$pm10)
  class(SamenMeten_final$lat)
  class(SamenMeten_final$lon)
  
  SamenMeten_final$lat <- as.numeric(as.character(SamenMeten_final$lat))
  SamenMeten_final$lon <- as.numeric(as.character(SamenMeten_final$lon))
  
  # Vervolgens is het de bedoeling dat alle kolomnamen overeenkomen met die van de oude dataset
  # Met de functie colnames worden de kolomnamen verandert naar de kolomnamen die in de dataset van
  # het Hollandse luchten platform wordt gebruikt.
  
  colnames(SamenMeten_final)[2] <- "kit_id"
  colnames(SamenMeten_final)[7] <- "pm25"
  colnames(SamenMeten_final)[9] <- "pm10"
  colnames(SamenMeten_final)[11] <- "pm25_kal"
  colnames(SamenMeten_final)[13] <- "pm10_kal"
  
  ## Opslaan van de dataset ----
  # Met behulp van de onderstaande code wordt de data opgeslagen als rds bestand (Rstudio)
  # Door het als rds bestand op te slaan, kan het r-script makkelijk geïntegreerd worden 
  saveRDS(SamenMeten_final, file = "LTD_24801.rds")
} 
