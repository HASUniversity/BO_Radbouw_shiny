library(jsonlite)
library(tidyverse)
library(httr)
library(jsonlite)
library(magrittr)
library(purrrlyr)

#Data inlezen, XY-data als numeriek
#Zie global voor uiteindelijke versie


#URL
Linkluftdaten <- "http://api.luftdaten.info/static/v1/data.json"
#Get the data
dataluftd <- GET(Linkluftdaten)
luftdaten_text <- content(dataluftd, as = "text")

get_data_luftd_json <- fromJSON(luftdaten_text, flatten = TRUE) %>% 
  filter(location.country == "NL")

#Optie
get_r_data <- function(r) {
  cbind(r %>% select(-sensordatavalues), r$sensordatavalues)
}

#Via for-loop
n <- nrow(get_data_luftd_json)
l <- vector(mode = "list", length = n)
l <- vector(length = n)
df_luftdaten <- get_r_data(get_data_luftd_json %>% slice(1))
for(i  in 2:n) {
  r_i <- get_r_data(get_data_luftd_json%>% slice(i))
  df_luftdaten <- rbind(r, r_i)
}

#Nog uitzoeken hoe het sneller kan via lapply


#Kan dit niet eenvoudiger?
df_Luftdaten <- get_data_luftd_json %>% 
  filter(location.country == "NL") %>% 
  mutate(location.longitude = as.numeric(location.longitude)) %>% 
  mutate(location.latitude  = as.numeric(location.latitude)) %>% 
  mutate(sensordatavalues = matrix(sensordatavalues),
         id = sapply(sensordatavalues, function(x) extract2(x, 1)),
         value = sapply(sensordatavalues, function(x) extract2(x, 2)),
         values_type = sapply(sensordatavalues, function(x) extract2(x, 3)),
         sensordatavalues = NULL) %>% 
  select(-c(sampling_rate,location.exact_location, id, location.altitude, location.indoor, sensor.pin, sensor.sensor_type.id))

#Rephrase
colnames(df_Luftdaten)[2] <- "kit_id"
colnames(df_Luftdaten)[3] <- "lat"
colnames(df_Luftdaten)[4] <- "lon"
colnames(df_Luftdaten)[10] <- "Var"
colnames(df_Luftdaten)[1] <- "date"

df_Luftdaten_test <- df_Luftdaten %>% 
  filter(grepl("P1|P2", Var))

#Onderstaande functie werkt niet correct!
df_Luftdaten2 <- df_Luftdaten %>%  
  filter(!grepl('temperature|humidity', Var)) %>% 
  separate(col=value, into = c("P1","P2"), sep = ",")

df_Luftdaten2 <- df_Luftdaten %>%  
  filter(!grepl('temperature|humidity', Var)) %>% 
  separate(col=value, into = c("P1","P2"), sep = ",")


## ---------------------------------------------------------
## R Script voor het inroepen van de Luftdaten API.     
## Dit script wordt verwerkt en gebruikt in het ontwikkelde platform en maakt hierdoor deel uit van de R-scripts.
## Auteur: 
## Thomas Geurts van Kessel 
## Laatste versie: juni 2020
## ---------------------------------------------------------
## Opmerkingen: 
## Het script roept de Luftdaten API op.
## De data uit de API wordt gebruikt in de grote kaart van het platform
## Er wordt bij elke belangrijke stap een onderbouwing gegeven.
## ---------------------------------------------------------

# Voor het binnenhalen van API's dienen de onderste twee packages gedownload te worden. 
# Er kan worden gewerkt met twee soorten API's namelijk JSON bestanden en XML bestanden. 

# Deze package maakt het mogelijk om uit te leggen hoe de API gelezen moet worden (get, delete)
library("httr")

# Deze package maakt het mogelijk om interactie te leggen tussen JSON bestanden en R bestanden
library("jsonlite")

# Met behulp van een link wordt de API ingelezen. Binnen de link kan aangegeven worden wat je binnen wilt halen
Linkluftdaten <- "http://api.luftdaten.info/static/v1/data.json"

# Plakken van de link in de workspace
#pastelink <- paste(Linkluftdaten)
#pastelink

# Met de functie GET geef je aan dat je de data uit call1 (link) wilt binnenhalen
# Content type dient application/json te zijn, anders kan er niet mee worden gewerkt (omgezet)
# Wanneer de status 200 is betekent het dat de data correct is ingeladen
GET(Linkluftdaten)

# Met de functie content -> text geef je aan dat de data omgezet moet worden naar tekst
get_data_luftd_text <- content(getdataluftd,"text")

get_data_luftd_text

# Met de functie fromJSON wordt een JSON bestand omgezet naar een R bestand! 
# Door deze omzetting kan de data in RStudio bewerkt worden
get_data_luftd_json <- fromJSON(get_data_luftd_text, flatten = TRUE)

# Door de omzetting kan er een tabel worden gemaakt van de data. 
# De tabel wordt gemaakt met behulp van de functie as.data.frame
# Er moet eerst een dataframe worden gemaakt voordat de data verder bewerkt (gefilterd kan worden)
get_data_luftd_df <- as.data.frame(get_data_luftd_json)

# Met behulp van de class funtie kan een data type worden bekeken
class(get_data_luftd_df$location.country)

# Om de filter functie te kunnen gebruiken moet de dplyr library gedownoad worden
library(dplyr)

# Bij de filter functie worden de sensoren binnen Nederland uit de dataset gefilterd
sensoren_Nederland_filter <- filter(get_data_luftd_df,location.country == "NL")

# Bij de filter functie worden de coordinaten omgezet van tekstwaardes naar nummerieke waardes
sensoren_Nederland_filter_nr <- transform(sensoren_Nederland_filter, location.longitude = as.numeric(location.longitude),
location.latitude = as.numeric(location.latitude))

class(sensoren_Nederland_filter_nr$location.latitude)
as.data.frame(sensoren_Nederland_filter_nr, drop=false)

# met behulp van de onderstaande code wordt de data.frame met sensordatavalues 
# in de huidige tabel gezet. Eerst wordt het data.frame omgezet naar een matrix.
# vervolgens worden de kolommen (id, value, value_type) uitgepakt (extract) en omgezet
# naar een list dat in de tabel te vinden is. Nu zijn alle waardes van de API in één tabel te vinden. 
library(dplyr)
test<- sensoren_Nederland_filter_nr %>%
  dplyr::mutate(sensordatavalues = matrix(sensordatavalues),
                id = sapply(sensordatavalues, function(x) magrittr::extract2(x, 1)),
                value = sapply(sensordatavalues, function(x) magrittr::extract2(x, 2)),
                value_type = sapply(sensordatavalues, function(x) magrittr::extract2(x, 3)),
                sensordatavalues = NULL)

# Met de class functie is het mogelijk om de type data van de kolom id te achterhalen 
class(test$id)

# In de onderstaande code worden onnodige kolommen uit de dataset gefilterd 
Luftdaten_final = subset(test, select = -c(sampling_rate,location.exact_location, id, location.altitude, location.indoor, sensor.pin, sensor.sensor_type.id))

# Met de functie colnames worden de kolomnamen veranderd naar de namen die in de R code van 
# het Hollandse luchten platform worden gebruikt
colnames(Luftdaten_final)[2] <- "kit_id"
colnames(Luftdaten_final)[3] <- "lat"
colnames(Luftdaten_final)[4] <- "lon"
colnames(Luftdaten_final)[10] <- "Var"
colnames(Luftdaten_final)[1] <- "date"

# Bij de onderstaande code worden de meetwaardes die P1 en P2 bevatten uit de dataset gefilterd
# Dit wordt gedaan door de kolommen op te geven die de values temperature & humidity bevatten
# Deze kolommen worden uit de dataset verwijderd 
Luftdaten_final2 <- dplyr::filter(Luftdaten_final, !grepl('temperature|humidity', Var))

View(Luftdaten_final2)

# Bij de onderstaande code worden de values van P1 en p2 uit een kolom omgezet 
# naar twee kolommen met de naam P1 en P2. De scheiding hierbij vindt plaats bij de ,

library(dplyr)
library(tidyr)

Luftdaten_final4 <- separate(Luftdaten_final2, col=value, into = c("P1","P2"), sep = ",")
View(Luftdaten_final4)

# Bij de onderstaande code worden de tekens c( uit de kolom P1 verwijderd
# Dit wordt gerealiseerd door het gedeelte rechts van het teken (
# te splitsen van de betreffende waardes
# Kolom P1 wordt vervolgens weggeschreven als een aparte tabel
P1 <- sapply(strsplit(Luftdaten_final4$P1, split='(', fixed=TRUE), function(x) (x[2]))
as.data.frame(P1)

# Bij de onderstaande code worden de tekens "..." uit de kolom P1 verwijderd
# Dit wordt gerealiseerd door de tekens "..." uit de dataset te splitsen (filteren)
# van de betreffende waardes. Het verwijderen van deze tekens is nodig om de factor 
# waardes om te kunnen zetten naar numerieke waardes
P1 <- sapply(strsplit(P1, split='"', fixed=TRUE), function(x) (x[2]))

# Bij de onderstaande code worden de factor waardes omgezet naar numerieke waardes
P1 <- as.numeric(P1)

# Met de functie cbind worden de twee data.frames aan elkaar gekoppeld 
# Het gaat om de data.frames P1 en Luftdaten_final4
Luftdaten_final_P1 <- cbind(Luftdaten_final4, P1)
View(Luftdaten_final_P1)

# Bij de onderstaande code worden de tekens ) uit de kolom P2 verwijderd
# Dit wordt gerealiseerd door het gedeelte links van het teken )
# te splitsen van de betreffende waardes
# Kolom P2 wordt vervolgens weggeschreven als een aparte tabel
P2 <- sapply(strsplit(Luftdaten_final4$P2, split=')', fixed=TRUE), function(x) (x[1]))
as.data.frame(P2)

# Bij de onderstaande code worden de tekens "..." uit de kolom P2 verwijderd
# Dit wordt gerealiseerd door de tekens "..." uit de dataset te splitsen (filteren)
# van de betreffende waardes. Het verwijderen van deze tekens is nodig om de factor 
# waardes om te kunnen zetten naar numerieke waardes
P2 <- sapply(strsplit(P2, split='"', fixed=TRUE), function(x) (x[2]))

# Bij de onderstaande code worden de factor waardes omgezet naar numerieke waardes
P2 <- as.numeric(P2)

# Met de functie cbind worden de twee data.frames aan elkaar gekoppeld 
# Het gaat om de data.frames P2 en Luftdaten_final_P1
Luftdaten_P1_P2 <- cbind(Luftdaten_final_P1, P2)

# In de onderstaande code wordt de onnodige kolom P1, P2 en Var uit de dataset verwijderd
# Dit gebeurd op basis van de positie binnen de tabel
Luftdaten_API <- Luftdaten_P1_P2[,- (9:11)] 

# In de onderstaande code worden alle coordinaten met 0.0000 uit de dataset verwijderd
Luftdaten_API <- Luftdaten_API[grep('^[1-9]', Luftdaten_API$lat),]

# In de onderstaande code worden alle NA waardes uit de dataset verwijderd
Luftdaten_API <- Luftdaten_API[grep('^[0-9]', Luftdaten_API$P1),]

# In de onderstaande code worden alle dubbele rijen op basis van sensor.id uit de dataset
# verwijderd. Hierdoor blijft van elke sensor.id alleeen de meeste actuele meetwaarde over. 
Luftdaten_API <- distinct(Luftdaten_API, sensor.id, .keep_all = TRUE)

View(Luftdaten_API)


# Met behulp van de onderstaande code wordt de data opgeslagen als rds bestand (Rstudio)
saveRDS(Luftdaten_API, "API_Luftdaten.rds")

# met behulp van qtm wordt er snel een thematische kaart gemaakt.
# Dit is mogelijk doordat de WFS uit zichzelf al een aantal instellingen heeft
# Binnen QTM is er de mogelijkheid om de symoblogie aan te passen en basemaps toe te voegen
# Met de st_union(gemeente_Arn...... geef je aan de je eengemaakte filter wilt visualiseren in de kaart)
library(leaflet) 
mymap <- leaflet(data = Luftdaten_API) %>% addTiles() %>% 
addMarkers(lat = ~lat, lng = ~lon,
popup = paste("ID", Luftdaten_API$P2))
mymap


