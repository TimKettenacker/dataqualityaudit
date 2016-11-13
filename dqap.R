## this file contains only the code and is stripped from the documentation
## render .rmd file to ioslides in RStudio to present cntent the customer


# first chunk: data preparation
raw <- read.csv("dqaudit_testset.csv", sep=";", na.strings = "", colClasses = c("X_Koordinate"="character", "Y_Koordinate"="character", "PLZ"="character", "POST_Postleitzahl"="character", "Ergebnisklasse_der_Zustelladresse"="character"), fileEncoding = "UTF-8")

# second chunk: data preparation
require(DT)

raw <- read.csv("dqaudit_testset.csv", sep=";", na.strings = "", colClasses = c("X_Koordinate"="character", "Y_Koordinate"="character", "PLZ"="character", "POST_Postleitzahl"="character", "Ergebnisklasse_der_Zustelladresse"="character"), fileEncoding = "UTF-8")
datatable(raw[c(8333:8336, 9933:9940, 111:113),9:12], options = list(pageLength = 5))

# third chunk: data preparation 
require(plotly)
require(ggplot2)
require(ggthemes)
require(plyr)

## make sure read file command contains na.strings parameter to be able to evaluate empty fields

raw <- read.csv("dqaudit_testset.csv", sep=";", na.strings = "", colClasses = c("X_Koordinate"="character", "Y_Koordinate"="character", "PLZ"="character", "POST_Postleitzahl"="character", "Ergebnisklasse_der_Zustelladresse"="character"), fileEncoding = "UTF-8")

e_first <- count(is.na(raw$VORNAME))
e_last <- count(is.na(raw$NAME))
e_str <- count(is.na(raw$STRASSE))
e_zip <- count(is.na(raw$PLZ))
e_city <- count(is.na(raw$STADT))

e_first$column <- "first name"
e_last$column <- "last name"
e_str$column <- "street"
e_zip$column <- "zip code"
e_city$column <- "city"

e_fields <- rbind(e_first, e_last, e_str, e_zip, e_city)
empties.df = ddply(e_fields, .(column), transform, percent = freq/sum(freq) * 100)
empties.df = ddply(empties.df, .(column), transform, pos = (cumsum(freq) - 0.5 * freq))
empties.df$label = paste0(sprintf("%.0f", empties.df$percent), "%")
g1 <- ggplot(empties.df, aes(x = column, y = freq, fill = x)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Column filling ratio") +
  geom_text(aes(y = pos, label = label), size = 3) +
  coord_flip() + theme_solarized_2() + scale_fill_manual(values=c("#8b7765", "#E69F00")) + theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank())
ggplotly(g1, tooltip = c("x", "y"))


# 4th chunk: postal validation

require(sp)
require(leaflet)

## load data, ensure conversion is omitted for geo coordinate columns
raw <- read.csv("dqaudit_testset.csv", sep=";", na.strings = "", colClasses = c("X_Koordinate"="character", "Y_Koordinate"="character", "PLZ"="character", "POST_Postleitzahl"="character", "Ergebnisklasse_der_Zustelladresse"="character"), fileEncoding = "UTF-8")

## rename geo coordinate columns to latitude and longitude
colnames(raw)[colnames(raw)=="Y_Koordinate"] <- "latitude"
colnames(raw)[colnames(raw)=="X_Koordinate"] <- "longitude"

## do some data wrangling and cleanups to bring data to spatial format
# add dot
for(c in 1:length(raw$latitude)){
  raw$latitude[c] <- sub("([[:digit:]]{2,5})$", ".\\1", raw$latitude[c])
}
for(c in 1:length(raw$longitude)){
  raw$longitude[c] <- sub("([[:digit:]]{2,5})$", ".\\1", raw$longitude[c])
}
raw$latitude <- as.numeric(raw$latitude)
raw$longitude <- as.numeric(raw$longitude)

# delete NAs
raw <- raw[!is.na(raw$latitude),]

# bind longitude and latitude to Spatials
coords <- SpatialPoints(cbind(raw$longitude, raw$latitude))
raw <- SpatialPointsDataFrame(coords, raw)
popup <- paste0(      "<center>", "<b>",
                      raw$VORNAME, " ", raw$NAME, "</center>", "</b>",  
                      "<center>",
                      raw$POST_Strasse_und_Hausnummer, ", ",
                      raw$POST_Postleitzahl, " ",
                      raw$POST_Ort,
                      "</center>")


leaflet(raw) %>% addTiles() %>% addMarkers(
  popup = popup,
  clusterOptions = markerClusterOptions()
) 


# 5th chunk: postal validation
require(sp)
require(leaflet)

## load data, ensure conversion is omitted for geo coordinate columns
raw <- read.csv("dqaudit_testset.csv", sep=";", na.strings = "", colClasses = c("X_Koordinate"="character", "Y_Koordinate"="character", "PLZ"="character", "POST_Postleitzahl"="character", "Ergebnisklasse_der_Zustelladresse"="character", "Veraenderung_Postleitzahl" = "character", "Veraenderung_Ortsname"="character", "Veraenderung_Strassenname"="character", "Veraenderung_Hausnummer"="character"), fileEncoding = "UTF-8")

## rename geo coordinate columns to latitude and longitude
colnames(raw)[colnames(raw)=="Y_Koordinate"] <- "latitude"
colnames(raw)[colnames(raw)=="X_Koordinate"] <- "longitude"

## do some data wrangling and cleanups to bring data to spatial format
# add dot
for(c in 1:length(raw$latitude)){
  raw$latitude[c] <- sub("([[:digit:]]{2,5})$", ".\\1", raw$latitude[c])
}
for(c in 1:length(raw$longitude)){
  raw$longitude[c] <- sub("([[:digit:]]{2,5})$", ".\\1", raw$longitude[c])
}
raw$latitude <- as.numeric(raw$latitude)
raw$longitude <- as.numeric(raw$longitude)
# replace Uniserv abbreviations in detected changes with more meaningful explanation
raw$Veraenderung_Postleitzahl[raw$Veraenderung_Postleitzahl == "OK"] <- "no changes to zip code"
raw$Veraenderung_Postleitzahl[raw$Veraenderung_Postleitzahl == "C"] <- "changes to zip code"
raw$Veraenderung_Postleitzahl[raw$Veraenderung_Postleitzahl == "SC"] <- "severe changes to zip code"
raw$Veraenderung_Ortsname[raw$Veraenderung_Ortsname == "OK"] <- "no changes to city name"
raw$Veraenderung_Ortsname[raw$Veraenderung_Ortsname == "C"] <- "changes to city name"
raw$Veraenderung_Ortsname[raw$Veraenderung_Ortsname == "SC"] <- "severe changes to city name"
raw$Veraenderung_Strassenname[raw$Veraenderung_Strassenname == "OK"] <- "no changes to street name"
raw$Veraenderung_Strassenname[raw$Veraenderung_Strassenname == "C"] <- "changes to street name"
raw$Veraenderung_Strassenname[raw$Veraenderung_Strassenname == "SC"] <- "severe changes to street name"
raw$Veraenderung_Hausnummer[raw$Veraenderung_Hausnummer == "OK"] <- "no changes to housenumber"
raw$Veraenderung_Hausnummer[raw$Veraenderung_Hausnummer == "C"] <- "changes to housenumber"
raw$Veraenderung_Hausnummer[raw$Veraenderung_Hausnummer == "SC"] <- "severe changes to housenumber"

# delete NAs
raw <- raw[!is.na(raw$latitude),]

# prepare data to show deviations between input data and corrected data
rescl <- cbind(raw["longitude"], raw["latitude"], raw["Ergebnisklasse_der_Zustelladresse"], raw["STRASSE"], raw["POST_Strasse_und_Hausnummer"], raw["PLZ"], raw["POST_Postleitzahl"], raw["STADT"], raw["POST_Ort"], raw["Veraenderung_Postleitzahl"], raw["Veraenderung_Ortsname"], raw["Veraenderung_Strassenname"], raw["Veraenderung_Hausnummer"])

# separate data frame in chunks of result classes and apply geospatial mapping
rescl1 <- rescl[rescl$Ergebnisklasse_der_Zustelladresse == 1,]
rescl1 <- SpatialPointsDataFrame(SpatialPoints(cbind(rescl1$longitude, rescl1$latitude)), rescl1)
rescl2 <- rescl[rescl$Ergebnisklasse_der_Zustelladresse == 2,]
rescl2 <- SpatialPointsDataFrame(SpatialPoints(cbind(rescl2$longitude, rescl2$latitude)), rescl2)
rescl3 <- rescl[rescl$Ergebnisklasse_der_Zustelladresse == 3,]
rescl3 <- SpatialPointsDataFrame(SpatialPoints(cbind(rescl3$longitude, rescl3$latitude)), rescl3)

cl1popup <- paste0(  "<center>", "<strong>", " Original: ", "</strong>", 
                     rescl1$STRASSE, ", ", rescl1$PLZ, ", ", rescl1$STADT, "</center>",  
                     "<center>", "<strong>", " Validated: " ,"</strong>",
                     rescl1$POST_Strasse_und_Hausnummer, ", ", rescl1$POST_Postleitzahl, ", ", 
                     rescl1$POST_Ort, "</center>",
                     "<center>", "<small>", "<em>", rescl1$Veraenderung_Strassenname, "</em>",                        "</small>","<center>",
                     "<center>", "<small>", "<em>", rescl1$Veraenderung_Hausnummer, "</em>",                          "</small>","<center>",
                     "<center>", "<small>", "<em>", rescl1$Veraenderung_Postleitzahl, "</em>",                        "</small>","<center>",
                     "<center>", "<small>", "<em>", rescl1$Veraenderung_Ortsname, "</em>",                            "</small>","<center>")

cl2popup <- paste0(  "<center>", "<strong>", " Original: ", "</strong>", 
                     rescl2$STRASSE, ", ", rescl2$PLZ, ", ", rescl2$STADT, "</center>",  
                     "<center>", "<strong>", " Output: " ,"</strong>",
                     rescl2$POST_Strasse_und_Hausnummer, ", ", rescl2$POST_Postleitzahl, ", ", 
                     rescl2$POST_Ort, "</center>",
                     "<center>", "<small>", "<em>", rescl2$Veraenderung_Strassenname, "</em>",                        "</small>","<center>",
                     "<center>", "<small>", "<em>", rescl2$Veraenderung_Hausnummer, "</em>",                          "</small>","<center>",
                     "<center>", "<small>", "<em>", rescl2$Veraenderung_Postleitzahl, "</em>",                        "</small>","<center>",
                     "<center>", "<small>", "<em>", rescl2$Veraenderung_Ortsname, "</em>",                            "</small>","<center>")

cl3popup <- paste0(  "<center>", "<strong>", " Original: ", "</strong>", 
                     rescl3$STRASSE, ", ", rescl3$PLZ, ", ", rescl3$STADT, "</center>",  
                     "<center>", "<strong>", " Validated: " ,"</strong>",
                     rescl3$POST_Strasse_und_Hausnummer, ", ", rescl3$POST_Postleitzahl, ", ", 
                     rescl3$POST_Ort, "</center>",
                     "<center>", "<small>", "<em>", rescl3$Veraenderung_Strassenname, "</em>",                        "</small>","<center>",
                     "<center>", "<small>", "<em>", rescl3$Veraenderung_Hausnummer, "</em>",                          "</small>","<center>",
                     "<center>", "<small>", "<em>", rescl3$Veraenderung_Postleitzahl, "</em>",                        "</small>","<center>",
                     "<center>", "<small>", "<em>", rescl3$Veraenderung_Ortsname, "</em>",                            "</small>", "<center>")

leaflet() %>% addTiles() %>%
  addCircleMarkers(data = rescl1, popup = cl1popup, group = "Class 1", radius = 6, color = "#f9d62e", opacity = 0.37, fillOpacity = 0.35) %>%
  addCircleMarkers(data = rescl2, popup = cl2popup, group = "Class 2", radius = 6, color = "#fc913a", opacity = 0.37, fillOpacity = 0.35) %>%
  addCircleMarkers(data = rescl3, popup = cl3popup, group = "Class 3", radius = 6, color = "#ff4e50", opacity = 0.37, fillOpacity = 0.35) %>%
  addLayersControl(overlayGroups = c("Class 1", "Class 2", "Class 3"), position = "topright", options = layersControlOptions(collapsed = TRUE))

# sixth chunk: postal validation
require(ggplot2)
require(ggthemes)
require(plotly)

raw <- read.csv("dqaudit_testset.csv", sep=";", na.strings = "", colClasses = c("X_Koordinate"="character", "Y_Koordinate"="character", "PLZ"="character", "POST_Postleitzahl"="character", "Ergebnisklasse_der_Zustelladresse"="character"), fileEncoding = "UTF-8")
colnames(raw)[colnames(raw)=="Ergebnisklasse_der_Zustelladresse"] <- "Result Class"
g <- ggplot(data=raw) + geom_bar(aes(x = `Result Class`), fill = "#191970", color = "#191970") + labs(title = "Frequency of result classes", x = "result class", y = "Frequency")
ggplotly(g + theme_minimal())


# seventh chunk: postal validation
require(ggplot2)
require(ggthemes)
require(plotly)

raw <- read.csv("dqaudit_testset.csv", sep=";", na.strings = "", colClasses = c("X_Koordinate"="character", "Y_Koordinate"="character", "PLZ"="character", "POST_Postleitzahl"="character", "Ergebnisklasse_der_Zustelladresse"="character"), fileEncoding = "UTF-8")
colnames(raw)[colnames(raw)=="Ergebnisklasse_der_Zustelladresse"] <- "Result Class"
g <- ggplot(data=raw) + geom_bar(aes(x = `Result Class`), fill = "#191970", color = "#191970") + labs(title = "Frequency of result classes", x = "result class", y = "Frequency")
ggplotly(g + theme_minimal())


# 8th chunk: matching
require(sp)
require(leaflet)
require(plyr)

## load data, ensure conversion is omitted for geo coordinate columns
raw <- read.csv("dqaudit_testset.csv", sep=";", na.strings = "", colClasses = c("X_Koordinate"="character", "Y_Koordinate"="character", "PLZ"="character", "POST_Postleitzahl"="character", "Ergebnisklasse_der_Zustelladresse"="character"), fileEncoding = "UTF-8")

## rename geo coordinate columns to latitude and longitude
colnames(raw)[colnames(raw)=="Y_Koordinate"] <- "latitude"
colnames(raw)[colnames(raw)=="X_Koordinate"] <- "longitude"

## do some data wrangling and cleanups to bring data to spatial format
# add dot
for(c in 1:length(raw$latitude)){
  raw$latitude[c] <- sub("([[:digit:]]{2,5})$", ".\\1", raw$latitude[c])
}
for(c in 1:length(raw$longitude)){
  raw$longitude[c] <- sub("([[:digit:]]{2,5})$", ".\\1", raw$longitude[c])
}
raw$latitude <- as.numeric(raw$latitude)
raw$longitude <- as.numeric(raw$longitude)

# delete NAs
raw <- raw[!is.na(raw$latitude),]

# subset df to head and normal duplicates
df.head_duplicates <- raw[raw$SRC_mail2Stat == '2',]
df.sub_duplicates <- raw[raw$SRC_mail2Stat == '3',]
df.duplicates <- rbind(df.head_duplicates, df.sub_duplicates)

# get distribution of duplicates per duplicate group (just as t does in the other graph)
ov <- count(df.duplicates$SRC_mail2Group)

# add column to later count frequencies of each duplicate group and mapping it to the head
df.head_duplicates$CountMatchGroupFrequency <- 1

# do the lookup in the distribution table and put frequency in CountMatchGroupFrequency
for(i in df.head_duplicates$SRC_mail2Group){
  df.head_duplicates[i,"CountMatchGroupFrequency"] <- ov[i,2] 
}

# do spatial mapping
sp.duplicates <- SpatialPointsDataFrame(SpatialPoints(cbind(df.head_duplicates$longitude, df.head_duplicates$latitude)), df.head_duplicates)

dupop <- paste0(  "<center>", "<strong>", " Duplicates: ", "</strong>", 
                  sp.duplicates$VORNAME, " ", sp.duplicates$NAME, "</center>",  
                  "<center>", sp.duplicates$POST_Strasse_und_Hausnummer, ", ",
                  sp.duplicates$POST_Postleitzahl, ", ", 
                  sp.duplicates$POST_Ort, "</center>")

leaflet(sp.duplicates) %>% addTiles() %>%
  addCircles(radius = ~CountMatchGroupFrequency * 1000, popup = dupop, color = "#c51b45", fillOpacity = 0.8)


# 9th chunk: matching

require(plotly)
require(plyr)
require(ggplot2)
require(ggthemes)

raw <- read.csv("dqaudit_testset.csv", sep=";", na.strings = "", colClasses = c("X_Koordinate"="character", "Y_Koordinate"="character", "PLZ"="character", "POST_Postleitzahl"="character", "Ergebnisklasse_der_Zustelladresse"="character"), fileEncoding = "UTF-8")

p <- count(raw$SRC_mail2Stat)
p["x"] <- c("Singles", "Head duplicates", "duplicates")
plot_ly(p, labels = x, values = freq, type = "pie")


# 10th chunk: matching

require(plotly)
require(plyr)
require(ggplot2)
require(ggthemes)
raw <- read.csv("dqaudit_testset.csv", sep=";", na.strings = "", colClasses = c("X_Koordinate"="character", "Y_Koordinate"="character", "PLZ"="character", "POST_Postleitzahl"="character", "Ergebnisklasse_der_Zustelladresse"="character"), fileEncoding = "UTF-8")
t <- as.data.frame(table(raw$SRC_mail2Group))
t <- t[-1,]
colnames(t)[colnames(t)=="Freq"] <- "# of duplicates per group"
g <- ggplot(data=t, aes(`# of duplicates per group`)) + geom_histogram(aes(fill=..count..), alpha = 0.75, binwidth = 1) + labs(title = "Frequency of duplicate groups", x = "Number of duplicates per duplicate group", y = "Frequency")
g <- g + theme_hc()
ggplotly(g, tooltip = c("x", "count"))
