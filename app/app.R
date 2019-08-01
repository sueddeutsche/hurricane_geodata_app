library(shiny)
library(rvest)
library(dplyr)
library(stringr)
library(rgdal)
library(geojsonio)
library(rmapshaper)
library(geojson)

ui <- fluidPage(
    mainPanel(
      titlePanel("Hurrikan-Daten downloaden"),
      selectInput("hurricane_select", "Hurrikan auswählen",
                  c("Warte auf aktuelle Daten...")),
      p(),
      downloadButton("download_storm", "Download"),
      p("Die Daten können dann entpackt und in Datawrapper oder Maps4News als Layer angezeigt werden.")
  ),
  hr(),
  strong(paste0("Stand NOAA-Daten:")),
  textOutput("text_stand"),
  strong("Datenquelle:"),
  a("National Hurricane Center, NOAA, USA", href = "https://www.nhc.noaa.gov/gis/"),
  p(),
  strong("Programmierung:"), 
  a("Benedict Witzenberger, Süddeutsche Zeitung", href = "mailto:benedict.witzenberger@sz.de"),
  p(),
  strong("Idee und Datenskript:"), 
  a("Marie-José Kolly, NZZ", href = "https://github.com/nzzdev/st-methods/tree/master/1825-hurrikan%20kartenmethodik")
)

server <- function(input, output, session) {
  
  Sys.setlocale("LC_TIME", locale = "en_US.UTF-8")
  
  current_year <- reactive({
    format(Sys.Date(), format = "%Y")
  })
  
  
  timestamp_output <- reactive({
    
    html_stand <- read_html("https://www.nhc.noaa.gov/gis/") %>% 
      html_nodes(css = "b > i") %>%
      .[[2]] %>% 
      html_text() %>% 
      str_replace("\\n", "") %>% 
      str_remove("As of") %>% 
      str_remove("UTC") %>% 
      trimws()
      
      time_formatted <- as.POSIXct(html_stand, tz = "UTC", format = "%a, %d %b %Y %H:%M:%S")
      attributes(time_formatted)$tzone <- "Europe/Berlin"
      time_formatted <- format(time_formatted, "%d.%m.%Y %H:%M")
      time_formatted
  })
  
  output$text_stand <- renderText({
    timestamp_output()
  })
  
  storm_table <- reactive({
    
    html <- read_html(paste0("https://www.nhc.noaa.gov/gis/archive_forecast.php?year=", current_year()))
    
    html %>% 
      html_node("table") %>% 
      html_table() %>% 
      .[2:nrow(.),] -> storm_table
    
    rownames(storm_table) <- c(1:nrow(storm_table))
    
    storm_table
  })
  
  # Scrape Hurricanes and offer as Selection
  observe({
    
    # First: Show all hurricanes in the current year
    
    storm_vector <- paste0(match(storm_table()$X2, storm_table()$X2), ": ", storm_table()$X2, " (ID: ", storm_table()$X1, ")")
    
    # Can also set the label and select items
    updateSelectInput(session, "hurricane_select",
                      label = "Hurrikan auswählen",
                      choices = storm_vector,
                      selected = head(storm_vector, 1)
    )
  })
  
  # calculate storm id
  storm_id <- reactive({
    id_index <- as.numeric(str_extract(input$hurricane_select, "^\\d+"))
    storm_table()$X1[id_index]
  })
  
  storm_name <- renderText({
    id_index <- as.numeric(str_extract(input$hurricane_select, "^\\d+"))
    
    storm_name <- strsplit(storm_table()$X2[id_index], " ")[[1]]
    storm_name <- strsplit("Hurrican FLOSSIE", " ")[[1]]
    storm_name <- paste(toupper(substring(storm_name, 1,1)), tolower(substring(storm_name, 2)),
          sep = "", collapse = " ")
    storm_name
  })
  
  # download files for selected storm
  output$download_storm <- downloadHandler(filename = function() {
    paste0(storm_id(), format(Sys.time(), "%d-%m-%Y_%H-%M"), ".zip") 
  }, 
                                           content = function(zipfile) {
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Hole Hurricane-Daten", value = 0)
    
    # define colors
    seqOpaque <- c("#ce1b1b", "#d4362a", "#de5c47", "#e88570", "#eea492", "#efb2a2", "#efc0b3", "#efccc1", "#efd7ce", "#eee0d8", "#ece8e3")
    seqStrength <- c("DB" = "#99d6d3", "HU" = "#d65db3", "LO" = "#d6ecd4", "TS" = "#9e92e3")
    seq <- paste0(seqOpaque, 99) #add some transparency
    trackCol <- "#05032D"
    
    # define projection
    wgs84CRS <- CRS("+init=epsg:4326")
    
    # small function to change kmz to kml
    change_to_kml <- function(x) {
      gsub("kmz", "kml", x, fixed = TRUE)
    }
    
    to_lowerID <- function(x) {
      extraction <- tolower(str_extract(x, "^(\\D+)"))
      str_replace(x, "^(\\D+)", extraction)
    }
    
    # Download files
    progress$inc(1/9, detail = paste("Download läuft"))
    ## First: Scrape Advisory Forecast Track
    url_adv <- paste0("https://www.nhc.noaa.gov/gis/archive_forecast_results.php?id=", storm_id(), "&year=", current_year())
    
    html_adv <- read_html(url_adv)
    
    html_adv %>% 
      html_nodes("a") %>% 
      html_text() %>% 
      .[grepl("adv_TRACK", ., fixed = T)] %>% # Target: Highest Number with _TRACK -> find last one in vector
      tail(., n = 1) -> adv_filename
    
    url_adv_file <- paste0("https://www.nhc.noaa.gov/storm_graphics/api/", adv_filename)  
    
    download.file(url_adv_file, destfile = paste0("downloads/", adv_filename))
    
    unzip(paste0("downloads/", adv_filename), exdir = "downloads/")
    
    #filename_adv <- file.path("downloads", change_to_kml(adv_filename))
    track <- readOGR(paste0("downloads/", change_to_kml(to_lowerID(adv_filename))), require_geomType = "wkbLineString")
    points_adv <- readOGR(paste0("downloads/", change_to_kml(to_lowerID(adv_filename))), require_geomType = "wkbPoint", pointDropZ = T)
    
    progress$inc(1/7, detail = paste(adv_filename, " gedownloadet"))
    
    # Second: Scrape Preliminary Best Track
    
    # Find the file with zip ending
    
    url_best <- paste0("https://www.nhc.noaa.gov/gis/archive_besttrack_results.php?id=", storm_id(), "&year=", current_year())
    
    html_best <- read_html(url_best)
    
    html_best %>% 
      html_nodes("a") %>% 
      html_text() %>% 
      .[grepl(".zip", ., fixed = T)] %>% 
      tail(., n = 1) -> best_filename
    
    url_best_file <- paste0("https://www.nhc.noaa.gov/gis/best_track/", best_filename)  
    
    download.file(url_best_file, destfile = paste0("downloads/", best_filename))
    
    unzip(paste0("downloads/", best_filename), exdir = "downloads/")
    
    # get ID from best_filename
    id_best <- toupper(str_extract(best_filename, "^\\D+[0-9]+"))
    
    pasttrack_pts <- readOGR(paste0("downloads/", id_best, "_pts.shp"))
    pasttrack_lin <- readOGR(paste0("downloads/", id_best, "_lin.shp"))
    
    progress$inc(1/5, detail = paste(id_best, "-Track gedownloadet"))
    
    ## Third: Scrape Wind Probabilities for 34knots
    url_wind <- paste0("https://www.nhc.noaa.gov/gis/archive_wsp.php?year=", current_year())
    
    html_wind <- read_html(url_wind)
    
    html_wind %>% 
      html_nodes("a") %>% 
      html_text() %>% 
      .[grepl("34knt", ., fixed = T)] %>% 
      tail(., n = 1) -> wind_filename
    
    url_wind_file <- paste0("https://www.nhc.noaa.gov/gis/forecast/archive/", wind_filename)  
    
    download.file(url_wind_file, destfile = paste0("downloads/", wind_filename))
    
    unzip(paste0("downloads/", wind_filename), exdir = "downloads/")
    
    windspeed <- readOGR(paste0("downloads/", change_to_kml(wind_filename)))
    windspeed <- readOGR(paste0("downloads/", change_to_kml(wind_filename)))
    
    progress$inc(1/3, message = "Berechnung läuft", detail = paste(wind_filename, "gedownloadet"))
    
    # PROCESS DATA #
    
    ##
    # Windspeed
    
    ## simplify shape so the file doesn't take too long to load in Q:
    windspeed <- ms_simplify(windspeed, keep = .05) # this function from the library rmapshaper uses the Visvalingam simplification method
    
    # round coordinates: 5 decimal places = 1 meter (4 places = 10 meters)
    for (i in 1:length(windspeed@polygons)){ # 11 polygons = 11 probability buckets
      windspeed@polygons[[i]]@Polygons[[1]]@coords <- round(windspeed@polygons[[i]]@Polygons[[1]]@coords, 5)
    }
    
    for (i in 1:(length(windspeed@polygons) - 1)){ # one (90% = no hole) or two lines (hole in polygon)
      windspeed@polygons[[i]]@Polygons[[2]]@coords <- round(windspeed@polygons[[i]]@Polygons[[2]]@coords, 5)
    }
    # ignore warning, if there is one
    
    # add properties to geojson
    windspeed$fill <- rev(seqOpaque[1:nrow(windspeed@data)])
    windspeed$`fill-opacity` <- rep(.5, nrow(windspeed))
    windspeed$stroke <- rev(seqOpaque[1:nrow(windspeed@data)])
    windspeed$`stroke-opacity` <- rep(1, nrow(windspeed))
    windspeed$`stroke-width` <- rep(.5, nrow(windspeed))
    windspeed$Name <- gsub("%", "", windspeed$Name)
    windspeed$label <- paste0(" ")
    
    if (">90" %in% windspeed@data[,1]) windspeed@data[which(windspeed@data[,1] == ">90"),]$`fill-opacity` <- .75 # stronger opacity for >90%
    if ("<5" %in% windspeed@data[,1]) windspeed@data[which(windspeed@data[,1] == "<5"),]$`label` <- paste0(windspeed$Name[1], "%") #  only keep label for >90% and <5%
    if (">90" %in% windspeed@data[,1]) windspeed@data[which(windspeed@data[,1] == ">90"),]$`label` <- paste0(windspeed$Name[11], "%")
    
    # reverse order, as we want 90% to appear first in the legend
    windspeed <- windspeed[rev(order(as.numeric(row.names(windspeed)))),]
    
    progress$inc(1/2.5, message = "Berechnung läuft", detail = paste("Wind berechnet"))
    
    ##
    ## Predicted Track:
    
    # keep the 120-hour forecast track and get rid of the others
    track <- track[which(grepl("120 Hour", track@data[,2]) == T),]
    
    # add label for legend
    track$name <- "Verlauf (Prognose: gepunktet)"
    
    # add color and line type
    track$stroke <- rep(trackCol, nrow(track))
    track$`stroke-width` <- rep(1, nrow(track))
    track$dashArray <- rep(2, nrow(track))
    
    # get rid of columns Name and Description
    track <- track[,-c(1:2)]
    
    progress$inc(1/2.3, message = "Berechnung läuft", detail = paste("Track berechnet"))
    ##
    # Points
    #points_adv <- readOGR(paste0("downloads/al032019_004adv_TRACK.kml"), require_geomType = "wkbPoint")
    
    # extract label information // bigger change to NZZ's skript - using Regex here
    # set variable for: is description written with capital D?
    description_path <- names(points_adv@data)[grepl("escription", names(points_adv@data))]
    
    if (substr(description_path, 1, 1) == "D") {
      pattern <- paste(paste0("(?<=", month.name, "\\s)\\d+"), collapse = "|")
      day <- str_extract(points_adv@data$Description, pattern)
      
      pattern <- paste0(month.name, collapse = "|")
      month <- str_extract(points_adv@data$Description, pattern)
    } else {
      pattern <- paste(paste0("(?<=", month.name, "\\s)\\d+"), collapse = "|")
      day <- str_extract(points_adv@data$description, pattern)
      
      pattern <- paste0(month.name, collapse = "|")
      month <- str_extract(points_adv@data$description, pattern)
    }

    monthTranslator <- function(x) gsub("January", "Januar", x) %>%
      gsub("February", "Februar", .) %>%
      gsub("March", "März", .) %>%
      gsub("May", "Mai", .) %>%
      gsub("June", "Juni", .) %>%
      gsub("July", "Juli", .) %>%
      gsub("October", "Oktober", .) %>%
      gsub("December", "Dezember", .)
    
    month <- monthTranslator(month)
    
    withLogErrors({
      points_adv@data$name <- paste(day, month, sep = ". ")
    })
    
    points_adv$color <- "#000000"

    # add label type and label position
    points_adv$type <- rep("pointLightLabel", nrow(points_adv)) #pointLightLabel (small) or pointHeavyLabel (larger) or pointOnly (no label, which is not what we want, here)
    points_adv$labelPosition <- c("bottom", rep("top",floor(nrow(points_adv)-1)))
    
    # if day starts with 0, delete that 0
    for (i in 1:nrow(points_adv)) ifelse(substr(points_adv$label[i], 1,1) == 0, points_adv$label[i] <- substr(points_adv$label[i], 2,nchar(points_adv$label[i])), points_adv$label[i] <- points_adv$label[i])
    
    # get rid of columns Name and Description
    withLogErrors({
      points_adv <- points_adv[,-c(1:(ncol(points_adv) - 4))]
    })
    # option for later:
    # keep first and last point of forecast, toss the rest (additional points - out of this data - can be added by code or by hand, on a case-by-case base, by looking at points@data)
    # right now: return all points
    # points_adv <- points_adv[c(1, nrow(points_adv)),]

    progress$inc(1/2, message = "Berechnung läuft", detail = paste("Points berechnet"))
    ##
    # Past Track

    # add label for legend
    pasttrack_lin$name <- rep("Bisheriger Verlauf", nrow(pasttrack_lin)) #for the day we get dashed legends in Q: pasttrack_lin$label[1]<-"Bisheriger Verlauf"
    
    # add color and line type
    pasttrack_lin$stroke <- rep(trackCol, nrow(track))
    pasttrack_lin$`stroke-width` <- rep(1, nrow(track))
    pasttrack_lin$dashArray <- rep(0, nrow(pasttrack_lin))
    
    # get rid of all columns but label, stroke, stroke-width, dashArray
    pasttrack_lin <- pasttrack_lin[,c((ncol(pasttrack_lin) - 3):(ncol(pasttrack_lin)))]

    ## 
    # Past Points
    
    # extract label information
    day <- as.character(pasttrack_pts$DAY)
    month <- as.character(pasttrack_pts$MONTH)
    
    monthNumberTranslator <- function(x) gsub("01", "Januar", x) %>%
      gsub("02", "Februar", .) %>%
      gsub("03", "März", .) %>%
      gsub("04", "April", .) %>%
      gsub("05", "Mai", .) %>%
      gsub("06", "Juni", .) %>%
      gsub("07", "Juli", .) %>%
      gsub("08", "August", .) %>%
      gsub("09", "September", .) %>%
      gsub("10", "Oktober", .) %>%
      gsub("11", "November", .) %>%
      gsub("12", "Dezember", .)
    
    month <- monthNumberTranslator(month)
    
    pasttrack_pts$name <- paste(day, month, sep = ". ")
    pasttrack_pts$color <- "#000000"
    
    progress$inc(1/1.9, message = "Berechnung läuft", detail = paste("Pasttrack berechnet"))
    
    # if we want classification by stormtype:
    # pasttrack_pts$color <- seqStrength[pasttrack_pts$STORMTYPE]
    
    # if day starts with 0, delete that 0
    for (i in 1:nrow(pasttrack_pts)) ifelse(substr(pasttrack_pts$label[i], 1,1) == 0, pasttrack_pts$label[i]<-substr(pasttrack_pts$label[i], 2,nchar(pasttrack_pts$label[i])), pasttrack_pts$label[i] <- pasttrack_pts$label[i])
    
    # add label type and label position
    pasttrack_pts$type <- rep("pointLightLabel", nrow(pasttrack_pts)) #pointLightLabel (small) or pointHeavyLabel (larger) or pointOnly (no label, which is not what we want, here)
    pasttrack_pts$labelPosition <- rep("bottom", nrow(pasttrack_pts))
    
    # get rid of all columns but label, type, labelPositon
    pasttrack_pts <- pasttrack_pts[,c((ncol(pasttrack_pts) - 3):(ncol(pasttrack_pts)))]
    
    # keep first point of past track, toss the rest (additional points - out of this data - can be added by code or by hand, on a case-by-case base, by looking at pasttrack_pts@data)
    pasttrack_pts <- pasttrack_pts[1,]
  
    progress$inc(1/1.3, message = "Daten werden gespeichert", detail = paste("Daten berechnet"))
    
    ##
    # BIND DATASETS TOGETHER #
    ##
    
    ## lines will not be bind
    
    # attribute projection to pasttrack_lin
    pasttrack_lin <- spTransform(pasttrack_lin, proj4string(track))
    
    ## points
    
    # attribute projection to pasttrack_pts
    pasttrack_pts <- spTransform(pasttrack_pts, proj4string(points_adv))
    
    # bind
    withLogErrors({
      allPoints <- rbind(pasttrack_pts, points_adv)
    })
    
    # add "useForInitialView":true to points
    allPoints$useForInitialView <- rep(TRUE, nrow(allPoints))
    
    progress$inc(1/1.1, message = "Speichern...")
    
    ##
    # Create GeoJsons
    
    filenameWindspeed <- paste0("output/", "hurricaneProb_", Sys.Date(), ".geojson")
    filenameTracksFuture <- paste0("output/", "hurricaneTracksFuture_", Sys.Date(), ".geojson")
    filenameTracksHistoric <- paste0("output/", "hurricaneTracksHistoric_", Sys.Date(), ".geojson")
    filenamePoints <- paste0("output/", "hurricanePoints_", Sys.Date(), ".geojson")
    
    # write windspeed probabilities
    writeOGR(windspeed, filenameWindspeed, layer = "windspeed", driver = "GeoJSON", check_exists = TRUE,
             overwrite_layer = TRUE)
    
    # write track future
    writeOGR(track, filenameTracksFuture, layer = "track", driver = "GeoJSON", check_exists = TRUE,
             overwrite_layer = TRUE)
    
    # write track historic
    writeOGR(pasttrack_lin, filenameTracksHistoric, layer = "pasttrack_lin", driver = "GeoJSON", check_exists = TRUE,
             overwrite_layer = TRUE)
    
    # write points
    writeOGR(allPoints, filenamePoints, layer = "points", driver = "GeoJSON", check_exists = TRUE,
             overwrite_layer = TRUE)
    
    progress$inc(1/1, detail = paste("Daten gespeichert."))
    
    # Create Output.txt file containing all data
    title <- paste0("Überschrift: ", storm_name())
    subtitle <- paste0("Unterzeile: ", "Bisheriger Verlauf und zukünftiger Pfad")
    legend_title <- paste0("Titel der Legende: ", "Wahrscheinlichkeit in Prozent für Auftreten des Hurrikans.")
    timestamp <- paste0("Stand: ", timestamp_output(), " Uhr")
    source_text <- "Quelle: National Hurricane Center, NOAA"
    
    content_txt <- c(title, subtitle, legend_title, timestamp, source_text)
    
    write.table(content_txt, file = "output/LiesMich.txt", sep = "\t",
                row.names = FALSE, quote = FALSE, col.names = "Infos zum Hurrikan:")
    
    # zip all files in the end, before moving to download
    files_to_zip <- list.files("output", pattern = ".", full.names = TRUE)
    
    zip(zipfile, files_to_zip)
  })
  
  # delete all downloaded and created files
  session$onSessionEnded(function() { 
    system(paste("rm -r", "downloads/*"))
    system(paste("rm -r", "output/*"))
  })
  
}

shinyApp(ui, server)
