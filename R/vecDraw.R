
#' digitizing vector features using a simple leaflet base map
#'
#' @description  vecDraw is designed for straightforward digitizing of simple geometries without adding attributes. It provides a bunch of leaflet base maps and optionally a sf* object can be loaded for orientation. 
#'
#' @note Yu can either save the digitized object to a json (JS) or kml (KML) file.
#' @param mapCenter center of the leaflet map
#' @param zoom set initial zoom level of leaflet map
#' @param line enable/disable line tool
#' @param poly enable/disable polygon tool 
#' @param circle enable/disable circle tool
#' @param rectangle enable/disable polygon tool
#' @param point enable/disable point tool
#' @param remove enable/disable the remove feature of the draw tool
#' @param position  toolbar layout (topright, topleft, bottomright, bottomleft)
#' @param cex size of item 
#' @param lwd line width of item
#' @param opacity opacity of item
#' @param maplayer string as provided by leaflet-provider 
#' @param preset character default is "uav" for line based mission digitizing, "ext" for rectangles, NULL for all drawing items
#' @param locPreset character location preset, default is "muf" for Marburg University Forest,  "tra" Traddelstein, "hag" Hagenstein, "baw" Bayerwald.
#' @param overlay optional sp* object may used for orientation  
#' 
#'
#' @examples
#' \dontrun{
#' # fully featured without overlay
#' require(mapview)
#' 
#' # preset for digitizing uav flight areas using Meuse data set as overlay
#' require(sp)
#' data(meuse) 
#' sp::coordinates(meuse) <- ~x+y 
#' sp::proj4string(meuse) <-CRS("+init=epsg:28992") 
#' m <- sp::spTransform(meuse,CRSobj = sp::CRS("+init=epsg:4326"))
#' vecDraw(overlay = m, preset = "uav")
#'   
#' # preset for digitizing simple rectangles extents
#' vecDraw(preset="ext",overlay = m)
#'}
#' @export vecDraw

vecDraw <- function(mapCenter=NULL,
                     zoom=15, 
                     line = TRUE, 
                     rectangle = TRUE, 
                     poly = TRUE, 
                     circle = TRUE, 
                     point = TRUE,
                     remove = TRUE, 
                     position= "topright", 
                     maplayer=c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","Thunderforest.Landscape","OpenTopoMap"),
                     overlay=NULL,

                     preset = "all",
                     locPreset = "muf",
                     cex = 10,
                     lwd = 2,

                     opacity = 0.7) {
  alpha = 0.6  
  features=NULL
  if (is.null(mapCenter)) {
    if ( locPreset == "muf") {
      mapCenter<-c(50.84,8.68)
    } else if (locPreset == "tra") {
      mapCenter<-c(51.13,8.97)  
    } else if (locPreset == "hag") {
      mapCenter<-c(51.16,8.90)  
    }else if (locPreset == "baw") {
      mapCenter<-c(48.92,13.40)  
    }
  }
  
  else {
    mapCenter<-mapCenter
  }
  
  # create tmp path
  tmpPath<- createTempDataTransfer()
  
  if (!is.null(overlay)){

    if  (methods::is(overlay[1], "character")) {stop("overlay has to be a sp* object") }
    
    if (methods::is(overlay[1],"SpatialPointsDataFrame") | 
        methods::is(overlay[1],"SpatialLinesDataFrame") |
        methods::is(overlay[1],"SpatialPoints")) {
      #e <- as(raster::extent(overlay), "SpatialPolygons")
      #e <- sp::SpatialPolygonsDataFrame(e, data.frame(ID="overlay"))
      sp::proj4string(overlay) <- sp::proj4string(overlay)
      #overlay<-sp::spTransform(overlay,CRSobj = sp::CRS("+init=epsg:4326"))
    } 
    if  (methods::is(overlay[1],"SpatialPolygonsDataFrame")) {
      overlay<-sp::spTransform(overlay,CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
      #overlay <- sp::SpatialPolygonsDataFrame(overlay, data.frame(ID="overlay"))
    } 
    if (methods::is(overlay[1],"sf") | 
        methods::is(overlay[1],"sfc")) {

    
    sf::st_write(overlay, paste(tmpPath, "jsondata", sep=.Platform$file.sep), "OGRGeoJSON", driver="GeoJSON")
    
    # for fastet json read in a html document we wrap it with var data = {};
    # and we fix the crs item of ogr2json
    # TODO loop a list of data
    
    # main data object
    lns <- data.table::fread(paste(tmpPath, "jsondata", sep=.Platform$file.sep), header = FALSE, sep = "\n", data.table = FALSE)
    
    # do it for main
    lns[1,] <-paste0('var jsondata = {')
    lns[3,]<-paste0('"crs": { "type": "name", "properties": { "name": "EPSG:4326" } },')
    lns[length(lns[,1]),]<- '};'
    utils::write.table(lns, paste(tmpPath, "jsondata", sep=.Platform$file.sep), sep="\n", row.names=FALSE, col.names=FALSE, quote = FALSE)
    features<-names(overlay)
    # correct if only Lines or Polygons (obsolete here?)
    # if (methods::is(overlay)[1] == 'SpatialPolygonsDataFrame' | methods::is(overlay)[1] == 'SpatialPolygons'){
    #   noFeature <- length(overlay@polygons)
    # } else if (methods::is(overlay)[1] == 'SpatialLinesDataFrame' | methods::is(overlay)[1] == 'SpatialLines'){
    #   noFeature <- length(overlay@lines)
    # } 
    jsondata<-1
    
    
    
    mapCenter<-c(sf::st_bbox(overlay)[2] +(sf::st_bbox(overlay)[4]-sf::st_bbox(overlay)[2]),sf::st_bbox(overlay)[1]+(sf::st_bbox(overlay)[3]-sf::st_bbox(overlay)[1]))
    features<-overlay
    
    }  else if (!methods::is(overlay[1],"sf") | 
    
        !methods::is(overlay[1],"sfc")) {
    # #  sf::sf::st_write(overlay, dsn = paste(tmpPath, "jsondata", sep=.Platform$file.sep), layer = paste(tmpPath, "jsondata", sep=.Platform$file.sep), driver = "Esri", update = TRUE)
       sf::st_write(sf::st_as_sf(overlay), dsn = paste(tmpPath, "jsondata", sep=.Platform$file.sep), layer = paste(tmpPath, "jsondata.shp", sep=.Platform$file.sep),driver="GeoJSON", append = FALSE)
       
       conn<-file(paste(tmpPath, "jsondata", sep=.Platform$file.sep))
       lns <- readLines(paste(tmpPath, "jsondata", sep=.Platform$file.sep))
       
       text1 <-paste0('var jsondata = ')
       text2 <- ';'
    #   # open the file and read in all the lines 
    # 
    # # concatenate the old file with the new text
     newjson <- c(text1,lns[1:length(lns)],text2) 
     writeLines(newjson, conn, sep="\n")
     close(conn)
    # 
       jsondata<-0
   }
  } else {jsondata<-0}
  if ( preset == "uav") {
    if (is.null(mapCenter)){
      mapCenter<-c(50.80801,8.72993)}
    else {
      mapCenter<-mapCenter
    }
    zoom<-15 
    line<-TRUE
    rectangle<-FALSE
    poly<-FALSE
    circle<-FALSE
    point<-FALSE
    remove<-TRUE
    maplayer=c("Esri.WorldImagery","OpenStreetMap","Thunderforest.Landscape","OpenTopoMap")
    overlay=overlay
  } 
  else if (preset == "ext") {
    if (is.null(mapCenter)){
      mapCenter<-c(50.80801,8.72993)}
    else {
      mapCenter<-mapCenter
    }
    zoom<-10
    line<-FALSE
    rectangle<-TRUE
    poly<-FALSE
    circle<-FALSE
    point<-FALSE
    remove<-FALSE   
    position<-"topright"
    maplayer=c("OpenStreetMap","CartoDB.Positron","Esri.WorldImagery","Thunderforest.Landscape","OpenTopoMap")
    overlay=NULL
  } else {
    mapCenter<-mapCenter
    zoom<-zoom
    line<-line
    maplayer=c("OpenStreetMap","CartoDB.Positron","Esri.WorldImagery","Thunderforest.Landscape","OpenTopoMap")
    overlay=overlay
    rectangle<-rectangle
    poly<-poly
    circle<-circle
    point<-point
    remove<-remove
    position<-position
  }
  
  
  ### create the rest of the JS strings
  CRSvarMapCenter<-paste0('var mapCenter = [',mapCenter[1],',',mapCenter[2],'];')
  CRSinitialZoom<-paste('var initialZoom = ',zoom,';')
  
  ### write it to CRS.js
  # assign tmpfilename for CRS definition
  tmpCRS <- paste0(tmpPath,"/crs.js")
  # write the proj4leaflet CRS
  write(CRSinitialZoom,tmpCRS,append = TRUE)
  write(CRSvarMapCenter,tmpCRS,append = TRUE)
  
  
  # create parameter list for the widget
  x <- list(data  = 'undefined',
            features=features,
            layer=maplayer,
            zoom = zoom,
          #  html = getPopupStyle(),
            #refpoint=refpoint,
            line=line,
            rectangle=rectangle,
            poly=poly,
            circle=circle,
            point=point,
            remove=remove,
            position=position,
            scaleBar=TRUE,
            color=mapview::mapviewGetOption("raster.palette")(256),
            na.color=mapview::mapviewGetOption("na.color"),
            cex = cex,
            lwd = lwd,
            alpha = alpha,
            legend = FALSE,
            opacity = opacity,
            overlay=jsondata
            
  )
  vecDrawInternal(tmpPath, x = x)  
}






