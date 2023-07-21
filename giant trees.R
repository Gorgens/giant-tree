## Activate packages --------------------

require(raster)
require(sp)
require(topmodel)
#require(SpaDES)   # https://www.rdocumentation.org/packages/SpaDES/versions/2.0.3
require(leaflet)
require(dplyr)

# Import layers ---------------------

t128dtm = raster("C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\NP_T-0128dtm.asc")            # Importar modelo digital de terreno
  sirgas22n = "+proj=utm +zone=22 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  crs(t128dtm) = sirgas22n
  crs(t128dtm)

t128chm = raster("C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\NP_T-0128chm.asc")            # Importar modelo digital de altura de dossel
  sirgas22n = "+proj=utm +zone=22 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  crs(t128chm) = sirgas22n
  crs(t128chm)
  
t128dsm = raster("C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\NP_T-0128dsm.asc")            # Importar modelo digital de altura de dossel
  sirgas22n = "+proj=utm +zone=22 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  crs(t128dsm) = sirgas22n
  crs(t128dsm)

windSpeed = raster("C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\wind speed.tif")            # Importar camada de velocidade de vento
  crs(windSpeed)
  # Reproject raster
  # https://www.rdocumentation.org/packages/raster/versions/2.8-19/topics/projectRaster
  windSpeed = projectRaster(windSpeed, crs=sirgas22n) # Reprojetar para SIRGAS 2000 22N

## Clip the study area and creation of new layers ---------------------

cropBox = c(242000, 246000, 79000, 82000)              # Definir área de interesse para corte

t128dtmCrop = crop(t128dtm, cropBox)                   # Crop DTM
  # Create terrain ramp colour
  # https://www.rdocumentation.org/packages/grDevices/versions/3.5.2/topics/colorRamp
  pal = terrain.colors(5)
  #image(t128dtmCrop, col=pal)
  #summary(t128dtmCrop)
  #writeRaster(t128dtmCrop, "t128dtm.tif", format = "GTiff")

t128chmCrop = crop(t128chm, cropBox)                   # Crop CHM
  palChm = colorNumeric(c("#e5f2e5", "#66b266", "#008000"), values(t128chmCrop), na.color = "transparent")
  #image(t128chmCrop)
  #summary(t128chmCrop)
  #writeRaster(t128chmCrop, "t128chm.tif", format = "GTiff")
  
t128dsmCrop = crop(t128dsm, cropBox)                   # Crop DSM
  palDsm = colorNumeric(c("#e5f2e5", "#66b266", "#008000"), values(t128dsmCrop), na.color = "transparent")
  #image(t128dsmCrop)
  #summary(t128dsmCrop)
  #writeRaster(t128dsmCrop, "t128dsm.tif", format = "GTiff")

windSpeedCrop = crop(windSpeedp, cropBox)              # Crop Wind Speed
  palWind = colorNumeric(c("#ff9999", "#ff4c4c", "#ff0000"), values(windSpeedCrop), na.color = "transparent")
  #summary(windSpeedCrop)
  #writeRaster(windSpeedCrop, "t128windSpeed.tif", format = "GTiff")

                                                       # Creating TPI in R
                                                       # https://www.rdocumentation.org/packages/raster/versions/2.8-19/topics/terrain
t128Slp = terrain(t128dtmCrop, opt='slope', unit='degrees', neighbors=8)      # Cria modelo de inclinação
  #image(t128Slp)
  #writeRaster(t128Slp, "t128slp.tif", format = "GTiff")

t128tpi = terrain(t128dtmCrop, opt='TPI', unit='degrees', neighbors=8)        # Cria topographic index
  #image(t128tpi)
  #writeRaster(t128tpi, "t128tpi.tif", format = "GTiff")

rm(t128dtm, t128chm, t128dsm, windSpeed)  
  
## Locate trees ---------------------------
  
f = function(X) max(X, na.rm=TRUE)                                  # function to return the maximum value inside window
ww = matrix(1, nrow=51, ncol=51)                                    # weight matrix for cells in moving window
localmax = focal(t128chmCrop, fun=f, w=ww, pad=TRUE, padValue=NA)   # move the function through the raster
localMaxRaster = t128chmCrop==localmax                              # create mask for local maximas
maxXY = as.data.frame(xyFromCell(localMaxRaster,                    # Create dataframe of coordinates of local maximas
                                 Which(localMaxRaster==1, cells=TRUE)))
  #image(localMaxRaster)
coordinates(maxXY) = cbind(maxXY$x, maxXY$y)                        # transform dataframe into spatial point layer
proj4string(maxXY) = CRS(sirgas22n)                                 # assign projection to spatial point layer
treeCHM = data.frame(coordinates(maxXY),                            # extract height value from CHM to each local maxima
                   extract(t128chmCrop, maxXY))
names(treeCHM) = c("x", "y", "height")                              # rename columns to match file pattern
treeCHM = treeCHM[treeCHM$height>80,]                              # filter local maximas above 80 meters
coordinates(treeCHM) = cbind(treeCHM$x, treeCHM$y)                  # transform dataframe into spatial point layer
proj4string(treeCHM) = CRS(sirgas22n)                               # assign projection to spatial point layer
                                                                    # reproject shapefile                                                                    # help: https://www.earthdatascience.org/courses/earth-analytics/spatial-data-r/reproject-vector-data/
treesWGS84 = spTransform(treeCHM, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## Import manual trees-----------------

tree80 = shapefile("trees80Sirgas.shp")
tree80WGS84 = spTransform(tree80, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


## Layers visualization ------------------------

# How to use leaflet
# https://rstudio.github.io/leaflet/showhide.html
leaflet() %>% addTiles() %>%                          # Visualizar CHM no leaflet
  #addRasterImage(t128chmCrop, colors = palChm, opacity = 0.8, group = "chm") %>%
  #addRasterImage(windSpeedCrop, colors = palWind, opacity = 0.8, group = "factors") %>%
  addCircleMarkers(data = treesWGS84, group = "trees", color = 'red') %>%
  addCircleMarkers(data = tree80WGS84, group = "trees80", color = 'black') %>%
  addLayersControl(
    overlayGroups = c("chm", "factors", "trees", "trees80"),
    options = layersControlOptions(collapsed = FALSE)
  )
