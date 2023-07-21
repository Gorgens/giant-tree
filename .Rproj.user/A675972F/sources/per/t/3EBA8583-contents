#
# Author: Prof. Eric Bastos Gorgens
# Title: Extreção de árvores usando o LiDR - Transecto
#
############################################################################33

require(lidR)
require(magrittr)
require(dynatopmodel)
require(raster)

rm(list = ls(globalenv()))
gc()

# Importa o catalog do transecto -------------------------------------------
ctg = catalog('C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\NP_T-0128_FWF.las')
#ctg = catalog('C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\tree segmentation\\T0128_1.las')
  projection(ctg) = '+init=epsg:31976'

  #summary(ctg)
  opt_chunk_size(ctg) = 100          # define o tamanho em metros do tile
  opt_chunk_buffer(ctg) = 5         # define o buffer em metros de cada tile

opt_output_files(ctg) = 'C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\tree segmentation transect\\gnd\\T0128gnd_{ID}'
ctg %>% plot(chunk_pattern = TRUE)

# Filtra pontos do terreno -------------

gndClassification = function(cloud){
  las = cloud %>% readLAS()
  if(is.empty(las)) return(NULL)
  lascsf = las %>% lasground(csf())
  lascsf %<>% lasfilter(buffer == 0)
  return(lascsf)
}

lascsf_list = catalog_apply(ctg, gndClassification)


# Cria o mdt -----------------------------------------------
ctg = catalog('C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\tree segmentation\\gnd\\')
  
opt_output_files(ctg) = 'C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\tree segmentation\\dtm\\T0128dtm_{ID}'
ctg %>% plot(chunk_pattern = TRUE)

dtmCreation = function(cloud)
{
  lascsf = cloud %>% readLAS()
  if(is.empty(lascsf)) return(NULL)
  dtm = lascsf %>% grid_terrain(1, knnidw())
  return(dtm)
}

dtm_list = catalog_apply(ctg, dtmCreation)

dtm_list2 = lapply(dtm_list,raster)
dtm = do.call(merge, dtm_list2)
  plot(dtm)

# Normalizar a nuvem --------------------
ctg = catalog('C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\tree segmentation\\gnd\\')

opt_output_files(ctg) = 'C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\tree segmentation\\norm\\T0128norm_{ID}'
ctg %>% plot(chunk_pattern = TRUE)
  
normalization = function(cloud, dtm)
{
  lascsf = cloud %>% readLAS()
  if(is.empty(lascsf)) return(NULL)
  lasnorm = lascsf %>% lasnormalize(knnidw())
  lasnorm %<>% lasfilter(buffer == 0)
  return(lasnorm)
}

lasnorm_list = catalog_apply(ctg, normalization)

# Criar CHM ---------------------
ctg = catalog('C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\tree segmentation\\norm\\')

opt_output_files(ctg) = 'C:\\Users\\gorge\\Documents\\GIS DataBase\\FES Paru\\tree segmentation\\chm\\T0128chm_{ID}'
ctg %>% plot(chunk_pattern = TRUE)

chmCreation = function(cloud)
{
  lasnorm = cloud %>% readLAS()
  if(is.empty(lasnorm)) return(NULL)
  chm = lasnorm %>% grid_canopy(0.5, p2r(subcircle = 0.15))
  return(chm)
}

chm_list = catalog_apply(ctg, chmCreation)

chm_list2 = lapply(chm_list, raster)
chm = do.call(merge, chm_list2)
plot(chm)

# Extrair top trees ------------------

kernel = matrix(1,3,3)
schm = raster::focal(chm, w = kernel, fun = median, na.rm = TRUE)
ttops = tree_detection(schm, lmf(30))   # Encontrar pontos de máximo
  head(ttops)
  col1 = height.colors(50)
  plot(schm, col = col1)
  plot(ttops, col = "black", add = T)