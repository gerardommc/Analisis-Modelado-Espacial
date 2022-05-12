library(raster)

r <- stack(list.files("Actividades/Uso de suelo/", "tif", full.names = T))

ext <- raster("Unidad-IV/Densidad-pob.tif")

r <- crop(r, extent(ext))

plot(r)

writeRaster(r[[1]], "Actividades/Uso de suelo/Poblacion-MID-2020", "GTiff")
writeRaster(r[[2]], "Actividades/Uso de suelo/Poblacion-MID-2000", "GTiff")
