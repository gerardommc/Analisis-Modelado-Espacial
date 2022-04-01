library(dismo)
library(raster)
library(rgdal)

dem <- getData("SRTM", lat = 21.076985, lon = -101.177875, path = "Actividades/Analisis-multicriterio/")

pobl <- raster("Actividades/Analisis-multicriterio/mex_ppp_2020_UNadj_constrained.tif")

pobl.s <- crop(pobl, extent(-101.2779, -101.0779, 20.97698, 21.17699))
pobl.s[is.na(pobl.s[])] <- 0

puntos <- randomPoints(pobl.s, 500)

dem.s <- crop(dem, extent(pobl.s))

puntos <- data.frame(puntos)

puntos$Altitud <-extract(dem.s, puntos)

write.csv(puntos, "Actividades/Analisis-multicriterio/Localidades-altura.csv", row.names = F)

writeRaster(pobl.s, "Actividades/Analisis-multicriterio/Poblacion", "GTiff")

## Viento

v <- stack(list.files("~/Descargas/wc2.1_30s_wind", "tif", full.names = T))


v <- crop(v, extent(pobl.s))
v.max <- max(v)

v.max <- resample(v.max, pobl.s)

writeRaster(v.max, "Actividades/Analisis-multicriterio/Velocidad-viento", "GTiff")

#Rios

rios <- readOGR("Actividades/Analisis-multicriterio/Rios/RH12Hb_dr.shp")
rios <- spTransform(rios, CRS("+init=epsg:4326"))

plot(dem.s)
plot(rios, add = T)
