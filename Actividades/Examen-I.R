library(raster)

set.seed(3561)

pop <- raster("../Examenes/Var-1.tif")

plot(pop, ext = c(-89.8, -89.4, 20.8, 21.2))

p1 <- crop(pop, extent(c(-90, -89.4, 20.8, 21.2)))



temp <- raster("../Examenes/CHELSA_bio1_1981-2010_V.2.1.tif")

temp <- crop(temp, extent(p1))

points <- dismo::randomPoints(p1, 200, prob = T)

x.noise <- rnorm(200, mean = 0, sd = 0.01)
y.noise <- rnorm(200, mean = 0, sd = 0.01)

poi.noised <- data.frame(points)

poi.noised$x <- poi.noised$x + x.noise
poi.noised$y <- poi.noised$y + y.noise

plot(p1)
points(poi.noised)

Hum.pop <- extract(p1, poi.noised[, 1:2])
Temp <- extract(temp, poi.noised[, 1:2])

alpha <- rnorm(200, 10, sd = 2)
beta.1 <- rnorm(200, mean = 0.001, sd = 0.000025)
beta.2 <- rnorm(200, mean = 0.1, sd = 0.005)

Accidentes <- alpha + beta.1 * Hum.pop + beta.2 * Temp
Accidentes <- sapply(Accidentes, function(x){rpois(1, x)})


poi.noised$Accidentes <- Accidentes
poi.noised$Temp <- round(sapply(Temp, function(x){rnorm(1, x, 1)}), 1)

t.range <- raster("../Examenes/CHELSA_bio19_1981-2010_V.2.1.tif")
t.range <- crop(t.range, c(-90, -89.4, 20.8, 21.2))

writeRaster(p1, "../Examenes/Poblacion", "GTiff")

pairs(stack(temp, t.range))

writeRaster(t.range, "../Examenes/Var-2.tif", "GTiff")

head(poi.noised)

write.csv(poi.noised, "../Examenes/Datos-examen.csv")
