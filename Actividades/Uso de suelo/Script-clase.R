library(raster)
library(rasterVis)

r <- stack("Actividades/Uso de suelo/Poblacion-MID-2000.tif",
           "Actividades/Uso de suelo/Poblacion-MID-2020.tif")

levelplot(r)

r.urb <- r > 2500
plot(r.urb)

plot(r.urb[[1]] + r.urb[[2]])
plot(sum(r.urb))

#Suburbanas

r.sub.1 <- r <= 2500
r.sub.2 <- r > 1000

r.sub <- r.sub.1*r.sub.2

plot(r.sub)

### Rural

r.rur.1 <- r <= 1000
r.rur.2 <- r > 20

r.rur <- r.rur.1 * r.rur.2

plot(sum(r.rur))

### Otro

r.otro <-  r <= 20

plot(r.otro)
plot(sum(r.otro))

###
r.urb.1 <- data.frame(rasterToPoints(r.urb))
r.sub.11 <- data.frame(rasterToPoints(r.sub))
r.rur.11 <- data.frame(rasterToPoints(r.rur))
r.otro.1 <- data.frame(rasterToPoints(r.otro))

r.sub.11[r.sub.11 == 1] <- 2
r.rur.11[r.rur.11 == 1] <- 3
r.otro.1[r.otro.1 == 1] <- 4

clases <- r.urb.1+r.sub.11+r.rur.11+r.otro.1

r.clases <- rasterFromXYZ(data.frame(r.urb.1[, c("x", "y")],
                                     clases[, c("Poblacion.MID.2000", "Poblacion.MID.2020")]))

plot(r.clases)

###
prop.urb <- cellStats(r.urb, mean)
prop.sub <- cellStats(r.sub, mean)
prop.rur <- cellStats(r.rur, mean)

