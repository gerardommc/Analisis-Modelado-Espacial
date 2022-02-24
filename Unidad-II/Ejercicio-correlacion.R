r <- stack(list.files("Unidad-II/Capas-ejemplo", "tif", full.names = T))
r1 <- crop(r, extent(c(-105, -100, 25, 30)))

x <- runif(100, -104.5, -100)
y <- runif(100, 25.5, 29.7)

df <- data.frame(Longitud = x, Latitud = y)

dat <- extract(r1[[1]], df)

beta <- runif(1, min = -0.1, max = -0.01)
alpha <- rnorm(100, 20, 0.15)

var <- alpha + dat * beta

plot(dat, var)

df$Mediciones <- var

write.csv(df, "Unidad-II/Datos-ejercicio.csv")

writeRaster(r1[[1]], "Unidad-II/Capas-ejemplo/Var-1", "GTiff")
writeRaster(r1[[2]], "Unidad-II/Capas-ejemplo/Var-2", "GTiff")
writeRaster(r1[[3]], "Unidad-II/Capas-ejemplo/Var-3", "GTiff")

# Ejercicio 2

r <- stack(list.files("Unidad-II/Capas-ejemplo", "Var", full.names = T))

x <- runif(100, -104.5, -100)
y <- runif(100, 25.5, 29.7)

df <- data.frame(Longitud = x, Latitud = y)

dat <- extract(r[[2]], df)
dat.1 <- extract(r[[3]], df)

beta <- runif(1, min = -0.1, max = -0.01)
alpha <- rnorm(100, 20, 0.15)

var <- alpha + dat * beta - dat.1^2*0.00025


plot(dat, var)


df$Mediciones <- var

mod.1 <- lm(var ~ dat)
plot(r[[2]])
points(x, y, cex = residuals(mod.1))

write.csv(df, "Unidad-II/Datos-ejercicio-tarea.csv")
