library(raster)

df <- read.csv("Unidad-V/Datos-ejercicio-II.csv")

r <- stack(list.files("Unidad-V/Capas-ejemplo/", "tif", full.names = T))

plot(r[[1]])

points(df[, c("Longitud", "Latitud")])


ext <- data.frame(extract(r, df[, c("Longitud", "Latitud")]))


df <- data.frame(df, ext)


with(df, plot(Mediciones, Var.1))
with(df, plot(Mediciones, Var.2))

pairs(r)

m1 <- lm(Mediciones~Var.1 + Var.2, df)

df$resid.1 <- residuals(m1)

library(spdep)

vecindad <- dnearneigh(x = as.matrix(df[, c("Longitud", "Latitud")]), d1 = 0, d2 = 75, longlat = T)
vec.listw <- nb2listw(vecindad)
S0 <- sum(nb2mat(vecindad))

I.1 <- moran.test(x = df$resid.1, listw = vec.listw)
I.1

par(mfrow = c(2,2))
plot(m1)

## Gls

library(gstat); library(nlme)
coordinates(df) <- ~ Latitud + Longitud
variog <- variogram(residuals(m1) ~
                   1, data = df, cutoff = 6)
plot(variog)

m1.exp <- gls(Mediciones ~ Var.1 + Var.2, data = df,
              correlation = corExp(form = ~Latitud + Longitud, nugget = TRUE),
              method = "REML")

m1.gaus <- gls(Mediciones ~ Var.1 + Var.2, data = df,
              correlation = corGaus(form = ~Latitud + Longitud, nugget = TRUE),
              method = "REML")

m1.Lin <- gls(Mediciones ~ Var.1 + Var.2, data = df,
              correlation = corLin(form = ~Latitud + Longitud, nugget = TRUE),
              method = "REML")

m1.ratio <- gls(Mediciones ~ Var.1 + Var.2, data = df,
              correlation = corRatio(form = ~Latitud + Longitud, nugget = TRUE),
              method = "REML")

m1.sph <- gls(Mediciones ~ Var.1 + Var.2, data = df,
              correlation = corSpher(form = ~Latitud + Longitud, nugget = TRUE),
              method = "REML")


AIC(m1)
AIC(m1.exp)
AIC(m1.gaus)
AIC(m1.Lin)
AIC(m1.ratio)
AIC(m1.sph)


