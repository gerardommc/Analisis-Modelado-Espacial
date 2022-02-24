x <- round(runif(10), 1)
y <- round(runif(10), 1)
cober <- rpois(10, 20)
amb <- cober + rnorm(10)
amb.1 <- rnorm(10)
amb.2 <- cober*(-1) + rnorm(10)


x
y
cober


df.1 <- data.frame(x = x, y = y, 
                   cober = cober, amb = amb,
                   amb.1 = amb.1, amb.2 = amb.2)

plot(df.1$amb, df.1$cober)
plot(df.1$amb.1, df.1$cober)
plot(df.1$amb.2, df.1$cober)



plot(df.1$x, df.1$y, cex = df.1$cober)

############## Correlación

cober.mean <- df.1$cober - mean(df.1$cober)
amb.mean <- df.1$amb - mean(df.1$amb)

prod.sum <- sum(cober.mean * amb.mean)
sqrt.sum <- sqrt(sum(cober.mean^2)*sum(amb.mean^2))

prod.sum/sqrt.sum

##############

cor.test(df.1$cober, df.1$amb)
cor.test(df.1$cober, df.1$amb.1)
cor.test(df.1$cober, df.1$amb.2)

####################

library(raster)

r <- raster("Unidad-II/Capas-ejemplo/bio1.tif")
crop(r, extent(c(-105, -100, 25, 30)))

plot(r)
