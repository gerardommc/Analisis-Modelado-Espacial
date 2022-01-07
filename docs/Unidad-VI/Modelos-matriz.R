p1 <- 0.2; R <- 1.5; p2 <- 0.8
L <- matrix(c(p1, R, p2, 0), nrow = 2, ncol = 2, byrow = T)


N1 <- 0
N2 <- 5
N <- matrix(NA, nrow = 2, ncol = 25, byrow = F)

N[,1] <- c(N1, N2)

for(t in 2:ncol(N)){
    N[,t] <- L %*% N[, t-1]
}

plot(1:ncol(N), N[1, ], type = "s", col = "red")
plot(1:ncol(N), N[2, ], type = "s", col = "red")

plot(1:ncol(N), N[1, ]/colSums(N), type = "l", col = "red")
plot(1:ncol(N), N[2, ]/colSums(N), type = "l", col = "red")

plot(1:ncol(N), colSums(N), type = "s", col = "red")

N.tot <- colSums(N)

Growth <- N.tot[2:ncol(N)]/N.tot[1:(ncol(N)-1)]

plot(1:length(Growth), Growth, type = "l")

prop <- eigen(L); prop

vec.prop <- prop$vectors[, 1]

vec.prop/sum(vec.prop)
