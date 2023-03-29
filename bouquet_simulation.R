#### BOUQUET SIMULADO

# Sacar los circulos.

library(ggplot2)
library(tcd)
library(trend)

## MODEL 1
# Two classes with initial opposite angles.

# Parameters class 1
tita_0 = pi
alpha_epsilon = 0.05
size = 0.1
po = c(0,0)
m = 30

n1 = 100
tray1 = vector(mode="list", length=n1)
for (j in 1:n1) {
  P = matrix(0, nrow=m, ncol=2)
  P[1,] = po
  tita = tita_0
  for (i in 2:m) {
    tita = c(tita, tita[i-1] + alpha_epsilon*rbinom(1,1,0.5))
    P[i,] = P[i-1,] + size*c(cos(tita[i]), sin(tita[i]))
  }
  tray1[[j]] = P
}
clase = rep(1,n1)

# Parameters class 2
tita_0 = 0
alpha_epsilon = 0.05
size = 0.1
po = c(0,0)
m = 30

n2 = 100
tray2 = vector(mode="list", length=n2)
for (j in 1:n2) {
  P = matrix(0, nrow=m, ncol=2)
  P[1,] = po
  tita = tita_0
  for (i in 2:m) {
    tita = c(tita, tita[i-1] + alpha_epsilon*rbinom(1,1,0.5))
    P[i,] = P[i-1,] + size*c(cos(tita[i]), sin(tita[i]))
  }
  tray2[[j]] = P
}
clase = c(clase, rep(2,n2))

tray = c(tray1, tray2)
n = n1 + n2

# Depth
depth = icd(tray, probs = seq(0.1,1,0.05), type="cartesian")
q_max = which(depth$depth>quantile(depth$depth, 0.9))
q_min = which(depth$depth<quantile(depth$depth, 0.10))

par(mfrow=c(1,2))
plot(tray[[1]], type="n", xlim=c(-3,3), ylim=c(-2,2), xlab="", ylab="", main="Ejemplo 1")
for (i in 1:n) lines(tray[[i]], col=clase[i])

plot(tray[[1]], type="n", xlim=c(-3,3), ylim=c(-2,2), xlab="", ylab="", main="Ejemplo 1")
r=depth$grid[depth$grid>quantile(depth$grid,0.1)]
t = seq(0,2*pi,0.01)
for (i in 1:length(r)) lines(r[i]*cos(t), r[i]*sin(t), col="grey", lty=2)
for (i in 1:n) lines(tray[[i]], col="lightgrey")
for (i in q_max) lines(tray[[i]], col="blue")
for (i in q_min) lines(tray[[i]], col="red")

## Model 2
# Three classes with initial angles of 60 degrees.

# Parameters class 1
tita_0 = 2*pi/3
alpha_epsilon = 0.05
size = 0.1
po = c(0,0)
m = 30

n1 = 100
tray1 = vector(mode="list", length=n1)
for (j in 1:n1) {
  P = matrix(0, nrow=m, ncol=2)
  P[1,] = po
  tita = tita_0
  for (i in 2:m) {
    tita = c(tita, tita[i-1] + alpha_epsilon*rbinom(1,1,0.5))
    P[i,] = P[i-1,] + size*c(cos(tita[i]), sin(tita[i]))
  }
  tray1[[j]] = P
}

clase = rep(1,n1)

# Parameters class 2
tita_0 = 4*pi/3
alpha_epsilon = 0.05
size = 0.1
po = c(0,0)
m = 30

n2 = 100
tray2 = vector(mode="list", length=n2)
for (j in 1:n2) {
  P = matrix(0, nrow=m, ncol=2)
  P[1,] = po
  tita = tita_0
  for (i in 2:m) {
    tita = c(tita, tita[i-1] + alpha_epsilon*rbinom(1,1,0.5))
    P[i,] = P[i-1,] + size*c(cos(tita[i]), sin(tita[i]))
  }
  tray2[[j]] = P
}
clase = c(clase, rep(2,n2))

# Parameters class 3
tita_0 = 6*pi/3
alpha_epsilon = 0.05
size = 0.1
po = c(0,0)
m = 30

n3 = 100
tray3 = vector(mode="list", length=n2)
for (j in 1:n3) {
  P = matrix(0, nrow=m, ncol=2)
  P[1,] = po
  tita = tita_0
  for (i in 2:m) {
    tita = c(tita, tita[i-1] + alpha_epsilon*rbinom(1,1,0.5))
    P[i,] = P[i-1,] + size*c(cos(tita[i]), sin(tita[i]))
  }
  tray3[[j]] = P
}
clase = c(clase, rep(3,n3))

n = n1 + n2 + n3
tray = c(tray1, tray2, tray3)

plot(tray[[1]], type="n", xlim=c(-3,3), ylim=c(-3,3), xlab="", ylab="", main="Ejemplo 2")
for (i in 1:n) lines(tray[[i]], col=clase[i])

# Depth
depth = icd(tray,beta=0.25, probs = seq(0.1,1,0.05), type="cartesian")
q_max = which(depth$depth>quantile(depth$depth, 0.95))
q_min = which(depth$depth<quantile(depth$depth, 0.1))

par(mfrow=c(1,2))
plot(tray[[1]], type="n", xlim=c(-3,3), ylim=c(-3,3), xlab="", ylab="", main="Ejemplo 1")
for (i in 1:n) lines(tray[[i]], col=clase[i])

plot(tray[[1]], type="n", xlim=c(-3,3), ylim=c(-3,3), xlab="", ylab="", main="Ejemplo 1")
r=depth$grid[depth$grid>quantile(depth$grid,0.1)]
t = seq(0,2*pi,0.01)
for (i in 1:length(r)) lines(r[i]*cos(t), r[i]*sin(t), col="grey", lty=2)
for (i in 1:n) lines(tray[[i]], col="lightgrey")
for (i in q_max) lines(tray[[i]], col="blue")
for (i in q_min) lines(tray[[i]], col="red")


## MODEL 3

# Parameters class 1
tita_0 = pi
alpha_epsilon = 0.05
size = 0.1
po = c(0,0)
m = 30

n1 = 100
tray1 = vector(mode="list", length=n1)
for (j in 1:n1) {
  P = matrix(0, nrow=m, ncol=2)
  P[1,] = po
  tita = tita_0
  for (i in 2:m) {
    tita = c(tita, tita[i-1] + alpha_epsilon*rbinom(1,1,0.5))
    P[i,] = P[i-1,] + size*c(cos(tita[i]), sin(tita[i]))
  }
  tray1[[j]] = P
}

clase = rep(1,n1)

# Parameters class 2
tita_0 = pi/2
alpha_epsilon = -0.1
size = 0.15
po = c(0,0)
m = 30

n2 = 50
tray2 = vector(mode="list", length=n2)
for (j in 1:n2) {
  P = matrix(0, nrow=m, ncol=2)
  P[1,] = po
  tita = tita_0
  for (i in 2:m) {
    tita = c(tita, tita[i-1] + alpha_epsilon*rbinom(1,1,0.5))
    P[i,] = P[i-1,] + size*c(cos(tita[i]), sin(tita[i]))
  }
  tray2[[j]] = P
}
clase = c(clase, rep(2,n2))

n = n1 + n2
tray = c(tray1, tray2)

# Depth
depth = icd(tray, probs = seq(0.1,0.95,0.01), type="cartesian")
q_max = which(depth$depth>quantile(depth$depth, 0.9))
q_min = which(depth$depth<quantile(depth$depth, 0.1))

plot(tray[[1]], type="n", xlim=c(-3,4), ylim=c(-4,4), xlab="", ylab="", main="Ejemplo 1")
r=depth$grid[depth$grid>quantile(depth$grid,0.1)]
t = seq(0,2*pi,0.01)
for (i in 1:length(r)) lines(r[i]*cos(t), r[i]*sin(t), col="grey", lty=2)
for (i in 1:n) lines(tray[[i]], col="lightgrey")
for (i in q_max) lines(tray[[i]], col="blue")
for (i in q_min) lines(tray[[i]], col="red")

# Plot con ggplot
data = data.frame(x=tray1[[1]][,1], y=tray1[[1]][,2])
g3 = ggplot(data=data, aes(x=x, y=y)) + geom_line(color="grey") + xlim(-3,4) +
  ylim(-2,4) + theme_light()
for (i in 1:n1) {
  data = data.frame(x=tray1[[i]][,1], y=tray1[[i]][,2])
  g3 = g3 + geom_path(data=data, aes(x=x, y=y), color="lightgrey")
}
for (i in 1:n2) {
  data = data.frame(x=tray2[[i]][,1], y=tray2[[i]][,2])
  g3 = g3 + geom_path(data=data, aes(x=x, y=y), color="lightgrey")
}
for (i in q_max) {
  data = data.frame(x=tray[[i]][,1], y=tray[[i]][,2])
  g3 = g3 + geom_path(data=data, aes(x=x, y=y), color="blue")
}
for (i in q_min) {
  data = data.frame(x=tray[[i]][,1], y=tray[[i]][,2])
  g3 = g3 + geom_path(data=data, aes(x=x, y=y), color="red")
}
g3


## MODEL 4
# A 10% of outliers.

# Parameters class 1
tita_0 = pi
alpha_epsilon = 0.05
size = 0.1
po = c(0,0)
m = 30

n1 = 90
tray1 = vector(mode="list", length=n1)
for (j in 1:n1) {
  P = matrix(0, nrow=m, ncol=2)
  P[1,] = po
  tita = tita_0
  for (i in 2:m) {
    tita = c(tita, tita[i-1] + alpha_epsilon*rbinom(1,1,0.5))
    P[i,] = P[i-1,] + size*c(cos(tita[i]), sin(tita[i]))
  }
  tray1[[j]] = P
}

clase = rep(1,n1)

# Parameters class 2
tita_0 = pi
alpha_epsilon = 0.05
size = 0.5
po = c(0,0)
m = 30

n2 = 10
tray2 = vector(mode="list", length=n2)
for (j in 1:n2) {
  P = matrix(0, nrow=m, ncol=2)
  P[1,] = po
  tita = runif(1,0,2*pi) #tita_0
  for (i in 2:m) {
    tita = c(tita, tita[i-1] + runif(1,0,0.5)*rbinom(1,1,0.8)) #alpha_epsilon*rbinom(1,1,0.5))
    P[i,] = P[i-1,] + size*c(cos(tita[i]), sin(tita[i]))
  }
  tray2[[j]] = P
}
clase = c(clase, rep(2,n2))

n = n1 + n2
tray = c(tray1, tray2)

# Depth
depth = icd(tray, probs = seq(0.05,1,0.01), type="cartesian")
q_max = which(depth$depth>quantile(depth$depth, 0.9))
q_min = which(depth$depth<quantile(depth$depth, 0.1))

plot(tray[[1]], type="n", xlim=c(-7.5,5.5), ylim=c(-7,6), xlab="", ylab="", main="Ejemplo 4")
for (i in 1:n) lines(tray[[i]], col="lightgrey")
for (i in q_max) lines(tray[[i]], col="blue")
for (i in q_min) lines(tray[[i]], col="red")


