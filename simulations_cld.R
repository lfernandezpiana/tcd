#### SIMULATION M1 TO M4
#### Circular local depth

library(ggplot2)
library(circular)
library(tidyverse)
library(latex2exp)
library(tcd)


### MODELOS

## M1

# Parametros
n = 2000
mu1 = pi/2
kappa1 = 10
data = rvonmises(n, mu1, kappa1)
data = as.numeric(data)
t = seq(0,2*pi,0.01)

# Profundidad
beta = seq(0.2,0.8,0.2)
depth = matrix(NA, ncol=4, nrow=length(t))
for (j in 1:4) depth[,j] =  cld2(t, data, beta[j])

# Data frame para ggplot
df = data.frame(t=t, depth)
colnames(df) = c("t", "beta0.2", "beta0.4", "beta0.6", "beta0.8")
df_long = reshape(df, idvar = "t", timevar="beta", varying = c("beta0.2", "beta0.4", "beta0.6", "beta0.8"),
                  direction = "long", new.row.names = NULL)
colnames(df_long) = c("t", "beta", "depth"); row.names(df_long) = NULL
for (j in 1:4) {
  df_long$beta[df_long$beta==2*j] = beta[j]
}
df_long$beta = as.factor(df_long$beta)
df_long = data.frame(df_long, M=rep("M1", nrow(df_long)))
df_plot = df_long


## M2

# Parametros
n = 2000
mu1 = pi/2; mu2 = (3/2)*pi
kappa1 = 10
prop = 0.5
data = rmixedvonmises(n, mu1, mu2, kappa1, kappa1, prop)
data = as.numeric(data)
t = seq(0,2*pi,0.01)

# Profundidad
beta = seq(0.2,0.8,0.2)
depth = matrix(NA, ncol=4, nrow=length(t))
for (j in 1:4) depth[,j] =  cld2(t, data, beta[j])

# Data frame para ggplot
df = data.frame(t=t, depth)
colnames(df) = c("t", "beta0.2", "beta0.4", "beta0.6", "beta0.8")
df_long = reshape(df, idvar = "t", timevar="beta", varying = c("beta0.2", "beta0.4", "beta0.6", "beta0.8"),
                  direction = "long", new.row.names = NULL)
colnames(df_long) = c("t", "beta", "depth"); row.names(df_long) = NULL
for (j in 1:4) {
  df_long$beta[df_long$beta==2*j] = beta[j]
}
df_long$beta = as.factor(df_long$beta)
df_long = data.frame(df_long, M=rep("M2", nrow(df_long)))
df_plot = rbind(df_plot, df_long)


## M3

# Parametros
n = 2000
mu1 = pi/2; mu2 = (3/2)*pi
kappa1 = 10; kappa2 = 5
prop = 0.7
data = rmixedvonmises(n, mu1, mu2, kappa1, kappa1, prop)
data = as.numeric(data)
t = seq(0,2*pi,0.01)

# Profundidad
beta = seq(0.2,0.8,0.2)
depth = matrix(NA, ncol=4, nrow=length(t))
for (j in 1:4) depth[,j] =  cld2(t, data, beta[j])

# Data frame para ggplot
df = data.frame(t=t, depth)
colnames(df) = c("t", "beta0.2", "beta0.4", "beta0.6", "beta0.8")
df_long = reshape(df, idvar = "t", timevar="beta", varying = c("beta0.2", "beta0.4", "beta0.6", "beta0.8"),
                  direction = "long", new.row.names = NULL)
colnames(df_long) = c("t", "beta", "depth"); row.names(df_long) = NULL
for (j in 1:4) {
  df_long$beta[df_long$beta==2*j] = beta[j]
}
df_long$beta = as.factor(df_long$beta)
df_long = data.frame(df_long, M=rep("M3", nrow(df_long)))
df_plot = rbind(df_plot, df_long)


## M4

# Parametros
n = 2000
mu1 = pi/2; mu2 = (5/4)*pi
kappa1 = 0.5; kappa2 = 2
prop = 0.3
data = rmixedvonmises(n, mu1, mu2, kappa1, kappa2, prop)
data = as.numeric(data)
t = seq(0,2*pi,0.01)

# Profundidad
beta = seq(0.2,0.8,0.2)
depth = matrix(NA, ncol=4, nrow=length(t))
for (j in 1:4) depth[,j] =  cld2(t, data, beta[j])

# Data frame para ggplot
df = data.frame(t=t, depth)
colnames(df) = c("t", "beta0.2", "beta0.4", "beta0.6", "beta0.8")
df_long = reshape(df, idvar = "t", timevar="beta", varying = c("beta0.2", "beta0.4", "beta0.6", "beta0.8"),
                  direction = "long", new.row.names = NULL)
colnames(df_long) = c("t", "beta", "depth"); row.names(df_long) = NULL
for (j in 1:4) {
  df_long$beta[df_long$beta==2*j] = beta[j]
}
df_long$beta = as.factor(df_long$beta)
df_long = data.frame(df_long, M=rep("M4", nrow(df_long)))
df_plot = rbind(df_plot, df_long)


## Plot
ggplot(df_plot, aes(x=t, y=depth, colour=beta, group=beta, linetype=beta)) + geom_path() +
  xlab(TeX("$\\theta$")) + theme_bw() + facet_grid(~M) +
  theme(strip.background =element_rect(fill="white"))


## Grafico Densidades

t = seq(0,2*pi,0.01)

# M1
mu1 = pi/2
kappa1 = 10
d_m1 = dvonmises(t, mu1, kappa1)
d_m1 = as.numeric(d_m1)

# M2
mu1 = pi/2; mu2 = (3/2)*pi
kappa1 = 10; kappa2 = 5
prop = 0.5
d_m2 = dmixedvonmises(t, mu1, mu2, kappa1, kappa1, prop)
d_m2 = as.numeric(d_m2)


# M3
mu1 = pi/2; mu2 = (3/2)*pi
kappa1 = 10; kappa2 = 5
prop = 0.7
d_m3 = dmixedvonmises(t, mu1, mu2, kappa1, kappa1, prop)
d_m3 = as.numeric(d_m3)


# M4
mu1 = pi/2; mu2 = (5/4)*pi
kappa1 = 0.5; kappa2 = 2
prop = 0.3
d_m4 = dmixedvonmises(t, mu1, mu2, kappa1, kappa2, prop)
d_m4 = as.numeric(d_m4)


df_d = data.frame(t=t, d=d_m1, model="M1")
df_d = rbind(df_d, data.frame(t=t, d=d_m2, model="M2"))
df_d = rbind(df_d, data.frame(t=t, d=d_m3, model="M3"))
df_d = rbind(df_d, data.frame(t=t, d=d_m4, model="M4"))

ggplot(df_d, aes(x=t, y=d)) + geom_path() +
  xlab(TeX("$\\theta$")) + ylab("Density") + theme_bw() + facet_grid(~model) +
  theme(strip.background =element_rect(fill="white"))


