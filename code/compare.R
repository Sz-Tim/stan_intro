# Intro to stan
# Tim Szewczyk


## Introduction to stan for Allen lab meeting on 2017 Oct 19. This script
## provides a basic comparison of a simple linear regression using standard
## frequentist methods in R vs. a Bayesian implementation in stan. While this
## example is much easier to do using base R, the advantages of stan (and
## Bayesian methods) become clear with more complex model structures.


##---
## set up environment
##---

library(rstan)
rstan_options(auto_write=TRUE); options(mc.cores=parallel::detectCores())



##---
## data simulation
##---

n <- 250
a <- 10
b <- 3
y_sd <- 2
sim.df <- data.frame(x=rnorm(n, 20, 2))
sim.df$y <- rnorm(n, a + b*sim.df$x, y_sd)
plot(y ~ x, data=sim.df)



##---
## linear regression with lm()
##---

out_lm <- lm(y ~ x, data=sim.df)
summary(out_lm)
plot(out_lm)



##---
## linear regression with stan
##---

stan_d <- list(n=n, x=sim.df$x, y=sim.df$y)
out_stan <- stan(file="code/lm.stan", data=stan_d)
summary(out_stan)
plot(out_stan)
traceplot(out_stan)
pairs(out_stan)
stan_diag(out_stan)


