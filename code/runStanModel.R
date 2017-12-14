#'---
#' title: Intro to stan
#' author: Tim Szewczyk
#' output: github_document
#'---

## This is a slimmed down version of compare.R that only runs the stan model


##---
## set up environment
##---

if(!require(pacman)) install.packages("pacman", dependencies=TRUE)
p_load(tidyverse, rstan, ggmcmc); theme_set(theme_bw())
rstan_options(auto_write=TRUE); options(mc.cores=parallel::detectCores())



##---
## data simulation
##---

n <- 100
a <- 5
b <- 3
sigma <- 5
sim.df <- data.frame(x=rnorm(n, 0, 1))
sim.df$y <- rnorm(n, a + b*sim.df$x, sigma)
plot(y ~ x, data=sim.df)


##---
## linear regression with stan
##---

# save the data as a named list, where each item corresponds to a variable in
# the data{} block of lm.stan
stan_d <- list(n=n, x=sim.df$x, y=sim.df$y)

# stan() runs the model -- it compiles to C++, saves the model as a .rds file,
# and runs the actual MCMC chains. See ?stan for a list of options you can 
# adjust. If you just want to see if your model will actually compile and run,
# you can set iter=20, chains=1 or something similar.
out_stan <- stan(file="code/lm.stan", data=stan_d, iter=5000, thin=5)


out_stan
plot(out_stan)
traceplot(out_stan)
pairs(out_stan)
stan_rhat(out_stan)

stan.gg <- ggs(out_stan)
ggs_density(stan.gg)
ggs_crosscorrelation(stan.gg)
ggs_autocorrelation(stan.gg)




