#'---
#' title: Intro to stan
#' author: Tim Szewczyk
#' output: github_document
#'---

## This simulates data for and runs a simple hierarchical model


##---
## set up environment
##---

if(!require(pacman)) install.packages("pacman", dependencies=TRUE)
p_load(tidyverse, rstan, ggmcmc, coda); theme_set(theme_bw())
rstan_options(auto_write=TRUE); options(mc.cores=parallel::detectCores())



##---
## data simulation
##---

n <- 1000
s <- 5
site <- sample.int(s, n, replace=TRUE)

alpha <- 5
beta <- -3
sigma_a <- 1
sigma_b <- 2
sigma_e <- 3
a_s <- rnorm(s, alpha, sigma_a)
b_s <- rnorm(s, beta, sigma_b)

sim.df <- data.frame(x=rnorm(n, 0, 1))
sim.df$y <- rnorm(n, a_s[site] + b_s[site]*sim.df$x, sigma_e)
sim.df$site <- factor(site)
ggplot(sim.df, aes(x=x, y=y, colour=site)) + geom_point() + 
  stat_smooth(aes(group=site, colour=site), method="lm", se=FALSE) +
  stat_smooth(method="lm", colour="black", se=FALSE, size=1.5) + 
  scale_colour_brewer(type="qual", palette=2)


##---
## linear regression with stan
##---

# save the data as a named list, where each item corresponds to a variable in
# the data{} block of lm.stan
stan_d <- list(n=n, s=s, site=site, x=sim.df$x, y=sim.df$y)

# stan() runs the model -- it compiles to C++, saves the model as a .rds file,
# and runs the actual MCMC chains. See ?stan for a list of options you can 
# adjust. If you just want to see if your model will actually compile and run,
# you can set iter=20, chains=1 or something similar.
out_stan <- stan(file="code/lm_hier.stan", data=stan_d, iter=5000, thin=5)

out_mcmc <- As.mcmc.list(out_stan)
gelman.plot(out_mcmc)

out_stan
plot(out_stan, pars=c("alpha", "beta", "sigma_a", "sigma_b"))
traceplot(out_stan, pars=c("alpha", "beta", "sigma_a", "sigma_b"))
pairs(out_stan, pars=c("alpha", "beta", "sigma_a", "sigma_b"))
stan_rhat(out_stan)

stan.gg <- ggs(out_stan) 
true.vals <- data.frame(val=c(alpha, beta, a_s, b_s, sigma_a, sigma_b, sigma_e),
                        Parameter=c("alpha", "beta", 
                              paste0("a_s", "[", 1:s, "]"),
                              paste0("b_s", "[", 1:s, "]"),
                              "sigma_a", "sigma_b", "sigma_e"))

ggs_density(stan.gg) + facet_wrap(~Parameter, scales="free") + 
  geom_vline(data=true.vals, aes(xintercept=val))
ggs_caterpillar(stan.gg) + 
  geom_point(data=true.vals, aes(x=val, y=Parameter), colour="red")
ggs_crosscorrelation(stan.gg)
ggs_autocorrelation(stan.gg)




