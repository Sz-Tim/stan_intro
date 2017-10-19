#'---
#' title: Intro to stan
#' author: Tim Szewczyk
#' output: github_document
#'---

## Introduction to stan for Allen lab meeting on 2017 Oct 19. This script
## provides a basic comparison of a simple linear regression using standard
## frequentist methods in R vs. a Bayesian implementation in stan. While this
## example is much easier to do using base R, the advantages of stan (and
## Bayesian methods) become clear with more complex model structures.


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
## linear regression with lm()
##---

out_lm <- lm(y ~ x, data=sim.df)
summary(out_lm)
par(mfrow=c(2,2))
plot(out_lm)

new.x <- data.frame(x=seq(min(sim.df$x), max(sim.df$x), length.out=100))
pred.lm <- predict(out_lm, new.x, interval="confidence")
par(mfrow=c(1,1))
plot(y ~ x, data=sim.df)
abline(out_lm, col="blue")
segments(sim.df$x, sim.df$y, sim.df$x, out_lm$fitted.values, 
         col=rgb(0,0,1,0.5))
hist(out_lm$residuals, breaks=10)
plot(y ~ x, data=sim.df, cex.axis=2, cex.lab=2)
abline(out_lm, col="blue")
lines(new.x$x, pred.lm[,2], col="blue", lty=2)
lines(new.x$x, pred.lm[,3], col="blue", lty=2)


##---
## linear regression with stan
##---

stan_d <- list(n=n, x=sim.df$x, y=sim.df$y)
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



##---
## comparisons
##---

nGG <- attr(stan.gg, "nChains")*attr(stan.gg, "nIterations")
stan.gg$iter <- rep(1:nGG, times=n_distinct(stan.gg$Parameter))
plot(y ~ x, data=sim.df)
for(i in 1:nGG) {
  abline(a=stan.gg$value[stan.gg$Parameter=="a" & stan.gg$iter==i],
         b=stan.gg$value[stan.gg$Parameter=="b" & stan.gg$iter==i],
         col=rgb(84/240,48/240,5/240,1/sqrt(nGG)))
}
abline(out_lm, col=rgb(1/240,102/240,94/240), lwd=2)
lines(new.x$x, pred.lm[,2], lty=2, col=rgb(1/240,102/240,94/240), lwd=2)
lines(new.x$x, pred.lm[,3], lty=2, col=rgb(1/240,102/240,94/240), lwd=2)

comp.df <- stan.gg %>% group_by(Parameter) %>%
  summarise(mn=mean(value), 
            loCI=quantile(value, 0.025),
            hiCI=quantile(value, 0.975)) %>%
  mutate(model="stan")
comp.df <- comp.df %>% 
  add_row(Parameter=c("a", "b", "sigma"),
          mn=c(coef(out_lm), summary(out_lm)$sigma),
          loCI=c(confint(out_lm)[,1], NA),
          hiCI=c(confint(out_lm)[,2], NA),
          model="lm") %>%
  add_row(Parameter=c("a", "b", "sigma"),
          mn=c(a, b, sigma),
          loCI=NA, hiCI=NA, model="true")


ggplot(comp.df, aes(x=model, y=mn, ymin=loCI, ymax=hiCI)) + 
  geom_linerange() + geom_point() + facet_wrap(~Parameter)


