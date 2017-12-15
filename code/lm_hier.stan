data {
  int n;  //number of observations
  int s;  //number of sites
  int site[n];  //site ID -- this syntax makes it a 1D array of integers
  vector[n] x;  //covariate
  vector[n] y;  //observations
}

parameters {
  vector[s] a_s;  //site-level intercept
  vector[s] b_s;  //site-level slope
  real<lower=0> sigma_a;  //standard deviation of site-level intercepts
  real<lower=0> sigma_b;  //standard deviation of site-level slopes
  real alpha;  //species-level intercept
  real beta;  //species-level slope
  real<lower=0> sigma_e;  //residual error
}

model {
  vector[n] mu;  // temporary container for means for each observation
  
  //priors
  a_s ~ normal(alpha, sigma_a);
  b_s ~ normal(beta, sigma_b);
  sigma_a ~ cauchy(0, 2.5);
  sigma_b ~ cauchy(0, 2.5);
  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
  sigma_e ~ cauchy(0, 2.5);
  
  //calculate each observation mean
  for(i in 1:n) {
    mu[i] = a_s[site[i]] + b_s[site[i]]*x[i];
  }
  
  //likelihood
  y ~ normal(mu, sigma_e);
}
