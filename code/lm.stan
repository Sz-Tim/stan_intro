data {
	int n;  //number of observations
	vector[n] x;  //covariate
	vector[n] y;  //observations
}

parameters {
	real a;  //intercept
	real b;  //slope
	real<lower=0> sigma;  //residual error standard deviation
}

model {
  //priors
  a ~ normal(0, 10);
  b ~ normal(0, 10);
  sigma ~ cauchy(0, 2.5);
  //likelihood
	y ~ normal(a + b*x, sigma);
}
