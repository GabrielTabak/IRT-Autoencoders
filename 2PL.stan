data {
  int<lower=1> N;  // Number of persons
  int<lower=1> I;  // Number of items
  int<lower=0,upper=1> y[N, I];  // Response matrix (N persons by I items)
}

parameters {
  vector[N] theta;       // Ability parameters for persons
  vector[I] alpha;       // Discrimination parameters for items
  vector[I] beta;        // Difficulty parameters for items
}

model {
  // Priors
  theta ~ normal(0, 1);
  alpha ~ lognormal(0, 1);
  beta ~ normal(0, 1);

  // Likelihood
  for (n in 1:N) {
    for (i in 1:I) {
      y[n, i] ~ bernoulli_logit(alpha[i] * (theta[n] - beta[i]));
    }
  }
}

