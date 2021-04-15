data {
  int<lower=0> P; // number of distinct populations
  int<lower=0> R; // number of distinct regions
  int<lower=0> M; // size of augumented data set
  int<lower=0> T; // number of sampling occasions
  int<lower=0, upper=1> y[P, R, M, T]; // capture-history matrix
}

transformed data {
  // total number of captures for each potential member
  int<lower=0> s[P, R, M];

  // size of observed data set
  int<lower=0> C[P, R];

  for(i in 1:P) { // for each faction (population)
    for(j in 1:R) { // for each region
      C[i, j] = 0;

      for (k in 1:M) {
        // store the number of captures across all sampling occasions for this potential member.
        s[i, j, k] = sum(y[i, j, k]);

        // if this potential member was observed, increment the count.
        if (s[i, j, k] > 0) {
          C[i, j] = C[i, j] + 1;
        }
      }
    }
  }
}

parameters {
  // inclusion probability - the probability that any potential member of N (that is, any member of
  // M) is in the true population N.
  real<lower=0, upper=1> omega[P, R];

  // detection probability - the probability that any member of N is captured
  vector<lower=0, upper=1>[T] p[P, R];

  real<lower=0, upper=1> p_bar_population[P];
  real<lower=0> sigma_population[P];

  real<lower=0, upper=1> p_bar_region[R];
  real<lower=0> sigma_region[R];
}

model {
  // priors are implicitly defined as `uniform(0, 1)'

  for (i in 1:P) { // for each faction (population)
    for (j in 1:R) { // for each region
      p[i, j] ~ normal(p_bar_population[i], sigma_population[i]);
      p[i, j] ~ normal(p_bar_region[j], sigma_region[j]);

      for (k in 1:M) {
        if (s[i, j, k] > 0) {
          // this potential member was observed, which means it's a member of N.

          // probability that this one is a real member of N.
          target += bernoulli_lpmf(1 | omega[i, j]) +
          // probability that this one was sighted during the sampling occasions.
            bernoulli_lpmf(y[i, j, k] | p[i, j]);
        } else { // s[i, j, k] == 0
          // this potential member was not observed, which means we don't know if it's a member of
          // N or not.

          // probability that it is a real member but wasn't sighted in any of the sampling
          // occasions.
          target += log_sum_exp(bernoulli_lpmf(1 | omega[i, j])
                                + bernoulli_lpmf(y[i, j, k] | p[i, j]),
          // probability that it's not a real member.
                                bernoulli_lpmf(0 | omega[i, j]));
        }
      }
    }
  }
}

generated quantities {
  // because stan does not support vectors where each element has a different upper/lower constaint,
  // we need to calculate the population size as scaled to (0, 1) & then rescale it.
  //
  // => https://mc-stan.org/docs/2_18/stan-users-guide/vectors-with-varying-bounds.html
  real<lower=0, upper=1> N_raw[P, R];
  real<lower=0, upper=M> N[P, R];

  for (i in 1:P) { // for each faction (population)
    for (j in 1:R) { // for each region
      // calculate the population size.

      // probability of a member, real or not, not being sighted across all sampling occasions.
      real pr = prod(1 - p[i, j]);

      // `omega_nd` is the probability that a potential member is real given it was never detected.

      // probability of a member being real but not being sighted across all sampling occasions.
      real omega_nd = (omega[i, j] * pr) /
        // probability of a member, real or not, not being sighted across all sampling occasions.
        (omega[i, j] * pr + (1 - omega[i, j]));

      // calculate the estimated population size minus the known minimum, scaled to (0, 1).
      N_raw[i, j] = binomial_rng(M - C[i, j], omega_nd) * 1.0 / (M - C[i, j]);

      // rescale the estimated population size minus the known minimum, then add the known minimum
      // to produce the final estimate.
      N[i, j] = N_raw[i, j] * (M - C[i, j]) + C[i, j];
    }
  }
}
