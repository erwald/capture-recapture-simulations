data {
  int<lower=0> P; // Number of distinct populations
  int<lower=0> R; // Number of distinct regions
  int<lower=0> M; // Size of augumented data set
  int<lower=0> T; // Number of sampling occasions
  int<lower=0, upper=1> y[P, R, M, T]; // Capture-history matrix
}

transformed data {
  // Totals number of captures for each potential member.
  int<lower=0> s[P, R, M];

  // Size of observed data set.
  int<lower=0> C[P, R];

  // Go through all potential members.
  for(i in 1:P) {
    for(j in 1:R) {
      C[i, j] = 0;

      for (k in 1:M) {
        // Store the number of captures across all sampling occasions for this potential member.
        s[i, j, k] = sum(y[i, j, k]);

        // If this potential member was observed, add to the count.
        if (s[i, j, k] > 0) {
          C[i, j] = C[i, j] + 1;
        }
      }
    }
  }
}

parameters {
  // Inclusion probability - the probability that any potential member of N (that is, any member of
  // M) is in the true population N.
  real<lower=0, upper=1> omega[P, R];

  // Detection probability - the probability that any member of N is captured.
  vector<lower=0, upper=1>[T] p[P, R];
  vector<lower=0, upper=1>[T] p_bar_population[P];
  vector<lower=0, upper=1>[T] p_bar_region[R];
  vector<lower=0, upper=1>[T] p_bar_combined;
  real<lower=0> sigma_population;
  real<lower=0> sigma_region;
  real<lower=0> sigma_combined;
}

model {
  // Priors are implicitly defined;
  //  omega ~ uniform(0, 1);
  //  p ~ uniform(0, 1);
  //  p_bar_population ~ uniform(0, 1);
  //  p_bar_region ~ uniform(0, 1);

  // Likelihood

  for (i in 1:P) {
    p_bar_combined ~ normal(p_bar_population[i], sigma_population);
  }
  for (i in 1:R) {
    p_bar_combined ~ normal(p_bar_region[i], sigma_region);
  }

  // Go through all potential members.
  for (i in 1:P) {
    for (j in 1:R) {
      p[i, j] ~ normal(p_bar_combined, sigma_combined);

      for (k in 1:M) {
        if (s[i, j, k] > 0) {
          // This potential member was observed, which means it's a member of N.

          // Probability of 1 given probability omega. That is, the chance that this one is a real
          // member of N.
          target += bernoulli_lpmf(1 | omega[i, j]) +
          // Chance of s[i, j, k] successes in T trials given probability p[i, j]. That is, the
          // probability that this one was sighted during the sampling occasions.
            bernoulli_lpmf(y[i, j, k] | p[i, j]);
        } else { // s[i, j, k] == 0
          // This potential member was not observed, which means we don't know if it's a member of N
          // or not.

          // First, add probability that it it is a real member but wasn't sighted in any of the
          // sampling occasions.
          target += log_sum_exp(bernoulli_lpmf(1 | omega[i, j])
          // (chance of 0 successes in T trials given probability p[i, j])
                                + bernoulli_lpmf(0 | p[i, j]),
          // Second, add probability that it's not a real member.
          // (chance of 0 given probability omega)
                                bernoulli_lpmf(0 | omega[i, j]));
        }
      }
    }
  }
}

generated quantities {
  // Because Stan does not support vectors where each element has a different upper/lower constaint,
  // we need to calculate the population size as scaled to (0, 1) & then rescale it.
  // See: https://mc-stan.org/docs/2_18/stan-users-guide/vectors-with-varying-bounds.html
  real<lower=0, upper=1> N_raw[P, R];
  real<lower=0, upper=M> N[P, R];

  // For each distinct population, calculate the population size.
  for (i in 1:P) {
    for (j in 1:P) {
      // Probability of a member, real or not, not being sighted across all sampling occasions.
      real pr = prod(1 - p[i, j]);

      // `omega_nd` is the probability that a potential member is real given it was never detected.
      // Probability of a member being real but not being sighted across all sampling occasions.
      real omega_nd = (omega[i, j] * pr) /
        // Probability of a member, real or not, not being sighted across all sampling occasions.
        (omega[i, j] * pr + (1 - omega[i, j]));

      N_raw[i, j] = binomial_rng(M - C[i, j], omega_nd) * 1.0 / (M - C[i, j]);
      N[i, j] = N_raw[i, j] * (M - C[i, j]) + C[i, j];
    }
  }
}
