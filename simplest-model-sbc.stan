data {
  int<lower=0> P; // Number of distinct populations
  int<lower=0> R; // Number of distinct regions
  int<lower=0> M; // Size of augumented data set
  int<lower=0> T; // Number of sampling occasions
}

transformed data {
  // true parameter values
  real omega_ = uniform_rng(0, 1);
  real p_ = uniform_rng(0, 1);
  int<lower=0, upper=1> z[P, R, M];
  
  // data
  int<lower=0, upper=1> y[P, R, M, T]; // Capture-history matrix
  int<lower=0> s[P, R, M]; // total # of captures for each potential member
  int<lower=0> C[P, R]; // size of observed data set
  
  for (i in 1:P) {
    for (j in 1:R) {
      for (k in 1:M) {
        z[i, j, k] = bernoulli_rng(omega_);
        for (t in 1:T) {
          y[i, j, k, t] = bernoulli_rng(z[i, j, k] * p_);
        }
      }
    }
  }
  
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
  real<lower=0, upper=1> omega;

  // Detection probability - the probability that any member of N is captured.
  real<lower=0, upper=1> p;
}

model {
  for (i in 1:P) {
    for (j in 1:R) {
      for (k in 1:M) {
        if (s[i, j, k] > 0) {
          // This potential member was observed, which means it's a member of N.

          // Probability of 1 given probability omega. That is, the chance that this one is a real
          // member of N.
          target += bernoulli_lpmf(1 | omega) +
          // Chance of s[i, j, k] successes in T trials given probability p. That is, the
          // probability that this one was sighted during the sampling occasions.
            bernoulli_lpmf(y[i, j, k] | p);
        } else { // s[i, j, k] == 0
          // This potential member was not observed, which means we don't know if it's a member of N
          // or not.

          // First, add probability that it it is a real member but wasn't sighted in any of the
          // sampling occasions.
          target += log_sum_exp(bernoulli_lpmf(1 | omega)
          // (chance of 0 successes in T trials given probability p[i, j])
                                + bernoulli_lpmf(y[i, j, k] | p),
          // Second, add probability that it's not a real member.
          // (chance of 0 given probability omega)
                                bernoulli_lpmf(0 | omega));
        }
      }
    }
  }
}

generated quantities {
  int<lower=0, upper=1> y_[P, R, M, T] = y; 
  vector[2] pars_;
  int ranks_[2];
  
  pars_[1] = omega_;
  pars_[2] = p_;
  ranks_[1] = omega > omega_;
  ranks_[2] = p > p_;
}
