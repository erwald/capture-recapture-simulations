library(rstan)
options(mc.cores = parallel::detectCores())

m_init <- stan_model("simplest-model-sbc.stan")

stan_d <- list(P = 2, R = 2, M = 1000, T = 2)

# note that this takes a while, especially when M is large
sbc_out <- sbc(stanmodel = m_init, 
               data = stan_d, 
               M = 1000)
plot(sbc_out)
