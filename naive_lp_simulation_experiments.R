library(tidyverse)
library(rethinking)
library(ggthemes)
library(Metrics)

# Generate capture-recapture data sets for Lincoln-Petersen estimations.

f <- function(N, p) {
  n <- rbinom(1, size = N, prob = p)
  K <- rbinom(1, size = N, prob = p)
  k <- rbinom(1, size = K, prob = n/N)
  return(c(k, n*(K/k)))
}

set.seed(1789)

# Try out some different values.
f(500, .1)
f(500, .5)
f(5000, .1)
f(50000, .01)

# n*K/k
# 169 167
#   90

# Plot different k counterfactuals.
k <- 1:200
df <- data.frame(k)
ggplot(df, aes(k)) +
  stat_function(fun = function(x) { return(sle(258.375*8/x, 500)) }, aes(color = 'N = 500\np = .1')) +
  stat_function(fun = function(x) { return(sle(489.8571*126/x, 500)) }, aes(color = 'N = 500\np = .5')) +
  stat_function(fun = function(x) { return(sle(4068.119*59/x, 5000)) }, aes(color = 'N = 5000\np = .1')) +
  stat_function(fun = function(x) { return(sle(29701.75*8/x, 50000)) }, aes(color = 'N = 50000\np = .01')) +
  scale_color_colorblind() +
  labs(x = "k",
       y = "Squared log error",
       caption = "Counterfactual plot of varying k (recaptured marked individuals) for the different experiments.
         Where the squared log error dips down to touch 0 very briefly, estimates are volatile & therefore unreliable.",
       color = "Scenario") +
  theme_pander() +
  theme(strip.placement = "outside", legend.position="top")
ggsave('lp_captures_counterfactual.png', width = 6, height = 4, dpi = 600)

# Plot different k counterfactuals for Plettinckx et al.
k <- 1:200
df <- data.frame(k)
ggplot(df, aes(k)) +
  stat_function(fun = function(x) { return(sle(169*167/x, 703)) }, aes(color = 'FW-OC')) +
  geom_vline(linetype = 'dashed', aes(xintercept = 51, color = 'FW-OC')) +
  stat_function(fun = function(x) { return(sle(169*90/x, 703)) }, aes(color = 'FW-RC')) +
  geom_vline(linetype = 'dashed', aes(xintercept = 56, color = 'FW-RC')) +
  stat_function(fun = function(x) { return(sle(167*90/x, 703)) }, aes(color = 'OC-RC')) +
  geom_vline(linetype = 'dashed', aes(xintercept = 40, color = 'OC-RC')) +
  scale_color_colorblind() +
  labs(x = "k",
       y = "Squared log error",
       caption = "Counterfactual plot of varying k (recaptured marked individuals) for data sets in Plettinckx et al.
         Where the squared log error dips down to touch 0 very briefly, estimates are volatile & therefore unreliable.",
       color = "Scenario") +
  theme_pander() +
  theme(strip.placement = "outside", legend.position="top")
ggsave('lp_captures_counterfactual_brussels_injections.png', width = 6, height = 4, dpi = 600)
