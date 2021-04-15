library(tidyverse)
library(rethinking)
library(ggthemes)

# This file contains some not very beautiful code for simulating & experimenting with
# capture-recapture data.

set.seed(100)
M = 1000

f <- function(N, p1, p2, p3) {
  y1 <- rbinom(N, 1, (p1 * p2 * p3))
  y2 <- rbinom(N, 1, (p1 * p2))
  yfull <- matrix(c(y1, y2), ncol = 2)
  yobs <- yfull[apply(yfull, 1, max) == 1, ]
  return(rbind(yobs, array(0, dim = c(M - length(yobs) / 2, 2))))
}
aug1 <- f(600, .8, .5, .5) # pop 1, region 1
aug2 <- f(800, .8, .3, .5) # pop 1, region 2
aug3 <- f(350, .6, .5, .5) # pop 2, region 1
aug4 <- f(450, .6, .3, .5) # pop 2, region 2
# I'm ashamed of this ugly way of getting the right array format, but couldn't find a better way:
y <- aperm(array(c(aperm(aug1, c(2, 1)),
                   aperm(aug2, c(2, 1)),
                   aperm(aug3, c(2, 1)),
                   aperm(aug4, c(2, 1))), dim = c(2, M, 2, 2)),
           c(4, 3, 2, 1))
m <- stan(file = 'model.stan',
          data = list(P = 2, R = 2, M = M, T = 2, y = y),
          chains = 4,
          iter = 2000,
          cores = 4)
precis(m, 3)

# Try the unchanged Mt model (which has no partial pooling).
fit <- stan(file = 'model_mt.stan',
            data = list(M = M, T = 2, y = aug4),
            chains = 4,
            iter = 2000,
            cores = 4)
precis(fit, 2)
dens(extract(fit)$N)

# Naive Lincoln-Petersen estimates.
sum(aug1[,1]) * sum(aug1[,2]) / sum(apply(aug1, 1, min))
sum(aug2[,1]) * sum(aug2[,2]) / sum(apply(aug2, 1, min))
sum(aug3[,1]) * sum(aug3[,2]) / sum(apply(aug3, 1, min))
sum(aug4[,1]) * sum(aug4[,2]) / sum(apply(aug4, 1, min))

# Simulate LP estimates for varying population sizes & varying detection probabilities.
plot_sim_lp_estimates <- function(scale) {
  N <- seq(500, 10000 * scale, 500 * scale)
  p <- seq(.05, 1 * scale, .05 * scale)

  g <- function(N, p) {
    aug <- f(N, p, 1, 1)
    return(sum(aug[,1]) * sum(aug[,2]) / sum(apply(aug, 1, min)))
  }
  h <- function(N, p) {
    return(mean(replicate(5, g(N, p))))
  }
  percentage_error <- function(m, o) {
    return(abs(1 - m/o))
  }

  lp_sim_df <- crossing(N, p) %>%
    mutate(estimate = purrr::pmap_dbl(list(N, p), h)) %>%
    mutate(percentage_error = purrr::pmap_dbl(list(N, estimate), percentage_error))
  precis(lp_sim_df)

  plot <- lp_sim_df %>% ggplot(aes(x=N, y=p, color=percentage_error, alpha=percentage_error)) +
    geom_count() +
    theme_pander() +
    scale_color_continuous_tableau(palette = 'Classic Area Red') +
    labs(x = "Population size",
         y = "Detection probability",
         caption = "Percentage errors of Lincoln-Petersen estimates of varying population sizes & varying detection probabilities.") +
    theme(strip.placement = "outside", legend.position = "none")

  return(plot)
}

set.seed(1789)

plot_sim_lp_estimates(1)
ggsave('lp_estimates_1.png', width = 6, height = 5, dpi = 600)

plot_sim_lp_estimates(.25)
ggsave('lp_estimates_2.png', width = 6, height = 5, dpi = 600)

plot_sim_lp_estimates(.1)
ggsave('lp_estimates_3.png', width = 6, height = 5, dpi = 600)
