library(tidyverse)
library(rethinking)
library(ggthemes)

set.seed(1789)

sim <- function(killings, probs, suffix) {
  n <- length(killings)
  killings <- killings
  # Simulate observations of killings, with a random error.
  recorded <- rbinom(n, size=killings, prob=(rep(probs[1], n) + rnorm(4, 0, .05)))
  census <- rbinom(n, size=killings, prob=(rep(probs[2], n) + rnorm(4, 0, .05)))
  both <- rbinom(n, size=recorded, prob=(census / killings))
  res <- list(killings = killings, recorded = recorded, census = census, both = both)
  names(res) <- paste(names(res), suffix, sep = "_")
  return(res)
}

province <- c('Chaka', 'Wengti', 'Mujol', 'Hoshtengu')
population <- c(9000, 46000, 22000, 23000) # x.1, x.075, x.1, x.05

k <- sim(c(1344 + 288, 5301 + 1515, 3411, 1700 + 214), c(.6, .35), 'K') # x1.5
a <- sim(c(704 + 341, 3416 + 1043, 2357 + 584, 1257), c(.2, .1), 'A') # x1
z <- sim(c(260, 894, 567 + 481, 116 + 668), c(.15, .3), 'Z') # x0.25

df <- data.frame(province, population, k, a, z) %>%
  mutate(min_estimate_K = recorded_K + census_K - both_K,
         min_estimate_A = recorded_A + census_A - both_A,
         min_estimate_Z = recorded_Z + census_Z - both_Z,
         lp_estimate_K = recorded_K * census_K / both_K,
         lp_estimate_A = recorded_A * census_A / both_A,
         lp_estimate_Z = recorded_Z * census_Z / both_Z)

# Plot population per province.
ggplot(df, aes(province, population)) +
  geom_col(fill = 'black') +
  coord_flip() +
  scale_x_discrete(limits = c('Chaka', 'Mujol', 'Hoshtengu', 'Wengti')) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = element_blank(),
       y = element_blank(),
       caption = "Population of each of the provinces of Tamego.") +
  theme_pander() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(r = 15))
ggsave('tamego_population.png', width = 6, height = 2, dpi = 600)

reshaped <- df %>%
  select(province, population,
         killings_K, recorded_K, census_K, both_K,
         killings_A, recorded_A, census_A, both_A,
         killings_Z, recorded_Z, census_Z, both_Z,
         min_estimate_K, min_estimate_A, min_estimate_Z,
         lp_estimate_K, lp_estimate_A, lp_estimate_Z) %>%
  gather(-province, -population, key = variable, value = killings) %>%
  mutate(faction = if_else(str_detect(variable, fixed('_K')),
                           'King',
                           if_else(str_detect(variable, fixed('_A')), 'Angu', 'Zid')),
         medium = if_else(str_detect(variable, fixed('recorded')),
                           'Recorded',
                           if_else(str_detect(variable, fixed('census')),
                                   'Census',
                                   if_else(str_detect(variable, fixed('both')), 'Both', 'Killings'))),
         estimate = if_else(str_detect(variable, fixed('min_estimate')),
                            'Minimum estimate',
                            if_else(str_detect(variable, fixed('lp_estimate')), 'LP estimate', 'NA'))) %>%
  arrange(desc(medium))

# Plot killings per faction, medium & province.
reshaped %>%
  filter(!grepl('Killings', medium)) %>%
  ggplot(aes(province,
                     killings,
                     fill = factor(medium, levels = c('Recorded', 'Census', 'Both')))) +
  geom_col(position = "dodge") +
  facet_wrap(~ faction, strip.position = "bottom", scales = "free_x") +
  scale_x_discrete(labels = rep(province, 3)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = element_blank(),
       y = element_blank(),
       caption = "Recorded killings by each faction in each of the provinces of Tamego as taken down by each method.",
       fill = "Method") +
  scale_fill_colorblind() +
  theme_pander() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.placement = "outside",
        legend.position="top")
ggsave('tamego_killings_recorded.png', width = 6, height = 4, dpi = 600)

# Plot estimates per faction, medium & province.
reshaped %>%
  filter(!grepl('killings_', variable)) %>%
  ggplot(aes(province,
             killings,
             fill = factor(
               interaction(medium, estimate),
               c('Recorded.NA', 'Census.NA', 'Both.NA', 'Killings.Minimum estimate', 'Killings.LP estimate')
               ))) +
  geom_col(position = "dodge") +
  facet_wrap(~ faction, strip.position = "bottom", scales = "free_x") +
  scale_x_discrete(labels = rep(province, 3)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = element_blank(),
       y = element_blank(),
       caption = "Recorded & estimated killings by each faction in each of the provinces of Tamego.",
       fill = element_blank()) +
  scale_fill_colorblind(labels = c("Recorded", "Census", "Both", "Minimum estimate", "LP estimate")) +
  theme_pander() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.placement = "outside",
        legend.position="top")
ggsave('tamego_killings_estimates.png', width = 6, height = 4, dpi = 600)

# Plot estimates & true killings per faction, medium & province.
reshaped %>%
  filter(grepl('Killings', medium)) %>%
  ggplot(aes(province,
             killings,
             fill = factor(
               interaction(medium, estimate),
               c('Killings.Minimum estimate', 'Killings.LP estimate', 'Killings.NA')))
         ) +
  geom_col(position = "dodge") +
  facet_wrap(~ faction, strip.position = "bottom", scales = "free_x") +
  scale_x_discrete(labels = rep(province, 3)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = colorblind_pal()(6)[4:6],
                    labels = c("Minimum estimate", "Lincoln-Petersen estimate", "Actual killings")) +
  labs(x = element_blank(),
       y = element_blank(),
       caption = "Estimated & actual killings by each faction in each of the provinces of Tamego.",
       fill = element_blank()) +
  theme_pander() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.placement = "outside",
        legend.position="top")
ggsave('tamego_killings_true.png', width = 6, height = 4, dpi = 600)

# Transform data into a more convenient format for the model.
df_for_stan <- reshaped %>%
  select(-variable) %>%
  spread(medium, killings) %>%
  rename_with(tolower) %>%
  mutate(province_faction_idx = as.integer(as.factor(interaction(province, faction))),
         province_idx = as.integer(as.factor(province)),
         faction_idx = as.integer(as.factor(faction)),
         recorded = as.double(recorded),
         census = as.double(census),
         both = as.double(both),
         log_recorded = log(recorded),
         log_census = log(census),
         log_both = log(both))

# Calculate real sum total of killings.
sum(df$killings_A + df$killings_K + df$killings_Z)

df_K <- df_for_stan[df_for_stan$faction == "King",]
df_A <- df_for_stan[df_for_stan$faction == "Angu",]
df_Z <- df_for_stan[df_for_stan$faction == "Zid",]

# Calculate minimum total of killings as well as Lincoln-Petersen
# total of killings.
df_for_stan[df_for_stan$estimate == "NA",] %>%
  group_by(faction) %>%
  summarize(min_total = sum(recorded + census - both),
            lp_total = sum(recorded * census / both))

# N = killings, M = recorded, C = census, R = both

# Estimate killings using Bayesian model.
df_for_stan_2 <- df_for_stan[df_for_stan$estimate == "NA",]
M = 10000
f <- function(recorded, census, both) {
  y1 <- c(rep(1, recorded), rep(0, census - both))
  y2 <- c(rep(1, both), rep(0, recorded - both), rep(1, census - both))
  yfull <- matrix(c(y1, y2), ncol = 2)
  yobs <- yfull[apply(yfull, 1, max) == 1, ]
  return(rbind(yobs, array(0, dim = c(M - length(yobs) / 2, 2))))
}
g <- function(row) {
  aug <- f(as.integer(row['recorded']), as.integer(row['census']), as.integer(row['both']))
  return(aperm(aug, c(2, 1)))
}
augs <- apply(df_for_stan_2, 1, g)
y <- aperm(array(augs, dim = c(2, M, 3, 4)), c(3, 4, 2, 1))
m <- stan(file = 'model.stan',
          data = list(P = 3, R = 4, M = M, T = 2, y = y),
          chains = 4,
          iter = 2000,
          cores = 4)
precis(m, 3)
draws <- extract(m)
summary <- summary(m, pars = 'N', probs = c(.055, 1 - .055))
plot(m)

means <- summary$summary[, c('mean')]
lb <- summary$summary[, c('5.5%')]
ub <- summary$summary[, c('94.5%')]

# Plot estimates (including Bayesian) & true killings per faction, medium & province.
reshaped %>%
  filter(grepl('Killings', medium)) %>%
  select(-population, -variable, -medium) %>%
  mutate(lb = NA, ub = NA) %>%
  add_row(province = 'Chaka', faction = 'King',
          killings = means['N[2,1]'], lb = lb['N[2,1]'], ub = ub['N[2,1]'], estimate = 'Bayesian') %>%
  add_row(province = 'Wengti', faction = 'King',
          killings = means['N[2,4]'], lb = lb['N[2,4]'], ub = ub['N[2,4]'], estimate = 'Bayesian') %>%
  add_row(province = 'Mujol', faction = 'King',
          killings = means['N[2,3]'], lb = lb['N[2,3]'], ub = ub['N[2,3]'], estimate = 'Bayesian') %>%
  add_row(province = 'Hoshtengu', faction = 'King',
          killings = means['N[2,2]'], lb = lb['N[2,2]'], ub = ub['N[2,2]'], estimate = 'Bayesian') %>%
  add_row(province = 'Chaka', faction = 'Angu',
          killings = means['N[1,1]'], lb = lb['N[1,1]'], ub = ub['N[1,1]'], estimate = 'Bayesian') %>%
  add_row(province = 'Wengti', faction = 'Angu',
          killings = means['N[1,4]'], lb = lb['N[1,4]'], ub = ub['N[1,4]'], estimate = 'Bayesian') %>%
  add_row(province = 'Mujol', faction = 'Angu',
          killings = means['N[1,3]'], lb = lb['N[1,3]'], ub = ub['N[1,3]'], estimate = 'Bayesian') %>%
  add_row(province = 'Hoshtengu', faction = 'Angu',
          killings = means['N[1,2]'], lb = lb['N[1,2]'], ub = ub['N[1,2]'], estimate = 'Bayesian') %>%
  add_row(province = 'Chaka', faction = 'Zid',
          killings = means['N[3,1]'], lb = lb['N[3,1]'], ub = ub['N[3,1]'], estimate = 'Bayesian') %>%
  add_row(province = 'Wengti', faction = 'Zid',
          killings = means['N[3,4]'], lb = lb['N[3,4]'], ub = ub['N[3,4]'], estimate = 'Bayesian') %>%
  add_row(province = 'Mujol', faction = 'Zid',
          killings = means['N[3,3]'], lb = lb['N[3,3]'], ub = ub['N[3,3]'], estimate = 'Bayesian') %>%
  add_row(province = 'Hoshtengu', faction = 'Zid',
          killings = means['N[3,2]'], lb = lb['N[3,2]'], ub = ub['N[3,2]'], estimate = 'Bayesian') %>%
  ggplot(aes(province,
             killings,
             fill = factor(estimate, c('Minimum estimate', 'LP estimate', 'NA', 'Bayesian')))) +
  geom_col(position = "dodge") +
  geom_linerange(aes(ymin = lb, ymax = ub), position = position_dodge(width=0.9)) +
  facet_wrap(~ faction, strip.position = "bottom", scales = "free_x") +
  scale_x_discrete(labels = rep(province, 3)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = cbind(colorblind_pal()(6)[4:6], colorblind_pal()(8)[8]),
                    labels = c("Minimum estimate", "LP estimate", "Actual killings", "Bayesian estimate")) +
  labs(x = element_blank(),
       y = element_blank(),
       caption = paste("Estimated & actual killings by each faction in each of the provinces of Tamego.",
         "The Bayesian estimates also show 89% uncertainty intervals.", sep = '\n'),
       fill = element_blank()) +
  theme_pander() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.placement = "outside",
        legend.position = "top")
ggsave('tamego_killings_bayesian_estimate.png', width = 6, height = 4, dpi = 600)
