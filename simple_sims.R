library(tidyverse)
library(rethinking)

set.seed(100)
M = 1000

f <- function(N, p1, p2) {
  y1 <- rbinom(N, 1, (p1 * p2)/2)
  y2 <- rbinom(N, 1, (p1 * p2))
  yfull <- matrix(c(y1, y2), ncol = 2)
  yobs <- yfull[apply(yfull, 1, max) == 1, ]
  return(rbind(yobs, array(0, dim = c(M - length(yobs) / 2, 2))))
}
aug1 <- f(600, .8, .5) # pop 1, region 1
aug2 <- f(800, .8, .3) # pop 1, region 2
aug3 <- f(350, .6, .5) # pop 2, region 1
aug4 <- f(450, .6, .3) # pop 2, region 2
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
