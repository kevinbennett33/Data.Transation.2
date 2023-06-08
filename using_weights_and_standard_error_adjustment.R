library(fixest)
library(tidyverse)

help(feols)

data(mtcars)

feols(mpg ~ hp, data = mtcars, weights =  mtcars$wt)
feols(mpg ~ hp, data = mtcars, weights =  ~ wt)

# Applying weights to a mean / aggregation
# note that weighted.mean is a specific function that exists, 
# you can't just add "weighted." to any function and expect it to work
mtcars %>%
  group_by(am) %>%
  summarize(mean_mpg = weighted.mean(mpg, wt))


mtcars %>%
  filter(am == 1) %>%
  feols(mpg ~ hp, weights =  mtcars$wt)
mtcars %>%
  filter(am == 1) %>%
  feols(mpg ~ hp, weights =  ~wt)

# heteroskedasticity-robust SEs
feols(mpg ~ hp, data = mtcars, weights =  ~ wt, vcov = 'hetero')
# clustering by AM
feols(mpg ~ hp, data = mtcars, weights =  ~ wt, vcov = ~ am)
# autocorrelation-consistent standard errors
data(economics)
economics <- economics %>%
  mutate(fake_id = 1)
feols(uempmed ~ pop, data = economics, vcov = 'NW', panel.id = c('fake_id','date'))
