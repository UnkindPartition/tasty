library(tidyverse)
library(broom)
d <- read_tsv("times.tsv")
d %>% ggplot() + geom_line(aes(n,time,color=mode))

d1 <- d %>% group_by(mode) %>% do(augment(lm(time ~ n + I(n^2), data=.))) %>% ungroup %>%
  gather(key = "source", value = "time", time, .fitted)

d1 %>% ggplot() + geom_line(aes(n,time,color=mode,linetype=source))
