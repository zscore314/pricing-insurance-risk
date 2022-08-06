# Setup -------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
library(janitor)
library(readxl)
library(AonInternal)
library(AonAltair)
options(dplyr.summarise.inform = FALSE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$`path`))

# Exact
actuar::levgamma(Inf, tame$alpha[2], tame$rate[2]) - (actuar::levgamma(tame_ri$exhaust, tame$alpha[2], tame$rate[2]) - actuar::levgamma(tame_ri$attach, tame$alpha[2], tame$rate[2]))

buckets <- 2^12
pml <- 200 # maximum of discretized distribution
step_size <- pml / buckets

cdf <- tibble(start = 0:(buckets-1) * step_size,
       mid = start + step_size / 2) %>%
  mutate(net = mid - pmin(tame_ri$limit, pmax(mid - tame_ri$attach, 0))) %>%
  mutate(ground_up = pgamma(mid, tame$alpha[2], tame$rate[2]))

cdf %>% CopyClip

  mutate(ground_up = pgamma(mid, tame$alpha[2], tame$rate[2])) %>%
  mutate(across(Fx, ~if_else(row_number() == 1, ., . - lag(.))))

# Mean
cdf %>% summarise(gross = sum(mid * Fx),
                  net = sum(net * Fx))

cdf2 %>%
  mutate(across(ceded, ~if_else(row_number() == 1, ., . - lag(.)))) %>%
  summarise(sum(ceded * mid))


z <- matrix(c(cdf$mid, cdf$net), nrow = 4096, ncol = 2) %>% fft %>% fft(inverse = T)

Re(z[,2]) / 4096
mutate(Total = Re(fft(fft(A) * fft(B), inverse = T)) / buckets)

cdf %>%
  filter(mid > 56)
  summarise(sum(Fx * net * 1000))





