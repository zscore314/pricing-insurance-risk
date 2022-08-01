library(tidyverse)

discrete_sum <- function(x, p){
  
  .moment <- function(x, p, k = 1) {
    
    m = sum(x * p)
    sum((x - m) ^ k * p)
  }
  
  tibble(Mean = sum(x * p),
         CV = (.moment(x, p, 2) ^ .5) / Mean,
         Skewness = .moment(x, p, 3) / .moment(x, p , 2) ^ (3/2),
         Kurtosis = .moment(x, p, 4) / .moment(x, p , 2) ^ 2 - 3
  )
  
  
}


# 2.4.1 The Simple Discrete Example

x1 <- tibble(x1_gross = c(0, 8, 10),
             x1_net = c(0, 8, 10),
             px1 = c(.5, .25, .25))


x2 <- tibble(x2_gross = c(0, 1, 90),
             x2_net = c(0, 1, 20),
             px2 = c(.5, .25, .25))


x_total <- crossing(x1, x2) %>%
  mutate(total_gross = x1_gross + x2_gross,
         total_net = x1_net + x2_net,
         p = px1 * px2) %>%
  select(-px1, -px2)

x_total %>%
  pivot_longer(-p,names_to = c("Line", "GN"), names_sep = "_") %>%
  group_by(GN, Line) %>%
  nest() %>%
  summarise(s = map(data, ~ discrete_sum(.x$value, .x$p))) %>%
  ungroup() %>%
  unnest(s) %>%
  pivot_longer(Mean:Kurtosis, names_to = "Statistic") %>%
  mutate(Line = factor(Line, levels = c('x1', 'x2', 'total'))) %>%
  arrange(GN, Line) %>%
  pivot_wider(names_from = c(GN, Line)) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  flextable::flextable() %>%
  flextable::set_header_labels(Statistics = "Statistic",
                               gross_total = "Total",
                               gross_x1 = "X1",
                               gross_x2 = "X2",
                               net_total = "Total",
                               net_x1 = "X1",
                               net_x2 = "X2") %>%
  flextable::add_header_row(
    values = c("","Gross", "Net"),
    colwidths = c(1, 3, 3)) %>%
  flextable::align(align = "left", part = "header")
  

# we could also sample from this distribution
x_total %>% 
  slice_sample(n = 1e3, weight_by = p, replace = T) %>%
  select(-p)

#' Simulate gamma function with alternate parameterization
#'
#' @param n 
#' @param mean 
#' @param cv 
#'
#' @return
#' @export
#'
#' @examples
rgamma_alt <- function(n, mean, cv){
  
  shape = cv ^ -2
  rate = shape / mean
  rgamma(n, shape, rate)
}

# 2.4.2 Tame Case Study
sims <- tibble(trial = 1:10e3) %>%
  mutate(A = rgamma_alt(n(), 50, .1)) %>%
  mutate(B = rgamma_alt(n(), 50, .15))

sims %>% summarise(across(c(A, B), list(mean = mean, cv = ~ sd(.) / mean(.))))


