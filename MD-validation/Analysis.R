library(tidyverse)
library(psych)
library(brms)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(bayestestR)
library(correlation)

# read & tidy data
raw = read_csv("Moral Disengagement and Politics To Analyse.csv")
raw$ResponseId = factor(raw$ResponseId)
raw$Age = raw$Age + 17
raw$Gender = factor(case_match(raw$Gender, 
                               1 ~ 'Male',
                               2 ~ 'Female',
                               3 ~ 'NB',
                               4 ~ 'None'))
raw$Forced_Pol = factor(case_match(raw$Forced_Pol,
                                   1 ~ 'Democrat',
                                   2 ~ 'Republican'))


# exclude new scale columns
raw <- rename(raw, MD_5_O_blame=MD_5_N_blame) # fixing naming error
raw = raw %>% select_if(!str_detect(names(raw), '_N_'))

# create new data frame with final vars
d1 = raw[,1:7]
d1$Politics = scale(rowMeans(d1[,5:7]))
d1$MD_m = scale(rowMeans(raw[,str_detect(names(raw), 'MD_')]))

# basic plot
ggplot(d1, aes(x = Politics, y = MD_m)) + 
  geom_point() + geom_smooth(method = 'lm')

# Descriptives

describe(d1)

# Correlation Table

cor_tab = correlation(d1[,c(3,5:9)], bayesian = T)

# Regression
m1 = brm(MD_m ~ Politics, data = d1,
         prior = c(prior(normal(0, 1), class = 'Intercept'),
                   prior(normal(.15, .07), class = 'b')),
         iter = 3000,
         warmup = 1000,
         seed = 1917169)
summary(m1)
hdi(m1)
plot(conditional_effects(m1), points = T)

m2 = brm(MD_m ~ Politics, data = d1,
         prior = c(prior(normal(0, 1), class = 'Intercept'),
                   prior(normal(0, 1), class = 'b')),
         iter = 3000,
         warmup = 1000,
         seed = 1917169)
summary(m2)
hdi(m2)
plot(conditional_effects(m2), points = T)

# robustness check - MLE 
m3 <- lm(MD_m ~ Politics, data = d1)
summary(m2)

plot(m3, points = T)
