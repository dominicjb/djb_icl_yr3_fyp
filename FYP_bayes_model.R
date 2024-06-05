getwd()
setwd("~/Documents/FYP_WP/code")

library(tidyverse)
library(readr)
library(Hmisc)
library(broom)
library(dplyr)
library(RColorBrewer)

# Load package
install.packages("rstanarm")
library(rstanarm)

#IMPORT DATA
data <- read_csv("../data/DJB_BEF_DATA_COMBINED_standardised_w_continentsv2.csv", 
                 col_types = cols(Biodiversity_value_x = col_number(), 
                                  Biodiversiy_value_SD = col_number(), 
                                  Ecosystem_function_value_y = col_number()))

#REMOVE NAs
data = subset(data, !is.na(Ecosystem_function_value_y))
data = subset(data, !is.na(Biodiversity_value_x))

data$Biodiversity_value_x <- data$Biodiversity_value_x + 0.0000001

data_pollination <- filter(data, Ecosystem_function_metric %in% "Pollination_and_dispersal_of_seeds_and_other_propagules")

install.packages("gtsummary")
library(gtsummary)

data_pollination %>% tbl_summary()

# Fit model
bayes.model <- stan_glmer(Ecosystem_function_value_y ~ Biodiversity_value_x + (1|DOI), data=data_pollination)

bayes.model <- stan_glmer(Ecosystem_function_value_y ~ Biodiversity_value_x + (1|DOI) + (1|Initials), adapt_delta = .999, data=data_pollination)

bayes.model2 <- stan_glmer(Ecosystem_function_value_y ~ Biodiversity_value_x + (1|DOI) + (1|Initials), adapt_delta = .9995, data=data_pollination)

# only pollination data
bayes.model3 <- stan_glmer(Ecosystem_function_value_y ~ Biodiversity_value_x + (1|DOI) + (1|Initials) + (Biodiversity_value_x|Continent), 
                           adapt_delta = .9995, data=data_pollination)

# shorter set of coefficients - gives the 
bayes.model3

# shows coefficients of a model
coef(bayes.model3)

# More comprehensive and useful coefficients
posterior_interval(bayes.model, prob=0.95) # just looks at 2.5% and 97.5% , median = 50%

summary(bayes.model3, probs=c(0.375, .975)) 

# calc r2
r2_bayes <- bayes_R2(bayes.model3) 
hist(r2_bayes) 
print(c(median(r2_bayes), mean(r2_bayes), sd(r2_bayes)))

(r2_bayes.model3 <- bayes_R2(bayes.model3))
print(mean(r2_bayes.model3))
print(sd(r2_bayes.model3))
hist(r2_bayes.model3)

install.packages("geomtextpath")
library(geomtextpath)

# pollination by biodiversity - should by using ONLY pollination data
(biodiversity_plot <- ggplot(data_pollination, aes(x= Biodiversity_value_x, y= Ecosystem_function_value_y)) + 
  scale_y_continuous(limits = c(0, 6), breaks = c(0, 2, 4, 6)) + 
  scale_x_continuous(limits = c(0, 6.1)) + 
  labs(x = "Biodiversity", y = "Pollination") + # ggtitle() + 
  geom_point(size=1.2) + 
  geom_abline(slope=0.2, intercept=1.4, colour='orange2', size=0.9) +
    theme_bw(base_size = 18) + 
    theme(axis.title.x = element_text(size = 14)) + 
    theme(axis.title.y = element_text(size = 14)))

# by continent comparison
(biodiversity_cont_plot <- ggplot(data_pollination, aes(x= Biodiversity_value_x, y= Ecosystem_function_value_y, colour=Continent)) + 
    scale_y_continuous(limits = c(0, 6), breaks = c(0, 2, 4, 6)) + 
    scale_x_continuous(limits = c(0, 6)) + 
    labs(x = "Biodiversity", y = "Pollination") + # ggtitle("Biodiversity by Pollination for the Continents") + 
    geom_point(size=1.2) +
    geom_abline(slope=0.09, intercept=1.38, # , label = "Asia",
                color = "red3", hjust = 0.75, vjust = -0.4, linewidth=0.8) + # the code ignores hjust and vjust as it for geom_textabline
    geom_abline(slope=0.17, intercept=1.34, # label = "Europe", 
                color = "green3", linewidth=0.8) +
    geom_abline(slope=0.19, intercept=1.34, # label = "North America", 
                color = "cornflowerblue", linewidth=0.8) +
    geom_abline(slope=0.24, intercept=1.33, # label = "South America", 
                color = "darkorchid", linewidth=0.8) +
    theme_bw(base_size = 18) + 
    theme(axis.title.x = element_text(size = 14)) + 
    theme(axis.title.y = element_text(size = 14)) + 
    theme(
      legend.text = element_text(size = 12),  # Adjust text size
      legend.title = element_text(size = 12)))  # Adjust title size)

# "Caterpillar" coefficient plots 
plot(bayes.model3)

# MCMC diagnostics
plot(bayes.model3, "trace")

# summarise bayesian model output in a table
install.packages("sjPlot")
library(sjPlot)

install.packages("brms")
library(brms)

tab_model(bayes.model3)

# plot model
install.packages("bayesplot")
library(bayesplot)

# plotting coefficients

install.packages("modelsummary")
library(modelsummary)

modelplot(bayes.model3)
modelplot(bayes.model3, coef_omit = 'Intercept')

# checking the model
pp_check(bayes.model3) # to check the predictive accuracy of the posterior distribution
waic(bayes.model) # model performance (improvement on DIC)
loo(bayes.model) # Another method alongside WAIC for comparing out-of-sample predictive ability is to apply leave-one-out cross-validation (LOO).

launch_shinystan(bayes.model3) # add this line of code into console to see a range of diagnostics

