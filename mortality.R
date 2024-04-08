# Import libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(readxl)
library(writexl)
library(ggsignif)

# Importing data and removing cells with no values
mortality_data <- read.csv("/Users/bjork/Downloads/Lab Stuff/mortality.csv")
mortality_data <- replace(mortality_data, is.na(mortality_data), 1)

# Compiling all the "day_" columns into one column
mortality_data_long <- mortality_data %>%
  pivot_longer(cols = starts_with("day"),
               names_to = "day",
               values_to = "mortality") %>%
  mutate(day = as.numeric(gsub("day_", "", day)))

# Calculate "average survival" by averaging 1s (alive) and 0s (dead) at different dosages
mortality_average <- mortality_data_long %>%
  mutate(dose = factor(dose)) %>%
  group_by(dose) %>%
  summarize(mean_survival = mean(mortality))

mortality_data_long <- merge(mortality_average, mortality_data_long, by = "dose")

# Statistical testing
mortality_lm<-lm(mortality~factor(dose), data=mortality_data_long)
mortality_anova<-aov(mortality_lm)
summary(mortality_anova)
mortality_t<-TukeyHSD(mortality_anova)
mortality_t

# Graphing mortality
ggplot(mortality_data_long, aes(x = factor(dose), y = mortality)) +
  theme_minimal() +
  stat_summary(fun='mean', 
               geom='bar',
               show.legend = F,
               fill="goldenrod2",
               color = 'black') +
  geom_signif(comparisons = list(c("0", "5"),
                                 c("0", "7"),
                                 c("0", "9"),
                                 c("0", "11"),
                                 c("0", "13")),
              map_signif_level = TRUE,
              y_position = c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5)) +
  geom_text(aes(label = paste(format(round(mean_survival*100,
                                           digits = 1),
                                     nsmall = 1),
                              "%"),
                vjust = -0.6,
                hjust = 0.5,
                x = dose,
                y = mean_survival,
                size = 2),
            show.legend = F) +
  labs(title  = "Average 30-Day Survival",
       x = "Dose (ug/ul)", 
       y = "Proportion Alive") +
  theme(plot.title = element_text(hjust = 0.5))

# Use logistic regression with probit link function to assess the probability of survival across doses
probit <- glm(mortality ~ dose, 
              data = mortality_data_long, 
              family = binomial(link = "probit"))
summary(probit)

# Plot mortality
ggplot(mortality_data_long, aes(x = dose, y = mortality)) + 
  theme_light()+
  geom_point(position = position_jitter(height = 0.05), 
             show.legend = T, 
             size = 3, 
             color = "black",
             fill = "darkgoldenrod3",
             pch=21) + 
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"), 
              show.legend = F,
              color = "black", 
              fullrange = T) + 
  theme_set(theme_cowplot(12)) +
  geom_hline(yintercept = 0.75, 
             color = "darkorange4", 
             linetype = "longdash", 
             size = 1) +
  labs(title = "30-Day Survival Probability",
       x = "Dose (ug/ul)", 
       y = "Proportion Alive") +
  theme(plot.title = element_text(hjust = 0.5))
  theme(axis.title = element_text(size = 15))
