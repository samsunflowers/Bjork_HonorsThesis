# Import libraries
library(cowplot)
library(readxl)
library(writexl)

# Importing data and cleaning
dosing_data <- read.csv("C:/Users/bjork/Downloads/Lab Stuff/chlorothalonil_dosing.csv")
dosing_data[5] <- list(NULL) # That column had my notes

# Calculate "average survival" by averaging 1s (alive) and 0s (dead) at different doses
lc_averages <- dosing_data %>%
  mutate(dose = factor(dose)) %>%
  group_by(dose) %>%
  summarize(mean_survival = mean(survival))

# Visualize survival over doses
ggplot(lc_averages, aes(x = dose, y = mean_survival)) + 
  geom_bar(stat = "identity", 
           position = position_dodge(), 
           show.legend = F, 
           fill = "goldenrod3", 
           color = "black") +
  geom_text(aes(label = paste(format(round(mean_survival*100,
                                           digits = 2), 
                                           nsmall =2),
                                           "%"), 
                vjust = -0.6, 
                hjust = 0.5, 
                size = 3), 
            show.legend = F) +
  labs(title  = "Average 48-Hour Survival",
     x = "Dose (ug/ul)", 
     y = "Proportion Alive") +
  theme(plot.title = element_text(hjust = 0.5))

# Use logistic regression with probit link function to assess the probability of survival across doses
probit <- glm(survival ~ dose, 
              data = dosing_data, 
              family = binomial(link = "probit"))
summary(probit)

# Define a function to extract the LC values
findInt <- function(model, value) {
  function(x) {
    predict(model, data.frame(dose=x), 
            type="response") - value
  }
}

# LC25 value
uniroot(findInt(probit, .75), 
        range(dosing_data$dose))$root

# LC20 value
uniroot(findInt(probit, .80), 
        range(dosing_data$dose))$root

# LC15 value
uniroot(findInt(probit, .85), 
        range(dosing_data$dose))$root

# LC10 value
uniroot(findInt(probit, .90), 
        range(dosing_data$dose))$root

# LC5 value
uniroot(findInt(probit, .95), 
        range(dosing_data$dose))$root

# Graphing dosing data
ggplot(dosing_data, aes(x = dose, y = survival)) + 
  geom_point(position = position_jitter(height = 0.05), 
             show.legend = F, 
             size = 4, 
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
  scale_x_continuous(expand = c(0,0)) +
  labs(title = "48-Hour Survival Probability",
       x = "Dose (ug/ul)", 
       y = "Proportion Alive") +
  theme(plot.title = element_text(hjust = 0.5))
  theme(axis.title = element_text(size = 15))

