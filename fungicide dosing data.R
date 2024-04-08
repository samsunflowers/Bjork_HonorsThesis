# Libraries needed for analysis
library(tidyverse)
library(cowplot)
library(readxl)
library(writexl)
data <- read.csv("C:/Users/bjork/Downloads/Lab Stuff/chlorothalonil dosing.csv")
names(data) <- c("dosage","position", "well", "survival")
data[5] <- list(NULL)

# Summarize data and visualize survival over dosages

# Calculate "average survival" by averaging 1s (alive) and 0s (dead) at different dosages
lc_averages <- data %>%
  mutate(dosage = factor(dosage)) %>%
  group_by(dosage) %>%
  summarize(mean_survival = mean(survival))

# Visualize survival over dosages as a bar graph

ggplot(lc_averages, aes(x = dosage, y = mean_survival)) + 
  geom_bar(stat = "identity", 
           position = position_dodge(), 
           show.legend = F, 
           fill = "pink", 
           color = "black") +
  geom_text(aes(label = paste(format(round(mean_survival*100,
                                           digits = 2), 
                                     nsmall =2),
                              "%"), 
                vjust = -0.6, 
                hjust = 0.5, 
                size = 3), 
            show.legend = F) +
  theme_set(theme_cowplot(12)) +
  labs(x = "Dosage (ug/ul)", y = "Average 48-hr survival (% alive)") +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.ticks.x = element_blank())

dev.off()

### STEP 3: Fit logistic regression analysis with a probit link function
### In this step, we fit the data using logistic regression with a probit link function to assess the probability of survival across pesticide dosage treatments.

# Run probit analysis (logistic regression with probit link function) using glm()
probit <- glm(survival ~ dosage, 
              data = data, 
              family = binomial(link = "probit"))

# Get coefficients of probit function
summary(probit)

# Define a function to extract the lc25 value (dosage at which 75% of individuals survive on average)
findInt <- function(model, value) {
  function(x) {
    predict(model, data.frame(dosage=x), 
            type="response") - value
  }
}

# LC25 value
uniroot(findInt(probit, .75), 
        range(data$dosage))$root

# LC5 value
uniroot(findInt(probit, .95), 
        range(data$dosage))$root

# LC10 value
uniroot(findInt(probit, .90), 
        range(data$dosage))$root

# LC15 value
uniroot(findInt(probit, .85), 
        range(data$dosage))$root

# LC20 value
uniroot(findInt(probit, .8), 
        range(data$dosage))$root


x = data$dosage
cdf_1 <- ecdf(x)
x[which(cdf_1(x) == 7)]

ggplot(data, aes(x = dosage, y = survival)) + 
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
  labs(x = "Dosage (ug/ul)", y = "Survival Probability") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.ticks.x = element_blank())

