# Import libraries
library(ggsignif)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

# Importing data and removing cells with no values
adult_data <- read.csv("/Users/bjork/Downloads/Lab Stuff/adult_weights.csv")
adult_data <- na.omit(adult_data)

# Calculate average weight for each dose group
avg_weight <- adult_data %>%
  group_by(dose) %>%
  summarise(avg_weight = mean(weight))

# Is there a significant change across all doses? ANOVA
anova <- aov(weight ~ dose, data = adult_data)
summary(anova)

# Factoring the doses to improve x-axis representation
adult_data$dose <- as.factor(adult_data$dose)
avg_weight$dose <- as.factor(avg_weight$dose)

# Bargraph and boxplot representing days to pupation
ggplot(avg_weight, aes(x = dose, y = avg_weight)) +
  geom_bar(stat = "identity", fill = "goldenrod2", color = 'black') +
  labs(title = "Comparison of Average Adult Weight After Emergence",
       x = "Dose (ug/ul)", 
       y = "Average Adult Weight (mg)") +
  geom_text(aes(label = paste(format(round(avg_weight,
                                           digits = 2), 
                                           nsmall = 2),
                                          "mg"),
                vjust = -0.6, 
                hjust = 0.5, 
                size = 3), 
            show.legend = F) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = adult_data, aes(x=dose, y=weight, fill=dose)) +
  geom_boxplot(alpha=0.7) +
  theme_minimal() + 
  labs(title = "Weight of Adults After Emergence",
       x = "Dose (ug/ul)",
       y = "Weight (mg)",
       color = "Dose") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual((name="Dose"),
                    labels=c("0 ug/ul", "5 ug/ul", "7 ug/ul", "9 ug/ul", "13 ug/ul"),
                    values=brewer.pal(n=6, name ="YlOrRd"))+
                    geom_signif(comparisons = list(c("0", "9"),
                                                   c("0", "13")),
                                map_signif_level = TRUE,
                                y_position = c(120, 130, 140))

# ("0","5") and ("0","7") was not significant and there were no 11 values.
