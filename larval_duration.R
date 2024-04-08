# Import libraries
library(ggsignif)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

# Importing data and removing cells with no values
pupation_data <- read.csv("/Users/bjork/Downloads/Lab Stuff/days_to_pupation.csv")
pupation_data <- na.omit(pupation_data)

# Calculate average weight for each dose group
avg_pupation_days <- pupation_data %>%
  group_by(dose) %>%
  summarise(avg_pupation_days = mean(days_to_pupate))

# Is there a significant change across all doses? ANOVA
anova <- aov(days_to_pupate ~ dose, data = pupation_data)
summary(anova)

# Factoring the doses to improve x-axis representation
pupation_data$dose <- as.factor(pupation_data$dose)
avg_pupation_days$dose <- as.factor(avg_pupation_days$dose)

# Bargraph and boxplot representing days to pupation
ggplot(avg_pupation_days, aes(x = dose, y = avg_pupation_days)) +
  geom_bar(stat = "identity", fill = "goldenrod2", color = "black") +
  labs(title ="Comparison of Average Days to Pupation",
       x = "Dose (ug/ul)", 
       y = "Days to Pupation") +
  geom_text(aes(label = paste(format(round(avg_pupation_days,
                                           digits = 1), 
                                           nsmall = 1),
                                          "days"),
                vjust = -0.6, 
                hjust = 0.5, 
                size = 3), 
            show.legend = F) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = pupation_data, aes(x=dose, y=days_to_pupate, fill=dose)) +
  geom_boxplot(alpha=0.7) +
  theme_minimal() +
  labs(title = "Days To Pupation",
       x = "Dose (ug/ul)",
       y = "Days",
       color = "Dose") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual((name="Dose"),
                    labels=c("0 ug/ul", "5 ug/ul", "7 ug/ul", "9 ug/ul", "11 ug/ul", "13 ug/ul"),
                    values=brewer.pal(n=6, name ="YlOrRd")) +
                    geom_signif(comparisons = list(c("0", "5"),
                                                   c("0", "7"),
                                                   c("0", "9"),
                                                   c("0", "13")),
                                map_signif_level = TRUE,
                                y_position = c(24, 27, 30, 33))
# ("0","11") was not significant since there was only one value for 11 ug/ul.
