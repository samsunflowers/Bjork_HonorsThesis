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

weight_data_long <- merge(adult_data, avg_weight, by = "dose")

# Is there a significant change across all doses? ANOVA
weight_lm<-lm(weight~factor(dose), data=weight_data_long)
weight_anova<-aov(weight_lm)
summary(weight_anova)
weight_t<-TukeyHSD(weight_anova)
weight_t

# Bargraph and boxplot representing days to pupation
ggplot(weight_data_long, aes(x = factor(dose), y = weight)) +
  theme_minimal() +
  stat_summary(fun='mean', 
               geom='bar',
               show.legend = F,
               fill="goldenrod2",
               color = 'black') +
  geom_signif(comparisons = list(c("0", "5"),
                                 c("0", "7"),
                                 c("0", "9"),
                                 c("0", "13")),
              map_signif_level = TRUE,
              y_position = c(95, 105, 115, 125, 135)) +
  labs(title = "Comparison of Average Adult Weight After Emergence",
       x = "Dose (ug/ul)", 
       y = "Average Adult Weight (mg)") +
  geom_text(aes(label = paste(format(round(avg_weight,
                                           digits = 2), 
                                     nsmall = 2),
                              "mg"),
                vjust = -0.6, 
                hjust = 0.5,
                y = avg_weight,
                size = 2), 
            show.legend = F) +
  theme(plot.title = element_text(hjust = 0.5))

# There were not enough values in the 11 ug/ul group to map significance

ggplot(data = adult_data, aes(x=as.factor(dose), y=weight, fill=as.character(dose))) +
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
                    geom_signif(comparisons = list(c("0", "5"),
                                                   c("0", "7"),
                                                   c("0", "9"),
                                                   c("0", "13")),
                                map_signif_level = TRUE,
                                y_position = c(120, 130, 140, 150))

# There were no values for the 11 ug/ul dose.
