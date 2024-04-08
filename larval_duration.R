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

pupation_data_long <- merge(pupation_data, avg_pupation_days, by = "dose")

# Is there a significant change across all doses? ANOVA
duration_lm<-lm(days_to_pupate~factor(dose), data=pupation_data)
duration_anova<-aov(duration_lm)
summary(duration_anova)
duration_t<-TukeyHSD(duration_anova)
duration_t

# Bargraph and boxplot representing days to pupation
ggplot(pupation_data_long, aes(x = factor(dose), y = days_to_pupate)) +
  theme_minimal()+
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
              y_position = c(20, 24, 28, 32, 36)) +
  labs(title ="Comparison of Average Days to Pupation",
       x = "Dose (ug/ul)", 
       y = "Days to Pupation") +
  geom_text(aes(label = paste(format(round(avg_pupation_days,
                                           digits = 1), 
                                           nsmall = 1),
                                          "days"),
                vjust = -0.6, 
                hjust = 0.5,
                y = avg_pupation_days,
                size = 3), 
            show.legend = F) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = pupation_data, aes(x=factor(dose), y=days_to_pupate, fill = as.character(dose))) +
  geom_boxplot(alpha = 0.7) +
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
                                                   c("0", "11"),
                                                   c("0", "13")),
                                map_signif_level = TRUE,
                                y_position = c(24, 27, 30, 33, 36))
