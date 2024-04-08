# Libraries needed for analysis
library(tidyverse)
library(ggplot2)
library(tidyr)
library(cowplot)
library(readxl)
library(writexl)
library(gridExtra)
library(ggsignif)

# Importing data and cleaning data
data <- read.csv("/Users/bjork/Downloads/Lab Stuff/all_histone_data_chloro.csv")
data$unique_id <- paste(data$modification, data$date, sep = '_')

# What is the amount of protein in each dose of H3K27me3?
h3k27me3_july <- (data[data$unique_id %in% "h3k27me3_july_23", ])
h3k27me3_july$amt_ngmg <- ((((h3k27me3_july$absorbance) - 0.006) / (300 * h3k27me3_july$slope) * 1000))

h3k27me3_march <- (data[data$unique_id %in% "h3k27me3_march_24", ])
h3k27me3_march$amt_ngmg <- ((((h3k27me3_march$absorbance) - 0.043) / (300 * h3k27me3_march$slope) * 1000))

h3k27me3 <- merge(h3k27me3_july, h3k27me3_march, all=TRUE)

# What is the amount of protein in each dose of H3K27ac?
h3k27ac_july <- (data[data$unique_id %in% "h3k27ac_july_23", ])
h3k27ac_july$amt_ngmg <- ((((h3k27ac_july$absorbance) - 0.043) / (300 * h3k27ac_july$slope) * 1000))

h3k27ac_march <- (data[data$unique_id %in% "h3k27ac_march_24", ])
h3k27ac_march$amt_ngmg <- ((((h3k27ac_march$absorbance) - 0.043) / (300 * h3k27ac_march$slope) * 1000))

h3k27ac <- merge(h3k27ac_july, h3k27ac_march, all=TRUE)

# What is the amount of protein in each dose of H3K4me3?
#h3k4me3 <- (data[data$unique_id %in% "h3k4me3_july_23", ])
#h3k4me3$amt_ngmg <- ((((h3k4me3$absorbance) - 0.043) / (300* h3k4me3$slope)) * 1000)

#Combining all data frames together for plotting
all_histone_protein <- rbind(h3k27ac, h3k27me3)

# Fixing x-axis spacing
all_histone_protein$dose <- as.numeric(all_histone_protein$dose)
all_histone_protein$dose <- as.factor(all_histone_protein$dose)
h3k27me3$dose <- as.numeric(h3k27me3$dose)
h3k27me3$dose <- as.factor(h3k27me3$dose)
h3k27ac$dose <- as.numeric(h3k27ac$dose)
h3k27ac$dose <- as.factor(h3k27ac$dose)
#h3k4me3$dose <- as.numeric(h3k4me3$dose)
#h3k4me3$dose <- as.factor(h3k4me3$dose)

# Barplot representing the different protein amounts
library(RColorBrewer)
ggplot(all_histone_protein, aes(x=dose, y=amt_ngmg)) + 
  geom_bar(stat = "summary",  width = 0.75, aes(fill=modification),
           position = "dodge2", fun="mean", color = "black",
           show.legend = T) +
  theme_minimal() +
  labs(title = "Amount of Modified Histone Protein Amount (ng/mg)",
       x = "Dose (ug/ul)",
       y = "Modified Histone Protein (ng/mg)",
       color = "Dose") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual((name="Histone"),
                    labels=c("H3K27ac","H3K27me3"),
                    values=brewer.pal(n=3, name ="YlOrRd"))

# Math to add a hline at the median for 0 ug/ul
h3k27me3_0ug <- (h3k27me3[h3k27me3$dose %in% "0", ])
median(h3k27me3_0ug$amt_ngmg) # 0.6853457

h3k27ac_0ug <- (h3k27ac[h3k27ac$dose %in% "0", ])
median(h3k27ac_0ug$amt_ngmg) # 0.2775453

# Boxplot
h3k27me3$dose <- as.factor(h3k27me3$dose)
h3k27ac$dose <- as.factor(h3k27ac$dose)

boxplot_histone <- ggplot(all_histone_protein, aes(x=dose, y=amt_ngmg, fill=modification)) +
  geom_boxplot(alpha=0.7) +
  theme_light() +
  labs(title = "Amount of Modified Histone Protein Amount (ng/mg)",
       x = "Dose (ug/ul)",
       y = "Modified Histone Protein (ng/mg)",
       color = "Dose") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual((name="Histone"),
                    labels=c("H3K27ac","H3K27me3"),
                    values=brewer.pal(n=3, name ="YlOrRd"))

boxplot_histone + geom_hline(yintercept = c(0.6853457, 0.2775453), color = c("darkorange4", "darkgoldenrod3"), linetype = c("dashed", "longdash"))

# Graphs of each modification
h3k27me3_plot <-ggplot(data=h3k27me3, aes(x=dose, y=amt_ngmg, fill=dose)) +
  geom_bar(stat = "summary", fun="mean", width = 0.75, color = "black",
           show.legend = T) +
  theme_minimal() +
  labs(title = "Amount of Modified Histone Protein Amount (ng/mg)",
       x = "Dose (ug/ul)",
       y = "Modified Histone Protein (ng/mg)",
       color = "Dose") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual((name="Dose"),
                    labels=c("0 ug/ul", "5 ug/ul", "7 ug/ul", "9ug/ul", "11 ug/ul", "13 ug/ul"),
                    values=brewer.pal(n=6, name ="YlOrRd")) +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = 0, vjust = 0.9, size = 6)

h3k27me3_plot

h3k27ac_plot <-ggplot(data=h3k27ac, aes(x=dose, y=amt_ngmg, fill=dose)) +
  geom_bar(stat = "summary", fun="mean", width = 0.75, color = 'black',
           show.legend = T) +
  theme_minimal() +
  labs(title = "Amount of Modified Histone Protein Amount (ng/mg)",
       x = "Dose (ug/ul)",
       y = "Modified Histone Protein (ng/mg)",
       color = "Dose") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual((name="Dose"),
                    labels=c("0 ug/ul", "5 ug/ul", "7 ug/ul", "9ug/ul", "11 ug/ul", "13 ug/ul"),
                    values=brewer.pal(n=6, name ="YlOrRd")) +
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = 0, vjust = 0.9, size = 6)

h3k27ac_plot

# h3k4me3_plot <-ggplot(data=h3k4me3, aes(x=dose, y=amt_ngmg, fill=dose)) +
#   geom_bar(stat = "summary", fun="mean", width = 0.75, color = 'black',
#            show.legend = T) +
#   theme_minimal() +
#   labs(title = "Amount of Modified Histone Protein Amount (ng/mg)",
#        x = "Dose (ug/ul)",
#        y = "Modified Histone Protein (ng/mg)",
#        color = "Dose") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual((name="Dose"),
#                     labels=c("0 ug/ul", "5 ug/ul", "7 ug/ul", "9ug/ul", "11 ug/ul", "13 ug/ul"),
#                     values=brewer.pal(n=6, name ="YlOrRd"))
# 
# h3k4me3_plot

grid.arrange(h3k27me3_plot, h3k27ac_plot)

# Line graph representing this data
h3k27me3$dose <- as.factor(h3k27me3$dose)
h3k27ac$dose <- as.factor(h3k27ac$dose)

histone_plot <- ggplot(all_histone_protein, aes(x=dose, y=amt_ngmg, group = modification)) +
  geom_smooth(method=loess, aes(color=modification),lty=1,se=F) +
  scale_color_manual(name="Modification",
                     labels=c("H3K27ac","H3K27me3","H3K4me3"),
                     values=brewer.pal(n=3, name ="YlOrRd"))+
  theme_light() +
  labs(title = "Trend of Modified Histone Protein Amount (ng/mg)",
       x = "Dose (ug/ul)",
       y = "Modified Histone Protein (ng/mg)",
       color = "Dose") +
  theme(plot.title = element_text(hjust = 0.5))

histone_plot

### For some reason you need to rerun the code from lines 11 to 31 because the doses get corrupted
# Fit a loess regression model H3K27me3
h3k27me3$dose <- as.numeric(h3k27me3$dose)
fit_h3k27me3 <- loess(amt_ngmg ~ dose, data = h3k27me3)
dose_min_h3k27me3 <- min(h3k27me3$dose)
dose_max_h3k27me3 <- max(h3k27me3$dose)
predicted_min_h3k27me3 <- predict(fit_h3k27me3, newdata = data.frame(dose = dose_min_h3k27me3))
predicted_max_h3k27me3 <- predict(fit_h3k27me3, newdata = data.frame(dose = dose_max_h3k27me3))
slope_h3k27me3 <- (predicted_max_h3k27me3 - predicted_min_h3k27me3) / (dose_max_h3k27me3 - dose_min_h3k27me3)
print(slope_h3k27me3)

h3k27me3_lm <- lm(amt_ngmg~factor(dose), data=h3k27me3)
summary(h3k27me3_lm)
anova_h3k27me3<-aov(h3k27me3_lm)
summary(anova_h3k27me3)

# Fit a loess regression model H3K27me3
h3k27ac$dose <- as.numeric(h3k27ac$dose)
fit_h3k27ac <- loess(amt_ngmg ~ dose, data = h3k27ac)
dose_min_h3k27ac <- min(h3k27ac$dose)
dose_max_h3k27ac <- max(h3k27ac$dose)
predicted_min_h3k27ac <- predict(fit_h3k27ac, newdata = data.frame(dose = dose_min_h3k27ac))
predicted_max_h3k27ac <- predict(fit_h3k27ac, newdata = data.frame(dose = dose_max_h3k27ac))
slope_h3k27ac <- (predicted_max_h3k27ac - predicted_min_h3k27ac) / (dose_max_h3k27ac - dose_min_h3k27ac)
print(slope_h3k27ac)

h3k27ac_lm <- lm(amt_ngmg~factor(dose), data=h3k27ac)
summary(h3k27ac_lm)
anova_h3k27ac<-aov(h3k27ac_lm)
summary(anova_h3k27ac)

# What is the relative percent change of histone modifications to its control?
# h3k27me3 percent change
h3k27me3$dose <- as.factor(h3k27me3$dose)
h3k27ac$dose <- as.factor(h3k27ac$dose)

avg_absorbance_h3k27me3 <- h3k27me3 %>%
  group_by(dose) %>%
  summarise(avg_absorbance = mean(absorbance))

avg_absorbance_h3k27me3$percent_change <- ((((avg_absorbance_h3k27me3$avg_absorbance) - 0.006) / (0.15059717 - 0.006))*100)
avg_absorbance_h3k27me3$modification <- c("h3k27me3")

# h3k27ac percent change
avg_absorbance_h3k27ac <- h3k27ac %>%
  group_by(dose) %>%
  summarise(avg_absorbance = mean(absorbance))

avg_absorbance_h3k27ac$percent_change<- ((((avg_absorbance_h3k27ac$avg_absorbance) - 0.006) / (0.13604167 - 0.006))*100)
avg_absorbance_h3k27ac$modification <- c("h3k27ac")

# Binding all data together
data <- merge(avg_absorbance_h3k27ac, avg_absorbance_h3k27me3, all = TRUE)
data$dose <- as.factor(data$dose)

ggplot(data, aes(x=dose, y=percent_change, group=modification)) +
  geom_point(size = 2, aes(color=modification)) +
  geom_smooth(method=loess, aes(color=modification), se=F, lwd=1.5) +
  theme_light() +
  scale_color_manual((name="Histone"),
                     labels=c("H3K27ac","H3K27me3"),
                     values=brewer.pal(n=3, name ="YlOrRd")) +
  labs(title = "Relative Histone Modification Change to Control (%)",
       x = "Dose (ug/ul)",
       y = "Relative Change (%)",
       color = "Dose") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(aes(yintercept = 100), col='red', lty=2, lwd=1)

