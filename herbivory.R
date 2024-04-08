# Import libraries
library(dplyr)
library(ggsignif)
library(ggplot2)
library(gridExtra)

# Importing data and cleaning data
herbivory_data <- read.csv("/Users/bjork/Downloads/Lab Stuff/all_compiled_herbivory.csv")
colnames(herbivory_data) <- c("date", "time", "total_leaf","consumed_leaf", "percent_consumed", "scale", "time_after", "stressor", "dose", "eat", "ID_number", "replicate")
herbivory_data$unique_id <- paste(herbivory_data$dose, herbivory_data$ID_number, herbivory_data$replicate, sep = '_')
herbivory_data <- data.frame(herbivory_data)

# Group the data by specimen and count the number of observations for each specimen
df_counts <- herbivory_data %>%
  group_by(unique_id) %>%
  summarise(num_obs = n_distinct(time_after))

# Filter the data to keep only specimens with observations at all three time points
df_filtered <- df_counts %>%
  filter(num_obs == 3) %>%
  inner_join(herbivory_data, by = "unique_id")  # Join with the original data to retain matching rows


# Separate data into three data frames based on time points
df_time0 <- df_filtered %>% filter(time_after == "0")
df_time24 <- df_filtered %>% filter(time_after == "24")
df_time72 <- df_filtered %>% filter(time_after == "72")

# Is there a significant change across all doses? ANOVA
anova_0 <- aov(percent_consumed ~ factor(dose), data = df_time0)
summary(anova_0)

anova_24 <- aov(percent_consumed ~ factor(dose), data = df_time24)
summary(anova_24)

anova_72 <- aov(percent_consumed ~ factor(dose), data = df_time72)
summary(anova_72)

# Factoring the doses to improve x-axis representation
df_time0$dose <- as.factor(df_time0$dose)
df_time24$dose <- as.factor(df_time24$dose)
df_time72$dose <- as.factor(df_time72$dose)

# Plot herbivory over time
hr0_plot <- ggplot(data = df_time0, aes(x=dose, y=percent_consumed, fill=dose)) +
  geom_boxplot(alpha=0.7) +
  theme_minimal() + 
  labs(title = "0 Hour Post-Exposure Herbivory",
       x = "Dose (ug/ul)",
       y = "Total Herbivory",
       color = "Dose") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual((name="Dose"),
                    labels=c("0 ug/ul", "5 ug/ul", "7 ug/ul", "9ug/ul", "11 ug/ul", "13 ug/ul"),
                    values=brewer.pal(n=6, name ="YlOrRd")) +
                    geom_signif(comparisons = list(c("0", "5"),
                                                   c("0", "7"),
                                                   c("0", "9"),
                                                   c("0", "11"),
                                                   c("0", "13")),
                                  map_signif_level = TRUE,
                                  y_position = c(18, 20, 22, 24, 26)) +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = 0, vjust = 1, size = 7)

hr0_plot

hr24_plot <- ggplot(data = df_time24, aes(x=dose, y=percent_consumed, fill=dose)) +
  geom_boxplot(alpha=0.7) +
  theme_minimal() + 
  labs(title = "24 Hour Post-Exposure Herbivory",
       x = "Dose (ug/ul)",
       y = "Total Herbivory",
       color = "Dose (ug/ul") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual((name="Dose"),
                    labels=c("0 ug/ul", "5 ug/ul", "7 ug/ul", "9ug/ul", "11 ug/ul", "13 ug/ul"),
                    values=brewer.pal(n=6, name ="YlOrRd")) +
                    geom_signif(comparisons = list(c("0", "5"),
                                                   c("0", "7"),
                                                   c("0", "9"),
                                                   c("0", "11"),
                                                   c("0", "13")),
                                  map_signif_level = TRUE,
                                  y_position = c(19, 21, 23, 25, 27)) +
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = 0, vjust = 1, size = 7)

hr24_plot

hr72_plot <- ggplot(data = df_time72, aes(x=dose, y=percent_consumed, fill=dose)) +
  geom_boxplot(alpha=0.7) +
  theme_minimal() + 
  labs(title = "72 Hour Post-Exposure Herbivory",
       x = "Dose (ug/ul)",
       y = "Total Herbivory",
       color = "Dose (ug/ul)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual((name="Dose"),
                    labels=c("0 ug/ul", "5 ug/ul", "7 ug/ul", "9ug/ul", "11 ug/ul", "13 ug/ul"),
                    values=brewer.pal(n=6, name ="YlOrRd")) +
                    geom_signif(comparisons = list(c("0", "5"),
                                                   c("0", "7"),
                                                   c("0", "9"),
                                                   c("0", "11"),
                                                   c("0", "13")),
                                map_signif_level = TRUE,
                                y_position = c(28, 31, 34, 37, 40)) +
  annotate("text", x = -Inf, y = Inf, label = "C", hjust = 0, vjust = 1, size = 7)

hr72_plot

# Combine graphs into a single plot
combined_plot <- grid.arrange(hr0_plot, hr24_plot, hr72_plot, nrow = 1)  # Arrange graphs in a single row

# Display the combined plot
print(combined_plot)
