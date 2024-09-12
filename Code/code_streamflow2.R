library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)

setwd('D:\\Ref.Data\\Data')

station = c('405129')

# Read the CSV file
dat <- read.csv("405219\\405219.streamflow.Raw_Data.csv", header = TRUE)

colnames(dat)

colnames(dat) <- c("Datetime", "Flow", "QC", "Comment")

#dat <- dat %>% select(-c(QC, Comment))

dat$Datetime <- as.POSIXct(dat$Datetime, format = "%Y/%m/%d %H:%M:%S")

#na_count <- sum(is.na(dat$Datetime))
#na_rows <- which(is.na(dat$Datetime))

flow_2023 <- dat %>% filter(year(Datetime) == 2023)

sum(is.na(flow_2023$Datetime))
sum(is.na(flow_2023$Flow))


# Calculate the time difference between consecutive rows
time_diffs <- flow_2023 %>%
  arrange(Datetime) %>%
  mutate(Time_Diff = c(NA, diff(Datetime))) %>%
  filter(!is.na(Time_Diff)) %>%
  summarise(All_15_Minutes = all(Time_Diff == minutes(15))) %>% print()

#Test Write CSV for year 2023
write.csv(flow_2023, "405219//flow_2023_test.csv", row.names = FALSE)

#start_time <- min(flow_2023$Datetime) %>% format("%Y-%m-%d %H:%M:%S")
#end_time <- max(flow_2023$Datetime) %>% format("%Y-%m-%d %H:%M:%S")

all_intervals <- seq.POSIXt(from = as.POSIXct("2023-01-12 00:00:00"), 
                            to = as.POSIXct("2023-12-31 23:45:00"), by = "15 min")

all_intervals_df <- data.frame(Datetime = all_intervals)

complete_data <- merge(all_intervals, flow_2023, by = "Datetime", all.x = TRUE)

#-------------------------------------------------------------------------------

frequency <- "15 minutes"

#We need to check sum and mean for the intervals and determine which one is appropriate

# Create bins for each interval and aggregate data
flow_2023_resampled_sum <- flow_2023 %>%
  # Adjust timestamps to the end of the interval
  mutate(Datetime = ceiling_date(Datetime, unit = frequency)) %>%
  group_by(Datetime) %>%
  summarise(Flow = sum(Flow, na.rm = TRUE))

flow_2023_resampled_mean <- flow_2023 %>%
  # Adjust timestamps to the end of the interval
  mutate(Datetime = ceiling_date(Datetime, unit = frequency)) %>%
  group_by(Datetime) %>%
  summarise(Flow = mean(Flow, na.rm = TRUE))

#here we plot both sum and mean and identify the difference
library(ggplot2)

ggplot() + 
  geom_line(data = flow_2023_resampled_sum, aes(Datetime, Flow), color = "coral") +
  geom_line(data = flow_2023_resampled_mean, aes(Datetime, Flow), color = "steelblue")

#Difference Plot

#We marge both dataframes

merge_2023 <- merge(flow_2023_resampled_sum, flow_2023_resampled_mean, by = "Datetime", 
                    suffixes = c("_sum", "_mean"))

merge_2023$Flow_diff <- merge_2023$Flow_sum - merge_2023$Flow_mean

#Plot the difference 
ggplot(merge_2023, aes(Datetime, Flow_diff))+
  geom_line(color = "purple")+ labs(title = "Flow Difference",
                                    x = "Datetime",
                                    y = "Flow Difference") + theme_minimal()

#We are going to use mean

rm(flow_2023_resampled_mean, flow_2023_resampled_sum, merge_2023)

#------------------------------------------------------------------

#Before re sampling, we need to deal with QC codes

#check NA in flow 

sum(is.na(flow_2023$Flow))

flow_2023 <- flow_2023 %>% select(-c("Comment"))

print(which(flow_2023$QC == 180))

#Continuous color plot
library(ggplot2)
library(viridis)

ggplot() +
  geom_line(data = flow_2023, aes(x = Datetime, y = Flow, color = QC)) +
  labs(x = "", y = "Flow", title = "") +
  viridis::scale_color_viridis("QC") +
  theme_classic()

plot(flow_2023$QC, type = 'l')

unique(flow_2023$QC)
#Not very good, we are going to use discrete color scale for the QC values

# Define custom colors for each QC value
custom_colors <- c("2" = "green", "11" = "lightblue", "15" = "blue", "100" = "orange", "180" = "red")

legend_labels <- c("2" = "Good Quality Data", 
                   "11" = "Data Without Field Adjustment", 
                   "15" = "Minor Adjustment", 
                   "100" = "Irregular Data, Use with Caution", 
                   "180" = "Data not Recorded, Equipment Malfunction.")


library(stringr)

# Wrap legend labels to improve readability
wrapped_labels <- str_wrap(legend_labels, width = 20)  # Adjust width as needed

# Plot with custom color scale, adjustable point size, and custom legend spacing
ggplot(data = flow_2023, aes(x = Datetime, y = Flow, color = as.factor(QC))) +
  geom_point(size = 1) +  # Adjust point size here
  labs(x = "", y = "Flow", title = "Flow vs QC") +
  scale_color_manual(values = custom_colors, labels = wrapped_labels) +  # Apply custom color mapping and wrapped labels
  theme_classic() +
  guides(color = guide_legend(
    override.aes = list(size = 2)  # Increase legend point size
  )) +
  theme(
    legend.text = element_text(size = 10)  # Adjust font size of legend text
  )+ labs(color='Data Quality') 









