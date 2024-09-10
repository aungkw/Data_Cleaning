library(tidyverse)
library(lubridate)

setwd('D:\\Ref.Data\\Data')

station = c('405129')

# Read the CSV file
dat <- read.csv("405219\\405219.rainfall.Raw_Data.csv", header = TRUE)

colnames(dat) <- c("Datetime", "Rain", "QC", "Comment")

dat <- dat %>% select(-c(QC, Comment))

# Convert Datetime to POSIXct
#dat$Datetime <- as.POSIXct(dat$Datetime, format = "%d/%m/%Y %H:%M")

dat$Datetime <- as.POSIXct(dat$Datetime, format = "%Y/%m/%d %H:%M:%S")

# Remove seconds and convert to POSIXct format
#dat$Datetime <- as.POSIXct(format(dat$Datetime, "%Y/%m/%d %H:%M"), format = "%Y/%m/%d %H:%M")

na_count <- sum(is.na(dat$Datetime))
na_rows <- which(is.na(dat$Datetime))

#remove NA row
dat <- dat %>% filter(!is.na(Datetime))

#Check Start Date and End Date of Data
date_range <- range(dat$Datetime)

#for all years

frequency <- "15 minutes"

# Create bins for each interval and aggregate data
dat_resampled <- dat %>%
  # Adjust timestamps to the end of the interval
  mutate(Datetime = ceiling_date(Datetime, unit = frequency)) %>%
  group_by(Datetime) %>%
  summarise(Rain = sum(Rain, na.rm = TRUE))

# Create a sequence of all desired time intervals within the range of the data
start_time <- min(dat_resampled$Datetime)
end_time <- max(dat_resampled$Datetime)

all_intervals <- seq.POSIXt(from = start_time, to = end_time, by = "15 min")

all_intervals_df <- data.frame(Datetime = all_intervals)

final_resampled_data <- merge(all_intervals_df, dat_resampled, by = "Datetime", all.x = TRUE)
final_resampled_data$Rain[is.na(final_resampled_data$Rain)] <- 0

#Check
sum(final_resampled_data$Rain)
sum(dat$Rain)

# Write resampled data to a CSV file
write.csv(final_resampled_data, "405219\\405219_rain_15min.csv", row.names = FALSE)

#------------------------------------------------#

#Calculate annual rainfall
#from Original Data

# Extract year from Datetime
dat$Year <- lubridate::year(dat$Datetime)

# Group by year and calculate total rainfall for each year
annual_rainfall <- dat %>%
  group_by(Year) %>%
  summarise(total_rainfall = sum(Rain, na.rm = TRUE))

# Print the annual rainfall
print(annual_rainfall)

library(ggplot2)

# Assuming 'annual_rainfall' dataframe is already created

# Plot annual rainfall using ggplot2
ggplot(annual_rainfall, aes(x = Year, y = total_rainfall)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Annual Rainfall",
       x = "Year",
       y = "Total Rainfall (mm)") +
  theme_minimal()

#for resampled data

dat1 = final_resampled_data

colnames(dat1) = c("Datetime","Rainfall")

dat1$Year = lubridate::year(dat1$Datetime)

annual_rainfall_1 <- dat1 %>% 
  group_by(Year) %>% 
  summarise(total_rainfall= sum(Rainfall, na.rm = T))

#Merge data

# Merge the annual rainfall data
merged_data <- merge(annual_rainfall, annual_rainfall_1, by = "Year", suffixes = c("_original", "_resampled"))


# Reshape the data for plotting
merged_data_long <- merged_data %>%
  pivot_longer(cols = starts_with("total_rainfall"),
               names_to = "Dataset",
               values_to = "Total_Rainfall")


# Plot the comparison using ggplot2
ggplot(merged_data_long, aes(x = factor(Year), y = Total_Rainfall, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
  labs(title = "Comparison of Annual Rainfall vs Adjusted Rainfall",
       x = "Year",
       y = "Total Rainfall (mm)",
       fill = "Dataset") +
  theme_bw() +
  scale_x_discrete(drop = FALSE)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


# Plot the comparison using ggplot2 with geom_col and a gap between groups
ggplot(merged_data_long, aes(x = factor(Year), y = Total_Rainfall, fill = Dataset)) + 
  geom_col(colour="black",width=0.5,    
           position=position_dodge(0.5)) +
  scale_fill_manual(values=c("#ffcc00ff","#ffffff"), name = "Dataset") + 
  labs(title = "Annual vs 15 min Adjusted Rainfall @ Goulburn River\nStation: 405219", x="Year", y = "Total Rainfall (mm)")+
  theme_minimal() +
  theme(axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        panel.border = element_blank())+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))+ 
  theme(legend.position="bottom")



#---------------Testing for One Year-----------------------##

dat_2001 <- dat %>% filter(format(Datetime, "%Y") == "2001")

frequency <- "15 minutes"

#floor_date, we don't use
#dat_resampled <- dat_2001 %>%
#mutate(Datetime = floor_date(Datetime, unit = frequency)) %>%
#group_by(Datetime) %>%
#summarise(Rain = sum(Rain, na.rm = TRUE))

#use cealing date
# Create bins for each interval and aggregate data
dat_resampled <- dat_2001 %>%
  # Adjust timestamps to the end of the interval
  mutate(Datetime = ceiling_date(Datetime, unit = frequency)) %>%
  group_by(Datetime) %>%
  summarise(Rain = sum(Rain, na.rm = TRUE))

# Create a sequence of all desired time intervals within the range of the data
start_time <- min(dat_resampled$Datetime)
end_time <- max(dat_resampled$Datetime)

all_intervals <- seq.POSIXt(from = start_time, to = end_time, by = "15 min")

all_intervals_df <- data.frame(Datetime = all_intervals)

final_resampled_data <- merge(all_intervals_df, dat_resampled, by = "Datetime", all.x = TRUE)
final_resampled_data$Rain[is.na(final_resampled_data$Rain)] <- 0

#Check
sum(final_resampled_data$Rain)
sum(dat_2001$Rain)

