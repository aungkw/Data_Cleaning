library(tidyverse)

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


# Specify the range manually from start and end of original rainfall data

start_datetime <- as.POSIXct("2001-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
end_datetime <- as.POSIXct("2023-12-31 12:00:00", format = "%Y-%m-%d %H:%M:%S")

#start_datetime <- as.POSIXct(date_range[1], format = "%Y-%m-%d %H:%M:%S")
#end_datetime <- as.POSIXct(date_range[2], format = "%Y-%m-%d %H:%M:%S")

# Create a complete time series with 1 minute intervals,
#cannot do larger 15 min intervals because rainfall data have irregular increment.
#so choose the finest resolution 1min and later resample to 15 min to avoid error.
complete_time_series <- seq.POSIXt(from = start_datetime, to = end_datetime, by = "1 min")

# Convert to data frame
complete_data <- data.frame(Datetime = complete_time_series)

# Merge with original data to fill in missing values with zeros
complete_data <- merge(complete_data, dat, by = "Datetime", all.x = TRUE)

# Replace NA values with zeros
complete_data$Rain[is.na(complete_data$Rain)] <- 0

# Resample the data to 15-minute intervals and sum the rainfall
resampled_data <- complete_data %>%
  group_by(interval = cut(Datetime, breaks = "15 min")) %>%
  summarise(total_rainfall = sum(Rain))

#Check Data
total_rainfall = sum(dat$Rain)
resample_rainfall = sum(resampled_data$total_rainfall)

# Write resampled data to a CSV file
write.csv(resampled_data, "rainfall_resampled_15min.csv", row.names = FALSE)


#Calculate annual rainfall
#from Original Data

# Extract year from Datetime
dat$Year <- lubridate::year(dat$Datetime)

# Group by year and calculate total rainfall for each year
annual_rainfall <- dat %>%
  group_by(Year) %>%
  summarise(total_rainfall = sum(Rainfall, na.rm = TRUE))

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

dat1 = resampled_data

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








###Archive Code Not Working.

#Try 1

library(tidyverse)
library(dplyr)

setwd('C:\\Users\\akkya\\Desktop\\New folder (2)')

dat = read.csv("405219_Rainfall.csv", header = T)

dat$Datetime = as.POSIXct(dat$Datetime, format = "%d/%m/%Y %H:%M")

resampled_data <- dat %>%
  group_by(interval = cut(Datetime, breaks = "15 min")) %>%
  summarise(total_rainfall = sum(Rainfall, na.rm = TRUE))

total_rainfall = sum(dat$Rainfall)
resample_rainfall = sum(resampled_data$total_rainfall)

write.csv(resampled_data, "Rainfall_resampled_15min.csv", row.names = FALSE)

#Results have major problems in time intervals such as 

#30/03/2023 8:15
#30/03/2023 8:30
#30/03/2023 15:15
#31/03/2023 1:15
#31/03/2023 7:15

#skipping of time and date. 
#Aim, to get constant increment of 15 mins and if no rainfall value, it need to return zero.


