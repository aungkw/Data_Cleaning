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

dat <- dat %>% select(-c(QC, Comment))

dat$Datetime <- as.POSIXct(dat$Datetime, format = "%Y/%m/%d %H:%M:%S")

na_count <- sum(is.na(dat$Datetime))
na_rows <- which(is.na(dat$Datetime))

#print(dat[c(na_rows[1] - 1, na_rows[1], na_rows[1] + 1), ])

#--------------------
dat1 <- dat[c(na_rows[1] - 1, na_rows[1], na_rows[1] + 1), ]

# Interpolate missing Datetime values and then interpolate Flow values
data_interpolated <- dat1 %>%
  mutate(
    Datetime = zoo::na.approx(as.numeric(Datetime), na.rm = FALSE) %>% 
      as.POSIXct(origin = "1970-01-01")  # Convert numeric back to POSIXct
  ) %>%
  mutate(
    Flow = zoo::na.approx(Flow, na.rm = FALSE)  # Interpolate missing Flow values
  )

#--------------Loop

dat0 = dat

dat0 <- dat0 %>% mutate(
  Datetime = zoo::na.approx(as.numeric(Datetime), na.rm = FALSE) %>% 
    as.POSIXct(origin = "1970-01-01")  # Convert numeric back to POSIXct
  ) %>% mutate(
    Flow = zoo::na.approx(Flow, na.rm = FALSE)  # Interpolate missing Flow values
  )

na_count0 <- sum(is.na(dat0$Datetime))
na_rows0 <- which(is.na(dat0$Datetime))

i = 71
print(dat[max(1, na_rows[i] - 5):min(nrow(dat), na_rows[i] + 5), ])
print(dat0[max(1, na_rows[i] - 5):min(nrow(dat0), na_rows[i] + 5), ])



#remove NA row
#dat <- dat %>% filter(!is.na(Datetime))

#Check Start Date and End Date of Data
date_range <- range(dat0$Datetime)

# Filter data for the years 2010 to 2023
resample_dat_2010_2023 <- dat0 %>%
  filter(year(Datetime) >= 2011 & year(Datetime) <= 2012)

resample_2023 <- dat0 %>%
  filter(year(Datetime) == 2023)

# Calculate the time difference between consecutive rows
time_diffs <- resample_2023 %>%
  arrange(Datetime) %>%
  mutate(Time_Diff = c(NA, diff(Datetime))) %>%
  filter(!is.na(Time_Diff)) %>%
  summarise(All_15_Minutes = all(Time_Diff == minutes(15)))

# Print the result
print(time_diffs)

plot(resample_2023, type = "l")

y2023_0 <- dat0 %>% filter(year(Datetime) == 2023)
y2023 <- dat %>% filter(year(Datetime) == 2010)

# Plot the data
plot(y2023_0, type = 'l')
plot(y2023, type = 'l', col = "red")


#
start_datetime <- as.POSIXct("2023-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
end_datetime <- as.POSIXct("2023-12-31 12:00:00", format = "%Y-%m-%d %H:%M:%S")

complete_time_series <- seq.POSIXt(from = start_datetime, to = end_datetime, by = "1 min")

# Convert to data frame
complete_data <- data.frame(Datetime = complete_time_series)

# Merge with original data to fill in missing values with zeros
complete_data <- merge(complete_data, y2023_0, by = "Datetime", all.x = TRUE)

complete_data$Flow[is.na(complete_data$Flow)] <- 0

resampled_data <- complete_data %>%
  group_by(interval = cut(Datetime, breaks = "15 min")) %>%
  summarise(Flow = sum(Flow))

total_flow = sum(y2023_0$Flow)
resample_flow = sum(resampled_data$Flow)




