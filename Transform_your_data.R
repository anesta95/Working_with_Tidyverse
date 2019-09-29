library(tidyverse)
library(ggplot2)
library(lubridate)
bakers <- read_csv("baker_results.csv")

# Create skill variable with 3 levels
bakers_skill <- bakers %>% 
  mutate(skill = case_when(
    star_baker > technical_winner ~ "super_star",
    star_baker < technical_winner ~ "high_tech",
    TRUE ~ "well_rounded"
  ))

# Filter zeroes to examine skill variable
bakers_skill %>% 
  filter(star_baker == 0 & technical_winner == 0) %>% 
  count(skill)

# Add pipe to drop skill = NA
bakers_skill <- bakers %>% 
  mutate(skill = case_when(
    star_baker > technical_winner ~ "super_star",
    star_baker < technical_winner ~ "high_tech",
    star_baker == 0 & technical_winner == 0 ~ NA_character_,
    star_baker == technical_winner  ~ "well_rounded"
  )) %>% 
  drop_na(skill)

# Count bakers by skill
bakers_skill %>% count(skill)


# Cast skill as a factor
bakers <- bakers_skill %>% 
  mutate(skill = as.factor(skill))

# Examine levels
bakers %>% pull(skill) %>% levels()

# Plot counts of bakers by skill, fill by winner
ggplot(bakers, aes(x = skill, fill = as.factor(series_winner))) + geom_bar()

# Edit to reverse x-axis order
ggplot(bakers, aes(x = fct_rev(skill), fill = as.factor(series_winner))) +
  geom_bar()

baker_dates <- bakers %>% 
  select(series, baker, "first_date_appeared_uk"=first_date_appeared, "last_date_appeared_uk"=last_date_appeared, "first_date_appeared_us"=first_date_us, "last_date_appeared_us"=last_date_us)

# Cast last_date_appeared_us as a date
baker_dates_cast <- baker_dates %>% mutate(last_date_appeared_uk = ymd(last_date_appeared_uk))
str(baker_dates_cast)

# Add a line to extract labeled month
baker_dates_cast <- baker_dates %>% 
  mutate(last_date_appeared_uk = ymd(last_date_appeared_uk),
         last_month_uk = month(last_date_appeared_uk, label = TRUE))

# Make bar chart by last month
ggplot(baker_dates_cast, aes(x = last_month_uk)) + geom_bar()

baker_time <- baker_dates %>% mutate(time_on_air = interval(first_date_appeared_uk, last_date_appeared_uk))

# Add a line to create whole months and weeks on air variable
baker_time <- baker_time  %>% 
  mutate(time_on_air = interval(first_date_appeared_uk, last_date_appeared_uk),
         weeks_on_air = time_on_air / weeks(1),
         months_on_air = time_on_air %/% months(1))

bakers_messy <- read_csv("messy_baker_results.csv")

bakers_messy %>% count(position_reached)

# Convert to upper case. Add another mutate to replace "-" with " ". Add another mutate to replace "THIRD PLACE" with "RUNNER UP"and count
bakers_messy <- bakers_messy %>% 
  mutate(position_reached = str_to_upper(position_reached),
         position_reached = str_replace(position_reached, "-", " "),
         position_reached = str_replace(position_reached, "THIRD PLACE", "RUNNER UP"))

# Count rows
bakers_messy %>% count(position_reached)

# Convert to lower case. # Add a line to create new variable called student. 
bakers_messy <- bakers_messy %>% 
  mutate(occupation = str_to_lower(occupation), 
         student = str_detect(occupation, "student"))

# Find all students and examine occupations
bakers_messy %>% filter(student == T) %>% select(baker, occupation, student) 

