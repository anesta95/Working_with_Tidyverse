install.packages("tidyverse")
library(tidyverse)

multiple_choice_responses <- read.csv("multiple_choice_responses.csv")

# Grouping and reshaping similar columns
# Select only the columns with LearningPlatformUsefulness in the name.
# Change the data from wide to long format with two columns, learning_platform and usefulness.
# Remove rows where usefulness is NA.
# Remove "LearningPlatformUsefulness" from each string in learning_platform.

learning_platform_usefulness <- multiple_choice_responses %>%
  # select columns with LearningPlatformUsefulness in title
  select(contains("LearningPlatformUsefulness")) %>%
  # change data from wide to long
  gather(learning_platform, usefulness) %>%
  # remove rows where usefulness is NA
  filter(!is.na(usefulness)) %>%
  # remove "LearningPlatformUsefulness" from each string in learning_platform 
  mutate(learning_platform = str_remove(learning_platform, "LearningPlatformUsefulness"))

# Summarizing Data
# Use count() to change the dataset to have one row per learning_platform 
# usefulness pair with a column that is the number of entries with that pairing.

# Use add_count() to create a column with the total number of answers to that learning_platform.

# Create a new column, perc, that's the percent of people giving a certain answer for that question. 
# Save everything as a new dataset, perc_useful_platform.

# For each learning platform, create a line graph with usefulness on the x-axis and percentage of 
# responses within the learning platforms on the y-axis.

perc_useful_platform <- learning_platform_usefulness %>%
  # change dataset to one row per learning_platform usefulness pair with number of entries for each
  count(learning_platform, usefulness) %>%
  # use add_count to create column with total number of answers for that learning_platform
  add_count(learning_platform, wt = n) %>%
  # create a new column, perc, that is the percentage of people giving that response for that learning_platform
  mutate(perc = n / nn)

# create a line graph for each question with usefulness on x-axis and percentage of responses on y
ggplot(perc_useful_platform, aes(x = usefulness, y = perc, group = learning_platform)) + 
  geom_line() + 
  facet_wrap(~ learning_platform)

# Creating an initial plot
# Using the dataset learning_platform_usefulness, change usefulness to equal 
# 0 if someone answered "Not Useful" and 1 otherwise.

# Group the data by each platform.
# Create a new column, avg_usefulness, the mean usefulness of each platform.
# Save the result as a new dataset, usefulness_by_platform

usefulness_by_platform <- learning_platform_usefulness %>%
  # If usefulness is "Not Useful", make 0, else 1 
  mutate(usefulness = if_else(usefulness == "Not Useful", 0, 1)) %>%
  # Group by learning platform 
  group_by(learning_platform) %>%
  # Summarize the mean usefulness for each platform
  summarize(avg_usefulness = mean(usefulness))

# Make a scatter plot of average usefulness by learning platform 
ggplot(usefulness_by_platform, aes(x = learning_platform, y = avg_usefulness)) + 
  geom_point()

# Editing plot text
# Change the x-axis text to be rotated 90 degrees.
# Change the x-axis label to "Learning Platform" 
# and y-axis label to "Percent finding at least somewhat useful".
# Change the y-axis scale to be a percentage.

ggplot(usefulness_by_platform, aes(x = learning_platform, y = avg_usefulness)) + 
  geom_point() + 
  # rotate x-axis text by 90 degrees
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  # rename y and x axis labels
  labs(x = "Learning Platform", y = "Percent finding at least somewhat useful") + 
  # change y axis scale to percentage
  scale_y_continuous(labels = scales::percent)

# Reordering graphs
# Order learning_platform in the graph by avg_usefulness so that, 
# from left to right, it goes from highest usefulness to lowest.

usefulness_by_platform %>%
  # reorder learning_platform by avg_usefulness
  mutate(learning_platform = fct_reorder(learning_platform, avg_usefulness)) %>%
  # reverse the order of learning_platform
  mutate(learning_platform = fct_rev(learning_platform)) %>%
  ggplot(aes(x = learning_platform, y = avg_usefulness)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Learning Platform", y = "Percent finding at least somewhat useful") + 
  scale_y_continuous(labels = scales::percent)

# case_when() with single variable
# First, check the min and max of the variable Age, removing NAs.

# Check the min age
min(multiple_choice_responses$Age, na.rm = T)
# Check the max age
max(multiple_choice_responses$Age, na.rm = T)

# Filter for rows where Age is between 10 and 90.
# Use case_when() to create a new column, generation.
# Get a count of the number of people in each generation.

multiple_choice_responses %>%
  # Filter for rows where Age is between 10 and 90
  filter(between(Age, 10, 90)) %>%
  # Create the generation variable based on age
  mutate(generation = case_when(
    between(Age, 10, 22) ~ "Gen Z", 
    between(Age, 23, 37) ~ "Gen Y", 
    between(Age, 38, 52) ~ "Gen X", 
    between(Age, 53, 71) ~ "Baby Boomer", 
    between(Age, 72, 90) ~ "Silent"
  )) %>%
  # Get a count of how many answers in each generation
  count(generation)

# case_when from multiple columns
# Create a new variable, job_identity, based on their 
# current job title and whether they fully identify as a data scientist.

str(multiple_choice_responses)

multiple_choice_responses %>%
  # Filter out people who selected Data Scientist as their Job Title
  filter(CurrentJobTitleSelect != "Data Scientist") %>%
  # Create a new variable, job_identity
  mutate(job_identity = case_when(
    CurrentJobTitleSelect == "Data Analyst" & 
      DataScienceIdentitySelect == "Yes" ~ "DS analysts", 
    CurrentJobTitleSelect == "Data Analyst" & 
      DataScienceIdentitySelect %in% c("No", "Sort of (Explain more)") ~ "NDS analyst", 
    CurrentJobTitleSelect != "Data Analyst" & 
      DataScienceIdentitySelect == "Yes" ~ "DS non-analysts", 
    TRUE ~ "NDS non analysts")) %>%
  # Get the average job satisfaction by job_identity, removing NAs
  group_by(job_identity) %>%
  summarize(avg_js = mean(JobSatisfaction, na.rm = T))


