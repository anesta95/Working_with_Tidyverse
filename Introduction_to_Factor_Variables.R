install.packages("tidyverse")
library(tidyverse)

multiple_choice_responses <- read.csv("multiple_choice_responses.csv")

# Print out the dataset
multiple_choice_responses 

# Check if CurrentJobTitleSelect is a factor
is.factor(multiple_choice_responses$CurrentJobTitleSelect)

# Change all the character columns to factors
responses_as_factors <- multiple_choice_responses %>%
  mutate_if(is.character, as.factor)

number_of_levels <- responses_as_factors %>%
  # apply the function nlevels to each column
  summarise_all(nlevels) %>%
  # change the dataset from wide to long
  gather(variable, num_levels)

# There are more handy functions - summarise_at() and mutate_at(). 
# These let you select columns by name-based select helpers.

# Select the 3 rows with the highest number of levels
number_of_levels %>%
  top_n(3, num_levels)

number_of_levels %>%
  # filter for where variable equals CurrentJobTitleSelect
  filter(variable == "CurrentJobTitleSelect") %>%
  # pull num_levels
  pull(num_levels)


responses_as_factors %>%
  # pull CurrentJobTitleSelect
  pull(CurrentJobTitleSelect) %>%
  # get the values of the levels
  levels()


# Make a bar plot
ggplot(multiple_choice_responses, aes(EmployerIndustry)) + 
  geom_bar() + 
  # flip the coordinates
  coord_flip()

# Make a bar plot
ggplot(multiple_choice_responses, aes(fct_rev(fct_infreq(EmployerIndustry)))) + 
  geom_bar() + 
  # flip the coordinates
  coord_flip()


multiple_choice_responses %>%
  # remove NAs
  filter(!is.na(EmployerIndustry) & !is.na(Age)) %>%
  # get mean_age by EmployerIndustry
  group_by(EmployerIndustry) %>%
  summarize(mean_age = mean(Age)) %>%
  # reorder EmployerIndustry by mean_age 
  mutate(EmployerIndustry = fct_reorder(EmployerIndustry, mean_age)) %>%
  # make a scatterplot of EmployerIndustry by mean_age
  ggplot(aes(x = EmployerIndustry, y = mean_age)) + 
  geom_point() + 
  coord_flip()
