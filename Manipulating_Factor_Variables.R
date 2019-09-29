install.packages("tidyverse")
library(tidyverse)

multiple_choice_responses <- read.csv("multiple_choice_responses.csv")

# Changing the order of factor levels

# Print the levels of WorkInternalVsExternalTools.
# Manually reorder the levels from Entirely internal 
# to Entirely external, putting Do not know last.

# Get the levels of WorkInternalVsExternalTools
levels(multiple_choice_responses$WorkInternalVsExternalTools)

# Reorder the levels from internal to external 
mc_responses_reordered <- multiple_choice_responses %>%
  mutate(WorkInternalVsExternalTools = fct_relevel(WorkInternalVsExternalTools,
                                                   "Entirely internal", 
                                                   "More internal than external",
                                                   "Approximately half internal and half external",
                                                   "More external than internal", 
                                                   "Entirely external",
                                                   "Do not know"))


# Using the new dataset, mc_responses_reordered, make a bar plot of the frequency of the responses 
# of WorkInternalVsExternalTools.

# Make a bar plot of the responses
ggplot(mc_responses_reordered, aes(x = WorkInternalVsExternalTools)) + 
  geom_bar() + 
  coord_flip()

# In three mutate calls, change FormalEducation in the following ways:
# Move "I did not complete any formal education past high school" and "Some college/university study without earning a bachelor's degree" to the front.
# Move "I prefer not to answer" to be the last level.
# Move "Doctoral degree" to be the sixth level (after the fifth level).

multiple_choice_responses %>%
  # Move "I did not complete any formal education past high school" and "Some college/university study without earning a bachelor's degree" to the front
  mutate(FormalEducation = fct_relevel(FormalEducation, "I did not complete any formal education past high school", "Some college/university study without earning a bachelor's degree")) %>%
  # Move "I prefer not to answer" to be the last level.
  mutate(FormalEducation = fct_relevel(FormalEducation, "I prefer not to answer", after = Inf)) %>%
  # Move "Doctoral degree" to be after the 5th level
  mutate(FormalEducation = fct_relevel(FormalEducation, "Doctoral degree", after = 5)) %>%
  # Examine the new level order
  pull(FormalEducation) %>%
  levels()

# Renaming a few levels
# Make a bar plot of the frequency of different education levels.
# make a bar plot of the frequency of FormalEducation
ggplot(multiple_choice_responses, aes(FormalEducation)) + 
  geom_bar()

# Now, rename "I did not complete any formal education past high school" 
# to "High school" and "Some college/university study without earning a bachelor's degree" 
# to "Some college".
# Create a new bar plot.

multiple_choice_responses %>%
  # rename levels
  mutate(FormalEducation = fct_recode(FormalEducation, 
                                      "High school" = "I did not complete any formal education past high school", 
                                      "Some college" = "Some college/university study without earning a bachelor's degree")) %>%
  # make a bar plot of FormalEducation
  ggplot(aes(x = FormalEducation)) + 
  geom_bar()

# Manually collapsing levels
# Collapse the levels of CurrentJobTitleSelect into a new variable, grouped_titles.
# Then take grouped_titles and put everything that isn't one of those three grouped 
# titles into "Other Title".
# Finally, get the count of all the grouped titles.

multiple_choice_responses %>%
  # Create new variable, grouped_titles, by collapsing levels in CurrentJobTitleSelect
  mutate(grouped_titles = fct_collapse(CurrentJobTitleSelect, 
                                       "Computer Scientist" = c("Programmer", "Software Developer/Software Engineer"), 
                                       "Researcher" = "Scientist/Researcher", 
                                       "Data Analyst/Scientist/Engineer" = c("DBA/Database Engineer", "Data Scientist", 
                                                                             "Business Analyst", "Data Analyst", 
                                                                             "Data Miner", "Predictive Modeler"))) %>%
  # Turn every title that isn't now one of the grouped_titles into "Other"
  mutate(grouped_titles = fct_other(grouped_titles, 
                                    keep = c("Computer Scientist", 
                                             "Researcher", 
                                             "Data Analyst/Scientist/Engineer"))) %>% 
  # Get a count of the grouped titles
  count(grouped_titles)

# Lumping variables by proportion
# Remove people who didn't select a method.
# 
# Create a new variable, ml_method, from MLMethodNextYearSelect 
# that preserves titles that at least 5% of respondents have and lump the 
# rest as "Other" (the default value).
# 
# Finally, count your new variable, sorted in descending order.

multiple_choice_responses %>%
  # remove NAs of MLMethodNextYearSelect
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # create ml_method, which lumps all those with less than 5% of people into "Other"
  mutate(ml_method = fct_lump(MLMethodNextYearSelect, prop = .05)) %>%
  # count the frequency of your new variable, sorted in descending order
  count(ml_method, sort = T)


# Preserving the most common levels
# Remove people who didn't select a method.
# 
# Create a new variable, ml_method, from MLMethodNextYearSelect that 
# preserves 5 most common titles and lumps the rest as "other method" using the 
# argument other_level.
# 
# Count the frequency of each ml_method, sorting in descending order.

multiple_choice_responses %>%
  # remove NAs 
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # create ml_method, retaining the 5 most common methods and renaming others "other method" 
  mutate(ml_method = fct_lump(MLMethodNextYearSelect, n = 5, other_level = "other method")) %>%
  # count the frequency of your new variable, sorted in descending order
  count(ml_method, sort = T)



