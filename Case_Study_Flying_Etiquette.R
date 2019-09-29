library(tidyverse)
flying_etiquette <- read.csv("flying_etiquette.csv")

colnames(flying_etiquette) <- str_replace_all(colnames(flying_etiquette), "\\.", " ")

# Changing characters to factors
# Change all character columns into factor columns.
# Remove people who responded "Never" to a question asking if they have flown before.

flying_etiquette %>%
  # Change characters to factors
  mutate_if(is.character, as.factor) %>%
  # Filter out those who have never flown on a plane
  filter("How often do you travel by plane." != "Never")

# Tidying data
# Select columns where "rude" is in the column name.
# Change the dataset from "wide" to "long", with the 
# variable names in a column called "response_var" and the values in a column called "value."

gathered_data <- flying_etiquette %>%
  mutate_if(is.character, as.factor) %>%
  filter("How often do you travel by plane " != "Never") %>%
  # Select columns containing "rude"
  select(contains("rude")) %>%
  # Change format from wide to long
  gather(response_var, value)

# Cleaning up strings
# Use str_remove to remove everything before and including "rude to " 
# (with the space at the end) in the response_var column.
# Use str_remove to remove "on a plane" from the response_var column.

gathered_data %>%
  # Remove everything before and including "rude to " (with that space at the end!)
  mutate(response_var = str_remove(response_var, ".*rude to ")) %>%
  # Remove "on a plane"
  mutate(response_var = str_remove(response_var, "on a plane"))


gathered_data$value[gathered_data$value == ""] <- NA

dichotimized_data <- gathered_data %>%
  mutate(response_var = str_replace(response_var, '.*rude to ', '')) %>%
  mutate(response_var = str_replace(response_var, 'on a plane', '')) %>%
  # Remove rows that are NA in the value column
  filter(!is.na(value)) %>%
  # Dichotomize the value variable to make a new variable, rude
  mutate(rude = if_else(value %in% c('No, not rude at all', 'No, not at all rude'), 0, 1))

# Summarizing data
# Right now, our data is still in the format of one row per person per question. 
# But for us to graph it, we'll want to change that so each row is a question 
# with the summary information about the response to that question.

# Summarize the data set into two columns, the question 
# (i.e. response_var), and a new column, perc_rude, 
# the mean of the rude column for each question.
# 
# Save it as rude_behaviors and then view your new dataset.

rude_behaviors <- gathered_data %>%
  mutate(response_var = str_replace(response_var, '.*rude to ', '')) %>%
  mutate(response_var = str_replace(response_var, 'on.a.plane', '')) %>%
  # Remove rows that are NA in the value column
  filter(!is.na(value)) %>%
  mutate(rude = if_else(value %in% c("No, not rude at all", "No, not at all rude"), 0, 1)) %>%
  # Group by response_var
  group_by(response_var) %>%
  # Create perc_rude, the percent considering each behavior rude
  summarize(perc_rude = mean(rude))

head(rude_behaviors)

# Creating an initial plot
# Order response_var by perc_rude
# Make a bar plot of response_var by perc_rude. Save it as initial_plot.

initial_plot <- rude_behaviors %>%
  # reorder response_var by perc_rude
  mutate(response_var = fct_reorder(response_var, perc_rude)) %>%
  # make a bar plot of perc_rude by response_var
  ggplot(aes(x = response_var, y = perc_rude)) + 
  geom_col()


# View your plot
initial_plot

titled_plot <- initial_plot + 
  # Add the title, subtitle, and caption
  labs(title = "Hell Is Other People In A Pressurized Metal Tube",
       subtitle = "Percentage of 874 air-passenger respondents who said action is very or somewhat rude",
       caption = "Source: SurveyMonkey Audience", 
       # Remove the x- and y-axis labels
       x = "", 
       y = "") 

titled_plot

flipped_plot <- titled_plot + 
  # Flip the axes
  coord_flip() + 
  # Remove the x-axis ticks and labels
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

flipped_plot + 
  # Apply percent() to perc_rude to label above the bar with the perc value
  geom_text(aes(label = scales::percent(perc_rude), 
                y = perc_rude + .03), 
            position = position_dodge(0.9),
            vjust = 1)

