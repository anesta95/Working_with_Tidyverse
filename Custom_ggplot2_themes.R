library(tidyverse)
library(ggplot2)
load(file = "ilo_hourly_compensation.RData")
load(file = "ilo_working_hours.RData")
str(ilo_hourly_compensation)
str(ilo_working_hours)

# Join the two data sets together
# Combine both ILO data frames ilo_hourly_compensation and ilo_working_hours 
# using the inner_join() function of dplyr.
# Join both data frames by the variables "country" and "year".

# Join both data frames
ilo_data <- ilo_hourly_compensation %>%
  inner_join(ilo_working_hours, by = c("country", "year"))

# Count the resulting rows
ilo_data  %>% 
  count()

# Examine ilo_data
ilo_data


# Turn year and country into a factor
ilo_data <- ilo_data %>%
  mutate(year = as.factor(as.numeric(year)),
         country = as.factor(country))

europe <- c("Finland France Italy Norway Spain Sweden Switzerland UnitedKingdom Belgium Ireland Luxembourg Portugal Netherlands Germany Hungary Austria Czechia")
european_countries <- str_split(europe, " ")
european_countries <- unlist(european_countries)
# Examine the European countries vector
european_countries

# Only retain European countries
ilo_data <- ilo_data %>%
  filter(country %in% european_countries)

# Examine the structure of ilo_data
str(ilo_data)

# Group and summarize the data
ilo_data %>%
  group_by(year) %>%
  summarize(mean_hourly_compensation = mean(hourly_compensation),
            mean_working_hours = mean(working_hours))

# Filter for 2006
plot_data <- ilo_data %>%
  filter(year == "2006")

# Create the scatter plot
ggplot(plot_data) +
  geom_point(aes(x = working_hours, y = hourly_compensation))

# Create the plot
ggplot(plot_data) +
  geom_point(aes(x = working_hours, y = hourly_compensation)) +
  # Add labels
  labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017"
  )

# Save your current plot into a variable: ilo_plot
ilo_plot <- ggplot(plot_data) +
  geom_point(aes(x = working_hours, y = hourly_compensation)) +
  labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017"
  )

# Try out theme_minimal
ilo_plot +
  theme_minimal()

# Try out any other possible theme function
ilo_plot +
  theme_dark()

ilo_plot <- ilo_plot +
  theme_minimal() +
  # Customize the "minimal" theme with another custom "theme" call
  theme(
    text = element_text(family = "Bookman"),
    title = element_text(color = "gray25"),
    plot.caption = element_text(color = "gray30"),
    plot.subtitle = element_text(size = 12)
  )

# Render the plot object
ilo_plot

ilo_plot +
  # "theme" calls can be stacked upon each other, so this is already the third call of "theme"
  theme(
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  )

