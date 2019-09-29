library(tidyverse)
library(ggplot2)
load(file = "ilo_hourly_compensation.RData")
load(file = "ilo_working_hours.RData")

# Filter ilo_data to retain the years 1996 and 2006
ilo_data <- ilo_data %>%
  filter(year == "1996" | year == "2006")

# Again, you save the plot object into a variable so you can save typing later on
ilo_plot <- ggplot(ilo_data, aes(x = working_hours, y = hourly_compensation)) +
  geom_point() +
  labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017"
  ) +
  # Add facets here
  facet_grid(. ~ year)

ilo_plot


# For a starter, let's look at what you did before: adding various theme calls to your plot object
ilo_plot +
  theme_minimal() +
  theme(
    text = element_text(family = "Bookman", color = "gray25"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  )

# Define your own theme function below
theme_ilo <- function() {
  theme_minimal() +
    theme(text = element_text(family = "Bookman", color = "gray25"),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(color = "gray30"),
          plot.background = element_rect(fill = "gray95"),
          plot.margin = unit(c(5, 10, 5, 10), units = "mm")
    )
}

ilo_plot + theme_ilo()


# Apply your theme function (dont't forget to call it with parentheses!)
ilo_plot <- ilo_plot +
  theme_ilo()

# Examine ilo_plot
ilo_plot

ilo_plot +
  # Add another theme call
  theme(
    # Change the background fill and color
    strip.background = element_rect(fill = "gray60", color = "gray95"),
    # Change the color of the text
    strip.text = element_text(color = "white")
  )


# Create the dot plot
ggplot(ilo_data) + geom_path(aes(x = working_hours, y = country))

# Add arrows to the lines in the plot
ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
            # Add an arrow to each path
            arrow = arrow(length = unit(1.5, "mm"), type = "closed"))

# Add some labels to each country
ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
            arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  # Add a geom_text() geometry
  geom_text(
    aes(x = working_hours,
        y = country,
        label = round(working_hours, 1))
  )

# x-axis is now obsolote and can be removed in the remainder of this chapter.

# The fct_reorder function
# Let's say you add median as third argument to the fct_reorder() function. What does it do?

# The median of all values in the second argument (for each factor level) 
# is taken for reordering the factor.

# Reordering the elements in the plot
library(forcats)

# Reorder country factor levels
ilo_data <- ilo_data %>%
  # Arrange data frame
  arrange(year) %>%
  # Reorder countries by working hours in 2006
  mutate(country = fct_reorder(country,
                               working_hours,
                               last))

# Plot again
ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
            arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  geom_text(
    aes(x = working_hours,
        y = country,
        label = round(working_hours, 1))
  )

# Correct ugly label positions
# Save plot into an object for reuse
ilo_dot_plot <- ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
            arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  # Specify the hjust aesthetic with a conditional value
  geom_text(
    aes(x = working_hours,
        y = country,
        label = round(working_hours, 1),
        hjust = ifelse(year == "2006", 1.4, -0.4)
    ),
    # Change the appearance of the text
    size = 3,
    family = "Bookman",
    color = "gray25"
  )

ilo_dot_plot

# A new problem: The labels on the very left and very right overlap with the margins of
# the plot.

# Change the viewport so labels don't overlap with plot border
# Reuse ilo_dot_plot
ilo_dot_plot <- ilo_dot_plot +
  # Add labels to the plot
  labs(
    x = "Working hours per week",
    y = "Country",
    title = "People work less in 2006 compared to 1996",
    subtitle = "Working hours in European countries, development since 1996",
    caption = "Data source: ILO, 2017"
  ) +
  # Apply your theme
  theme_ilo() +
  # Change the viewport
  coord_cartesian(xlim = c(25, 41))

# View the plot
ilo_dot_plot

# Compute temporary data set for optimal label placement
median_working_hours <- ilo_data %>%
  group_by(country) %>%
  summarize(median_working_hours_per_country = median(working_hours)) %>%
  ungroup()

# Have a look at the structure of this data set
str(median_working_hours)

ilo_dot_plot +
  # Add label for country
  geom_text(data = median_working_hours,
            aes(y = country,
                x = median_working_hours_per_country,
                label = country),
            vjust = 2,
            family = "Bookman",
            color = "gray25") +
  # Remove axes and grids
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    # Also, let's reduce the font size of the subtitle
    plot.subtitle = element_text(size = 9)
  )
