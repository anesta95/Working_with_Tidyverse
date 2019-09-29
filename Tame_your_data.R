install.packages("tidyverse")
install.packages("janitor")
library(janitor)
library(tidyverse)
library(ggplot2)
# Find format to parse uk_airdate 
parse_date("17 August 2010", format = "%d %B %Y")

# Edit to cast uk_airdate
desserts <- read_csv("desserts.csv", 
                     col_types = cols(
                       uk_airdate = col_date(format = "%d %B %Y")))

# Arrange by descending uk_airdate
desserts %>% 
  arrange(desc(uk_airdate))

# Edit code to fix the parsing error 
desserts <- read_csv("desserts.csv",
                     col_types = cols(
                       uk_airdate = col_date(format = "%d %B %Y"),
                       technical = col_number()
                     ))

# View parsing problems
problems(desserts)

desserts <- read_csv("desserts.csv",
                     col_types = cols(
                       uk_airdate = col_date(format = "%d %B %Y"),
                       technical = col_number()
                     ),
                     na = c("", "NA", "N/A") 
)

problems(desserts)


# Cast result a factor
desserts <- read_csv("desserts.csv", 
                     na = c("", "NA", "N/A"),
                     col_types = cols(
                       uk_airdate = col_date(format = "%d %B %Y"),
                       technical = col_number(),                       
                       result = col_factor(levels = NULL)))

# Glimpse to view
glimpse(desserts)


# Count rows grouping by nut variable
desserts %>% 
  count(signature_nut, sort = TRUE)

# Edit code to recode "no nut" as missing and "filbert" as "hazelnut
desserts_2 <- desserts %>% 
  mutate(signature_nut = recode(signature_nut, "filbert" = "hazelnut", 
                      "no nut" = NA_character_))

# Count rows again 
desserts_2 %>% 
  count(signature_nut, sort = TRUE)


# Create new tech_win binary column & edit to recode tech_win as factor
desserts <- desserts %>% 
  mutate(tech_win = recode_factor(technical, `1` = 1,
                                  .default = 0))

# Count to compare values                      
desserts %>% 
  count(technical == 1, tech_win)

ratings <- read_csv("02.03_messy_ratings.csv")

# For series with 10 episodes, which showed the most growth in viewers from the premiere to the finale? Which showed the least?

ratings <- ratings %>% 
  filter(episodes == 10) %>% 
    group_by(series) %>% 
      mutate(growth = e10_viewers_7day - e1_viewers_7day)

# Recode channel as factor: "Channel 4" (0) or not (1)
ratings <- ratings %>% 
  mutate(bbc = recode_factor(channel, 
                             "Channel 4" = 0, 
                             .default = 1))

# Select to look at variables to plot next
ratings %>% select(series, channel, bbc, growth)


# Make a filled bar chart
ggplot(ratings, aes(x = series, y = growth, fill = bbc)) +
  geom_col()

# Move channel to first column
ratings %>% 
  select(channel, everything())

# Drop 7- and 28-day episode ratings
ratings %>% 
  select(-ends_with("day"))

# Move channel to front and drop 7-/28-day episode ratings
ratings %>% 
  select(channel, everything(), -ends_with("day"))

messy_ratings <- read_csv("02.03_messy_ratings.csv")

# Glimpse to see variable names
glimpse(messy_ratings)


# Reformat to lower camelcase
ratings2 <- messy_ratings %>%
  clean_names(case = "lower_camel")

# Glimpse new tibble
glimpse(ratings2)


# Reformat to snake case
ratings3 <- messy_ratings %>%  
  clean_names("snake")

# Glimpse cleaned names
glimpse(ratings3)

# Select 7-day viewer data by series
viewers_7day <- ratings3 %>%
  select(series, ends_with("7day"))


# Glimpse
glimpse(viewers_7day)

# Adapt code to also rename 7-day viewer data
viewers_7day <- ratings %>% 
  select(series, viewers_7day_ = ends_with("7day"))

# Glimpse
glimpse(viewers_7day)


# Adapt code to drop 28-day columns; keep 7-day in front
viewers_7day <- ratings3 %>% 
  select(viewers_7day_ = ends_with("7day"),
         everything(),
         -ends_with("28day"))

# Glimpse
glimpse(viewers_7day)

# Adapt code to keep original order
viewers_7day <- ratings %>% 
  select(everything(),
         viewers_7day_ = ends_with("7day"),
         -ends_with("28day"))

# Glimpse
glimpse(viewers_7day)
