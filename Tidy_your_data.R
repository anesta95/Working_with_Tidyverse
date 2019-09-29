library(tidyverse)
library(ggplot2)

ratings4 <- read_csv("messy_ratings.csv")
# Plot of episode 1 viewers by series
ggplot(ratings4, aes(x = series, y = e1)) + geom_col()
# Adapt code to plot episode 2 viewers by series
ggplot(ratings4, aes(x = series, y = e2)) +
  geom_col()

tidy_ratings <- ratings4 %>%
  # Gather and convert episode to factor
  gather(key = "episode", value = "viewers_7day", -series, 
         factor_key = TRUE, na.rm = TRUE) %>%
  # Sort in ascending order by series and episode
  arrange(series, episode) %>% 
  # Create new variable using row_number()
  mutate(episode_count = row_number())

# Plot viewers by episode and series
ggplot(tidy_ratings, aes(x = episode_count, 
                         y = viewers_7day, 
                         fill = series)) +
  geom_col()

ratings5 <- read_csv("messy_ratings2.csv")

week_ratings <- ratings5  %>% 
  # Select 7-day viewer ratings
  select(series, ends_with("7day")) %>% 
  # Gather 7-day viewers by episode
  gather(episode, viewers_7day, ends_with("7day"), na.rm = TRUE, factor_key = TRUE)

# Plot 7-day viewers by episode and series
ggplot(week_ratings, aes(x = episode, 
                         y = viewers_7day, 
                         group = series)) +
  geom_line() +
  facet_wrap(~series)


week_ratings <- ratings5  %>% 
  # Select 7-day viewer ratings
  select(series, ends_with("7day")) %>% 
  # Gather 7-day viewers by episode
  gather(episode, viewers_7day, ends_with("7day"), na.rm = TRUE, factor_key = TRUE) %>% 
  # Edit to separate key column and drop extra
  separate(col = episode, c("episode"), extra = "drop") %>% 
  # Edit to parse episode number
  mutate(episode = parse_number(episode))


# Print to view
print(week_ratings)


# Edit your code to color by series and add a theme
ggplot(week_ratings, aes(x = episode, y = viewers_7day, 
                         group = series, color = series)) +
  geom_line() +
  facet_wrap(~series) +
  guides(color = FALSE) +
  theme_minimal()

viewers_millions <- c('2 3 3 2 3 2 3 3 3 3 3 4 4 5 3 4 4 4 4 4 5 5 5 6 6 6 7 6 6 7 7 7 7 9 8 8 9 10 9 10 10 9 10 13 11 11 12 12 12 12 12 11 12 15 13 13 13 13 13 13 13 13 13 15 9 9 8 8 8 8 9 8 9 10')
viewers_millions <- str_split(viewers_millions, " ")
viewers_millions <- as.double(viewers_millions[[1]])

viewers_decimal <- c('.24 .00 .00 .6 .03 .75 .1 .53 .82 .6 .83 .25 .42 .06 .85 .6 .53 .71 .61 .82 .1 .35 .7 .74 .6 .65 .17 .82 .95 .32 .76 .41 .41 .45 .51 .79 .28 .25 .95 .13 .28 .02 .67 .51 .62 .59 .01 .36 .39 .00 .35 .09 .65 .05 .58 .45 .01 .29 .12 .13 .45 .26 .44 .9 .46 .23 .68 .55 .61 .61 .01 .95 .03 .04')
viewers_decimal <- as.double(str_split(viewers_decimal, " ")[[1]])

series <- c('1 1 1 1 1 1 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 7 7 7 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8')
series <- as.double(str_split(series, " ")[[1]])

episode <- c('1 2 3 4 5 6 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10')
episode <- as.double(str_split(episode, " ")[[1]])

ratings6 <- cbind(series, episode, viewers_millions, viewers_decimal)
ratings6 <- as.data.frame(ratings6)
ratings6$viewers_7day <- paste0(ratings6$viewers_millions, ratings6$viewers_decimal)
ratings6 <- ratings6 %>% select(series, episode, viewers_7day)
str(ratings6)
ratings6 <- ratings6 %>% mutate(viewers_7day = parse_number(viewers_7day))
str(ratings6)

# Create tidy data with 7- and 28-day viewers
tidy_ratings_all <- ratings5 %>%
  gather(episode, viewers, ends_with("day"), na.rm = TRUE) %>% 
  separate(episode, into = c("episode", "days")) %>%  
  mutate(episode = parse_number(episode),
         days = parse_number(days)) 

tidy_ratings_all %>% 
  # Count viewers by series and days
  count(series, days, wt = viewers) %>%
  # Adapt to spread counted values
  spread(key = days, value = n, sep = "_")


# Fill in blanks to get premiere/finale data
tidy_ratings_scatter <- ratings4 %>%
  gather(episode, viewers, -series, na.rm = TRUE) %>%
  mutate(episode = parse_number(episode)) %>% 
  group_by(series) %>% 
  filter(episode == 1 | episode == max(episode)) %>% 
  ungroup()

# Recode first/last episodes
first_last <- tidy_ratings_scatter %>% 
  mutate(episode = recode(episode, `1` = "first", .default = "last")) 

# Switch the variables mapping x-axis and color
ggplot(first_last, aes(x = episode, y = viewers, color = series)) +
  geom_point() + 
  geom_line(aes(group = series))

# Switch the variables mapping x-axis and color
ggplot(first_last, aes(x = series, y = viewers, color = episode)) +
  geom_point() + # keep
  geom_line(aes(group = series)) + # keep
  coord_flip() # keep

# Calculate relative increase in viewers
bump_by_series <- first_last %>% 
  spread(episode, viewers) %>%   
  mutate(bump = (last - first) / first)

# Fill in to make bar chart of bumps by series
ggplot(bump_by_series, aes(x = series, y = bump)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) # converts to %