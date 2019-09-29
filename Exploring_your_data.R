install.packages("dplyr")
install.packages("readr")
install.packages("skimr")
install.packages("ggplot2")
library(ggplot2)
# Load readr
library(readr)

# Create bakeoff but skip first row
#bakeoff <- read_csv("bakeoff.csv", skip = 1) not done because dataset has headers in first row
bakeoff <- read_csv("bakeoff.csv")

# Print bakeoff
bakeoff

# Load dplyr
library(dplyr)

# Filter rows where showstopper is UNKNOWN 
bakeoff %>% 
  filter(showstopper == "UNKNOWN")

# Edit to add list of missing values
bakeoff <- read_csv("bakeoff.csv",
                    na = c("", "NA", "UNKNOWN"))

# Filter rows where showstopper is NA 
bakeoff %>% filter(is.na(showstopper))

bakeoff %>% arrange(us_airdate) %>% glimpse()
#The first show episode to air in the US was on 12/28/2014

# Load skimr
library(skimr)

# Edit to filter, group by, and skim
bakeoff %>% 
  filter(!is.na(us_season)) %>% 
  group_by(us_season)  %>% 
  skim()

bakeoff %>% skim() %>% summary()
# View distinct results
bakeoff %>% distinct(result)
# Count rows for each result
bakeoff %>% count(result)

# Count whether or not star baker
bakeoff %>% 
  count(result == "SB")

# Count the number of rows by series and episode
bakeoff %>% count(series, episode)

# Add second count by series
bakeoff %>% 
  count(series, episode) %>% count(series)

# Count the number of rows by series and baker
bakers_by_series <- bakeoff %>% count(series, baker)


# Print to view
bakers_by_series

# Count again by series
bakers_by_series %>% 
  count(series)

# Count again by baker
bakers_by_series %>% count(baker, sort = T)

ggplot(bakeoff, aes(x = episode)) + 
  geom_bar() + 
  facet_wrap(~series)

