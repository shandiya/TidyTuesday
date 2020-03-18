# load packages
library(tidyverse)
library(ggridges)
library(here)

# get the data
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

# write to csv for future ref
write.csv(office_ratings, here("data", "office_ratings.csv"))

summary(office_ratings)

# add column with characters instead of numbers for seasons (inelegantly)
tidy_ratings <- office_ratings %>% 
  mutate(season_char = case_when(
    season == "1" ~ "One",
    season == "2" ~ "Two",
    season == "3" ~ "Three",
    season == "4" ~ "Four",
    season == "5" ~ "Five",
    season == "6" ~ "Six",
    season == "7" ~ "Seven",
    season == "8" ~ "Eight",
    season == "9" ~ "Nine",
    ))
  
# specify order of y axis
y_order <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine")

# plot
tidy_ratings %>%
  ggplot(aes(x = imdb_rating, y = factor(season_char, level = y_order), fill = season)) +
  geom_density_ridges(
    jittered_points = TRUE, point_size = 1, show.legend = FALSE, alpha = 0.7, point_alpha = 1
    ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "The Office Ratings",
    subtitle = "IMDB ratings across 9 seasons",
    x = "Rating",
    y = "Season"
    ) +
  scale_fill_viridis_c() +
  theme_ridges(grid = FALSE, font_size = 12) +
  ggsave(here("plots", "Office_ratings.png"))
