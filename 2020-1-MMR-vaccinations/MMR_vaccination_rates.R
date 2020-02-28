# title: MMR_vaccination_rates
# author: Shandiya Balasubramaniam
# date: 28 February 2020
# description: MMR vaccination rates across US schools in 3207 cities with different enrolment size

# load packages
library(tidyverse)
library(here)
library(ggthemes)
library(extrafont)

# load data file
measlesVac <- read_csv(here("Data", "measles.csv"))

glimpse(measlesVac)

# remove weird values and create new columns for 95% threshold and means by city 
clean_MMR <- measlesVac %>% 
  filter(mmr > 0) %>% 
  drop_na(enroll) %>% 
  mutate(threshold = ifelse(mmr > 95, "pass", "fail")) %>% 
  group_by(city) %>% 
  mutate(city_mean = mean(mmr))
    
summary(clean_MMR)

n_distinct(clean_MMR$city)
 
# plotting time!   
P <- clean_MMR %>% 
  ggplot(aes(x = enroll, y = mmr, color = threshold)) + 
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("pass" = "#3C9AB2", "fail" = "#F22300")) +
  geom_hline(yintercept = 95, linetype = "longdash", color = "darkgrey")
  
P +
  labs(
    title = "Measles, Mumps and Rubella (MMR) immunisation:",
    subtitle = "Mean immunisation rates across US schools in 3207 cities",
    caption = "Source: The Wall Street Journal",
    x = "School enrolment",
    y = "MMR immunisation rate (%)") +
  annotate(
    geom = "text",
    x = 3500,
    y = 90, 
    label = "95% threshold required for herd immunity",
    hjust = "left") +
  theme_tufte(base_family = "GillSans") +
  theme(legend.position = "none") + 
  ggsave(here("Plots", "MMR_vaccination_rates.png"))
