# Script to join and summarize the data

library(tidyverse)
library(sf)

in_dir <- "./data/"
out_dir <- "./results/"

# Load helper functions

source("./src/helper_funcs.R")

# Load in raw data

npls <- st_read("./data/npl.shp") %>%
  filter(state == "NC")

sovi <- st_read("./data/sovi.shp") %>%
  filter(st_abbr == "NC") %>%
  mutate(rpl_theme1 = ifelse(rpl_theme1 == -999, NA, rpl_theme1),
         rpl_theme2 = ifelse(rpl_theme2 == -999, NA, rpl_theme2),
         rpl_theme3 = ifelse(rpl_theme3 == -999, NA, rpl_theme3),
         rpl_theme4 = ifelse(rpl_theme4 == -999, NA, rpl_theme4),
         rpl_themes = ifelse(rpl_themes == -999, NA, rpl_themes))

cntys <- st_read("./data/counties.shp") %>%
  filter(statefp == 37)

# Generate random points to test NPL sites against

test_pts <- st_sample(cntys, nrow(npls)) %>%
  st_sf()

# Generate UHC method summary for NPL data

npls_uhc <- generate_uhc(sovi, npls)
test_pts_uhc <- generate_uhc(sovi, test_pts)

# Alternatively generate the mean value of rpl_themes for whole dataset

mean_rpl_themes <- mean(sovi$rpl_themes, na.rm = TRUE)

# Rework data and summarize to examine differences

npls_uhc <- npls_uhc %>%
  mutate(dataset = "npls") %>%
  select(dataset, rpl_themes)

test_pts_uhc <- test_pts_uhc %>%
  mutate(dataset = "test_pts") %>%
  select(dataset, rpl_themes)

t_test_data <- rbind(npls_uhc, test_pts_uhc) %>%
  filter(!is.na(rpl_themes)) %>%
  st_set_geometry(NULL) %>%
  as_data_frame()

# Run a one sided t-test to test the difference between the two

t.test(t_test_data$rpl_themes, mu = mean_rpl_themes)
