# Author: Kevin See
# Purpose: deal with kelts detected at JDA (or other mainstem sites downstream of Priest)
# Created: 12/7/21
# Last Modified: 12/7/21
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(PITcleanr)
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(magrittr)
library(here)

#-----------------------------------------------------------------
# set year
yr = 2016


# load PITcleanr prepped data
load(here('analysis/data/derived_data/PITcleanr',
          paste0('UC_Steelhead_', yr, '.rda')))


# use auto_keep_obs for the moment
tag_summ = summarizeTagData(prepped_ch %>%
                              mutate(user_keep_obs = auto_keep_obs),
                            bio_df |>
                              rename(tag_code = pit_tag))

# find all tags with a spawning node of JDA
jda_tags = tag_summ %>%
  filter(final_node == "JDA") %>%
  select(tag_code)

jda_tags %>%
  left_join(prepped_ch) %>%
  group_by(tag_code) %>%
  summarise(n_nodes = n_distinct(node[node != "PRA"]),
            .groups = "drop") %>%
  # filter(n_nodes == 2) %>%
  left_join(prepped_ch) %>%
  group_by(tag_code) %>%
  summarise(tag_detects = paste(node,
                                collapse = " ")) %>%
  xtabs(~ tag_detects, .)
