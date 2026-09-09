# Author: Kevin See
# Purpose: clean PTAGIS data with PITcleanr
# Created: 8/28/26
# Last Modified: 9/2/26
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(PITcleanr)
library(tidyverse)
library(lubridate)
library(janitor)
# library(readxl)
library(here)

#-----------------------------------------------------------------
# load configuration and site_df data
load(here('analysis/data/derived_data/site_config.rda'))

# build configuration table (requires internet connection)
org_config = buildConfig(node_assign = "array",
                         array_suffix = "UD")


# what sites have ended, as of latest configuration file?
sites_ended <-
  configuration |>
  filter_out(is.na(end_date)) |>
  slice_max(end_date,
            by = c(site_code,
                   node)) |>
  select(site_code,
         node,
         end_date) |>
  distinct() |>
  anti_join(configuration |>
              filter(is.na(end_date)) |>
              select(site_code) |>
              distinct(),
            by = join_by(site_code))

sites_ended

sites_ended |>
  tabyl(node) |>
  arrange(desc(n)) |>
  adorn_totals() |>
  adorn_pct_formatting()

# for some of these, are there other sites attached to those nodes?
sites_ended |>
  count(node,
        name = "n_gone") |>
  left_join(configuration |>
              filter(is.na(end_date)) |>
              select(site_code,
                     node) |>
              distinct() |>
              count(node,
                    name = "n_exist")) |>
  mutate(across(n_exist,
                ~ replace_na(., 0))) |>
  mutate(n_total = n_gone + n_exist,
         perc_gone = n_gone / n_total) |>
  arrange(desc(n_gone)) |>
  mutate(site_code = str_remove(node, "_U$"),
         across(site_code,
                ~ str_remove(., "_D$"))) |>
  relocate(site_code,
           .before = 0) |>
  left_join(buildNodeOrder(parent_child),
            by = join_by(site_code == node)) |>
  left_join(org_config |>
              select(site_code,
                     rkm) |>
              distinct()) |>
  relocate(rkm,
           .after = site_code) |>
  arrange(rkm)


configuration |>
  filter(str_detect(node, "MDR")) |>
  select(site_code,
         node,
         site_name) |>
  distinct() |>
  left_join(configuration) |>
  slice_max(start_date,
            by = site_code) |>
  select(site_code,
         node,
         site_name,
         rkm,
         site_type,
         start_date,
         end_date) |>
  distinct() |>
  arrange(rkm)



# ENL still exists
# ENA still exists
# ENM has been removed (was joined to ENA previously)
# ENS has been removed (was joined to ENA previously)
# ENF has been removed (could be joined to ENA for past years)

# MRC is still operating, even if some other sites that have been attached to it previously are not


# these sites have been identified to be dropped
drop_sites <-
  c("UWE",
    "ENF",
    "SA0",
    "OKW",
    "PRV")


#-----------------------------------------------------------------
# get raw observations from PTAGIS
# These come from running a saved query on the list of tags to be used
ptagis_obs <-
  tibble(file_nms = list.files(here("analysis/data/raw_data/PTAGIS"))) |>
  mutate(spawn_year = str_extract(file_nms,
                                  "[:digit:]+"),
         across(spawn_year,
                as.numeric)) |>
  select(spawn_year) |>
  mutate(ptagis_dets = map(spawn_year,
                           .f = function(yr) {
                             here("analysis/data/raw_data/PTAGIS",
                                  paste0("UC_Sthd_", yr, "_CTH.csv")) |>
                               readCTH()
                           })) |>
  unnest(ptagis_dets)

#-----------------------------------------------------------------
# extract sites with detections
# across all years
obs_sites <-
  extractSites(ptagis_obs) |>
  left_join(ptagis_obs |>
              summarize(n_tags = n_distinct(tag_code),
                        min_yr = min(spawn_year),
                        max_yr = max(spawn_year),
                        .by = event_site_code_value),
            by = join_by(site_code == event_site_code_value))

# for a specific year
# yr = 2026
yr = max(ptagis_obs$spawn_year)

obs_sites_yr <-
  extractSites(ptagis_obs |>
                 filter(spawn_year == yr)) |>
  left_join(ptagis_obs |>
              filter(spawn_year == yr) |>
              summarize(n_tags = n_distinct(tag_code),
                        .by = event_site_code_value),
            by = join_by(site_code == event_site_code_value))

# quick compression of latest data
comp_obs_yr <-
  ptagis_obs |>
  filter(spawn_year == yr) |>
  compress()

comp_obs_yr |>
  filter(node == "RIS") |>
  select(tag_code) |>
  distinct() |>
  left_join(comp_obs_yr) |>
  summarize(dets = paste(node, collapse = ", "),
            .by = tag_code) |>
  mutate(det_upstrm = str_detect(dets, "RIA")) |>
  as.data.frame()

# which sites are in the configuration file, but have no detections this year?
configuration |>
  filter_out(node %in% c("ICH_U",
                         "JDA",
                         "PRO_U",
                         "JD1_U",
                         "MDR_U")) |>
  select(site_code,
         node,
         rkm,
         site_type,
         site_name,
         ends_with("date"),
         site_description) |>
  distinct() |>
  slice_max(start_date,
            by = c(site_code)) |>
  anti_join(obs_sites_yr |>
              select(site_code,
                     rkm)) |>
  filter_out(is.na(end_date))
  tabyl(node) |>
  arrange(desc(n)) |>
  adorn_pct_formatting()



# sites with detections but not included in the configuration file
miss_sites <-
  # obs_sites |>
  obs_sites_yr |>
  select(-c(latitude,
            longitude)) |>
  left_join(configuration |>
              mutate(node_site = str_remove(node, "_U$"),
                     across(node_site,
                            ~ str_remove(., "_D$"))) |>
              select(site_code,
                     node_site,
                     node) |>
              distinct(),
            relationship = "many-to-many",
            by = join_by(site_code)) |>
  filter(is.na(node)) |>
  arrange(desc(n_tags)) |>
  select(-node) |>
  left_join(org_config |>
              slice_max(start_date,
                        by = c(site_code)) |>
              select(site_code,
                     ends_with("date")) |>
              distinct() |>
              mutate(across(ends_with("date"),
                            as.Date)),
            by = join_by(site_code))


# instream PIT tag antennas
miss_sites |>
  filter(str_detect(site_type, "Instream"))

# other interrogation sites
miss_sites |>
  filter(type == "INT") |>
  filter_out(str_detect(site_type, "Instream")) |>
  select(site_code:site_type,
         n_tags)

# MRR sites
miss_sites |>
  filter(type == "MRR") |>
  select(site_code,
         site_name,
         site_description,
         n_tags)


# look at detections for a particular site
comp_obs_yr |>
  filter(node == "OCW") |>
  select(tag_code) |>
  distinct() |>
  left_join(comp_obs_yr) |>
  summarize(dets = paste(node, collapse = ", "),
            .by = tag_code) |>
  mutate(det_upstrm = str_detect(dets, "RIA")) |>
  as.data.frame()


# sites to add to DABOM
add_sites <-
  c("MTB",
    "LOT",
    "WWB")


#-----------------------------------------------------------------
# all sites in Walla Walla
ques_sites <-
  org_config |>
  filter(str_detect(rkm, "^509")) |>
  select(site_code,
         # node,
         site_name,
         rkm,
         ends_with("date")) |>
  distinct() |>
  slice_max(start_date,
            by = site_code) |>
  mutate(across(ends_with("date"),
                as.Date)) |>
  arrange(rkm) |>
  left_join(configuration |>
              mutate(node_site = str_remove(node, "_U$"),
                     across(node_site,
                            ~ str_remove(., "_D$"))) |>
              select(site_code,
                     node_site) |>
              distinct() |>
              mutate(in_config = T),
            by = join_by(site_code)) |>
  mutate(across(in_config,
                ~ replace_na(., F))) |>
  left_join(obs_sites |>
              select(site_code,
                     n_tags:max_yr),
            by = join_by(site_code))

# sites not in DABOM
ques_sites |>
  filter(!in_config)

# sites with detected tags but not in DABOM
ques_sites |>
  filter(n_tags > 0,
         !in_config)

# sites that no longer operate
ques_sites |>
  filter(!is.na(end_date),
         in_config)

