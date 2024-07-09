# Author: Kevin See
# Purpose: clean PTAGIS data with PITcleanr
# Created: 4/27/20
# Last Modified: 3/25/24
# Notes:

# if needed, install development version of packages
# devtools::install_github("KevinSee/PITcleanr@develop")
# devtools::install_github("KevinSee/DABOM@develop")

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
# load configuration and site_df data
load(here('analysis/data/derived_data/site_config.rda'))

# which spawn year are we dealing with?
yr = 2014

for(yr in 2011:2023) {

# load and file biological data
bio_df = read_rds(here('analysis/data/derived_data/Bio_Data_2011_2023.rds')) %>%
  filter(spawn_year == yr)

# any double-tagged fish?
dbl_tag = bio_df %>%
  filter(!is.na(second_pit_tag))

dbl_tag |>
  select(spawn_year,
         contains("tag"),
         contains("pit"),
         contains("date"))

#-----------------------------------------------------------------
# start date is June 1 of previous year
start_date = paste0(yr - 1, '0601')
# when is the last possible observation date?
max_obs_date = paste0(yr, "0531")

# get raw observations from PTAGIS
# These come from running a saved query on the list of tags to be used
ptagis_file = here("analysis/data/raw_data/PTAGIS",
                   paste0("UC_Sthd_", yr, "_CTH.csv"))

# recode the PTAGIS observations of double tagged fish so that the tag code matches the TagID (not TagOther)
ptagis_obs = readCTH(ptagis_file)

# add some observations from Colockum (CLK), a temporary antenna that only operated in some years
if(yr %in% c(2015, 2018) ) {
  clk_obs = read_csv(here('analysis/data/raw_data/WDFW/CLK_observations.csv'),
                     show_col_types = F) %>%
    rename(tag_code = `Tag Code`) %>%
    mutate(event_type_name = "Observation",
           event_site_code_value = 'CLK',
           antenna_id = 'A1',
           antenna_group_configuration_value = 100,
           cth_count = 1) %>%
    mutate(time = if_else(is.na(time),
                          hms::hms(seconds = 0,
                                   minutes = 0,
                                   hours = 12),
                          time)) %>%
    mutate(event_date_time_value = paste(date, time)) %>%
    mutate(across(event_date_time_value,
                  lubridate::mdy_hms)) %>%
    filter(year(event_date_time_value) == yr) %>%
    select(-date, -time) %>%
    # get the origin info for these fish
    left_join(ptagis_obs %>%
                select(tag_code,
                       mark_species_name,
                       mark_rear_type_name) %>%
                distinct())

  ptagis_obs %<>%
    bind_rows(clk_obs)

  rm(clk_obs)
}

# any orphaned or disowned tags?
qcTagHistory(ptagis_obs,
             "PTAGIS",
             ignore_event_vs_release = T)

# deal with double tagged fish, assign all observations to one tag
if(nrow(dbl_tag) > 0) {
  ptagis_obs <-
    ptagis_obs |>
    left_join(dbl_tag %>%
                mutate(fish_id = 1:n()) %>%
                select(fish_id,
                       contains("tag")) %>%
                mutate(use_tag = pit_tag) %>%
                pivot_longer(cols = contains("pit_tag"),
                             names_to = "source",
                             values_to = "tag_code") %>%
                select(tag_code, use_tag) |>
                filter(tag_code != use_tag),
              by = "tag_code") %>%
    rowwise() %>%
    mutate(tag_code = if_else(!is.na(use_tag) &
                                !event_site_code_value %in% c("DISOWN",
                                                              "ORPHAN"),
                              use_tag,
                              tag_code)) %>%
    ungroup() %>%
    select(-use_tag) %>%
    distinct()
}

# compress and process those observations with PITcleanr
prepped_ch = PITcleanr::prepWrapper(cth_file = ptagis_obs,
                                    file_type = "PTAGIS",
                                    configuration = configuration,
                                    parent_child = parent_child %>%
                                      addParentChildNodes(configuration = configuration),
                                    min_obs_date = start_date,
                                    max_obs_date = max_obs_date,
                                    ignore_event_vs_release = F,
                                    filter_orphan_disown_tags = FALSE,
                                    add_tag_detects = T,
                                    save_file = T,
                                    file_name = here('outgoing/PITcleanr', paste0('UC_Steelhead_', yr, '.xlsx')))
                                    # file_name = paste0("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd",
                                    #                    "/inputs",
                                    #                    "/PITcleanr",
                                    #                    "/PITcleanr Initial",
                                    #                    "/UC_Steelhead_",
                                    #                    yr,
                                    #                    ".xlsx"))


# save some stuff
save(parent_child, configuration, start_date, bio_df, prepped_ch,
     file = here('analysis/data/derived_data/PITcleanr',
                 paste0('UC_Steelhead_', yr, '.rda')))


rm(start_date, bio_df, prepped_ch,
   ptagis_file,
   ptagis_obs,
   dbl_tag)
}

#-------------------------------------------
# NEXT STEPS
#-------------------------------------------
# open that Excel file, and filter on the column user_keep_obs, looking for blanks. Fill in each row with TRUE or FALSE, depending on whether that observation should be kept or not. The column auto_keep_obs provides a suggestion, but the biologist's best expert judgment should be used based on detection dates, detection locations before and after, etc.

# which spawn year are we dealing with?
yr = 2023

load(here('analysis/data/derived_data/PITcleanr',
          paste0('UC_Steelhead_', yr, '.rda')))

# read in PITcleanr output that's been reviewed by WDFW biologist
wdfw_df <-
  read_excel(paste0("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/PITcleanr/PITcleanr Final/",
                    "UC_Steelhead_",
                    yr,
                    ".xlsx")) |>
  mutate(across(c(duration,
                  travel_time),
                ~ as.difftime(., units = "mins"))) |>
  filter(!is.na(tag_code))

if(!"user_keep_obs" %in% names(wdfw_df)) {
  wdfw_df <-
    wdfw_df |>
    rename(user_keep_obs = user_keep_obs...16)
}

wdfw_df <-
  wdfw_df |>
  select(any_of(names(prepped_ch)))

if(! identical(n_distinct(prepped_ch$tag_code),
               n_distinct(wdfw_df$tag_code)) ) {
  cat(paste0("PITcleanr tags: ",
             n_distinct(prepped_ch$tag_code),
             "\n",
             "WDFW tags: ",
             n_distinct(wdfw_df$tag_code),
             "\n"))
}

if(!identical(nrow(wdfw_df),
              nrow(prepped_ch))) {
  cat(paste0("PITcleanr rows: ",
             nrow(prepped_ch),
             "\n",
             "WDFW rows: ",
             nrow(wdfw_df),
             "\n"))
}

# check if WDFW choices make sense
filter_obs <-
  wdfw_df |>
  mutate(across(user_keep_obs,
                ~ case_when(is.na(.) ~ auto_keep_obs,
                            .default = .))) |>
  filter(user_keep_obs)

# construct all valid paths
all_paths = buildPaths(addParentChildNodes(parent_child,
                                           configuration))

tag_path = summarizeTagData(filter_obs,
                            bio_df %>%
                              rename(tag_code = pit_tag)) %>%
  select(tag_code, final_node) %>%
  distinct() %>%
  left_join(all_paths,
            by = join_by(final_node == end_loc)) %>%
  rename(tag_path = path)

# check if any user defined keep_obs lead to invalid paths
error_tags = filter_obs %>%
  left_join(tag_path) %>%
  rowwise() %>%
  mutate(node_in_path = str_detect(tag_path, node)) %>%
  ungroup() %>%
  filter(!node_in_path) %>%
  select(tag_code) %>%
  distinct()

nrow(error_tags)
if(nrow(error_tags) > 0) {
  error_tags %>%
    # slice(4) |>
    # left_join(wdfw_df) %>%
    # left_join(prepped_ch) |>
    left_join(filter_obs) |>
    select(tag_code:max_det,
           direction,
           ends_with("obs")) |>
    group_split(tag_code)
  # as.data.frame()
}

# check out tags assigned to JDA
jda_tags <-
  tag_path |>
  filter(final_node == "JDA") |>
  pull(tag_code)

length(jda_tags)

if(length(jda_tags) > 0) {
  wdfw_df |>
    filter(tag_code %in% jda_tags) |>
    select(tag_code:max_det,
           ends_with("keep_obs")) |>
    # filter(auto_keep_obs != user_keep_obs) |>
    group_split(tag_code)
    # as.data.frame()
}

prepped_ch %>%
  select(-user_keep_obs) %>%
  anti_join(wdfw_df %>%
              select(tag_code:max_det,
                     -slot))

wdfw_df |>
  anti_join(prepped_ch |>
              select(tag_code:max_det,
                     -slot))# |>
  # select(tag_code) |>
  # distinct() |>
  # # left_join(prepped_ch) |>
  # left_join(wdfw_df) |>
  # select(tag_code:max_det,
  #        ends_with("keep_obs"))

prepped_ch <-
  prepped_ch %>%
  select(-user_keep_obs) %>%
  left_join(wdfw_df %>%
              select(tag_code:max_det,
                     user_keep_obs),
            by = join_by(tag_code,
                         node,
                         slot,
                         event_type_name,
                         n_dets,
                         min_det,
                         max_det)) |>
  mutate(across(user_keep_obs,
                ~ case_when(is.na(.) ~ auto_keep_obs,
                            .default = .))) |>
  select(all_of(names(prepped_ch)))

save(parent_child, configuration, start_date, bio_df, prepped_ch,
     file = here('analysis/data/derived_data/PITcleanr',
                 paste0('UC_Steelhead_', yr, '.rda')))


#-----------------------------------------------------------------
# tag summaries
#-----------------------------------------------------------------
# use auto_keep_obs for the moment
tag_summ = summarizeTagData(prepped_ch |>
                              mutate(across(user_keep_obs,
                                            ~ if_else(is.na(.),
                                                      auto_keep_obs,
                                                      .))),
                            bio_df %>%
                              rename(tag_code = pit_tag))

# any duplicated tags?
sum(duplicated(tag_summ$tag_code))
tag_summ %>%
  filter(tag_code %in% tag_code[duplicated(tag_code)]) %>%
  as.data.frame()

# where are tags assigned?
janitor::tabyl(tag_summ,
               final_node,
               origin) %>%
  janitor::adorn_totals("both") |>
  arrange(desc(Total))


# preliminary estimate of node efficiency
node_eff = prepped_ch %>%
  mutate(across(user_keep_obs,
                ~ if_else(is.na(.),
                          auto_keep_obs,
                          .))) |>
  filter(user_keep_obs) %>%
  estNodeEff(node_order = buildNodeOrder(addParentChildNodes(parent_child, configuration)))

node_eff %>%
  filter(tags_at_node > 0,
         eff_est < 1)

node_eff %>%
  filter(tags_at_node > 0) |>
  # arrange(desc(eff_se))
  arrange(eff_est)


#-----------------------------------------------------------------
# examine some of the output
#-----------------------------------------------------------------
# which tags have "strange" capture histories?
prepped_ch %>%
  summarise(n_tags = n_distinct(tag_code),
            n_weird = n_distinct(tag_code[direction == "unknown"]),
            n_fix = n_distinct(tag_code[is.na(user_keep_obs)]),
            prop_weird = n_weird / n_tags,
            prop_fix = n_fix / n_tags)

# look at which branch each tag was assigned to for spawning
brnch_df = buildNodeOrder(addParentChildNodes(parent_child, configuration)) %>%
  separate(col = path,
           into = paste("step", 1:max(.$node_order), sep = "_"),
           remove = F) %>%
  mutate(branch_nm = if_else(node == "PRA",
                             "Start",
                             if_else(grepl('LWE', path) | node %in% c("CLK"),
                                     "Wenatchee",
                                     if_else(grepl("ENL", path),
                                             "Entiat",
                                             if_else(grepl("LMR", path),
                                                     "Methow",
                                                     if_else(grepl("OKL", path) | node %in% c("FST"),
                                                             "Okanogan",
                                                             if_else(step_2 != "RIA" & !is.na(step_2),
                                                                     "Downstream",
                                                                     "Mainstem"))))))) %>%
  select(-starts_with("step"))

tag_summ %<>%
  left_join(brnch_df %>%
              select(final_node = node,
                     branch_nm))

# how many tags in each branch?
tag_summ %>%
  janitor::tabyl(branch_nm,
                 origin) %>%
  janitor::adorn_totals("both") |>
  # janitor::adorn_pct_formatting() %>%
  arrange(desc(Total))

# age comp in each branch, by sex
tag_summ %>%
  filter(!is.na(age),
         !is.na(sex)) %>%
  ggplot(aes(x = branch_nm,
             fill = as.ordered(age))) +
  geom_bar(position = position_fill()) +
  facet_wrap(~ sex) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(x = "Branch",
       y = "Percent of Tags",
       fill = "Age")


# look at run timing between branches
tag_summ %>%
  ggplot(aes(x = start_date,
             color = branch_nm,
             fill = branch_nm)) +
  geom_density(alpha = 0.2) +
  theme_bw() +
  scale_color_brewer(palette = 'Set1',
                     name = "Branch") +
  scale_fill_brewer(palette = 'Set1',
                    name = "Branch") +
  labs(x = "Trap Date at Priest Rapids")
