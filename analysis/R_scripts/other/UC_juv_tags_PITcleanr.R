# Author: Kevin See
# Purpose: clean PTAGIS data with PITcleanr
# Created: 4/27/20
# Last Modified: 11/25/25
# Notes: Using tags from fish tagged as juveniles in the UC

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

# reset all nodes downstream of John Day to be the site code
config_all <-
  configuration |>
  mutate(across(node,
                ~ case_when(. == "JDA" ~ site_code,
                            .default = .)),
         across(node,
                ~ case_when(rkm == "234" &
                              site_type_name == "Adult Fishway" ~ "BON",
                            rkm == "234" &
                              site_type_name == "Juvenile Fish Bypass Facility" ~ "B2J",
                            .default = .)))


# # load and file biological data
# bio_df = read_rds(here('analysis/data/derived_data/Bio_Data_2011_2025.rds')) %>%
#   filter(spawn_year == yr)
#
# # any double-tagged fish?
# dbl_tag = bio_df %>%
#   filter(!is.na(second_pit_tag))
#
# dbl_tag |>
#   select(spawn_year,
#          contains("tag"),
#          contains("pit"),
#          contains("date"))

#-----------------------------------------------------------------
# get raw observations from PTAGIS
# These come from running a saved query on the list of tags to be used
ptagis_file = here("analysis",
                   "data",
                   "raw_data",
                   "CCPUD",
                   "UC_juv_tags_CTH.csv")

# recode the PTAGIS observations of double tagged fish so that the tag code matches the TagID (not TagOther)
ptagis_obs = readCTH(ptagis_file)

# how many tags?
n_distinct(ptagis_obs$tag_code)

# any orphaned or disowned tags?
qcTagHistory(ptagis_obs,
             "PTAGIS",
             ignore_event_vs_release = T)

# pull out mark and release site information
mark_release <-
  ptagis_obs |>
  select(tag_code,
         starts_with("mark_site"),
         starts_with("release_site")) |>
  distinct()



#------------------------------------
# compress detections
comp_obs <-
  ptagis_obs |>
  # add release information as distinct detection
  bind_rows(ptagis_obs |>
              filter(event_type_name == "Mark") |>
              select(tag_code,
                     event_site_code_value = event_release_site_code_code,
                     event_date_time_value = event_release_date_time_value,
                     mark_species_name,
                     mark_rear_type_name) |>
              distinct() |>
              mutate(event_type_name = "Release",
                     antenna_group_configuration_value = 0,
                     cth_count = 1)) |>
  arrange(tag_code,
          event_date_time_value) |>
  compress(configuration = config_all,
           ignore_event_vs_release = F)

# add the correct release site code back
# some of them are grouped with arrays for carcass recoveries
comp_obs |>
  left_join(mark_release |>
              select(tag_code,
                     release_site_code_value)) |>
  mutate(across(node,
                ~ case_when(event_type_name == "Release" &
                              node != release_site_code_value ~ release_site_code_value,
                            .default = .)))

fish_marks <-
  comp_obs |>
  filter(event_type_name %in% c("Mark",
                                "Release")) |>
  select(tag_code,
         event_type_name,
         node,
         min_det) |>
  distinct() |>
  group_by(tag_code) |>
  filter(min_det == max(min_det)) |>
  ungroup() |>
  select(tag_code,
         mark_node = node,
         mark_date = min_det)

fish_bon <-
  comp_obs |>
  filter(node == "BON") |>
  select(tag_code,
         bon_det = min_det,
         travel_time) |>
  distinct() |>
  # identify potential spawn year
  mutate(spawn_year = year(bon_det) + 1) |>
  left_join(fish_marks |>
              select(tag_code,
                     mark_date) |>
              distinct(),
            by = join_by(tag_code)) |>
  mutate(bon_time = difftime(bon_det,
                             mark_date,
                             units = units(comp_obs$travel_time))) |>
  filter(as.numeric(bon_time) > 364 * (60 * 24)) |>
  filter(as.numeric(travel_time) > 364 * (60 * 24)) |>
  group_by(tag_code,
           spawn_year) |>
  filter(bon_det == min(bon_det)) |>
  ungroup() |>
  mutate(rep_spwn = if_else(tag_code %in% tag_code[duplicated(tag_code)],
                            T, F)) |>
  select(tag_code,
         spawn_year,
         bon_det,
         travel_time,
         bon_time,
         rep_spwn)

# these fish never made it to the ocean
comp_obs |>
  anti_join(fish_bon |>
              select(tag_code) |>
              distinct(),
            by = join_by(tag_code))

# these fish are adult steelhead, and should be included in the analysis
comp_keep <-
  comp_obs |>
  inner_join(fish_bon |>
               select(tag_code,
                      spawn_year,
                      bon_det),
             by = join_by(tag_code))

# compress and process those observations with PITcleanr
# keep only the adult steelhead tags
ptagis_keep <-
  ptagis_obs |>
  filter(tag_code %in% unique(comp_keep$tag_code))

prepped_ch <-
  PITcleanr::prepWrapper(cth_file = ptagis_keep,
                         file_type = "PTAGIS",
                         configuration = configuration,
                         parent_child = parent_child %>%
                           addParentChildNodes(configuration = configuration),
                         # min_obs_date = start_date,
                         # max_obs_date = max_obs_date,
                         ignore_event_vs_release = F,
                         filter_orphan_disown_tags = FALSE,
                         add_tag_detects = T,
                         save_file = F) |>
  relocate(ends_with("keep_obs"),
           .after = max_det)

# Sort through some tags detected at JDA (or downstream)
# try to determine which are kelt migrations where we should keep other upstream detections,
# and which are the only detections after Priest
jda_tags <-
  prepped_ch |>
  filter(node == "JDA",
         direction == "unknown") |>
  select(tag_code) |>
  distinct() |>
  left_join(prepped_ch,
            by = join_by(tag_code)) |>
  group_by(tag_code) |>
  summarize(n_dets = n_distinct(node[!node %in% c("PRA", "JDA")]),
            jda_date = max(min_det[node == "JDA"]),
            jda_travel = max(travel_time[node == "JDA"]),
            max_jda_slot = max(slot[node == "JDA"]),
            .groups = "drop") |>
  arrange(n_dets,
          jda_travel)
units(jda_tags$jda_travel) <- "days"

# when were the JDA detections?
jda_tags |>
  arrange(jda_date)

# filter out the JDA detection and re-apply filterDetections
jda_prepped <-
  jda_tags |>
  select(tag_code,
         max_jda_slot) |>
  left_join(prepped_ch,
            by = join_by(tag_code)) |>
  # filter out the last time a tag was detected at JDA
  filter(slot != max_jda_slot) |>
  select(tag_code:start_date,
         tag_detects,
         -max_jda_slot) |>
  filterDetections(parent_child = addParentChildNodes(parent_child,
                                                      configuration)) |>
  select(all_of(names(prepped_ch)))


prepped_ch <-
  prepped_ch |>
  left_join(jda_prepped |>
              rename(new_auto = auto_keep_obs,
                     new_user = user_keep_obs)) |>
  group_by(tag_code) |>
  mutate(need_fix = case_when(sum(is.na(user_keep_obs)) > 0 ~ T,
                              sum(is.na(user_keep_obs)) == 0 ~ F,
                              .default = NA)) |>
  mutate(across(auto_keep_obs,
                ~ case_when(tag_code %in% jda_tags$tag_code &
                              node != "JDA" ~ new_auto,
                            tag_code %in% jda_tags$tag_code &
                              node == "JDA" ~ FALSE,
                            .default = .)),
         across(user_keep_obs,
                ~ case_when(tag_code %in% jda_tags$tag_code &
                              node != "JDA" ~ new_user,
                            tag_code %in% jda_tags$tag_code &
                              !need_fix &
                              node == "JDA" ~ FALSE,
                            tag_code %in% jda_tags$tag_code &
                              need_fix &
                              node == "JDA" &
                              !is.na(new_user) ~ new_user,
                            tag_code %in% jda_tags$tag_code &
                              need_fix &
                              node == "JDA" &
                              is.na(new_user) ~ FALSE,
                            .default = .))) |>
  ungroup() |>
  select(all_of(names(prepped_ch)))


# # add some information back
# prepped_mark_info <-
#   prepped_ch |>
#   left_join(comp_obs |>
#               left_join(mark_release |>
#                           select(tag_code,
#                                  release_site_code_value),
#                         by = join_by(tag_code)) |>
#               mutate(across(node,
#                             ~ case_when(event_type_name == "Release" &
#                                           node != release_site_code_value ~ release_site_code_value,
#                                         .default = .))) |>
#               filter(event_type_name %in% c("Mark",
#                                             "Release")) |>
#               select(tag_code,
#                      event_type_name,
#                      node,
#                      min_det) |>
#               distinct() |>
#               mutate(across(event_type_name,
#                             str_to_lower)) |>
#               rename(dt = min_det) |>
#               pivot_wider(names_from = event_type_name,
#                           values_from = c(node,
#                                           dt),
#                           names_glue = "{event_type_name}_{.value}",
#                           names_vary = "slowest"),
#             by = join_by(tag_code)) |>
#   left_join(fish_bon |>
#               select(tag_code,
#                      spawn_year,
#                      bon_det),
#             by = join_by(tag_code)) |>
#   relocate(ends_with("keep_obs"),
#            .after = max_det) |>
#   relocate(spawn_year,
#            .after = tag_code)
#
# prepped_mark_info |>
#   filter(tag_code == prepped_mark_info$tag_code[1]) |>
#   as.data.frame()

# use mark and release location and dates as "biological" data for tag summary
tag_summ <-
  prepped_ch |>
  mutate(across(user_keep_obs,
                ~ case_when(is.na(.) ~ auto_keep_obs,
                            .default = .))) |>
  filter(user_keep_obs) |>
  summarizeTagData(bio_data = comp_obs |>
                     left_join(mark_release |>
                                 select(tag_code,
                                        release_site_code_value),
                               by = join_by(tag_code)) |>
                     mutate(across(node,
                                   ~ case_when(event_type_name == "Release" &
                                                 node != release_site_code_value ~ release_site_code_value,
                                               .default = .))) |>
                     filter(event_type_name %in% c("Mark",
                                                   "Release")) |>
                     select(tag_code,
                            event_type_name,
                            node,
                            min_det) |>
                     distinct() |>
                     mutate(across(event_type_name,
                                   str_to_lower)) |>
                     rename(dt = min_det) |>
                     pivot_wider(names_from = event_type_name,
                                 values_from = c(node,
                                                 dt),
                                 names_glue = "{event_type_name}_{.value}",
                                 names_vary = "slowest"))

# # look at which branch each tag was assigned to for spawning
# brnch_df = buildNodeOrder(addParentChildNodes(parent_child, configuration)) %>%
#   separate(col = path,
#            into = paste("step", 1:max(.$node_order), sep = "_"),
#            remove = F) %>%
#   mutate(spwn_pop = case_when(node == "PRA" ~ "Start",
#                                str_detect(path, "LWE") | node %in% c("CLK") ~ "Wenatchee",
#                                str_detect(path, "ENL") ~ "Entiat",
#                                str_detect(path, "LMR") ~ "Methow",
#                                str_detect(path, "OKL") | node %in% c("FST") ~ "Okanogan",
#                                step_2 != "RIA" & !is.na(step_2) ~ "Downstream",
#                                .default = "Mainstem")) |>
#   select(-starts_with("step"))
#
# tag_summ <-
#   tag_summ |>
#   left_join(brnch_df %>%
#               select(node,
#                      spwn_pop),
#             by = join_by(final_node == node))

#----------------------------------------------------
# save output
#----------------------------------------------------

# where should output be save?
# file_path <-
#   here("analysis",
#        "data",
#        "raw_data",
#        "CCPUD")

file_path <-
  paste0("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd",
         "/inputs",
         "/PITcleanr",
         "/PITcleanr Initial")

# write to Excel file
list(PITcleanr = prepped_ch,
     "Tag Summary" = tag_summ) |>
  writexl::write_xlsx(path = paste0(file_path,
                                    "/UC_Sthd_Juv_Tags.xlsx"))

#-----------------------------------------------------------
# examples of fish detections needing to be looked at
prepped_ch |>
  filter(is.na(user_keep_obs)) |>
  select(tag_code) |>
  distinct() |>
  slice_sample(n = 1) |>
  left_join(prepped_ch) |>
  select(tag_code,
         node:max_det,
         direction,
         auto_keep_obs)

# how many fish need to be examined?
prepped_ch |>
  group_by(tag_code) |>
  summarize(weird = if_else(sum(direction == "unknown", na.rm = T) > 0, T, F),
            fix = if_else(sum(is.na(user_keep_obs)) > 0, T, F),
            .groups = "drop") |>
  summarize(n_tags = n_distinct(tag_code),
            n_weird = sum(weird),
            n_fix = sum(fix),
            perc_weird = n_weird / n_tags,
            perc_fix = n_fix / n_tags)


prepped_ch |>
  group_by(tag_code) |>
  summarize(weird = if_else(sum(direction == "unknown") > 0, T, F),
            fix = if_else(sum(is.na(user_keep_obs)) > 0, T, F),
            .groups = "drop") |>
  filter(fix,
         !weird) |>
  select(tag_code) |>
  slice_sample(n = 1) |>
  left_join(prepped_ch) |>
  select(tag_code,
         node:max_det,
         direction,
         contains("keep_obs"))

# final_loc <-
#   prepped_ch |>
#   mutate(across(user_keep_obs,
#                 ~ case_when(is.na(.) ~ auto_keep_obs,
#                             .default = .))) |>
#   estimateFinalLoc() |>
#   left_join(parent_child |>
#               addParentChildNodes(configuration = configuration) |>
#               buildPaths() |>
#               rename(spwn_path = path),
#             by = join_by(final_node == end_loc))
#
# test <-
#   prepped_ch |>
#   filter(is.na(user_keep_obs)) |>
#   # select(tag_code:min_det,
#   #        direction,
#   #        contains("keep_obs")) |>
#   left_join(final_loc |>
#               select(tag_code,
#                      spwn_path)) |>
#   mutate(keep = case_when(auto_keep_obs ~ T,
#                           !auto_keep_obs &
#                             str_detect(spwn_path, node) ~ T,
#                           .default = F)) |>
#   group_by(tag_code) |>
#   mutate(n_rows = n(),
#          n_keep = sum(keep)) |>
#   ungroup() |>
#   mutate(across(user_keep_obs,
#                 ~ case_when(n_rows == n_keep ~ auto_keep_obs,
#                             .default = .))) |>
#   select(all_of(names(prepped_ch)))
#   # filter(n_rows != n_keep)
#   filter(tag_code == "3DD.003D4FB5F3") |>
#   select(-spwn_path) |>
#   as.data.frame()



#-------------------------------------------
# NEXT STEPS
#-------------------------------------------
# open that Excel file, and filter on the column user_keep_obs, looking for blanks. Fill in each row with TRUE or FALSE, depending on whether that observation should be kept or not. The column auto_keep_obs provides a suggestion, but the biologist's best expert judgment should be used based on detection dates, detection locations before and after, etc.

# which spawn year are we dealing with?
yr = 2025

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
  filter(!is.na(tag_code)) |>
  filter(tag_code %in% unique(prepped_ch$tag_code))

if(!"user_keep_obs" %in% names(wdfw_df)) {
  wdfw_df <-
    wdfw_df |>
    rename(user_keep_obs = user_keep_obs...16)
}

wdfw_df <-
  wdfw_df |>
  select(any_of(names(prepped_ch)))


identical(dim(prepped_ch),
          dim(wdfw_df))

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

# # pull in calls from Colville Tribes for Okanogan fish
# okl_df <-
#   read_excel(paste0("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/PITcleanr/PITcleanr Worksheet Files/",
#                     "UC_Steelhead_",
#                     yr,
#                     "_OKL.xlsx")) |>
#   mutate(across(c(duration,
#                   travel_time),
#                 ~ as.difftime(., units = "mins"))) |>
#   filter(!is.na(tag_code)) |>
#   filter(tag_code %in% unique(prepped_ch$tag_code)) |>
#   select(any_of(names(prepped_ch)))
#
# # compare WDFW and Colville calls
# comp_det <-
#   okl_df |>
#   select(tag_code) |>
#   distinct() |>
#   left_join(prepped_ch) |>
#   left_join(wdfw_df |>
#               select(tag_code,
#                      node,
#                      slot,
#                      wdfw_keep = user_keep_obs)) |>
#   left_join(okl_df |>
#               select(tag_code,
#                      node,
#                      slot,
#                      okl_keep = user_keep_obs))
#
# # ignoring PRA (sometimes marked FALSE for some reason), examine different calls
# comp_det |>
#   filter(node != "PRA") |>
#   filter(wdfw_keep != okl_keep) |>
#   select(tag_code) |>
#   distinct() |>
#   slice(1) |>
#   left_join(comp_det) |>
#   select(tag_code,
#          node,
#          min_det,
#          direction,
#          contains("keep")) |>
#   as.data.frame()

# fix WDFW file when necessary, and then re-read it back in

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

tag_path <-
  estimateFinalLoc(filter_obs) |>
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
tag_summ <-
  summarizeTagData(prepped_ch |>
                     mutate(across(user_keep_obs,
                                   ~ case_when(is.na(.) ~ auto_keep_obs,
                                               .default = .))),
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
