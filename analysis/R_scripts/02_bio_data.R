# Author: Kevin See
# Purpose: create tag lists to feed to PTAGIS query
# Created: 8/15/2023
# Last Modified: 3/15/2024
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(PITcleanr)
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(magrittr)
library(writexl)
library(here)

#-----------------------------------------------------------------
# uses a complete tag history of marks/recaptures at PRD or PRDLD1
sthd_tags <-
  read_csv(here("analysis/data/raw_data",
                "tagging_recapture",
                "PriestRapids_Mark_Recapture_2011_2024.csv"),
                # "PriestRapids_Sthd_SY2011_present.csv"),
           show_col_types = F) |>
  clean_names() |>
  mutate(across(contains("_date_mm"),
                mdy),
         across(contains("date_time"),
                mdy_hms)) |>
  mutate(spawn_year = case_when(!is.na(event_release_date_mmddyyyy) &
                                  month(event_release_date_mmddyyyy) < 6 ~ year(event_release_date_mmddyyyy),
                                !is.na(event_release_date_mmddyyyy) &
                                  month(event_release_date_mmddyyyy) >= 6 ~ year(event_release_date_mmddyyyy) + 1,
                                is.na(event_release_date_mmddyyyy) &
                                  month(event_date_mmddyyyy) < 6 ~ year(event_date_mmddyyyy),
                                is.na(event_release_date_mmddyyyy) &
                                  month(event_date_mmddyyyy) >= 6 ~ year(event_date_mmddyyyy) + 1,
                                .default = NA_real_)) |>
  # filter out one tagging file that is not a WDFW project
  filter(event_file_name != "CLD14105.WA1") |>
  # grab only tags from the adult ladder
  filter(event_capture_method_code == "LADDER")

# remove tags that are spawning in a future year
sthd_tags <-
  sthd_tags |>
  filter(spawn_year <= year(today()))

# pull out MRR data about all PIT tags from those MRR files
all_tags <-
  sthd_tags |>
  select(spawn_year,
         event_file_name) |>
  distinct() |>
  arrange(spawn_year,
          event_file_name) |>
  # filter out one tagging file that is not a WDFW project
  filter(event_file_name != "CLD14105.WA1") |>
  mutate(tag_file = map(event_file_name,
                        .f = function(x) {
                          out <-
                            tryCatch(queryMRRDataFile(x),
                                     error =
                                       function(cond) {
                                         message(paste("Error with file", x))
                                         message("Error message:")
                                         message(cond)
                                         return(NULL)
                                       },
                                     warning =
                                       function(cond) {
                                         message(paste("Warning with file", x))
                                         message("Warning message:")
                                         message(cond)
                                         return(NULL)
                                       })
                          if("spawn_year" %in% names(out)) {
                            out <- out |>
                              select(-spawn_year)
                          }

                          return(out)
                        },
                        .progress = T)) |>
  unnest(tag_file) |>
  mutate(year = if_else(month(event_date) < 6,
                        year(event_date),
                        year(event_date) + 1)) |>
  relocate(year,
           .before = 0) |>
  distinct() |>
  arrange(year,
          event_date) |>
  # # pull adult out steelhead tags
  # filter(life_stage == "Adult",
  #        str_detect(species_run_rear_type, "^3"),
  #        # ignore tags identified as rainbow trout
  #        str_detect(species_run_rear_type, "^30", negate = T),
  #        # ignore strange tag numbers
  #        str_detect(pit_tag, "\\.\\.\\.", negate = T)) |>
  # fix a few metrics
  mutate(across(poh,
                as.numeric)) |>
  # assign sex based on conditional comments
  mutate(sex = case_when(str_detect(conditional_comments, "FE") ~ "Female",
                         str_detect(conditional_comments, "MA") ~ "Male",
                         .default = NA_character_)) |>
  # determine CWT and ad-clip status from conditional comments
  mutate(cwt = if_else(str_detect(conditional_comments, "CP") |
                         str_detect(conditional_comments, "CW"),
                       T, F),
         ad_clip = case_when(str_detect(conditional_comments, "AD") ~ T,
                             str_detect(conditional_comments, "AI") ~ F,
                             .default = FALSE)) |>
  # correct any lengths that were mistakenly entered as cm, convert to mm
  mutate(across(length,
                ~ if_else(. < 100,
                          . * 10,
                          .)))

# are all tags from first file accounted for?
sthd_tags$tag_code[!sthd_tags$tag_code %in% all_tags$pit_tag]
sthd_tags$tag_code[!(sthd_tags$tag_code %in% all_tags$pit_tag |
                       sthd_tags$tag_code %in% na.omit(all_tags$second_pit_tag))]


# what tags are not in the first file, but appear to be adult steelhead?
extra_tags <-
  all_tags |>
  filter(spawn_year %in% unique(sthd_tags$spawn_year)) |>
  filter(!(pit_tag %in% sthd_tags$tag_code |
             second_pit_tag %in% sthd_tags$tag_code)) |>
  filter(life_stage == "Adult",
         str_detect(species_run_rear_type, "^3"),
         # ignore tags identified as rainbow trout
         str_detect(species_run_rear_type, "^30", negate = T),
         # ignore strange tag numbers
         str_detect(pit_tag, "\\.\\.\\.", negate = T))

extra_tags |>
  tabyl(spawn_year,
        event_type) |>
  adorn_totals()
# some are recoveries (i.e. died at Priest)
# the rest are orphan or disowned tags (https://www.ptagis.org/FAQ#11); missing mark information
extra_tags |>
  filter(event_type != "Recovery") |>
  select(spawn_year,
         pit_tag,
         event_date,
         release_date,
         event_type,
         contains("comments")) |>
  distinct()

extra_tags |>
  filter(event_type != "Recovery") |>
  select(spawn_year,
         pit_tag,
         event_file_name) |>
  distinct()


# pull out data for tags from first file (sthd_tags)
# and a few additional tags from extra_tags
tagging_df <-
  all_tags |>
  filter(pit_tag %in% sthd_tags$tag_code |
           second_pit_tag %in% sthd_tags$tag_code |
           pit_tag %in% extra_tags$pit_tag[extra_tags$event_type != "Recovery"]) |>
  # put a few filters on
  filter(life_stage == "Adult",                                # filter for adults
         str_detect(species_run_rear_type, "^3"),              # filter for O. mykiss
         str_detect(species_run_rear_type, "^30", negate = T), # ignore tags identified as rainbow trout
         str_detect(pit_tag, "\\.\\.\\.", negate = T))         # ignore strange tag numbers



#------------------------------------------------
# # some second PIT tags only appear in text comments
# second_tags <-
#   tagging_df |>
#   filter(str_detect(text_comments, coll("same as", ignore_case = T))) |>
#   mutate(text_code = str_split(text_comments, coll("same as", ignore_case = T), simplify = T)[,2],
#          across(text_code,
#                 ~ str_remove(., "\\#")),
#          across(text_code,
#                 ~ str_remove(., "SD$")),
#          across(text_code,
#                 str_squish),
#          across(text_code,
#                 ~ str_remove(., " ")),
#          across(text_code,
#                 str_to_upper),
#          across(text_code,
#                 ~ if_else(str_detect(text_code, "^3", negate = T),
#                           paste0("3D9.", .),
#                           .))) |>
#   filter(second_pit_tag != text_code) |>
#   select(spawn_year,
#          pit_tag,
#          second_pit_tag,
#          text_comments,
#          text_code)
#
# second_tags
# # the two tags left were recorded incorrectly in the comments (neither of those codes exist in PTAGIS).
# # The second tag recorded in PTAGIS now is correct


# pull out certain columns to use going forward
bio_df <-
  tagging_df |>
  # determine origin
  mutate(origin = str_extract(species_run_rear_type, "[:alpha:]$")) |>
  # round off the hours/minutes/seconds
  mutate(across(ends_with("_date"),
                ~ floor_date(., unit = "days"))) |>
  # filter out any recovery mortalities
  filter(event_type %in% c("Mark",
                           "Recapture")) |>
  select(spawn_year,
         event_file_name,
         pit_tag,
         second_pit_tag,
         event_date,
         release_date,
         event_type,
         species_run_rear_type,
         origin,
         sex,
         cwt,
         ad_clip,
         length,
         poh,
         conditional_comments,
         text_comments,
         scale_id) |>
  mutate(across(release_date,
                ~ if_else(is.na(.),
                          event_date,
                          .)))
# use release date as the trap date


# deal with fish with second PIT tags, reduce to one row / fish
two_tag_fish <-
  bio_df |>
  # filter(pit_tag %in% na.omit(second_pit_tag)) |>
  filter(!is.na(second_pit_tag)) |>
  select(spawn_year,
         release_date,
         pit_tag,
         second_pit_tag,
         event_type) |>
  arrange(release_date,
          event_type,
          pit_tag) |>
  mutate(fish_id = NA_real_)

# assign an ID number to each fish (same pair of tags)
id_num = 1
for(i in 1:nrow(two_tag_fish)) {
  if(!is.na(two_tag_fish$fish_id[i])) {
    next
  } else {

  two_tag_fish$fish_id[two_tag_fish$second_pit_tag == two_tag_fish$pit_tag[i] |
                 two_tag_fish$pit_tag == two_tag_fish$pit_tag[i]] = id_num
  id_num = id_num + 1
  }
}

# which record to keep for each fish?
two_tag_keep <-
  two_tag_fish |>
  nest(.by = fish_id) |>
  mutate(keep_data = map(data,
                         .f = function(x) {
                           if(sum(x$event_type == "Mark") > 0) {
                             keep_row <-
                               x |>
                               filter(event_type == "Mark")
                           } else {
                             keep_row = x
                           }

                           if(nrow(keep_row) > 1) {
                             keep_row <-
                               keep_row |>
                               filter(release_date == min(release_date))
                           }
                           if(nrow(keep_row) > 1) {
                             keep_row <-
                               keep_row |>
                               slice(1)
                           }
                           return(keep_row)
                         })) |>
  unnest(keep_data) |>
  select(-fish_id,
         -data)

# reduce records to one row per fish (with 2nd tag listed as second PIT tag)
bio_df <-
  bio_df |>
  anti_join(two_tag_fish) |>
  bind_rows(two_tag_keep |>
              left_join(bio_df)) |>
  arrange(spawn_year,
          release_date,
          pit_tag)

sum(bio_df$second_pit_tag %in% bio_df$pit_tag)



bio_df |>
  tabyl(spawn_year, ad_clip) |>
  adorn_totals(where = "col")

bio_df |>
  tabyl(spawn_year, cwt) |>
  adorn_totals(where = "col")

tabyl(bio_df,
      spawn_year,
      species_run_rear_type) |>
  adorn_totals(where = "both")

# any tags left from initial PTAGIS query that aren't in the bio data now?
sthd_tags |>
  anti_join(bio_df |>
              select(contains("pit")) |>
              pivot_longer(everything(),
                           names_to = "order",
                           values_to = "tag_code") |>
              filter(!is.na(tag_code)) |>
              select(tag_code)) |>
  select(pit_tag = tag_code) |>
  inner_join(all_tags) |>
  select(spawn_year,
         pit_tag,
         species_run_rear_type,
         everything()) |>
  as.data.frame()
# only one left is a rainbow trout (SRR == 30W)


#-----------------------------------------------------------------
# add age and final origin data from scales
# scale_age_file <- "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Bio Data/PIT Tag PRD Scale Ages Run Years 2022 to present.xlsx"

scale_age_file <- "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Bio Data/PIT Tag PRD Scale Ages Spawn Years 2011 to present.xlsx"

scale_age_df <-
  tibble(sheet_nm = excel_sheets(scale_age_file)) |>
  mutate(scale_data = map(sheet_nm,
                          .f = function(x) {
                            read_excel(scale_age_file,
                                       sheet = x) |>
                              clean_names()
                          },
                          .progress = TRUE)) |>
  unnest(scale_data) |>
  filter(spawn_year %in% unique(bio_df$spawn_year)) |>
  # fix a few typo things in the ages
  mutate(across(age_scales,
                str_to_upper),
         across(age_scales,
                ~ recode(.,
                         "R" = "R.0",
                         "R." = "R.0",
                         "R.`" = "R.0")),
         across(age_scales,
                ~ str_remove(., "^H")),
         across(age_scales,
                ~ str_remove(., "^W"))) |>
  mutate(across(age_scales,
                ~ if_else(nchar(.) > 10 & !is.na(as.numeric(.)),
                          as.character(round(as.numeric(.), 1)),
                          .))) |>
  # mutate(scale_id = if_else(!is.na(scale_card),
  #                           scale_card,
  #                           scale_cell)) |>
  mutate(across(scale_id,
                ~ if_else(!is.na(.),
                            paste(scale_card,
                                  scale_cell,
                                  sep = "-"),
                          .))) |>

  relocate(scale_id,
           .after = "scale_cell") |>
  select(-scale_card,
         -scale_cell) |>
  rename(age = age_scales) |>
  distinct()

# which PIT tags are duplicated?
dup_ages <-
  scale_age_df |>
  unite(sy_pit,
        spawn_year,
        primary_pit_tag,
        remove = F) |>
  filter(!is.na(age)) |>
  filter(sy_pit %in% sy_pit[duplicated(sy_pit)]) |>
  arrange(sy_pit)

# and what are the ages associated with those?
if(nrow(dup_ages) > 0) {
  dup_ages |>
    arrange(primary_pit_tag,
            spawn_year) |>
    select(spawn_year,
           primary_pit_tag,
           age) |>
    group_by(spawn_year,
             primary_pit_tag) |>
    mutate(n_rec = 1:n()) |>
    ungroup() |>
    pivot_wider(names_from = n_rec,
                values_from = age) #|>
    # filter(`1` != "UNREADABLE",
    #        `2` != "UNREADABLE")
}

# filter out rows for duplicated tags that have "UNREADABLE" or "NS" (no scales?) ages
scale_age_df <-
  scale_age_df |>
  mutate(across(age,
                ~ recode(.,
                         "NS" = NA_character_,
                         "UNREADABLE" = NA_character_))) |>
  unite(sy_pit,
        spawn_year, primary_pit_tag,
        remove = F) |>
  filter(!(sy_pit %in% sy_pit[duplicated(sy_pit)] &
             is.na(age))) |>
           # age %in% c("UNREADABLE",
           #            "NS"))) |>
  # for one tag with multiple ages, choose R.2 or W1.2 (Mike Hughes said so)
  filter(!(primary_pit_tag == "3DD.003D552F68" &
             age == "R.2")) |>
  select(-sy_pit)

# any more duplicated SY / tags?
scale_age_df |>
  unite(sy_pit,
        spawn_year, primary_pit_tag,
        remove = F) |>
  filter(sy_pit %in% sy_pit[duplicated(sy_pit)]) |>
  arrange(sy_pit)

# differences in PTAGIS file names
setdiff(unique(scale_age_df$ptagis_file_name), unique(bio_df$event_file_name))
setdiff(unique(bio_df$event_file_name[bio_df$spawn_year %in% unique(scale_age_df$spawn_year)]),
        unique(scale_age_df$ptagis_file_name))

# what tags do not have age data associated with them?
bio_df |>
  filter(!pit_tag %in% scale_age_df$primary_pit_tag,
         !second_pit_tag %in% scale_age_df$primary_pit_tag)

# pull out some information about those tags
bio_df |>
  filter(spawn_year %in% unique(scale_age_df$spawn_year)) |>
  left_join(scale_age_df |>
              select(spawn_year,
                     pit_tag = primary_pit_tag,
                     age) |>
              mutate(age_data_exists_v1 = T),
            by = join_by(spawn_year,
                         pit_tag)) |>
  left_join(scale_age_df |>
              select(spawn_year,
                     second_pit_tag = primary_pit_tag,
                     age_v2 = age) |>
              mutate(age_data_exists_v2 = T),
            by = join_by(spawn_year,
                         second_pit_tag)) |>
  mutate(across(starts_with("age_data_exists"),
                ~ replace_na(., F))) |>
  mutate(age_data_exists = if_else(age_data_exists_v1 | age_data_exists_v2, T, F),
         age = if_else(is.na(age) & !is.na(age_v2),
                       age_v2,
                       age)) |>
  filter(!age_data_exists) |>
  arrange(event_date,
          pit_tag) |>
  select(spawn_year,
         pit_tag,
         species_run_rear_type,
         event_date,
         event_type,
         contains("comments"),
         age) #|>
# write_csv(here("outgoing/other/missing_scale_data.csv"))
# mutate(event_month = month(event_date,
#                            label = T)) |>
#   tabyl(spawn_year, event_month) |>
#   adorn_totals(where = "both")

# what age data is associated with a tag not in our sample?
scale_age_df |>
  filter(primary_pit_tag %in% na.omit(bio_df$second_pit_tag)) |>
  select(spawn_year,
         primary_pit_tag,
         age) |>
  # tabyl(spawn_year)
  left_join(bio_df |>
               select(spawn_year,
                      primary_pit_tag = pit_tag,
                      second_pit_tag,
                      species_run_rear_type))

scale_age_df |>
  filter(primary_pit_tag == "3D9.1BF26E6B42")

#-----------------------------------------
# add scale data to bio_df
bio_age_df <-
  bio_df |>
  left_join(scale_age_df |>
              select(spawn_year,
                     pit_tag = primary_pit_tag,
                     age),
            by = join_by(spawn_year,
                         pit_tag)) |>
  left_join(scale_age_df |>
              select(spawn_year,
                     second_pit_tag = primary_pit_tag,
                     age_v2 = age),
            by = join_by(spawn_year,
                         second_pit_tag)) |>
  mutate(age = if_else(is.na(age) & !is.na(age_v2),
                       age_v2,
                       age)) |>
  select(-age_v2)

# any tags with missing ages?
bio_age_df |>
  filter(is.na(age)) |>
  select(spawn_year,
         contains("pit_tag"),
         init_age = age) |>
  pivot_longer(contains("pit_tag"),
               values_to = "primary_pit_tag") |>
  filter(!is.na(primary_pit_tag)) |>
  left_join(scale_age_df |>
              select(spawn_year,
                     primary_pit_tag,
                     age) |>
              mutate(age_data_exists = if_else(!is.na(age), T, F),
                     pit_tag_recorded = T),
            by = join_by(spawn_year,
                         primary_pit_tag)) |>
  filter(age_data_exists)
# any missing ages are because that tag doesn't have a scale age associated with it

bio_age_df |>
  filter(is.na(age)) |>
  tabyl(spawn_year)

#-----------------------------------------------------------------
# reduce to one row per tag / spawn year
bio_age_df |>
  ungroup() |>
  unite(sy_pit,
        spawn_year, pit_tag,
        remove = F) |>
  summarize(n_row = n(),
            n_records = n_distinct(sy_pit),
            n_dups = sum(duplicated(sy_pit)))

# look at duplicated records briefly
bio_age_df |>
  unite(sy_pit,
        spawn_year, pit_tag,
        remove = F) |>
  filter(sy_pit %in% sy_pit[duplicated(sy_pit)]) |>
  arrange(sy_pit) |>
  select(spawn_year,
         pit_tag,
         event_file_name,
         release_date,
         # event_date,
         event_type,
         srr = species_run_rear_type,
         sex,
         cwt,
         ad_clip,
         age,
         length,
         conditional_comments)

bio_final_df <-
  bio_age_df |>
  relocate(release_date,
           .after = event_date) |>
  nest(.by = c(spawn_year,
               pit_tag)) |>
  mutate(keep_data = map(data,
                         .f = function(x) {
                           if(sum(x$event_type == "Mark") > 0) {
                             keep_row <-
                               x |>
                               filter(event_type == "Mark")
                           } else {
                             keep_row = x
                           }

                           if(nrow(keep_row) > 1) {
                             keep_row <-
                               keep_row |>
                               filter(release_date == min(release_date))
                           }
                           if(nrow(keep_row) > 1) {
                             keep_row <-
                               keep_row |>
                               slice(1)
                           }
                           return(keep_row)
                         },
                         .progress = TRUE)) |>
  unnest(keep_data) |>
  select(-data)

bio_final_df |>
  unite(sy_pit,
        spawn_year, pit_tag,
        remove = F) |>
  select(sy_pit) |>
  distinct() |>
  nrow() |>
  identical(nrow(bio_final_df))

# any tags missing ages that shouldn't be?
bio_final_df |>
  filter(is.na(age)) |>
  select(spawn_year,
         pit_tag) |>
  distinct() |>
  inner_join(scale_age_df |>
               filter(!is.na(age)) |>
               select(spawn_year,
                      pit_tag = primary_pit_tag,
                      age))

# any tags missing lengths that shouldn't be?
bio_final_df |>
  filter(is.na(length))

summary(bio_final_df)
colSums(is.na(bio_final_df))

#-----------------------------------------------------------------
# save as Excel file
#-----------------------------------------------------------------
bio_final_df %>%
  split(f = ~ spawn_year) %>%
  write_xlsx(path = here('analysis/data/derived_data',
                         'PRA_Sthd_BioData.xlsx'))

#-----------------------------------------------------------------
# for tag lists
#-----------------------------------------------------------------
# put bounds around years
min_yr = min(bio_final_df$spawn_year)
max_yr = max(bio_final_df$spawn_year)


# pull out PIT tag numbers by spawn year
tag_list <-
  # bio_final_df |>
  tagging_df |>
  select(spawn_year,
         contains("pit_tag")) |>
  pivot_longer(cols = contains("pit_tag"),
               names_to = "source",
               values_to = "tag_code") %>%
  filter(!is.na(tag_code)) |>
  select(-source) |>
  distinct() |>
  nest(.by = spawn_year)

# # save tags to upload to PTAGIS
# # all years
# for(yr in tag_list$spawn_year) {
#   tag_list |>
#     filter(spawn_year == yr) |>
#     pull(data) |>
#     extract2(1) |>
#     write_delim(file = here('analysis/data/raw_data/tag_lists',
#                             paste0('UC_Sthd_Tags_', yr, '.txt')),
#                 delim = '\n',
#                 col_names = F)
# }

# just write the latest year
tag_list |>
  filter(spawn_year == max_yr) |>
  pull(data) |>
  extract2(1) |>
  write_delim(file = here('analysis/data/raw_data/tag_lists',
                          paste0('UC_Sthd_Tags_', max_yr, '.txt')),
              delim = '\n',
              col_names = F)


#-----------------------------------------------------------------
# save biological data for later
write_rds(bio_final_df,
          file = here('analysis/data/derived_data',
                      paste0('Bio_Data_', min_yr, '_', max_yr, '.rds')))

save(sthd_tags,
     tagging_df,
     bio_final_df,
     file = here('analysis/data/derived_data',
                 paste0('Bio_Tag_Data_', min_yr, '_', max_yr, '.rda')))

#-----------------------------------------------------------------
# decode conditional comments
cond_comm_codes <- read_csv(
  paste0("T:/DFW-Team FP Upper Columbia Escapement - General/",
         "UC_Sthd/inputs/PTAGIS/",
         "Glossary_ConditionalComment_ValidationCodes.csv")) |>
  clean_names()

bio_final_df |>
  filter(spawn_year == 2023) |>
  select(pit_tag,
         conditional_comments) |>
  # sample_n(1000) |>
  mutate(tmp = map(conditional_comments,
                   .f = function(x) {
                     str_split(x, "[:space:]") |>
                       unlist() |>
                       as_tibble() |>
                       rename(code = value)
                   })) |>
  select(-conditional_comments) |>
  unnest(tmp) |>
  left_join(cond_comm_codes |>
              select(-definition))

