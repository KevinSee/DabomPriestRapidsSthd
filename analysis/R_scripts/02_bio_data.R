# Author: Kevin See
# Purpose: create tag lists to feed to PTAGIS query
# Created: 8/15/2023
# Last Modified: 9/14/2026
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
# this is a query set up on Kevin See's PTAGIS account, and finds every steelhead marked or recaptured
# at Priest Rapids from June 1, 2010 through today
sthd_tags <-
  read_delim(
    "https://api.ptagis.org/reporting/reports/kevinsee/file/PriestRapids_Sthd_SY2011_present.csv",
    delim = ",",
    locale = readr::locale(encoding = "UTF-16LE"),
    show_col_types = FALSE) |>
  clean_names() |>
  rename(tag_code = tag,
         event_capture_method_code = x5,
         event_rear_type_code = x14,
         event_species_name = x16,
         event_srr_name = x18,
         event_conditional_comments_name = x20) |>
  mutate(across(ends_with("_date"),
                mdy),
         across(ends_with("date_time"),
                mdy_hms)) |>
  mutate(spawn_year = case_when(!is.na(event_release_date) &
                                  month(event_release_date) < 6 ~ year(event_release_date),
                                !is.na(event_release_date) &
                                  month(event_release_date) >= 6 ~ year(event_release_date) + 1,
                                is.na(event_release_date) &
                                  month(event_date) < 6 ~ year(event_date),
                                is.na(event_release_date) &
                                  month(event_date) >= 6 ~ year(event_date) + 1,
                                .default = NA_real_)) |>
  # grab only tags from the adult ladder
  filter(event_capture_method_code == "LADDER")

# remove tags that are spawning in a future year
sthd_tags <-
  sthd_tags |>
  filter(spawn_year <= year(today()))

# drop any steelhead tags that were put in during Spring Chinook sampling (prior to July 1)
# since Spring Chinook sampling only occurred for 3 years
sthd_tags <-
  sthd_tags |>
  filter_out(between(month(event_release_date), 1, 6))


# pull out MRR data about all PIT tags from those MRR files
all_tags <-
  sthd_tags |>
  select(spawn_year,
         event_file) |>
  distinct() |>
  arrange(spawn_year,
          event_file) |>
  # # filter out one tagging file that is not a WDFW project
  # filter(event_file != "CLD14105.WA1") |>
  mutate(tag_file = map(event_file,
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
  # mutate(across(poh,
  #               as.numeric)) |>
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
  adorn_totals("both")
# some are recoveries (i.e. died at Priest)
# Some are orphan or disowned tags (https://www.ptagis.org/FAQ#11); missing mark information
# 3D9.1BF27A5A27 is an orphan tag
# 3DD.003DA168BC is disowned
# in 2024, these 4 extra tags are actually the 2nd PIT tag in these fish
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
         event_file) |>
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

tagging_df |>
  anti_join(sthd_tags |>
              select(tag_code),
            by = join_by(pit_tag == tag_code))

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
         event_file,
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
         # poh,
         conditional_comments,
         text_comments,
         scale_id) |>
  mutate(across(release_date,
                ~ if_else(is.na(.),
                          event_date,
                          .)))
# use release date as the trap date?

# bio_df |>
#   filter(pit_tag %in% pit_tag[duplicated(pit_tag)]) |>
#   arrange(pit_tag, event_date, release_date)


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
# keep mark event tag, or first release date
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
sum(bio_df$pit_tag %in% bio_df$second_pit_tag)

bio_df |>
  filter(second_pit_tag %in% pit_tag) |>
  as.data.frame()
bio_df |>
  filter(pit_tag %in% second_pit_tag) |>
  as.data.frame()

# there are 4 fish with duplicate records (all from SY2024)
# one record lists 2 tags
# the other lists only the 2nd tag as the primary one, with no 2nd tag
# dropping the latter
bio_df <-
  bio_df |>
  anti_join(bio_df |>
              filter(pit_tag %in% second_pit_tag))



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
              select(tag_code),
            by = join_by(tag_code)) |>
  select(pit_tag = tag_code) |>
  inner_join(all_tags,
             by = join_by(pit_tag)) |>
  select(spawn_year,
         pit_tag,
         species_run_rear_type,
         everything()) |>
  as.data.frame()
# only one left is a rainbow trout (SRR == 30W)

#-----------------------------------------------------------------
# QA / QC
#-----------------------------------------------------------------

# mis-match 1st and 2nd PIT tag codes
sum(bio_df$second_pit_tag %in% bio_df$pit_tag)
sum(bio_df$pit_tag %in% bio_df$second_pit_tag)

# fish caught more than once
bio_df |>
  # filter(spawn_year == 2026) |>
  filter(spawn_year >= 2025) |>
  get_dupes(spawn_year,
            pit_tag) |>
  summarize(n_caps = n(),
            n_marks = sum(event_type == "Mark"),
            n_recaps = sum(event_type == "Recapture"),
            time_between = difftime(max(event_date),
                                    min(event_date),
                                    units = "days"),
            across(c(species_run_rear_type,
                     origin,
                     sex,
                     cwt,
                     ad_clip,
                     length,
                     conditional_comments),
                   n_distinct),
            lngth_diff = max(length, na.rm = T) - min(length, na.rm = T),
            .by = c(spawn_year,
                    pit_tag))

# fish with 2nd PIT tag
bio_df |>
  # filter(spawn_year == 2026) |>
  filter_out(is.na(second_pit_tag)) |>
  select(spawn_year,
         pit_tag) |>
  left_join(bio_df)

#-----------------------------------------------------------------
# add age and final origin data from scales
# scale_age_files <- c("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Bio Data/PIT Tag PRD Scale Ages Spawn Years 2011 to 2021.xlsx",
#                      "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Bio Data/PIT Tag PRD Scale Ages Run Years 2022 to present.xlsx")

# scale_age_file <- "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Bio Data/PIT Tag PRD Scale Ages Spawn Years 2011 to present.xlsx"


scale_age_df <-
  tibble(folder = file.path("T:",
                            "DFW-Team FP Upper Columbia Escapement - General",
                            "UC_Sthd",
                            "inputs",
                            "Bio Data")) |>
  mutate(file_name = map(folder, .f = list.files)) |>
  unnest(file_name) |>
  filter(str_detect(file_name, "Scale Ages")) |>
  unite(file_nm, folder, file_name, sep = "/") |>
  mutate(sheet_nm = map(file_nm,
                        .f = excel_sheets)) |>
  unnest(sheet_nm) |>
  mutate(scale_data = map2(file_nm,
                           sheet_nm,
                           .f = function(x, y) {
                             read_excel(x,
                                        sheet = y) |>
                               clean_names() |>
                               mutate(across(scale_cell,
                                             as.character))
                           },
                           .progress = T)) |>
  unnest(scale_data) |>
  mutate(across(scale_card,
                ~ case_when(is.na(.) & str_detect(scale_cell, "-") ~ str_split_i(scale_cell, "-", 1),
                            .default = .)),
         across(scale_id,
                ~ case_when(is.na(.) &
                              !is.na(scale_cell) &
                              str_detect(scale_cell, "-") ~ scale_cell,
                            is.na(.) &
                              str_detect(scale_cell, "-", negate = T) &
                              !is.na(scale_card) ~ paste(scale_card,
                                                         scale_cell,
                                                         sep = "-"),
                            .default = .)),
         across(scale_cell,
                ~ case_when(!is.na(.) &
                              str_detect(., "-") &
                              str_detect(scale_id, "-")~ str_split_i(., "-", 2),
                            .default = .))) |>
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
  relocate(scale_id,
           .after = "scale_cell") |>
  # select(-scale_card,
  #        -scale_cell) |>
  rename(age = age_scales) |>
  distinct()

# which PIT tags are duplicated?
dup_ages <-
  scale_age_df |>
  filter(!is.na(age)) |>
  get_dupes(spawn_year,
            primary_pit_tag) |>
  arrange(spawn_year,
          primary_pit_tag)

# and what are the ages associated with those?
# for records with mis-matched ages
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
                values_from = age) |>
    # filter(`1` != "UNREADABLE",
    #        `2` != "UNREADABLE") |>
    mutate(same_ages = if_else(`1` == `2`, T, F)) |>
    filter(!same_ages) |>
    arrange(spawn_year,
            primary_pit_tag) |>
    as.data.frame()
}

# filter out rows for duplicated tags that have "UNREADABLE" or "NS" (no scales?) ages
scale_age_df <-
  scale_age_df |>
  mutate(across(age,
                ~ replace_values(.,
                                 "NS" ~ NA_character_,
                                 "UNREADABLE" ~ NA_character_,
                                 "NA" ~ NA_character_))) |>
  unite(sy_pit,
        spawn_year, primary_pit_tag,
        remove = F) |>
  filter(!sy_pit %in% sy_pit[duplicated(sy_pit)] |
           (sy_pit %in% sy_pit[duplicated(sy_pit)] &
              !is.na(age))) |>
  # filter(!(sy_pit %in% sy_pit[duplicated(sy_pit)] &
  #            is.na(age))) |>
  #          # age %in% c("UNREADABLE",
  #          #            "NS"))) |>
  # for one tag with multiple ages, choose R.2 or W1.2 (Mike Hughes said so)
  filter_out(primary_pit_tag == "3DD.003D552F68" &
               age == "R.2") |>
  select(-sy_pit)

# any more duplicated SY / tags?
scale_age_df |>
  filter(!is.na(age)) |>
  get_dupes(spawn_year,
            primary_pit_tag) |>
  arrange(spawn_year,
          primary_pit_tag)


# differences in PTAGIS file names
setdiff(unique(scale_age_df$ptagis_file_name), unique(bio_df$event_file))
setdiff(unique(bio_df$event_file[bio_df$spawn_year %in% unique(scale_age_df$spawn_year)]),
        unique(scale_age_df$ptagis_file_name))

# what tags do not have age data associated with them?
bio_df |>
  filter(!pit_tag %in% scale_age_df$primary_pit_tag,
         !second_pit_tag %in% scale_age_df$primary_pit_tag) |>
  as.data.frame()
# all from SY2011

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
  filter_out(age_data_exists) |>
  arrange(event_date,
          pit_tag) |>
  select(spawn_year,
         pit_tag,
         species_run_rear_type,
         event_date,
         release_date,
         event_type,
         contains("comments"),
         age) #|>
# write_csv(here("outgoing/other/missing_scale_data.csv"))
# mutate(event_month = month(event_date,
#                            label = T)) |>
#   tabyl(spawn_year, event_month) |>
#   adorn_totals(where = "both")

# duplicate records from latest spawn year
scale_age_df |>
  filter_out(is.na(age)) |>
  get_dupes(spawn_year,
            primary_pit_tag) |>
  arrange(spawn_year,
          primary_pit_tag) |>
  slice_max(spawn_year) |>
  select(spawn_year,
         pit_tag = primary_pit_tag,
         age,
         scale_id,
         notes_comments) |>
  distinct() |>
  left_join(bio_df,
            by = join_by(spawn_year,
                         pit_tag,
                         scale_id))

# same tag in same SY, different ages
scale_age_df |>
  select(spawn_year,
         pit_tag = primary_pit_tag,
         age) |>
  distinct() |>
  get_dupes(spawn_year,
            pit_tag)


# what age data is associated with a tag not in our sample?
scale_age_df |>
  filter_out(primary_pit_tag %in% bio_df$pit_tag |
             primary_pit_tag %in% na.omit(bio_df$second_pit_tag)) |>
  tabyl(spawn_year)

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
                     age) |>
              distinct(),
            by = join_by(spawn_year,
                         pit_tag)) |>
  left_join(scale_age_df |>
              select(spawn_year,
                     second_pit_tag = primary_pit_tag,
                     age_v2 = age) |>
              distinct(),
            by = join_by(spawn_year,
                         second_pit_tag)) |>
  mutate(across(age,
                ~ case_when(is.na(.) &
                              !is.na(age_v2) ~ age_v2,
                            .default = .))) |>
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
  tabyl(spawn_year) |>
  left_join(scale_age_df |>
              group_by(spawn_year) |>
              summarize(miss_age = sum(is.na(age))))

bio_age_df |>
  select(spawn_year,
         pit_tag,
         second_pit_tag,
         age) |>
  distinct() |>
  get_dupes(spawn_year, pit_tag)


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
  get_dupes(spawn_year,
            pit_tag) |>
  arrange(spawn_year,
          pit_tag) |>
  select(dupe_count,
         spawn_year,
         pit_tag,
         event_file,
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

# keep the Mark record, or the first recap
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
  select(spawn_year,
         pit_tag) |>
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
                      age),
             by = join_by(spawn_year, pit_tag))

# any tags missing lengths that shouldn't be?
bio_final_df |>
  filter(is.na(length))

summary(bio_final_df)
colSums(is.na(bio_final_df))

# any tags from initial list missing in this final dataframe?
sthd_tags |>
  anti_join(bio_final_df |>
              select(spawn_year,
                     contains("pit_tag")) |>
              pivot_longer(contains("pit_tag"),
                           names_to = "position",
                           values_to = "tag_code") |>
              filter_out(is.na(tag_code)))


#-----------------------------------------------------------------
# read in genetics data, starting with SY2025
gen_df <-
  tibble(gen_folder = file.path("T:",
                                "DFW-Team FP Upper Columbia Escapement - General",
                                "UC_Sthd",
                                "inputs",
                                "Bio Data",
                                "Sex and Origin PRD-Brood Comparison Data",
                                "Genetics_PBT_GSI")) |>
  mutate(yr_folder = map(gen_folder, .f = list.files)) |>
  unnest(yr_folder) |>
  filter(str_detect(yr_folder, "^RY")) |>
  mutate(folder = file.path(gen_folder, yr_folder)) |>
  mutate(file_nm = map(folder, .f = list.files)) |>
  unnest(file_nm) |>
  filter(str_detect(file_nm, "\\.xls")) |>
  filter(str_detect(file_nm, "Douglas -")) |>
  mutate(sheet_nm = map2(folder,
                         file_nm,
                        .f = function(x, y) excel_sheets(file.path(x, y)))) |>
  unnest(sheet_nm) |>
  filter(sheet_nm == "PBT_GSI") |>
  # as.data.frame()
  # slice(2) |>
  mutate(gen_data = pmap(list(x = folder,
                                y = file_nm,
                                z = sheet_nm),
                           .f = function(x, y, z) {
                             read_excel(file.path(x, y),
                                        sheet = z,
                                        skip = 1) |>
                               clean_names() |>
                               mutate(across(field_name,
                                             as.character))
                           },
                           .progress = T)) |>
  select(-c(gen_folder:sheet_nm)) |>
  unnest(gen_data) |>
  rename(gsi_assignment_2 = x2nd_best_estimate,
         gsi_assignment_3 = x3rd_best_estimate,
         byrne_statwk = byrn_estatwk,
         pit_tag = pit_tag_number,
         gen_rear = rear) |>
  mutate(probability_gsi_1 = case_when(!is.na(probability_19) ~ probability_19,
                                       !is.na(probability_18) ~ probability_18,
                                       .default = NA_real_),
         probability_gsi_2 = case_when(!is.na(probability_22) ~ probability_22,
                                       !is.na(probability_21) ~ probability_21,
                                       .default = NA_real_),
         probability_gsi_3 = case_when(!is.na(probability_24) ~ probability_24,
                                       !is.na(probability_23) ~ probability_23,
                                       .default = NA_real_)) |>
  select(-any_of(paste0("probability_", c(18:24)))) |>
  mutate(spawn_year = sample_year + 1) |>
  # drop any row with no PIT tag number
  filter(!is.na(pit_tag)) |>
  # flip any PIT tag that's marked as the "secondary" PIT tag in the bio data
  left_join(bio_final_df |>
              select(spawn_year,
                     primary_pit_tag = pit_tag,
                     second_pit_tag),
            by = join_by(spawn_year,
                         pit_tag == second_pit_tag)) |>
  mutate(across(pit_tag,
                ~ case_when(is.na(primary_pit_tag) ~ .,
                            !is.na(primary_pit_tag) ~ primary_pit_tag,
                            .default = .))) |>
  select(-primary_pit_tag)

# any duplicated tags?
gen_df |>
  get_dupes(spawn_year,
            pit_tag) |>
  as.data.frame()

gen_df |>
  get_dupes(spawn_year,
            pit_tag) |>
  select(spawn_year,
         pit_tag) |>
  distinct() |>
  left_join(bio_final_df)

bio_final_df |>
  filter(spawn_year >= 2025) |>
  anti_join(gen_df |>
              select(spawn_year,
                     pit_tag) |>
              distinct())

# drop the genetic duplicates where one of them is "failed"
gen_df <-
  gen_df |>
  anti_join(gen_df |>
              get_dupes(spawn_year,
                        pit_tag) |>
              select(spawn_year,
                     pit_tag,
                     assignment_method) |>
              filter(assignment_method == "failed"))


# get some broodstock data
brood_df <-
  read_excel(file.path("T:",
                       "DFW-Team FP Upper Columbia Escapement - General",
                       "UC_Sthd",
                       "inputs",
                       "Bio Data",
                       "Sex and Origin PRD-Brood Comparison Data",
                       "STHD UC Brood Collections_2011 to current.xlsx"),
             sheet = 1) |>
  clean_names() |>
  rename(pit_tag = recaptured_pit)

# make a big comparison file
bio_comp <-
  bio_final_df |>
  filter(spawn_year >= 2025) |>
  # filter(spawn_year == 2026) |>
  mutate(snub_dorsal = str_detect(conditional_comments, "DF")) |>
  select(spawn_year,
         pit_tag,
         species_run_rear_type,
         origin_bio = origin,
         sex_field = sex,
         cwt,
         ad_clip,
         snub_dorsal,
         age,
         conditional_comments) |>
  left_join(scale_age_df |>
              select(spawn_year,
                     pit_tag = primary_pit_tag,
                     origin_field,
                     origin_scales,
                     origin_sc_final = origin_final),
            by = join_by(spawn_year,
                         pit_tag)) |>
  left_join(gen_df |>
              # filter(sample_year >= 2024) |>
              select(spawn_year,
                     pit_tag,
                     sex_gen = genetic_sex,
                     origin_gen = gen_rear,
                     assignment_method,
                     popname,
                     observed_gsi_assignment,
                     probability_gsi_1),
            by = join_by(spawn_year,
                         pit_tag)) |>
  left_join(brood_df |>
              select(spawn_year,
                     pit_tag,
                     sex_brood = sex_final,
                     origin_brood = origin_final,
                     brood_status = handling_status_spawning)) |>
  mutate(origin_final = case_when(ad_clip ~ "H",
                                  assignment_method == "PBT" ~ "H",
                                  origin_sc_final == "H" ~ "H",
                                  is.na(origin_sc_final) &
                                    origin_scales == "H" ~ "H",
                                  assignment_method == "GSI" &
                                    origin_sc_final == "W" ~ "W",
                                  assignment_method == "GSI" &
                                    is.na(origin_sc_final) &
                                    origin_scales == "W" ~ "W",
                                  origin_scales == "W" ~ "W",
                                  is.na(origin_scales) &
                                    (cwt | snub_dorsal) ~ "H",
                                  assignment_method != "PBT" &
                                    !ad_clip &
                                    !cwt &
                                    !snub_dorsal &
                                    is.na(origin_scales) ~ "W",
                                  .default = origin_bio),
         across(origin_final,
                ~ case_when(. == "H" & !ad_clip ~ "HNC",
                            .default = .))) |>
  relocate(origin_final,
           .after = origin_gen)

bio_comp |>
  tabyl(origin_bio,
        origin_final,
        spawn_year)

bio_comp |>
  # filter(spawn_year >= 2025) |>
  tabyl(sex_gen,
        sex_field,
        spawn_year) |>
  adorn_totals("both") |>
  adorn_percentages() |>
  adorn_pct_formatting()

# compile spreadsheet of changes that need to be made in PTAGIS
# pick a minimum year
min_yr = 2026

list("Sex change" =

       bio_comp |>
       filter(spawn_year >= min_yr) |>
       filter(sex_field != sex_gen,
              sex_gen != "Unknown") |>
       select(spawn_year,
              pit_tag,
              conditional_comments,
              starts_with("sex")) |>
       mutate(to_do = "change sex in PTAGIS"),

     "Origin change" =

       bio_comp |>
       filter(spawn_year >= min_yr) |>
       filter(origin_bio == "W",
              origin_final == "HNC") |>
       select(spawn_year,
              pit_tag,
              cwt,
              ad_clip,
              snub_dorsal,
              conditional_comments,
              age,
              origin_ptagis = origin_bio,
              origin_field,
              origin_scales,
              origin_sc_final,
              origin_gen,
              origin_brood,
              origin_final,
              assignment_method) |>
       mutate(reason = case_when(assignment_method == "PBT" ~ "PBT assignment",
                                 assignment_method == "GSI" &
                                   origin_scales == "H" ~ "Scale read",
                                 .default = NA_character_)) |>
       mutate(to_do = "change origin in PTAGIS")
) #|>
  # write_xlsx(file.path("T:",
  #                      "DFW-Team FP Upper Columbia Escapement - General",
  #                      "UC_Sthd",
  #                      "inputs",
  #                      "Bio Data",
  #                      "Sex and Origin PRD-Brood Comparison Data",
  #                      "Genetics_PBT_GSI",
  #                      "PTAGIS_changes.xlsx"))


bio_comp |>
  filter(origin_final != origin_gen) |>
  arrange(desc(assignment_method),
          spawn_year,
          origin_ptagis = origin_bio,
          origin_final,
          origin_gen,
          snub_dorsal,
          age,
          origin_scales) |>
  select(-starts_with("sex")) |>
         # -origin_bio,
         # -origin_sc_final,
         # -species_run_rear_type) |>
  # filter(spawn_year == 2025)
  filter(spawn_year == 2026) |>
  write_csv(file.path("T:",
                      "DFW-Team FP Upper Columbia Escapement - General",
                      "UC_Sthd",
                      "inputs",
                      "Bio Data",
                      "Sex and Origin PRD-Brood Comparison Data",
                      "Genetics_PBT_GSI",
                      "genetic_origin_comparison.csv"))



bio_comp |>
  mutate(across(origin_final,
                ~ replace_values(.,
                                 "HNC" ~ "H"))) |>
  filter(origin_final != origin_bio) |>
  filter(origin_final != origin_sc_final) |>
  tabyl(spawn_year)


# bio_final_df <-
#   bio_final_df |>
#   left_join(bio_comp |>
#               filter(spawn_year >= 2025) |>
#               mutate(across(origin_final,
#                             ~ case_match(.,
#                                          "HNC" ~ "H",
#                                          .default = .))) |>
#               select(spawn_year,
#                      pit_tag,
#                      origin_final),
#             by = join_by(spawn_year,
#                          pit_tag)) |>
#   mutate(across(origin,
#                 ~ case_when(!is.na(origin_final) ~ origin_final,
#                             .default = .))) |>
#   select(-origin_final)

#-----------------------------------------------------------------
# add selected genetic data where available
#-----------------------------------------------------------------
bio_final_df <-
  bio_final_df |>
  # select(-c(assignment_method:gsi_prob)) |>
  left_join(gen_df |>
              mutate(spawn_year >= 2025) |>
              select(spawn_year,
                     pit_tag,
                     assignment_method,
                     popname,
                     hatchery_by,
                     hatchery_byname,
                     gsi_assignment = observed_gsi_assignment,
                     gsi_prob = probability_gsi_1) |>
              distinct(),
            by = join_by(spawn_year,
                         pit_tag))



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
  slice_max(spawn_year) |>
  pull(data) |>
  extract2(1) |>
  write_delim(file = here("analysis",
                          "data",
                          "raw_data",
                          "tag_lists",
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


# save genetics data for later
gen_df |>
  # filter(spawn_year >= 2025) |>
  write_rds(file = here('analysis/data/derived_data',
                      paste0('Genetic_Data_', min(gen_df$spawn_year), '_', max(gen_df$spawn_year), '.rds')))
                      # paste0('Genetic_Data_2025_', max(gen_df$spawn_year), '.rds')))

#-----------------------------------------------------------------
# decode conditional comments
cond_comm_codes <-
  read_csv(
    file.path("T:",
              "DFW-Team FP Upper Columbia Escapement - General/",
              "UC_Sthd",
              "inputs",
              "PTAGIS",
              "Glossary_ConditionalComment_ValidationCodes.csv")) |>
  clean_names()

bio_final_df |>
  filter(spawn_year == 2026) |>
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

