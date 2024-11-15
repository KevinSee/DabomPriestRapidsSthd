# Author: Kevin See
# Purpose: summarize DABOM results
# Created: 4/1/20
# Last Modified: 11/28/2023
# Notes:

#-----------------------------------------------------------------
# for years prior to 2023, install an older version that doesn't include some of the uppermost sites in the Okanogan
# devtools::install_github("KevinSee/DABOM",
#                          ref = "develop",
#                          ref = "v2.0.1")

#-----------------------------------------------------------------
# load needed libraries
library(DABOM)
library(PITcleanr)
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(STADEM)
library(writexl)
library(msm)
library(moments)
library(coda)
library(here)


#-----------------------------------------------------------------
# load configuration and site_df data
load(here('analysis/data/derived_data',
          'site_config.rda'))

#-----------------------------------------------------------------
# set year
yr = 2024

# what dam count to use?
dam_cnt_name = c("PriestRapids",
                 "RockIsland",
                 "RockyReach")[2]

#-----------------------------------------------------------------
# run for set of years
for(yr in 2011:2023) {

  cat(paste("Working on", yr, "\n\n"))

  #   if(yr %in% c(2011:2015, 2018)) {
  #     dam_cnt_name == "PriestRapids"
  #   } else {
  #     dam_cnt_name == "RockIsland"
  #   }


  # load compressed detections and biological data
  load(here('analysis/data/derived_data/PITcleanr',
            paste0('UC_Steelhead_', yr, '.rda')))

  # load JAGS MCMC results
  load(here("analysis/data/derived_data/model_fits",
            paste0('PRA_DABOM_Steelhead_', yr,'.rda')))

  # rename a few columns from bio data
  bio_df <-
    bio_df |>
    rename(tag_code = pit_tag,
           fork_length = length)

  # # read in updated biological data
  # bio_df <- read_rds(here('analysis/data/derived_data',
  #                         'Bio_Data_2011_2023.rds')) |>
  #   filter(spawn_year == yr) |>
  #   rename(tag_code = pit_tag) #|>
  #   # rename(age = age_scales,
  #   #        fork_length = length)


  # estimate final spawning location
  tag_summ = summarizeTagData(filter_obs,
                              bio_df)

  # look at which branch each tag was assigned to for spawning
  brnch_df = buildNodeOrder(addParentChildNodes(parent_child, configuration)) %>%
    mutate(group = case_when(node == "PRA" ~ "Start",
                             str_detect(path, 'LWE') |
                               node %in% c("CLK") ~ "Wenatchee",
                             str_detect(path, "ENL") ~ "Entiat",
                             str_detect(path, "LMR") ~ "Methow",
                             str_detect(path, "OKL") |
                               node %in% c("FST") ~ "Okanogan",
                             str_detect(path, "RIA", negate = T) &
                               str_detect(path, " ") ~ "BelowPriest",
                             node == "WEA" ~ "WellsPool",
                             .default = "Other"
                             )) |>
    mutate(group = factor(group,
                          levels = c("Wenatchee",
                                     "Entiat",
                                     "Methow",
                                     "Okanogan",
                                     "BelowPriest",
                                     "WellsPool",
                                     "Start",
                                     "Other")))

  tag_summ %<>%
    left_join(brnch_df,
              by = c("final_node" = "node"))



  # summarize detection probabilities
  detect_summ = summariseDetectProbs(dabom_mod = dabom_mod,
                                     filter_ch = filter_obs)

  # which sites had detection probabilities fixed at 0% or 100%
  detect_summ %>%
    filter(sd == 0)

  # look at all the other sites
  detect_summ %>%
    filter(sd > 0) %>%
    arrange(desc(n_tags))
    # arrange(desc(sd))

  # compile all movement probabilities, and multiply them appropriately
  trans_post = extractTransPost(dabom_mod,
                                parent_child,
                                configuration)


  trans_df = compileTransProbs(trans_post,
                               parent_child) |>
    select(-main_branch) |>
    mutate(across(origin,
                  ~ case_match(.,
                               1 ~ "W",
                               2 ~ "H",
                               .default = NA_character_)))

  # summarize transition probabilities
  trans_summ = summarisePost(trans_df,
                             value,
                             param,
                             origin)

  #-----------------------------------------------------------------
  # total escapement past Priest, by origin
  start_date = paste0(yr-1, '0601')
  end_date = paste0(yr, '0531')


  # # start with PIT-tag based reascension data
  # org_escape = queryPITtagData(damPIT = 'PRA',
  #                              spp = "Steelhead",
  #                              start_date = start_date,
  #                              end_date = end_date) %>%
  #   filter(!str_detect(TagId, "000.0")) %>%
  #   mutate(SpawnYear = yr) %>%
  #   mutate(across(TagIdAscentCount,
  #                 tidyr::replace_na,
  #                 0)) %>%
  #   mutate(ReAscent = ifelse(TagIdAscentCount > 1, T, F)) %>%
  #   group_by(Species, SpawnYear, Date) %>%
  #   summarise(tot_tags = n_distinct(TagId),
  #             reascent_tags = n_distinct(TagId[ReAscent]),
  #             .groups = "drop") %>%
  #   group_by(Species, SpawnYear) %>%
  #   summarise(across(matches('tags'),
  #                    sum,
  #                    na.rm = T),
  #             .groups = "drop") %>%
  #   mutate(reasc_rate = reascent_tags / tot_tags,
  #          reasc_rate_se = sqrt(reasc_rate * (1 - reasc_rate) / tot_tags)) %>%
  #   # add window counts
  #   bind_cols(getWindowCounts(dam = 'PRD',
  #                             spp = "Steelhead",
  #                             start_date = start_date,
  #                             end_date = end_date) %>%
  #               summarise_at(vars(win_cnt),
  #                            list(sum),
  #                            na.rm = T) %>%
  #               select(tot_win_cnt = win_cnt)) %>%
  #   mutate(adj_win_cnt = tot_win_cnt * (1 - reasc_rate),
  #          adj_win_cnt_se = tot_win_cnt * reasc_rate_se) %>%
  #   bind_cols(bio_df %>%
  #               group_by(origin) %>%
  #               summarise(n_tags = n_distinct(tag_code)) %>%
  #               mutate(prop = n_tags / sum(n_tags),
  #                      prop_se = sqrt((prop * (1 - prop)) / sum(n_tags)))) %>%
  #   rowwise() %>%
  #   mutate(tot_escp = adj_win_cnt * prop,
  #          tot_escp_se = msm::deltamethod(~ x1 * x2,
  #                                         mean = c(adj_win_cnt, prop),
  #                                         cov = diag(c(adj_win_cnt_se, prop_se)^2))) %>%
  #   select(Species, SpawnYear, origin, reasc_rate, matches('escp'))


  #----------------------------------------------------------------
  # better way to use upstream dam counts
  # add origin to prepped capture histories
  # pit_obs = prepped_ch %>%
  #   left_join(bio_df %>%
  #               select(tag_code,
  #                      origin) |>
  #               distinct()) %>%
  #   select(tag_code, origin,
  #          everything())

  # get the total dam counts from various dams
  dam_escp_df = tibble(year = yr,
                       dam = as_factor(c("PriestRapids",
                                         "RockIsland")),
                       # "RockyReach"),
                       dam_code = c("PRD",
                                    "RIS"),
                       # "RRH"),
                       pit_code = c("PRA",
                                    "RIA")) %>%
                                    # "RRF")) %>%
    rowwise() %>%
    mutate(win_cnt = map_dbl(dam_code,
                             .f = function(x) {
                               suppressMessages(STADEM::getWindowCounts(dam = x,
                                                       spp = "Steelhead",
                                                       start_date = start_date,
                                                       end_date = end_date)) %>%
                                 summarise_at(vars(win_cnt),
                                              list(sum),
                                              na.rm = T) %>%
                                 pull(win_cnt)
                             })) %>%
    # add re-ascension data
    mutate(reasc_df = map(pit_code,
                          .f = function(x) {
                            dart_df = try(suppressMessages(STADEM::queryPITtagData(damPIT = x,
                                                                           spp = "Steelhead",
                                                                           start_date = start_date,
                                                                           end_date = end_date)))
                            if(class(dart_df)[1] == "try-error") {
                              # return(NA)
                              tibble(origin = c("H", "W"),
                                     tot_tags = NA,
                                     reascent_tags = NA) %>%
                                return()
                            } else {
                              dart_df %>%
                                filter(!str_detect(TagId, "000.0")) %>%
                                mutate(SpawnYear = yr) %>%
                                mutate(across(TagIdAscentCount,
                                              as.numeric)) %>%
                                mutate(across(TagIdAscentCount,
                                              ~ tidyr::replace_na(., 0))) %>%
                                mutate(ReAscent = ifelse(TagIdAscentCount > 1, T, F)) %>%
                                filter(RearType %in% c("H", "W")) %>%
                                group_by(Species,
                                         origin = RearType,
                                         SpawnYear, Date) %>%
                                summarise(tot_tags = n_distinct(TagId),
                                          reascent_tags = n_distinct(TagId[ReAscent]),
                                          .groups = "drop") %>%
                                group_by(Species,
                                         origin,
                                         SpawnYear) %>%
                                summarise(across(matches('tags'),
                                                 ~ sum(., na.rm = T)),
                                          .groups = "drop") %>%
                                mutate(reasc_rate = reascent_tags / tot_tags,
                                       reasc_rate_se = sqrt(reasc_rate * (1 - reasc_rate) / tot_tags)) %>%
                                select(-Species,
                                       -SpawnYear) %>%
                                return()
                            }
                          })) %>%
    unnest(reasc_df) %>%
    # if no re-ascension data, use Priest Rapids
    arrange(year,
            origin,
            dam) %>%
    fill(reasc_rate,
         reasc_rate_se,
         .direction = "down") %>%
    arrange(year,
            dam,
            origin) %>%
    # adjust for re-ascension
    mutate(adj_win_cnt = win_cnt * (1 - reasc_rate),
           adj_win_cnt_se = win_cnt * reasc_rate_se) %>%
    # crossing(origin = c("W", "H")) %>%
    # break down hatchery and wild based on upstream tags
    mutate(n_obs = map2_int(pit_code,
                            origin,
                            .f = function(x, y) {
                              tag_summ %>%
                                filter(origin == y) %>%
                                summarize(n_obs = sum(str_detect(tag_detects, x))) %>%
                                pull(n_obs)
                            })) %>%
    mutate(n_upstrm = map2_int(pit_code,
                               origin,
                               .f = function(x, y) {
                                 tag_summ %>%
                                   filter(origin == y) %>%
                                   summarize(n_path = sum(str_detect(path, paste0(" ", x)))) %>%
                                   pull(n_path)
                               }),
           n_upstrm = if_else(dam == "PriestRapids",
                              n_obs,
                              n_upstrm)) %>%
    # generate proportion hatchery/wild based on upstream tags
    group_by(dam) %>%
    mutate(prop_org = n_upstrm / sum(n_upstrm),
           prop_org_se = sqrt((prop_org * (1 - prop_org)) / sum(n_upstrm))) %>%
    # generate proportion hatchery/wild based on tags detected at dam
    # mutate(prop_org = n_obs / sum(n_obs),
    #        prop_org_se = sqrt((prop_org * (1 - prop_org)) / sum(n_obs))) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(tot_escp = win_cnt * (1 - reasc_rate) * prop_org,
           tot_escp_se = msm::deltamethod(~ x1 * (1 - x2) * x3,
                                          mean = c(win_cnt,
                                                   reasc_rate,
                                                   prop_org),
                                          cov = diag(c(0,
                                                       reasc_rate_se,
                                                       prop_org_se)^2))) %>%
    ungroup() %>%
    # add estimated transistion parameters
    left_join(trans_summ %>%
                filter(param %in% c("RIA", "RRF")) %>%
                select(origin,
                       pit_code = param,
                       trans_est = mean,
                       trans_se = sd) %>%
                bind_rows(crossing(origin = c("H", "W"),
                                   pit_code = "PRA",
                                   trans_est = 1,
                                   trans_se = 0))) %>%
    rowwise() %>%
    # calculate equivalent adjusted count at Priest
    mutate(priest_cnt = tot_escp / trans_est,
           priest_cnt_se = msm::deltamethod(~ x1 / x2,
                                            mean = c(tot_escp,
                                                     trans_est),
                                            cov = diag(c(tot_escp_se,
                                                         trans_se)^2))) %>%
    ungroup()


  for(dam_cnt_name in c("PriestRapids",
                        "RockIsland")) {

    cat(paste("\t Using", dam_cnt_name, "dam \n\n"))

    org_escape <- dam_escp_df %>%
      filter(dam == dam_cnt_name) %>%
      mutate(Species = "Steelhead") %>%
      select(Species,
             SpawnYear = year,
             dam,
             origin,
             reasc_rate,
             tot_escp = priest_cnt,
             tot_escp_se = priest_cnt_se)

    # generate MCMC draws of total abundance by origin
    abund_post <-
      org_escape |>
      crossing(chain = 1:max(trans_df$chain)) |>
      group_by(origin,
               chain) |>
      summarise(tot_esc_samp = map2(tot_escp,
                                    tot_escp_se,
                                    .f = function(x, y) {
                                      tibble(tot_abund = rnorm(max(trans_df$iter),
                                                               mean = x,
                                                               sd = y)) %>%
                                        mutate(iter = 1:n())
                                    }),
                .groups = "drop") |>
      unnest(cols = tot_esc_samp)


    # translate movement estimates to escapement
    escape_post <- calcAbundPost(trans_df,
                                 abund_post)

    escape_summ = summarisePost(escape_post,
                                abund,
                                location = param,
                                origin) %>%
      mutate(across(c(mean, median, mode, sd, skew, kurtosis, matches('CI$')),
                    ~ round(.,
                            digits = 2))) %>%
      arrange(desc(origin), location) %>%
      tibble::add_column(species = "Steelhead",
                         spawn_year = yr,
                         .before = 0)

    #-----------------------------------------------------------------
    # save some of these objects
    save(tag_summ,
         bio_df,
         trans_df,
         trans_summ,
         dam_escp_df,
         org_escape,
         escape_post,
         escape_summ,
         detect_summ,
         configuration,
         flowlines,
         parent_child,
         sites_sf,
         file = here("analysis/data/derived_data/estimates",
                     dam_cnt_name,
                     paste0("UC_Sthd_DABOM_", yr, ".rda")))
  }
}

#-----------------------------------------------------------------
# create some summaries of biological information

#-----------------------------------------------------------------
# run for set of years
for(yr in 2011:2023) {

  cat(paste("Summarizing year", yr, "\n\n"))

  if(yr %in% c(2011:2015, 2018)) {
    dam_cnt_name = "PriestRapids"
  } else {
    dam_cnt_name = "RockIsland"
  }


  load(here("analysis/data/derived_data/estimates",
            dam_cnt_name,
            paste0("UC_Sthd_DABOM_", yr, ".rda")))

  bio_summ = pit_obs %>%
    group_by(group,
             origin,
             sex,
             age) %>%
    summarise(n_tags = n_distinct(tag_code),
              mean_FL = mean(fork_length, na.rm = T),
              sd_FL = sd(fork_length, na.rm = T),
              .groups = "drop") %>%
    full_join(expand(pit_obs,
                     group,
                     origin,
                     nesting(sex, age))) %>%
    mutate(across(n_tags,
                  ~ replace_na(.,
                               0))) %>%
    group_by(group, origin) %>%
    mutate(tot_tags = sum(n_tags)) %>%
    ungroup() %>%
    mutate(prop = n_tags / tot_tags,
           prop_se = sqrt((prop * (1 - prop)) / (tot_tags))) %>%
    arrange(group, origin, sex, age)

  # population (or branch) level summary of escapement
  pop_summ = escape_post %>%
    filter(param %in% c('LWE',
                        'ENL',
                        'LMR',
                        'OKL',
                        'ICH', 'JD1', 'JDA', 'PRH', 'PRO', 'PRV', 'RSH', 'TMF')) %>%
    mutate(param = recode(param,
                          'LWE' = 'Wenatchee',
                          'ENL' = 'Entiat',
                          'LMR' = 'Methow',
                          'OKL' = 'Okanogan',
                          'ICH' = 'BelowPriest',
                          'JD1' = 'BelowPriest',
                          'JDA' = 'BelowPriest',
                          'PRH' = 'BelowPriest',
                          'PRO' = 'BelowPriest',
                          'PRV' = 'BelowPriest',
                          'RSH' = 'BelowPriest',
                          'TMF' = 'BelowPriest')) %>%
    mutate(param = factor(param,
                          levels = c("Wenatchee",
                                     "Entiat",
                                     "Methow",
                                     "Okanogan",
                                     "BelowPriest"))) %>%
    group_by(chain, iter, origin, param) %>%
    summarize(across(abund,
                     sum),
              .groups = "drop") %>%
    summarisePost(abund,
                  origin,
                  group = param) %>%
    mutate(across(c(mean, median, mode, sd, skew, kurtosis, matches('CI$')),
                  ~ round(., digits = 2))) %>%
    arrange(desc(origin), group) %>%
    tibble::add_column(species = "Steelhead",
                       spawn_year = yr,
                       .before = 0)

  #------------------------------------------------------------
  # estimates of abundance by population, origin, sex and age
  full_summ = pop_summ %>%
    # filter(group != "WellsPool") %>%
    select(species, spawn_year, group,
           origin, escape = mean, escp_se = sd) %>%
    left_join(bio_summ) %>%
    rowwise() %>%
    mutate(est = escape * prop,
           est_se = deltamethod(~ x1 * x2,
                                mean = c(escape, prop),
                                cov = diag(c(escp_se, prop_se)^2))) %>%
    ungroup() %>%
    mutate(across(est,
                  ~ as.integer(round(.)))) %>%
    select(species:origin, sex, age, n_tags, mean_FL,
           starts_with("prop"),
           starts_with("est"))

  #------------------------------------------------------------
  # sex proportions by origin
  sex_origin_summ = pop_summ %>%
    # filter(group != "WellsPool") %>%
    select(species, spawn_year, group,
           origin, escape = mean, escp_se = sd) %>%
    left_join(pit_obs %>%
                group_by(group, origin, sex) %>%
                summarise(n_tags = n_distinct(tag_code[!is.na(sex)]),
                          .groups = "drop") %>%
                filter(!is.na(sex)) %>%
                full_join(expand(pit_obs,
                                 group,
                                 origin, sex)) %>%
                mutate(across(n_tags,
                              replace_na,
                              0)) %>%
                group_by(group, origin) %>%
                mutate(total_sexed = sum(n_tags)) %>%
                mutate(prop = n_tags / total_sexed,
                       prop_se = sqrt((prop * (1 - prop)) / total_sexed)) %>%
                ungroup() %>%
                arrange(group, origin, sex)) %>%
    rowwise() %>%
    mutate(est = escape * prop,
           est_se = deltamethod(~ x1 * x2,
                                mean = c(escape, prop),
                                cov = diag(c(escp_se, prop_se)^2))) %>%
    ungroup() %>%
    select(species:origin, sex, n_tags, total_sexed, starts_with('prop'), starts_with("est")) %>%
    arrange(species, spawn_year, group, origin, sex)

  # sex proportions overall
  sex_summ = pop_summ %>%
    # filter(group != "WellsPool") %>%
    group_by(species, spawn_year, group) %>%
    summarize(escape = sum(mean),
              escp_se = sqrt(sum(sd^2)),
              .groups = "drop") %>%
    left_join(pit_obs %>%
                group_by(group, sex) %>%
                summarise(n_tags = n_distinct(tag_code[!is.na(sex)]),
                          .groups = "drop") %>%
                filter(!is.na(sex)) %>%
                full_join(expand(tag_summ,
                                 group,
                                 sex)) %>%
                mutate(across(n_tags,
                              replace_na,
                              0)) %>%
                group_by(group) %>%
                mutate(total_sexed = sum(n_tags)) %>%
                mutate(prop = n_tags / total_sexed,
                       prop_se = sqrt((prop * (1 - prop)) / total_sexed)) %>%
                ungroup() %>%
                arrange(group, sex)) %>%
    rowwise() %>%
    mutate(est = escape * prop,
           est_se = deltamethod(~ x1 * x2,
                                mean = c(escape, prop),
                                cov = diag(c(escp_se, prop_se)^2))) %>%
    ungroup() %>%
    select(species:group, sex, n_tags, total_sexed, starts_with('prop'), starts_with("est")) %>%
    arrange(species, spawn_year, group, sex)

  # combine sex summaries
  sex_all_summ = sex_origin_summ %>%
    bind_rows(sex_summ %>%
                mutate(origin = "All")) %>%
    mutate(across(origin,
                  factor,
                  levels = c("W", 'H', "All"))) %>%
    arrange(group, origin, sex) %>%
    mutate(across(est,
                  round))

  #------------------------------------------------------------
  # age proportions by origin
  age_origin_summ = pop_summ %>%
    # filter(group != "WellsPool") %>%
    select(species, spawn_year, group,
           origin, escape = mean, escp_se = sd) %>%
    left_join(tag_summ %>%
                group_by(group, origin, age) %>%
                summarise(n_tags = n_distinct(tag_code[!is.na(age)]),
                          .groups = "drop") %>%
                filter(!is.na(age)) %>%
                full_join(expand(tag_summ,
                                 group,
                                 origin, age)) %>%
                mutate(across(n_tags,
                              replace_na,
                              0)) %>%
                group_by(group, origin) %>%
                mutate(total_aged = sum(n_tags)) %>%
                mutate(prop = n_tags / total_aged,
                       prop_se = sqrt((prop * (1 - prop)) / total_aged)) %>%
                ungroup() %>%
                arrange(group, origin, age)) %>%
    rowwise() %>%
    mutate(est = escape * prop,
           est_se = deltamethod(~ x1 * x2,
                                mean = c(escape, prop),
                                cov = diag(c(escp_se, prop_se)^2))) %>%
    ungroup() %>%
    select(species:origin, age, n_tags, total_aged, starts_with('prop'), starts_with("est")) %>%
    arrange(species, spawn_year, group, origin, age)

  # age proportions overall
  age_summ = pop_summ %>%
    # filter(group != "WellsPool") %>%
    group_by(species, spawn_year, group) %>%
    summarize(escape = sum(mean),
              escp_se = sqrt(sum(sd^2)),
              .groups = "drop") %>%
    left_join(tag_summ %>%
                group_by(group, age) %>%
                summarise(n_tags = n_distinct(tag_code[!is.na(age)]),
                          .groups = "drop") %>%
                filter(!is.na(age)) %>%
                full_join(expand(tag_summ,
                                 group,
                                 age)) %>%
                mutate(across(n_tags,
                              replace_na,
                              0)) %>%
                group_by(group) %>%
                mutate(total_aged = sum(n_tags)) %>%
                mutate(prop = n_tags / total_aged,
                       prop_se = sqrt((prop * (1 - prop)) / total_aged)) %>%
                ungroup() %>%
                arrange(group, age)) %>%
    rowwise() %>%
    mutate(est = escape * prop,
           est_se = deltamethod(~ x1 * x2,
                                mean = c(escape, prop),
                                cov = diag(c(escp_se, prop_se)^2))) %>%
    ungroup() %>%
    select(species:group, age, n_tags, total_aged, starts_with('prop'), starts_with("est")) %>%
    arrange(species, spawn_year, group, age)

  # combine age summaries
  age_all_summ = age_origin_summ %>%
    bind_rows(age_summ %>%
                mutate(origin = "All")) %>%
    mutate(across(origin,
                  factor,
                  levels = c("W", 'H', "All"))) %>%
    arrange(group, origin, age) %>%
    mutate(across(est,
                  round))

  #------------------------------------------------------------
  # origin proportion based on tags observed in each branch/population
  org_summ = tag_summ %>%
    filter(group %in% pop_summ$group) %>%
    mutate(across(group,
                  fct_drop)) %>%
    group_by(group, origin) %>%
    summarise(n_tags = n_distinct(tag_code),
              .groups = "drop") %>%
    pivot_wider(names_from = "origin",
                values_from = "n_tags",
                values_fill = 0) %>%
    mutate(prop_H = H / (H + W),
           prop_W = 1 - prop_H,
           prop_se = sqrt((prop_W * (1 - prop_W)) / (W + H)))

  # # origin proportion based on DABOM estimates of abundance in each branch/population
  # org_summ = pop_summ %>%
  #   select(species:mean, sd) %>%
  #   pivot_wider(names_from = "origin",
  #               values_from = c("mean", "sd")) %>%
  #   mutate(prop_W = mean_W / (mean_W + mean_H),
  #          prop_H = 1 - prop_W) %>%
  #   rowwise() %>%
  #   mutate(prop_se = msm::deltamethod(~ x1 / (x1 + x2),
  #                                     mean = c(mean_W, mean_H),
  #                                     cov = diag(c(sd_W, sd_H)^2))) %>%
  #   arrange(group)

  # biological summaries, based on tags detected within each stream / population
  bio_list = list('Origin' = org_summ,
                  'Sex' = sex_all_summ,
                  "Sexed Tags" = sex_all_summ %>%
                    select(-starts_with(c("prop", "est"))) %>%
                    pivot_wider(names_from = "sex",
                                values_from = "n_tags",
                                values_fill = 0,
                                names_sort = T),
                  'Age' = age_all_summ,
                  'Aged Tags' = age_all_summ %>%
                    select(-starts_with(c("prop", "est"))) %>%
                    pivot_wider(names_from = "age",
                                values_from = "n_tags",
                                values_fill = 0,
                                names_sort = T),
                  'Biological Summary' = full_summ)


  #-----------------------------------------------------------------
  # break down hatchery estimates by mark type
  #-----------------------------------------------------------------
  mark_tag_summ = tag_summ %>%
    filter(!is.na(ad_clip)) |>
    # filter(origin == "H") %>%
    # mutate(cwt = if_else(is.na(cwt), F,
    #                      if_else(cwt %in% c("SN", "BD"),
    #                              T, NA)),
    #        ad_clip = if_else(is.na(ad_clip), F,
    #                          if_else(ad_clip == "AD", T, NA))) %>%
    mutate(ad_clip_chr = recode(as.character(ad_clip),
                                "TRUE" = "AD",
                                "FALSE" = "AI"),
           cwt_chr = recode(as.character(cwt),
                            "TRUE" = "CWT",
                            "FALSE" = "noCWT")) %>%
    mutate(across(cwt,
                  ~ if_else(origin == "W",
                            F,
                            .)),
           across(cwt_chr,
                  ~ if_else(origin == "W",
                            "noCWT",
                            .))) |>
    tidyr::unite("mark_grp", ad_clip_chr, cwt_chr, remove = T) %>%
    mutate(mark_grp = if_else(origin == "W",
                              "Wild",
                              mark_grp))

  # proportions of each type of fish (H/W, Ad-clip, CWT combinations) past each site
  node_order <- buildNodeOrder(parent_child)

  mark_grp_prop = mark_tag_summ %>%
    # mutate(spawn_site = str_remove(spawn_node, "B0$"),
    #        spawn_site = str_remove(spawn_site, "A0$"),
    #        spawn_site = recode(spawn_site,
    #                            "S" = "SA0")) %>%
    mutate(spawn_site = str_remove(final_node, "_U"),
           across(spawn_site,
                  ~ str_remove(., "_D"))) |>
    left_join(node_order |>
                separate(path,
                         into = paste("site", 1:max(node_order$node_order), sep = "_")) |>
                pivot_longer(starts_with('site_'),
                             names_to = "name",
                             values_to = "site_code") %>%
                filter(!is.na(site_code)) %>%
                select(spawn_site = node,
                       site_code),
              by = join_by(spawn_site),
              relationship = "many-to-many") %>%
    group_by(origin, site_code, ad_clip, cwt, mark_grp) %>%
    summarise(n_tags = n_distinct(tag_code),
              .groups = "drop") %>%
    right_join(crossing(site_code = union(parent_child$parent, parent_child$child),
                        expand(mark_tag_summ, nesting(origin, ad_clip, cwt, mark_grp)))) %>%
    arrange(site_code, mark_grp) %>%
    mutate(across(n_tags,
                  replace_na,
                  0)) %>%
    group_by(origin, site_code) %>%
    mutate(tot_tags = sum(n_tags),
           prop = n_tags / tot_tags,
           # using normal approximation
           prop_se = sqrt((prop * (1 - prop))/tot_tags)) %>%
    ungroup()

  # generate posterior samples of mark proportions
  set.seed(6)
  n_iter = max(escape_post$iter)
  prop_samps = mark_grp_prop %>%
    filter(tot_tags > 0) %>%
    group_by(origin, site_code,
             tot_tags) %>%
    nest() %>%
    mutate(samp = map(data,
                      .f = function(x) {
                        rmultinom(n_iter, sum(x$n_tags), x$prop) %>%
                          set_colnames(1:ncol(.)) %>%
                          set_rownames(x$mark_grp) %>%
                          as_tibble(rownames = 'mark_grp') %>%
                          pivot_longer(-1,
                                       names_to = "iter",
                                       values_to = "n_tags") %>%
                          mutate(across(iter,
                                        as.integer)) %>%
                          group_by(iter) %>%
                          mutate(prop = n_tags / sum(n_tags)) %>%
                          arrange(iter, mark_grp) %>%
                          ungroup()
                      })) %>%
    ungroup() %>%
    select(-data) %>%
    unnest(samp) %>%
    left_join(mark_grp_prop %>%
                select(-n_tags,
                       -starts_with("prop"))) %>%
    bind_rows(mark_grp_prop %>%
                filter(tot_tags == 0) %>%
                crossing(iter = 1:n_iter) %>%
                select(-prop_se)) %>%
    arrange(site_code, origin, mark_grp, iter) %>%
    select(iter, any_of(names(mark_grp_prop)))

  # posterior samples
  mark_post = escape_post %>%
    inner_join(prop_samps,
               by = c("iter", "origin",
                      "param" = "site_code")) %>%
    mutate(across(c(prop,
                    n_tags,
                    tot_tags),
                  replace_na,
                  0)) %>%
    mutate(prop = if_else(value == 0,
                          0,
                          prop)) %>%
    mutate(n_fish = abund * prop)

  mark_grp_summ = mark_post |>
    summarisePost(n_fish,
                  origin,
                  location = param,
                  ad_clip,
                  cwt,
                  mark_grp) |>
    left_join(mark_grp_prop %>%
                rename(location = site_code,
                       proportion = prop)) %>%
    select(origin:mark_grp,
           n_tags:proportion,
           prop_se,
           everything()) %>%
    arrange(location, mark_grp)

  pop_mark_grp = mark_grp_summ %>%
    filter(location %in% c("LWE", 'ENL', 'LMR', 'OKL')) %>%
    mutate(population = recode(location,
                               'LWE' = 'Wenatchee',
                               'ENL' = 'Entiat',
                               'LMR' = 'Methow',
                               'OKL' = 'Okanogan'),
           population = factor(population,
                               levels = c('Wenatchee',
                                          'Entiat',
                                          'Methow',
                                          'Okanogan'))) %>%
    arrange(population, mark_grp) %>%
    select(origin, population, everything(), -location)

  # biological summary that includes mark group
  mark_grp_bio_summ = mark_tag_summ %>%
    group_by(group,
             mark_grp,
             origin,
             sex,
             age) %>%
    summarise(n_tags = n_distinct(tag_code),
              mean_FL = mean(fork_length, na.rm = T),
              sd_FL = sd(fork_length, na.rm = T),
              .groups = "drop") %>%
    full_join(expand(mark_tag_summ,
                     group,
                     nesting(mark_grp, origin),
                     nesting(sex, age))) %>%
    mutate(across(n_tags,
                  replace_na,
                  0)) %>%
    group_by(group, origin, mark_grp) %>%
    mutate(tot_tags = sum(n_tags)) %>%
    ungroup() %>%
    mutate(prop = n_tags / tot_tags,
           prop_se = sqrt((prop * (1 - prop)) / (tot_tags))) %>%
    arrange(group, origin, mark_grp, sex, age)

  mark_grp_list = list('Mark Group Population' = pop_mark_grp %>%
                         select(-skew, -kurtosis) %>%
                         mutate_at(vars(mean:mode),
                                   list(round),
                                   digits = 0) %>%
                         mutate_at(vars(se:upperCI),
                                   list(round),
                                   digits = 1) %>%
                         rename(estimate = mean) %>%
                         select(-median, -mode),
                       'Mark Group Site Escapement' = mark_grp_summ %>%
                         select(-skew, -kurtosis) %>%
                         mutate_at(vars(mean:mode),
                                   list(round),
                                   digits = 0) %>%
                         mutate_at(vars(se:upperCI),
                                   list(round),
                                   digits = 1) %>%
                         rename(estimate = mean) %>%
                         select(-median, -mode),
                       'Mark Group Tag Summary' = mark_tag_summ,
                       'Mark Group Bio Summary' = mark_grp_bio_summ)

  #-----------------------------------------------------------------
  # pull together information about the tag rate at Priest
  priest_tag_rate <- dam_escp_df |>
    filter(dam == "PriestRapids") |>
    mutate(run_year = year - 1) |>
    select(run_year,
           spawn_year = year,
           win_cnt,
           origin,
           contains("reasc_rate"),
           contains("priest_cnt")) |>
    mutate(total = sum(priest_cnt),
           total_se = sqrt(sum(priest_cnt_se^2))) |>
    left_join(bio_df |>
                group_by(origin) |>
                summarize(n_tags = n_distinct(tag_code),
                          .groups = "drop") |>
                mutate(total_tags = sum(n_tags)),
              by = "origin") |>
    mutate(across(origin,
                  recode,
                  "H" = "Hatchery",
                  "W" = "Natural")) |>
    rlang::set_names(nm = function(x) {
      x |>
        str_replace("_", " ") |>
        str_to_title()
    }) |>
    rename(`PRD Ladder Count` = `Win Cnt`,
           `Adult Reascension Adjustment Rate` = `Reasc Rate`,
           `Reascension Rate SE` = `Reasc Rate_se`,
           `Priest Est` = `Priest Cnt`,
           `Priest SE` = `Priest Cnt_se`,
           `Total SE` = `Total Se`)

  if(!yr %in% c(2011:2015, 2018)) {
    priest_tag_rate %<>%
      left_join(dam_escp_df %>%
                  filter(dam == "RockIsland") %>%
                  select(origin,
                         #contains("reasc_rate"),
                         contains("priest_cnt"),
                         dam) %>%
                  mutate(total = sum(priest_cnt),
                         total_se = sqrt(sum(priest_cnt_se^2))) %>%
                  relocate(dam,
                           .after = "total_se") %>%
                  mutate(across(origin,
                                recode,
                                "H" = "Hatchery",
                                "W" = "Natural"),
                         across(dam,
                                recode,
                                "RockIsland" = "Rock Island")) %>%
                  rlang::set_names(nm = function(x) {
                    x %>%
                      str_replace("_", " ") %>%
                      str_to_title()
                  }) %>%
                  rename(#`PRD Ladder Count` = `Win Cnt`,
                    # `Adult Reascension Adjustment Rate` = `Reasc Rate`,
                    # `Reascension Rate SE` = `Reasc Rate_se`,
                    `Modelled Priest Est` = `Priest Cnt`,
                    `Modelled Priest SE` = `Priest Cnt_se`,
                    `Modelled Total` = Total,
                    `Modelled Total SE` = `Total Se`,
                    `Dam Method` = Dam),
                by = "Origin") %>%
      relocate(c(`N Tags`,
                 `Total Tags`),
               .after = "Dam Method") %>%
      mutate(`Tag Rate` = if_else(is.na(`Dam Method`),
                                  `N Tags` / `Priest Est`,
                                  `N Tags` / `Modelled Priest Est`),
             `Total Tag Rate` = if_else(is.na(`Dam Method`),
                                        `Total Tags` / Total,
                                        `Total Tags` / `Modelled Total`))
  } else {
    priest_tag_rate %<>%
      mutate(`Dam Method` = "Priest Rapids") %>%
      relocate(c(`N Tags`,
                 `Total Tags`),
               .after = "Dam Method") %>%
      mutate(`Tag Rate` = `N Tags` / `Priest Est`,
             `Total Tag Rate` = `Total Tags` / Total)
  }



  #-----------------------------------------------------------------
  # write results to an Excel file
  save_list = c(list('Population Escapement' = pop_summ %>%
                       select(-skew, -kurtosis) %>%
                       mutate(across(mean:mode,
                                     janitor::round_half_up)) %>%
                       mutate(across(sd:upperCI,
                                     round,
                                     digits = 1)) %>%
                       rename(estimate = mean,
                              se = sd) %>%
                       select(-median, -mode),
                     'All Escapement' = escape_summ %>%
                       select(-skew, -kurtosis) %>%
                       mutate(across(mean:mode,
                                     janitor::round_half_up)) %>%
                       mutate(across(sd:upperCI,
                                     round,
                                     digits = 1)) %>%
                       rename(estimate = mean,
                              se = sd) %>%
                       select(-median, -mode),
                     'Detection' = detect_summ %>%
                       mutate(across(-c(node, n_tags),
                                     round,
                                     digits = 3)) %>%
                       rename(estimate = mean,
                              se = sd) %>%
                       select(-median, -mode),
                     'Tag Summary' = tag_summ,
                     "Priest Rapids Escapement" = priest_tag_rate |>
                       mutate(across(ends_with("Year"),
                                     as.integer)) |>
                       mutate(across(c("PRD Ladder Count",
                                       ends_with("Priest Est"),
                                       ends_with("Total")),
                                     ~ as.integer(round_half_up(.)))) |>
                       mutate(across(where(is.double),
                                     round,
                                     digits = 3))),
                bio_list,
                mark_grp_list)

  # save results to be accessed later
  save(save_list,
       # detect_summ,
       # trans_summ,
       # escape_summ,
       pop_summ,
       dam_cnt_name,
       # dam_escp_df,
       # configuration,
       # flowlines,
       # parent_child,
       # sites_sf,
       file = here(paste0('analysis/data/derived_data/estimates/', dam_cnt_name),
                   paste0("PRA_DABOM_Steelhead_", yr, ".rda")))

  excel_file_path = if_else(dam_cnt_name == "PriestRapids",
                            'outgoing/estimates',
                            paste0('outgoing/estimates/', dam_cnt_name, "Cnts"))

  writexl::write_xlsx(x = save_list,
                      path = here(excel_file_path,
                                  paste0('UC_Steelhead_', yr, '_', format(Sys.Date(), '%Y%m%d'), '.xlsx')))

  # excel_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/Data Requests/"
  #
  # writexl::write_xlsx(x = save_list,
  #                     path = here(excel_file_path,
  #                                 paste0('UC_Steelhead_', yr, '_', format(Sys.Date(), '%Y%m%d'), '4FWS_CCT.xlsx')))

}



#---------------------------------------------------
# compare estimates with some dam counts
#---------------------------------------------------

# query dam counts at Priest, Rock Island, Rocky Reach, Wells and Tumwater
dam_cnts = tibble(dam = c("RockIsland",
                          "RockyReach",
                          "Wells",
                          "Tumwater"),
                  dam_code = c("RIS",
                               "RRH",
                               "WEL",
                               "TUM")) %>%
  crossing(spp = c("Steelhead",
                   "Wild_Steelhead")) %>%
  mutate(win_cnt = map2_dbl(dam_code,
                            spp,
                            .f = function(x, y) {
                              STADEM::getWindowCounts(dam = x,
                                                      spp = y,
                                                      start_date = start_date,
                                                      end_date = end_date) %>%
                                summarise_at(vars(win_cnt),
                                             list(sum),
                                             na.rm = T) %>%
                                pull(win_cnt)
                            })) %>%
  pivot_wider(names_from = spp,
              values_from = win_cnt) %>%
  rename(All = Steelhead,
         W = Wild_Steelhead) %>%
  mutate(H = All - W) %>%
  pivot_longer(cols = c("All", "W", 'H'),
               names_to = 'origin',
               values_to = "win_cnt")

dam_est = escape_summ %>%
  filter(location %in% c("RIA",
                         "RRF",
                         "WEA",
                         "TUM")) %>%
  mutate(dam = recode(location,
                      "RIA" = "RockIsland",
                      "RRF" = "RockyReach",
                      "WEA" = "Wells",
                      "TUM" = "Tumwater"))
comp_df = dam_est %>%
  group_by(dam) %>%
  summarize(across(c(mean:mode, ends_with("ci")),
                   sum),
            .groups = "drop") %>%
  mutate(origin = "All") %>%
  bind_rows(dam_est %>%
              select(dam,
                     origin,
                     mean:mode,
                     ends_with("ci"))) %>%
  left_join(dam_cnts) %>%
  rowwise() %>%
  mutate(larger = if_else(win_cnt > mean,
                          "Counts",
                          "DABOM")) %>%
  mutate(in_ci = between(win_cnt, lower_ci, upper_ci)) %>%
  ungroup() %>%
  mutate(dam = fct_relevel(dam,
                           "Tumwater",
                           after = Inf)) %>%
  arrange(dam,
          origin)

# add adjustments if re-ascension rate is the same everywhere as it is at Priest
comp_df <-
  comp_df |>
  left_join(org_escape |>
              select(origin,
                     reasc_rate)) |>
  mutate(adj_win_cnt = win_cnt * (1 - reasc_rate))

comp_df %>%
  ggplot(aes(x = dam,
             y = mean,
             color = "DABOM")) +
  geom_errorbar(aes(ymin = lower_ci,
                    ymax = upper_ci),
                width = 0) +
  geom_point(size = 3) +
  geom_point(aes(y = win_cnt,
                 color = "Dam Count"),
             size = 3,
             position = position_dodge(width = 1)) +
  geom_point(aes(y = adj_win_cnt,
                 color = "Adj. Dam Count"),
             size = 3,
             position = position_dodge(width = 1)) +
  scale_color_manual(values = c("DABOM" = "gray20",
                                "Dam Count" = "red",
                                "Adj. Dam Count" = "blue"),
                     name = "Source") +
  facet_wrap(~ origin,
             scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = "bottom") +
  labs(x = "Dam",
       y = "Estimate",
       title = paste("Steelhead", yr),
       subtitle = "Using Rock Island Counts")
