# Author: Kevin See
# Purpose: format results to be saved
# Created: 11/30/22
# Last Modified: 12/11/2024
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(PITcleanr)
library(janitor)
library(readxl)
library(writexl)
library(magrittr)
library(msm)
library(here)
library(DescTools)
library(sroem)


# load age table
data("age_table")

#-----------------------------------------------------------------
# function to fix all table names
makeTableNms = function(df) {
  df |>
    rlang::set_names(nm = function(x) {
      x |>
        str_replace_all("_", " ") |>
        str_to_title() |>
        str_replace("Nos", "NOS") |>
        str_replace("Hos", "HOS") |>
        str_replace("Se$", "SE") |>
        str_replace("Sd$", "SD") |>
        str_replace("Cv$", "CV") |>
        str_replace("Fpr", "FpR") |>
        str_replace("Phos", "pHOS") |>
        str_replace("Lowerci", "LCI") |>
        str_replace("Upperci", "UCI") |>
        str_replace("Upper Ci", "UCI") |>
        str_replace("Lower Ci", "LCI") |>
        str_replace("Lci", "LCI") |>
        str_replace("Uci", "UCI") |>
        str_replace("Cwt$", "CWT")
    }) %>%
    return()
}

#-----------------------------------------------------------------
# set the highest year to be included
max_yr = 2024
# max_yr = lubridate::year(lubridate::today()) - 1

#-----------------------------------------------------------------
# generate tables of redds and spawners

# set some thresholds
# minimum number of total redds observed
min_redds = 2
# minimum number of weeks with at least one new redd observed
min_non0_wks = 3


#-----------------------------------------------------------------
spwn_est <-
  crossing(population = c("Wenatchee",
                          "Methow"),
           spawn_year = 2014:max_yr) |>
           # spawn_year = 2020) |>
  mutate(run_year = spawn_year - 1) %>%
  select(run_year, spawn_year, population) %>%
  mutate(results_list = map2(spawn_year,
                             population,
                             .f = possibly(function(yr, pop) {

                               message(paste("Prepping data from", pop, yr, "\n\n"))

                               rm(redd_df,
                                  # wen_tags,
                                  # met_tags,
                                  basin_tags,
                                  sex_err,
                                  fpr_df,
                                  trib_spawners,
                                  # escp_wen,
                                  # escp_met,
                                  escp_est,
                                  rem_df,
                                  trib_NAs)

                               phos_method = case_when(pop == "Wenatchee" ~ "escapement",
                                                       pop == "Methow" ~ "tags",
                                                       .default = NA_character_)

                               prep_uc_sthd_data(basin = pop,
                                                 query_year = yr,
                                                 n_observers = "two",
                                                 phos_data = phos_method,
                                                 save_rda = F)

                               # for tributary estimates, make any 0s NAs
                               trib_NAs <-
                                 trib_spawners |>
                                 filter(spawners == 0,
                                        spawners_se == 0) |>
                                 select(origin,
                                        river = location) |>
                                 mutate(across(origin,
                                               ~ recode(.,
                                                        "Hatchery" = "na_hos",
                                                        "Natural" = "na_nos")),
                                        value = T) |>
                                 pivot_wider(names_from = origin,
                                             values_from = value)

                               if(nrow(trib_NAs) == 0) {
                                 trib_NAs <- tibble(river = NA_character_,
                                                    na_hos = NA,
                                                    na_nos = NA)
                               }

                               # for 2020, we'll need to do something different
                               if(!(pop == 'Wenatchee' & yr == 2020)) {

                                 results_lst <- summarize_redds(redd_df,
                                                                species = "Steelhead",
                                                                group_vars = c("river", "reach", "index", "survey_type"),
                                                                summ_vars = c("river", "location", "index"),
                                                                min_non0_wks = min_non0_wks,
                                                                min_redds = min_redds,
                                                                gauc = T,
                                                                add_zeros = T,
                                                                use_cor = T)


                                 redd_results <- results_lst$rch_est |>
                                   arrange(river,
                                           reach,
                                           index) %>%
                                   relocate(river, reach, location,
                                            .before = 1) |>
                                   mutate(location = if_else(location == "Tributaries",
                                                             as.character(river),
                                                             location))

                                 spwn_rch = redd_results %>%
                                   select(river, location, reach, index, redd_est, redd_se) %>%
                                   left_join(fpr_df %>%
                                               select(location, starts_with('fpr')),
                                             by = "location") %>%
                                   left_join(fpr_df %>%
                                               select(location, starts_with('phos')),
                                             by = "location") %>%
                                   rowwise() %>%
                                   mutate(Tot_Spawners = redd_est * fpr,
                                          Hatchery = redd_est * fpr * phos,
                                          Natural = redd_est * fpr * (1 - phos),
                                          Hatchery_SE = ifelse(Tot_Spawners > 0,
                                                               deltamethod(~ x1 * x2 * x3,
                                                                           mean = c(redd_est, fpr, phos),
                                                                           cov = diag(c(redd_se, fpr_se, phos_se)^2)),
                                                               NA),
                                          Natural_SE = ifelse(Tot_Spawners > 0,
                                                              deltamethod(~ x1 * x2 * (1 - x3),
                                                                          mean = c(redd_est, fpr, phos),
                                                                          cov = diag(c(redd_se, fpr_se, phos_se)^2)),
                                                              NA),
                                          Hatchery_lci = qnorm(0.025, Hatchery, Hatchery_SE),
                                          Hatchery_uci = qnorm(0.975, Hatchery, Hatchery_SE),
                                          Natural_lci = qnorm(0.025, Natural, Natural_SE),
                                          Natural_uci = qnorm(0.975, Natural, Natural_SE)) %>%
                                   ungroup() %>%
                                   mutate(reach = factor(reach,
                                                         levels = c(paste0('W', 1:10),
                                                                    "C1",
                                                                    "N1",
                                                                    "P1",
                                                                    paste0("MRW", 1:8),
                                                                    "MH1",
                                                                    "WN1",
                                                                    "T1"))) %>%
                                   select(river, location, reach,
                                          type = index,
                                          Hatchery:Natural_uci) %>%
                                   mutate(across(type,
                                                 ~ recode(.,
                                                          'Y' = 'Index',
                                                          'N' = 'Non-Index')),
                                          across(location,
                                                 ~ fct_recode(.,
                                                              "Below Tumwater (mainstem)" = "Below Tumwater",
                                                              "Above Tumwater (mainstem)" = "Above Tumwater"))) %>%
                                   bind_rows(trib_spawners %>%
                                               rename(spawners_SE = spawners_se) %>%
                                               pivot_wider(names_from = origin,
                                                           values_from = c(spawners,
                                                                           spawners_SE,
                                                                           lci, uci),
                                                           names_glue = "{origin}_{.value}") %>%
                                               rlang::set_names(function(x) str_remove(x, "_spawners")) %>%
                                               mutate(river = location) %>%
                                               mutate(type = 'DABOM') %>%
                                               select(-spawn_year)) %>%
                                   mutate(across(c(Hatchery, Natural),
                                                 round_half_up)) %>%
                                   mutate(across(ends_with("lci"),
                                                 ~ if_else(. < 0, 0, .))) |>
                                   mutate(location = factor(location,
                                                            levels = c('Below Tumwater (mainstem)',
                                                                       'Above Tumwater (mainstem)',
                                                                       'Icicle',
                                                                       'Peshastin',
                                                                       'Mission',
                                                                       'Chumstick',
                                                                       'Chiwaukum',
                                                                       'Chiwawa',
                                                                       'Nason',
                                                                       'Little Wenatchee',
                                                                       'White River',
                                                                       "Lower Methow",
                                                                       "Gold",
                                                                       "Libby",
                                                                       "Methow Fish Hatchery",
                                                                       "Upper Methow",
                                                                       "Twisp",
                                                                       "Chewuch",
                                                                       "Spring Creek",
                                                                       "Beaver"))) %>%
                                   arrange(location, reach, type) %>%
                                   mutate(h_cv = Hatchery_SE / Hatchery,
                                          n_cv = Natural_SE / Natural)


                                 redd_spwn_yr <- redd_results |>
                                   mutate(reach_type = recode(index,
                                                              'Y' = 'Index',
                                                              'N' = 'Non-Index')) |>
                                   left_join(fpr_df %>%
                                               select(location,
                                                      starts_with("fpr")),
                                             by = "location") |>
                                   select(river,
                                          reach,
                                          reach_type,
                                          observed_redds = tot_feat,
                                          redd_est,
                                          redd_se,
                                          fpr,
                                          fpr_se) %>%
                                   mutate(redd_cv = redd_se / redd_est) %>%
                                   relocate(redd_cv,
                                            .before = fpr) %>%
                                   left_join(spwn_rch %>%
                                               select(river,
                                                      reach,
                                                      reach_type = type,
                                                      NOS_est = Natural,
                                                      NOS_se = Natural_SE,
                                                      NOS_cv = n_cv,
                                                      HOS_est = Hatchery,
                                                      HOS_se = Hatchery_SE,
                                                      HOS_cv = h_cv),
                                             by = c("river", "reach", "reach_type"))

                                 # lump all mainstem reaches together
                                 if(pop == "Methow") {
                                   spwn_yr <- spwn_rch |>
                                     group_by(river) |>
                                     summarize(across(c(nos_est = Natural,
                                                        hos_est = Hatchery),
                                                      ~ sum(., na.rm = T)),
                                               across(c(nos_se = Natural_SE,
                                                        hos_se = Hatchery_SE),
                                                      ~ sqrt(sum(.^2, na.rm = T))),
                                               .groups = "drop") %>%
                                     mutate(nos_lci = qnorm(0.025, nos_est, nos_se),
                                            nos_uci = qnorm(0.975, nos_est, nos_se),
                                            hos_lci = qnorm(0.025, hos_est, hos_se),
                                            hos_uci = qnorm(0.975, hos_est, hos_se),
                                            nos_cv = nos_se / nos_est,
                                            hos_cv = hos_se / hos_est) %>%
                                     mutate(total_spawners = nos_est + hos_est) |>
                                     mutate(across(river,
                                                   recode,
                                                   "Wenatchee" = "Wenatchee (mainstem)",
                                                   "Methow" = "Lower Methow (mainstem)"),
                                            across(river,
                                                   as.factor),
                                            across(river,
                                                   ~ fct_relevel(.,
                                                                 "Wenatchee (mainstem)",
                                                                 "Lower Methow (mainstem)",
                                                                 after = Inf))) |>
                                     arrange(river) |>
                                     select(river,
                                            total_spawners,
                                            starts_with("nos"),
                                            starts_with("hos"))
                                 }

                                 # split mainstem Wenatchee into reaches above/below Tumwater
                                 if(pop == "Wenatchee") {
                                   spwn_yr <- spwn_rch |>
                                     group_by(river,
                                              location) |>
                                     summarize(across(c(nos_est = Natural,
                                                        hos_est = Hatchery),
                                                      ~ sum(.,
                                                            na.rm = T)),
                                               across(c(nos_se = Natural_SE,
                                                        hos_se = Hatchery_SE),
                                                      ~ sqrt(sum(.^2, na.rm = T))),
                                               .groups = "drop") %>%
                                     mutate(nos_lci = qnorm(0.025, nos_est, nos_se),
                                            nos_uci = qnorm(0.975, nos_est, nos_se),
                                            hos_lci = qnorm(0.025, hos_est, hos_se),
                                            hos_uci = qnorm(0.975, hos_est, hos_se),
                                            nos_cv = nos_se / nos_est,
                                            hos_cv = hos_se / hos_est) %>%
                                     mutate(across(ends_with("lci"),
                                                   ~ if_else(. < 0, 0, .))) |>
                                     mutate(total_spawners = nos_est + hos_est) |>
                                     mutate(across(river,
                                                   ~ recode(.,
                                                            "Wenatchee" = "Wenatchee (mainstem)",
                                                            "Methow" = "Lower Methow (mainstem)",
                                                            "Lower Methow" = "Lower Methow (mainstem)")),
                                            across(river,
                                                   as.factor),
                                            across(river,
                                                   ~ fct_relevel(.,
                                                                 "Wenatchee (mainstem)",
                                                                 "Lower Methow (mainstem)",
                                                                 after = Inf))) |>
                                     arrange(river,
                                             desc(location)) |>
                                     select(river = location,
                                            total_spawners,
                                            starts_with("nos"),
                                            starts_with("hos"))
                                 }

                                 spwn_yr <-
                                   spwn_yr |>
                                   left_join(trib_NAs,
                                             by = join_by(river)) |>
                                   mutate(across(c(na_hos, na_nos),
                                                 ~ replace_na(., FALSE))) |>
                                   rowwise() |>
                                   mutate(across(starts_with("nos"),
                                                 ~ if_else(na_nos & nos_est == 0, NA_real_, .)),
                                          across(starts_with("hos"),
                                                 ~ if_else(na_hos & hos_est == 0, NA_real_, .))) |>
                                   ungroup() |>
                                   select(-starts_with("na_"))

                                 spwn_yr <-
                                   spwn_yr %>%
                                   bind_rows(spwn_yr %>%
                                               summarize(river = "Total",
                                                         across(c(total_spawners,
                                                                  nos_est,
                                                                  hos_est),
                                                                ~ sum(., na.rm = T)),
                                                         across(ends_with("_se"),
                                                                ~ sqrt(sum(.^2, na.rm = T))),
                                                         .groups = "drop") %>%
                                               mutate(nos_lci = qnorm(0.025, nos_est, nos_se),
                                                      nos_uci = qnorm(0.975, nos_est, nos_se),
                                                      hos_lci = qnorm(0.025, hos_est, hos_se),
                                                      hos_uci = qnorm(0.975, hos_est, hos_se),
                                                      nos_cv = nos_se / nos_est,
                                                      hos_cv = hos_se / hos_est) %>%
                                               mutate(across(ends_with("lci"),
                                                             ~ if_else(. < 0, 0, .)))) %>%
                                   rowwise() %>%
                                   mutate(phos = hos_est / (hos_est + nos_est),
                                          phos_se = deltamethod(~ x1 / (x1 + x2),
                                                                mean = c(hos_est,
                                                                         nos_est),
                                                                cov = diag(c(hos_se,
                                                                             nos_se)^2))) %>%
                                   ungroup() %>%
                                   mutate(across(river,
                                                 as.factor),
                                          across(river,
                                                 fct_relevel,
                                                 c("Wenatchee (mainstem)",
                                                   "Above Tumwater (mainstem)",
                                                   "Below Tumwater (mainstem)",
                                                   "Methow (mainstem)",
                                                   "Total"),
                                                 after = Inf)) %>%
                                   arrange(river)

                                 # get female spawners by origin in Wenatchee
                                 if(pop == "Wenatchee") {

                                   # get pHOS of females to apply to redd estimates
                                   phos_female <-
                                     basin_tags |>
                                     filter(sex == "Female") |>
                                     count(location, origin, sex) |>
                                     group_by(location) |>
                                     mutate(phos_f = n / sum(n),
                                            phos_f_se = sqrt((phos_f * (1 - phos_f)) / sum(n))) |>
                                     ungroup() |>
                                     mutate(across(origin,
                                                   ~ case_match(.,
                                                                "H" ~ "Hatchery",
                                                                "W" ~ "Natural",
                                                                .default = .)))

                                   fem_spwn <-
                                     # female estimates in tributaries based on PIT tags
                                     trib_spawners |>
                                     mutate(across(location,
                                                   ~ case_when(. %in% unique(basin_tags$location) ~ .,
                                                               .default = "Other Tributaries"))) |>
                                     group_by(spawn_year,
                                              origin,
                                              location) |>
                                     summarise(across(spawners,
                                                      sum),
                                               across(spawners_se,
                                                      ~ sqrt(sum(.^2))),
                                               .groups = "drop") |>
                                     left_join(basin_tags |>
                                                 count(location, origin, sex) |>
                                                 group_by(location) |>
                                                 mutate(prop = n / sum(n),
                                                        prop_se = sqrt((prop * (1 - prop)) / sum(n))) |>
                                                 ungroup() |>
                                                 mutate(across(origin,
                                                               ~ case_match(.,
                                                                            "H" ~ "Hatchery",
                                                                            "W" ~ "Natural",
                                                                            .default = .))) |>
                                                 filter(str_detect(location, "Tumwater", negate = T)) |>
                                                 filter(sex == "Female"),
                                               by = join_by(origin, location)) |>
                                     rowwise() |>
                                     mutate(female_spawners = spawners * prop,
                                            female_spawners_se = msm::deltamethod(~ x1 * x2,
                                                                                  c(spawners,
                                                                                    prop),
                                                                                  diag(c(spawners_se,
                                                                                         prop_se)^2))) |>
                                     ungroup() |>
                                     mutate(source = "DABOM") |>
                                     # redd survey estimates of females
                                     bind_rows(
                                       redd_spwn_yr |>
                                         mutate(location = river) |>
                                         mutate(across(location,
                                                       ~ case_when(reach %in% paste0("W", 8:10) ~ "Above Tumwater",
                                                                   reach %in% paste0("W", 1:7) ~ "Below Tumwater",
                                                                   .default = .))) |>
                                         group_by(location) |>
                                         summarize(across(redd_est,
                                                          sum),
                                                   across(redd_se,
                                                          ~ sqrt(sum(.^2)))) |>
                                         left_join(phos_female,
                                                   by = join_by(location)) |>
                                         rowwise() |>
                                         mutate(female_spawners = redd_est * phos_f,
                                                female_spawners_se = msm::deltamethod(~ x1 * x2,
                                                                                      c(redd_est,
                                                                                        phos_f),
                                                                                      diag(c(redd_se,
                                                                                             phos_f_se)^2))) |>
                                         ungroup() |>
                                         mutate(source = "Redds")) |>
                                     select(location,
                                            source,
                                            origin,
                                            starts_with("female")) |>
                                     arrange(location,
                                             source,
                                             origin)


                                 } else {
                                   fem_spwn = NULL
                                 }

                                 list(results_lst = results_lst,
                                      fpr = fpr_df,
                                      redd_results = redd_results,
                                      spwn_rch = spwn_rch,
                                      redd_spwn_yr = redd_spwn_yr,
                                      spwn_yr = spwn_yr,
                                      fem_spwn = fem_spwn,
                                      basin_tags = basin_tags) %>%
                                   return()

                               } else if(pop == "Wenatchee" & yr == 2020) {
                                 # data from WDFW (Nate Fuchs' radio telemetry study)
                                 rt_df = tibble(subbasin = "Wenatchee",
                                                year = rep(2015:2016, each = 2),
                                                origin = rep(c("Hatchery", "Natural"), 2),
                                                ow_fish = c(20, 25, 4, 12),
                                                surv_fish = c(16, 24, 3, 9)) |>
                                   bind_rows(tibble(subbasin = "Methow",
                                                    year = rep(2015:2016, each = 2),
                                                    origin = rep(c("Hatchery", "Natural"), 2),
                                                    ow_fish = c(26, 16, 64, 13),
                                                    surv_fish = c(21, 16, 60, 12))) |>
                                   mutate(phi = surv_fish / ow_fish,
                                          phi_se = sqrt((phi * (1 - phi))/ ow_fish))

                                 # add years together
                                 rt_df %<>%
                                   bind_rows(rt_df %>%
                                               group_by(subbasin, origin) %>%
                                               summarize(across(c(ow_fish, surv_fish),
                                                                sum),
                                                         .groups = "drop") %>%
                                               mutate(phi = surv_fish / ow_fish,
                                                      phi_se = sqrt((phi * (1 - phi)) / ow_fish))) %>%
                                   mutate(across(year,
                                                 as.factor)) %>%
                                   mutate(across(year,
                                                 ~ fct_na_value_to_level(.,
                                                                         level = "Total"))) %>%
                                   arrange(subbasin,
                                           year,
                                           origin)

                                 spwn_yr <- escp_est %>%
                                   filter(str_detect(location, "_all$")) |>
                                   mutate(subbasin = case_when(str_detect(location, "Met") ~
                                                                 "Methow",
                                                               str_detect(location, "Wen") ~
                                                                 "Wenatchee",
                                                               str_detect(location, "Tumwater") ~
                                                                 "Wenatchee",
                                                               .default = NA_character_)) |>
                                   relocate(subbasin, .before = 1) %>%
                                   left_join(rem_df %>%
                                               group_by(origin) %>%
                                               summarize(across(removed,
                                                                sum),
                                                         .groups = "drop"),
                                             by = "origin") %>%
                                   mutate(across(removed,
                                                 ~ replace_na(., 0))) |>
                                   rowwise() %>%
                                   mutate(
                                     across(
                                       c(lci, uci),
                                       ~ max(0, . - removed)
                                     )) |>
                                   mutate(escp = max(0, estimate - removed)) %>%
                                   ungroup() %>%
                                   left_join(rt_df %>%
                                               filter(year == "Total") %>%
                                               select(-year,
                                                      -ends_with("fish")),
                                             by = c("subbasin", "origin")) %>%
                                   rowwise() %>%
                                   mutate(all_spwn = escp * phi) %>%
                                   mutate(all_spwn_se = msm::deltamethod(~ x1 * x2,
                                                                         mean = c(escp, phi),
                                                                         cov = diag(c(se, phi_se)^2))) %>%
                                   ungroup() %>%
                                   left_join(trib_spawners %>%
                                               group_by(origin) %>%
                                               summarize(across(spawners,
                                                                sum),
                                                         across(spawners_se,
                                                                ~ sqrt(sum(.^2))),
                                                         .groups = "drop") %>%
                                               rename(trib_spwn = spawners,
                                                      trib_se = spawners_se),
                                             by = "origin") %>%
                                   mutate(main_spwn = all_spwn - trib_spwn,
                                          main_spwn_se = sqrt(all_spwn_se^2 + trib_se^2),
                                          across(main_spwn,
                                                 ~ if_else(. < 0, 0, .))) %>%
                                   mutate(location = paste(subbasin, "(mainstem)"),
                                          river = location,
                                          type = "RT") %>%
                                   select(river,
                                          origin,
                                          location,
                                          type,
                                          spawners = main_spwn,
                                          spawners_se = main_spwn_se) %>%
                                   mutate(lci = qnorm(0.025, spawners, spawners_se),
                                          uci = qnorm(0.975, spawners, spawners_se),
                                          across(lci,
                                                 ~ if_else(. < 0, 0, .))) |>
                                   bind_rows(trib_spawners %>%
                                               select(-spawn_year) |>
                                               mutate(river = location) |>
                                               mutate(type = 'DABOM')) |>
                                   mutate(across(
                                     origin,
                                     ~ recode(.,
                                              "Hatchery" = "hos",
                                              "Natural" = "nos")
                                   )) |>
                                   mutate(cv = spawners_se / spawners) |>
                                   pivot_wider(names_from = origin,
                                               values_from = c(spawners,
                                                               spawners_se,
                                                               lci, uci,
                                                               cv),
                                               names_glue = "{origin}_{.value}") |>
                                   rlang::set_names(function(x) str_remove(x, "_spawners")) |>
                                   mutate(across(
                                     location,
                                     ~ factor(.,
                                              levels = c('Below_TUM',
                                                         'TUM_bb',
                                                         'Icicle',
                                                         'Peshastin',
                                                         'Mission',
                                                         'Chumstick',
                                                         'Chiwaukum',
                                                         'Chiwawa',
                                                         'Nason',
                                                         'Little Wenatchee',
                                                         'White River',
                                                         "Wenatchee (mainstem)",
                                                         "Lower Methow",
                                                         "Gold",
                                                         "Libby",
                                                         "Methow Fish Hatchery",
                                                         "Upper Methow",
                                                         "Twisp",
                                                         "Chewuch",
                                                         "Spring Creek",
                                                         "Beaver")))) |>
                                   arrange(location, type) |>
                                   mutate(total_spawners = nos + hos) |>
                                   rename(nos_est = nos,
                                          hos_est = hos) |>
                                   select(river,
                                          # reach,
                                          # reach_type = type,
                                          total_spawners,
                                          starts_with("nos"),
                                          starts_with("hos"))

                                 spwn_yr <-
                                   spwn_yr |>
                                   left_join(trib_NAs,
                                             by = join_by(river)) |>
                                   mutate(across(c(na_hos, na_nos),
                                                 ~ replace_na(., FALSE))) |>
                                   rowwise() |>
                                   mutate(across(starts_with("nos"),
                                                 ~ if_else(na_nos & nos_est == 0, NA_real_, .)),
                                          across(starts_with("hos"),
                                                 ~ if_else(na_hos & hos_est == 0, NA_real_, .))) |>
                                   ungroup() |>
                                   select(-starts_with("na_"))

                                 spwn_yr <-
                                   spwn_yr |>
                                   bind_rows(spwn_yr %>%
                                               summarize(river = "Total",
                                                         across(c(total_spawners,
                                                                  nos_est,
                                                                  hos_est),
                                                                ~ sum(., na.rm = T)),
                                                         across(ends_with("_se"),
                                                                ~ sqrt(sum(.^2, na.rm = T))),
                                                         .groups = "drop") %>%
                                               mutate(nos_lci = qnorm(0.025, nos_est, nos_se),
                                                      nos_uci = qnorm(0.975, nos_est, nos_se),
                                                      hos_lci = qnorm(0.025, hos_est, hos_se),
                                                      hos_uci = qnorm(0.975, hos_est, hos_se),
                                                      nos_cv = nos_se / nos_est,
                                                      hos_cv = hos_se / hos_est) %>%
                                               mutate(across(ends_with("lci"),
                                                             ~ if_else(. < 0, 0, .)))) %>%
                                   rowwise() %>%
                                   mutate(phos = hos_est / (hos_est + nos_est),
                                          phos_se = deltamethod(~ x1 / (x1 + x2),
                                                                mean = c(hos_est,
                                                                         nos_est),
                                                                cov = diag(c(hos_se,
                                                                             nos_se)^2))) %>%
                                   ungroup() %>%
                                   mutate(across(river,
                                                 as.factor),
                                          across(river,
                                                 ~ fct_relevel(.,
                                                               "Wenatchee (mainstem)",
                                                               "Methow (mainstem)",
                                                               "Total",
                                                               after = Inf))) %>%
                                   arrange(river)


                                 fem_spwn <-
                                   # female estimates in tributaries based on PIT tags
                                   trib_spawners |>
                                   mutate(across(location,
                                                 ~ case_when(. %in% unique(basin_tags$location) ~ .,
                                                             .default = "Other Tributaries"))) |>
                                   group_by(origin,
                                            location) |>
                                   summarise(across(spawners,
                                                    sum),
                                             across(spawners_se,
                                                    ~ sqrt(sum(.^2))),
                                             .groups = "drop") |>
                                   left_join(basin_tags |>
                                               count(location, origin, sex) |>
                                               right_join(expand(basin_tags,
                                                                 location,
                                                                 origin,
                                                                 sex),
                                                          by = join_by(location, origin, sex)) |>
                                               mutate(across(n,
                                                             ~ replace_na(., 0))) |>
                                               group_by(location) |>
                                               mutate(prop = n / sum(n),
                                                      prop_se = sqrt((prop * (1 - prop)) / sum(n))) |>
                                               ungroup() |>
                                               mutate(across(origin,
                                                             ~ case_match(.,
                                                                          "H" ~ "Hatchery",
                                                                          "W" ~ "Natural",
                                                                          .default = .))) |>
                                               filter(str_detect(location, "Tumwater", negate = T)) |>
                                               filter(sex == "Female"),
                                             by = join_by(origin, location)) |>
                                   rowwise() |>
                                   mutate(female_spawners = spawners * prop,
                                          female_spawners_se = msm::deltamethod(~ x1 * x2,
                                                                                c(spawners,
                                                                                  prop),
                                                                                diag(c(spawners_se,
                                                                                       prop_se)^2))) |>
                                   ungroup() |>
                                   mutate(source = "DABOM") |>
                                   arrange(origin,
                                           location) |>
                                   # female estimates in mainstem areas
                                   bind_rows(
                                     spwn_yr |>
                                       filter(str_detect(river, "mainstem")) |>
                                       select(location = river,
                                              nos_est,
                                              nos_se,
                                              hos_est,
                                              hos_se) |>
                                       pivot_longer(-location) |>
                                       mutate(est_type = case_when(str_detect(name, "_est") ~ "spawners",
                                                                   str_detect(name, "_se") ~ "spawners_se",
                                                                   .default = NA_character_),
                                              origin = case_when(str_detect(name, "nos") ~ "Natural",
                                                                 str_detect(name, "hos") ~ "Hatchery",
                                                                 .default = NA_character_)) |>
                                       select(-name) |>
                                       pivot_wider(names_from = est_type,
                                                   values_from = value) |>
                                       left_join(basin_tags |>
                                                   filter(str_detect(location, "Tumwater")) |>
                                                   mutate(location = "Wenatchee (mainstem)") |>
                                                   count(location, origin, sex) |>
                                                   group_by(location) |>
                                                   mutate(prop = n / sum(n),
                                                          prop_se = sqrt((prop * (1 - prop)) / sum(n))) |>
                                                   ungroup() |>
                                                   mutate(across(origin,
                                                                 ~ case_match(.,
                                                                              "H" ~ "Hatchery",
                                                                              "W" ~ "Natural",
                                                                              .default = .))) |>
                                                   filter(sex == "Female"),
                                                 by = join_by(origin, location)) |>
                                       rowwise() |>
                                       mutate(female_spawners = spawners * prop,
                                              female_spawners_se = msm::deltamethod(~ x1 * x2,
                                                                                    c(spawners,
                                                                                      prop),
                                                                                    diag(c(spawners_se,
                                                                                           prop_se)^2))) |>
                                       ungroup() |>
                                       mutate(source = "DABOM and RT")
                                   )

                                 list(results_lst = NA,
                                      fpr = fpr_df,
                                      redd_results = NA,
                                      spwn_rch = NA,
                                      redd_spwn_yr = NA,
                                      spwn_yr = spwn_yr,
                                      fem_spwn = fem_spwn,
                                      basin_tags = basin_tags) %>%
                                   return()
                               }
                             },
                             quiet = F,
                             otherwise = NULL)))

redd_spwn <-
  spwn_est %>%
  mutate(redd_spwn_yr = map(results_list,
                            function(x) x[["redd_spwn_yr"]])) %>%
  select(-results_list) %>%
  unnest(redd_spwn_yr) %>%
  filter(!is.na(observed_redds)) %>%
  makeTableNms()

spwn_df <-
  spwn_est %>%
  mutate(spwn_yr = map(results_list,
                       function(x) x[["spwn_yr"]])) %>%
  select(-results_list) %>%
  unnest(spwn_yr) %>%
  mutate(across(river,
                ~ fct_relevel(.,
                              "Total",
                              after = Inf))) |>
  arrange(spawn_year,
          population,
          river) |>
  makeTableNms()

basin_tag_df <-
  spwn_est %>%
  mutate(basin_tags = map(results_list,
                          function(x) {
                            x[["basin_tags"]] |>
                              select(-spawn_year)
                          })) |>
  select(-results_list) |>
  unnest(basin_tags)

fpr_df <-
  spwn_est %>%
  mutate(fpr = map(results_list,
                   function(x) {
                     x[["fpr"]] |>
                       select(-spawn_year)
                   })) %>%
  select(-results_list) %>%
  unnest(fpr)

# mark_grp_prop <-
#   fpr_df %>%
#   select(run_year:population,
#          location,
#          # contains("phos"),
#          n_wild,
#          n_hor_sn,
#          n_hor_c) %>%
#   mutate(n_origin = n_wild + n_hor_sn + n_hor_c,
#          n_hatch = n_hor_sn + n_hor_c) %>%
#   pivot_longer(cols = n_wild:n_hor_c,
#                names_to = "group",
#                values_to = "n_fish") |>
#   mutate(prop_all = n_fish / n_origin,
#          prop_all_se = sqrt((prop_all * (1 - prop_all)) / n_origin),
#          prop_hatch = case_when(group == "n_wild" ~ 1,
#                                 .default = n_fish / n_hatch),
#          prop_hatch_se = sqrt((prop_hatch * (1 - prop_hatch)) / n_hatch)) |>
#   select(-starts_with("n_")) |>
#   add_column(origin = NA_character_,
#              .before = "group") |>
#   mutate(across(origin,
#                 ~ case_when(str_detect(group, "wild") ~ "Natural",
#                             str_detect(group, "hor") ~ "Hatchery",
#                             .default = .)),
#          across(group,
#                 ~ case_match(.,
#                              "n_wild" ~ "Natural",
#                              "n_hor_sn" ~ "HOR-SN",
#                              "n_hor_c" ~ "HOR-C",
#                              .default = .)))

# for female spawners in the Wenatchee, by origin
wen_fem <-
  spwn_est |>
  filter(population == "Wenatchee") |>
  mutate(fem_spwn = map(results_list,
                       function(x) {
                         x |>
                           extract2("fem_spwn")
                       })) %>%
  select(-results_list) %>%
  unnest(fem_spwn) |>
  group_by(run_year,
           spawn_year,
           population,
           origin) |>
  summarize(across(female_spawners,
                   ~ sum(., na.rm = T)),
            across(female_spawners_se,
                   ~ sqrt(sum(.^2, na.rm = T))),
            .groups = "drop") |>
  arrange(spawn_year,
          desc(origin)) |>
  makeTableNms()


#-----------------------------------------------------------------
# generate tables based on DABOM output
dabom_df <-
  crossing(spawn_year = c(2011:max_yr)) %>%
  mutate(run_year = spawn_year - 1) %>%
  select(run_year, spawn_year) %>%
  mutate(dam_cnt_name = if_else(spawn_year %in% c(2011:2015, 2018),
                                "PriestRapids",
                                "RockIsland"))
# # focus only on max year
# dabom_df <-
#   dabom_df |>
#   filter(spawn_year == max_yr)

# get all the tag summaries
all_tags <-
  dabom_df |>
  mutate(tag_summ = map2(spawn_year,
                         dam_cnt_name,
                         .f = function(yr, dam_nm) {
                           sroem::query_dabom_results(dabom_dam_nm = dam_nm,
                                                      query_year = yr,
                                                      result_type = "tag_summ") |>
                             select(-any_of("spawn_year"))
                         },
                         .progress = T))

# age proportions
age_prop <-
  all_tags |>
  mutate(res = map(tag_summ,
                   .f = function(tag_summ) {
                     tag_summ |>
                       filter(!group %in% c("Start", "Other")) |>
                       mutate(across(group,
                                     recode,
                                     "BelowPriest" = "Below Priest",
                                     "WellsPool" = "Wells Pool"),
                              across(origin,
                                     recode,
                                     "H" = "Hatchery",
                                     "W" = "Natural")) |>
                       select(tag_code,
                              population = group,
                              origin,
                              age) |>
                       left_join(age_table |>
                                   select(age,
                                          fresh_water,
                                          salt_water,
                                          total_age),
                                 by = "age") |>
                       pivot_longer(cols = fresh_water:total_age,
                                    names_to = "age_type",
                                    values_to = "age_label") |>
                       group_by(population,
                                origin,
                                age_type) |>
                       count(age_label) |>
                       filter(!is.na(age_label)) |>
                       mutate(sample_size = sum(n),
                              prop = n / sample_size) |>
                       ungroup() |>
                       # add data for all Priest tags
                       bind_rows(tag_summ |>
                                   mutate(across(origin,
                                                 recode,
                                                 "H" = "Hatchery",
                                                 "W" = "Natural")) |>
                                   select(tag_code,
                                          population = group,
                                          origin,
                                          age) |>
                                   left_join(age_table |>
                                               select(age,
                                                      fresh_water,
                                                      salt_water,
                                                      total_age),
                                             by = "age") |>
                                   pivot_longer(cols = fresh_water:total_age,
                                                names_to = "age_type",
                                                values_to = "age_label") |>
                                   group_by(origin,
                                            age_type) |>
                                   count(age_label) |>
                                   filter(!is.na(age_label)) |>
                                   mutate(sample_size = sum(n),
                                          prop = n / sample_size) |>
                                   ungroup() |>
                                   add_column(population = "Priest Rapids Tags",
                                              .before = 0))
                   })) |>
  select(-dam_cnt_name,
         -tag_summ) %>%
  unnest(res) |>
  mutate(across(
    population,
    ~ factor(.,
             levels = c('Wenatchee',
                        'Entiat',
                        'Methow',
                        'Okanogan',
                        "Below Priest",
                        "Wells Pool",
                        "Priest Rapids Tags"))
  ))

tot_age_prop <- age_prop |>
  filter(age_type == "total_age") |>
  mutate(across(age_label,
                factor,
                levels = 2:11)) |>
  select(-c(age_type, n)) |>
  pivot_wider(names_from = age_label,
              names_expand = T,
              values_from = prop,
              values_fill = 0) |>
  makeTableNms()

sw_age_prop <- age_prop |>
  filter(age_type == "salt_water") |>
  mutate(across(age_label,
                factor,
                levels = 0:4)) |>
  select(-c(age_type, n)) |>
  pivot_wider(names_from = age_label,
              names_expand = T,
              values_from = prop,
              values_fill = 0) |>
  makeTableNms()

fw_age_prop <- age_prop |>
  filter(age_type == "fresh_water") |>
  mutate(across(age_label,
                ~ paste0(., ".X")),
         across(age_label,
                factor,
                levels = paste0(1:5, ".X"))) |>
  select(-c(age_type, n)) |>
  pivot_wider(names_from = age_label,
              names_expand = T,
              values_from = prop,
              values_fill = 0) |>
  makeTableNms()

# population escapement
escape_post <-
  dabom_df |>
  mutate(post = map2(spawn_year,
                     dam_cnt_name,
                     .f = function(yr, dam_nm) {
                       sroem::query_dabom_results(dabom_dam_nm = dam_nm,
                                                  query_year = yr,
                                                  result_type = "escape_post")
                     },
                     .progress = T))
pop_escp <-
  escape_post |>
  mutate(pop_est = map(post,
                       .f = function(x) {
                         x |>
                           filter(param %in% c('LWE',
                                               'ENL',
                                               'LMR',
                                               'OKL', 'FST',
                                               'ICH', 'JD1', 'JDA', 'PRH', 'PRO', 'PRV', 'RSH', 'TMF')) %>%
                           mutate(across(param,
                                         ~ case_match(.,
                                                      'LWE' ~ 'Wenatchee',
                                                      'ENL' ~ 'Entiat',
                                                      'LMR' ~ 'Methow',
                                                      'OKL' ~ 'Okanogan',
                                                      'FST' ~ 'Okanogan',
                                                      'ICH' ~ 'Below Priest',
                                                      'JD1' ~ 'Below Priest',
                                                      'JDA' ~ 'Below Priest',
                                                      'PRH' ~ 'Below Priest',
                                                      'PRO' ~ 'Below Priest',
                                                      'PRV' ~ 'Below Priest',
                                                      'RSH' ~ 'Below Priest',
                                                      'TMF' ~ 'Below Priest')),
                                  across(param,
                                         ~ factor(.,
                                                  levels = c("Wenatchee",
                                                             "Entiat",
                                                             "Methow",
                                                             "Okanogan",
                                                             "Below Priest")))) |>
                           group_by(chain, iter, origin, param) %>%
                           summarize(across(abund,
                                            sum),
                                     .groups = "drop") %>%
                           group_by(origin,
                                    population = param) %>%
                           summarise(est = median(abund),
                                     se = sd(abund),
                                     lci = coda::HPDinterval(coda::as.mcmc(abund))[,1],
                                     uci = coda::HPDinterval(coda::as.mcmc(abund))[,2],
                                     .groups = 'drop') %>%
                           mutate(across(c(est, se, lci),
                                         ~ if_else(. < 0, 0, .))) %>%
                           mutate(across(origin,
                                         recode,
                                         "H" = "hos",
                                         "W" = "nos"),
                                  cv = se / est) %>%
                           pivot_wider(names_from = origin,
                                       values_from = c(est, se, lci, uci, cv),
                                       names_glue = "{origin}_{.value}") %>%
                           rowwise() |>
                           mutate(total_escapement = nos_est + hos_est,
                                  phos = hos_est / (hos_est + nos_est),
                                  phos_se = deltamethod(~ x1 / (x1 + x2),
                                                        mean = c(hos_est,
                                                                 nos_est),
                                                        cov = diag(c(hos_se,
                                                                     nos_se)^2))) %>%
                           ungroup() |>
                           select(population,
                                  total_escapement,
                                  starts_with("nos"),
                                  starts_with("hos"),
                                  starts_with("phos"))
                       },
                       .progress = T)) |>
  select(-dam_cnt_name,
         -post) |>
  unnest(pop_est) |>
  makeTableNms()


# all escapement locations
all_escp <-
  dabom_df |>
  mutate(esp_summ = map2(spawn_year,
                         dam_cnt_name,
                         .f = function(yr, dam_nm) {
                           sroem::query_dabom_results(dabom_dam_nm = dam_nm,
                                                      query_year = yr,
                                                      result_type = "escape_summ") |>
                             select(-spawn_year)
                         },
                         .progress = T)) |>
  select(-dam_cnt_name) |>
  unnest(esp_summ) |>
  mutate(across(origin,
                recode,
                "H" = "Hatchery",
                "W" = "Natural")) %>%
  select(-c(species,
            skew,
            kurtosis)) %>%
  mutate(across(c(mean:mode,
                  ends_with("ci")),
                janitor::round_half_up),
         across(c(mean:mode,
                  ends_with("ci")),
                as.integer)) %>%
  rename(estimate = median,
         se = sd) %>%
  select(-mean, -mode) |>
  # how many tags went past each location?
  left_join(all_tags |>
              select(-dam_cnt_name),
            by = join_by(run_year,
                         spawn_year)) |>
  mutate(n_tags = map2(location,
                       tag_summ,
                       .f = function(x, y) {
                         if(x == "PRA_bb") {
                           n_tags <-
                             y |>
                             group_by(tag_origin = origin) |>
                             summarize(n_tags = n_distinct(tag_code))
                         } else {
                           n_tags <-
                             y |>
                             group_by(tag_origin = origin) |>
                             summarize(n_tags = n_distinct(tag_code[str_detect(path, paste0(" ", str_remove(x, "_bb$")))]))
                         }
                         return(n_tags)
                       })) |>
  unnest(n_tags) |>
  mutate(across(tag_origin,
                recode,
                "H" = "Hatchery",
                "W" = "Natural")) %>%
  filter(origin == tag_origin) |>
  select(-tag_summ,
         -tag_origin) |>
  relocate(n_tags,
           .before = estimate) |>
  # make some 0s into NAs
  group_by(run_year,
           spawn_year,
           location) |>
  mutate(n_tot_tags = sum(n_tags)) |>
  ungroup() |>
  mutate(
    across(
      c(estimate:upper_ci),
      ~ if_else(n_tot_tags == 0 & estimate == 0, NA_real_, .)
    )
  ) |>
  select(-n_tot_tags) |>
  makeTableNms()

# tag summaries
# using bio data from PTAGIS
tag_df <-
  all_tags |>
  select(-dam_cnt_name) |>
  unnest(tag_summ) |>
  select(run_year:tag_code,
         # species,
         species_run_rear_type,
         # record_id,
         second_pit_tag,
         sex,
         age,
         ad_clip,
         cwt,
         trap_date = start_date,
         final_node:tag_detects,
         path) |>
  mutate(origin = str_extract(species_run_rear_type, "[:alpha:]$")) |>
  relocate(origin,
           .after = species_run_rear_type) |>
  mutate(population = case_when(str_detect(path, "LWE") ~ "Wenatchee",
                                str_detect(path, "ENL") ~ "Entiat",
                                str_detect(path, "LMR") ~ "Methow",
                                str_detect(path, "OKL") ~ "Okanogan",
                                .default = NA_character_)) |>
  makeTableNms()

# detection probabilities
detect_df <-
  dabom_df |>
  mutate(res = map2(spawn_year,
                    dam_cnt_name,
                    .f = function(yr, dam_nm) {
                      query_dabom_results(dabom_dam_nm = dam_nm,
                                          query_year = yr,
                                          result_type = "detect_summ")
                    })) |>
  select(-dam_cnt_name) |>
  unnest(res) |>
  select(-c(mean,
            mode)) |>
  rename(estimate = median,
         se = sd) |>
  makeTableNms()

sexed_tags <-
  all_tags |>
  select(-dam_cnt_name) |>
  unnest(tag_summ) |>
  filter(group %in% c("Wenatchee",
                      "Entiat",
                      "Methow",
                      "Okanogan",
                      "BelowPriest")) %>%
  mutate(across(group,
                fct_recode,
                "Below Priest" = "BelowPriest"),
         across(group,
                fct_drop)) %>%
  bind_rows(all_tags |>
              select(-dam_cnt_name) |>
              unnest(tag_summ) |>
              mutate(group = factor("Priest Rapids Tags"))) %>%
  group_by(run_year,
           spawn_year,
           population = group,
           origin,
           sex) %>%
  summarise(n_tags = n_distinct(tag_code[!is.na(sex)]),
            .groups = "drop") %>%
  filter(!is.na(sex)) %>%
  full_join(expand(.,
                   nesting(run_year,
                           spawn_year),
                   population,
                   origin,
                   sex),
            by = join_by(run_year,
                         spawn_year,
                         population,
                         origin,
                         sex)) %>%
  mutate(across(n_tags,
                ~ replace_na(.,
                             0)))

# sex proportions and escapement
sex_prop <- sexed_tags %>%
  bind_rows(sexed_tags %>%
              group_by(run_year,
                       spawn_year,
                       population,
                       sex) %>%
              summarize(across(n_tags,
                               sum),
                        .groups = "drop") %>%
              mutate(origin = "All")) %>%
  mutate(origin = factor(origin,
                         levels = c("W", "H", "All")),
         across(origin,
                fct_recode,
                "Natural" = "W",
                "Hatchery" = "H")) %>%
  mutate(group = paste(spawn_year, population, origin, sep = "_")) %>%
  arrange(spawn_year, population, origin, sex) %>%
  group_by(group) %>%
  mutate(total_sexed = sum(n_tags)) %>%
  mutate(prop = n_tags / total_sexed,
         prop_se = sqrt((prop * (1 - prop)) / total_sexed)) %>%
  ungroup() %>%
  left_join(pop_escp %>%
              clean_names() |>
              select(-starts_with("p_hos")) |>
              rename(total_est = total_escapement) %>%
              rowwise() %>%
              mutate(total_se = sqrt(sum(c(nos_se,
                                           hos_se)^2))) %>%
              ungroup() %>%
              select(spawn_year,
                     population,
                     ends_with("est"),
                     ends_with("se")) %>%
              pivot_longer(c(ends_with("est"),
                             ends_with("se"))) %>%
              mutate(type = if_else(str_detect(name, "_se$"),
                                    "se",
                                    "est"),
                     origin = str_split(name, "_", simplify = T)[,1],
                     across(origin,
                            recode,
                            "total" = "All",
                            "nos" = "Natural",
                            "hos" = "Hatchery")) %>%
              select(-name) %>%
              pivot_wider(names_from = type,
                          values_from = value),
            by = join_by(spawn_year,
                         population,
                         origin)) %>%
  rowwise() %>%
  mutate(across(est,
                ~ . * prop),
         se = deltamethod(~ x1 * x2,
                          mean = c(est, prop),
                          cov = diag(c(se, prop_se)^2))) %>%
  ungroup() %>%
  select(run_year,
         spawn_year,
         population,
         origin,
         sex,
         n_tags,
         total_sexed,
         starts_with("prop"),
         escp = est,
         escp_se = se) |>
  makeTableNms()

# size table
size_df <-
  all_tags |>
  select(-dam_cnt_name) |>
  unnest(tag_summ) |>
  filter(!group %in% c("Start", "Other")) |>
  mutate(across(group,
                fct_drop)) |>
  mutate(across(group,
                recode,
                "BelowPriest" = "Below Priest",
                "WellsPool" = "Wells Pool"),
         across(origin,
                recode,
                "H" = "Hatchery",
                "W" = "Natural")) |>
  select(run_year,
         spawn_year,
         tag_code,
         population = group,
         origin,
         age,
         fork_length) |>
  left_join(age_table |>
              select(age,
                     salt_water),
            by = "age") |>
  filter(!is.na(fork_length),
         !is.na(salt_water)) |>
  group_by(run_year,
           spawn_year,
           population,
           origin,
           salt_water) |>
  summarize(across(fork_length,
                   list(mean = mean,
                        sd = sd,
                        n = length),
                   .names = "{.fn}"),
            .groups = "drop") %>%
  rename(salt_water_age = salt_water) |>
  # add data for all Priest Rapids tags
  bind_rows(all_tags |>
              select(-dam_cnt_name) |>
              unnest(tag_summ) |>
              mutate(across(origin,
                            recode,
                            "H" = "Hatchery",
                            "W" = "Natural")) |>
              select(run_year,
                     spawn_year,
                     tag_code,
                     origin,
                     age,
                     fork_length) |>
              left_join(age_table |>
                          select(age,
                                 salt_water),
                        by = "age") |>
              filter(!is.na(fork_length),
                     !is.na(salt_water)) |>
              group_by(origin,
                       salt_water) |>
              summarize(across(fork_length,
                               list(mean = mean,
                                    sd = sd,
                                    n = length),
                               .names = "{.fn}"),
                        .groups = "drop") %>%
              rename(salt_water_age = salt_water) |>
              add_column(population = "Priest Rapids Tags",
                         .before = 0))

size_tab <- expand(size_df,
                   nesting(run_year, spawn_year),
                   population, origin, salt_water_age) |>
  left_join(size_df,
            by = c("run_year",
                   "spawn_year",
                   "population",
                   "origin",
                   "salt_water_age")) |>
  mutate(across(salt_water_age,
                ~ paste0("Salt-", .))) |>
  rename(Mean = mean,
         SD = sd,
         N = n) |>
  group_by(spawn_year,
           population,
           origin) |>
  mutate(sample_size = sum(N, na.rm = T)) |>
  ungroup() |>
  pivot_wider(names_from = salt_water_age,
              values_from = c(Mean, SD, N),
              names_glue = "{salt_water_age}_{.value}",
              names_vary = "slowest") |>
  makeTableNms()

# proportions / escapement of mark groups, by population
mark_tag_summ <-
  all_tags |>
  select(-dam_cnt_name) |>
  unnest(tag_summ) |>
  filter(!is.na(ad_clip)) |>
  mutate(across(cwt,
                ~ if_else(origin == "W",
                          F,
                          .)),
         ad_clip_chr = case_when(ad_clip ~ "AD",
                                 !ad_clip ~ "AI",
                                 .default = NA_character_),
         cwt_chr = case_when(origin == "W" ~ "noCWT",
                             origin == "H" & cwt ~ "CWT",
                             origin == "H" & !cwt ~ "noCWT")) |>
  tidyr::unite("mark_grp", ad_clip_chr, cwt_chr, remove = T) |>
  mutate(mark_grp = if_else(origin == "W",
                            "Wild",
                            mark_grp))


mark_tag_summ |>
  select(run_year:final_node,
         ad_clip,
         cwt,
         group,
         mark_grp)

# proportions of each type of fish (H/W, Ad-clip, CWT combinations) past each site

# get the latest parent child table
parent_child <-
  dabom_df |>
  filter(spawn_year == max(spawn_year)) |>
  mutate(pc = map2(spawn_year,
                   dam_cnt_name,
                   .f = function(yr, dam) {
                     query_dabom_results(dabom_dam_nm = dam,
                                         query_year = yr,
                                         result_type = "parent_child")
                   })) |>
  select(pc) |>
  unnest(pc)

# determine which group/population each site is in
site_pop_df = buildNodeOrder(parent_child) |>
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
                           .default = "Other")) |>
  mutate(group = factor(group,
                        levels = c("Wenatchee",
                                   "Entiat",
                                   "Methow",
                                   "Okanogan",
                                   "BelowPriest",
                                   "WellsPool",
                                   "Start",
                                   "Other"))) |>
  rename(site_code = node,
         site_order = node_order,
         population = group)

# max number of sites
max_sites <-
  site_pop_df |>
  mutate(n_sites = str_count(path, " ") + 1) |>
  pull(n_sites) |>
  max()

mark_grp_prop = mark_tag_summ |>
  # mutate(spawn_site = str_remove(spawn_node, "B0$"),
  #        spawn_site = str_remove(spawn_site, "A0$"),
  #        spawn_site = recode(spawn_site,
  #                            "S" = "SA0")) |>
  mutate(spawn_site = str_remove(final_node, "_U$"),
         spawn_site = str_remove(spawn_site, "_D$")) |>
  left_join(site_pop_df |>
              select(end_loc = site_code,
                     population,
                     path) |>
              separate(path,
                       into = paste("site", 1:max_sites, sep = "_")) |>
              pivot_longer(starts_with('site_'),
                           names_to = "node_order",
                           values_to = "site_code") |>
              filter(!is.na(site_code)) |>
              select(-node_order),
            by = c("spawn_site" = "end_loc"),
            relationship = "many-to-many") |>
  group_by(run_year,
           spawn_year,
           origin,
           population = group,
           site_code, ad_clip, cwt, mark_grp) |>
  summarise(n_tags = n_distinct(tag_code),
            .groups = "drop") |>
  right_join(crossing(site_code = union(parent_child$parent, parent_child$child),
                      expand(mark_tag_summ,
                             nesting(run_year, spawn_year),
                             nesting(origin, ad_clip, cwt, mark_grp))) |>
               left_join(site_pop_df |>
                           select(site_code,
                                  population),
                         by = "site_code") |>
               relocate(population, .after = 1),
             by = c("run_year", "spawn_year",
                    "origin", "population", "site_code", "ad_clip", "cwt", "mark_grp")) |>
  arrange(spawn_year, site_code, mark_grp) |>
  mutate(across(n_tags,
                ~ replace_na(.,
                             0))) |>
  group_by(run_year,
           spawn_year,
           origin,
           site_code) |>
  mutate(tot_tags = sum(n_tags),
         prop = n_tags / tot_tags,
         # using normal approximation
         prop_se = sqrt((prop * (1 - prop))/tot_tags)) |>
  ungroup() |>
  arrange(spawn_year,
          population,
          site_code,
          origin,
          ad_clip,
          cwt)

# generate posterior samples of mark proportions
post_samp <-
  escape_post |>
  select(spawn_year,
         post) |>
  unnest(post)
n_chains = max(post_samp$chain)
n_iter = max(post_samp$iter)

set.seed(6)
prop_samps = mark_grp_prop |>
  filter(tot_tags > 0,
         prop > 0,
         prop < 1) |>
  nest(data = -c(run_year,
                 spawn_year,
                 origin,
                 population,
                 site_code,
                 tot_tags)) |>
  crossing(chain = 1:n_chains) |>
  mutate(samp = map(data,
                    .f = function(x) {
                      rmultinom(n_iter, sum(x$n_tags), x$prop) |>
                        set_colnames(1:n_iter) |>
                        set_rownames(x$mark_grp) |>
                        as_tibble(rownames = 'mark_grp') |>
                        pivot_longer(-1,
                                     names_to = "iter",
                                     values_to = "n_tags") |>
                        mutate(across(iter,
                                      as.integer)) |>
                        group_by(iter) |>
                        mutate(prop = n_tags / sum(n_tags)) |>
                        arrange(iter, mark_grp) |>
                        ungroup()
                    },
                    .progress = T)) |>
  select(-data) |>
  unnest(samp) |>
  mutate(ad_clip = if_else(str_detect(mark_grp, "^AD"), T, F),
         cwt = if_else(str_detect(mark_grp, "_CWT"), T, F)) |>
  relocate(ad_clip,
           cwt,
           .before = mark_grp) |>
  bind_rows(mark_grp_prop |>
              filter(tot_tags == 0 |
                       prop == 0 |
                       prop == 1) |>
              crossing(chain = 1:n_chains,
                       iter = 1:n_iter) |>
              select(-prop_se)) |>
  arrange(spawn_year,
          population,
          site_code,
          iter,
          origin, mark_grp) |>
  select(chain, iter, any_of(names(mark_grp_prop)))

# posterior samples
mark_post = post_samp |>
  inner_join(prop_samps,
             by = c("spawn_year",
                    "chain",
                    "iter", "origin",
                    "param" = "site_code"),
             multiple = "all") %>%
  mutate(across(c(prop,
                  n_tags,
                  tot_tags),
                replace_na,
                0)) |>
  mutate(prop = if_else(value == 0,
                        0,
                        prop)) |>
  mutate(n_fish = abund * prop)

mark_grp_df <-
  mark_post |>
  DABOM::summarisePost(value = n_fish,
                       run_year,
                       spawn_year,
                       population,
                       origin,
                       param,
                       ad_clip,
                       cwt,
                       mark_grp) |>
  rename(site_code = param,
         se = sd) |>
  left_join(mark_grp_prop |>
              rename(proportion = prop)) |>
  select(population,
         site_code,
         origin:mark_grp,
         n_tags:proportion,
         prop_se,
         everything()) |>
  arrange(population,
          site_code,
          mark_grp) |>
  mutate(across(origin,
                ~ fct_recode(.,
                             "Natural" = "W",
                             "Hatchery" = "H")),
         across(population,
                ~ factor(.,
                         levels = c('Wenatchee',
                                    'Entiat',
                                    'Methow',
                                    'Okanogan')))) |>
  arrange(spawn_year,
          population,
          site_code,
          mark_grp) |>
  select(run_year,
         spawn_year,
         population:prop_se,
         estimate = median,
         se,
         lowerCI = lower_ci,
         upperCI = upper_ci)


mark_grp_pop_df <-
  mark_grp_df |>
  filter(site_code %in% c("LWE",
                          "ENL",
                          "LMR",
                          "OKL")) |>
  select(-site_code) |>
  makeTableNms()

mark_grp_df <-
  mark_grp_df |>
  makeTableNms()

#-----------------------------------------------------------------
# pull together estimates of sex call error rates at Priest
# check for duplicate tags
read_excel(paste0("T:/DFW-Team FP Upper Columbia Escapement - General/",
                  "UC_Sthd/inputs/Bio Data/",
                  "Sex and Origin PRD-Brood Comparison Data/",
                  "STHD UC Brood Collections_2011 to current.xlsx"),
           sheet = "Brood Collected_PIT Tagged Only") |>
  clean_names() |>
  distinct() |>
  # one tag has two records; choose the one that matches PTAGIS recapture details
  filter(!(recaptured_pit == "3DD.0077DA4CC6" & sex_final == "M")) |>
  # filter(recaptured_pit %in% recaptured_pit[duplicated(recaptured_pit)]) |>
  # arrange(recaptured_pit) |>
  # group_by(recaptured_pit) |>
  # mutate(n_final_sex = n_distinct(sex_final)) |>
  # ungroup() |>
  # filter(n_final_sex > 1)
  # as.data.frame()
  select(spawn_year,
         tag_code = recaptured_pit,
         sex_final) |>
  distinct() |>
  filter(tag_code %in% tag_code[duplicated(tag_code)]) |>
  arrange(tag_code) #|>
# inner_join(tag_df |>
#              clean_names() |>
#              select(tag_code))

# estimate error rate for each sex
sex_err_rate <-
  tag_df |>
  clean_names() |>
  select(spawn_year,
         tag_code,
         sex_field = sex) |>
  mutate(across(sex_field,
                ~ recode(.,
                         "Male" = "M",
                         "Female" = "F"))) |>
  inner_join(read_excel(paste0("T:/DFW-Team FP Upper Columbia Escapement - General/",
                               "UC_Sthd/inputs/Bio Data/",
                               "Sex and Origin PRD-Brood Comparison Data/",
                               "STHD UC Brood Collections_2011 to current.xlsx"),
                        sheet = "Brood Collected_PIT Tagged Only") |>
               clean_names() |>
               # one tag has two records; choose the one that matches PTAGIS recapture details
               filter(!(recaptured_pit == "3DD.0077DA4CC6" & sex_final == "M")) |>
               rename(tag_code = recaptured_pit) |>
               select(spawn_year,
                      tag_code,
                      sex_final) |>
               distinct(),
             by = c("spawn_year",
                    "tag_code")) |>
  filter(!is.na(sex_final),
         !is.na(sex_field)) |>
  mutate(agree = if_else(sex_field == sex_final,
                         T, F)) |>
  group_by(spawn_year,
           sex = sex_field) |>
  summarize(n_tags = n_distinct(tag_code),
            n_true = sum(agree),
            n_false = sum(!agree),
            .groups = "drop") |>
  mutate(binom_ci = map2(n_false,
                         n_tags,
                         .f = function(x, y) {
                           DescTools::BinomCI(x, y) |>
                             as_tibble()
                         })) |>
  unnest(binom_ci) |>
  clean_names() |>
  rename(perc_false = est,
         lowerci = lwr_ci,
         upperci = upr_ci) |>
  mutate(perc_se = sqrt((perc_false * (1 - perc_false)) / n_tags)) |>
  relocate(perc_se,
           .after = "perc_false")

# re-calculate proportion of each sex in each population each year
sex_prop <-
  sex_prop |>
  clean_names() |>
  mutate(across(sex,
                ~ recode(.,
                         "Male" = "M",
                         "Female" = "F"))) |>
  # dabom_est |>
  # mutate(res = map(all_results,
  #                  "sex_yr")) |>
  # select(-dam_cnt_name,
  #        -all_results) |>
  # unnest(res) |>
  mutate(across(origin,
                as_factor)) |>
  select(run_year:total_sexed) |>
  rename(priest_tags = n_tags) |>
  left_join(sex_err_rate |>
              select(spawn_year,
                     sex,
                     starts_with("perc")),
            by = c("spawn_year", "sex")) |>
  pivot_wider(names_from = sex,
              values_from = c(priest_tags,
                              perc_false,
                              perc_se)) |>
  mutate(true_male = priest_tags_M - (priest_tags_M * perc_false_M) + (priest_tags_F * perc_false_F),
         true_female = priest_tags_F - (priest_tags_F * perc_false_F) + (priest_tags_M * perc_false_M)) |>
  # mutate(across(starts_with("true"),
  #               janitor::round_half_up)) |>
  rowwise() |>
  mutate(true_m_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
                                      mean = c(priest_tags_M,
                                               perc_false_M,
                                               priest_tags_F,
                                               perc_false_F),
                                      cov = diag(c(0,
                                                   perc_se_M,
                                                   0,
                                                   perc_se_F)^2)),
         true_f_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
                                      mean = c(priest_tags_F,
                                               perc_false_F,
                                               priest_tags_M,
                                               perc_false_M),
                                      cov = diag(c(0,
                                                   perc_se_F,
                                                   0,
                                                   perc_se_M)^2))) |>
  mutate(n_sexed = true_male + true_female,
         across(n_sexed,
                round_half_up),
         prop_m = true_male / (true_male + true_female),
         prop_se = msm::deltamethod(~ x1 / (x1 + x2),
                                    mean = c(true_male,
                                             true_female),
                                    cov = diag(c(true_m_se,
                                                 true_f_se)^2))) |>
  ungroup() |>
  select(run_year:origin,
         total_sexed,
         contains("priest"),
         n_sexed,
         starts_with("true"),
         starts_with("prop")) |>
  mutate(prop_f = 1 - prop_m) |>
  # filter(total_sexed != n_sexed)
  clean_names() |>
  pivot_longer(cols = c(starts_with("priest"),
                        starts_with("true"),
                        starts_with("prop"))) |>
  mutate(sex = if_else(str_detect(name, "_m"),
                       "M", "F"),
         across(name,
                str_remove,
                "_male"),
         across(name,
                str_remove,
                "_female"),
         across(name,
                str_remove,
                "_m"),
         across(name,
                str_remove,
                "_f"),
         across(name,
                str_remove,
                "_M"),
         across(name,
                str_remove,
                "_F"),
         across(name,
                str_replace,
                "true",
                "n_tags")) |>
  pivot_wider(names_from = "name",
              values_from = "value") |>
  arrange(spawn_year,
          population,
          origin,
          sex) |>
  fill(prop_se,
       .direction = "down") |>
  select(run_year:origin,
         sex,
         priest_tags,
         n_tags,
         total_sexed = n_sexed,
         # total_sexed,
         starts_with("prop")) |>
  mutate(across(c(n_tags,
                  total_sexed),
                round_half_up)) |>
  left_join(pop_escp |>
              clean_names() |>
              rename(total_est = total_escapement) %>%
              rowwise() %>%
              mutate(total_se = sqrt(sum(c(nos_se,
                                           hos_se)^2))) %>%
              ungroup() %>%
              select(run_year,
                     spawn_year,
                     population,
                     ends_with("est"),
                     ends_with("se")) %>%
              pivot_longer(c(ends_with("est"),
                             ends_with("se"))) %>%
              mutate(type = if_else(str_detect(name, "_se$"),
                                    "se",
                                    "est"),
                     origin = str_split(name, "_", simplify = T)[,1],
                     across(origin,
                            recode,
                            "total" = "All",
                            "nos" = "Natural",
                            "hos" = "Hatchery")) %>%
              select(-name) %>%
              pivot_wider(names_from = type,
                          values_from = value),
            by = c("run_year", "spawn_year", "population", "origin")) |>
  rowwise() |>
  mutate(escp = est * prop,
         escp_se = msm::deltamethod(~ x1 * x2,
                                    mean = c(est,
                                             prop),
                                    cov = diag(c(se, prop_se)^2))) |>
  ungroup() |>
  select(-est,
         -se) |>
  makeTableNms()


#-----------------------------------------------------------------
# generate table of escapement estimates at Priest
priest_df <-
  dabom_df |>
  select(run_year,
         spawn_year) |>
  mutate(prd_df = map(spawn_year,
                      .f = function(yr) {
                        query_dabom_results(dabom_dam_nm = "PriestRapids",
                                            query_year = yr,
                                            result_type = "dam_summ") |>
                          filter(dam == "PriestRapids") |>
                          select(win_cnt,
                                 origin,
                                 contains("reasc_rate"),
                                 contains("priest_cnt"),
                                 ) |>
                          mutate(total = sum(priest_cnt),
                                 total_se = sqrt(sum(priest_cnt_se^2)))
                      },
                      .progress = T)) |>
  unnest(prd_df) |>
  left_join(tag_df |>
              clean_names() |>
              group_by(run_year,
                       spawn_year,
                       origin) |>
              summarize(n_tags = n_distinct(tag_code),
                        .groups = "drop") |>
              group_by(run_year,
                       spawn_year) |>
              mutate(total_tags = sum(n_tags)) |>
              ungroup()) |>
  mutate(across(origin,
                ~ case_match(.,
                             "H" ~ "Hatchery",
                             "W" ~ "Natural",
                             .default = .))) |>
  makeTableNms() |>
  rename(`PRD Ladder Count` = `Win Cnt`,
         `Adult Reascension Adjustment Rate` = `Reasc Rate`,
         `Reascension Rate SE` = `Reasc Rate SE`,
         `Priest Est` = `Priest Cnt`,
         `Priest SE` = `Priest Cnt SE`)

# now a table for years when Rock Island was used
rock_isl_df <-
  dabom_df |>
  filter(dam_cnt_name == "RockIsland") |>
  select(run_year,
         spawn_year) |>
  mutate(prd_df = map(spawn_year,
                      .f = function(yr) {
                        query_dabom_results(dabom_dam_nm = "RockIsland",
                                            query_year = yr,
                                            result_type = "dam_summ") |>
                          filter(dam == "RockIsland") |>
                          select(origin,
                                 dam,
                                 # contains("reasc_rate"),
                                 contains("priest_cnt"),
                          ) |>
                          mutate(total = sum(priest_cnt),
                                 total_se = sqrt(sum(priest_cnt_se^2)))
                      },
                      .progress = T)) |>
  unnest(prd_df) |>
  # left_join(tag_df |>
  #             clean_names() |>
  #             group_by(run_year,
  #                      spawn_year,
  #                      origin) |>
  #             summarize(n_tags = n_distinct(tag_code),
  #                       .groups = "drop") |>
  #             group_by(run_year,
  #                      spawn_year) |>
  #             mutate(total_tags = sum(n_tags)) |>
  #             ungroup()) |>
mutate(across(origin,
              ~ case_match(.,
                           "H" ~ "Hatchery",
                           "W" ~ "Natural",
                           .default = .)),
       across(dam,
                ~ recode(.,
                         "RockIsland" = "Rock Island"))) |>
  makeTableNms() |>
  rename(
    `Modelled Priest Est` = `Priest Cnt`,
    `Modelled Priest SE` = `Priest Cnt SE`,
    `Modelled Total` = Total,
    `Modelled Total SE` = `Total SE`,
    `Dam Method` = Dam)

dam_cnt_tab <- priest_df |>
  left_join(rock_isl_df,
            by = c("Run Year",
                   "Spawn Year",
                   "Origin")) |>
  relocate(c(`Dam Method`,
             `N Tags`,
             `Total Tags`),
           .after = "Modelled Total SE") |>
  mutate(`Tag Rate` = if_else(is.na(`Dam Method`),
                              `N Tags` / `Priest Est`,
                              `N Tags` / `Modelled Priest Est`),
         `Total Tag Rate` = if_else(is.na(`Dam Method`),
                                    `Total Tags` / Total,
                                    `Total Tags` / `Modelled Total`))

#-----------------------------------------------------------------
# put together in a list to save / write to Excel
# make some decisions about rounding
save_list <- list(
  "Redd Abundance" = redd_spwn |>
    mutate(across(ends_with("Year"),
                  as.integer),
           across(c(`Redd Est`,
                    `NOS Est`,
                    `HOS Est`),
                  round_half_up)) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "Run Escp by Origin" = pop_escp |>
    mutate(across(ends_with("Year"),
                  as.integer),
           across(c(`Total Escapement`,
                    ends_with("Est")),
                  round_half_up)) |>
    mutate(`Total Escapement` = `NOS Est` + `HOS Est`) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "Spawning Escp by Origin" = spwn_df |>
    bind_rows(pop_escp |>
                filter(Population == "Entiat") |>
                rename(`Total Spawners` = `Total Escapement`) |>
                mutate(River = as.factor("Total")) |>
                rowwise() |>
                mutate(`pHOS SE` = deltamethod(~ x1 / (x1 + x2),
                                               mean = c(`HOS Est`,
                                                        `NOS Est`),
                                               cov = diag(c(`HOS SE`,
                                                            `NOS SE`)^2))) |>
                ungroup() |>
                mutate(across(Population,
                              factor,
                              levels = levels(pop_escp$Population)))) |>
    arrange(`Spawn Year`,
            Population,
            River) |>
    mutate(across(ends_with("Year"),
                  as.integer),
           across(c(`Total Spawners`,
                    ends_with("Est"),
                    ends_with("CI")),
                  round_half_up)) |>
    # mutate(`Total Spawners` = `NOS Est` + `HOS Est`) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3),
           across(c(`NOS SE`,
                    `HOS SE`),
                  round,
                  digits = 2)),

  "Run Escp All Locations" = all_escp |>
    mutate(across(ends_with("Year"),
                  as.integer)) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "Detection Probability" = detect_df |>
    mutate(across(c(ends_with("Year"),
                    `N Tags`),
                  as.integer)) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "Tag Summary" = tag_df |>
    mutate(across(where(is.logical),
                  as.character)) |>
    mutate(across(ends_with("Year"),
                  as.integer)),

  "Sex Error Rates" = sex_err_rate |>
    makeTableNms() |>
    mutate(across(ends_with("Year"),
                  as.integer)) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "Run Escp Sex" = sex_prop |>
    mutate(across(ends_with("Year"),
                  as.integer),
           across(Escp,
                  ~ as.integer(round_half_up(.)))) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "Wenatchee Female Spwn" = wen_fem |>
    mutate(across(ends_with("Year"),
                  as.integer),
           across(ends_with("Spawners"),
                  ~ as.integer(round_half_up(.)))) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "SaltAge at Maturity" = sw_age_prop |>
    mutate(across(ends_with("Year"),
                  as.integer)) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "FWAge at Maturity" = fw_age_prop |>
    mutate(across(ends_with("Year"),
                  as.integer)) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "TotalAge at Maturity" = tot_age_prop |>
    mutate(across(ends_with("Year"),
                  as.integer)) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "Size at Maturity" = size_tab |>
    mutate(across(ends_with("Year"),
                  as.integer)) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "Priest Rapids Escapement" = dam_cnt_tab |>
    mutate(across(ends_with("Year"),
                  as.integer)) |>
    mutate(across(c("PRD Ladder Count",
                    ends_with("Priest Est"),
                    ends_with("Total")),
                  ~ as.integer(round_half_up(.)))) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "Mark Group Population" = mark_grp_pop_df |>
    mutate(across(ends_with("Year"),
                  as.integer)) |>
    mutate(across(Estimate,
                  ~ as.integer(round_half_up(.)))) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3)),

  "Mark Group All Locations" = mark_grp_df |>
    mutate(across(ends_with("Year"),
                  as.integer)) |>
    mutate(across(Estimate,
                  ~ as.integer(round_half_up(.)))) |>
    mutate(across(where(is.double),
                  round,
                  digits = 3))

)

#-----------------------------------------------------------------
# actually save entire file
write_xlsx(x = save_list,
           path = paste0("T:/DFW-Team FP Upper Columbia Escapement - General/",
                         "UC_Sthd/Estimates/",
                         "UC_STHD_Model_Output",
                         "_", format(today(), "%Y%m%d"),
                         ".xlsx"))

#--------------------------------------------------------------
# read in previous estimates, add latest year to them

# what year are we overwriting or adding?
yr = 2024

library(readxl)
output_path <- paste0("T:/DFW-Team FP Upper Columbia Escapement - General/",
                      "UC_Sthd/Estimates/",
                      "UC_STHD_Model_Output.xlsx")
                      # "UC_STHD_Model_Output_20240108.xlsx")

tab_nms <- excel_sheets(output_path)

previous_output <- as.list(tab_nms) |>
  rlang::set_names() %>%
  map(.f = function(x) {
    read_excel(output_path,
               sheet = x) %>%
      filter(`Spawn Year` != yr) |>
      mutate(
        across(c(ends_with("Est"),
                 ends_with("SE"),
                 ends_with("CV")),
               as.numeric))
  },
  .progress = T)

lst_one_yr <- save_list |>
  map(.f = function(x) {
    x |>
      filter(`Spawn Year` == yr)
  })



identical(names(previous_output), names(lst_one_yr))

save_list_new = vector("list",
                   length = length(previous_output))
names(save_list_new) = names(previous_output)
for(i in 1:length(previous_output)) {
  save_list_new[[i]] <- previous_output[[i]] |>
    bind_rows(lst_one_yr[[i]]) %>%
    arrange(`Spawn Year`)
}


write_xlsx(x = save_list_new,
           path = paste0("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/estimates/",
                         "UC_STHD_Model_Output",
                         # "_", format(today(), "%Y%m%d"),
                         ".xlsx"))
