# Author: Kevin See
# Purpose: recalculate estimates at Priest based on other dam counts
# Created: 1/25/2022
# Last Modified: 3/8/2022
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(STADEM)
library(here)
library(DABOM)
library(lubridate)
library(janitor)
library(msm)
library(magrittr)

theme_set(theme_bw())

#-----------------------------------------------------------------
# what years to examine?
all_yrs = 2011:2023

# set species
spp = "Steelhead"

#-----------------------------------------------------------------
# get the total dam counts from various dams
dam_cnts = tibble(dam = c("PriestRapids",
                          "RockIsland",
                          "RockyReach",
                          "Wells",
                          "Tumwater"),
                  dam_code = c("PRD",
                               "RIS",
                               "RRH",
                               "WEL",
                               "TUM")) %>%
  crossing(year = c(all_yrs, 2022)) %>%
  rowwise() %>%
  mutate(win_cnt = map2_dbl(dam_code,
                            year,
                            .f = function(x, y) {
                              STADEM::getWindowCounts(dam = x,
                                                      spp = spp,
                                                      start_date = paste0(y-1, '0601'),
                                                      end_date = paste0(y, '0531')) %>%
                                summarise_at(vars(win_cnt),
                                             list(sum),
                                             na.rm = T) %>%
                                pull(win_cnt)
                            })) %>%
  ungroup() %>%
  select(year, everything())

# got these directly from Ben for each spawn year (June 15 - June 14, which is how it's definted at Tumwater)
tum_cnts = tibble(year = 2013:2021,
                  dam = "Tumwater",
                  dam_code = "TUM",
                  win_cnt = c(2446,
                              1186,
                              1751,
                              1405,
                              554,
                              621,
                              390,
                              578,
                              776))

dam_cnts %<>%
  left_join(tum_cnts %>%
              rename(tum_cnt = win_cnt)) %>%
  mutate(win_cnt = if_else(!is.na(tum_cnt),
                           tum_cnt,
                           win_cnt)) %>%
  select(-tum_cnt)


dam_cnts %>%
  filter(dam_code != "TUM") %>%
  group_by(year) %>%
  mutate(perc_prd = win_cnt / win_cnt[dam_code == "PRD"]) %>%
  ungroup() %>%
  ggplot(aes(x = year,
             y = perc_prd,
             color = dam_code)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_brewer(palette = "Set1",
                     name = "Dam") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Year",
       y = "Percent of Priest Rapids Counts")

dam_cnts %>%
  filter(dam_code != "TUM") %>%
  group_by(year) %>%
  mutate(perc_ria = win_cnt / win_cnt[dam_code == "RIS"]) %>%
  ungroup() %>%
  ggplot(aes(x = year,
             y = perc_ria,
             color = dam_code)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_brewer(palette = "Set1",
                     name = "Dam") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Year",
       y = "Percent of Rock Island Counts")

#-----------------------------------------------------------------
# get re-ascension rates for some dams
reasc_rates <- crossing(year = all_yrs,
                        pit_code = c("PRA",
                                     "RIA",
                                     "RRF")) %>%
  mutate(reasc_df = map2(pit_code, year,
                         .f = function(x, y) {

                           res <- try(queryPITtagData(damPIT = x,
                                                      spp = "Steelhead",
                                                      start_date = paste0(y-1, '0601'),
                                                      end_date = paste0(y, '0531')) %>%
                                        filter(!str_detect(TagId, "000.0")) %>%
                                        mutate(across(TagIdAscentCount,
                                                      tidyr::replace_na,
                                                      0)) %>%
                                        mutate(ReAscent = ifelse(TagIdAscentCount > 1, T, F)) %>%
                                        mutate(origin = fct_recode(RearType,
                                                                   "W" = "U")) %>%
                                        group_by(Species, Date, origin) %>%
                                        summarise(tot_tags = n_distinct(TagId),
                                                  reascent_tags = n_distinct(TagId[ReAscent]),
                                                  .groups = "drop") %>%
                                        group_by(Species, origin) %>%
                                        summarise(across(matches('tags'),
                                                         sum,
                                                         na.rm = T),
                                                  .groups = "drop") %>%
                                        mutate(reasc_rate = reascent_tags / tot_tags,
                                               reasc_rate_se = sqrt(reasc_rate * (1 - reasc_rate) / tot_tags)) %>%
                                        select(origin, starts_with("reasc_rate")))
                           return(res)
                         })) %>%
  mutate(class = map_chr(reasc_df,
                         .f = function(x) class(x)[1])) %>%
  filter(class == "tbl_df") %>%
  select(-class) %>%
  unnest(reasc_df)

d_w = 0.5
reasc_rates %>%
  ggplot(aes(x = as.factor(year),
             y = reasc_rate,
             color = pit_code)) +
  geom_errorbar(aes(ymin = qnorm(0.025, reasc_rate, reasc_rate_se),
                    ymax = qnorm(0.975, reasc_rate, reasc_rate_se)),
                width = 0,
                position = position_dodge(d_w)) +
  geom_point(size = 3,
             position = position_dodge(d_w)) +
  facet_wrap(~ origin) +
  scale_color_brewer(palette = "Set1",
                     name = "Dam") +
  labs(x = "Year",
       y = "Reascension Rate")

#-----------------------------------------------------------------
# loop over all years
for(yr in all_yrs) {

  # set up a tibble to capture results
  if(yr == min(all_yrs)) {
    pit_move_tmp = NULL
  }

  # load compressed detections and biological data
  load(here('analysis/data/derived_data/PITcleanr',
            paste0('UC_', spp, '_', yr, '.rda')))


  # load JAGS MCMC results
  load(here("analysis/data/derived_data/model_fits",
            paste0('PRA_DABOM_', spp, '_', yr,'.rda')))

  # summarize detection probabilities
  detect_summ = summariseDetectProbs(dabom_mod = dabom_mod,
                                     filter_ch = filter_obs)
  # add origin to prepped capture histories
  pit_obs = prepped_ch %>%
    left_join(bio_df %>%
                select(tag_code = pit_tag,
                       origin)) %>%
    select(tag_code, origin,
           everything())

  # compile and calculate Priest equivalents
  pit_move_df = tibble(dam = c("PriestRapids",
                               "RockIsland",
                               "RockyReach",
                               "Wells",
                               "Tumwater"),
                       dam_code = c("PRD",
                                    "RIS",
                                    "RRH",
                                    "WEL",
                                    "TUM"),
                       pit_code = c("PRA",
                                    "RIA",
                                    "RRF",
                                    "WEA",
                                    "TUM")) %>%
    add_column(year = yr,
               .before = 0) %>%
    crossing(origin = c("W", "H")) %>%
    left_join(pit_obs %>%
                group_by(origin) %>%
                summarize(n_tags = n_distinct(tag_code))) %>%
    mutate(n_obs = map2_int(pit_code,
                            origin,
                            .f = function(x, y) {
                              pit_obs %>%
                                filter(origin == y) %>%
                                summarize(n_obs = n_distinct(tag_code[node == x])) %>%
                                pull(n_obs)
                            })) %>%
    mutate(n_upstrm = map2_int(pit_code,
                               origin,
                               .f = function(x, y) {
                                 pit_obs %>%
                                   filter(origin == y) %>%
                                   summarize(n_path = n_distinct(tag_code[str_detect(path, paste0(" ", x))])) %>%
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
    # mutate(prop_obs = n_obs / n_tags,
    #        prop_upstrm = n_upstrm / n_tags) %>%
    left_join(detect_summ %>%
                select(node, mean, sd),
              by = c("pit_code" = "node")) %>%
    mutate(est_tags = n_obs / mean,
           est_tags_se = n_obs * sd / mean^2,
           trans_est = est_tags / n_tags,
           trans_se = est_tags_se / n_tags) %>%
    mutate(trans_est = if_else(dam == "PriestRapids",
                               1,
                               trans_est),
           trans_se = if_else(dam == "PriestRapids",
                              0,
                              trans_se))

  if(is.null(pit_move_tmp)) {
    pit_move_tmp = pit_move_df
  } else {
    pit_move_tmp <- pit_move_tmp %>%
      bind_rows(pit_move_df)
  }

  rm(detect_summ,
     pit_obs,
     pit_move_df,
     dabom_mod,
     filter_obs)
}

pit_move_all <- pit_move_tmp %>%
  left_join(dam_cnts) %>%
  left_join(reasc_rates) %>%
  mutate(reasc_avail = if_else(!is.na(reasc_rate),
                               T, F)) %>%
  left_join(reasc_rates %>%
              group_by(pit_code, origin) %>%
              summarize(across(starts_with("reasc"),
                               list(mean = mean),
                               .names = "{.col}_{.fn}"),
                        .groups = "drop")) %>%
  rowwise() %>%
  mutate(across(reasc_rate,
                ~ if_else(reasc_avail,
                          .,
                          reasc_rate_mean)),
         across(reasc_rate_se,
                ~ if_else(reasc_avail,
                          .,
                          reasc_rate_se_mean))) %>%
  select(-reasc_rate_mean,
         -reasc_rate_se_mean) %>%
  group_by(year, origin) %>%
  mutate(reasc_rate_pra = reasc_rate[pit_code == "PRA"],
         reasc_se_pra = reasc_rate_se[pit_code == "PRA"]) %>%
  ungroup() %>%
  mutate(reasc_rate = if_else(!reasc_avail,
                              reasc_rate_pra,
                              reasc_rate),
         reasc_rate_se = if_else(!reasc_avail,
                                 reasc_se_pra,
                                 reasc_rate_se)) %>%
  rowwise() %>%
  mutate(tot_escp = win_cnt * prop_org * (1 - reasc_rate) / trans_est,
         tot_escp_se = msm::deltamethod(~ x1 * x2 * (1 - x3) / x4,
                                        mean = c(win_cnt,
                                                 prop_org,
                                                 reasc_rate,
                                                 trans_est),
                                        cov = diag(c(0,
                                                     prop_org_se,
                                                     reasc_rate_se,
                                                     trans_se)^2))) %>%
  mutate(tot_escp_cv = tot_escp_se / tot_escp) %>%
  mutate(priest_cnt = tot_escp * (1 + reasc_rate_pra),
         priest_cnt_se = msm::deltamethod(~ x1 * (1 + x2),
                                          mean = c(tot_escp,
                                                   reasc_rate_pra),
                                          cov = diag(c(tot_escp_se,
                                                       reasc_se_pra)^2))) %>%
  ungroup() %>%
  mutate(dam = fct_relevel(dam,
                           "Tumwater",
                           after = Inf))

# # drop Tumwater
# pit_move_all %<>%
#   filter(dam_code != "TUM")

#-----------------------------------------------------------------
# calculate what total Priest counts "should have been"
pit_move_all %>%
  group_by(year, dam, pit_code, reasc_avail) %>%
  summarize(priest_cnt = sum(priest_cnt),
            priest_cnt_se = sqrt(sum(priest_cnt_se^2))) %>%
  left_join(pit_move_all %>%
              filter(pit_code == "PRA") %>%
              select(year, win_cnt) %>%
              distinct()) %>%
  mutate(across(priest_cnt,
                ~ if_else(pit_code == "PRA",
                          win_cnt,
                          priest_cnt))) %>%
  mutate(across(priest_cnt_se,
                ~ if_else(pit_code == "PRA",
                          NA_real_,
                          .))) %>%
  rowwise() %>%
  mutate(rel_diff = (priest_cnt - win_cnt) / win_cnt,
         rel_diff_se = msm::deltamethod(~ (x1 - x2) / x2,
                                        c(priest_cnt,
                                          win_cnt),
                                        diag(c(priest_cnt_se, 0)^2)),
         across(starts_with("rel_diff"),
                ~ . * 100)) %>%
  ungroup() %>%
  filter(pit_code != "TUM") %>%
  ggplot(aes(x = as.factor(year),
             y = rel_diff,
             color = dam,
             shape = reasc_avail)) +
  geom_errorbar(aes(ymin = qnorm(0.025, rel_diff, rel_diff_se),
                    ymax = qnorm(0.975, rel_diff, rel_diff_se)),
                width = 0,
                position = position_dodge(d_w)) +
  geom_point(size = 3,
             position = position_dodge(d_w)) +
  scale_color_brewer(palette = "Set1",
                     name = "Dam") +
  scale_shape_manual(values = c("TRUE" = 19,
                                "FALSE" = 1),
                     name = "Reascension Query\nAvailable") +
  labs(x = "Spawn Year",
       y = "Relative Difference (%)\nvs. Priest Counts") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = "bottom")


#-----------------------------------------------------------------
# make a density plot
set.seed(5)
dens_comp_p = pit_move_all %>%
  select(year, dam, origin, win_cnt,
         matches('priest_cnt')) %>%
  group_by(year, dam, win_cnt) %>%
  summarise(priest_cnt = sum(priest_cnt),
            priest_cnt_se = sqrt(sum(priest_cnt_se^2)),
            .groups = "drop") %>%
  mutate(priest_cnt_se = if_else(dam == "PriestRapids",
                                 NA_real_,
                                 priest_cnt_se)) %>%
  # mutate(across(win_cnt:priest_cnt_se,
  #               ~ . / 1000)) %>%
  filter(dam != "PriestRapids") %>%
  mutate(samps = map2(priest_cnt,
                      priest_cnt_se,
                      .f = function(x, y) rnorm(10000, x, y))) %>%
  unnest(samps) %>%
  ggplot(aes(x = samps,
             color = dam,
             fill = dam)) +
  scale_color_brewer(palette = "Set2",
                     name = "Dam Count\nSource") +
  scale_fill_brewer(palette = "Set2",
                    name = "Dam Count\nSource") +
  geom_density(alpha = 0.4) +
  geom_vline(data = dam_cnts %>%
               filter(dam == "PriestRapids",
                      year %in% unique(pit_move_all$year)),
             aes(xintercept = win_cnt),
             linetype = 2,
             lwd = 1) +
  facet_wrap(~ year,
             scales = "free") +
  scale_x_continuous(labels = function(x) x / 1000) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Equivalent of Total Counts\nat Priest Rapids Dam (x1,000)")
dens_comp_p

xy_p = pit_move_all %>%
  select(year, dam, origin, win_cnt,
         matches('priest_cnt')) %>%
  group_by(year, dam, win_cnt) %>%
  summarise(priest_cnt = sum(priest_cnt),
            priest_cnt_se = sqrt(sum(priest_cnt_se^2)),
            .groups = "drop") %>%
  mutate(priest_cnt_se = if_else(dam == "PriestRapids",
                                 NA_real_,
                                 priest_cnt_se)) %>%
  ggplot(aes(x = dam,
             y = priest_cnt,
             color = dam)) +
  geom_errorbar(aes(ymax = qnorm(0.975, priest_cnt, priest_cnt_se),
                    ymin = qnorm(0.025, priest_cnt, priest_cnt_se)),
                width = 0.3) +
  geom_point(size = 4) +
  facet_wrap(~ year,
             scales = "free_y") +
  scale_color_brewer(palette = "Set1",
                     name = "Dam Count Source") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = "none") +
  scale_y_continuous(labels = function(x) x / 1000) +
  labs(x = "Dam Count Source",
       y = "Estimated Counts at Priest (x1,000)")
xy_p

rel_xy_p = pit_move_all %>%
  select(year, dam, origin, win_cnt,
         matches('priest_cnt')) %>%
  group_by(year, dam, win_cnt) %>%
  summarise(priest_cnt = sum(priest_cnt),
            priest_cnt_se = sqrt(sum(priest_cnt_se^2)),
            .groups = "drop") %>%
  mutate(priest_cnt_se = if_else(dam == "PriestRapids",
                                 NA_real_,
                                 priest_cnt_se)) %>%
  group_by(year) %>%
  mutate(across(starts_with("priest_cnt"),
                ~ . / win_cnt[dam == "PriestRapids"]),
         across(priest_cnt,
                ~ . - 1)) %>%
  ggplot(aes(x = dam,
             y = priest_cnt,
             color = dam)) +
  geom_errorbar(aes(ymax = qnorm(0.975, priest_cnt, priest_cnt_se),
                    ymin = qnorm(0.025, priest_cnt, priest_cnt_se)),
                width = 0.3) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  facet_wrap(~ year,
             scales = "fixed") +
  # scales = "free_y") +
  scale_color_brewer(palette = "Set1",
                     name = "Dam Count Source") +
  theme(axis.text.x = element_text(angle = 80,
                                   hjust = 1),
        legend.position = "none") +
  scale_y_continuous(labels = function(x) x * 100) +
  labs(x = "Dam Count Source",
       y = "Relative Difference\nCompared to Priest (%)")
rel_xy_p

ggsave(here("outgoing/other/dam_cnt_comp_density.pdf"),
       dens_comp_p,
       width = 8,
       height = 6)

ggsave(here("outgoing/other/dam_cnt_comp_xy.pdf"),
       xy_p,
       width = 8,
       height = 6)

ggsave(here("outgoing/other/dam_cnt_comp_rel.pdf"),
       rel_xy_p,
       width = 8,
       height = 6)

d_wd = 0.5
# pit_move_all %>%
#   select(year, dam, origin, win_cnt,
#          matches('priest_cnt')) %>%
#   group_by(year, dam, win_cnt) %>%
#   summarise(priest_cnt = sum(priest_cnt),
#             priest_cnt_se = sqrt(sum(priest_cnt_se^2)),
#             .groups = "drop") %>%
#   mutate(origin = "All") %>%
#   mutate(priest_cnt_se = if_else(dam == "PriestRapids",
#                                  NA_real_,
#                                  priest_cnt_se)) %>%
#   bind_rows(pit_move_all) %>%
#   arrange(year, origin, dam) %>%
pit_move_all %>%
  ggplot(aes(x = as_factor(year),
             y = priest_cnt,
             color = dam)) +
  geom_errorbar(aes(ymax = qnorm(0.975, priest_cnt, priest_cnt_se),
                    ymin = qnorm(0.025, priest_cnt, priest_cnt_se)),
                width = 0.1,
                position = position_dodge(width = d_wd)) +
  geom_point(size = 2,
             position = position_dodge(width = d_wd)) +
  facet_wrap(~ origin,
             # scales = "fixed") +
             scales = "free_y") +
  scale_color_brewer(palette = "Set1",
                     name = "Dam Count Source") +
  theme(axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = "none") +
  labs(x = "Dam Count Source",
       y = "Estimated Counts at Priest")
