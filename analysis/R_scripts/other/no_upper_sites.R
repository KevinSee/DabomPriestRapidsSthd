# Author: Kevin See
# Purpose: remove some sites and re-run DABOM
# Created: 4/27/26
# Last Modified: 4/27/26
# Notes: Remove CHU, NAU and PEU sites. run for 3 or 4 years with range of escapements

#-----------------------------------------------------------------
# load needed libraries
library(PITcleanr)
library(DABOM)
library(tidyverse)
library(rjags)
# library(magrittr)
library(lubridate)
library(here)

#-----------------------------------------------------------------
# load configuration and site_df data
load(here('analysis/data/derived_data/site_config.rda'))

#-----------------------------------------------------------------
# Load required DABOM data
#-----------------------------------------------------------------
# set year
yr = 2022

for(yr in c(2011, 2014, 2017, 2022)) {
  cat(paste("Working on", yr, "\n"))
  cat(paste("Started at", Sys.time(), "\n\n"))

  # load processed detection histories & biological data
  load(here('analysis/data/derived_data/PITcleanr',
            paste0('UC_Steelhead_', yr, '.rda')))

  # drop sites from configuration
  configuration <-
    configuration |>
    filter(!site_code %in% c("CHU",
                             "NAU",
                             "PEU"))

  parent_child <-
    parent_child |>
    filter(!child %in% c("CHU",
                         "NAU",
                         "PEU"))

  # filter to keep only the observations you want to keep
  filter_obs <-
    prepped_ch |>
    filter(!node %in% c("CHU_U", "CHU_D",
                        "NAU_U", "NAU_D",
                        "PEU_U", "PEU_D")) |>
    # filter based on user_keep_obs, or auto_keep_obs if user_keep_obs is NA
    mutate(across(user_keep_obs,
                  ~ case_when(is.na(.) ~ auto_keep_obs,
                            .default = .))) |>
    filter(user_keep_obs)

  # bio_df <-
  #   bio_df |>
  #   filter(pit_tag %in% filter_obs$tag_code |
  #            second_pit_tag %in% filter_obs$tag_code)

  # determine origin of each fish
  fish_origin <-
    bio_df %>%
    rename(tag_code = pit_tag) |>
    filter(tag_code %in% unique(filter_obs$tag_code)) %>%
    select(tag_code, origin) %>%
    distinct()

  # file path to the default and initial model
  basic_modNm = here("analysis",
                     "model_files",
                     "No_Upper_Sites",
                     "PRA_DABOM.txt")

  writeDABOM(file_name = basic_modNm,
             parent_child = parent_child,
             configuration = configuration,
             time_varying = F)

  #------------------------------------------------------------------------------
  # Alter default model code for species and year of
  # interest; sets prior for some detection node efficiencies at 0 or 100%
  # based on actual tag detection data; 0% if no tags were seen
  #------------------------------------------------------------------------------

  # filepath for specific JAGS model code for species and year
  mod_path = here("analysis",
                  "model_files",
                  "No_Upper_Sites",
                  paste0('PRA_Steelhead_', yr, '.txt'))

  # writes species and year specific jags code
  fixNoFishNodes(init_file = basic_modNm,
                 file_name = mod_path,
                 filter_ch = filter_obs,
                 parent_child = parent_child,
                 configuration = configuration,
                 by_origin = FALSE,
                 fish_origin = fish_origin)

  #------------------------------------------------------------------------------
  # Creates a function to spit out initial values for MCMC chains
  init_fnc = setInitialValues(filter_obs,
                              parent_child,
                              configuration)

  # Create all the input data for the JAGS model
  jags_data = createJAGSinputs(filter_ch = filter_obs,
                               parent_child = parent_child,
                               configuration = configuration,
                               fish_origin = fish_origin)

  # Tell JAGS which parameters in the model that it should save.
  jags_params = setSavedParams(model_file = mod_path,
                               time_varying = F)


  # # run the model
  # jags = jags.model(mod_path,
  #                   data = jags_data,
  #                   inits = init_fnc,
  #                   # n.chains = 1,
  #                   # n.adapt = 5)
  #                   n.chains = 4,
  #                   n.adapt = 10000)
  #
  #
  # #--------------------------------------
  # # take MCMC samples from the posteriors
  # dabom_mod = coda.samples(jags,
  #                          jags_params,
  #                          # n.iter = 10)
  #                          n.iter = 5000,
  #                          thin = 10)


  #-------------------------------------
  # use jagsUI to run in parallel
  library(jagsUI)

  set.seed(123)
  jags_mod <-
    jags(data = jags_data,
         inits = init_fnc,
         parameters.to.save = jags_params,
         model.file = mod_path,
         n.chains = 4,
         n.adapt = 1000,
         n.iter = 10000,
         n.burnin = 5000,
         n.thin = 10,
         parallel = T,
         n.cores = 4,
         DIC = F,
         verbose = T)

  dabom_mod <-
    jags_mod$samples
  rm(jags_mod)

  #-------------------------------------
  # save some objects
  save(dabom_mod,
       jags_data,
       filter_obs,
       bio_df,
       file = here("analysis",
                   "data",
                   "derived_data",
                   "model_fits",
                   "No_Upper_Sites",
                   paste0('PRA_DABOM_Steelhead_', yr,'.rda')))

  rm(dabom_mod, jags_data, filter_obs, bio_df)
}

#------------------------------------------------------------------------------
# make estimates of abundance and detection probability
#------------------------------------------------------------------------------
# yr <- c(2011, 2014, 2017, 2022)[4]

detect_old = detect_new = NULL
escape_old = escape_new = NULL
for(yr in c(2011, 2014, 2017, 2022)) {
  # load results from full model
  dam_cnt_name <-
    case_when(yr < 2016 ~ "PriestRapids",
              .default = "RockIsland")

  load(here("analysis/data/derived_data/estimates",
            dam_cnt_name,
            paste0("UC_Sthd_DABOM_", yr, ".rda")))

  # combine detection summaries
  if(is.null(detect_old)) {
    detect_old = detect_summ |>
      tibble::add_column(species = "Steelhead",
                         spawn_year = yr,
                         .before = 0)
  } else {
    detect_old <-
      detect_old |>
      bind_rows(detect_summ |>
                  tibble::add_column(species = "Steelhead",
                                     spawn_year = yr,
                                     .before = 0))
  }

  # combine escapement summaries
  if(is.null(escape_old)) {
    escape_old = escape_summ
  } else {
    escape_old <-
      escape_old |>
      bind_rows(escape_summ)
  }

  # drop sites from configuration
  configuration <-
    configuration |>
    filter(!site_code %in% c("CHU",
                             "NAU",
                             "PEU"))

  parent_child <-
    parent_child |>
    filter(!child %in% c("CHU",
                         "NAU",
                         "PEU"))

  # load missing sites model run
  load(here("analysis",
            "data",
            "derived_data",
            "model_fits",
            "No_Upper_Sites",
            paste0('PRA_DABOM_Steelhead_', yr,'.rda')))

  # summarize detection probabilities
  if(is.null(detect_new)) {
    detect_new <-
      summariseDetectProbs(dabom_mod = dabom_mod,
                           filter_ch = filter_obs) |>
      tibble::add_column(species = "Steelhead",
                         spawn_year = yr,
                         .before = 0)
  } else {
    detect_new <-
      detect_new |>
      bind_rows(
        summariseDetectProbs(dabom_mod = dabom_mod,
                             filter_ch = filter_obs) |>
          tibble::add_column(species = "Steelhead",
                             spawn_year = yr,
                             .before = 0)
      )
  }

  # compile all movement probabilities, and multiply them appropriately
  trans_post_new <-
    extractTransPost(dabom_mod,
                     parent_child,
                     configuration)


  trans_df_new <-
    compileTransProbs(trans_post_new,
                      parent_child) |>
    select(-main_branch) |>
    mutate(across(origin,
                  ~ case_match(.,
                               1 ~ "W",
                               2 ~ "H",
                               .default = NA_character_)))

  # generate MCMC draws of total abundance by origin
  abund_post_new <-
    org_escape |>
    crossing(chain = 1:max(trans_df_new$chain)) |>
    group_by(origin,
             chain) |>
    summarise(tot_esc_samp = map2(tot_escp,
                                  tot_escp_se,
                                  .f = function(x, y) {
                                    tibble(tot_abund = rnorm(max(trans_df_new$iter),
                                                             mean = x,
                                                             sd = y)) %>%
                                      mutate(iter = 1:n())
                                  }),
              .groups = "drop") |>
    unnest(cols = tot_esc_samp)


  # translate movement estimates to escapement
  escape_post_new <-
    calcAbundPost(trans_df_new,
                  abund_post_new)

  escape_summ_new <-
    summarisePost(escape_post_new,
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


  if(is.null(escape_new)) {
    escape_new = escape_summ_new
  } else {
    escape_new <-
      escape_new |>
      bind_rows(escape_summ_new)
  }

}

# combine old and new estimates
# for detection probability
detect_comp <-
  bind_rows(
    detect_old |>
      filter(str_detect(node, "CHL") |
               str_detect(node, "NAL") |
               str_detect(node, "PES")) |>
      arrange(spawn_year,
              node) |>
      mutate(source = "old"),

  detect_new |>
    filter(str_detect(node, "CHL") |
             str_detect(node, "NAL") |
             str_detect(node, "PES")) |>
    arrange(spawn_year,
            node) |>
    mutate(source = "new")
) |>
  select(spawn_year,
         node,
         n_tags,
         source,
         mean,
         median,
         mode,
         sd,
         ends_with("ci")) |>
  pivot_wider(names_from = source,
              values_from = c(mean:sd,
                              ends_with("ci"))) |>
  mutate(across(spawn_year,
                as.factor))

# for escapement
escape_comp <-
  bind_rows(
    escape_old |>
      filter(str_detect(location, "CHL") |
               str_detect(location, "NAL") |
               str_detect(location, "PES"),
             str_detect(location, "_bb", negate = T)) |>
      mutate(source = "old"),

    escape_new |>
      filter(str_detect(location, "CHL") |
               str_detect(location, "NAL") |
               str_detect(location, "PES")) |>
      mutate(source = "new")
  ) |>
  select(spawn_year,
         location,
         origin,
         source,
         mean,
         median,
         mode,
         sd,
         ends_with("ci")) |>
  pivot_wider(names_from = source,
              values_from = c(mean:sd,
                              ends_with("ci"))) |>
  mutate(across(spawn_year,
                as.factor))

#----------------------------------------------------
# make some figures
theme_set(theme_bw())
#----------------------------------------------------
# detection probability estimates
detect_comp |>
  filter(n_tags > 0) |>
  ggplot(aes(x = median_old,
             y = median_new)) +
  geom_abline(linetype = 2) +
  geom_errorbar(aes(ymin = lower_ci_new,
                    ymax = upper_ci_new,
                    color = node),
                width = 0) +
  geom_errorbar(aes(xmin = lower_ci_old,
                    xmax = upper_ci_old,
                    color = node),
                width = 0) +
  geom_point(aes(color = node,
                 shape = spawn_year),
             size = 3) +
  geom_smooth(method = lm,
              formula = y ~ x - 1) +
  labs(x = "Using All Sites",
       y = "Removing Upper Sites",
       title = "Detection Probability",
       color = "Node")



# comparing standard error of detection probabilities
detect_comp |>
  filter(n_tags > 0) |>
  ggplot(aes(x = sd_old,
             y = sd_new)) +
  geom_abline(linetype = 2) +
  geom_point(aes(color = node,
                 shape = spawn_year),
             size = 3) +
  geom_smooth(method = lm,
              formula = y ~ x - 1) +
  theme_bw() +
  labs(x = "Using All Sites",
       y = "Removing Upper Sites",
       title = "SE of Detection Probability",
       color = "Node")

# for abundance
escape_comp |>
  ggplot(aes(x = median_old,
             y = median_new)) +
  geom_abline(linetype = 2) +
  geom_errorbar(aes(ymin = lower_ci_new,
                    ymax = upper_ci_new,
                    color = spawn_year),
                width = 0) +
  geom_errorbar(aes(xmin = lower_ci_old,
                    xmax = upper_ci_old,
                    color = spawn_year),
                width = 0) +
  geom_point(aes(color = spawn_year,
                 shape = location),
             size = 3) +
  geom_smooth(method = lm,
              formula = y ~ x - 1) +
  theme_bw() +
  facet_wrap(~ origin) +
  # scale_x_continuous(trans = "log",
  #                    breaks = scales::breaks_pretty(n = 4)) +
  # scale_y_continuous(trans = "log",
  #                    breaks = scales::breaks_pretty(n = 4)) +
  labs(x = "Using All Sites",
       y = "Removing Upper Sites",
       title = "Escapement",
       shape = "Site",
       color = "Year")

#-------------------------------
# summary stats of comparisons

escape_comp |>
  mutate(err = median_new - median_old,
         rel_err = err / median_old) |>
  summarize(ME = mean(err),
            MRE = mean(rel_err) * 100,
            MAE = mean(abs(err)),
            MAPE = mean(abs(rel_err)) * 100,
            RMSE = sqrt(mean(err^2)))

escape_comp |>
  mutate(err = median_new - median_old,
         rel_err = err / median_old) |>
  group_by(location,
           origin) |>
  summarize(ME = mean(err),
            MRE = mean(rel_err) * 100,
            MAE = mean(abs(err)),
            MAPE = mean(abs(rel_err)) * 100,
            RMSE = sqrt(mean(err^2)),
            .groups = "drop")


detect_comp |>
  filter(n_tags != 0) |>
  mutate(err = median_new - median_old,
         rel_err = err / median_old) |>
  summarize(ME = mean(err),
            MRE = mean(rel_err) * 100,
            MAE = mean(abs(err)),
            MAPE = mean(abs(rel_err)) * 100,
            RMSE = sqrt(mean(err^2)))

detect_comp |>
  filter(n_tags != 0) |>
  # filter(mean_new != 1,
  #      mean_old != 1) |>
  mutate(err = median_new - median_old,
         rel_err = err / median_old) |>
  group_by(node) |>
  summarize(ME = mean(err),
            MRE = mean(rel_err) * 100,
            MAE = mean(abs(err)),
            MAPE = mean(abs(rel_err)) * 100,
            RMSE = sqrt(mean(err^2)),
            .groups = "drop")
