# Author: Kevin See
# Purpose: Develop configuration file for DABOM
# Created: 4/1/20
# Last Modified: 3/25/24
# Notes:
#
# # install some needed packages
# install.packages(c("tidyverse",
#                    "devtools",
#                    "here",
#                    "sf",
#                    "magritter",
#                    "readxl",
#                    "writexl",
#                    "janitor",
#                    "rjags",
#                    "msm",
#                    "moments",
#                    "coda"))
#
# remotes::install_github("KevinSee/STADEM")
# remotes::install_github("KevinSee/PITcleanr")
# remotes::install_github("KevinSee/DABOM")

#-----------------------------------------------------------------
# load needed libraries
library(PITcleanr)
library(tidyverse)
library(magrittr)
library(sf)
library(here)

#-----------------------------------------------------------------
# set starting point
root_site = "PRA"

# build configuration table (requires internet connection)
org_config = buildConfig(node_assign = "array",
                         array_suffix = "UD")

# customize some nodes based on DABOM framework
configuration = org_config %>%
  # manually add site for Colockum Creek (not in PTAGIS)
  bind_rows(tibble(site_code = 'CLK',
                   config_id = 100,
                   antenna_id = 'A1',
                   node = 'CLK',
                   # making these up
                   start_date = as.POSIXct(lubridate::ymd('20100101')),
                   site_type = 'INT',
                   site_name = 'Colockum Creek',
                   antenna_group = 'Single Colockum Ck',
                   site_description = 'Temporary single antenna.',
                   site_type_name = 'Instream Remote Detection System',
                   rkm = '740.001',
                   rkm_total = 741,
                   # this puts CLK upstream of RIA
                   latitude = 47.3707357269787,
                   longitude = -120.25617371760839)) %>%
  # this puts CLK on Colockum Creek, but between PRA and RIA
  # latitude = 47.29722788926544,
  # longitude = -120.10577913008702)) %>%
  filter(!(site_code == 'WAN' & site_type == 'MRR'),
         !(site_code == 'TMF' & site_type == 'MRR'),
         !(site_code == 'PRO' & site_type == 'MRR')) %>%
  mutate(across(node,
                ~ case_when(site_code %in% c('RIA', 'RRF', 'WEA', 'PRV', 'PRH') ~ site_code,
                            site_code == 'PRDLD1' ~ "PRA",
                            . == "LWE" ~ "LWE_D",
                            site_code == "LWB" ~ "LWE_D",
                            # any fish seen at Dryden dam should also be seen at LWE
                            site_code == 'DRY' ~ "LWE_U",
                            site_code %in% c('TUF', 'TUMFBY', 'TUM') ~ "TUM",
                            site_code == 'LNF' & antenna_id %in% c('01', '02') ~ "LNF_U",
                            site_code == 'LNF' & antenna_id %in% c('03', '04') ~ "LNF_D",
                            site_code == 'LEAV' ~ "LNF_U",
                            site_code == 'ICL' & config_id == 100 ~ "ICL_D",
                            site_code == 'CHIWAC' ~ "CHW_U",
                            site_code == 'CHIWAR' ~ "CHL_U",
                            site_code == "CWT" ~ "CHL_U",
                            site_code == 'WHITER' ~ "WTL_U",
                            site_code == 'LWENAT' ~ "LWN_U",
                            site_code == 'NASONC' ~ "NAL_U",
                            # any fish seen at Chiwawa acclimation pond gets moved to CHL
                            site_code == 'CHP' ~ 'CHL_U',
                            site_code == 'EBO' ~ "EBO_D",
                            site_code == 'RRJ' ~ 'RRF',
                            site_code == "MAD" & config_id == 110 & antenna_id == "01" ~ "MAD_U",
                            site_code == 'EHL' & config_id == 100 & antenna_id == '02' ~ 'EHL_D',
                            site_code == 'EHL' & config_id == 100 & antenna_id == '01' ~ 'EHL_U',
                            site_code == 'EHL' & config_id == 110 & antenna_id == '03' ~ 'EHL_D',
                            site_code == 'EHL' & config_id == 110 & antenna_id %in% c('01', '02') ~ 'EHL_U',
                            # combine a couple sites in the Entiat
                            site_code %in% c("ENS", "ENM") ~ "ENA_U",
                            site_code == "WEH" & antenna_id == "A2" ~ "WEH_D",
                            site_code == "WEH" & antenna_id != "A2" ~ "WEH_U",
                            site_code == "LMB" ~ "LMR_D",
                            site_code == 'MRC' ~ 'MRC_D',
                            site_code %in% c('SSC', '18N', 'MHB', 'M3R', 'MWF') ~ 'MRC_U',
                            site_code == 'LLC' & config_id == 100 & antenna_id %in% c("D1", "D2") ~ "LLC_U",
                            # ZSL has definitive up/down antennas in initial configurations, but it gets more complicated after that
                            site_code == "ZSL" &
                              str_detect(antenna_group,
                                         "(?i)Weir 3") &
                              config_id %in% c("100", "110") ~ "ZSL_D",
                            site_code == "ZSL" &
                              str_detect(antenna_group,
                                         "(?i)Weir 2") &
                              config_id %in% c("100", "110") ~ "ZSL_U",
                            site_code == "ZSL" &
                              !config_id %in% c("100", "110") ~ "ZSL_D",
                            site_code == 'BPC' &
                              config_id == 100 &
                              antenna_id %in% c("C1", "C2") ~ "BPC_U",
                            site_code == 'BPC' &
                              config_id == 100 &
                              antenna_id %in% c("C3") ~ "BPC_D",
                            site_code == 'PRO' & site_type == 'INT' ~ 'PRO_D',
                            # grab all sites upstream of Prosser dam, and assign them to PRO_U
                            site_code != "PRO" &
                              as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) == 539 &
                              as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,2]) >= 76 ~ "PRO_U",
                            site_code == 'ICH' ~ 'ICH_D',
                            str_detect(rkm, '522\\.') & rkm_total > 538 ~ 'ICH_U',
                            site_code == 'MDR' ~ 'MDR_D',
                            site_code %in% c('LWD', 'BGM', 'NBA', 'MCD') ~ 'MDR_U',
                            site_code == 'HST' ~ 'HST_D',
                            site_code %in% c('BBT', 'COP', 'PAT') ~ 'HST_U',
                            as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) == 351 &
                              site_code != "JD1" ~ "JD1_U",
                            site_code == 'JD1' ~ 'JD1_D',
                            site_code != 'JD1' &
                              as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) < 351 &
                              str_detect(site_code, "^COLR", negate = T) ~ 'JDA',
                            .default = node
                            ))) |>
  distinct() %>%
  # correct a couple rkm values
  mutate(across(rkm,
                ~ case_when(site_code == 'SA1' ~ '858.041.003',
                            site_code == 'TON' ~ '858.133.001',
                            str_detect(node, "WEH") ~ '829.001',
                            site_code == "MSH" ~ '843.082',
                            site_code == "METH" ~ '843.083',
                            .default = .)),
         across(rkm_total,
                ~ case_when(site_code == 'SA1' ~ 902,
                            site_code == 'TON' ~ 992,
                            str_detect(node, "WEH") ~ 830,
                            site_code == "MSH" ~ 925,
                            site_code == "METH" ~ 926,
                            .default = .)))

# Node network for DABOM

# get spatial object of sites used in model
sites_sf = writeOldNetworks()$PriestRapids %>%
  mutate(across(c(SiteID,
                  Step3),
                ~ recode(.,
                         "BelowJD1" = "JDA")),
         path = str_replace(path, "BelowJD1", "JDA")) %>%
  rename(site_code = SiteID) %>%
  select(site_code) |>
  # filter out site at the Methow Fish Hatchery, we're not going to use them
  filter(!site_code %in% c("MSH",
                           "METH")) |>
  # drop UWE in the upper Wenatchee, as it was removed in 2024
  filter(!site_code %in% c("UWE")) |>
  # add a few sites in the Okanogan region
  # exclude CHJO because fish detected there have some strange detection histories
  bind_rows(
    tibble(
      site_code = c(#"CHJO",
        "OMF",
        "OKM",
        "OKW",
        "SKA",
        "OKS",
        "OKP",
        "OMH",
        "SAD",
        "ANR"))) %>%
  # add a site in the upper Methow
  bind_rows(
    tibble(
      site_code = c("MRU")
    )
  ) |>
  left_join(configuration,
            by = join_by(site_code)) %>%
  group_by(site_code) %>%
  filter(config_id == max(config_id)) %>%
  ungroup() %>%
  select(site_code,
         site_name,
         site_type = site_type_name,
         type = site_type,
         rkm,
         site_description = site_description,
         latitude, longitude) %>%
  distinct() %>%
  filter(!is.na(latitude)) %>%
  st_as_sf(coords = c("longitude",
                      "latitude"),
           crs = 4326) %>%
  st_transform(crs = 5070)

#-----------------------------------------------------------------
# download the NHDPlus v2 flowlines
# upstream extent of study area (cut off areas further upstream)
upstrm_loc = "Chief Joseph Dam"

library(ggmap)

upstrm_comid = ggmap::geocode(upstrm_loc, output = "latlon") %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) %>%
  nhdplusTools::discover_nhdplus_id()

# do you want flowlines downstream of root site? Set to TRUE if you have downstream sites
dwn_flw = T
nhd_list = queryFlowlines(sites_sf = sites_sf,
                          root_site_code = root_site,
                          min_strm_order = 2,
                          max_upstream_comid = upstrm_comid,
                          combine_up_down = dwn_flw)

# compile the upstream and downstream flowlines
flowlines = nhd_list$flowlines

#-----------------------------------------------------------------
# plot the flowlines and the sites
ggplot() +
  geom_sf(data = flowlines,
          aes(color = as.factor(streamorde),
              linewidth = streamorde)) +
  scale_color_viridis_d(direction = -1,
                        option = "D",
                        name = "Stream\nOrder",
                        end = 0.9) +
  scale_linewidth_continuous(range = c(0.2, 2),
                             name = "Stream\nOrder") +
  geom_sf(data = nhd_list$basin,
          fill = NA,
          lwd = 2) +
  geom_sf(data = sites_sf,
          size = 4,
          color = "black") +
  geom_sf_label(data = sites_sf |>
                  filter(site_code != root_site),
                aes(label = site_code)) +
  # ggrepel::geom_label_repel(
  #   data = sites_sf |>
  #     filter(site_code != root_site),
  #   aes(label = site_code,
  #       geometry = geometry),
  #   size = 2,
  #   stat = "sf_coordinates",
  #   min.segment.length = 0,
  #   max.overlaps = 100
  # ) +
  geom_sf_label(data = sites_sf %>%
                  filter(site_code == root_site),
                aes(label = site_code),
                color = "red") +
  theme_bw() +
  theme(axis.title = element_blank())

ggsave(here('analysis/figures',
            "DABOM_site_map.pdf"),
       width = 8,
       height = 11)

#-----------------------------------------------------------------
# build parent child table
parent_child = sites_sf %>%
  buildParentChild(flowlines,
                   # rm_na_parent = T,
                   add_rkm = F) %>%
  editParentChild(fix_list = list(c("JDA", 'ICH', "PRA"),
                                  c("JDA", 'RSH', "PRA"),
                                  c("JDA", 'JD1', "PRA"),
                                  c("JDA", 'PRO', "PRA"),
                                  c("JDA", 'TMF', "PRA"),
                                  c("JDA", 'PRV', "PRA"),
                                  c("RSH", 'PRH', 'PRA'),
                                  c(NA, "JDA", 'PRA'),
                                  c("ICL", 'TUM', "LWE"),
                                  # c("LNF", 'ICM', "ICL"),
                                  c(NA, "LNF", "ICL"),
                                  # c("RIA", "WEA", 'RRF'),
                                  # c("RIA", "WEH", 'RRF'),
                                  # c("RIA", "ENL", "RRF"),
                                  c("RIA", "EBO", "RRF"),
                                  c("EBO", "WEH", 'RRF'),
                                  c("EBO", "WEA", 'RRF'),
                                  c("EBO", "ENL", 'RRF'),
                                  c("WEH", "LMR", "WEA"),
                                  c("WEH", "OKL", "WEA"),
                                  c("WEH", "FST", 'WEA'),
                                  c("EHL", 'ENA', 'ENL'),
                                  c("EHL", 'MAD', 'ENL'),
                                  c("SCP", "MRW", "MRC"),
                                  # c("METH", "MRW", "MRC"),
                                  # c("SCP", "METH", "MSH"),
                                  # c("SCP", 'MSH', 'MRC'),
                                  # c("SCP", "WINT", "MRC"),
                                  c("WHS", "OKC", "ZSL"),
                                  c("WHS", "ZSL", "OKL"),
                                  c("OMK", "OMF", "OBF"),
                                  c("OBF", "OMH", "OMF"),
                                  c("ZSL", 'OKV', 'OKC'),
                                  c("ZSL", "OKM", "OKC"),
                                  c("OKC", "SKA", "OKM"),
                                  c("OKC", "OKW", "OKM"),
                                  c("OKC", "OKS", "SKA"),
                                  c("OKC", "OKP", "SKA"),
                                  c("JOH", 'WHS', 'OKL'),
                                  c("JOH", 'BPC', 'OKL'),
                                  c("JOH", 'ANT', 'OKL'),
                                  c("JOH", 'TNK', 'OKL'),
                                  c("JOH", 'AEN', 'OKL')),
                  switch_parent_child = list(c("RSH", "PRA"))) %>%
  filter(!parent %in% c("WEH", "PRH"))

parent_child %>%
  filter(child %in% child[duplicated(child)])


paths <- buildPaths(parent_child)

test_sites <- paths |>
  filter(str_detect(path, "MRC")) |>
  pull(end_loc)
parent_child |>
  filter(parent %in% test_sites |
           child %in% test_sites) |>
  plotNodes()

# add RKMs from configuration file (since we had to fix at least one from PTAGIS)
parent_child %<>%
  left_join(configuration %>%
              select(parent = site_code,
                     parent_rkm = rkm) %>%
              distinct(),
            by = "parent") %>%
  left_join(configuration %>%
              select(child = site_code,
                     child_rkm = rkm) %>%
              distinct(),
            by = "child") %>%
  distinct()

# reduce configuration file to those sites included in parent-child table
configuration <-
  configuration |>
  mutate(node_site = str_remove(node, "_U$"),
         across(node_site,
                ~ str_remove(., "_D"))) |>
  filter(node_site %in% union(parent_child$parent,
                              parent_child$child))



sites_df = writeOldNetworks()$PriestRapids %>%
  mutate(across(c(SiteID, Step3),
                ~ recode(.,
                         "BelowJD1" = "JDA")),
         path = str_replace(path, "BelowJD1", "JDA")) %>%
  rename(site_code = SiteID)

# sites_df |>
#   anti_join(configuration)
#
# configuration |>
#   select(node_site) |>
#   distinct() |>
#   anti_join(sites_df,
#             by = join_by(node_site == site_code))


ques_locs = sites_df %>%
  filter(grepl('Wenatchee', path)) %>%
  # filter(grepl("Entiat", path)) %>%
  # filter(grepl('Methow', path)) %>%
  # filter(grepl("Okanogan", path)) %>%
  # filter(nchar(Step3) == 3,
  #        nchar(Step4) < 6,
  #        nchar(Step5) < 6,
  #        Step2 != "BelowPriest") %>%
  pull(site_code)

parent_child %>%
  filter(parent %in% ques_locs |
           child %in% ques_locs) %>%
  buildPaths() %>%
  left_join(sites_df %>%
              select(end_loc = site_code,
                     org_path = path))

#-----------------------------------------------------------------
# Save file.
save(configuration,
     sites_sf,
     flowlines,
     parent_child,
     file = here('analysis/data/derived_data/site_config.rda'))

#-----------------------------------------------------------------
# pull out configuration info about all sites in the model
uc_sites <- configuration %>%
  filter(site_code %in% sites_sf$site_code) %>%
  select(node) %>%
  distinct() %>%
  left_join(configuration) %>%
  select(site_code, node, site_name, site_type, rkm) %>%
  distinct() %>%
  arrange(node, site_code)

write_csv(uc_sites,
          file = here("analysis/data/derived_data",
                      "UC_DABOM_sites_nodes.csv"))

# save flowlines
st_write(flowlines,
         dsn = here("analysis/data/derived_data",
                    "UC_flowlines.gpkg"),
         append = FALSE)

#-----------------------------------------------------------------
# Build network diagram
# simple
pc_graph = plotNodes(parent_child,
                     layout = "tree")
pc_graph


pc_nodes_graph = parent_child %>%
  addParentChildNodes(configuration) %>%
  plotNodes()

# control more settings
node_order = buildNodeOrder(parent_child) %>%
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
                                                                     "Other"))))))) %>%
  select(-starts_with("step")) %>%
  mutate(across(branch_nm,
                as.factor))

nodes = buildNodeGraph(parent_child) %>%
  as_tibble() %>%
  left_join(node_order %>%
              select(label = node,
                     branch_nm))
edges = parent_child %>%
  left_join(nodes, by = c('parent' = 'label')) %>%
  rename(from = index) %>%
  left_join(nodes, by = c('child' = 'label')) %>%
  rename(to = index) %>%
  select(from, to)

library(ggraph)
node_graph = tidygraph::tbl_graph(nodes = nodes,
                                  edges = edges)

# pd = 0.1

node_p = node_graph %>%
  # ggraph(layout = "tree") +
  # ggraph(layout = "partition") +
  # ggraph(layout = "kk") +
  ggraph(layout = "tree",
         circular = F,
         flip.y = F) +
  geom_edge_link(arrow = arrow(length = unit(2, 'mm'),
                               type = "closed"),
                 end_cap = circle(4, 'mm')) +
  geom_node_point(size = 7,
                  # position = position_dodge2(width = pd),
                  aes(color = branch_nm)) +
  theme_graph(base_family = 'Times') +
  theme(legend.position = 'none') +
  scale_color_brewer(palette = "Set1",
                     na.value = "black") +
  geom_node_label(aes(label = label),
                  size = 1.5,
                  # position = position_dodge2(width = pd),
                  label.padding = unit(0.1, 'lines'),
                  label.size = 0.1)

node_p

# save as pdf
library(here)
ggsave(here("analysis/figures",
            "PriestRapids_DABOM_sites.pdf"),
       node_p,
       width = 9,
       height = 5)

# save as png
ggsave(here("analysis/figures",
            "PriestRapids_DABOM_sites.png"),
       node_p,
       width = 9,
       height = 5)
