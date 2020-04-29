# Length comp data for commercial BRF in SEAK
# Jane Sullivan
# Last updated: April 2020

# Background: directed commercial jig fishery for BRF.  NSEI and SSEI waters
# are closed, and I don't think there was historically much effort except for
# out of Sitka.
source("code/helper.R")

YEAR <- 2019 # most recent year of data

brf <- read_csv("data/brf_bio_port.csv",
           guess_max = 1000000)

colnames(brf) <- tolower(colnames(brf))

# only keep commercial longline (2) and commercial jig (4): based on pers comm
# with R Ehresmann the 2's are from DSR longline targets and are not directed.
# Because annual sample sizes are so low, I am combining them for the
# multiyear.csv and areafleet.csv but am removing project code 2 for the
# singleyear length comps
count(brf, project_code) 
count(brf, sex_code) # use female only data for brf! 1 = Male, 2 = Female, 99 = Indiscernible
count(brf, age_readability) # keep all but beware! not using age data in current models
count(brf, g_management_area_code) # keep all but know these are mostly representative of CSEO
count(brf, sample_type) # keep only random 

# Chose length bin ranges based on sportfish BRF distributions - could be revisted!

# Length bin definitions: If min_len = 24 cm and max_len = 58 cm, the length
# bins are 24, 26, 28, ... 58. These represent center of the bin, such that the
# 24 cm bin represent fish 23 cm to 24.9 cm. Fish smaller that 24 are omitted,
# fish larger are lumped into largest bin.

min_len <- 24; max_len <- 60

# Females only
brf_len <- brf %>% 
  filter(sex_code == 2 & project_code %in% c(4, 2) & 
           sample_type == "Random" & !is.na(length_millimeters)) %>% 
  mutate(length = length_millimeters / 10) %>% 
  filter(!c(length < min_len-1)) %>% 
  mutate(length2 = ifelse(length < min_len, min_len,
                          ifelse(length > max_len, max_len, length)),
         length_bin = cut(length2, breaks = seq(min_len-1.1, max_len+0.9, 2),
                          labels = paste(seq(min_len, max_len, 2)))) %>% 
  select(year, project_code, management_unit = g_management_area_code, length, length_bin)

# Multiyear ----

# Take years with sufficient sample size (originally was just going to do CSEO
# since that's where the bulk of the harvest is, but it's only a handful of
# year)
years <- brf_len %>% 
  # filter(management_unit == "CSEO") %>% 
  count(project_code, year) %>% 
  filter(n >= 50) %>% # Based on Cope expert opinion 2020-0429 
  pull(year)

ggplot(brf_len %>% 
         filter(year %in% years), #management_unit == "CSEO" & 
       aes(length, year, group = year, fill = year)) + 
  geom_vline(xintercept = 50) +
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      alpha = 0.3) +
  facet_wrap(~project_code) + 
  labs(x = "Fork length (cm)", y = "Year", title = "Commercial BRF length distributions") + 
  scale_y_reverse() +
  theme(legend.position = "none") 

ggsave("results/length_compositions/brf_length_distributions_multiyear_cf.png", dpi = 300, height = 6, width = 4, units = "in")

brf_len %>% 
  filter(year %in% years) %>% 
  count(year, length_bin) %>% 
  full_join(data.frame(expand.grid(year = sort(years),
                                   length_bin = sort(unique(brf_len$length_bin))))) %>% 
  replace_na(list(n = 0)) %>% 
  arrange(year, length_bin) %>% 
  dcast(year ~ length_bin, value.var = "n") %>% 
  write_csv("results/length_compositions/commbrf_multiyear_lencomps.csv")

# Areas as fleets (early years) ----

# Take areas with sufficient sample size pre-2006 (when sport rockfish starts)
# for SPR comparison - CSEO project 4 (jig) is directed, otherwise they're
# bycatch in the DSR longline (2)

brf_len <- brf_len %>% 
  mutate(management_unit_project = paste0(management_unit, "_", project_code))

areas <- brf_len %>% 
  filter( year < 2006) %>% 
  count(management_unit_project) %>% 
  filter(n >= 300) %>% 
  pull(management_unit_project)

ggplot(brf_len %>% 
         filter(year < 2006 & management_unit_project %in% areas), 
       aes(length, management_unit_project, group = management_unit_project, fill = management_unit_project)) + 
  geom_density_ridges(aes(point_fill = management_unit_project, point_color = management_unit_project),
                      alpha = 0.3) +
  labs(x = "Fork length (cm)", y = "Management area / Project", title = "Commercial BRF length distributions") + 
  scale_fill_viridis_d() +
  theme(legend.position = "none") 

ggsave("results/length_compositions/brf_length_distributions_areafleets_cf.png", dpi = 300, height = 6, width = 4, units = "in")

# Areas as fleets pre-2006 (early period)
brf_len %>% 
  filter(year < 2006 & management_unit_project %in% areas) %>% 
  count(management_unit_project, length_bin) %>% 
  full_join(data.frame(expand.grid(management_unit_project = areas,
                                   length_bin = sort(unique(brf_len$length_bin))))) %>% 
  replace_na(list(n = 0)) %>% 
  dcast(length_bin ~ management_unit_project, value.var = "n") %>% 
  arrange(length_bin) %>%
  write_csv("results/length_compositions/commbrf_areafleets_lencomps.csv")

# Single year ----

# Combine all data pre-2006 from directed fishery in CSEO
brf_len %>% 
  filter(year < 2006 & management_unit_project == "CSEO_4") %>% 
  count(length_bin) %>% 
  full_join(data.frame(expand.grid(length_bin = sort(unique(brf_len$length_bin))))) %>% 
  replace_na(list(n = 0)) %>% 
  arrange(length_bin) %>%
  write_csv("results/length_compositions/commbrf_singleyear_lencomps.csv")

