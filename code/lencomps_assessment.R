# Prep all length comps for assessment (sport and commercial)

# Set up ----

# Maps of management units:
# https://www.adfg.alaska.gov/index.cfm?adfg=CommercialByFisheryGroundfish.groundfishmaps_managementareas

source("code/helper.R")

YEAR <- 2019 # most recent year of data - filter out 2019 because catch data is only avail thru 2018


# Sport YEI & BRF data ----

# NOTE: from sport_rockfish_seak\data\...!! Transfered into
# comm_brf_yei_seak/data manually
sport <- full_join(read_csv(paste0("data/sf_brf_bio_aswl_seak_2006_", YEAR, ".csv"),
                          guess_max = 1000000),
                 read_csv(paste0("data/sf_ye_bio_wl_seak_2006_", YEAR, ".csv"),
                          guess_max = 1000000)) %>% 
  filter(year < YEAR)

sport <- sport %>% 
  filter(!is.na(snfl_length_mm) & !is.na(management_unit)) %>% 
  mutate(# Personal communication with K Howard 2020-04-23: EWYKT = EYKT + IBS
    # for harvest data. Make bio data match this assumption
    management_unit = ifelse(management_unit %in% c("EYKT", "IBS"), "EWYKT", management_unit),
    stock = case_when(species == "BRF" ~ "BRF",
                      species == "YE" & management_unit %in% c("SSEI", "NSEI") ~ "YEI",
                      species == "YE" & management_unit %in% c("CSEO", "NSEO", "EWYKT", "SSEO") ~ "YEO"),
    length = snfl_length_mm / 10) 

# Comm YEI data ----

yei <- read_csv("data/yei_bio_port.csv",
                guess_max = 1000000)
colnames(yei) <- tolower(colnames(yei))

# only keep commercial directed longline (2) and halibut longline bycatch (21),
# which we assume here has the same selectivity - could be revisited! Also keep
# atypical longline samples (8) - K Wood found out these were from increased
# sampling around Ketichikan during the fall of 1984 and winter/spring 1985
# (2020-04-27) https://www.adfg.alaska.gov/FedAidpdfs/afrbIL.258.pdf

yei <- yei %>% 
  filter(project_code %in% c(2, 8, 21) & sample_type == "Random" & 
           !is.na(length_millimeters)) %>% 
  mutate(length = length_millimeters / 10) %>% 
  filter(year < YEAR)

# YEI lencomps ----

# Decision 2020-04-29 to combine Sport and Commercial (just selected project
# codes) YE based on major overlap in length distributions (the two fleets
# select for similar lengths in the population)
sport %>% 
  filter(stock == "YEI") %>% 
  select(year, length) %>% 
  bind_rows(yei %>% 
              select(year, length)) %>% 
  arrange(year) -> yei_len

# Chose length bin ranges based on sportfish YE distributions - could be revisted!

# Length bin definitions: If min_len = 24 cm and max_len = 58 cm, the length
# bins are 24, 26, 28, ... 58. These represent center of the bin, such that the
# 24 cm bin represent fish 23 cm to 24.9 cm. Fish smaller that 24 are omitted,
# fish larger are lumped into largest bin.

min_len <- 28; max_len <- 72

yei_len <- yei_len %>% 
  filter(!c(length < min_len-1)) %>% 
  mutate(length2 = ifelse(length < min_len, min_len,
                          ifelse(length > max_len, max_len, length)),
         length_bin = cut(length2, breaks = seq(min_len-1.1, max_len+0.9, 2),
                          labels = paste(seq(min_len, max_len, 2)))) %>% 
  select(year, length, length_bin)

years <- yei_len %>% 
  count(year) %>% 
  filter(n >= 50) %>% # Expert opinion Jason Cope 2020-04-28 
  pull(year)

ggplot(yei_len %>% 
         filter(year %in% years), 
       aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      alpha = 0.3) +
  labs(x = "Fork length (cm)", y = "Year", title = "YEI length comps for assessment") + 
  scale_y_reverse() +
  theme(legend.position = "none") 

ggsave("results/assessment_inputs/YEI/yei_lengths.png", dpi = 300, height = 7, width = 4, units = "in")

yei_len %>% 
  filter(year %in% years) %>% 
  count(year, length_bin) %>% 
  full_join(data.frame(expand.grid(year = sort(years),
                                   length_bin = sort(unique(yei_len$length_bin))))) %>% 
  replace_na(list(n = 0)) %>% 
  arrange(year, length_bin) %>% 
  dcast(length_bin ~ year, value.var = "n") %>% 
  rename(Bin = length_bin) %>% 
  write_csv("results/assessment_inputs/YEI/Lengths.csv")

# Comm BRF data ----

brf <- read_csv("data/brf_bio_port.csv",
                guess_max = 1000000)
colnames(brf) <- tolower(colnames(brf))

# only keep commercial longline (2) and commercial jig (4): based on pers comm
# with R Ehresmann the 2's are from DSR longline targets and are not directed.
# Because annual sample sizes are so low, I am combining them for the
# multiyear.csv and areafleet.csv but am removing project code 2 for the
# singleyear length comps
count(brf, sex_code) # use female only data for brf! 1 = Male, 2 = Female, 99 = Indiscernible
count(brf, age_readability) # keep all but beware! not using age data in current models
count(brf, g_management_area_code) # keep all but know these are mostly representative of CSEO
count(brf, sample_type) # keep only random 

# Females only!
brf <- brf %>% 
  filter(sex_code == 2 & project_code %in% c(4, 2) & 
           sample_type == "Random" & !is.na(length_millimeters)) %>% 
  mutate(length = length_millimeters / 10) %>% 
  filter(year < YEAR)

# BRF len comps ----

# Two fleets here! Decision 2020-04-29 to combine Sport and Commercial jig
# (project 4) based on similarities in length distributions. Jig and sport
# select for smaller fish compared to commercial longline. This may be due to
# fishing in different habitat/depths. Sport and jig nearshore, shallower.
# Longline deeper. Initial run only use data where sex is known.

# Sport/commercial jig
brf_len <- brf %>% 
  filter(project_code == 4) %>% 
  select(year, length) %>% 
  bind_rows(sport %>% 
              # Females only!
              filter(species == "BRF" & sex == "F") %>% 
              select(year, length)) %>% 
  mutate(fishery = "Sport and Commercial Jig") %>% 
  bind_rows(brf %>% 
              filter(project_code == 2) %>% 
              select(year, length) %>% 
              mutate(fishery = "Commercial Longline"))

# Chose length bin ranges based on sportfish BRF distributions - could be
# revisted!

min_len <- 24; max_len <- 60

brf_len <- brf_len %>% 
  filter(!c(length < min_len-1)) %>% 
  mutate(length2 = ifelse(length < min_len, min_len,
                          ifelse(length > max_len, max_len, length)),
         length_bin = cut(length2, breaks = seq(min_len-1.1, max_len+0.9, 2),
                          labels = paste(seq(min_len, max_len, 2)))) %>% 
  select(year, fishery, length, length_bin)

brf_len <- brf_len %>% 
  mutate(year_fishery = paste0(year, "_", fishery))

years <- brf_len %>% 
  count(year_fishery) %>% 
  filter(n >= 50) %>% # Expert opinion Jason Cope 2020-04-28 
  pull(year_fishery)

ggplot(brf_len %>% 
         filter(year_fishery %in% years), 
       aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      alpha = 0.3) +
  geom_vline(xintercept = 50, lty = 2) +
  labs(x = "Fork length (cm)", y = "Year", title = "BRF length comps for assessment") + 
  scale_y_reverse() +
  theme(legend.position = "none") +
  facet_wrap(~fishery)

ggsave("results/assessment_inputs/BRF/brf_lengths.png", dpi = 300, height = 7, width = 5, units = "in")

# just sport/jig (sj) combos for years with sufficient sample size
sj_combos <- tibble(combos = years) %>% 
  filter(grepl("Sport", combos)) %>% 
  pull(combos)

sj_brf <- brf_len %>% 
  filter(year_fishery %in% sj_combos) 

sj_brf %>% 
  count(year, length_bin) %>% 
  full_join(data.frame(expand.grid(year = sort(unique(sj_brf$year)),
                                   length_bin = sort(unique(brf_len$length_bin))))) %>% 
  replace_na(list(n = 0)) %>% 
  arrange(year, length_bin) %>% 
  dcast(length_bin ~ year, value.var = "n") %>% 
  rename(Bin = length_bin) %>% 
  write_csv("results/assessment_inputs/BRF/Lengths.csv")

# just commercial longline comps for years with sufficient sample size
ll_combos <- tibble(combos = years) %>% 
  filter(grepl("Longline", combos)) %>% 
  pull(combos)

ll_brf <- brf_len %>% 
  filter(year_fishery %in% ll_combos)

ll_brf %>% 
  count(year, length_bin) %>% 
  full_join(data.frame(expand.grid(year = sort(unique(ll_brf$year)),
                                   length_bin = sort(unique(brf_len$length_bin))))) %>% 
  replace_na(list(n = 0)) %>% 
  arrange(year, length_bin) %>% 
  dcast(length_bin ~ year, value.var = "n") %>% 
  rename(Bin = length_bin) %>% 
  write_csv("results/assessment_inputs/BRF/Lengths2.csv")
