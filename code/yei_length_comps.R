# Length comp data for commercial YEI in SEAK
# Jane Sullivan
# Last updated: April 2020

# https://www.adfg.alaska.gov/index.cfm?adfg=CommercialByFisheryGroundfish.groundfishmaps_managementareas

source("code/helper.R")

YEAR <- 2019 # most recent year of data

yei <- read_csv("data/yei_bio_port.csv",
           guess_max = 1000000)

colnames(yei) <- tolower(colnames(yei))

# only keep commercial directed longline (2) and halibut longline bycatch (21),
# which we assume here has the same selectivity - could be revisited! Also keep
# atypical longline samples (8) - K Wood found out these were from increased
# sampling around Ketichikan during the fall of 1984 and winter/spring 1985
# (2020-04-27) [Monday 8:20 AM] Wood, Kellii L (DFG)
# https://www.adfg.alaska.gov/FedAidpdfs/afrbIL.258.pdf
count(yei, project_code) 
count(yei, sex_code) # keep all but know there are sex-specific data that could be utilized
count(yei, age_readability) # keep all but beware! not using age data in current models
count(yei, g_management_area_code) # should just be NSEI and SSEI
count(yei, sample_type) # keep only random 


# Chose length bin ranges based on sportfish YE distributions - could be revisted!

# Length bin definitions: If min_len = 24 cm and max_len = 58 cm, the length
# bins are 24, 26, 28, ... 58. These represent center of the bin, such that the
# 24 cm bin represent fish 23 cm to 24.9 cm. Fish smaller that 24 are omitted,
# fish larger are lumped into largest bin.

min_len <- 28; max_len <- 72

yei_len <- yei %>% 
  filter(project_code %in% c(2, 8, 21) & sample_type == "Random" & !is.na(length_millimeters)) %>% 
  mutate(length = length_millimeters / 10) %>% 
  filter(!c(length < min_len-1)) %>% 
  mutate(length2 = ifelse(length < min_len, min_len,
                          ifelse(length > max_len, max_len, length)),
         length_bin = cut(length2, breaks = seq(min_len-1.1, max_len+0.9, 2),
                          labels = paste(seq(min_len, max_len, 2)))) %>% 
  select(year, management_unit = g_management_area_code, length, length_bin)

# Multiyear ----

# Take years with sufficient sample size 
years <- yei_len %>% 
  count(year) %>% 
  filter(n >= 300) %>% 
  pull(year)

ggplot(yei_len %>% 
         filter(year %in% years), #management_unit == "CSEO" & 
       aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      alpha = 0.3) +
  labs(x = "Fork length (cm)", y = "Year", title = "Commercial YEI length distributions") + 
  scale_y_reverse() +
  theme(legend.position = "none") 

ggsave("results/length_compositions/yei_length_distributions_multiyear_cf.png", dpi = 300, height = 6, width = 4, units = "in")

yei_len %>% 
  filter(year %in% years) %>% 
  count(year, length_bin) %>% 
  full_join(data.frame(expand.grid(year = sort(years),
                                   length_bin = sort(unique(yei_len$length_bin))))) %>% 
  replace_na(list(n = 0)) %>% 
  arrange(year, length_bin) %>% 
  dcast(year ~ length_bin, value.var = "n") %>% 
  write_csv("results/length_compositions/commyei_multiyear_lencomps.csv")

# Areas as fleets (early years) ----

# Take areas with sufficient sample size pre-2006 (when sport rockfish starts)
# for SPR comparison
areas <- yei_len %>% 
  filter(year < 2006) %>% 
  count(management_unit) %>% 
  filter(n >= 300) %>% 
  pull(management_unit)

ggplot(yei_len %>% 
         filter(year < 2006 & management_unit %in% areas), 
       aes(length, management_unit, group = management_unit, fill = management_unit)) + 
  geom_density_ridges(aes(point_fill = management_unit, point_color = management_unit),
                      alpha = 0.3) +
  labs(x = "Fork length (cm)", y = "Management area", title = "Commercial YEI length distributions") + 
  scale_fill_viridis_d() +
  theme(legend.position = "none") 

ggsave("results/length_compositions/yei_length_distributions_areafleets_cf.png", dpi = 300, height = 6, width = 4, units = "in")

# Areas as fleets pre-2006 (early period)
yei_len %>% 
  filter(year < 2006 & management_unit %in% areas) %>% 
  count(management_unit, length_bin) %>% 
  full_join(data.frame(expand.grid(management_unit = areas,
                                   length_bin = sort(unique(yei_len$length_bin))))) %>% 
  replace_na(list(n = 0)) %>% 
  dcast(length_bin ~ management_unit, value.var = "n") %>% 
  arrange(length_bin) %>%
  write_csv("results/length_compositions/commyei_areafleets_lencomps.csv")

# Single year ----

# Combine all data pre-2006 from the core areas

yei_len %>% 
  filter(year < 2006 & management_unit %in% areas) %>% 
  count(length_bin) %>% 
  full_join(data.frame(expand.grid(length_bin = sort(unique(yei_len$length_bin))))) %>% 
  replace_na(list(n = 0)) %>% 
  arrange(length_bin) %>%
  write_csv("results/length_compositions/commyei_singleyear_lencomps.csv")

