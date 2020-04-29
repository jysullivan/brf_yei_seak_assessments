# Prep sport and commercial catch data for assessment
# Jane Sullivan
# Last updated 

# Set up ----

# Maps of management units:
# https://www.adfg.alaska.gov/index.cfm?adfg=CommercialByFisheryGroundfish.groundfishmaps_managementareas

source("code/helper.R")

YEAR <- 2019 # most recent year of data

# Sport YEI & BRF data ----

# NOTE: from sport_rockfish_seak\results\catch_reconstruction!! Transfered into
# comm_brf_yei_seak/data manually
sport <- read_csv("data/sportfish_catch_modelinputs.csv") %>% 
  select(year, catch_mt, stock) %>% 
  mutate(fishery = "Sport")

# Commercial YEI & BRF data ---
comm <- read_csv("results/catch/commercial_yeibrf_catch_modelinputs.csv") %>% 
  mutate(fishery = "Commercial")

catch <- bind_rows(sport, comm)

# Commercial split by bycatch (mostly DSR longline) and directed (jig)
brf_byfsh <- read_csv("results/catch/comm_brf_byfishery.csv")

# YEI ----

yei <- catch %>% 
  filter(stock == "YEI" & year < YEAR)

# By fishery
ggplot(yei,
       aes(x = year, y = catch_mt, fill = fishery)) +
  geom_area(alpha = 0.6, size = 0.5, colour = "white") +
  scale_fill_viridis_d() +
  labs(x = "Year", y = "Total mortality (mt)", 
       title = paste0("YEI estimated total mortality, 1888-2018"),
       fill = NULL) +
  geom_vline(xintercept = 1985, lty = 2) +
  theme(legend.position = "bottom") +
  scale_y_continuous(label = scales::comma) +
  scale_x_continuous(breaks = seq(1890, 2020, 10), labels = seq(1890, 2020, 10))

ggsave("results/assessment_inputs/YEI/yei_catch_byfishery.png", dpi = 300, height = 4, width = 6, units = "in")

# Total
yei_tot <- yei %>% 
  group_by(year) %>% 
  summarise(catch_mt = sum(catch_mt))

ggplot(yei_tot, aes(x = year, y = catch_mt)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Total mortality (mt)", 
       title = paste0("YEI estimated total mortality, 1888-2018")) +
  geom_vline(xintercept = 1985, lty = 2, col = "darkgrey") +
  theme(legend.position = "bottom") +
  scale_y_continuous(label = scales::comma) +
  scale_x_continuous(breaks = seq(1890, 2020, 10), labels = seq(1890, 2020, 10))

ggsave("results/assessment_inputs/YEI/yei_catch.png", dpi = 300, height = 4, width = 6, units = "in")

yei_tot %>% 
  rename(Year = year, Catches = catch_mt) %>% 
  write_csv("results/assessment_inputs/YEI/Catches.csv")

# BRF ----

brf <- catch %>% 
  filter(stock == "BRF" & year < YEAR)

# By fishery (sport vs comm)
ggplot(brf,
       aes(x = year, y = catch_mt, fill = fishery)) +
  geom_area(alpha = 0.6, size = 0.5, colour = "white") +
  scale_fill_viridis_d() +
  labs(x = "Year", y = "Total mortality (mt)", 
       title = paste0("BRF estimated total mortality, 1985-2018"),
       fill = NULL) +
  theme(legend.position = "bottom") +
  scale_y_continuous(label = scales::comma) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), labels = seq(1985, 2020, 5))

ggsave("results/assessment_inputs/BRF/brf_catch_byfishery.png", dpi = 300, height = 4, width = 6, units = "in")

# Total
brf_tot <- brf %>% 
  group_by(year) %>% 
  summarise(catch_mt = sum(catch_mt))

ggplot(brf_tot, aes(x = year, y = catch_mt)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Total mortality (mt)", 
       title = paste0("BRF estimated total mortality, 1985-2018")) +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, NA), label = scales::comma) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), labels = seq(1985, 2020, 5))

ggsave("results/assessment_inputs/brf/brf_catch.png", dpi = 300, height = 4, width = 6, units = "in")

brf_tot %>% 
  rename(Year = year, Catches = catch_mt) %>% 
  write_csv("results/assessment_inputs/brf/Catches_total.csv")

# BRF v2 ----

# Catches split by fishery definition used to separate length comps
brf_byfsh <- brf_byfsh %>% 
  bind_rows(
    sport %>% 
      filter(stock == "BRF") %>% 
      mutate(fishery = "Directed")) %>%  # Combine sport with commercial directed
  group_by(year, fishery) %>% 
  summarize(catch_mt = sum(catch_mt)) %>% 
  mutate(fishery = ifelse(fishery == "Directed", "Sport and Commercial Jig",
                          "Commercial Longline")) %>% 
  filter(year < YEAR)
  
# By fishery (sport/jig/directed vs comm longline bycatch)
ggplot(brf_byfsh,
       aes(x = year, y = catch_mt, fill = fishery)) +
  geom_area(alpha = 0.6, size = 0.5, colour = "white") +
  scale_fill_viridis_d() +
  labs(x = "Year", y = "Total mortality (mt)", 
       title = paste0("BRF estimated total mortality, 1985-2018"),
       fill = NULL) +
  theme(legend.position = "bottom") +
  scale_y_continuous(label = scales::comma) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), labels = seq(1985, 2020, 5))

ggsave("results/assessment_inputs/BRF/brf_catch_byfishery2.png", dpi = 300, height = 4, width = 6, units = "in")

# Save directed fishery (sport/comm jig)
brf_byfsh %>% 
  filter(fishery == "Sport and Commercial Jig") %>% 
  rename(Year = year, Catches = catch_mt) %>% 
  select(Year, Catches) %>% 
  write_csv("results/assessment_inputs/brf/Catches.csv")

# Save bycatch fishery (comm longline)
brf_byfsh %>% 
  filter(fishery == "Commercial Longline") %>% 
  rename(Year = year, Catches = catch_mt) %>% 
  select(Year, Catches) %>% 
  write_csv("results/assessment_inputs/brf/Catches2.csv")

