# Harvest summaries
# Jane Sullivan
# Last updated: April 2020

# Set up ----

source("code/helper.R")

YEAR <- 2019 # most recent year of data

yei <- read_csv("data/Yelloweyeinside.csv") %>% 
  mutate(stock = "YEI") %>% 
  select(-X1)

# Mandatory retention for YEI went into regulation in 2001. For YEO even though
# there is mandatory retention, the byacth is inflated by 10-15% to account for
# illegal catch or inaccurate reporting. We have never done this for YEI. In
# order to account for this in inside waters, inflate bycatch by 15% in
# 2001-present. For 1985-2000, inflate by 25% based on expert opinion (both from
# biometrics and management biologists.
yei <- yei %>% 
  mutate(round_lb = ifelse(fishery == "Bycatch" & year >= 2001,
                           round_lb + round_lb * 0.15,
                           ifelse(fishery == "Bycatch" & year < 2001,
                                  round_lb + round_lb * 0.25,
                                  round_lb)))

brf <- read_csv("data/Blackrockfish.csv") %>% 
  mutate(stock = "BRF",
         # Personal communication with K Howard 2020-04-23: EWYKT = EYKT + IBS
         # for harvest data. Make bio data match this assumption
         management_unit = ifelse(management_unit %in% c("EYKT", "IBS"), 
                                  "EWYKT", management_unit))

yei_expand <- data.frame(expand.grid(stock = "YEI",
                                     management_unit = unique(yei$management_unit),
                                     fishery = unique(yei$fishery),
                                     year = 1985:YEAR))
yei <- yei %>% 
  full_join(yei_expand) %>% 
  replace_na(list(round_lb = 0))

brf_expand <- data.frame(expand.grid(stock = "BRF",
                                     management_unit = unique(brf$management_unit),
                                     year = 1985:YEAR,
                                     fishery = unique(brf$fishery)))
brf <- brf %>% 
  full_join(brf_expand) %>% 
  replace_na(list(round_lb = 0))

# Since length comps are split up, also split up directed/byatch for input to
# the assessment
brf %>% 
  group_by(stock, fishery, year) %>% 
  summarize(catch_mt = 0.000453592 * sum(round_lb)) %>% 
  write_csv("results/catch/comm_brf_byfishery.csv")

total_mort <- bind_rows(yei, brf) 

# Combine bycatch and directed
total_mort <- total_mort %>% 
  group_by(stock, year, management_unit) %>% 
  mutate(total_mort_lb = sum(round_lb)) %>% 
  ungroup() %>% 
  arrange(year, stock, management_unit)

ggplot(total_mort %>% 
         distinct(year, management_unit, stock, total_mort_lb),
       aes(x = year, y = total_mort_lb, fill = management_unit)) +
  geom_area(alpha = 0.6, size = 0.5, colour = "white") +
  scale_fill_viridis_d() +
  facet_wrap(~stock, scales = "free") +
  labs(x = "Year", y = "Total mortality (round lb)", 
       title = paste0("Total mortality from commercial fisheries (round lb)"),
       fill = "Management area") +
  theme(legend.position = "bottom") +
  scale_y_continuous(label = scales::comma) 

ggsave("results/catch/total_harvest_byarea.png", dpi = 300, height = 4, width = 7, units = "in")

write_csv(total_mort %>% 
            distinct(year, management_unit, stock, total_mort_lb),
          "results/catch/total_harvest_yei_brf.csv")

# By source BRF ----

ggplot(total_mort %>% 
         filter(stock == "BRF"),
       aes(x = year, y = round_lb, fill = fishery)) +
  geom_area(alpha = 0.6, size = 0.5, colour = "white") +
  scale_fill_viridis_d() +
  facet_wrap(~management_unit, scales = "free") +
  labs(x = "Year", y = "Total mortality (round lb)", 
       title = paste0("BRF total mortality from commercial fisheries (round lb)"),
       fill = "Fishery") +
  theme(legend.position = "bottom") +
  scale_y_continuous(label = scales::comma) 

ggsave("results/catch/brf_harvest_byfishery.png", dpi = 300, height = 5, width = 7, units = "in")
# ggsave("results/catch/brf_harvest_byfishery_scaled.png", dpi = 300, height = 5, width = 7, units = "in")

# By source YE ----

ggplot(total_mort %>% 
         filter(stock == "YEI"),
       aes(x = year, y = round_lb, fill = fishery)) +
  geom_area(alpha = 0.6, size = 0.5, colour = "white") +
  scale_fill_viridis_d() +
  facet_wrap(~management_unit, scales = "free") +
  labs(x = "Year", y = "Total mortality (round lb)", 
       title = paste0("YEI total mortality from commercial fisheries (round lb)"),
       fill = "Fishery") +
  theme(legend.position = "bottom") +
  scale_y_continuous(label = scales::comma) 

ggsave("results/catch/yei_harvest_byfishery.png", dpi = 300, height = 4, width = 6, units = "in")
# ggsave("results/catch/yei_harvest_byfishery_scaled.png", dpi = 300, height = 4, width = 6, units = "in")

# Halibut harvest -----

halibut <- read_csv("data/halibut_comm_landing_mt.csv") %>% 
  rename(halibut_mt = `2C_halibut_comm_landings_mt`)

# This analysis assumes that pre-1985, the bycatch of YEI in the halibut fishery
# was 5% of the IPHC 2C area catch. 5% is based on expert opinion and K Wood's
# GIS work looking at bycatch rates over space and time and habitat suitability
# for YEI. It assumes the proportion of halibut catch in inside and outside
# waters in constant over time, and that bycatch rates have been constant in the
# halibut fishery (they may have been much higher in early years if YE were
# highly abundant)
halibut %>% 
  filter(year < 1985) %>% 
  mutate(catch_mt = 0.05 * halibut_mt,
         stock = "YEI") %>% 
  select(-halibut_mt) %>% 
  bind_rows(
    total_mort %>% 
      group_by(stock, year) %>% 
      summarize(catch_mt = sum(round_lb) * 0.000453592) %>% 
      ungroup()) %>% 
  arrange(year) -> mort
  
ggplot(mort %>% 
         filter(year <= YEAR & stock == "YEI"), 
       aes(x = year, y = catch_mt)) +
  geom_line() +
  geom_vline(xintercept = 1985, col = "grey", lty = 2) +
  geom_point() +
  labs(x = "Year", y = "Total mortality (mt)", title = "YEI commercial catch reconstruction, 1888 - 2019") +
  scale_x_continuous(breaks = seq(1890, 2020, 10), labels = seq(1890, 2020, 10))

ggsave("results/catch/yei_total_harvest.png", dpi = 300, height = 4, width = 6, units = "in")

write_csv(mort, "results/catch/commercial_yeibrf_catch_modelinputs.csv")
