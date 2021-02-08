
R.utils::sourceDirectory("R", modifiedOnly = FALSE)

### shift data load here
d_orig <- readRDS("impact.rds")

#average per disease
d <- d_orig %>% 
  mutate(year = as.numeric(year), 
         impact = as.numeric(impact), 
         focal_burden = as.numeric(focal_burden), 
         baseline_burden = as.numeric(baseline_burden)) %>% 
  group_by(country, year, modelling_group, burden_outcome, touchstone, disease, scenario_type, scenario_description) %>%
  summarise(impact = mean(impact, na.rm = TRUE),
            baseline_burden = mean(baseline_burden, na.rm = TRUE),
            focal_burden = mean(focal_burden, na.rm = TRUE)) %>%
  ungroup()

d <- d %>% filter(grepl("bau|scenario2|scenario7|scenario8", scenario_type))

d <- d %>% mutate(simple_scenario = case_when(grepl("bau", scenario_type) ~ "BAU",
                                              grepl("scenario2", scenario_type) ~ "Postpone 2020 SIAs -> 2021",
                                              grepl("scenario7", scenario_type) ~ "50% RI",
                                              grepl("scenario8", scenario_type) ~ "50% RI, postpone 2020 SIAs -> 2021"))

d <- d %>% mutate(simple_scenario = factor(simple_scenario, levels = c("BAU", 
                                                                       "50% RI",
                                                                       "Postpone 2020 SIAs -> 2021",
                                                                       "50% RI, postpone 2020 SIAs -> 2021")))

d <-  d %>% filter(!grepl("portnoy", scenario_type)) # choosing Wolfson CFR for measles

# get colours
disease_pal <- disease_palette(unique(d$disease))

scenario_pal <- rev(c("#EA7580", "#14A7B3","#F7BE9F",  "grey50"))

#tidy names
d <- d %>% ungroup() %>% mutate(disease = case_when(disease == "YF" ~ "Yellow fever",
                                                    disease == "MenA" ~ "Meningitis A",
                                                    disease == "Measles" ~ "Measles"))

p <- vimpact::get_population(con, touchstone_pop = "202005covid", country_ = unique(d$country))

p <- p %>% 
  rename(population = value) %>% 
  group_by(country, year, gender) %>% 
  summarise(population = sum(population, na.rm = TRUE))

d_pop <- d %>% left_join(p, by = c("year", "country"))


## translate modelling groups
d_pop <-  d_pop %>% mutate(modelling_group_tidy = case_when(modelling_group == "Cambridge-Trotter" ~ "Cambridge",
                                                            modelling_group == "KPW-Jackson" ~ "KP",
                                                            modelling_group == "LSHTM-Jit" ~ "DynaMICE",
                                                            modelling_group == "PSU-Ferrari" ~ "Penn State",
                                                            modelling_group == "IC-Garske" ~ "Imperial",
                                                            modelling_group == "UND-Perkins" ~ "Notre Dame",
                                                            modelling_group == "McCarthy-ETH" ~ "IDM",
                                                            modelling_group == "McCarthy-NGA" ~ "IDM"))

# remove ETH and UGA for YF
d_pop <- d_pop %>% filter(!(country %in% c("ETH", "UGA") & disease == "Yellow fever"))


saveRDS(d_pop, "d_pop.rds")

rmarkdown::render("report.Rmd")

# save tables

# plain impact overall
tmp <- table_maker_impact(d_pop, burden_t = "deaths", year_end= 2030, per_pop = FALSE)

write.csv(tmp, "excess_deaths_2020_2030.csv", row.names = FALSE)

tmp <- table_maker_impact(d_pop, burden_t = "deaths_under5", year_end= 2030, per_pop = FALSE)

write.csv(tmp, "excess_deaths_under5_2020_2030.csv", row.names = FALSE)

tmp <- table_maker_impact(d_pop, burden_t = "deaths", year_end= 2030, per_pop = TRUE)

write.csv(tmp, "excess_deaths_per_pop_2020_2030.csv", row.names = FALSE)


tmp <- table_maker_impact(d_pop, burden_t = "dalys", year_end= 2030, per_pop = FALSE) 

write.csv(tmp,"excess_dalys_2020_2030.csv", row.names = FALSE)

tmp <- table_maker_impact(d_pop, burden_t = "dalys_under5", year_end= 2030, per_pop = FALSE) 

write.csv(tmp,"excess_dalys_under5_2020_2030.csv", row.names = FALSE)

tmp <- table_maker_impact(d_pop, burden_t = "dalys", year_end= 2030, per_pop = TRUE) 

write.csv(tmp,"excess_dalys_per_pop_2020_2030.csv", row.names = FALSE)

# per disease and country per pop
lapply(unique(d_pop$disease),
       FUN = function(x){table_maker_impact_per_dis_country(d_pop, dis_name = x, year_end = 2030, "deaths", per_pop = TRUE) %>%
           write.csv(paste0("excess_deaths_per_pop_", x, "_country_2000_2030.csv"), row.names = FALSE)})

# per disease and country
lapply(unique(d_pop$disease),
       FUN = function(x){table_maker_impact_per_dis_country(d_pop, dis_name = x, year_end = 2030, "deaths", per_pop = FALSE) %>%
           write.csv(paste0("excess_deaths_", x, "_country_2000_2030.csv"), row.names = FALSE)})

# per disease and year per pop
lapply(unique(d_pop$disease),
       FUN = function(x){table_maker_impact_per_dis_year(d_pop, dis_name = x, year_end = 2030, "deaths", per_pop = TRUE) %>%
           write.csv(paste0("excess_deaths_per_pop_", x, "_year_2000_2030.csv"), row.names = FALSE)})

# per disease and year 
lapply(unique(d_pop$disease),
       FUN = function(x){table_maker_impact_per_dis_year(d_pop, dis_name = x, year_end = 2030, "deaths", per_pop = FALSE) %>%
           write.csv(paste0("excess_deaths_", x, "_year_2000_2030.csv"), row.names = FALSE)})

# percentage change from baseline
tmp <- table_maker_percent_change(d_pop, burden_t = "deaths", year_end = 2030)

write.csv(tmp, "percent_change_deaths_2020_2030.csv", row.names = FALSE)

tmp <- table_maker_percent_change(d_pop, burden_t = "dalys", year_end = 2030)

write.csv(tmp, "percent_change_dalys_2020_2030.csv", row.names = FALSE)

#percentage change overall
tmp <- table_maker_percent_change_all(d_pop, burden_t = "deaths", year_end = 2030)

write.csv(tmp, "percent_change_deaths_all_2020_2030.csv", row.names = FALSE)

tmp <- table_maker_percent_change_all(d_pop, burden_t = "dalys", year_end = 2030)

write.csv(tmp, "percent_change_dalys_all_2020_2030.csv", row.names = FALSE)