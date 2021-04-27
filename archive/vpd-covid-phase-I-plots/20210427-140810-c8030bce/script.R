
R.utils::sourceDirectory("R", modifiedOnly = FALSE)
options(dplyr.summarise.inform = FALSE)

#---------------------------------------------------------------------------------------------------------------------
## LOAD DATA

### shift data load here
d_orig <- readRDS("impact.rds")
country_names <- read.csv("country_names.csv", stringsAsFactors = FALSE)

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

d <- d %>% mutate(simple_scenario = case_when(grepl("bau", scenario_type) ~       "Business as usual",
                                              grepl("scenario2", scenario_type) ~ "Postpone 2020 SIAs until 2021",
                                              grepl("scenario7", scenario_type) ~ "50% reduction in RI",
                                              grepl("scenario8", scenario_type) ~ "50% reduction in RI, postpone 2020 SIAs until 2021"))

d <- d %>% mutate(simple_scenario = factor(simple_scenario, levels = c("Business as usual",
                                                                       "Postpone 2020 SIAs until 2021",
                                                                       "50% reduction in RI",
                                                                       "50% reduction in RI, postpone 2020 SIAs until 2021")))

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

#country names
d_pop <- d_pop %>% mutate(country_name = country_names$country_name[match(country, country_names$country)])

#---------------------------------------------------------------------------------------------------------------------
# FIGURES

#Deaths per year up to 2030 per disease
p1 <- figure_maker_burden_per_mod_dis(d_pop, burden_t = "deaths", scenario_pal, 
                                      year_end = 2030, dis_name = "Yellow fever",
                                      file_name = "deaths_by_year_YF", 
                                      figheight = 10, figwidth = 16)


p2 <- figure_maker_burden_per_mod_dis(d_pop, burden_t = "deaths", scenario_pal, 
                                      year_end = 2030, dis_name = "Meningitis A",
                                      file_name = "deaths_by_year_MeningitisA", 
                                      figheight = 10, figwidth = 16)

p3 <- figure_maker_burden_per_mod_dis(d_pop, burden_t = "deaths", scenario_pal, 
                                      year_end = 2030, dis_name = "Measles",
                                      file_name = "deaths_by_year_Measles", 
                                      figheight = 10, figwidth = 16)

leg <- get_legend(p3)

p4 <- cowplot::plot_grid(NULL,
                         p3 + theme(legend.position = "none"),
                         p2 + theme(legend.position = "none"),
                         p1 + theme(legend.position = "none"),
                         leg,
                         ncol = 1,
                         rel_heights = c(0.1,1,1,1,0.1),
                         labels= c("","Measles", "Meningitis A", "Yellow Fever"),
                         hjust = -0.1,
                         vjust = 0.1)

ggsave(plot = p4, filename = "deaths_by_year_all.png", height = 12, width = 16)
#---------------------------------------------------------------------------------------
#DALYs per year up to 2030 per disease
p1 <- figure_maker_burden_per_mod_dis(d_pop, burden_t = "dalys", scenario_pal, 
                                      year_end = 2030, dis_name = "Yellow fever",
                                      file_name = "dalys_by_year_YF", 
                                      figheight = 10, figwidth = 16)


p2 <- figure_maker_burden_per_mod_dis(d_pop, burden_t = "dalys", scenario_pal, 
                                      year_end = 2030, dis_name = "Meningitis A",
                                      file_name = "dalys_by_year_MeningitisA", 
                                      figheight = 10, figwidth = 16)

p3 <- figure_maker_burden_per_mod_dis(d_pop, burden_t = "dalys", scenario_pal, 
                                      year_end = 2030, dis_name = "Measles",
                                      file_name = "dalys_by_year_Measles", 
                                      figheight = 10, figwidth = 16)

leg <- get_legend(p3)

p4 <- cowplot::plot_grid(NULL,
                         p3 + theme(legend.position = "none"),
                         p2 + theme(legend.position = "none"),
                         p1 + theme(legend.position = "none"),
                         leg,
                         ncol = 1,
                         rel_heights = c(0.1,1,1,1,0.1),
                         labels= c("","Measles", "Meningitis A", "Yellow Fever"),
                         hjust = -0.1,
                         vjust = 0.1)

ggsave(plot = p4, filename = "dalys_by_year_all.png", height = 12, width = 16)
#---------------------------------------------------------------------------------------
#Deaths per year up to 2030 per disease
p1 <- figure_maker_burden_per_mod_dis(d_pop, burden_t = "deaths_under5", scenario_pal, 
                                      year_end = 2030, dis_name = "Yellow fever",
                                      file_name = "deaths_by_year_YF_under5", 
                                      figheight = 10, figwidth = 16)


p2 <- figure_maker_burden_per_mod_dis(d_pop, burden_t = "deaths", scenario_pal, 
                                      year_end = 2030, dis_name = "Meningitis A",
                                      file_name = "deaths_by_year_MeningitisA_under5", 
                                      figheight = 10, figwidth = 16)

p3 <- figure_maker_burden_per_mod_dis(d_pop, burden_t = "deaths_under5", scenario_pal, 
                                      year_end = 2030, dis_name = "Measles",
                                      file_name = "deaths_by_year_Measles_under5", 
                                      figheight = 10, figwidth = 16)

leg <- get_legend(p3)

p4 <- cowplot::plot_grid(NULL,
                         p3 + theme(legend.position = "none"),
                         p2 + theme(legend.position = "none"),
                         p1 + theme(legend.position = "none"),
                         leg,
                         ncol = 1,
                         rel_heights = c(0.1,1,1,1,0.1),
                         labels= c("","Measles", "Meningitis A", "Yellow Fever"),
                         hjust = -0.1,
                         vjust = 0.1)

ggsave(plot = p4, filename = "deaths_by_year_all_under5.png", height = 12, width = 16)
#---------------------------------------------------------------------------------------
#Deaths per 100,000 per year up to 2030 per disease
p1 <- figure_maker_burden_per_mod_dis_pop(d_pop, burden_t = "deaths", scenario_pal, 
                                          year_end = 2030, dis_name = "Yellow fever",
                                          file_name = "deaths_by_year_pop_YF", 
                                          figheight = 10, figwidth = 12)

p2 <- figure_maker_burden_per_mod_dis_pop(d_pop, burden_t = "deaths", scenario_pal, 
                                          year_end = 2030, dis_name = "Meningitis A",
                                          file_name = "deaths_by_year_pop_MeningitisA", 
                                          figheight = 10, figwidth = 12)

p3 <- figure_maker_burden_per_mod_dis_pop(d_pop, burden_t = "deaths", scenario_pal, 
                                          year_end = 2030, dis_name = "Measles",
                                          file_name = "deaths_by_year_pop_Measles", 
                                          figheight = 10, figwidth = 12)

leg <- get_legend(p3)

p4 <- cowplot::plot_grid(NULL,
                         p3 + theme(legend.position = "none"),
                         p2 + theme(legend.position = "none"),
                         p1 + theme(legend.position = "none"),
                         leg,
                         ncol = 1,
                         rel_heights = c(0.1,1,1,1,0.1),
                         labels= c("","Measles", "Meningitis A", "Yellow Fever"),
                         hjust = -0.1,
                         vjust = 0.1)

ggsave(plot = p4, filename = "deaths_by_year_pop_all.png", height = 12, width = 16)
#---------------------------------------------------------------------------------------
# Excess deaths per year up to 2030 as a percentage change from baseline.
p1 <- figure_maker_excess_timeline_per_disease_mod(d_pop, burden_t = "deaths", scenario_pal, 
                                                   year_end = 2030, dis_name = "Yellow fever",
                                                   file_name = "excess_deaths_by_year_YF", 
                                                   figheight = 10, figwidth = 12)

p2 <- figure_maker_excess_timeline_per_disease_mod(d_pop, burden_t = "deaths", scenario_pal, 
                                                   year_end = 2030, dis_name = "Meningitis A",
                                                   file_name = "excess_deaths_by_year_MeningitisA", 
                                                   figheight = 10, figwidth = 12)

p3 <- figure_maker_excess_timeline_per_disease_mod(d_pop, burden_t = "deaths", scenario_pal, 
                                                   year_end = 2030, dis_name = "Measles",
                                                   file_name = "excess_deaths_by_year_Measles", 
                                                   figheight = 10, figwidth = 12)

leg <- get_legend(p3)

p4 <- cowplot::plot_grid(NULL,
                         p3 + theme(legend.position = "none"),
                         p2 + theme(legend.position = "none"),
                         p1 + theme(legend.position = "none"),
                         leg,
                         ncol = 1,
                         rel_heights = c(0.1,1,1,1,0.1),
                         labels= c("","Measles", "Meningitis A", "Yellow Fever"),
                         hjust = -0.1,
                         vjust = 0.1)

ggsave(plot = p4, filename = "excess_deaths_by_year_all.png", height = 12, width = 16)
#---------------------------------------------------------------------------------------
#Deaths per 100,000 per year up to 2030 per disease
p1 <- figure_maker_burden_per_mod_dis_pop(d_pop, burden_t = "deaths", scenario_pal, 
                                          year_end = 2030, dis_name = "Yellow fever",
                                          file_name = "deaths_by_year_pop_YF", 
                                          figheight = 10, figwidth = 12)

p2 <- figure_maker_burden_per_mod_dis_pop(d_pop, burden_t = "deaths", scenario_pal, 
                                          year_end = 2030, dis_name = "Meningitis A",
                                          file_name = "deaths_by_year_pop_MeningitisA", 
                                          figheight = 10, figwidth = 12)

p3 <- figure_maker_burden_per_mod_dis_pop(d_pop, burden_t = "deaths", scenario_pal, 
                                          year_end = 2030, dis_name = "Measles",
                                          file_name = "deaths_by_year_pop_Measles", 
                                          figheight = 10, figwidth = 12)

leg <- get_legend(p3)

p4 <- cowplot::plot_grid(NULL,
                         p3 + theme(legend.position = "none"),
                         p2 + theme(legend.position = "none"),
                         p1 + theme(legend.position = "none"),
                         leg,
                         ncol = 1,
                         rel_heights = c(0.1,1,1,1,0.1),
                         labels= c("","Measles", "Meningitis A", "Yellow Fever"),
                         hjust = -0.1,
                         vjust = 0.1)

ggsave(plot = p4, filename = "deaths_by_year_pop_all.png", height = 12, width = 16)
#---------------------------------------------------------------------------------------
# Excess deaths per year up to 2030 as a percentage change from baseline.
p1 <- figure_maker_excess_timeline_per_disease_mod(d_pop, burden_t = "deaths_under5", scenario_pal, 
                                                   year_end = 2030, dis_name = "Yellow fever",
                                                   file_name = "excess_deaths_by_year_YF_under5", 
                                                   figheight = 10, figwidth = 12)

p2 <- figure_maker_excess_timeline_per_disease_mod(d_pop, burden_t = "deaths_under5", scenario_pal, 
                                                   year_end = 2030, dis_name = "Meningitis A",
                                                   file_name = "excess_deaths_by_year_MeningitisA_under5", 
                                                   figheight = 10, figwidth = 12)

p3 <- figure_maker_excess_timeline_per_disease_mod(d_pop, burden_t = "deaths_under5", scenario_pal, 
                                                   year_end = 2030, dis_name = "Measles",
                                                   file_name = "excess_deaths_by_year_Measles_under5", 
                                                   figheight = 10, figwidth = 12)

leg <- get_legend(p3)

p4 <- cowplot::plot_grid(NULL,
                         p3 + theme(legend.position = "none"),
                         p2 + theme(legend.position = "none"),
                         p1 + theme(legend.position = "none"),
                         leg,
                         ncol = 1,
                         rel_heights = c(0.1,1,1,1,0.1),
                         labels= c("","Measles", "Meningitis A", "Yellow Fever"),
                         hjust = -0.1,
                         vjust = 0.1)

ggsave(plot = p4, filename = "excess_deaths_by_year_all_under5.png", height = 12, width = 16)
#---------------------------------------------------------------------------------------
# survival change per year up to 2030 as a percentage change from baseline.
p1 <- figure_maker_survival_timeline_per_disease_mod(d_pop, burden_t = "deaths", scenario_pal, 
                                                     year_end = 2030, dis_name = "Yellow fever",
                                                     file_name = "survival_change_by_year_YF", 
                                                     figheight = 10, figwidth = 12)

p2 <- figure_maker_survival_timeline_per_disease_mod(d_pop, burden_t = "deaths", scenario_pal, 
                                                     year_end = 2030, dis_name = "Meningitis A",
                                                     file_name = "survival_change_by_year_MeningitisA", 
                                                     figheight = 10, figwidth = 12)

p3 <- figure_maker_survival_timeline_per_disease_mod(d_pop, burden_t = "deaths", scenario_pal, 
                                                     year_end = 2030, dis_name = "Measles",
                                                     file_name = "survival_change_by_year_Measles", 
                                                     figheight = 10, figwidth = 12)

leg <- get_legend(p3)

p4 <- cowplot::plot_grid(NULL,
                         p3 + theme(legend.position = "none"),
                         p2 + theme(legend.position = "none"),
                         p1 + theme(legend.position = "none"),
                         leg,
                         ncol = 1,
                         rel_heights = c(0.1,1,1,1,0.1),
                         labels= c("","Measles", "Meningitis A", "Yellow Fever"),
                         hjust = -0.1,
                         vjust = 0.1)

ggsave(plot = p4, filename = "survival_change_by_year_all.png", height = 12, width = 16)

#---------------------------------------------------------------------------------------
#Excess deaths per 100,000 population per year from 2020 to 2030 for the model averaged predictions for Measles, 
# Meningitis A and Yellow Fever by country.The error bars range from max to min group preditions.
figure_maker_excess_country_pop(d_pop, "deaths", scenario_pal, file_name = "excess_deaths_per_pop_by_country",
                                figheight = 10, figwidth = 16)

figure_maker_excess_country_pop(d_pop, "dalys", scenario_pal, file_name = "excess_dalys_per_pop_by_country",
                                figheight = 10, figwidth = 16)
#---------------------------------------------------------------------------------------
#Normalised excess deaths from 2020 to 2030 for the model averaged predictions for Measles, Meningitis A and Yellow Fever 
# per country
p1 <- figure_maker_norm_per_country_dis(d_pop, "deaths", dis_name = "Yellow fever",
                                        scenario_pal, file_name = "norm_deaths_per_country_YF", figwidth = 16, figheight = 10)
p2 <- figure_maker_norm_per_country_dis(d_pop, "deaths", dis_name = "Meningitis A",
                                        scenario_pal, file_name = "norm_deaths_per_country_MenA", figwidth = 16, figheight = 10)
p3 <- figure_maker_norm_per_country_dis(d_pop, "deaths", dis_name = "Measles",
                                        scenario_pal, file_name = "norm_deaths_per_country_Measles", figwidth = 16, figheight = 10)
leg <- get_legend(p3)
shape_leg <- get_legend(figure_maker_norm_per_country_dis(d_pop, "deaths", dis_name = "Measles",
                                                          scenario_pal, file_name = "norm_deaths_per_country_Measles", 
                                                          figwidth = 16, figheight = 10,
                                                          shape_leg = TRUE))
p4 <- cowplot::plot_grid(NULL,
                         p3+theme(legend.position = "none"),
                         p2+theme(legend.position = "none"),
                         p1+theme(legend.position = "none"),
                         leg,
                         shape_leg,
                         ncol = 1,
                         rel_heights = c(0.1,1,1,1, 0.1, 0.1),
                         labels= c("","Measles", "Meningitis A", "Yellow Fever"),
                         hjust = -0.1,
                         vjust = 0.1)

ggsave(plot = p4, filename = "norm_deaths_per_country.png", height = 12, width = 16)
#---------------------------------------------------------------------------------------
#Normalised excess dalys from 2020 to 2030 for the model averaged predictions for Measles, Meningitis A and Yellow Fever 
# per country
p1 <- figure_maker_norm_per_country_dis(d_pop, "dalys", dis_name = "Yellow fever",
                                        scenario_pal, file_name = "norm_dalys_per_country_YF", figwidth = 16, figheight = 10)
p2 <- figure_maker_norm_per_country_dis(d_pop, "dalys", dis_name = "Meningitis A",
                                        scenario_pal, file_name = "norm_dalys_per_country_MenA", figwidth = 16, figheight = 10)
p3 <- figure_maker_norm_per_country_dis(d_pop, "dalys", dis_name = "Measles",
                                        scenario_pal, file_name = "norm_dalys_per_country_Measles", figwidth = 16, figheight = 10)
leg <- get_legend(p3)
shape_leg <- get_legend(figure_maker_norm_per_country_dis(d_pop, "dalys", dis_name = "Measles",
                                                          scenario_pal, file_name = "norm_dalys_per_country_Measles", 
                                                          figwidth = 16, figheight = 10,
                                                          shape_leg = TRUE))
p4 <- cowplot::plot_grid(NULL,
                         p3+theme(legend.position = "none"),
                         p2+theme(legend.position = "none"),
                         p1+theme(legend.position = "none"),
                         leg,
                         shape_leg,
                         ncol = 1,
                         rel_heights = c(0.1,1,1,1, 0.1, 0.1),
                         labels= c("","Measles", "Meningitis A", "Yellow Fever"),
                         hjust = -0.1,
                         vjust = 0.1)

ggsave(plot = p4, filename = "norm_dalys_per_country.png", height = 12, width = 16)

#---------------------------------------------------------------------------------------
#Excess deaths in under5s
figure_maker_under5(d_pop)

#---------------------------------------------------------------------------------------------------------------------
# SAVE TABLES

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

dev.off()