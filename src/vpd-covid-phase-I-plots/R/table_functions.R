# tables

table_maker_impact <- function(d_pop, burden_t = "deaths", year_end = 2030, per_pop = TRUE){
  tmp <- d_pop %>%
    
    bind_rows(d_pop %>%
                filter(simple_scenario == "Postpone 2020 SIAs -> 2021") %>%
                group_by(country, year, modelling_group_tidy, disease, burden_outcome) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE),
                          population = sum(population, na.rm = TRUE)) %>%
                mutate(simple_scenario = "BAU")) %>%
    mutate(simple_scenario = factor(simple_scenario, levels = c("BAU", 
                                                                "50% RI",
                                                                "Postpone 2020 SIAs -> 2021",
                                                                
                                                                "50% RI, postpone 2020 SIAs -> 2021"))) %>%
    
    filter(year<=year_end, burden_outcome == burden_t) %>%
    group_by(modelling_group_tidy, simple_scenario, disease) %>%
    summarise(Excess = -sum(impact, na.rm = TRUE),
              pop = sum(population, na.rm = TRUE)) %>%
    
    
    mutate(disease = factor(disease, levels = c("Measles", "Meningitis A", "Yellow fever"))) %>%
    
    arrange(disease) 
  
  
  if(per_pop){
    tmp <- tmp %>% mutate(Excess = Excess/pop*100000) %>% select(-pop)
  } else { 
    tmp <- tmp %>% select(-pop)
  }
  
  tmp %>%
    
    pivot_wider(values_from = Excess, names_from = c(disease,modelling_group_tidy), names_sep = ", ") %>%
    
    rename(Scenario = simple_scenario) 
}


table_maker_percent_change <- function(d_pop, burden_t = "deaths", year_end = 2030){
  tmp <- d_pop %>%
    
    bind_rows(d_pop %>%
                filter(simple_scenario == "Postpone 2020 SIAs -> 2021") %>%
                group_by(country, year, modelling_group_tidy, disease, burden_outcome) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE),
                          population = sum(population, na.rm = TRUE)) %>%
                mutate(simple_scenario = "BAU")) %>%
    mutate(simple_scenario = factor(simple_scenario, levels = c("BAU", 
                                                                "50% RI",
                                                                "Postpone 2020 SIAs -> 2021",
                                                                
                                                                "50% RI, postpone 2020 SIAs -> 2021"))) %>%
    
    filter(year<=year_end, burden_outcome == burden_t) %>%
    group_by(modelling_group_tidy, simple_scenario, disease) %>%
    summarise(percent_diff = ifelse(sum(baseline_burden)>0,
                                    (sum(focal_burden) - sum(baseline_burden))/sum(baseline_burden) * 100,
                                    0)) %>%
    
    
    mutate(disease = factor(disease, levels = c("Measles", "Meningitis A", "Yellow fever"))) %>%
    
    arrange(disease) 
  
  
  tmp %>%
    
    filter(simple_scenario != "BAU") %>%
    
    pivot_wider(values_from = percent_diff, names_from = c(disease,modelling_group_tidy), names_sep = ", ") %>%
    
    rename(Scenario = simple_scenario) 
}

table_maker_percent_change_all <- function(d_pop, burden_t = "deaths", year_end = 2030){
  tmp <- d_pop %>%
    
    bind_rows(d_pop %>%
                filter(simple_scenario == "Postpone 2020 SIAs -> 2021") %>%
                group_by(country, year, modelling_group_tidy, disease, burden_outcome) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE),
                          population = sum(population, na.rm = TRUE)) %>%
                mutate(simple_scenario = "BAU")) %>%
    mutate(simple_scenario = factor(simple_scenario, levels = c("BAU", 
                                                                "50% RI",
                                                                "Postpone 2020 SIAs -> 2021",
                                                                
                                                                "50% RI, postpone 2020 SIAs -> 2021"))) %>%
    
    filter(year<=year_end, burden_outcome == burden_t) %>%
    group_by( simple_scenario, disease, year, country) %>%
    summarise(baseline_burden = mean(baseline_burden), focal_burden = mean(focal_burden)) %>%
    ungroup() %>%
    group_by(simple_scenario) %>%
    summarise(percent_diff = ifelse(sum(baseline_burden)>0,
                                    (sum(focal_burden) - sum(baseline_burden))/sum(baseline_burden) * 100,
                                    0)) 
 
}


table_maker_impact_per_dis_country <- function(d_pop, dis_name = "Measles", year_end = 2030, burden_t = "deaths" , per_pop = TRUE){
  tmp <- d_pop %>% 
    filter(year<= year_end) %>%
    filter(disease == dis_name) %>%
    filter(burden_outcome == burden_t) %>%
    group_by(country, modelling_group_tidy, simple_scenario) %>%
    summarise(value = -sum(impact, na.rm = TRUE),
              pop = sum(population, na.rm = TRUE)) %>%
    arrange(simple_scenario) 
  
  if(per_pop){
    tmp <- tmp %>% mutate(value = round(value/pop*100000,2)) %>% select(-pop)
  } else { 
    tmp <- tmp %>% select(-pop) %>% mutate(value = round(value))
  }
  
  
  tmp %>%
    pivot_wider(values_from = value, names_from = c(simple_scenario, modelling_group_tidy), names_sep = ", ") 
  
}

table_maker_impact_per_dis_year <- function(d_pop, dis_name = "Measles", year_end = 2030, burden_t = "deaths", per_pop = TRUE){
  tmp <- d_pop %>% 
    filter(year<= year_end) %>%
    filter(disease == dis_name) %>%
    filter(burden_outcome == burden_t) %>%
    group_by(year, modelling_group_tidy, simple_scenario) %>%
    summarise(value = -sum(impact, na.rm = TRUE),
              pop = sum(population, na.rm = TRUE)) %>%
    arrange(simple_scenario) 
  
  if(per_pop){
    tmp <- tmp %>% mutate(value = round(value/pop*100000,2)) %>% select(-pop)
  } else { 
    tmp <- tmp %>% select(-pop) %>% mutate(value = round(value))
  }
  
  
  tmp %>%
    pivot_wider(values_from = value, names_from = c(simple_scenario, modelling_group_tidy), names_sep = ", ") 
  
}
